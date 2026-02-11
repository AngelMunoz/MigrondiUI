module MigrondiUI.VirtualFs

open System
open System.Collections.Generic
open System.IO
open System.Text
open System.Text.RegularExpressions
open System.Threading
open System.Threading.Tasks
open System.Runtime.InteropServices
open IcedTasks

open Migrondi.Core
open Migrondi.Core.FileSystem

open JDeck
open MigrondiUI.Projects
open Microsoft.Extensions.Logging
open FsToolkit.ErrorHandling

[<Literal>]
let MigrationNameSchema: string = "^(?<Timestamp>[0-9]+)_(?<Name>.+).(sql|SQL)$"

[<return: Struct>]
let (|HasGroup|_|) (name: string) (groups: Match) =
  if not groups.Success then
    ValueNone
  else
    match groups.Groups[name] with
    | group when group.Length = 0 -> ValueNone
    | group -> ValueSome group.Value

let mnRegex = lazy Regex(MigrationNameSchema)

let extractTimestampAndName(name: string) =
  let groups = mnRegex.Value.Match(name)

  match groups with
  | HasGroup "Timestamp" timestamp & HasGroup "Name" name ->
    let timestamp = Int64.Parse timestamp
    let name = name.Trim()

    if String.IsNullOrWhiteSpace name then
      failwith $"Invalid migration name %s{name}"

    struct (timestamp, name)
  | _ -> failwith $"Invalid migration name %s{name}"

let internal migrationDelimiter(key: string, value: string option) =
  let start =
    match value with
    | Some _ -> "-- "
    | None -> "-- ---------- "

  let value =
    match value with
    | Some value -> $"={value}"
    | None -> " ----------"

  $"{start}MIGRONDI:%s{key}{value}"

let internal encodeMigrationText(migration: Migration) : string =
  let sb = StringBuilder()
  let name = migrationDelimiter("NAME", Some migration.name)

  let timestamp =
    migrationDelimiter("TIMESTAMP", Some(migration.timestamp.ToString()))

  let manualTransaction =
    if migration.manualTransaction then
      migrationDelimiter(
        "ManualTransaction",
        Some(migration.manualTransaction.ToString())
      )
      + "\n"
    else
      ""

  let up = migrationDelimiter("UP", None)
  let down = migrationDelimiter("DOWN", None)

  sb
    .Append(name)
    .Append('\n')
    .Append(timestamp)
    .Append('\n')
    .Append(manualTransaction)
    .Append(up)
    .Append('\n')
    .Append(migration.upContent)
    .Append('\n')
    .Append(down)
    .Append('\n')
    .Append(migration.downContent)
    .Append('\n')
    .ToString()

let internal decodeMigrationText(content: string) : Result<Migration, string> = result {
  let upDownMatcher =
    Regex(
      "-- ---------- MIGRONDI:(?<Identifier>UP|DOWN) ----------",
      RegexOptions.Multiline
    )

  let metadataMatcher =
    Regex(
      "-- MIGRONDI:(?<Key>[a-zA-Z0-9_-]+)=(?<Value>[a-zA-Z0-9_-]+)",
      RegexOptions.Multiline
    )

  let upDownCollection = upDownMatcher.Matches(content)
  let metadataCollection = metadataMatcher.Matches(content)

  let! name =
    metadataCollection
    |> Seq.tryFind(fun value ->
      System.String.Equals(
        value.Groups["Key"].Value,
        "NAME",
        StringComparison.OrdinalIgnoreCase
      ))
    |> Option.map(fun v -> v.Groups["Value"].Value)
    |> Result.requireSome "Missing Migration Name In metadata"

  let! timestamp =
    metadataCollection
    |> Seq.tryFind(fun value ->
      System.String.Equals(
        value.Groups["Key"].Value,
        "TIMESTAMP",
        StringComparison.OrdinalIgnoreCase
      ))
    |> Option.map(fun v -> v.Groups["Value"].Value)
    |> Result.requireSome "Missing Migration Timestamp In metadata"
    |> Result.bind(fun value ->
      try
        int64 value |> Ok
      with ex ->
        Error $"Invalid timestamp: {ex.Message}")

  let manualTransaction =
    metadataCollection
    |> Seq.tryFind(fun value ->
      System.String.Equals(
        value.Groups["Key"].Value,
        "ManualTransaction",
        StringComparison.OrdinalIgnoreCase
      ))
    |> Option.map(fun v ->
      let value = v.Groups["Value"].Value

      match value.ToLowerInvariant() with
      | "true" -> true
      | _ -> false)
    |> Option.defaultValue false

  do!
    upDownCollection.Count = 2
    |> Result.requireTrue "Invalid Migrations Format"
    |> Result.ignore

  let upIndex =
    upDownCollection
    |> Seq.find(fun value -> value.Groups["Identifier"].Value = "UP")
    |> _.Index

  let downIndex =
    upDownCollection
    |> Seq.find(fun value -> value.Groups["Identifier"].Value = "DOWN")
    |> _.Index

  let slicedUp = content[upIndex .. downIndex - 1]
  let fromUp = slicedUp.IndexOf('\n') + 1
  let slicedDown = content[downIndex..]
  let fromDown = content.Substring(downIndex).IndexOf('\n') + 1

  return {
    name = name
    upContent = slicedUp[fromUp..].Trim()
    downContent = slicedDown[fromDown..].Trim()
    timestamp = timestamp
    manualTransaction = manualTransaction
  }
}

type MigrondiUIFs =
  inherit IMiMigrationSource

  abstract member ExportToLocal:
    project: Guid * projectPath: string -> CancellableTask<string>

  abstract member ImportFromLocal:
    project: LocalProject -> CancellableTask<Guid>

  abstract member ImportFromLocal: projectPath: string -> CancellableTask<Guid>

let getVirtualFs
  (logger: ILogger<MigrondiUIFs>, vpr: IVirtualProjectRepository)
  =
  let importProjectFromPath (configPath: string) (projectName: string) = cancellableTask {
    let rootDir = Path.GetDirectoryName(configPath)

    logger.LogInformation(
      "Importing project {projectName} from {rootDir}",
      projectName,
      rootDir
    )

    let! config = cancellableTask {
      let! token = CancellableTask.getCancellationToken()
      let! content = File.ReadAllTextAsync(configPath, token)

      return
        Decoding.fromString(content, MigrondiUI.Json.migrondiConfigDecoder)
        |> function
          | Ok c -> c
          | Error e -> failwith $"Failed to decode config: %A{e}"
    }

    logger.LogInformation("Found config {config}", config)

    let migrationsDir =
      DirectoryInfo(Path.Combine(nonNull rootDir, config.migrations))

    let! migrations =
      migrationsDir.EnumerateFiles("*.sql", SearchOption.TopDirectoryOnly)
      |> Seq.map(fun f -> asyncEx {
        let! token = Async.CancellationToken
        let! content = File.ReadAllTextAsync(f.FullName, token)
        logger.LogDebug("Found migration {migrationPath}", f.FullName)

        return
          decodeMigrationText content
          |> function
            | Ok m -> m
            | Error e ->
              failwith $"Failed to decode migration %s{f.Name}: %s{e}"
      })
      |> Async.Parallel

    logger.LogInformation("Found migrations {migrations}", migrations.Length)

    logger.LogInformation(
      "Creating new virtual project {projectName}",
      projectName
    )

    let! vProjectId =
      vpr.InsertProject {
        name = projectName
        description = $"Imported from local ({projectName})"
        connection = config.connection
        tableName = config.tableName
        driver = config.driver
      }

    let! migrations =
      migrations
      |> Array.map(fun migration -> asyncEx {
        let! token = Async.CancellationToken

        let vMigration = {
          id = Guid.NewGuid()
          name = migration.name
          timestamp = migration.timestamp
          upContent = migration.upContent
          downContent = migration.downContent
          manualTransaction = migration.manualTransaction
          projectId = vProjectId
        }

        logger.LogDebug(
          "Creating virtual migration {vMigration}",
          vMigration.name
        )

        return! vpr.InsertMigration vMigration token
      })
      |> Async.Parallel

    logger.LogInformation(
      "Created virtual project {projectName} with {migrations} migrations",
      projectName,
      migrations.Length
    )

    return vProjectId
  }

  { new MigrondiUIFs with
      member this.ReadContent(uri: Uri) =
        failwith
          "Synchronous read is not supported. Use ReadContentAsync instead."

      member this.ReadContentAsync(uri, [<Optional>] ?cancellationToken) = task {
        let ct = defaultArg cancellationToken CancellationToken.None

        logger.LogDebug("Reading content for {uri}", uri)

        // migrondi-ui://projects/{projectId}/config
        // migrondi-ui://projects/{projectId}/migrations/{migrationName}
        match uri.Scheme, uri.Host, uri.Segments with
        | "migrondi-ui", "projects", [| "/"; projectIdStr; "config" |] ->
          let projectId = Guid.Parse(projectIdStr.TrimEnd('/'))
          let! project = vpr.GetProjectById projectId ct

          match project with
          | None -> return failwith $"Project {projectId} not found"
          | Some p ->
            let config = p.ToMigrondiConfig()
            let node = MigrondiUI.Json.migrondiConfigEncoder config

            let options =
              System.Text.Json.JsonSerializerOptions(WriteIndented = true)

            return node.ToJsonString(options)

        | "migrondi-ui",
          "projects",
          [| "/"; projectIdStr; "migrations/"; migrationName |] ->
          let! migration = vpr.GetMigrationByName migrationName ct

          match migration with
          | None -> return failwith $"Migration {migrationName} not found"
          | Some m -> return encodeMigrationText(m.ToMigration())

        | _ -> return failwith $"Unsupported URI: {uri}"
      }

      member this.WriteContent(uri, content) =
        failwith
          "Synchronous write is not supported. Use WriteContentAsync instead."

      member this.WriteContentAsync
        (uri, content, [<Optional>] ?cancellationToken)
        =
        task {
          let ct = defaultArg cancellationToken CancellationToken.None

          logger.LogDebug("Writing content to {uri}", uri)

          match uri.Scheme, uri.Host, uri.Segments with
          | "migrondi-ui", "projects", [| "/"; projectIdStr; "config" |] ->
            let projectId = Guid.Parse(projectIdStr.TrimEnd('/'))
            let! project = vpr.GetProjectById projectId ct

            match project with
            | None -> return failwith $"Project {projectId} not found"
            | Some p ->
              let config =
                Decoding.fromString(
                  content,
                  MigrondiUI.Json.migrondiConfigDecoder
                )
                |> function
                  | Ok c -> c
                  | Error e -> failwith $"Failed to decode config: %A{e}"

              let updatedProject = {
                p with
                    connection = config.connection
                    tableName = config.tableName
                    driver = config.driver
              }

              return! vpr.UpdateProject updatedProject ct

          | "migrondi-ui",
            "projects",
            [| "/"; projectIdStr; "migrations/"; migrationName |] ->
            let projectId = Guid.Parse(projectIdStr.TrimEnd('/'))

            let migration =
              decodeMigrationText content
              |> function
                | Ok m -> m
                | Error e -> failwith $"Failed to decode migration: %s{e}"

            let virtualMigration: VirtualMigration = {
              id = Guid.NewGuid()
              name = migration.name
              timestamp = migration.timestamp
              upContent = migration.upContent
              downContent = migration.downContent
              projectId = projectId
              manualTransaction = migration.manualTransaction
            }

            let! existing = vpr.GetMigrationByName migrationName ct

            match existing with
            | Some _ -> return! vpr.UpdateMigration virtualMigration ct
            | None ->
              let! _ = vpr.InsertMigration virtualMigration ct
              return ()

          | _ -> return failwith $"Unsupported URI: {uri}"
        }

      member this.ListFiles(locationUri) =
        failwith
          "Synchronous listing is not supported. Use ListFilesAsync instead."

      member this.ListFilesAsync(locationUri, [<Optional>] ?cancellationToken) = task {
        let ct = defaultArg cancellationToken CancellationToken.None

        logger.LogDebug("Listing files in {uri}", locationUri)

        match locationUri.Scheme, locationUri.Host, locationUri.Segments with
        | "migrondi-ui", "projects", [| "/"; projectIdStr; "migrations/" |] ->
          let projectId = Guid.Parse(projectIdStr.TrimEnd('/'))
          let! migrations = vpr.GetMigrations projectId ct

          return
            migrations
            |> List.map(fun m ->
              Uri(locationUri, $"{m.timestamp}_{m.name}.sql"))
            :> Uri seq

        | _ -> return failwith $"Unsupported URI: {locationUri}"
      }

      member _.ExportToLocal(project, path) = cancellableTask {
        let! token = CancellableTask.getCancellationToken()

        logger.LogInformation(
          "Exporting project {project} to {path}",
          project,
          path
        )

        let! found = taskOption {
          let! project = vpr.GetProjectById project token
          let! migrations = vpr.GetMigrations project.id token
          return project, migrations
        }

        match found with
        | None -> return failwith $"Project with id {project} not found"
        | Some(project, migrations) ->
          let config = {
            project.ToMigrondiConfig() with
                migrations = "./migrations"
          }

          let projectRoot = Directory.CreateDirectory(path)
          let migrationsDir = projectRoot.CreateSubdirectory("migrations")

          let configPath = Path.Combine(projectRoot.FullName, "migrondi.json")

          let configContent: string =
            let node = MigrondiUI.Json.migrondiConfigEncoder config

            let options =
              System.Text.Json.JsonSerializerOptions(WriteIndented = true)

            node.ToJsonString(options)

          do! File.WriteAllTextAsync(configPath, configContent, token)

          do!
            migrations
            |> List.map(fun vm -> asyncEx {
              let! token = Async.CancellationToken
              let content = encodeMigrationText(vm.ToMigration())

              let migrationPath =
                Path.Combine(
                  migrationsDir.FullName,
                  $"{vm.timestamp}_{vm.name}.sql"
                )

              do! File.WriteAllTextAsync(migrationPath, content, token)
            })
            |> Async.Parallel
            |> Async.Ignore

          return projectRoot.FullName
      }

      member _.ImportFromLocal(project: LocalProject) : CancellableTask<Guid> =
        importProjectFromPath project.migrondiConfigPath project.name

      member _.ImportFromLocal(projectConfigPath: string) =
        let projectName = Path.GetDirectoryName projectConfigPath |> nonNull
        importProjectFromPath projectConfigPath projectName
  }
