module MigrondiUI.VirtualFs

open System
open System.IO
open System.Threading
open System.Threading.Tasks
open System.Runtime.InteropServices
open IcedTasks

open Migrondi.Core
open Migrondi.Core.FileSystem
open Migrondi.Core.Serialization

open FsToolkit.ErrorHandling

open MigrondiUI.Projects
open Microsoft.Extensions.Logging

type MigrondiUIFs =
  inherit IMiMigrationSource

  abstract member ExportToLocal:
    project: Guid * projectPath: string -> CancellableTask<string>

  abstract member ImportFromLocal:
    project: LocalProject -> CancellableTask<Guid>

  abstract member ImportFromLocal: projectPath: string -> CancellableTask<Guid>

type VirtualProjectResource =
  | ProjectConfig of projectId: Guid
  | Migration of projectId: Guid * migrationName: string
  | MigrationList of projectId: Guid

type VirtualFsContext =
  { logger: ILogger<MigrondiUIFs>
    vpr: IVirtualProjectRepository }

let parseVirtualProjectUri (uri: Uri) : VirtualProjectResource option =
  if uri.Scheme <> "migrondi-ui" then None
  else
    let segments = uri.Segments |> Array.map (fun s -> s.TrimEnd('/'))

    match segments with
    | [| ""; "virtual"; projectId; "config" |] ->
      match Guid.TryParse projectId with
      | true, id -> Some(ProjectConfig id)
      | _ -> None

    | [| ""; "virtual"; projectId |] ->
      match Guid.TryParse projectId with
      | true, id -> Some(MigrationList id)
      | _ -> None

    | [| ""; "virtual"; projectId; "migrations" |] ->
      match Guid.TryParse projectId with
      | true, id -> Some(MigrationList id)
      | _ -> None

    | [| ""; "virtual"; projectId; "migrations"; migrationName |] ->
      match Guid.TryParse projectId with
      | true, id -> Some(Migration(id, migrationName))
      | _ -> None

    | [| ""; "virtual"; projectId; migrationName |] ->
      match Guid.TryParse projectId with
      | true, id when not (String.IsNullOrEmpty migrationName) ->
        Some(Migration(id, migrationName))
      | _ -> None

    | _ -> None

module Operations =

  let readProjectConfig
    (ctx: VirtualFsContext)
    (projectId: Guid)
    (ct: CancellationToken)
    =
    task {
      let! project = ctx.vpr.GetProjectById projectId ct

      match project with
      | None ->
        ctx.logger.LogWarning("Project {projectId} not found", projectId)
        return None
      | Some p ->
        let config = p.ToMigrondiConfig()
        ctx.logger.LogDebug("Read config for project {projectId}", projectId)
        return Some(MiSerializer.Encode config)
    }

  let readMigrationContent
    (ctx: VirtualFsContext)
    (migrationName: string)
    (ct: CancellationToken)
    =
    task {
      let! migration = ctx.vpr.GetMigrationByName migrationName ct

      match migration with
      | None ->
        ctx.logger.LogWarning("Migration {migrationName} not found", migrationName)
        return None
      | Some m ->
        ctx.logger.LogDebug("Read migration {migrationName}", migrationName)
        return Some(MiSerializer.Encode(m.ToMigration()))
    }

  let writeProjectConfig
    (ctx: VirtualFsContext)
    (projectId: Guid)
    (content: string)
    (ct: CancellationToken)
    =
    task {
      let! project = ctx.vpr.GetProjectById projectId ct

      match project with
      | None ->
        ctx.logger.LogWarning("Project {projectId} not found for write", projectId)
        return None
      | Some p ->
        let config = MiSerializer.DecodeConfig content

        let updatedProject = {
          p with
              connection = config.connection
              tableName = config.tableName
              driver = config.driver
        }

        do! ctx.vpr.UpdateProject updatedProject ct
        ctx.logger.LogDebug("Wrote config for project {projectId}", projectId)
        return Some()
    }

  let writeMigrationContent
    (ctx: VirtualFsContext)
    (projectId: Guid)
    (migrationName: string)
    (content: string)
    (ct: CancellationToken)
    =
    task {
      let migration = MiSerializer.Decode(content, migrationName)

      let virtualMigration: VirtualMigration = {
        id = Guid.NewGuid()
        name = migration.name
        timestamp = migration.timestamp
        upContent = migration.upContent
        downContent = migration.downContent
        projectId = projectId
        manualTransaction = migration.manualTransaction
      }

      let! existing = ctx.vpr.GetMigrationByName migration.name ct

      match existing with
      | Some _ ->
        do! ctx.vpr.UpdateMigration virtualMigration ct
        ctx.logger.LogDebug("Updated migration {name}", migration.name)
      | None ->
        let! _ = ctx.vpr.InsertMigration virtualMigration ct
        ctx.logger.LogDebug("Inserted migration {name}", migration.name)

      return Some()
    }

  let listMigrations
    (ctx: VirtualFsContext)
    (projectId: Guid)
    (baseUri: Uri)
    (ct: CancellationToken)
    =
    task {
      let! migrations = ctx.vpr.GetMigrations projectId ct

      ctx.logger.LogDebug(
        "Listed {count} migrations for project {projectId}",
        migrations.Length,
        projectId
      )

      let baseUri =
        if baseUri.AbsolutePath.EndsWith "/" then baseUri
        else Uri($"{baseUri.AbsoluteUri}/")

      return
        migrations
        |> List.map(fun m -> Uri(baseUri, m.name))
        :> Uri seq
    }

  let exportProjectToLocal
    (ctx: VirtualFsContext)
    (projectId: Guid)
    (path: string)
    =
    cancellableTask {
      let! token = CancellableTask.getCancellationToken()

      ctx.logger.LogInformation(
        "Exporting project {projectId} to {path}",
        projectId,
        path
      )

      let! found = taskOption {
        let! project = ctx.vpr.GetProjectById projectId token
        let! migrations = ctx.vpr.GetMigrations project.id token
        return project, migrations
      }

      match found with
      | None ->
        ctx.logger.LogWarning("Project {projectId} not found for export", projectId)
        return failwith $"Project with id {projectId} not found"
      | Some(project: VirtualProject, migrations: VirtualMigration list) ->
        let config = {
          project.ToMigrondiConfig() with
              migrations = "./migrations"
        }

        let projectRoot = Directory.CreateDirectory(path)
        let migrationsDir = projectRoot.CreateSubdirectory("migrations")

        let configPath = Path.Combine(projectRoot.FullName, "migrondi.json")

        let configContent = MiSerializer.Encode config

        do! File.WriteAllTextAsync(configPath, configContent, token)

        do!
          migrations
          |> List.map(fun (vm: VirtualMigration) -> asyncEx {
            let! token = Async.CancellationToken
            let content: string = MiSerializer.Encode(vm.ToMigration())

            let migrationPath =
              Path.Combine(
                migrationsDir.FullName,
                $"{vm.timestamp}_{vm.name}.sql"
              )

            do! File.WriteAllTextAsync(migrationPath, content, token)
          })
          |> Async.Parallel
          |> Async.Ignore

        ctx.logger.LogInformation(
          "Exported project {projectId} to {path}",
          projectId,
          projectRoot.FullName
        )

        return projectRoot.FullName
    }

  let importProjectFromPath
    (ctx: VirtualFsContext)
    (configPath: string)
    (projectName: string)
    =
    cancellableTask {
      let rootDir = Path.GetDirectoryName(configPath)

      ctx.logger.LogInformation(
        "Importing project {projectName} from {rootDir}",
        projectName,
        rootDir
      )

      let! config = cancellableTask {
        let! token = CancellableTask.getCancellationToken()
        let! content = File.ReadAllTextAsync(configPath, token)

        return MiSerializer.DecodeConfig content
      }

      ctx.logger.LogDebug("Found config {config}", config)

      let migrationsDir =
        DirectoryInfo(Path.Combine(nonNull rootDir, config.migrations))

      let! migrations =
        migrationsDir.EnumerateFiles("*.sql", SearchOption.TopDirectoryOnly)
        |> Seq.map(fun f -> asyncEx {
          let! token = Async.CancellationToken
          let! content = File.ReadAllTextAsync(f.FullName, token)
          ctx.logger.LogDebug("Found migration {migrationPath}", f.FullName)

          return MiSerializer.Decode(content, f.Name)
        })
        |> Async.Parallel

      ctx.logger.LogDebug("Found {count} migrations", migrations.Length)

      let! vProjectId =
        ctx.vpr.InsertProject {
          name = projectName
          description = $"Imported from local ({projectName})"
          connection = config.connection
          tableName = config.tableName
          driver = config.driver
        }

      let! _ =
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

          ctx.logger.LogDebug("Creating virtual migration {name}", vMigration.name)

          return! ctx.vpr.InsertMigration vMigration token
        })
        |> Async.Parallel

      ctx.logger.LogInformation(
        "Created virtual project {projectName} with {count} migrations",
        projectName,
        migrations.Length
      )

      return vProjectId
    }

let getVirtualFs
  (logger: ILogger<MigrondiUIFs>, vpr: IVirtualProjectRepository)
  =
  let ctx = { logger = logger; vpr = vpr }

  { new MigrondiUIFs with
      member _.ReadContent(uri: Uri) =
        failwith "Synchronous read is not supported. Use ReadContentAsync instead."

      member _.ReadContentAsync(uri, [<Optional>] ?cancellationToken) = task {
        let ct = defaultArg cancellationToken CancellationToken.None
        logger.LogDebug("Reading content for {uri}", uri)

        match parseVirtualProjectUri uri with
        | Some(ProjectConfig projectId) ->
          let! result = Operations.readProjectConfig ctx projectId ct
          return result |> Option.defaultWith (fun () -> failwith $"Project {projectId} not found")

        | Some(Migration(_, migrationName)) ->
          let! result = Operations.readMigrationContent ctx migrationName ct
          return result |> Option.defaultWith (fun () -> failwith $"Migration {migrationName} not found")

        | Some(MigrationList _) ->
          return failwith "Cannot read content from migration list URI"

        | None -> return failwith $"Unsupported URI: {uri}"
      }

      member _.WriteContent(uri, content) =
        failwith "Synchronous write is not supported. Use WriteContentAsync instead."

      member _.WriteContentAsync(uri, content, [<Optional>] ?cancellationToken) = task {
        let ct = defaultArg cancellationToken CancellationToken.None
        logger.LogDebug("Writing content to {uri}", uri)

        match parseVirtualProjectUri uri with
        | Some(ProjectConfig projectId) ->
          let! result = Operations.writeProjectConfig ctx projectId content ct
          return result |> Option.defaultWith (fun () -> failwith $"Project {projectId} not found")

        | Some(Migration(projectId, migrationName)) ->
          let! result = Operations.writeMigrationContent ctx projectId migrationName content ct
          return result |> Option.defaultWith (fun () -> failwith $"Failed to write migration {migrationName}")

        | Some(MigrationList _) ->
          return failwith "Cannot write content to migration list URI"

        | None -> return failwith $"Unsupported URI: {uri}"
      }

      member _.ListFiles(locationUri) =
        failwith "Synchronous listing is not supported. Use ListFilesAsync instead."

      member _.ListFilesAsync(locationUri, [<Optional>] ?cancellationToken) = task {
        let ct = defaultArg cancellationToken CancellationToken.None
        logger.LogDebug("Listing files in {uri}", locationUri)

        match parseVirtualProjectUri locationUri with
        | Some(MigrationList projectId) ->
          return! Operations.listMigrations ctx projectId locationUri ct

        | Some(ProjectConfig _) ->
          return failwith "Cannot list files from config URI"

        | Some(Migration _) ->
          return failwith "Cannot list files from single migration URI"

        | None -> return failwith $"Unsupported URI: {locationUri}"
      }

      member _.ExportToLocal(project, path) =
        Operations.exportProjectToLocal ctx project path

      member _.ImportFromLocal(project: LocalProject) =
        Operations.importProjectFromPath ctx project.migrondiConfigPath project.name

      member _.ImportFromLocal(projectConfigPath: string) =
        let projectName = Path.GetDirectoryName projectConfigPath |> nonNull
        Operations.importProjectFromPath ctx projectConfigPath projectName
  }
