module MigrondiUI.Projects

open System
open System.IO

open FsToolkit.ErrorHandling
open Migrondi.Core

open IcedTasks
open MigrondiUI.Database

type IVirtualProjectRepository =
  abstract member GetProjects: unit -> CancellableTask<VirtualProject list>
  abstract member GetProjectById: Guid -> CancellableTask<VirtualProject option>

  abstract member InsertProject: VirtualProject -> CancellableTask<unit>
  abstract member UpdateProject: VirtualProject -> CancellableTask<unit>

  abstract member InsertMigration: VirtualMigration -> CancellableTask<unit>
  abstract member UpdateMigration: VirtualMigration -> CancellableTask<unit>
  abstract member RemoveMigration: Guid -> CancellableTask<unit>

  abstract member GetMigrations: Guid -> CancellableTask<VirtualMigration list>


  abstract member GetMigrationByName:
    string -> CancellableTask<VirtualMigration option>

type ILocalProjectRepository =
  abstract member GetProjects: unit -> CancellableTask<LocalProject list>
  abstract member GetProjectById: Guid -> CancellableTask<LocalProject option>

  /// <summary>
  /// Inserts a local project into the database.
  /// </summary>
  /// <param name="name">The name of the project.</param>
  /// <param name="configPath">The path to the project configuration file. (migrondi.json)</param>
  /// <param name="description">An optional description of the project.</param>
  /// <returns>The ID of the inserted project.</returns>
  abstract member InsertProject:
    name: string * configPath: string * ?description: string ->
      CancellableTask<Guid>

  abstract member UpdateProject: LocalProject -> CancellableTask<unit>

  abstract member UpdateProjectConfigPath:
    id: Guid * path: string -> CancellableTask<unit>

let GetRepository createDbConnection =
  let readConfig(path: string) = option {
    try

      let json = File.ReadAllText path

      return!
        JDeck.Decoding.fromString(json, Decoders.migrondiConfigDecoder)
        |> Result.toOption
    with :? FileNotFoundException ->
      return! None
  }

  let findLocalProjects =
    Database.FindLocalProjects(readConfig, createDbConnection)

  let findLocalProjectById =
    Database.FindLocalProjectById(readConfig, createDbConnection)

  let insertLocalProject = Database.InsertLocalProject createDbConnection
  let updateProject = Database.UpdateProject createDbConnection

  let updateLocalProjectConfigPath =
    Database.UpdateLocalProjectConfigPath createDbConnection

  { new ILocalProjectRepository with

      member _.GetProjectById projectId = findLocalProjectById projectId

      member _.GetProjects() = findLocalProjects()

      member _.InsertProject(name, path, description) =
        insertLocalProject {
          name = name
          description = description
          configPath = path
        }

      member _.UpdateProject project =
        updateProject {
          id = project.id
          name = project.name
          description = project.description
        }

      member _.UpdateProjectConfigPath(id, path) =
        updateLocalProjectConfigPath(id, path)
  }
