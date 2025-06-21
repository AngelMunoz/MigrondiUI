module MigrondiUI.Projects

open System
open System.IO

open IcedTasks

open FsToolkit.ErrorHandling
open FSharp.UMX

open Migrondi.Core
open MigrondiUI.Database

type NewVirtualProjectArgs = {
  name: string
  description: string
  connection: string
  tableName: string
  driver: MigrondiDriver
}

type IVirtualProjectRepository =
  abstract member GetProjects: unit -> CancellableTask<VirtualProject list>

  abstract member GetProjectByProjectId:
    Guid<ProjectId> -> CancellableTask<VirtualProject option>

  abstract member GetProjectById:
    Guid<VProjectId> -> CancellableTask<VirtualProject option>

  abstract member InsertProject:
    NewVirtualProjectArgs -> CancellableTask<Guid<VProjectId>>

  abstract member UpdateProject: VirtualProject -> CancellableTask<unit>

  abstract member RemoveProjects: Project seq -> CancellableTask<unit>

  abstract member InsertMigration: VirtualMigration -> CancellableTask<Guid>
  abstract member UpdateMigration: VirtualMigration -> CancellableTask<unit>
  abstract member RemoveMigrationByName: string -> CancellableTask<unit>

  abstract member GetMigrations:
    Guid<ProjectId> -> CancellableTask<VirtualMigration list>


  abstract member GetMigrationByName:
    string -> CancellableTask<VirtualMigration option>

type ILocalProjectRepository =
  abstract member GetProjects: unit -> CancellableTask<LocalProject list>

  abstract member GetProjectByProjectId:
    Guid<ProjectId> -> CancellableTask<LocalProject option>

  abstract member GetProjectById:
    Guid<LProjectId> -> CancellableTask<LocalProject option>

  /// <summary>
  /// Inserts a local project into the database.
  /// </summary>
  /// <param name="name">The name of the project.</param>
  /// <param name="configPath">The path to the project configuration file. (migrondi.json)</param>
  /// <param name="description">An optional description of the project.</param>
  /// <returns>The ID of the inserted project.</returns>
  abstract member InsertProject:
    name: string * configPath: string * ?description: string ->
      CancellableTask<Guid<LProjectId>>

  abstract member UpdateProject: LocalProject -> CancellableTask<unit>

  abstract member UpdateProjectConfigPath:
    id: Guid<ProjectId> * path: string -> CancellableTask<unit>

  abstract member RemoveProjects: Project seq -> CancellableTask<unit>

let GetLocalProjectRepository createDbConnection =
  let readConfig(path: string) = option {
    try

      let json = File.ReadAllText path

      return!
        JDeck.Decoding.fromString(json, Json.migrondiConfigDecoder)
        |> Result.toOption
    with :? FileNotFoundException ->
      return! None
  }

  let findLocalProjects = FindLocalProjects(readConfig, createDbConnection)

  let findLocalProjectByProjectId =
    FindLocalProjectByProjectId(readConfig, createDbConnection)

  let findLocalProjectById =
    FindLocalProjectById(readConfig, createDbConnection)

  let insertLocalProject = InsertLocalProject createDbConnection
  let updateProject = UpdateProject createDbConnection

  let updateLocalProjectConfigPath =
    UpdateLocalProjectConfigPath createDbConnection

  let batchDeleteProjects = BatchDeleteProjects createDbConnection

  { new ILocalProjectRepository with

      member _.GetProjectByProjectId projectId =
        findLocalProjectByProjectId projectId

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
          id = UMX.tag<ProjectId> project.projectId
          name = project.name
          description = project.description
        }

      member _.UpdateProjectConfigPath(id, path) =
        updateLocalProjectConfigPath(id, path)

      member _.RemoveProjects projects =
        batchDeleteProjects(
          projects |> Seq.map(_.ProjectId >> UMX.tag<ProjectId>)
        )
  }

let GetVirtualProjectRepository createDbConnection =
  let findVirtualProjects = FindVirtualProjects createDbConnection

  let findVirtualProjectByProjectId =
    FindVirtualProjectByProjectId createDbConnection

  let insertVirtualProject = InsertVirtualProject createDbConnection
  let updateVirtualProject = UpdateVirtualProject createDbConnection
  let updateProject = UpdateProject createDbConnection

  let findVirtualMigrationByName = FindVirtualMigrationByName createDbConnection

  let findVirtualMigrationsByProjectId =
    FindVirtualMigrationsByProjectId createDbConnection

  let findVirtualProjectById = FindVirtualProjectById createDbConnection

  let insertVirtualMigration = InsertVirtualMigration createDbConnection

  let updateVirtualMigration = UpdateVirtualMigration createDbConnection

  let removeVirtualMigrationByName =
    RemoveVirtualMigrationByName createDbConnection

  let batchDeleteProjects = BatchDeleteProjects createDbConnection

  { new IVirtualProjectRepository with
      member _.GetProjects() = findVirtualProjects()

      member _.GetProjectByProjectId projectId =
        findVirtualProjectByProjectId projectId

      member _.GetProjectById projectId = findVirtualProjectById projectId

      member _.InsertProject project =
        insertVirtualProject {
          name = project.name
          description = project.description |> Some
          connection = project.connection
          tableName = project.tableName
          driver = project.driver.AsString
        }

      member _.UpdateProject project = cancellableTask {
        // First update the base project information
        do!
          updateProject {
            id = UMX.tag<ProjectId> project.projectId
            name = project.name
            description = project.description
          }

        return!
          updateVirtualProject {
            id = UMX.tag<VProjectId> project.id
            connection = project.connection
            tableName = project.tableName
            driver = project.driver.AsString
          }
      }

      member _.InsertMigration migration =
        insertVirtualMigration {
          name = migration.name
          timestamp = migration.timestamp
          upContent = migration.upContent
          downContent = migration.downContent
          virtualProjectId = migration.projectId
          manualTransaction = migration.manualTransaction
        }

      member _.UpdateMigration migration =
        updateVirtualMigration {
          name = migration.name
          upContent = migration.upContent
          downContent = migration.downContent
          manualTransaction = migration.manualTransaction
        }

      member _.RemoveMigrationByName migrationName =
        removeVirtualMigrationByName migrationName

      member _.GetMigrations projectId =
        findVirtualMigrationsByProjectId projectId

      member _.GetMigrationByName name = findVirtualMigrationByName name

      member _.RemoveProjects projects =
        batchDeleteProjects(
          projects |> Seq.map(_.ProjectId >> UMX.tag<ProjectId>)
        )
  }

let inline GetRepositories createDbConnection =
  GetLocalProjectRepository createDbConnection,
  GetVirtualProjectRepository createDbConnection
