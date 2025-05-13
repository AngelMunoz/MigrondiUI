namespace MigrondiUI

open System
open System.Data
open System.Threading
open Donald

module Queries =

  let inline sql(value: string) = value

  [<Literal>]
  let GetLocalProjects =
    """
      select
        p.id as id, p.name as name, p.description as description, lp.config_path as config_path
      from projects as p
      left join local_projects as lp
        on
        lp.project_id = p.id;
      """

  [<Literal>]
  let GetLocalProjectById =
    """
    select
      p.id as id, p.name as name, p.description as description, lp.config_path as config_path
    from projects as p
    left join local_projects as lp
      on
      lp.project_id = p.id
    where p.id = @id;
    """

  [<Literal>]
  let InsertProject =
    """
    insert into projects (id, name, description)
    values (@id, @name, @description)
    """

  [<Literal>]
  let InsertLocalProject =
    """
    insert into local_projects (id, config_path, project_id)
    values (@id, @config_path, @project_id);
    """

  [<Literal>]
  let InsertVirtualProject =
    """
    insert into virtual_projects (id, connection, table_name, driver, project_id)
    values (@id, @connection, @table_name, @driver, @project_id);
    """

  [<Literal>]
  let UpdateProjectQuery =
    """
    update projects
    set
      name = coalesce(@name, name),
      description = coalesce(@description, description)
    where id = @id;
    """

  [<Literal>]
  let UpdateLocalProjectConfigPathQuery =
    """
    update local_projects
    set
      config_path = coalesce(@config_path, config_path)
    where project_id = @project_id;
    """

  [<Literal>]
  let GetVirtualProjects =
    """
    select
      p.id as id, p.name as name, p.description as description,
      vp.connection as connection, vp.table_name as table_name, vp.driver as driver
    from projects as p
    left join virtual_projects as vp
      on vp.project_id = p.id;
    """

  [<Literal>]
  let GetVirtualProjectById =
    """
    select
      p.id as id, p.name as name, p.description as description,
      vp.connection as connection, vp.table_name as table_name, vp.driver as driver
    from projects as p
    left join virtual_projects as vp
      on vp.project_id = p.id
    where p.id = @id;
    """

  [<Literal>]
  let UpdateVirtualProjectQuery =
    """
    update virtual_projects
    set
      connection = coalesce(@connection, connection),
      table_name = coalesce(@table_name, table_name),
      driver = coalesce(@driver, driver)
    where project_id = @project_id;
    """

  [<Literal>]
  let FindByVirtualMigrationName =
    """
    select
      vm.id as id,
      vm.name as name,
      vm.timestamp as timestamp,
      vm.up_content as upContent,
      vm.down_content as downContent,
      vm.virtual_project_id as virtualProjectId,
      vm.manual_transaction as manualTransaction
    from virtual_migrations as vm
    where vm.name = @name;
    """

  [<Literal>]
  let FindVirtualMigrationsByProjectId =
    """
    select
      vm.id as id,
      vm.name as name,
      vm.timestamp as timestamp,
      vm.up_content as upContent,
      vm.down_content as downContent,
      vm.virtual_project_id as virtualProjectId,
      vm.manual_transaction as manualTransaction
    from virtual_migrations as vm
    inner join virtual_projects as vp on vm.virtual_project_id = vp.id
    where vp.project_id = @projectId;
    """

  [<Literal>]
  let UpdateVirtualMigrationByName =
    """
    update virtual_migrations
    set
      up_content = coalesce(@up_content, up_content),
      down_content = coalesce(@down_content, down_content),
      manual_transaction = coalesce(@manual_transaction, manual_transaction)
    where name = @name;
    """

module Mappers =
  let mapLocalProject readConfig (r: IDataReader) =
    let id = r.ReadGuid "id"
    let name = r.ReadString "name"
    let description = r.ReadStringOption "description"
    let configPath = r.ReadString "config_path"
    let config = readConfig configPath

    {
      id = id
      name = name
      description = description
      config = config
      migrondiConfigPath = configPath
    }

module Database =
  open Microsoft.Data.Sqlite

  let inline setCancellationToken
    (cancellationToken: CancellationToken option)
    (dbUnit: DbUnit)
    =
    match cancellationToken with
    | Some token -> Db.setCancellationToken token dbUnit
    | None -> dbUnit

  let ConnectionFactory() : IDbConnection =
    let dbPath = System.IO.Path.Combine(AppContext.BaseDirectory, "migrondi.db")
    let connectionString = $"Data Source={dbPath};"
    new SqliteConnection(connectionString)

  let FindLocalProjects(readConfig, createDbConnection: unit -> IDbConnection) =
    fun cts ->
      use connection = createDbConnection()
      connection.TryOpenConnection()

      connection
      |> Db.newCommand Queries.GetLocalProjects
      |> setCancellationToken cts
      |> Db.Async.query(Mappers.mapLocalProject readConfig)

  let FindLocalProjectById
    (readConfig, createDbConnection: unit -> IDbConnection)
    =
    fun (projectId: Guid, cts) ->
      use connection = createDbConnection()
      connection.TryOpenConnection()

      connection
      |> Db.newCommand Queries.GetLocalProjectById
      |> setCancellationToken cts
      |> Db.setParams [ "id", sqlString projectId ]
      |> Db.Async.querySingle(Mappers.mapLocalProject readConfig)

  let InsertLocalProject(createDbConnection: unit -> IDbConnection) =
    fun (name: string, description: string option, configPath: string, cts) -> task {
      use connection = createDbConnection()
      connection.TryOpenConnection()
      let projectId = Guid.NewGuid()

      use! trx =
        connection.TryBeginTransactionAsync(
          defaultArg cts CancellationToken.None
        )

      do!
        connection
        |> Db.newCommand Queries.InsertProject
        |> Db.setTransaction trx
        |> setCancellationToken cts
        |> Db.setParams [
          "id", sqlString projectId
          "name", sqlString name
          "description", sqlStringOrNull description
        ]
        |> Db.Async.exec

      do!
        connection
        |> Db.newCommand Queries.InsertLocalProject
        |> Db.setTransaction trx
        |> setCancellationToken cts
        |> Db.setParams [
          "id", sqlString(Guid.NewGuid())
          "config_path", sqlString configPath
          "project_id", sqlString projectId
        ]
        |> Db.Async.exec

      do! trx.TryCommitAsync(defaultArg cts CancellationToken.None)
      return projectId
    }

  let UpdateProject(createDbConnection: unit -> IDbConnection) =
    fun (id: Guid, name: string, description: string option, cts) ->
      use connection = createDbConnection()
      connection.TryOpenConnection()

      connection
      |> Db.newCommand Queries.UpdateProjectQuery
      |> setCancellationToken cts
      |> Db.setParams [
        "id", sqlString id
        "name", sqlString name
        "description", sqlStringOrNull description
      ]
      |> Db.Async.exec

  let UpdateLocalProjectConfigPath(createDbConnection: unit -> IDbConnection) =
    fun (projectId: Guid, configPath: string, cts) ->
      use connection = createDbConnection()
      connection.TryOpenConnection()

      connection
      |> Db.newCommand Queries.UpdateLocalProjectConfigPathQuery
      |> setCancellationToken cts
      |> Db.setParams [
        "project_id", sqlString projectId
        "config_path", sqlString configPath
      ]
      |> Db.Async.exec
