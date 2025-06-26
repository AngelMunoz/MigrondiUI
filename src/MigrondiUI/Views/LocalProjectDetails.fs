module MigrondiUI.Views.LocalProjectDetails

open System
open System.IO
open System.Threading.Tasks

open Microsoft.Extensions.Logging

open Avalonia.Controls

open NXUI.Extensions

open IcedTasks
open FSharp.UMX
open FsToolkit.ErrorHandling
open FSharp.Data.Adaptive

open Navs
open Navs.Avalonia
open Migrondi.Core

open MigrondiUI
open MigrondiUI.Projects
open MigrondiUI.Database
open MigrondiUI.Components
open MigrondiUI.Components.MigrationRunnerToolbar
open MigrondiUI.Components.Fields
open SukiUI.Controls
open SukiUI.Dialogs

type LocalProjectDetailsVM
  (
    logger: ILogger<LocalProjectDetailsVM>,
    dialogManager: SukiUI.Dialogs.ISukiDialogManager,
    migrondi: IMigrondi,
    project: LocalProject
  ) =

  let _migrations = cval [||]
  let lastDryRun = cval [||]

  let currentShow = cval ProjectDetails.CurrentShow.Migrations

  let handleException(work: Async<unit>) = asyncEx {
    let! result = work |> Async.Catch

    match result with
    | Choice1Of2 _ -> return ()
    | Choice2Of2 ex ->
      logger.LogError(ex, "An error occurred while processing the request")
      currentShow.setValue(ProjectDetails.CurrentShow.ExceptionThrown ex)
      return ()
  }

  do
    logger.LogDebug "LocalProjectDetailsVM created"
    migrondi.Initialize()

  member _.Project = project

  member _.Migrations: MigrationStatus[] aval = _migrations

  member _.LastDryRun: Migration[] aval = lastDryRun

  member _.CurrentShow: ProjectDetails.CurrentShow aval = currentShow

  member _.OpenFileExplorer() = asyncEx {
    logger.LogDebug "Opening file explorer"

    let migrationsDir =
      project.migrondiConfigPath |> Path.GetDirectoryName |> nonNull

    logger.LogDebug("Migrations directory: {migrationsDir}", migrationsDir)

    do! OsOperations.OpenDirectory migrationsDir
    logger.LogDebug "File explorer opened"
  }

  member _.ListMigrations() = asyncEx {
    logger.LogDebug "Listing migrations"
    let! token = Async.CancellationToken
    let! migrations = migrondi.MigrationsListAsync token

    logger.LogDebug("Migrations listed: {migrations}", migrations.Count)

    migrations |> Seq.toArray |> _migrations.setValue

    lastDryRun.setValue [||]
    logger.LogDebug "Last dry run set to empty array"

    currentShow.setValue ProjectDetails.CurrentShow.Migrations
    return ()
  }

  member this.NewMigration(name: string) =
    asyncEx {
      logger.LogDebug "Creating new migration"
      let! token = Async.CancellationToken

      logger.LogDebug("New migration name: {name}", name)
      let! migration = migrondi.RunNewAsync(name, cancellationToken = token)
      logger.LogDebug("New migration created: {migration}", migration)
      return! this.ListMigrations()
    }
    |> handleException

  member this.RunMigrations(kind: ProjectDetails.RunMigrationKind, steps: int) =
    asyncEx {
      let! token = Async.CancellationToken
      logger.LogDebug("Running migrations: {kind}, {steps}", kind, steps)

      match kind with
      | ProjectDetails.RunMigrationKind.Up ->
        logger.LogDebug("Running migrations up")
        do! migrondi.RunUpAsync(steps, cancellationToken = token) :> Task
        do! this.ListMigrations()
        logger.LogDebug("Migrations up completed")
        return ()
      | ProjectDetails.RunMigrationKind.Down ->
        logger.LogDebug("Running migrations down")
        do! migrondi.RunDownAsync(steps, cancellationToken = token) :> Task
        do! this.ListMigrations()
        logger.LogDebug("Migrations down completed")
        return ()
      | ProjectDetails.RunMigrationKind.DryUp ->
        logger.LogDebug("Running migrations dry up")
        let! run = migrondi.DryRunUpAsync(steps, cancellationToken = token)

        logger.LogDebug(
          "Dry run up completed and found {count} migrations",
          run.Count
        )

        run |> Seq.toArray |> lastDryRun.setValue

        currentShow.setValue(
          ProjectDetails.CurrentShow.DryRun ProjectDetails.RunMigrationKind.Up
        )

        logger.LogDebug("Current show set to dry run up")
        return ()
      | ProjectDetails.RunMigrationKind.DryDown ->
        logger.LogDebug("Running migrations dry down")
        let! run = migrondi.DryRunDownAsync(steps, cancellationToken = token)

        logger.LogDebug(
          "Dry run down completed and found {count} migrations",
          run.Count
        )

        run |> Seq.toArray |> lastDryRun.setValue

        currentShow.setValue(
          ProjectDetails.CurrentShow.DryRun ProjectDetails.RunMigrationKind.Down
        )

        logger.LogDebug("Current show set to dry run down")
        return ()
    }
    |> handleException

  member _.ExportToVirtualProject() = failwith "Not Implemented"

  member _.ConfirmRun(args: ProjectDetails.RunMigrationKind) = asyncEx {
    logger.LogDebug("Confirming run for {args}", args)

    let! token = Async.CancellationToken

    match args with
    | ProjectDetails.RunMigrationKind.DryUp
    | ProjectDetails.RunMigrationKind.DryDown -> return true
    | ProjectDetails.RunMigrationKind.Up
    | ProjectDetails.RunMigrationKind.Down ->
      return!
        dialogManager
          .CreateDialog()
          .WithTitle("This is a potentially destructive operation")
          .WithContent(
            TextBlock().Classes("h4").Text("Are you sure you want to continue?")
          )
          .WithYesNoResult("Yes", "No")
          .TryShowAsync(token)
  }



// Reusing shared component from SharedComponents module

let localProjectView
  (
    project: LocalProject,
    onRunMigrationsRequested: ProjectDetails.RunMigrationKind -> Async<bool>,
    onApplyMigrations: ProjectDetails.RunMigrationKind * int -> Async<unit>
  ) : Control =
  let description = defaultArg project.description "No description"

  let config =
    project.config |> Option.defaultWith(fun _ -> failwith "No config found")

  let configView(configPath: string, config: MigrondiConfig) : Control =
    let migrationsDir =
      option {
        let! path = Path.GetDirectoryName(configPath)
        let path = Path.Combine(path, config.migrations)
        return Path.GetFullPath path
      }
      |> Option.defaultValue "Unable to resolve the project's root directory"

    Grid()
      .RowDefinitions("*,Auto,Auto")
      .ColumnDefinitions("10*,90*")
      .Children(
        LabeledField
          .Horizontal("Connection String:", config.connection)
          .Row(0)
          .Column(0)
          .ColumnSpan(2),
        LabeledField.Horizontal("Driver:", $"{config.driver}").Row(1).Column(0),
        LabeledField
          .Horizontal("Migrations Directory:", migrationsDir)
          .Row(1)
          .Column(1)
      )

  Expander()
    .Header(
      StackPanel()
        .Spacing(8)
        .OrientationHorizontal()
        .Children(
          TextBlock()
            .Text($"{project.name} - {description}")
            .VerticalAlignmentCenter(),
          MigrationsRunnerToolbar(onRunMigrationsRequested, onApplyMigrations)
            .VerticalAlignmentCenter()
        )
    )
    .Content(
      GlassCard().Content(configView(project.migrondiConfigPath, config))
    )

let toolbar
  (
    onNavigateBack: unit -> unit,
    onNewMigration: string -> unit,
    onRefresh: unit -> unit,
    onOpenInExplorer: unit -> unit
  ) : Control =
  let isEnabled = cval false

  let nameTextBox =
    TextBox()
      .Watermark("Enter migration name")
      .Width(200)
      .Height(28.)
      .VerticalAlignmentCenter()
      .VerticalContentAlignmentCenter()
      .AcceptsReturn(false)
      .OnTextChangedHandler(fun txtBox _ ->
        if String.IsNullOrWhiteSpace txtBox.Text |> not then
          isEnabled.setValue true
        else
          isEnabled.setValue false)

  let createButton =
    Button()
      .Content("Create Migration")
      .Classes("Accent", "Outlined")
      .IsEnabled(isEnabled |> AVal.toBinding)
      .OnClickHandler(fun _ _ ->
        let text = (nonNull nameTextBox.Text).Trim().Replace(' ', '-')
        onNewMigration text
        nameTextBox.Text <- "")

  let openInExplorerButton =
    Button()

      .Content("Open in Explorer")
      .OnClickHandler(fun _ _ -> onOpenInExplorer())

  // Grid-based layout replacing Toolbar
  Grid()
    .RowDefinitions("Auto")
    .ColumnDefinitions("Auto,Auto,Auto,Auto,Auto")
    .ColumnSpacing(8)
    .Children(
      Button()
        .Content("Back")
        .OnClickHandler(fun _ _ -> onNavigateBack())
        .Column(0),
      Button()
        .Content("Refresh")
        .OnClickHandler(fun _ _ -> onRefresh())
        .Column(1),
      openInExplorerButton.Column(2),
      createButton.Column(3),
      nameTextBox.Column(4)
    )

type LProjectDetailsView(vm: LocalProjectDetailsVM, onNavigateBack) =
  inherit UserControl()

  let onNewMigration(name: string) =
    vm.NewMigration name |> Async.StartImmediate

  let onRefresh() =
    vm.ListMigrations() |> Async.StartImmediate

  let onOpenInExplorer() =
    vm.OpenFileExplorer() |> Async.StartImmediate

  let onMigrationsRunRequested args = vm.ConfirmRun args

  let onApplyMigrations args = vm.RunMigrations args

  do
    vm.ListMigrations() |> Async.StartImmediate

    base.Name <- "ProjectDetails"

    base.Content <-
      Grid()
        .RowDefinitions("Auto,Auto,*")
        .ColumnDefinitions("Auto,*,*")
        .ColumnSpacing(8)
        .RowSpacing(8)
        .Children(
          GlassCard()
            .Content(
              toolbar(
                onNavigateBack,
                onNewMigration,
                onRefresh,
                onOpenInExplorer
              )
            )
            .Row(0)
            .Column(0)
            .ColumnSpan(3)
            .HorizontalAlignmentStretch(),
          GlassCard()
            .Content(
              localProjectView(
                vm.Project,
                onMigrationsRunRequested,
                onApplyMigrations
              )
            )
            .Row(1)
            .Column(0)
            .ColumnSpan(3)
            .VerticalAlignmentTop()
            .HorizontalAlignmentStretch()
            .MarginY(8),
          GlassCard()
            .Content(
              ProjectDetails.MigrationsPanel(
                currentShow = vm.CurrentShow,
                migrations = vm.Migrations,
                lastDryRun = vm.LastDryRun,
                migrationsView = ProjectDetails.migrationListView,
                dryRunView = ProjectDetails.dryRunListView
              )
            )
            .Row(2)
            .Column(0)
            .ColumnSpan(3)
            .VerticalAlignmentStretch()
            .HorizontalAlignmentStretch()
            .MarginY(8)
        )

let buildDetailsView
  (
    projectId: Guid<LProjectId>,
    logger: ILogger<LocalProjectDetailsVM>,
    mLogger: ILogger<IMigrondi>,
    projects: ILocalProjectRepository,
    dialogManager: ISukiDialogManager,
    onNavigateBack: unit -> unit
  ) =
  asyncOption {
    let! cancellationToken = Async.CancellationToken
    let! project = projects.GetProjectById projectId cancellationToken

    logger.LogDebug("Project from repository: {project}", project)

    let! config = project.config

    let projectRoot =
      Path.GetDirectoryName project.migrondiConfigPath |> nonNull

    let migrondi = Migrondi.MigrondiFactory(config, projectRoot, mLogger)

    let vm = LocalProjectDetailsVM(logger, dialogManager, migrondi, project)
    return LProjectDetailsView(vm, onNavigateBack) :> Control
  }

let buildProjectNotFound(id: Guid<LProjectId>) : Control =
  UserControl()
    .Name("ProjectNotFound")
    .Content(TextBlock().Text($"Project with ID {id} was not found."))

let buildLoading(id: Guid<LProjectId>) : Control =
  UserControl()
    .Name("ProjectLoading")
    .Content(TextBlock().Text($"Loading project with ID {id}..."))

let View
  (
    logger: ILogger<LocalProjectDetailsVM>,
    mLogger: ILogger<IMigrondi>,
    projects: ILocalProjectRepository,
    dialogManager: ISukiDialogManager
  )
  (context: RouteContext)
  (nav: INavigable<Control>)
  : Control =
  let lProjectId =
    context.getParam<Guid> "lProjectId"
    |> ValueOption.map UMX.tag<LProjectId>
    |> ValueOption.toOption

  let view =
    let projectId = defaultArg lProjectId (UMX.tag<LProjectId> Guid.Empty)
    cval(buildLoading projectId)

  let onNavigateBack() =
    asyncEx {
      match! nav.NavigateByName("landing") with
      | Ok _ -> ()
      | Error(e) ->
        logger.LogWarning("Navigation failed: {error}", e.StringError())
    }
    |> Async.StartImmediate

  match lProjectId with
  | Some projectId ->
    logger.LogDebug("Project ID from route parameters: {projectId}", projectId)

    asyncEx {
      match!
        buildDetailsView(
          projectId,
          logger,
          mLogger,
          projects,
          dialogManager,
          onNavigateBack
        )
      with
      | Some builtView -> view.setValue(builtView)
      | None -> view.setValue(buildProjectNotFound projectId)
    }
    |> Async.StartImmediate
  | None ->
    logger.LogDebug("No project ID found in route parameters")
    view.setValue(buildProjectNotFound(UMX.tag<LProjectId> Guid.Empty))

  UserControl()
    .Name("LocalProjectDetails")
    .Content(view |> AVal.toBinding)
    .Margin(8)
