namespace MigrondiUI.Views

open System
open System.IO

open Microsoft.Extensions.Logging

open Avalonia.Controls
open Avalonia.Controls
open Avalonia.Controls.Templates
open Avalonia.Media

open NXUI.Extensions

open IcedTasks
open IcedTasks.Polyfill.Async.PolyfillBuilders
open FsToolkit.ErrorHandling
open FSharp.Data.Adaptive

open Navs
open Navs.Avalonia
open Migrondi.Core
open MigrondiUI
open MigrondiUI.Projects
open MigrondiUI.Views.Components
open System.Threading.Tasks
open TextEditor

module SharedProjectDetails =
  open System.Collections.Generic

  [<Struct>]
  type RunMigrationKind =
    | Up
    | Down
    | DryUp
    | DryDown

  [<Struct>]
  type CurrentShow =
    | Migrations
    | DryRun of RunMigrationKind

  let migrationView =
    FuncDataTemplate<MigrationStatus>(fun migrationStatus _ ->
      let migration =
        match migrationStatus with
        | Applied m -> m
        | Pending m -> m

      let status =
        match migrationStatus with
        | Applied _ -> "Applied"
        | Pending _ -> "Pending"

      let strDate =
        DateTimeOffset.FromUnixTimeMilliseconds migration.timestamp
        |> _.ToString("G")

      let migrationContent =
        Grid()
          .ColumnDefinitions("*,4,*")
          .Children(
            LabeledField
              .Vertical("Migrate Up", TxtEditor.Readonly migration.upContent)
              .Column(0),
            GridSplitter()
              .Column(1)
              .ResizeDirectionColumns()
              .IsEnabled(false)
              .Background("Black" |> SolidColorBrush.Parse)
              .MarginX(8)
              .CornerRadius(5),
            LabeledField
              .Vertical(
                "Migrate Down",
                TxtEditor.Readonly migration.downContent
              )
              .Column(2)
          )

      Expander()
        .Header(
          StackPanel()
            .Children(
              TextBlock().Text(migration.name),
              TextBlock()
                .Text($" [{status}]")
                .Foreground(
                  match migrationStatus with
                  | Applied _ -> "Green"
                  | Pending _ -> "OrangeRed"
                  |> SolidColorBrush.Parse
                ),
              TextBlock().Text($" - {strDate}")
            )
            .OrientationHorizontal()
        )
        .Content(
          StackPanel()
            .Children(
              LabeledField.Horizontal(
                "Manual Transaction:",
                $" %b{migration.manualTransaction}"
              ),
              migrationContent
            )
            .Spacing(8)
        )
        .HorizontalAlignmentStretch()
        .VerticalAlignmentStretch()
        .MarginY(4))

  let dryRunView show =
    FuncDataTemplate<Migration>(fun (migration) _ ->
      let content =
        match show with
        | Up
        | DryUp -> migration.upContent
        | Down
        | DryDown -> migration.downContent

      let content =
        if migration.manualTransaction then
          content
        else
          $"-- ----------START TRANSACTION----------\n{content}\n-- ----------COMMIT TRANSACTION----------"

      TxtEditor.Readonly(content).Name("MigrationContent"))

  let migrationsPanel
    (
      currentShow: aval<CurrentShow>,
      migrations: aval<MigrationStatus[]>,
      lastDryRun: aval<Migration[]>
    ) : Control =
    let migrationListView(migrations: MigrationStatus[]) : Control =
      if migrations.Length = 0 then
        TextBlock().Text "No migrations found." :> Control
      else
        ItemsControl().ItemsSource(migrations).ItemTemplate(migrationView)

    let dryRunListView(migrations: Migration[]) : Control =
      if migrations.Length = 0 then
        TextBlock().Text "No dry run found."
      else
        match currentShow.getValue() with
        | Migrations -> TextBlock().Text "No dry run found."
        | DryRun kind ->
          let direction =
            match kind with
            | Up
            | DryUp -> "Apply Migrations"
            | Down
            | DryDown -> "Rollback Migrations"

          StackPanel()
            .Spacing(12)
            .Children(
              TextBlock()
                .Classes("DryRunHeader")
                .Text(
                  $"This is a simulation {direction}. The database will not be affected:"
                ),
              ItemsControl()
                .ItemsSource(migrations)
                .ItemTemplate(dryRunView kind),
              TextBlock()
                .Text(
                  "Simulation completed. No changes were made to the database."
                )
            )

    let content =
      (currentShow, migrations, lastDryRun)
      |||> AVal.map3(fun show mStatus drMigrations ->
        match show with
        | Migrations -> migrationListView mStatus
        | DryRun _ -> dryRunListView drMigrations)

    ScrollViewer().Content(content |> AVal.toBinding)

  let toolbar
    (
      (onNavigateBack: unit -> unit),
      (onNewMigration: string -> unit),
      (onRefresh: unit -> unit),
      (onOpenInExplorer: unit -> unit)
    ) : Control =
    let isEnabled = cval false

    let nameTextBox =
      TextBox()
        .Name("New Migration Name:")
        .Watermark("Enter migration name")
        .Width(200)
        .AcceptsReturn(false)
        .OnTextChangedHandler(fun txtBox _ ->
          if String.IsNullOrWhiteSpace txtBox.Text |> not then
            isEnabled.setValue true
          else
            isEnabled.setValue false)

    let createButton =
      Button()
        .Content("Create Migration")
        .IsEnabled(isEnabled |> AVal.toBinding)
        .OnClickHandler(fun _ _ ->
          let text = (nameTextBox.Text |> nonNull).Trim().Replace(' ', '-')
          onNewMigration text
          nameTextBox.Text <- "")

    let openInExplorerButton =
      Button()
        .Content("Open in Explorer")
        .OnClickHandler(fun _ _ -> onOpenInExplorer())

    StackPanel()
      .OrientationHorizontal()
      .Spacing(8)
      .Children(
        Button().Content("Back").OnClickHandler(fun _ _ -> onNavigateBack()),
        Button().Content("Refresh").OnClickHandler(fun _ _ -> onRefresh()),
        nameTextBox,
        createButton,
        openInExplorerButton
      )

module LocalProjectDetails =
  type LocalProjectDetailsVM
    (
      logger: ILogger<LocalProjectDetailsVM>,
      migrondi: IMigrondi,
      project: LocalProject
    ) =

    let _migrations = cval [||]
    let lastDryRun = cval [||]

    let currentShow = cval SharedProjectDetails.CurrentShow.Migrations

    do
      logger.LogDebug "LocalProjectDetailsVM created"
      migrondi.Initialize()

    member _.Project = project

    member _.Migrations: MigrationStatus[] aval = _migrations

    member _.LastDryRun: Migration[] aval = lastDryRun

    member _.CurrentShow: SharedProjectDetails.CurrentShow aval = currentShow

    member _.OpenFileExplorer() = async {
      logger.LogDebug "Opening file explorer"

      let migrationsDir =
        project.migrondiConfigPath |> Path.GetDirectoryName |> nonNull

      logger.LogDebug("Migrations directory: {migrationsDir}", migrationsDir)

      do! OsOperations.OpenDirectory migrationsDir
      logger.LogDebug "File explorer opened"
    }

    member _.ListMigrations() = async {
      logger.LogDebug "Listing migrations"
      let! token = Async.CancellationToken
      let! migrations = migrondi.MigrationsListAsync token

      logger.LogDebug("Migrations listed: {migrations}", migrations.Count)

      migrations |> Seq.toArray |> _migrations.setValue

      lastDryRun.setValue [||]
      logger.LogDebug "Last dry run set to empty array"

      currentShow.setValue SharedProjectDetails.CurrentShow.Migrations
      return ()
    }

    member this.NewMigration(name: string) = async {
      logger.LogDebug "Creating new migration"
      let! token = Async.CancellationToken

      logger.LogDebug("New migration name: {name}", name)
      let! migration = migrondi.RunNewAsync(name, cancellationToken = token)
      logger.LogDebug("New migration created: {migration}", migration)
      return! this.ListMigrations()
    }

    member this.RunMigrations
      (kind: SharedProjectDetails.RunMigrationKind, steps: int)
      =
      async {
        let! token = Async.CancellationToken
        logger.LogDebug("Running migrations: {kind}, {steps}", kind, steps)

        match kind with
        | SharedProjectDetails.RunMigrationKind.Up ->
          logger.LogDebug("Running migrations up")
          do! migrondi.RunUpAsync(steps, cancellationToken = token) :> Task
          do! this.ListMigrations()
          logger.LogDebug("Migrations up completed")
          return ()
        | SharedProjectDetails.RunMigrationKind.Down ->
          logger.LogDebug("Running migrations down")
          do! migrondi.RunDownAsync(steps, cancellationToken = token) :> Task
          do! this.ListMigrations()
          logger.LogDebug("Migrations down completed")
          return ()
        | SharedProjectDetails.RunMigrationKind.DryUp ->
          logger.LogDebug("Running migrations dry up")
          let! run = migrondi.DryRunUpAsync(steps, cancellationToken = token)

          logger.LogDebug(
            "Dry run up completed and found {count} migrations",
            run.Count
          )

          run |> Seq.toArray |> lastDryRun.setValue

          currentShow.setValue(
            SharedProjectDetails.CurrentShow.DryRun
              SharedProjectDetails.RunMigrationKind.Up
          )

          logger.LogDebug("Current show set to dry run up")
          return ()
        | SharedProjectDetails.RunMigrationKind.DryDown ->
          logger.LogDebug("Running migrations dry down")
          let! run = migrondi.DryRunDownAsync(steps, cancellationToken = token)

          logger.LogDebug(
            "Dry run down completed and found {count} migrations",
            run.Count
          )

          run |> Seq.toArray |> lastDryRun.setValue

          currentShow.setValue(
            SharedProjectDetails.CurrentShow.DryRun
              SharedProjectDetails.RunMigrationKind.Down
          )

          logger.LogDebug("Current show set to dry run down")
          return ()
      }

  let runMigrations
    (onRunMigrationsRequested:
      SharedProjectDetails.RunMigrationKind * int -> unit)
    : Control =
    let dryRun = cval false
    let steps = cval 1M

    let getIntValue() =
      try
        let v = steps.getValue() |> int
        if v < 0 then 1 else v
      with :? OverflowException ->
        1

    let applyPendingButton =
      let abutton =
        dryRun
        |> AVal.map(fun dryRun ->
          if dryRun then
            Button()
              .Content("Apply Pending (Dry Run)")
              .OnClickHandler(fun _ _ ->
                onRunMigrationsRequested(
                  SharedProjectDetails.RunMigrationKind.DryUp,
                  getIntValue()
                ))
            :> Control
          else
            SplitButton()
              .Content("Apply Pending")
              .Flyout(
                Flyout()
                  .Content(
                    Button()
                      .Content("Confirm Apply")
                      .OnClickHandler(fun _ _ ->
                        onRunMigrationsRequested(
                          SharedProjectDetails.RunMigrationKind.Up,
                          getIntValue()
                        ))
                  )
              ))

      UserControl()
        .Name("ApplyPendingButton")
        .Content(abutton |> AVal.toBinding)

    let rollbackButton =
      let rbutton =
        dryRun
        |> AVal.map(fun dryRun ->
          if dryRun then
            Button()
              .Content("Rollback (Dry Run)")
              .OnClickHandler(fun _ _ ->
                onRunMigrationsRequested(
                  SharedProjectDetails.RunMigrationKind.DryDown,
                  getIntValue()
                ))
            :> Control
          else
            SplitButton()
              .Content("Rollback")
              .Flyout(
                Flyout()
                  .Content(
                    Button()
                      .Content("Confirm Rollback")
                      .OnClickHandler(fun _ _ ->
                        onRunMigrationsRequested(
                          SharedProjectDetails.RunMigrationKind.Down,
                          getIntValue()
                        ))
                  )
              ))

      UserControl().Name("RollbackButton").Content(rbutton |> AVal.toBinding)

    let numericUpDown =
      NumericUpDown()
        .Minimum(0)
        .Value(steps |> AVal.toBinding)
        .Watermark("Amount to run")
        .OnValueChangedHandler(fun _ value ->
          match value.NewValue |> ValueOption.ofNullable with
          | ValueNone -> steps.setValue 1M
          | ValueSome value -> steps.setValue value)

    let checkBox =
      CheckBox()
        .Content("Dry Run")
        .IsChecked(dryRun |> AVal.toBinding)
        .OnIsCheckedChangedHandler(fun checkbox _ ->

          let isChecked =
            checkbox.IsChecked
            |> ValueOption.ofNullable
            |> ValueOption.defaultValue true

          dryRun.setValue isChecked)

    StackPanel()
      .OrientationHorizontal()
      .Spacing(8)
      .Children(applyPendingButton, rollbackButton, checkBox, numericUpDown)

  let localProjectView
    (
      project: LocalProject,
      onRunMigrationsRequested:
        SharedProjectDetails.RunMigrationKind * int -> unit
    ) : Control =
    let description = defaultArg project.description "No description"

    let config =
      project.config |> Option.defaultWith(fun _ -> failwith "No config found")

    let configView(configPath: string, config: MigrondiConfig) : Control =
      let migrationsDir =
        option {
          let! path = configPath |> Path.GetDirectoryName
          return Path.Combine(path, config.migrations) |> Path.GetFullPath
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
          LabeledField
            .Horizontal("Driver:", $"{config.driver}")
            .Row(1)
            .Column(0),
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
            runMigrations(onRunMigrationsRequested).VerticalAlignmentCenter()
          )
      )
      .Content(configView(project.migrondiConfigPath, config))

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
        .Name("New Migration Name:")
        .Watermark("Enter migration name")
        .Width(200)
        .AcceptsReturn(false)
        .OnTextChangedHandler(fun txtBox _ ->
          if String.IsNullOrWhiteSpace txtBox.Text |> not then
            isEnabled.setValue true
          else
            isEnabled.setValue false)

    let createButton =
      Button()
        .Content("Create Migration")
        .IsEnabled(isEnabled |> AVal.toBinding)
        .OnClickHandler(fun _ _ ->
          let text = (nameTextBox.Text |> nonNull).Trim().Replace(' ', '-')
          onNewMigration text
          nameTextBox.Text <- "")

    let openInExplorerButton =
      Button()
        .Content("Open in Explorer")
        .OnClickHandler(fun _ _ -> onOpenInExplorer())

    Toolbar
      .get(Spacing 8., Orientation Horizontal)
      .Children(
        Button().Content("Back").OnClickHandler(fun _ _ -> onNavigateBack()),
        Button().Content("Refresh").OnClickHandler(fun _ _ -> onRefresh()),
        nameTextBox,
        createButton,
        openInExplorerButton
      )

  let View
    (
      logger: ILogger<LocalProjectDetailsVM>,
      mLogger: ILogger<IMigrondi>,
      projects: ILocalProjectRepository
    )
    (context: RouteContext)
    (nav: INavigable<Control>)
    : Async<Control> =
    async {
      let getProjectbyId(projectId: Guid) = async {
        let! project = projects.GetProjectById projectId

        match project with
        | Some project -> return Some project
        | _ ->
          logger.LogWarning(
            "We're not supposed to have a virtual project here. Project ID: {projectId}",
            projectId
          )

          return None
      }

      let vm = asyncOption {
        let! projectId =
          context.getParam<Guid> "projectId" |> ValueOption.toOption

        logger.LogDebug(
          "Project ID from route parameters: {projectId}",
          projectId
        )

        let! project = getProjectbyId projectId
        logger.LogDebug("Project from repository: {project}", project)

        let! config = project.config

        let projectRoot =
          Path.GetDirectoryName project.migrondiConfigPath |> nonNull

        let migrondi = Migrondi.MigrondiFactory(config, projectRoot, mLogger)

        return LocalProjectDetailsVM(logger, migrondi, project)
      }

      let onNavigateBack() =
        async {
          match! nav.NavigateByName("landing") with
          | Ok _ -> ()
          | Error(e) ->
            logger.LogWarning("Navigation failed: {error}", e.StringError())
        }
        |> Async.StartImmediate

      match! vm with
      | Some vm ->
        vm.ListMigrations() |> Async.StartImmediate

        let onNewMigration name =
          vm.NewMigration name |> Async.StartImmediate

        let onRefresh() =
          vm.ListMigrations() |> Async.StartImmediate

        let onOpenInExplorer() =
          vm.OpenFileExplorer() |> Async.StartImmediate

        let onRunMigrationsRequested args =
          vm.RunMigrations args |> Async.StartImmediate

        return
          UserControl()
            .Name("ProjectDetails")
            .Content(
              Grid()
                .RowDefinitions("Auto,Auto,*")
                .ColumnDefinitions("Auto,*,*")
                .Children(
                  toolbar(
                    onNavigateBack,
                    onNewMigration,
                    onRefresh,
                    onOpenInExplorer
                  )
                    .Row(0)
                    .Column(0)
                    .ColumnSpan(2)
                    .HorizontalAlignmentStretch(),
                  localProjectView(vm.Project, onRunMigrationsRequested)
                    .Row(1)
                    .Column(0)
                    .ColumnSpan(3)
                    .VerticalAlignmentTop()
                    .HorizontalAlignmentStretch()
                    .MarginY(8),
                  SharedProjectDetails
                    .migrationsPanel(
                      vm.CurrentShow,
                      vm.Migrations,
                      vm.LastDryRun
                    )
                    .Row(2)
                    .Column(0)
                    .ColumnSpan(3)
                    .VerticalAlignmentStretch()
                    .HorizontalAlignmentStretch()
                    .MarginY(8)
                )
            )
            .Margin(8)
          :> Control
      | None ->
        logger.LogWarning("Project ID not found in route parameters.")
        return TextBlock().Text("Project ID not found.")
    }

module VirtualProjectDetails =

  type VirtualProjectDetailsVM
    (
      logger: ILogger<VirtualProjectDetailsVM>,
      migrondi: IMigrondi,
      project: VirtualProject
    ) =
    let _migrations = cval [||]
    let lastDryRun = cval [||]
    let currentShow = cval SharedProjectDetails.CurrentShow.Migrations

    do
      logger.LogDebug "VirtualProjectDetailsVM created"
      migrondi.Initialize()

    member _.Project = project

    member _.Migrations: MigrationStatus[] aval = _migrations

    member _.LastDryRun: Migration[] aval = lastDryRun

    member _.CurrentShow: SharedProjectDetails.CurrentShow aval = currentShow

    member _.ListMigrations() = async {
      logger.LogDebug "Listing migrations"
      let! token = Async.CancellationToken
      let! migrations = migrondi.MigrationsListAsync token

      logger.LogDebug("Migrations listed: {migrations}", migrations.Count)

      migrations |> Seq.toArray |> _migrations.setValue

      lastDryRun.setValue [||]
      logger.LogDebug "Last dry run set to empty array"

      currentShow.setValue SharedProjectDetails.CurrentShow.Migrations
      return ()
    }

  let toolbar
    (
      (onNavigateBack: unit -> unit),
      (onNewMigration: string -> unit),
      (onRefresh: unit -> unit)
    ) =
    let isEnabled = cval false

    let nameTextBox =
      TextBox()
        .Name("New Migration Name:")
        .Watermark("Enter migration name")
        .Width(200)
        .AcceptsReturn(false)
        .OnTextChangedHandler(fun txtBox _ ->
          if String.IsNullOrWhiteSpace txtBox.Text |> not then
            isEnabled.setValue true
          else
            isEnabled.setValue false)

    let createButton =
      Button()
        .Content("Create Migration")
        .IsEnabled(isEnabled |> AVal.toBinding)
        .OnClickHandler(fun _ _ ->
          let text = (nameTextBox.Text |> nonNull).Trim().Replace(' ', '-')
          onNewMigration text
          nameTextBox.Text <- "")

    Toolbar
      .get(Spacing 8., Orientation Horizontal)
      .Children(
        Button().Content("Back").OnClickHandler(fun _ _ -> onNavigateBack()),
        Button().Content("Refresh").OnClickHandler(fun _ _ -> onRefresh()),
        nameTextBox,
        createButton
      )

  let View
    (
      logger: ILogger<VirtualProjectDetailsVM>,
      projects: IVirtualProjectRepository,
      vMigrondi: IMigrondi
    )
    (context: RouteContext)
    (nav: INavigable<Control>)
    : Async<Control> =
    asyncEx {
      let getProjectById(projectId: Guid) = asyncEx {
        let! project = projects.GetProjectById projectId

        match project with
        | Some project -> return Some project
        | _ ->
          logger.LogWarning(
            "Virtual project not found. Project ID: {projectId}",
            projectId
          )

          return None
      }

      let vm = asyncOption {
        let! projectId =
          context.getParam<Guid> "projectId" |> ValueOption.toOption

        logger.LogDebug(
          "Project ID from route parameters: {projectId}",
          projectId
        )

        let! project = getProjectById projectId
        logger.LogDebug("Project from repository: {project}", project)

        return VirtualProjectDetailsVM(logger, vMigrondi, project)
      }

      let onNavigateBack() =
        async {
          match! nav.NavigateByName("landing") with
          | Ok _ -> ()
          | Error(e) ->
            logger.LogWarning("Navigation failed: {error}", e.StringError())
        }
        |> Async.StartImmediate

      match! vm with
      | Some vm ->
        vm.ListMigrations() |> Async.StartImmediate

        let onNewMigration _ =
          logger.LogWarning(
            "New migration is not supported for virtual projects."
          )

        let onRefresh() =
          vm.ListMigrations() |> Async.StartImmediate

        return
          UserControl()
            .Name("VirtualProjectDetails")
            .Content(
              Grid()
                .RowDefinitions("Auto,*")
                .ColumnDefinitions("Auto,*,*")
                .Children(
                  toolbar(onNavigateBack, onNewMigration, onRefresh)
                    .Row(0)
                    .Column(0)
                    .ColumnSpan(2)
                    .HorizontalAlignmentStretch(),
                  SharedProjectDetails
                    .migrationsPanel(
                      vm.CurrentShow,
                      vm.Migrations,
                      vm.LastDryRun
                    )
                    .Row(1)
                    .Column(0)
                    .ColumnSpan(3)
                    .VerticalAlignmentStretch()
                    .HorizontalAlignmentStretch()
                    .MarginY(8)
                )
            )
            .Margin(8)
          :> Control
      | None ->
        logger.LogWarning("Project ID not found in route parameters.")
        return TextBlock().Text("Project ID not found.")
    }
