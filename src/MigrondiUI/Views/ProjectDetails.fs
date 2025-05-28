namespace MigrondiUI.Views

open System
open System.IO

open Microsoft.Extensions.Logging

open Avalonia.Controls
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

module VirtualProjectDetails =
  open MigrondiUI.MigrondiExt

  type VirtualProjectDetailsVM
    (
      logger: ILogger<VirtualProjectDetailsVM>,
      migrondi: IMigrondiUI,
      project: VirtualProject
    ) =
    let _migrations = cval [||]
    let lastDryRun = cval [||]
    let currentShow = cval ProjectDetails.CurrentShow.Migrations

    do
      logger.LogDebug "VirtualProjectDetailsVM created"
      migrondi.Initialize()

    member _.Project = project

    member _.Migrations: MigrationStatus[] aval = _migrations

    member _.LastDryRun: Migration[] aval = lastDryRun

    member _.CurrentShow: ProjectDetails.CurrentShow aval = currentShow

    member _.ListMigrations() = async {
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
      vMigrondiFactory: MigrondiConfig * string -> IMigrondiUI
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

        let migrondi =

          let projectRoot =
            let rootPath =
              Path.Combine(Path.GetTempPath(), project.id.ToString())

            try
              Directory.CreateDirectory(rootPath).FullName
            with ex ->
              logger.LogWarning(
                "Exception thrown while creating project root directory: {error}",
                ex
              )

              Path.GetFullPath rootPath

          logger.LogDebug(
            "Using project root directory: {projectRoot}",
            projectRoot
          )

          vMigrondiFactory(project.ToMigrondiConfig(), projectRoot)

        return VirtualProjectDetailsVM(logger, migrondi, project)
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
                  ProjectDetails
                    .MigrationsPanel(
                      currentShow = vm.CurrentShow,
                      migrations = vm.Migrations,
                      lastDryRun = vm.LastDryRun,
                      migrationsView = ProjectDetails.migrationListView,
                      dryRunView = ProjectDetails.dryRunListView
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
