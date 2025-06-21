module MigrondiUI.Views.Landing

open System

open System.Windows.Input
open Microsoft.Extensions.Logging


open Avalonia
open Avalonia.Controls
open Avalonia.Controls.Templates
open Avalonia.Input
open Avalonia.Styling
open Avalonia.Layout

open SukiUI.Controls
open NXUI
open NXUI.Extensions
open NXUI.Interactivity

open IcedTasks
open FsToolkit.ErrorHandling
open FSharp.Data.Adaptive

open Navs
open Navs.Avalonia

open MigrondiUI
open MigrondiUI.Projects
open MigrondiUI.Commands
open System.Diagnostics


type Action =
  | Edit
  | Remove
  | Visit

type ViewState =
  | Idle
  | Loading
  | Error of exn

type LandingVM
  (
    logger: ILogger<LandingVM>,
    projects: ILocalProjectRepository,
    vProjects: IVirtualProjectRepository
  ) =

  let _projects: Project list cval = cval []

  let selectedItems: Project seq cval = cval Seq.empty

  let viewState = cval Idle

  do logger.LogDebug "LandingVM created"

  member _.ViewState: ViewState aval = viewState

  member _.SelectedItems: Project seq aval = selectedItems

  member _.Projects: Project list aval = _projects

  member _.LoadProjects() = asyncEx {
    logger.LogDebug "Loading projects"
    viewState.setValue Loading

    try
      let! projects = projects.GetProjects()
      let! vProjects = vProjects.GetProjects()

      _projects.setValue [
        yield! projects |> List.map(fun p -> Local p)
        yield! vProjects |> List.map(fun p -> Virtual p)
      ]

      logger.LogInformation(
        "Projects loaded successfully: {Count}",
        _projects.Value.Length
      )

      viewState.setValue Idle
      return ()
    with ex ->
      logger.LogError(ex, "Failed to load projects")
      viewState.setValue(Error ex)
      return ()
  }

  member this.RemoveSelectedItems() = asyncEx {
    viewState.setValue Loading
    let items = selectedItems |> AVal.force

    logger.LogDebug("Removing selected items: {Items}", items)

    if Seq.isEmpty items then
      logger.LogWarning "No items selected for removal"
      return ()

    logger.LogDebug("Removing items: {Items}", items)

    try
      do! projects.RemoveProjects(items)
      this.SetSelectedItems Seq.empty

      logger.LogInformation("Successfully removed selected items")
      do! this.LoadProjects()
      viewState.setValue Idle
      return ()
    with ex ->
      logger.LogError(ex, "Failed to remove selected items")
      viewState.setValue(Error ex)
      do! this.LoadProjects()
      return ()
  }

  member _.SetSelectedItems(items: Project seq) =
    logger.LogTrace("Setting selected items to {Items}", items)
    selectedItems.setValue items

  member _.SetLoading() =
    logger.LogTrace "Setting view state to Loading"
    viewState.setValue Loading

  member _.SetIdle() =
    logger.LogTrace "Setting view state to Idle"
    viewState.setValue Idle

  member _.SetError(ex: exn) =
    logger.LogError(ex, "An error occurred in LandingVM")
    viewState.setValue(Error ex)


type ActionsBar
  (selectedProjects: Project seq aval, actAgainstSelected: Action -> Async<unit>) as this
  =
  inherit UserControl()

  let progress = cval false
  let projects = selectedProjects |> AVal.map Seq.toList

  let visitProjectBtn =
    Button()
      .Content("Visit Project")
      .ShowProgress(progress |> AVal.toBinding)
      .IsEnabled(
        projects
        |> AVal.map2
          (fun inProgress projectList ->
            let isEnabled = projectList |> List.tryExactlyOne |> Option.isSome

            isEnabled && not inProgress)
          progress
        |> AVal.toBinding
      )
      .OnClickHandler(fun _ _ ->
        asyncEx {
          progress.setValue true
          do! actAgainstSelected Visit
          progress.setValue false

        }
        |> Async.StartImmediate)

  let editProjectBtn =
    Button()
      .Content("Edit")
      .ShowProgress(progress |> AVal.toBinding)
      .IsEnabled(
        projects
        |> AVal.map2
          (fun inProgress projectList ->
            let isEnabled = projectList |> List.tryExactlyOne |> Option.isSome

            isEnabled && not inProgress)
          progress
        |> AVal.toBinding
      )
      .OnClickHandler(fun _ _ ->
        asyncEx {
          progress.setValue true
          do! actAgainstSelected Edit
          progress.setValue false
        }
        |> Async.StartImmediate)

  let removeBtn =
    Button()
      .Classes("Danger")
      .Content("Remove")
      .ShowProgress(progress |> AVal.toBinding)
      .IsEnabled(
        projects
        |> AVal.map2
          (fun inProgress projects -> projects.Length > 0 && not inProgress)
          progress
        |> AVal.toBinding
      )
      .OnClickHandler(fun _ _ ->
        asyncEx {
          progress.setValue true
          do! actAgainstSelected Remove
          progress.setValue false
        }
        |> Async.StartImmediate)

  do
    base.Classes.Add("ActionsBar")
    base.Name <- nameof ActionsBar

    base.Content <-
      StackPanel()
        .Classes("ActionsBar_StackPanel")
        .Children(visitProjectBtn, editProjectBtn, removeBtn)

    this.ApplyStyles()

  member private this.ApplyStyles() =
    this.Styles.AddRange [
      Style()
        .Selector(_.OfType<StackPanel>().Class("ActionsBar_StackPanel"))
        .SetStackLayoutOrientation(Orientation.Horizontal)
        .SetStackLayoutSpacing
        4.0
    ]


type RepositoryList(projects: Project list aval, onSelectionChanged) as this =
  inherit UserControl()

  let emptyProjectsView: Control =
    StackPanel()
      .Classes("RepositoryList_EmptyView")
      .Children(TextBlock().Text("No projects available"))

  let repositoryItem =
    FuncDataTemplate<Project | null>(fun project _ ->
      let icon =
        match project with
        | Local _ -> "💾 (Local)"
        | Virtual _ -> "💻 (Virtual)"

      if project = null then
        UserControl()
      else
        let project = nonNull project

        StackPanel()
          .Classes("RepositoryList_Item")
          .Children(
            TextBlock().Text $"{project.Name} - {icon}",
            TextBlock().Text(defaultArg project.Description "No description")
          ))

  let projectList =
    ListBox()
      .MultipleSelection()
      .ItemsSource(projects |> AVal.toBinding)
      .ItemTemplate(repositoryItem)
      .OnSelectionChangedHandler(fun sender _ ->
        match sender.SelectedItems with
        | null -> onSelectionChanged Seq.empty
        | selected -> selected |> Seq.cast<Project> |> onSelectionChanged)

  do
    base.Name <- nameof RepositoryList
    base.Classes.Add("RepositoryList")

    base.KeyBindings.Add(
      KeyBinding()
        .Gesture(KeyGesture Key.Escape)
        .Command(
          Command.OfAction(fun () ->
            match projectList.SelectedItems with
            | null -> ()
            | selected -> selected.Clear())
        )
    )


    base.Content <-
      ScrollViewer()
        .Content(
          projects
          |> AVal.map(fun projects ->
            match projects with
            | [] -> emptyProjectsView
            | _ -> projectList)
          |> AVal.toBinding
        )

    this.ApplyStyles()

  member private this.ApplyStyles() =
    this.Styles.AddRange [
      Style()
        .Selector(_.OfType<StackPanel>().Class("RepositoryList_EmptyView"))
        .SetStackLayoutSpacing(5.0)
        .SetLayoutableMargin(Thickness 5.0)
        .SetStackLayoutOrientation
        Orientation.Vertical
      Style()
        .Selector(_.OfType<StackPanel>().Class("RepositoryList_Item"))
        .SetStackLayoutSpacing(5.0)
        .SetLayoutableMargin(Thickness 5.0)
        .SetStackLayoutOrientation
        Orientation.Vertical
    ]

type LandingPage(vm: LandingVM, logger: ILogger, nav: INavigable<Control>) as this
  =
  inherit UserControl()

  let actAgainstSelected(action: Action) = asyncEx {
    logger.LogInformation("Action triggered: {Action}", action)
    vm.SetLoading()


    match action with
    | Edit ->
      let item = vm.SelectedItems |> AVal.force |> Seq.exactlyOne

      let type' =
        match item with
        | Local _ -> "local"
        | Virtual _ -> "virtual"

      let! res =
        nav.NavigateByName("edit-project", Map.ofList [ "type", type' ])

      res
      |> Result.teeError(fun error ->
        logger.LogWarning("Navigation Failure: {error}", error.StringError()))
      |> Result.iter vm.SetIdle

      return ()
    | Visit ->
      let item = vm.SelectedItems |> AVal.force |> Seq.exactlyOne

      let route =
        match item with
        | Local _ -> $"/projects/local/{item.ProjectId}"
        | Virtual _ -> $"/projects/virtual/{item.ProjectId}"

      let! res = nav.Navigate route

      res
      |> Result.teeError(fun error ->
        logger.LogWarning("Navigation Failure: {error}", error.StringError()))
      |> Result.iter vm.SetIdle

      return ()
    | Remove ->
      logger.LogInformation "Remove action triggered"
      do! vm.RemoveSelectedItems()
      return ()
  }

  let progressIndicator =
    UserControl()
      .Content(
        vm.ViewState
        |> AVal.map(fun state ->
          match state with
          | Error _
          | Idle -> UserControl() :> Control
          | Loading -> ProgressBar().IsIndeterminate true)
        |> AVal.toBinding
      )

  let actions =
    GlassCard().Content(ActionsBar(vm.SelectedItems, actAgainstSelected)).Row(1)

  let repoList =
    GlassCard().Content(RepositoryList(vm.Projects, vm.SetSelectedItems)).Row(2)

  do
    vm.LoadProjects() |> Async.StartImmediate

    base.Name <- nameof LandingPage
    base.Classes.Add "LandingPage"

    base.Content <-
      Grid()
        .Classes("LandingPage_Grid")
        .RowDefinitions("2,Auto,Auto")
        .Children(progressIndicator, actions, repoList)

    this.AddKeyBindings()
    this.ApplyStyles()

  member private this.ApplyStyles() =
    this.Styles.AddRange [
      Style()
        .Selector(_.OfType<Grid>().Class("LandingPage_Grid"))
        .SetGridRowSpacing(12.0)
        .SetLayoutableMargin(Thickness(12.0, 2.0, 12.0, 2.0))
    ]

  member private this.AddKeyBindings() =
    this.KeyBindings.AddRange [
      KeyBinding()
        .Gesture(KeyGesture Key.F5)
        .Command(
          Command.OfAction(fun () -> vm.LoadProjects() |> Async.StartImmediate)
        )
      KeyBinding()
        .Gesture(KeyGesture Key.Delete)
        .Command(
          Command.OfAVal(
            vm.SelectedItems |> AVal.map(Seq.isEmpty >> not),
            fun () -> actAgainstSelected Remove |> Async.StartImmediate
          )
        )
      KeyBinding()
        .Gesture(KeyGesture Key.Enter)
        .Command(
          Command.OfAVal(
            vm.SelectedItems |> AVal.map(Seq.tryExactlyOne >> Option.isSome),
            fun () -> actAgainstSelected Visit |> Async.StartImmediate
          )
        )
      KeyBinding()
        .Gesture(KeyGesture(Key.Enter, KeyModifiers.Shift))
        .Command(
          Command.OfAVal(
            vm.SelectedItems |> AVal.map(Seq.tryExactlyOne >> Option.isSome),
            fun () -> actAgainstSelected Edit |> Async.StartImmediate
          )
        )
    ]


let View (vm: LandingVM, logger: ILogger) _ nav : Control =
  LandingPage(vm, logger, nav)
