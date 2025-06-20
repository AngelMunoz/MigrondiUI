module MigrondiUI.Views.Landing

open System

open Microsoft.Extensions.Logging


open Avalonia
open Avalonia.Controls
open Avalonia.Controls.Templates
open Avalonia.Styling
open Avalonia.Layout

open SukiUI.Controls
open NXUI.Extensions

open IcedTasks
open FsToolkit.ErrorHandling
open FSharp.Data.Adaptive

open Navs
open Navs.Avalonia

open MigrondiUI
open MigrondiUI.Projects


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
    do! Async.Sleep(1000)
    // Simulate removal logic
    do! this.LoadProjects()
    viewState.setValue Idle
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
    FuncDataTemplate<Project>(fun project _ ->
      let icon =
        match project with
        | Local _ -> "💾 (Local)"
        | Virtual _ -> "💻 (Virtual)"

      StackPanel()
        .Classes("RepositoryList_Item")
        .Tag(project.Id)
        .Children(
          TextBlock().Text $"{project.Name} - {icon}",
          TextBlock().Text(defaultArg project.Description "No description")
        ))

  do
    base.Name <- nameof RepositoryList
    base.Classes.Add("RepositoryList")

    base.Content <-
      ScrollViewer()
        .Content(
          projects
          |> AVal.map(fun projects ->
            match projects with
            | [] -> emptyProjectsView
            | projects ->
              ListBox()
                .MultipleSelection()
                .ItemsSource(projects)
                .ItemTemplate(repositoryItem)
                .OnSelectionChangedHandler(fun sender _ ->
                  match sender.SelectedItems with
                  | null -> onSelectionChanged Seq.empty
                  | selected ->
                    selected |> Seq.cast<Project> |> onSelectionChanged))
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

    let item = vm.SelectedItems |> AVal.force |> Seq.exactlyOne

    match action with
    | Edit ->
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
      let route =
        match item with
        | Local _ -> $"/projects/local/{item.Id}"
        | Virtual _ -> $"/projects/virtual/{item.Id}"

      let! res = nav.Navigate(route)

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

    this.ApplyStyles()

  member private this.ApplyStyles() =
    this.Styles.AddRange [
      Style()
        .Selector(_.OfType<Grid>().Class("LandingPage_Grid"))
        .SetGridRowSpacing(12.0)
        .SetLayoutableMargin(Thickness(12.0, 2.0, 12.0, 2.0))
    ]


let View (vm: LandingVM, logger: ILogger) _ nav : Control =
  LandingPage(vm, logger, nav)
