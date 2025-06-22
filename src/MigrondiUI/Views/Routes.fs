module MigrondiUI.Views.Routes


open System
open Microsoft.Extensions.Logging

open Avalonia.Controls
open Avalonia.Interactivity
open NXUI
open NXUI.Extensions

open Navs.Avalonia

open MigrondiUI.MigrondiExt
open SukiUI.Controls
open SukiUI.Theme
open MigrondiUI.Components

[<NoComparison; NoEquality>]
type AppEnvironment = {
  lf: ILoggerFactory
  lProjects: MigrondiUI.Projects.ILocalProjectRepository
  vProjects: MigrondiUI.Projects.IVirtualProjectRepository
  vfs: MigrondiUI.VirtualFs.MigrondiUIFs
  vMigrondiFactory: Migrondi.Core.MigrondiConfig * string * Guid -> IMigrondiUI
  dialogManager: SukiUI.Dialogs.ISukiDialogManager
  toastManager: SukiUI.Toasts.ISukiToastManager
  window: SukiWindow
}


let private landingView appEnvironment =
  let {
        lf = lf
        lProjects = lProjects
        vProjects = vProjects
      } =
    appEnvironment

  let logger = lf.CreateLogger<Landing.LandingVM>()
  Landing.View(Landing.LandingVM(logger, lProjects, vProjects), logger)

let private newProjectView appEnvironment =
  let {
        lf = lf
        lProjects = lProjects
        vProjects = vProjects
        vfs = vfs
      } =
    appEnvironment

  let logger = lf.CreateLogger<NewProject.NewProjectVM>()

  NewProject.View(
    NewProject.NewProjectVM(logger, lProjects, vProjects, vfs),
    logger
  )

let private projectDetailsView appEnvironment =
  let { lf = lf; lProjects = projects } = appEnvironment
  let logger = lf.CreateLogger<LocalProjectDetails.LocalProjectDetailsVM>()
  let mLogger = lf.CreateLogger<Migrondi.Core.IMigrondi>()
  LocalProjectDetails.View(logger, mLogger, projects)

let private vProjectDetailsView appEnvironment =
  let {
        lf = lf
        vProjects = vProjects
        vMigrondiFactory = vMigrondiFactory
      } =
    appEnvironment

  let logger = lf.CreateLogger<VirtualProjectDetails.VirtualProjectDetailsVM>()
  VirtualProjectDetails.View(logger, vProjects, vMigrondiFactory)



type SidenavRoutes =
  | ProjectList
  | NewProject


type MigrondiUIAppHost(env: AppEnvironment) =
  inherit UserControl()

  let routesLogger = env.lf.CreateLogger<Routes>()

  let content =
    Routes(logger = routesLogger)
      .Children(
        Route(
          Route.define("landing", "/", landingView env)
          |> Navs.Route.cache Navs.NoCache
        ),
        Route("new-project", "/projects/new", newProjectView env),
        Route(
          "local-project-details",
          "/projects/local/:lProjectId<guid>",
          projectDetailsView env
        ),
        Route(
          "virtual-project-details",
          "/projects/virtual/:vProjectId<guid>",
          vProjectDetailsView env
        )
      )

  let sideMenu =
    SukiSideMenu(IsMenuExpanded = false)
      .HeaderContent(
        Border()
          .HorizontalAlignmentCenter()
          .MarginX(8)
          .Child(
            StackPanel()
              .HorizontalAlignmentCenter()
              .Spacing(4)
              .Children(
                TextBlock()
                  .Classes("h4", "Title")
                  .Text("Migrondi UI")
                  .HorizontalAlignmentCenter(),
                TextBlock()
                  .Classes("h6", "Caption")
                  .Text("Migrations Management Tool")
                  .HorizontalAlignmentCenter()
              )
          )
      )
      .FooterContent(
        Border()
          .HorizontalAlignmentCenter()
          .MarginX(8)
          .Child(
            TextBlock().Classes("h5", "Caption").Text("Powered by Migrondi")
          )
      )
      .MenuItems(
        SukiSideMenuItem()
          .Tag(ProjectList)
          .Icon(Icons.ListBox)
          .Header("Projects")
          .PageContent(content),
        SukiSideMenuItem()
          .Tag(NewProject)
          .Icon(Icons.DatabasePlus)
          .Header("New Project")
          .PageContent(content)
      )

  do
    sideMenu.ObserveSelectedItem()
    |> Observable.add(fun item ->
      match item with
      | :? SukiSideMenuItem as menuItem ->
        routesLogger.LogInformation(
          "Selected menu item: {Header}",
          menuItem.Header
        )

        match menuItem.Tag with
        | :? SidenavRoutes as route when route = ProjectList ->
          content.Router.Value.NavigateByName("landing")
          |> Async.AwaitTask
          |> Async.Ignore
          |> Async.StartImmediate
        | :? SidenavRoutes as route when route = NewProject ->
          content.Router.Value.NavigateByName("new-project")
          |> Async.AwaitTask
          |> Async.Ignore
          |> Async.StartImmediate
        | _ ->
          routesLogger.LogWarning(
            "Selected item does not match any known route: {Tag}",
            menuItem.Tag
          )
      | o ->
        routesLogger.LogWarning(
          "Selected item is not a SukiSideMenuItem: {Item}",
          o
        ))

    base.Classes.Add(nameof MigrondiUIAppHost)
    base.Name <- nameof MigrondiUIAppHost

    base.Content <- sideMenu
