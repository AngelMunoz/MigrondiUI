module MigrondiUI.Views.NewProject

open System
open System.IO
open System.Text.Json

open Microsoft.Extensions.Logging

open IcedTasks

open Avalonia
open Avalonia.Controls
open Avalonia.Layout
open Avalonia.Platform.Storage
open Avalonia.Styling

open NXUI.Extensions

open FSharp.UMX
open FsToolkit.ErrorHandling

open Navs
open Navs.Avalonia
open Migrondi.Core
open MigrondiUI
open MigrondiUI.Projects
open MigrondiUI.Database
open MigrondiUI.Components.NewVirtualProjectForm
open MigrondiUI.VirtualFs
open SukiUI.Controls

type LocalProjectTarget =
  | CreateLocal
  | ImportToVirtual

type NewProjectVM
  (
    logger: ILogger<NewProjectVM>,
    projects: ILocalProjectRepository,
    vProjects: IVirtualProjectRepository,
    vfs: MigrondiUIFs
  ) =

  do logger.LogDebug "NewProjectVM created"

  member _.LoadLocalProject(view: Control) = asyncOption {
    let! token = Async.CancellationToken
    logger.LogDebug "Loading local project"
    let! topLevel = TopLevel.GetTopLevel(view)

    let! file =
      topLevel.StorageProvider.OpenFilePickerAsync(
        FilePickerOpenOptions(
          Title = "Select Project File",
          AllowMultiple = false,
          FileTypeFilter = [|
            FilePickerFileType(
              "Migrondi Config",
              Patterns = [ "migrondi.json" ]
            )
          |]
        )
      )

    let! file = file |> Seq.tryHead
    let! parentFolder = file.GetParentAsync()
    let! parentFolder = parentFolder
    let name = parentFolder.Name

    logger.LogDebug("Selected file: {File}", file.Name)

    let! lpid =
      projects.InsertProject (name, configPath = file.Path.LocalPath) token

    return lpid
  }

  member _.CreateNewLocalProject(view) = asyncOption {
    logger.LogDebug "Creating new local project"
    let! token = Async.CancellationToken
    let! topLevel = TopLevel.GetTopLevel view

    let! directory = asyncOption {
      let! directory =
        topLevel.StorageProvider.OpenFolderPickerAsync(
          FolderPickerOpenOptions(AllowMultiple = false)
        )

      let! selected = directory |> Seq.tryHead

      return! selected.TryGetLocalPath()
    }

    let config = MigrondiConfig.Default

    logger.LogDebug("Selected directory: {Directory}", directory)

    try
      Directory.CreateDirectory(Path.Combine(directory, "migrations")) |> ignore
    with
    | :? IOException
    | :? UnauthorizedAccessException as ex ->

      logger.LogWarning(
        "Failed to create migrations directory: {Message}",
        ex.Message
      )

    let configPath = Path.Combine(directory, "migrondi.json")
    logger.LogDebug("Creating config file in {configfile}", configPath)

    File.WriteAllText(
      configPath,
      Json
        .migrondiConfigEncoder(config)
        .ToJsonString(
          JsonSerializerOptions(WriteIndented = true, IndentSize = 2)
        )
    )

    let! dirName = Path.GetFileNameWithoutExtension directory

    logger.LogDebug("Inserting local project with name {Name}", dirName)

    logger.LogDebug(
      "Inserting local project with config path {Path}",
      directory
    )

    logger.LogDebug("Inserting local project with config {Config}", config)

    let configPath = Path.Combine(directory, "migrondi.json")

    let! pid = projects.InsertProject (dirName, configPath = configPath) token

    logger.LogDebug("Inserted local project with id {Id}", pid)
    return pid
  }

  member _.CreateNewVirtualProject(args: NewVirtualProjectArgs) = asyncEx {
    logger.LogDebug "Creating new virtual project"
    let! pid = vProjects.InsertProject(args)
    logger.LogDebug("Inserted virtual project with id {Id}", pid)
    return pid
  }

  member _.ImportToVirtualProject(view: Control) = asyncOption {
    logger.LogDebug "Importing local project to virtual project"
    let! token = Async.CancellationToken
    let! topLevel = TopLevel.GetTopLevel(view)

    let! file =
      topLevel.StorageProvider.OpenFilePickerAsync(
        FilePickerOpenOptions(
          Title = "Select Project File",
          AllowMultiple = false,
          FileTypeFilter = [|
            FilePickerFileType(
              "Migrondi Config",
              Patterns = [ "migrondi.json" ]
            )
          |]
        )
      )

    let! file = file |> Seq.tryHead
    logger.LogDebug("Selected file: {File}", file.Name)
    let! vpid = vfs.ImportFromLocal file.Path.LocalPath token
    return vpid
  }

type LocalProjectTab
  (
    handleSelectLocalProject: unit -> unit,
    handleCreateNewLocalProject: unit -> unit
  ) as this =
  inherit UserControl()


  do
    this.Name <- nameof LocalProjectTab

    this.Content <-
      StackPanel()
        .Classes("LocalProjectTab_StackPanel")
        .Children(
          Button()
            .Content("Create New Local Project")
            .OnClickHandler(fun _ _ -> handleCreateNewLocalProject()),
          Button()
            .Content("Select Local Project")
            .OnClickHandler(fun _ _ -> handleSelectLocalProject())
        )

    this.ApplyStyles()

  member private this.ApplyStyles() =
    this.Styles.AddRange [
      Style()
        .Selector(_.OfType<StackPanel>().Class("LocalProjectTab_StackPanel"))
        .SetStackLayoutSpacing(10.0)
        .SetLayoutableMargin(Thickness 10.0)
    ]

type NewProjectView(vm: NewProjectVM, logger: ILogger, nav: INavigable<Control>) as this
  =
  inherit UserControl()

  let handleSelectLocalProject(target: LocalProjectTarget) = asyncEx {
    let! projectId = asyncEx {
      match target with
      | CreateLocal ->
        let! guid = vm.LoadLocalProject this
        return guid |> Option.map UMX.untag
      | ImportToVirtual ->
        let! guid = vm.ImportToVirtualProject this
        return guid |> Option.map(UMX.untag)
    }

    match projectId with
    | None ->
      logger.LogWarning "No project id returned"
      return ()
    | Some projectId ->

      let target =
        match target with
        | CreateLocal -> "local"
        | ImportToVirtual -> "virtual"

      match! nav.Navigate $"/projects/{target}/{projectId}" with
      | Ok _ -> ()
      | Error(e) ->
        logger.LogWarning("Navigation Failure: {error}", e.StringError())
  }

  let handleCreateNewLocalProject() = asyncEx {
    let! projectId = vm.CreateNewLocalProject this

    match projectId with
    | None ->
      logger.LogWarning "No project id returned"
      return ()
    | Some projectId ->
      match! nav.Navigate $"/projects/local/%s{projectId.ToString()}" with
      | Ok _ -> ()
      | Error(e) ->
        logger.LogWarning("Navigation Failure: {error}", e.StringError())
  }

  let handleCreateVirtualProject(args: NewVirtualProjectArgs) = asyncEx {
    let! createdId = vm.CreateNewVirtualProject args

    match! nav.Navigate $"/projects/virtual/{createdId}" with
    | Ok _ -> ()
    | Error(e) ->
      logger.LogWarning("Navigation Failure: {error}", e.StringError())
  }


  let tabControl =
    TabControl()
      .ItemsSource(
        TabItem()
          .Header("Local Project")
          .Content(
            GlassCard()
              .Classes("Accent")
              .Content(
                LocalProjectTab(
                  (fun () ->
                    handleSelectLocalProject CreateLocal
                    |> Async.StartImmediate),
                  (fun () ->
                    handleCreateNewLocalProject() |> Async.StartImmediate)
                )
              )
          ),
        TabItem()
          .Header("Virtual Project")
          .Content(
            GlassCard()
              .Classes("Accent")
              .Content(
                NewVirtualProjectForm(
                  handleCreateVirtualProject >> Async.StartImmediate,
                  (fun () ->
                    handleSelectLocalProject ImportToVirtual
                    |> Async.StartImmediate)
                )
              )
          )
      )

  do

    this.Name <- nameof NewProjectView

    this.Content <-
      GlassCard().Classes("NewProjectView_GlassCard").Content(tabControl)

    this.ApplyStyles()

  member private this.ApplyStyles() =
    this.Styles.AddRange [
      Style()
        .Selector(_.OfType<GlassCard>().Class("NewProjectView_GlassCard"))
        .SetLayoutableMargin(Thickness 12.0)
    ]

let View
  (vm: NewProjectVM, logger: ILogger)
  _
  (nav: INavigable<Control>)
  : Control =
  NewProjectView(vm, logger, nav)
