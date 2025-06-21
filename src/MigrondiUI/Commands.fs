module MigrondiUI.Commands

open System
open System.Windows.Input
open FSharp.Data.Adaptive

type DisposableCommand(canExecute: bool aval, action: unit -> unit) as this =

  let event = new Event<EventHandler, EventArgs>()
  let mutable _canExecute = canExecute |> AVal.force

  let sub =
    canExecute.AddCallback(fun canExecute ->
      if canExecute <> _canExecute then
        _canExecute <- canExecute
        event.Trigger(this, EventArgs.Empty))

  interface ICommand with
    member this.CanExecute(parameter: obj) = AVal.force canExecute

    member this.Execute(parameter: obj) = action()

    [<CLIEvent>]
    member this.CanExecuteChanged = event.Publish

  interface IDisposable with
    member this.Dispose() = sub.Dispose()

type SimpleCommand(canExecute: unit -> bool, action: unit -> unit) =
  let event = new Event<EventHandler, EventArgs>()

  interface ICommand with
    member this.CanExecute(_) = canExecute()

    member this.Execute(_) = action()

    [<CLIEvent>]
    member this.CanExecuteChanged = event.Publish


type Command =

  static member inline OfAVal
    (canExecute: bool aval, execute: unit -> unit)
    : ICommand =
    new DisposableCommand(canExecute, execute)

  static member inline OfFunc
    (canExecute: unit -> bool, execute: unit -> unit)
    : ICommand =
    SimpleCommand(canExecute, execute)

  static member inline OfAction action : ICommand =
    SimpleCommand((fun () -> true), action)
