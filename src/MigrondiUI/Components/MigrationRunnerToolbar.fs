module MigrondiUI.Components.MigrationRunnerToolbar

open System

open Avalonia
open Avalonia.Controls
open Avalonia.Styling
open FSharp.Data.Adaptive
open NXUI.Extensions
open Navs.Avalonia
open MigrondiUI.Components.ProjectDetails

let ApplyRunButton
  (onRunRequested: unit -> bool, onConfirmedRun: RunMigrationKind -> unit)
  : Control =
  Button()
    .Classes("Warning")
    .Content("Apply Pending")
    .OnClickHandler(fun _ _ ->
      if onRunRequested() then
        onConfirmedRun(RunMigrationKind.Up))

let RollbackRunButton
  (onRunRequested: unit -> bool, onConfirmedRun: RunMigrationKind -> unit)
  : Control =
  Button()
    .Classes("Warning")
    .Content("Rollback")
    .OnClickHandler(fun _ _ ->
      if onRunRequested() then
        onConfirmedRun(RunMigrationKind.Down))

let ApplyDryRunButton(onConfirmedRun: RunMigrationKind -> unit) : Control =
  Button()
    .Classes("Primary")
    .Content("Apply Pending (Dry Run)")
    .OnClickHandler(fun _ _ -> onConfirmedRun(RunMigrationKind.DryUp))

let RollbackDryRunButton(onConfirmedRun: RunMigrationKind -> unit) : Control =
  Button()
    .Classes("Primary")
    .Content("Rollback (Dry Run)")
    .OnClickHandler(fun _ _ -> onConfirmedRun(RunMigrationKind.DryDown))

let numericUpDown(steps: _ cval) : Control =
  NumericUpDown()
    .Minimum(0)
    .Value(steps |> AVal.toBinding)
    .Watermark("Amount to run")
    .OnValueChangedHandler(fun _ value ->
      match value.NewValue |> ValueOption.ofNullable with
      | ValueNone -> steps.setValue 1M
      | ValueSome value -> steps.setValue value)

let checkBox(dryRun: _ cval) : Control =
  CheckBox()
    .Content("Dry Run")
    .IsChecked(dryRun |> AVal.toBinding)
    .OnIsCheckedChangedHandler(fun checkbox _ ->

      let isChecked =
        checkbox.IsChecked
        |> ValueOption.ofNullable
        |> ValueOption.defaultValue true

      dryRun.setValue isChecked)



type MigrationsRunnerToolbar
  (onRunRequested: unit -> bool, onConfirmApply: RunMigrationKind * int -> unit) as this
  =
  inherit UserControl()
  let dryRun = cval false
  let steps = cval 1M

  let getIntValue() =
    try
      let v = steps.getValue() |> int
      if v < 0 then 1 else v
    with :? OverflowException ->
      1

  let applyPendingButton(dryRun: bool aval) =
    dryRun
    |> AVal.map(fun isDryRun ->
      if isDryRun then
        ApplyDryRunButton(fun kind -> onConfirmApply(kind, getIntValue()))
      else
        ApplyRunButton(

          onRunRequested,
          (fun kind -> onConfirmApply(kind, getIntValue()))
        ))
    |> AVal.toBinding

  let rollbackButton(dryRun: bool aval) =
    dryRun
    |> AVal.map(fun isDryRun ->
      if isDryRun then
        RollbackDryRunButton(fun kind -> onConfirmApply(kind, getIntValue()))
      else
        RollbackRunButton(
          onRunRequested,
          (fun kind -> onConfirmApply(kind, getIntValue()))
        ))
    |> AVal.toBinding

  do
    base.Classes.Add("MigrationsRunnerToolbar")

    base.Content <-
      StackPanel()
        .Classes("MigrationsRunnerToolbarPanel")
        .Children(
          Border().Child(applyPendingButton(dryRun)),
          Border().Child(rollbackButton(dryRun)),
          checkBox(dryRun),
          numericUpDown(steps)
        )

    this.ApplyStyles()

  member private this.ApplyStyles() =
    this.Styles.AddRange [
      // Main toolbar panel styles
      Style()
        .Selector(_.OfType<StackPanel>().Class("MigrationsRunnerToolbarPanel"))
        .SetStackLayoutOrientation(Layout.Orientation.Horizontal)
        .SetStackLayoutSpacing(8)
        .SetLayoutableMargin(Thickness(0, 4, 0, 8))

      // Button styles
      Style()
        .Selector(_.OfType<UserControl>().Name("ApplyPendingButton"))
        .SetLayoutableMargin(Thickness(0, 0, 4, 0))

      Style()
        .Selector(_.OfType<UserControl>().Name("RollbackButton"))
        .SetLayoutableMargin(Thickness(0, 0, 8, 0))

      // CheckBox styles
      Style()
        .Selector(_.OfType<CheckBox>())
        .SetLayoutableMargin(Thickness(0, 0, 8, 0))
        .SetLayoutableVerticalAlignment(Layout.VerticalAlignment.Center)

      // NumericUpDown styles
      Style()
        .Selector(_.OfType<NumericUpDown>())
        .SetLayoutableWidth(120)
        .SetLayoutableVerticalAlignment(Layout.VerticalAlignment.Center)
    ]
