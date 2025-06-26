module MigrondiUI.Components.MigrationRunnerToolbar

open System

open Avalonia
open Avalonia.Controls
open Avalonia.Layout
open Avalonia.Styling
open FSharp.Data.Adaptive
open IcedTasks
open NXUI.Extensions
open Navs.Avalonia
open MigrondiUI.Components.ProjectDetails

type ApplyPendingButtonArgs = {
  kind: RunMigrationKind aval
  onMigrationRunRequested: RunMigrationKind -> Async<bool>
  onApplyMigrations: RunMigrationKind -> Async<unit>
}

type ApplyPendingButton(args: ApplyPendingButtonArgs) =
  inherit UserControl()
  let isEnabled = cval true

  let onRunMigrationsRequested kind =
    asyncEx {
      isEnabled.setValue false
      let! runApproved = args.onMigrationRunRequested kind

      if runApproved then
        do! args.onApplyMigrations kind

      isEnabled.setValue true
      return ()
    }
    |> Async.StartImmediate

  let text =
    args.kind
    |> AVal.map(fun kind ->
      match kind with
      | RunMigrationKind.DryUp -> "Apply Pending (Dry Run)"
      | RunMigrationKind.DryDown -> "Rollback Pending (Dry Run)"
      | RunMigrationKind.Up -> "Apply Pending"
      | RunMigrationKind.Down -> "Rollback Pending")

  do
    base.Name <- nameof ApplyPendingButton

    base.Content <-
      Button()
        .Classes("Accent", "Outlined")
        .IsEnabled(isEnabled |> AVal.toBinding)
        .ShowProgress(isEnabled |> AVal.map not |> AVal.toBinding)
        .Content(text |> AVal.toBinding)
        .OnClickHandler(fun _ _ ->
          onRunMigrationsRequested(args.kind |> AVal.force))

let numericUpDown(steps: _ cval) =
  NumericUpDown()
    .Minimum(0)
    .Value(steps |> AVal.toBinding)
    .Watermark("Amount to run")
    .OnValueChangedHandler(fun _ value ->
      match value.NewValue |> ValueOption.ofNullable with
      | ValueNone -> steps.setValue 1M
      | ValueSome value -> steps.setValue value)

let checkBox(dryRun: _ cval) =
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
  (
    onRunMigrationsRequested: RunMigrationKind -> Async<bool>,
    onApplyMigrations: RunMigrationKind * int -> Async<unit>
  ) as this =
  inherit UserControl()
  let dryRun = cval false
  let steps = cval 1M

  let getIntValue() =
    try
      let v = steps.getValue() |> int
      if v < 0 then 1 else v
    with :? OverflowException ->
      1

  let upArgs = {
    kind =
      dryRun
      |> AVal.map(fun d ->
        if d then RunMigrationKind.DryUp else RunMigrationKind.Up)
    onMigrationRunRequested = onRunMigrationsRequested
    onApplyMigrations = fun kind -> onApplyMigrations(kind, getIntValue())
  }

  let downArgs = {
    kind =
      dryRun
      |> AVal.map(fun d ->
        if d then
          RunMigrationKind.DryDown
        else
          RunMigrationKind.Down)
    onMigrationRunRequested = onRunMigrationsRequested
    onApplyMigrations = fun kind -> onApplyMigrations(kind, getIntValue())
  }

  do
    base.Classes.Add("MigrationsRunnerToolbar")

    base.Content <-
      StackPanel()
        .Classes("MigrationsRunnerToolbarPanel")
        .Children(
          ApplyPendingButton(upArgs),
          ApplyPendingButton(downArgs),
          checkBox(dryRun),
          numericUpDown(steps)
        )

    this.ApplyStyles()

  member private this.ApplyStyles() =
    this.Styles.AddRange [
      // Main toolbar panel styles
      Style()
        .Selector(_.OfType<StackPanel>().Class("MigrationsRunnerToolbarPanel"))
        .SetStackLayoutOrientation(Orientation.Horizontal)
        .SetStackLayoutSpacing(8)
        .SetLayoutableMargin(Thickness(0, 4, 0, 8))
        .SetLayoutableVerticalAlignment(VerticalAlignment.Center)

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
        .SetLayoutableVerticalAlignment(VerticalAlignment.Center)

      // NumericUpDown styles
      Style()
        .Selector(_.OfType<NumericUpDown>())
        .SetLayoutableWidth(120)
        .SetLayoutableVerticalAlignment(VerticalAlignment.Center)
    ]
