module MigrationRunnerToolbarTests

open MigrondiUI.Components.ProjectDetails
open Xunit
open Avalonia.Controls
open Avalonia.Headless.XUnit
open MigrondiUI.Components.MigrationRunnerToolbar
open FSharp.Data.Adaptive
open Avalonia.Interactivity



[<AvaloniaFact>]
let ``Pending Button with RunMigrationKind.Up will have an apply pending button``() =
  let mutable clicked = false
  let mutable clicked2 = false

  let pendingButton =
    ApplyPendingButton({
      kind = AVal.constant(RunMigrationKind.Up)
      onMigrationRunRequested = (fun _ -> async { clicked <- true; return true })
      onApplyMigrations = (fun _ -> async { clicked2 <- true; return () })
    })

  let confirmBtn = pendingButton.Content :?> Button
  Assert.NotNull(confirmBtn)
  Assert.Equal("Apply Pending", $"{confirmBtn.Content}")

  confirmBtn.RaiseEvent(RoutedEventArgs(Button.ClickEvent))

  Assert.True(clicked)
  Assert.True(clicked2)

[<AvaloniaFact>]
let ``Pending Button with RunMigrationKind.DryUp will have a dry run button``() =
  let mutable clicked = false
  let mutable clicked2 = false

  let pendingButton =
    ApplyPendingButton({
      kind = AVal.constant(RunMigrationKind.DryUp)
      onMigrationRunRequested = (fun _ -> async { clicked <- true; return true })
      onApplyMigrations = (fun _ -> async { clicked2 <- true; return () })
    })

  let button = pendingButton.Content :?> Button

  Assert.NotNull(button)
  Assert.Equal("Apply Pending (Dry Run)", $"{button.Content}")

  button.RaiseEvent(RoutedEventArgs(Button.ClickEvent))

  Assert.True(clicked)
  Assert.True(clicked2)


[<AvaloniaFact>]
let ``Rollback Button with RunMigrationKind.Down will have a confirm rollback button``
  ()
  =
  let mutable clicked = false
  let mutable clicked2 = false

  let buttonControl =
    ApplyPendingButton({
      kind = AVal.constant(RunMigrationKind.Down)
      onMigrationRunRequested = (fun _ -> async { clicked <- true; return true })
      onApplyMigrations = (fun _ -> async { clicked2 <- true; return () })
    })

  let button = buttonControl.Content :?> Button
  Assert.NotNull(button)
  Assert.Equal("Rollback Pending", $"{button.Content}")

  button.RaiseEvent(RoutedEventArgs(Button.ClickEvent))

  Assert.True(clicked)
  Assert.True(clicked2)

[<AvaloniaFact>]
let ``Rollback Button with RunMigrationKind.DryDown will have a dry run button``() =
  let mutable clicked = false
  let mutable clicked2 = false

  let buttonControl =
    ApplyPendingButton({
      kind = AVal.constant(RunMigrationKind.DryDown)
      onMigrationRunRequested = (fun _ -> async { clicked <- true; return true })
      onApplyMigrations = (fun _ -> async { clicked2 <- true; return () })
    })

  let button = buttonControl.Content :?> Button

  Assert.NotNull(button)
  Assert.Equal("Rollback Pending (Dry Run)", $"{button.Content}")

  button.RaiseEvent(RoutedEventArgs(Button.ClickEvent))

  Assert.True(clicked)
  Assert.True(clicked2)

[<AvaloniaFact>]
let ``NumericUpDown will update cval on change``() =
  let steps = cval 1M
  let numUpDown = numericUpDown(steps)

  numUpDown.Value <- 10M

  numUpDown.SetCurrentValue(NumericUpDown.ValueProperty, 10M)

  let actual = steps |> AVal.force

  Assert.Equal(10M, actual)

[<AvaloniaFact>]
let ``CheckBox will update cval on change``() =
  let dryRun = cval false
  let chkBox = checkBox(dryRun)

  chkBox.SetCurrentValue(CheckBox.IsCheckedProperty, true)

  let actual = dryRun |> AVal.force
  Assert.True(actual)

[<AvaloniaFact>]
let ``MigrationsRunnerToolbar - Apply button with dryRun enabled calls back with RunMigrationKind.DryUp``
  ()
  =
  let mutable calledKind = Unchecked.defaultof<RunMigrationKind>
  let mutable calledSteps = 0

  let toolbar =
    MigrationsRunnerToolbar(
      (fun _ -> async { return true }),
      (fun (kind, steps) ->
        calledKind <- kind
        calledSteps <- steps
        async { return () }))

  // Find the checkbox and check it
  let checkbox =
    toolbar.Content :?> StackPanel |> fun sp -> sp.Children[2] :?> CheckBox

  checkbox.SetCurrentValue(CheckBox.IsCheckedProperty, true)

  // Find and click the apply button
  let applyButton =
    toolbar.Content :?> StackPanel
    |> fun sp -> sp.Children[0] :?> UserControl
    |> fun uc -> uc.Content :?> Button

  applyButton.RaiseEvent(RoutedEventArgs(Button.ClickEvent))

  Assert.Equal(RunMigrationKind.DryUp, calledKind)
  Assert.Equal(1, calledSteps) // Default value

[<AvaloniaFact>]
let ``MigrationsRunnerToolbar - Apply button with dryRun disabled calls back with RunMigrationKind.Up after confirmation``
  ()
  =
  let mutable calledKind = Unchecked.defaultof<RunMigrationKind>
  let mutable calledSteps = 0

  let toolbar =
    MigrationsRunnerToolbar(
      (fun _ -> async { return true }),
      (fun (kind, steps) ->
        calledKind <- kind
        calledSteps <- steps
        async { return () }))

  // Find and click the apply button
  let applyButton =
    toolbar.Content :?> StackPanel
    |> fun sp -> sp.Children[0] :?> UserControl
    |> fun uc -> uc.Content :?> Button

  applyButton.RaiseEvent(RoutedEventArgs(Button.ClickEvent))

  Assert.Equal(RunMigrationKind.Up, calledKind)
  Assert.Equal(1, calledSteps) // Default value

[<AvaloniaFact>]
let ``MigrationsRunnerToolbar - Rollback button with dryRun enabled calls back with RunMigrationKind.DryDown``
  ()
  =
  let mutable calledKind = Unchecked.defaultof<RunMigrationKind>
  let mutable calledSteps = 0

  let toolbar =
    MigrationsRunnerToolbar(
      (fun _ -> async { return true }),
      (fun (kind, steps) ->
        calledKind <- kind
        calledSteps <- steps
        async { return () }))

  // Find the checkbox and check it
  let checkbox =
    toolbar.Content :?> StackPanel |> fun sp -> sp.Children[2] :?> CheckBox

  checkbox.SetCurrentValue(CheckBox.IsCheckedProperty, true)

  // Find and click the rollback button
  let rollbackButton =
    toolbar.Content :?> StackPanel
    |> fun sp -> sp.Children[1] :?> UserControl
    |> fun uc -> uc.Content :?> Button

  rollbackButton.RaiseEvent(RoutedEventArgs(Button.ClickEvent))

  Assert.Equal(RunMigrationKind.DryDown, calledKind)
  Assert.Equal(1, calledSteps) // Default value

[<AvaloniaFact>]
let ``MigrationsRunnerToolbar - Rollback button with dryRun disabled calls back with RunMigrationKind.Down after confirmation``
  ()
  =
  let mutable calledKind = Unchecked.defaultof<RunMigrationKind>
  let mutable calledSteps = 0

  let toolbar =
    MigrationsRunnerToolbar(
      (fun _ -> async { return true }),
      (fun (kind, steps) ->
        calledKind <- kind
        calledSteps <- steps
        async { return () }))

  // Find and click the rollback button
  let rollbackButton =
    toolbar.Content :?> StackPanel
    |> fun sp -> sp.Children[1] :?> UserControl
    |> fun uc -> uc.Content :?> Button

  rollbackButton.RaiseEvent(RoutedEventArgs(Button.ClickEvent))

  Assert.Equal(RunMigrationKind.Down, calledKind)
  Assert.Equal(1, calledSteps) // Default value

[<AvaloniaFact>]
let ``MigrationsRunnerToolbar - Steps value is passed to callback``() =
  let mutable calledKind = Unchecked.defaultof<RunMigrationKind>
  let mutable calledSteps = 0

  let toolbar =
    MigrationsRunnerToolbar(
      (fun _ -> async { return true }),
      (fun (kind, steps) ->
        calledKind <- kind
        calledSteps <- steps
        async { return () }))

  // Set the numeric up/down control value
  let numericUpDown =
    toolbar.Content :?> StackPanel |> fun sp -> sp.Children[3] :?> NumericUpDown

  numericUpDown.SetCurrentValue(NumericUpDown.ValueProperty, 5M)

  // Click the apply button with dry run enabled (resulting in RunMigrationKind.DryUp)
  let checkbox =
    toolbar.Content :?> StackPanel |> fun sp -> sp.Children[2] :?> CheckBox

  checkbox.SetCurrentValue(CheckBox.IsCheckedProperty, true)

  let applyButton =
    toolbar.Content :?> StackPanel
    |> fun sp -> sp.Children[0] :?> UserControl
    |> fun uc -> uc.Content :?> Button

  applyButton.RaiseEvent(RoutedEventArgs(Button.ClickEvent))

  Assert.Equal(RunMigrationKind.DryUp, calledKind)
  Assert.Equal(5, calledSteps) // Value we set
