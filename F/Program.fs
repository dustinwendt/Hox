namespace HoxApp

open Avalonia
open Avalonia.Controls.ApplicationLifetimes
open Avalonia.Themes.Fluent
open Avalonia.FuncUI.Hosts
open Avalonia.Controls
open Avalonia.FuncUI
open Avalonia.FuncUI.DSL
open Avalonia.Layout


open Colors
open Rules
open Types

module Main =

    let view () =
        Component(fun ctx ->
            let color = ctx.useState Blue
            let state = ctx.useState 0
            let gamestate = ctx.useState defaultGameState
            let infoBox = StackPanel.create [
                 StackPanel.dock Dock.Bottom
                 StackPanel.children [
                   TextBlock.create [
                     TextBlock.text ("White: " + string (Map.find (Colored White) (Map.find You gamestate.Current.players).manapool))
                   ]
                   TextBlock.create [
                     TextBlock.text ("Blue: " + string (Map.find (Colored Blue) (Map.find You gamestate.Current.players).manapool))
                   ]
                   TextBlock.create [
                     TextBlock.text ("Black: " + string (Map.find (Colored Black) (Map.find You gamestate.Current.players).manapool))
                   ]
                   TextBlock.create [
                     TextBlock.text ("Red: " + string (Map.find (Colored Red) (Map.find You gamestate.Current.players).manapool))
                   ]
                   TextBlock.create [
                     TextBlock.text ("Green: " + string (Map.find (Colored Green) (Map.find You gamestate.Current.players).manapool))
                   ]
                   TextBlock.create [
                     TextBlock.text ("Colorless: " + string (Map.find Colorless (Map.find You gamestate.Current.players).manapool))
                   ] ] ]
            let passB = StackPanel.create [
                       StackPanel.children [
                         TextBlock.create [
                               TextBlock.horizontalAlignment HorizontalAlignment.Center
                               TextBlock.text (string gamestate.Current.currPhase)
                            ]
                         Button.create [
                               Button.onClick (fun _ -> gamestate.Set(pass gamestate.Current))
                               Button.content "Pass"
                               Button.horizontalContentAlignment HorizontalAlignment.Center
                            ]
                         Button.create [
                            Button.dock Dock.Bottom
                            Button.onClick (fun _ -> gamestate.Set(draw gamestate.Current You 1))
                            Button.content "Draw"
                            Button.horizontalContentAlignment HorizontalAlignment.Center
                         ]
                         TextBlock.create [
                            TextBlock.dock Dock.Top
                            TextBlock.verticalAlignment VerticalAlignment.Center
                            TextBlock.horizontalAlignment HorizontalAlignment.Center
                            TextBlock.text (string gamestate.Current.test)
                         ]
                         TextBlock.create [
                            TextBlock.dock Dock.Top
                            TextBlock.verticalAlignment VerticalAlignment.Center
                            TextBlock.horizontalAlignment HorizontalAlignment.Center
                            TextBlock.text ("Life: " + string ((Map.find You gamestate.Current.players).life))
                         ]
                         TextBlock.create [
                            TextBlock.verticalAlignment VerticalAlignment.Center
                            TextBlock.dock Dock.Top
                            TextBlock.horizontalAlignment HorizontalAlignment.Center
                            TextBlock.text ("Graveyard: " + string ((Map.find You gamestate.Current.players).graveyard.Length))
                         ]
                         TextBlock.create [
                            TextBlock.dock Dock.Top
                            TextBlock.verticalAlignment VerticalAlignment.Center
                            TextBlock.horizontalAlignment HorizontalAlignment.Center
                            TextBlock.text ("Hand: " + string ((Map.find You gamestate.Current.players).hand.Length))
                         ]
                         TextBlock.create [
                            TextBlock.dock Dock.Top
                            TextBlock.verticalAlignment VerticalAlignment.Center
                            TextBlock.horizontalAlignment HorizontalAlignment.Center
                            TextBlock.text ("Library: " + string ((Map.find You gamestate.Current.players).library.Length))
                         ] ] ]
            StackPanel.create[
               StackPanel.orientation Orientation.Horizontal
               StackPanel.children [
                       infoBox
                       passB
            ] ] 

            // DockPanel.create [
            //     DockPanel.dock Dock.Top
            //     DockPanel.children [
            //         Button.create [
            //             Button.dock Dock.Bottom
            //             Button.onClick (fun _ -> state.Set(state.Current - 1))
            //             Button.content "-"
            //             // Button.horizontalAlignment HorizontalAlignment.Stretch
            //             Button.horizontalContentAlignment HorizontalAlignment.Center
            //         ]
            //         Button.create [
            //             Button.dock Dock.Bottom
            //             Button.onClick (fun _ -> state.Set(state.Current + 1))
            //             Button.content "+"
            //             // Button.horizontalAlignment HorizontalAlignment.Stretch
            //             Button.horizontalContentAlignment HorizontalAlignment.Center
            //         ]
            // ]
            //    ]
        )

// let getByName (s:string) = typeof<GameState>.GetProperties() |> Option.map (fun pi -> pi.GetValue test)

type MainWindow() =
    inherit HostWindow()
    do
        base.Title <- "Hox"
        base.Content <- Main.view ()

type App() =
    inherit Application()

    override this.Initialize() =
        this.Styles.Add (FluentTheme())
        this.RequestedThemeVariant <- Styling.ThemeVariant.Dark

    override this.OnFrameworkInitializationCompleted() =
        match this.ApplicationLifetime with
        | :? IClassicDesktopStyleApplicationLifetime as desktopLifetime ->
            desktopLifetime.MainWindow <- MainWindow()
        | _ -> ()

module Program =

    [<EntryPoint>]
    let main(args: string[]) =
        AppBuilder
            .Configure<App>()
            .UsePlatformDetect()
            .UseSkia()
            .StartWithClassicDesktopLifetime(args)
