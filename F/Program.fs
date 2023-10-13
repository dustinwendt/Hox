namespace HoxApp

open Avalonia
open Avalonia.Controls.ApplicationLifetimes
open Avalonia.Themes.Fluent
open Avalonia.FuncUI.Hosts
open Avalonia.Controls
open Avalonia.FuncUI
open Avalonia.FuncUI.DSL
open Avalonia.Layout
open System

open Colors
open ImageHelper
open Rules
open Types

module Main =

    let view () =
        Component(fun ctx ->
            let gamestate = ctx.useState defaultGameState

            let wPool x = TextBlock.create [ TextBlock.text ("White: " + string (Map.find (Colored White) (Map.find x gamestate.Current.players).manapool))]
            let uPool x = TextBlock.create [ TextBlock.text ("Blue: " + string (Map.find (Colored Blue) (Map.find x gamestate.Current.players).manapool))]
            let bPool x = TextBlock.create [ TextBlock.text ("Black: " + string (Map.find (Colored Black) (Map.find x gamestate.Current.players).manapool))]
            let rPool x = TextBlock.create [ TextBlock.text ("Red: " + string (Map.find (Colored Red) (Map.find x gamestate.Current.players).manapool))]
            let gPool x = TextBlock.create [ TextBlock.text ("Green: " + string (Map.find (Colored Green) (Map.find x gamestate.Current.players).manapool))]
            let cPool x = TextBlock.create [ TextBlock.text ("Colorless: " + string (Map.find Colorless (Map.find x gamestate.Current.players).manapool))]

            let phaseString =  TextBlock.create [ TextBlock.horizontalAlignment HorizontalAlignment.Center; TextBlock.text (string gamestate.Current.currPhase)]
            let passButton = Button.create [ Button.onClick (fun _ -> gamestate.Set(pass gamestate.Current)); Button.content "Pass"; Button.horizontalContentAlignment HorizontalAlignment.Center
                                             Grid.row 1
                                             Grid.column 0]

            let testImage = loadAvaloniaImage "phel.png"
            let pManaPools = StackPanel.create [
                 StackPanel.dock Dock.Bottom
                 StackPanel.children [ wPool You; uPool You; bPool You; rPool You; gPool You; cPool You]]
            let oManaPools = StackPanel.create [
                 StackPanel.dock Dock.Top
                 StackPanel.children [wPool Opp; uPool Opp; bPool Opp; rPool Opp; gPool Opp; cPool Opp]]

            let manaPools = StackPanel.create [
                 Grid.row 0
                 Grid.column 0
                 StackPanel.spacing 50
                 StackPanel.children [ TextBlock.create [TextBlock.text "Opp"]; oManaPools;  TextBlock.create [TextBlock.text "You"]; pManaPools]
                ]
            let drawButton = Button.create [ Grid.row 1
                                             Grid.column 1
                                             // Button.dock Dock.Bottom
                                             Button.content "Draw"
                                             Button.onClick (fun _ -> gamestate.Set(draw gamestate.Current You 1))
                                             Button.horizontalContentAlignment HorizontalAlignment.Center]
            let infoBox = StackPanel.create [
                       Grid.row 0
                       Grid.column 1
                       StackPanel.children [
                         Image.create [ Image.source testImage; Image.width 200]
                         phaseString
                         passButton
                         drawButton
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

            let f4Button = Button.create [ Grid.row 0; Grid.column 0; Button.content "F4"; Button.horizontalContentAlignment HorizontalAlignment.Center ]
            let f5Button = Button.create [ Grid.row 0; Grid.column 1; Button.content "F5"; Button.horizontalContentAlignment HorizontalAlignment.Center ]
            let f7Button = Button.create [ Grid.row 0; Grid.column 2; Button.content "F7"; Button.horizontalContentAlignment HorizontalAlignment.Center ]
            let f9Button = Button.create [ Grid.row 0; Grid.column 3; Button.content "F9"; Button.horizontalContentAlignment HorizontalAlignment.Center ]
            let f10Button = Button.create [ Grid.row 0; Grid.column 4; Button.content "F10"; Button.horizontalContentAlignment HorizontalAlignment.Center ]
            let f11Button = Button.create [ Grid.row 0; Grid.column 5; Button.content "F11"; Button.horizontalContentAlignment HorizontalAlignment.Center ]

            let fRow = Grid.create [ Grid.columnDefinitions "Auto, Auto, Auto, Auto, Auto, Auto"
                                     Grid.row 0
                                     Grid.column 0
                                     Grid.children [ f4Button; f5Button; f7Button; f9Button; f10Button; f11Button ]]
            let gRow = Grid.create [ Grid.columnDefinitions "Auto, Auto"
                                     Grid.rowDefinitions "Auto"
                                     Grid.row 1
                                     Grid.column 0
                                     Grid.children [passButton; drawButton]]
            let controlBox = Grid.create [ //Grid.spacing 10
                                           Grid.row 0
                                           Grid.column 2
                                           Grid.showGridLines true
                                           Grid.rowDefinitions "Auto, Auto"
                                           Grid.columnDefinitions "Auto"
                                           Grid.children [fRow; gRow]]

            let mainStack = StackPanel.create [ StackPanel.children [manaPools; infoBox]]

            let mainGrid = Grid.create [ //Grid.RowSpacing 10
                                         Grid.columnDefinitions "*, Auto"; Grid.showGridLines true
                                         Grid.children [infoBox; manaPools; controlBox]]

            let basicGrid = Grid.create [ Grid.columnDefinitions "Auto, Auto"
                                          Grid.rowDefinitions "Auto"
                                          Grid.children [ Image.create [ Grid.row 0; Grid.column 0; Image.source testImage; Image.width 200]
                                                          Image.create [ Grid.row 0; Grid.column 1; Image.source testImage; Image.width 200]]]

            // basicGrid

            // Either figure out grid spacing or everything is stackpanel

            // controlBox
            mainGrid
            // mainStack
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
