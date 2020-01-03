open LearningGameDev.Core
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Myra
open Microsoft.Xna.Framework.Input
open Myra.Graphics2D.UI

type MainScreen (context: ScreenContext, exit: unit -> unit) =

    let experiments = ListBox()

    let mutable isEscUpPrev = true
    
    let start _ = 
        match  Option.ofNullable experiments.SelectedIndex with
        | None -> ()
        | Some value -> 
            // TODO: Think about more flexible way of Experiment instantiation.
            // Reflection, Quotations, Computation Expressions etc.
            match experiments.Items.[value].Text with
            | "Experiment 0001" -> context.Next(Experiment_0001.Screen (context))
            | _ -> ()

    do
        let panel = new VerticalStackPanel()
        panel.HorizontalAlignment <- HorizontalAlignment.Center
        panel.VerticalAlignment <- VerticalAlignment.Center

        let experiment0001 = ListItem()
        experiment0001.Id <- ""
        experiment0001.Text <- "Experiment 0001"

        let experiment0002 = ListItem()
        experiment0002.Id <- null
        experiment0002.Text <- "Experiment 0002"
       
        experiments.Items.Add(experiment0001)
        experiments.Items.Add(experiment0002)

        let startButton = new TextButton()
        startButton.Text <- "Start Experiment"
        startButton.Id <- ""
        startButton.Click.Add(start)

        panel.Widgets.Add(experiments)
        panel.Widgets.Add(startButton)

        Desktop.Widgets.Add(panel)

    interface IScreen with 
        member _.Update(_: GameTime) =
            if not isEscUpPrev && Keyboard.GetState().IsKeyUp(Keys.Escape) then exit() else ()
            
            isEscUpPrev <- Keyboard.GetState().IsKeyUp(Keys.Escape)

        member _.Draw(_: GameTime) =
             Desktop.Render ()

type TheGame () as this =
    inherit Microsoft.Xna.Framework.Game()

    let graphics = new GraphicsDeviceManager(this)
    let screenWith = 1920 
    let screenHeight = 1080
    let screenContext = ScreenContext ()
    let mutable spriteBatch = Unchecked.defaultof<SpriteBatch>

    override _.LoadContent() =
        spriteBatch <- new SpriteBatch(this.GraphicsDevice)

    override this.Initialize () =

        base.Initialize()

        graphics.PreferredBackBufferWidth <- screenWith
        graphics.PreferredBackBufferHeight <- screenHeight
        
        #if RELEASE 
        graphics.IsFullScreen <- true
        #endif

        graphics.ApplyChanges();

        base.IsMouseVisible <- true

        MyraEnvironment.Game <- this

        screenContext.Next(MainScreen(screenContext, this.Exit))

    override _.Update (gameTime: GameTime) =

        screenContext.Screen.Update gameTime

        base.Update(gameTime)

    override _.Draw (gameTime: GameTime) =
        
        graphics.GraphicsDevice.Clear(Color.CornflowerBlue)

        spriteBatch.Begin()

        screenContext.Screen.Draw gameTime

        spriteBatch.End()

[<EntryPoint>]
let main _ =
    let game = new TheGame()
    game.Run()

    0