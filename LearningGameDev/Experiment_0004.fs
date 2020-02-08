module Experiment_0004

open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Input
open MonoGame.Extended
open Myra.Graphics2D.UI

open LearningGameDev.Core

type GameState = 
    | Pause
    | Run

type Cell = 
    | Live
    | Dead

module Cell =
    let rows = 90
    let columns = 54

    let inRange i j = if (i >= 0 && i < rows && j >= 0 && j < columns) then true else false 

    let private count (grid: Cell [,]) i j =
        let a1 = if inRange (i - 1) (j - 1) then (if grid.[i - 1, j - 1] = Live then 1 else 0) else 0
        let a2 = if inRange (i - 0) (j - 1) then (if grid.[i + 0, j - 1] = Live then 1 else 0) else 0
        let a3 = if inRange (i + 1) (j - 1) then (if grid.[i + 1, j - 1] = Live then 1 else 0) else 0
        let a4 = if inRange (i - 1) (j + 0) then (if grid.[i - 1, j + 0] = Live then 1 else 0) else 0
        let a5 = if inRange (i + 1) (j + 0) then (if grid.[i + 1, j + 0] = Live then 1 else 0) else 0
        let a6 = if inRange (i - 1) (j + 1) then (if grid.[i - 1, j + 1] = Live then 1 else 0) else 0
        let a7 = if inRange (i + 0) (j + 1) then (if grid.[i + 0, j + 1] = Live then 1 else 0) else 0
        let a8 = if inRange (i + 1) (j + 1) then (if grid.[i + 1, j + 1] = Live then 1 else 0) else 0

        a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8

    let decide (grid: Cell [,]) i j =
        let neighbours = count grid i j
        
        match grid.[i, j] with 
        | Dead -> if neighbours = 3 then Live else Dead
        | Live -> if neighbours = 2 || neighbours = 3 then Live else Dead

    let draw (spriteBatch: SpriteBatch) i j cell =
        match cell with 
        | Dead -> ()
        | Live -> spriteBatch.FillRectangle((float32 i) * 20.0f, (float32 j) * 20.0f, 20.0f, 20.0f, Color.Black)

type Screen (context: ScreenContext, spriteBatch: SpriteBatch) =

    let mutable isEscUpPrev = true
    let mutable leftButtonPreviousState = ButtonState.Released
    let mutable rightButtonPreviousState = ButtonState.Released

    let mutable grid =  Array2D.init Cell.rows Cell.columns (fun _ _ -> Dead)

    let mutable gameState = GameState.Pause
    let mutable next = 15.0f / 60.0f

    let speedSpin = new SpinButton()

    interface IScreen with 

        member _.Init () =

            Desktop.Widgets.Clear()
            let layout = new Grid()
            layout.ColumnSpacing <- 8
            layout.RowSpacing <- 8
            layout.HorizontalAlignment <- HorizontalAlignment.Right
            
            let proportion = new Proportion()
            let fillProportion = new Proportion()
            proportion.Type <- Myra.Graphics2D.UI.ProportionType.Auto
            fillProportion.Type <- Myra.Graphics2D.UI.ProportionType.Fill
            layout.DefaultRowProportion <- proportion
            layout.ColumnsProportions.Add(proportion)
            layout.ColumnsProportions.Add(proportion)
            layout.ColumnsProportions.Add(fillProportion)
            
            let startPauseButton  = new TextButton()
            startPauseButton.Text <- "Start"
            startPauseButton.GridRow <- 0
            startPauseButton.GridColumn <- 0
            startPauseButton.Click.Add(fun _ -> gameState <- if gameState = Pause then startPauseButton.Text <- "Pause"; Run else startPauseButton.Text <- "Start"; Pause)

            let clearButton = new TextButton()
            clearButton.Text <- "Clear"
            clearButton.GridRow <- 1
            clearButton.GridColumn <- 0
            clearButton.Click.Add(fun _ -> grid <- Array2D.init Cell.rows Cell.columns (fun _ _ -> Dead))
            
            speedSpin.Value <- System.Nullable<float32>(next * 60.0f)
            speedSpin.GridRow <- 2
            speedSpin.GridColumn <- 0

            layout.Widgets.Add(startPauseButton)
            layout.Widgets.Add(clearButton)
            layout.Widgets.Add(speedSpin)
            
            Desktop.Widgets.Add(layout)

        member _.Update(gameTime: GameTime) =
            let delta = gameTime.ElapsedGameTime.TotalSeconds |> float32
            if not isEscUpPrev && Keyboard.GetState().IsKeyUp(Keys.Escape) then context.Back() else ()
            isEscUpPrev <- Keyboard.GetState().IsKeyUp(Keys.Escape)

            let state = Mouse.GetState() 

            if leftButtonPreviousState = ButtonState.Pressed && state.LeftButton = ButtonState.Released then
                let x = state.X / 20
                let y = state.Y / 20

                if Cell.inRange x y 
                    then grid.[x, y] <- Live
                    else ()
            else 
                ()

            leftButtonPreviousState <- state.LeftButton

            if rightButtonPreviousState = ButtonState.Pressed && state.RightButton = ButtonState.Released then
               let x = state.X / 20
               let y = state.Y / 20

               if Cell.inRange x y 
                   then grid.[x, y] <- Dead
                   else ()
            else 
                ()

            match gameState with 
            | Pause -> 
                ()
            | Run when next <= 0.0f -> 
                grid <- grid |> Array2D.mapi (fun i j _ -> Cell.decide grid i j)
                next <- (speedSpin.Value.Value |> float32) / 60.0f
            | _ -> 
                next <- next - delta

            rightButtonPreviousState <- state.RightButton
            
        member _.Draw(gameTime: GameTime) =
            
            grid |> Array2D.iteri(fun i j cell -> Cell.draw spriteBatch i j cell)

            Desktop.Render()