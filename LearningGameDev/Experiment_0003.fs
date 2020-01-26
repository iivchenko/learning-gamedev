module Experiment_0003

open LearningGameDev.Core
open Microsoft.Xna.Framework.Graphics
open Myra.Graphics2D.UI
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Input

type Screen (context: ScreenContext, spriteBatch: SpriteBatch) =

    let mutable isEscUpPrev = true

    do
        Desktop.Widgets.Clear()

        let label = new Label()
        label.Text <- "Experiment 0003"

        Desktop.Widgets.Add(label)

    interface IScreen with 

        member _.Init () = ()

        member _.Update(gameTime: GameTime) =
            if not isEscUpPrev && Keyboard.GetState().IsKeyUp(Keys.Escape) then context.Back() else ()
            isEscUpPrev <- Keyboard.GetState().IsKeyUp(Keys.Escape)


            
        member _.Draw(gameTime: GameTime) =
            let delta = gameTime.ElapsedGameTime.TotalSeconds |> float32

            Desktop.Render()