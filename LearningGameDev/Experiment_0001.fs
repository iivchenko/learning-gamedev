module Experiment_0001

open LearningGameDev.Core
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Input

type Screen (context: ScreenContext) =

    let mutable isEscUpPrev = true

    interface IScreen with 
        member _.Update(gameTime: GameTime) =
            if not isEscUpPrev && Keyboard.GetState().IsKeyUp(Keys.Escape) then context.Back() else ()

            isEscUpPrev <- Keyboard.GetState().IsKeyUp(Keys.Escape) 

        member _.Draw(_: GameTime) =
             ()
    