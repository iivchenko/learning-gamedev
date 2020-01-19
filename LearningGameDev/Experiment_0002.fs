module Experiment_0002

open LearningGameDev.Core
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Input
open MonoGame.Extended
open Myra.Graphics2D.UI
open System

type Vector = | Vector of x: float32 * y: float32

module Vector = 
    let init x y = Vector(x, y)
    let zero () = Vector(0.0f, 0.0f)
    let unwrap (Vector(x, y)) = (x, y)
    let add (Vector(x1, y1)) (Vector(x2, y2)) = Vector(x1 + x2, y1 + y2) 
    let sub (Vector(x1, y1)) (Vector(x2, y2)) = Vector(x1 - x2, y1 - y2) 
    let mul scalar (Vector(x, y)) = Vector(x * scalar, y * scalar)
    let div scalar (Vector(x, y)) = Vector(x / scalar, y / scalar)
    let length (Vector(x, y)) = pown x 2 + pown y 2 |> sqrt
    let normalize (Vector(x, y)) =
        let length = pown x 2 + pown y 2 |> sqrt
        Vector(x/length, y/length)

type EntityType = Character | Target

type Entity =
    { Type : EntityType
      Position: Vector
      Orientation: float32
      Velocity: Vector
      Rotation: float32 
      MaxSpeed: float32 }

module Character =

    let update time character (linear, angular) =
        let position = Vector.mul time character.Velocity |> Vector.add character.Position
        let orientation = character.Orientation + time * angular
        let velocity = Vector.mul time linear |> Vector.add character.Velocity
        let rotation = character.Rotation + time * angular

        { character with Position = position; Orientation = orientation; Rotation = rotation; Velocity = if Vector.length velocity > character.MaxSpeed then velocity |> Vector.normalize |> Vector.mul character.MaxSpeed else velocity }

    let draw (spriteBatch: SpriteBatch) entity = 
        match entity.Type with 
        | Target ->
            let (x, y) = Vector.unwrap entity.Position
            spriteBatch.DrawCircle(Vector2(x, y), 25.0f, 100, Color.AliceBlue)
        | Character ->
            let (x, y) = Vector.unwrap entity.Position
            spriteBatch.DrawCircle(Vector2(x, y), 25.0f, 100, Color.Red, 25.0f)

module Behavior =
    
    let seek character target = 

        let maxAcceleration = 200.0f

        let linear = 
            Vector.sub target character.Position
            |> Vector.normalize
            |> Vector.mul maxAcceleration
        let angular = 0.0f

        (linear, angular)

    let flee character target = 
        
        let maxAcceleration = 200.0f

        let linear = 
            Vector.sub character.Position target
            |> Vector.normalize
            |> Vector.mul maxAcceleration
        let angular = 0.0f

        (linear, angular)

    let arrive character target = 

        let maxAcceleration = 200.0f
        let targetRadius = 3.0f
        let slowRadius = 300.0f
        let timeToTarget = 0.1f 

        let direction = Vector.sub target character.Position
        let distance = Vector.length direction

        if distance < targetRadius 
            then 
                (Vector.mul -1.0f character.Velocity, -character.Orientation)
            else
                let targetSpeed = if distance > slowRadius then character.MaxSpeed else (character.MaxSpeed * distance) / slowRadius
                let targetVelocity = direction |> Vector.normalize |> Vector.mul targetSpeed
                let linear = Vector.sub targetVelocity character.Velocity |> Vector.div timeToTarget

                if Vector.length linear > maxAcceleration
                    then (linear |> Vector.normalize |> Vector.mul maxAcceleration, 0.0f)
                    else (linear, 0.0f)

module World = 
    let private rnd = Random()

    let random min max = 
        rnd.Next(min, max) |> float32 

    let randomPosition () =
        Vector.init (rnd.Next(0, 1920) |> float32) (rnd.Next(0, 1080) |> float32)

    let generateCharacter position maxSpeed = 
        { 
            Type = Character
            Position = position
            Orientation = 0.0f
            MaxSpeed = maxSpeed
            Rotation = 0.0f
            Velocity = Vector.zero()
        }

    let generateTarget position =
        { 
            Type = Target
            Position = position
            Orientation = 0.0f
            MaxSpeed = 0.0f
            Rotation = 0.0f
            Velocity = Vector.zero()
        }

type ICase =
    abstract Update: float32 -> unit
    abstract Draw: float32 -> unit

type EmptyCase() =
    interface ICase with 

        member this.Update (delta: float32) = ()        
        member this.Draw (delta: float32) = ()

type SeekCase(spriteBatch: SpriteBatch) =
    let target = World.generateTarget (World.randomPosition())
    let mutable character = World.generateCharacter (World.randomPosition()) (World.random 100 1000)

    interface ICase with 

        member this.Update (delta: float32) =
            let velocities = Behavior.seek character target.Position

            character <- Character.update delta character velocities

        member this.Draw (delta: float32) =
            Character.draw spriteBatch target
            Character.draw spriteBatch character

type FleeCase(spriteBatch: SpriteBatch) =
    let position = World.randomPosition()
    let target = World.generateTarget position
    let mutable character = World.generateCharacter (Vector.add position (Vector.init (World.random -20 20) (World.random -20 20))) (World.random 100 1000)

    interface ICase with 

        member this.Update (delta: float32) =
            let velocities = Behavior.flee character target.Position 

            character <- Character.update delta character velocities

        member this.Draw (delta: float32) =
            Character.draw spriteBatch target
            Character.draw spriteBatch character

type ArriveCase(spriteBatch: SpriteBatch) =
    let target = World.generateTarget (World.randomPosition())
    let mutable character = World.generateCharacter (World.randomPosition()) (World.random 100 1000)

    interface ICase with 

        member this.Update (delta: float32) =
            let velocities = Behavior.arrive character target.Position

            character <- Character.update delta character velocities

        member this.Draw (delta: float32) =
            Character.draw spriteBatch target
            Character.draw spriteBatch character

type Screen (context: ScreenContext, spriteBatch: SpriteBatch) =

    let mutable isEscUpPrev = true
    let mutable case : ICase = EmptyCase() :> ICase

    do
        Desktop.Widgets.Clear()

        let panel = new VerticalStackPanel()
        panel.HorizontalAlignment <- HorizontalAlignment.Left
        panel.VerticalAlignment <- VerticalAlignment.Top

        let seekBtn = TextButton()
        seekBtn.Id <- ""
        seekBtn.Text <- "Seek"
        seekBtn.Click.Add((fun _ -> case <- SeekCase(spriteBatch)))

        let fleeBtn = TextButton()
        fleeBtn.Id <- ""
        fleeBtn.Text <- "Flee"
        fleeBtn.Click.Add((fun _ -> case <- FleeCase(spriteBatch)))

        let arriveBtn = TextButton()
        arriveBtn.Id <- ""
        arriveBtn.Text <- "Arrive"
        arriveBtn.Click.Add((fun _ -> case <- ArriveCase(spriteBatch)))

        panel.Widgets.Add(seekBtn)
        panel.Widgets.Add(fleeBtn)
        panel.Widgets.Add(arriveBtn)

        Desktop.Widgets.Add(panel)

    interface IScreen with 

        member _.Init () = ()

        member _.Update(gameTime: GameTime) =
            if not isEscUpPrev && Keyboard.GetState().IsKeyUp(Keys.Escape) then context.Back() else ()
            isEscUpPrev <- Keyboard.GetState().IsKeyUp(Keys.Escape)

            let delta = gameTime.ElapsedGameTime.TotalSeconds |> float32
            case.Update(delta)
            
        member _.Draw(gameTime: GameTime) =
            let delta = gameTime.ElapsedGameTime.TotalSeconds |> float32
            case.Draw(delta)

            Desktop.Render()