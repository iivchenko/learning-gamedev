﻿module Experiment_0002

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

module TheMath =

    let toRad degrees = (float degrees) * (System.Math.PI/180.0) |> float32

type Entity =
    { Position: Vector
      Orientation: float32
      Velocity: Vector
      Rotation: float32 
      MaxSpeed: float32 
      MaxRotation: float32 
      Color: Color }

module Character =

    let rotate (x: float32) (y: float32) (angle: float32) = Vector2(x * cos(angle) - y * sin(angle), x * sin(angle) + y * cos(angle))

    let update time character (linear, angular) =
        let position = Vector.mul time character.Velocity |> Vector.add character.Position
        let orientation = character.Orientation + time * character.Rotation
        let velocity = Vector.mul time linear |> Vector.add character.Velocity
        let rotation = character.Rotation + time * angular

        { character with 
            Position = position; 
            Orientation = orientation;
            Rotation = rotation //if abs(rotation) > abs(character.Rotation) then character.MaxRotation * (rotation/abs(rotation)) else rotation; 
            Velocity = if Vector.length velocity > character.MaxSpeed then velocity |> Vector.normalize |> Vector.mul character.MaxSpeed else velocity }

    let draw (spriteBatch: SpriteBatch) entity = 

        let points = new System.Collections.Generic.List<Vector2>()

        points.Add(rotate +15.0f -30.0f entity.Orientation)
        points.Add(rotate +0.0f +30.0f entity.Orientation)
        points.Add(rotate -15.0f -30.0f entity.Orientation)

        let (x, y) = Vector.unwrap entity.Position

        spriteBatch.DrawPolygon(Vector2(x, y), new Shapes.Polygon(points), entity.Color)

module Behavior =
    
    let seek character target = 

        let maxAcceleration = 200.0f

        let linear = 
            Vector.sub target.Position character.Position
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

        let direction = Vector.sub target.Position character.Position
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

    let align character target = 

        let maxAngularAcceleration = 200.0f |> TheMath.toRad
        let targetRadius = 10.0f |> TheMath.toRad
        let slowRadius = 100.0f |> TheMath.toRad
        let timeToTarget = 0.1f 

        let rotation = target.Orientation - character.Orientation |> Microsoft.Xna.Framework.MathHelper.WrapAngle
        let rotationSize = abs(rotation)

        if rotationSize < targetRadius 
            then 
                (Vector.mul -1.0f character.Velocity, -character.Rotation)
            else
                let targetRotation = if rotationSize > slowRadius then character.MaxRotation else (character.MaxRotation * rotationSize) / slowRadius
                let targetRotation' = targetRotation * rotation / rotationSize
                let angular = (targetRotation' - character.Rotation) / timeToTarget
                let angularAcceleration = abs(angular)

                if angularAcceleration > maxAngularAcceleration
                    then (Vector.zero(), angular / angularAcceleration * maxAngularAcceleration)
                    else (Vector.zero(), angular)

    let velocityMatch character target = 

        let maxAcceleration = 200.0f
        let timeToTarget = 0.1f 

        let linear = Vector.sub target.Velocity character.Velocity |> Vector.div timeToTarget

        if Vector.length linear > maxAcceleration
            then (linear |> Vector.normalize |> Vector.mul maxAcceleration, 0.0f)
            else (linear, 0.0f)

    let pursue character target =
        let maxPrediction = 5.0f
        let direction = Vector.sub target.Position character.Position
        let distance = Vector.length direction
        let speed = Vector.length character.Velocity

        let prediction = if speed <= distance / maxPrediction then maxPrediction else distance / speed
        let target = { target with Position = Vector.add target.Position (Vector.mul prediction target.Velocity) }
        seek character target

module World = 
    let private rnd = Random()

    let random min max = 
        rnd.Next(min, max) |> float32 

    let randomVector () =
        Vector.init (rnd.Next(0, 1920) |> float32) (rnd.Next(0, 1080) |> float32)

type ICase =
    abstract Update: float32 -> unit
    abstract Draw: float32 -> unit

type EmptyCase() =
    interface ICase with 

        member this.Update (delta: float32) = ()
        member this.Draw (delta: float32) = ()

type SeekCase(spriteBatch: SpriteBatch) =
    let target = 
        { 
            Position = World.randomVector()
            Orientation = 0.0f
            Rotation = 0.0f
            Velocity = Vector.zero()
            MaxSpeed = 0.0f
            MaxRotation = 0.0f
            Color = Color.AliceBlue
        }
        
    let mutable character =
        { 
            Position = World.randomVector()
            Orientation = 0.0f
            Rotation = 0.0f
            Velocity = Vector.zero()
            MaxSpeed = World.random 100 1000
            MaxRotation = 0.0f
            Color = Color.Red
        }

    interface ICase with 

        member this.Update (delta: float32) =
            let velocities = Behavior.seek character target

            character <- Character.update delta character velocities

        member this.Draw (delta: float32) =
            Character.draw spriteBatch target
            Character.draw spriteBatch character

type FleeCase(spriteBatch: SpriteBatch) =
    let position = World.randomVector()
    let target = 
        { 
            Position = position
            Orientation = 0.0f
            Rotation = 0.0f
            Velocity = Vector.zero()
            MaxSpeed = 0.0f
            MaxRotation = 0.0f
            Color = Color.AliceBlue
        }

    let mutable character =
        { 
            Position = (Vector.add position (Vector.init (World.random -20 20) (World.random -20 20)))
            Orientation = 0.0f
            Rotation = 0.0f
            Velocity = Vector.zero()
            MaxSpeed = (World.random 100 1000)
            MaxRotation = 0.0f
            Color = Color.Red
        }

    interface ICase with 

        member this.Update (delta: float32) =
            let velocities = Behavior.flee character target.Position 

            character <- Character.update delta character velocities

        member this.Draw (delta: float32) =
            Character.draw spriteBatch target
            Character.draw spriteBatch character

type ArriveCase(spriteBatch: SpriteBatch) =
    let target =
        { 
            Position = World.randomVector()
            Orientation = 0.0f
            Rotation = 0.0f
            Velocity = Vector.zero()
            MaxSpeed = 0.0f
            MaxRotation = 0.0f
            Color = Color.AliceBlue
        }

    let mutable character =
        { 
            Position = World.randomVector()
            Orientation = 0.0f
            Rotation = 0.0f
            Velocity = Vector.zero()
            MaxSpeed = World.random 100 1000
            MaxRotation = 0.0f
            Color = Color.Red
        }

    interface ICase with 

        member this.Update (delta: float32) =
            let velocities = Behavior.arrive character target

            character <- Character.update delta character velocities

        member this.Draw (delta: float32) =
            Character.draw spriteBatch target
            Character.draw spriteBatch character

type AlignCase(spriteBatch: SpriteBatch) =
    let target =
        { 
            Position = Vector.init (1920.0f/2.0f + 50.0f) (1080.0f/2.0f)
            Orientation = World.random 0 360 |> TheMath.toRad
            Rotation = 0.0f
            Velocity = Vector.zero()
            MaxSpeed = 0.0f
            MaxRotation = 0.0f
            Color = Color.AliceBlue
        }

    let mutable character =
        { 
            Position = Vector.init (1920.0f/2.0f - 50.0f) (1080.0f/2.0f)
            Orientation = World.random 0 360 |> TheMath.toRad
            Rotation = 0.0f
            Velocity = Vector.zero()
            MaxSpeed = 0.0f
            MaxRotation = World.random 360 1000 |> TheMath.toRad
            Color = Color.Red
        }
            
    interface ICase with 
            
        member this.Update (delta: float32) =
            let velocities = Behavior.align character target
            
            character <- Character.update delta character velocities
            
        member this.Draw (delta: float32) =
            Character.draw spriteBatch target
            Character.draw spriteBatch character

type VelocityMatchCase(spriteBatch: SpriteBatch) =

    let mutable target = 
        { 
            Position = Vector.init (1920.0f/2.0f + 50.0f) (1080.0f/2.0f)
            Orientation = World.random 100 1000
            Rotation = 0.0f
            Velocity = Vector.init (World.random 0 250) (World.random 0 250)
            MaxSpeed = World.random 0 100
            MaxRotation = 0.0f
            Color = Color.AliceBlue
        }
    
    let mutable character = 
        { 
            Position = Vector.init (1920.0f/2.0f - 50.0f) (1080.0f/2.0f)
            Orientation = World.random 100 1000
            Rotation = 0.0f
            Velocity = Vector.init (World.random 0 250) (World.random 0 250)
            MaxSpeed = World.random 100 250
            MaxRotation = 0.0f
            Color = Color.Red
        }

    interface ICase with 

        member this.Update (delta: float32) =
            let velocities = Behavior.velocityMatch character target

            target <- Character.update delta target (Vector.zero(), 0.0f)
            character <- Character.update delta character velocities

        member this.Draw (delta: float32) =
            Character.draw spriteBatch target
            Character.draw spriteBatch character

type PursueCase(spriteBatch: SpriteBatch) =

    let start = World.random 0 2

    let mutable target = 
        { 
            Position = if start = 0.0f then Vector.init 0.0f 100.0f else Vector.init 1920.0f 100.0f
            Orientation = 0.0f
            Rotation = 0.0f
            Velocity = if start = 0.0f then Vector.init 100.0f 0.0f else Vector.init -100.0f 0.0f
            MaxSpeed = 100.0f
            MaxRotation = 0.0f
            Color = Color.AliceBlue
        }
        
    let mutable character =
        { 
            Position = World.randomVector()
            Orientation = 0.0f
            Rotation = 0.0f
            Velocity = Vector.zero()
            MaxSpeed = World.random 100 1000
            MaxRotation = 0.0f
            Color = Color.Red
        }

    interface ICase with 

        member this.Update (delta: float32) =
            let velocities = Behavior.pursue character target

            target <- Character.update delta target (target.Velocity, 0.0f)
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

        let alignBtn = TextButton()
        alignBtn.Id <- ""
        alignBtn.Text <- "Align"
        alignBtn.Click.Add((fun _ -> case <- AlignCase(spriteBatch)))

        let velocityMatchBtn = TextButton()
        velocityMatchBtn.Id <- ""
        velocityMatchBtn.Text <- "Velocity Match"
        velocityMatchBtn.Click.Add((fun _ -> case <- VelocityMatchCase(spriteBatch)))

        let pursueBtn = TextButton()
        pursueBtn.Id <- ""
        pursueBtn.Text <- "Pursue"
        pursueBtn.Click.Add((fun _ -> case <- PursueCase(spriteBatch)))

        panel.Widgets.Add(seekBtn)
        panel.Widgets.Add(fleeBtn)
        panel.Widgets.Add(arriveBtn)
        panel.Widgets.Add(alignBtn)
        panel.Widgets.Add(velocityMatchBtn)
        panel.Widgets.Add(pursueBtn)

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