module Experiment_0001

open LearningGameDev.Core

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Input
open Microsoft.Xna.Framework.Graphics

open MonoGame.Extended

type Vector = 
    | Vector of x: float32 * y: float32
    static member (+) (Vector (x1, y1), Vector (x2, y2)) = Vector (x2 + x1, y2 + y1)
    static member (-) (Vector (x1, y1), Vector (x2, y2)) = Vector (x2 - x1, y2 - y1)
    static member (*) (Vector (x, y), scalar) = Vector (x * scalar, y * scalar)
    static member (*) (scalar, Vector (x, y)) = Vector (x * scalar, y * scalar)
    static member (/) (Vector (x, y), scalar) = Vector (x / scalar, y / scalar)

module Vector =

    let init (x) (y) = Vector (x, y)
    let unwrap (Vector(x, y)) = (x, y)
    let distance (Vector (x1, y1): Vector) (Vector(x2, y2): Vector) = pown (x2 - x1) 2 + pown (y2 - y1) 2 |> sqrt
    let length (Vector(x, y)) = pown x 2 + pown y 2 |> sqrt
    let normalize (v: Vector) = v / length v
    let fromAngle angle = Vector(-sin(angle), cos(angle))
    let zero () = Vector(0.0f, 0.0f)

type Body = 
| Static of position: Vector * orientation: float32
| Kinematic of position: Vector * orientation: float32 * velocity: Vector * rotation: float32 * maxSpeed: float32 * maxRotation: float32

type Behavior = 
| Seek
| Wander
| Flee

type AnimalType =
| Herbivore
| Predator
| Plant

type Animal = 
    { Type: AnimalType
      Body: Body
      Behavior: Behavior
      ViewRange: float32 
      IsAlive: bool
      Hungar: float32}

module Animal =
    
    let toRad degrees = (float degrees) * (System.Math.PI/180.0) |> float32    

    let randomBinomial () = 
        let random = System.Random()
        random.NextDouble () - random.NextDouble () |> float32

    let position animal =
        match animal.Body with
        | Static(position, _)
        | Kinematic (position, _, _, _, _, _) -> position
    
    let update time animal animals =

        let isClosePlant plant (Vector(x, y)) radius  = 
            let (Vector(px, py)) = position plant

            match plant.Type with
            | Plant when pown (px - x) 2 + pown (py - y) 2 |> sqrt < radius -> true
            | _ -> false

        let hasClosePlant animal animals =           

            let plant = 
                animals 
                    |> List.filter (fun x -> animal.Equals(x) |> not) 
                    |> List.filter(fun x -> isClosePlant x (position animal) animal.ViewRange)
                    |> List.tryHead

            match plant with 
            | Some _ -> true
            | _ -> false

        let isClosePredator predator (Vector(x, y)) radius  = 
            let (Vector(px, py)) = position predator

            match predator.Type with
            | Predator when pown (px - x) 2 + pown (py - y) 2 |> sqrt < radius -> true
            | _ -> false

        let hasClosePredator animal animals =           

            let predator = 
                animals 
                    |> List.filter (fun x -> animal.Equals(x) |> not) 
                    |> List.filter(fun x -> isClosePredator x (position animal) animal.ViewRange)
                    |> List.tryHead

            match predator with 
            | Some _ -> true
            | _ -> false
            
        let isCloseHerbivore herbivore (Vector(x, y)) radius  = 
            let (Vector(px, py)) = position herbivore

            match herbivore.Type with
            | Herbivore when pown (px - x) 2 + pown (py - y) 2 |> sqrt < radius -> true
            | _ -> false

        let hasCloseHerbivore animal animals = 
            let herbivore = 
                animals 
                    |> List.filter (fun x -> animal.Equals(x) |> not) 
                    |> List.filter(fun x -> isCloseHerbivore x (position animal) animal.ViewRange)
                    |> List.tryHead

            match herbivore with 
            | Some _ -> true
            | _ -> false

        let hungar = animal.Hungar - time

        if hungar <= 0.0f 
        then { animal with IsAlive = false }
        else     
            match animal.Body with 
        | Static (pos, _) -> 
            match animal.Type with 
            | Plant -> 
                let herbivore = 
                    animals 
                        |> List.filter (fun x -> animal.Equals(x) |> not) 
                        |> List.filter(fun x -> isCloseHerbivore x (position animal) animal.ViewRange)
                        |> List.tryHead

                match herbivore with 
                | Some p -> 
                    let isAlive = Vector.length (pos - (position p)) > 10.0f
                    { animal with IsAlive = isAlive; Hungar = hungar}
                | _ -> { animal with Hungar = hungar}
        | Kinematic (pos, orientation, velocity, rotation, maxSpeed, maxRotation) ->
            match animal.Behavior with            
            | Wander ->
                match animal.Type with 
                | Predator when hasCloseHerbivore animal animals -> { animal with Behavior = Seek; Hungar = hungar}
                | Herbivore when hasClosePredator animal animals -> { animal with Behavior = Flee; Hungar = hungar}
                | Herbivore when hasClosePlant animal animals -> { animal with Behavior = Seek; Hungar = hungar}
                | _ ->
                    let (Vector(x, y)) = pos
                    let rotation' = rotation + randomBinomial() * maxRotation * time
                    let orientation' = if x < 1920.0f && x > 0.0f && y < 1080.0f && y > 0.0f then orientation + rotation' * time else orientation + (float32 System.Math.PI)
                    let velocity' =  if x < 1920.0f && x > 0.0f && y < 1080.0f && y > 0.0f then maxSpeed * Vector.fromAngle orientation' else pos - Vector(1920.0f/2.0f, 1080.0f/2.0f)
                    let position' = pos + velocity' * time  

                    { animal with Body = Kinematic (position', orientation', velocity', rotation', maxSpeed, maxRotation); Hungar = hungar }
            
            | Seek -> 
                match animal.Type with
                | Predator -> 
                    let herbivore = 
                        animals 
                            |> List.filter (fun x -> animal.Equals(x) |> not) 
                            |> List.filter(fun x -> isCloseHerbivore x (position animal) animal.ViewRange)
                            |> List.tryHead

                    match herbivore with 
                    | Some p -> 
                        //let (Vector(x, y)) = pos
                        //let rotation' = rotation + randomBinomial() * maxRotation * time
                        let (Vector(x, y)) = pos
                        let velocity' = Vector.normalize (pos - (position p))
                        let (Vector(vx, vy)) = velocity'
                        let orientation' = System.Math.Atan2(float -vx, float vy)
                        let position' = pos + velocity' * maxSpeed * time

                        let hungar' = if Vector.length (position' - (position p)) < 20.0f then hungar + 5.0f else hungar

                        { animal with Body = Kinematic(position', float32 orientation', velocity', rotation, maxSpeed, maxRotation); Hungar =  hungar' }
                    | _ -> { animal with Behavior = Wander; Hungar = hungar }
                | Herbivore ->
                    let plant = 
                        animals 
                            |> List.filter (fun x -> animal.Equals(x) |> not) 
                            |> List.filter(fun x -> isClosePlant x (position animal) animal.ViewRange)
                            |> List.tryHead

                    match plant with 
                    | Some p -> 
                        //let (Vector(x, y)) = pos
                        //let rotation' = rotation + randomBinomial() * maxRotation * time
                        let (Vector(x, y)) = pos
                        let velocity' = Vector.normalize (pos - (position p))
                        let (Vector(vx, vy)) = velocity'
                        let orientation' = System.Math.Atan2(float -vx, float vy)
                        let position' = pos + velocity' * maxSpeed * time

                        let hungar' = if Vector.length (position' - (position p)) < 10.0f then hungar + 5.0f else hungar

                        { animal with Body = Kinematic(position', float32 orientation', velocity', rotation, maxSpeed, maxRotation); Hungar =  hungar' }
                    | _ -> { animal with Behavior = Wander; Hungar = hungar }
                    
            | Flee ->
                let predator = 
                    animals 
                        |> List.filter (fun x -> animal.Equals(x) |> not) 
                        |> List.filter(fun x -> isClosePredator x (position animal) animal.ViewRange)
                        |> List.tryHead

                match predator with 
                | Some p -> 
                    //let (Vector(x, y)) = pos
                    //let rotation' = rotation + randomBinomial() * maxRotation * time
                    let velocity' = Vector.normalize ((position p) - pos)
                    let (Vector(vx, vy)) = velocity'
                    let orientation' = System.Math.Atan2(float -vx, float vy)
                    let position' = pos + velocity' * maxSpeed * time  

                    let isAlive = Vector.length (position' - (position p)) > 20.0f

                    { animal with Body = Kinematic(position', float32 orientation', velocity', rotation, maxSpeed, maxRotation); IsAlive = isAlive; Hungar = hungar}
                | _ -> { animal with Behavior = Wander; Hungar = hungar }
                
    let draw (spriteBatch: SpriteBatch) animal =
        let rotate (x: float32) (y: float32) (angle: float32) = Vector2(x * cos(angle) - y * sin(angle), x * sin(angle) + y * cos(angle))

        match animal.Body with
        | Static (Vector(x, y), angle)            
        | Kinematic (Vector(x, y), angle, _, _, _, _) ->
             match animal.Type with 
            | Herbivore ->
                let a = new System.Collections.Generic.List<Vector2>()
                
                a.Add(rotate -25.0f -25.0f angle)
                a.Add(rotate +25.0f -25.0f angle)
                a.Add(rotate +25.0f +25.0f angle)
                a.Add(rotate -25.0f +25.0f angle)
                spriteBatch.DrawPolygon(Vector2(x, y), new Shapes.Polygon(a), Color.Green)

                //spriteBatch.DrawCircle(Vector2(x, y), animal.ViewRange, 100, Color.Blue)
            | Predator ->
                let a = new System.Collections.Generic.List<Vector2>()
                
                a.Add(rotate 15.0f -30.0f angle)
                a.Add(rotate 0.0f 30.f angle)
                a.Add(rotate -15.0f -30.0f angle)

                spriteBatch.DrawPolygon(Vector2(x, y), new Shapes.Polygon(a), Color.Aquamarine)
                //spriteBatch.DrawCircle(Vector2(x, y), animal.ViewRange, 100, Color.Blue)
            | Plant -> 
                spriteBatch.FillRectangle(x, y, 10.0f, 10.0f, Color.Beige)

    let generatePlant () =
        let rnd = System.Random()
        let animalType = Plant
        
        let position = Vector(rnd.Next(1920) |> float32, rnd.Next(1080) |> float32)
        let orientation = rnd.Next(360) |> toRad
        let maxVelocity = 0.0f
        let maxRotation = 0.0f
        let body = Static(position, orientation)
        let behavior = Wander
        let viewRange = 20.0f
            
        let hungar = 10000.0f
        
        { Type = animalType; Body = body; Behavior = behavior; ViewRange = viewRange; IsAlive = true; Hungar = hungar }

    let generateAnimal (rnd: System.Random) = 
        let animalType = if rnd.Next(2) = 0 then Herbivore else Predator

        let position = Vector(rnd.Next(1920) |> float32, rnd.Next(1080) |> float32)
        let orientation = rnd.Next(360) |> toRad
        let maxVelocity = rnd.Next(300) |> float32
        let maxRotation = rnd.Next(360) |> toRad
        //let body = if rnd.Next(2) = 0 then Static(position, orientation) else Kinematic(position, orientation, Vector.zero(), 0.0f, maxVelocity, maxRotation)
        let body = Kinematic(position, orientation, Vector.zero(), 0.0f, maxVelocity, maxRotation)
        let behavior = Wander
        let viewRange = rnd.Next(50, 300) |> float32
    
        let hungar = match animalType with | Herbivore -> 30.0f | _ -> 10.0f

        { Type = animalType; Body = body; Behavior = behavior; ViewRange = viewRange; IsAlive = true; Hungar = hungar }
     
    let rec generateAnimals (rnd: System.Random) (count: int) (animals: Animal list) = 
        match count with 
        | 0 -> animals
        | _ -> generateAnimals rnd (count - 1) ((generateAnimal rnd)::animals)

    let generateWorld count = 
        let rnd = System.Random()

        generateAnimals rnd count []

type Screen (context: ScreenContext, spriteBatch: SpriteBatch) =

    let mutable isEscUpPrev = true
    let mutable animals = []
    let nextPlant = 2.0f
    let mutable nextPlantValue = nextPlant

    do
        animals <- Animal.generateWorld 20

    interface IScreen with

        member _.Init() = ()

        member _.Update(gameTime: GameTime) =
            if not isEscUpPrev && Keyboard.GetState().IsKeyUp(Keys.Escape) then context.Back() else ()
            isEscUpPrev <- Keyboard.GetState().IsKeyUp(Keys.Escape) 

            let delta = gameTime.ElapsedGameTime.TotalSeconds |> float32
            animals <- animals |> List.map (fun x -> Animal.update delta x animals) |> List.filter(fun x -> x.IsAlive)

            nextPlantValue <- nextPlantValue - delta

            if nextPlantValue <= 0.0f 
            then 
                animals <- Animal.generatePlant() :: animals
                nextPlantValue <- nextPlant
            else
                ()


        member _.Draw(gameTime: GameTime) =
            animals |> List.iter (fun x -> Animal.draw spriteBatch x)