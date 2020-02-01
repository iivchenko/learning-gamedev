module Experiment_0003

open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Input
open MonoGame.Extended
open Myra.Graphics2D.UI

open LearningGameDev.Core
open System

type Vector = Vector of x: float32 * y: float32
type Matrix =
    | Matrix of m11: float32 * m12: float32 * m21: float32 * m22: float32
    static member (*) (scalar, (Matrix(m11, m12, m21, m22))) = Matrix(scalar * m11, scalar * m12, scalar * m21, scalar * m22)
    static member (*) ((Vector(v1, v2)), (Matrix(m11, m12, m21, m22))) = Vector(v1 * m11 + v2 * m12, v1 * m21 + v2 * m22)
    static member (*) ((Matrix(a11, a12, a21, a22)), (Matrix(b11, b12, b21, b22))) = Matrix(a11 * b11 + a12 * b21, a11 * b12 + a22 * b22, a21 * b11 + a22 * b21, a21 * b12 + a22 * b22)

type Shape = 
    | Triangle of a1: Vector * a2: Vector * a3: Vector
    | Rectangle of x: float32 * y: float32 * width: float32 * height: float32
    | Circle of x: float32 * y: float32 * r: float32

module Vector = 
    let init x y = Vector(x, y)
    let zero () = Vector(0.0f, 0.0f)

module Matrix = 
    let identity () = Matrix(1.0f, 0.0f, 0.0f, 1.0f)
    let rotation angle = Matrix(cos angle, sin angle, -sin angle, cos angle)
    let scale factor = Matrix(factor, 0.0f, 0.0f, factor)
    let reflection () = -1.0f * identity()
    let shear (Vector(x, y)) = Matrix(1.0f, y, x, 1.0f)

type Entity =
    { Position: Vector
      Orientation: float32
      Scale: float32
      Reflection: bool
      Shear: Vector
      Shape: Shape
      Color: Color }

module Character =

    let toV2 (Vector(x, y)) = Vector2(x, y)
    let toRad degrees = (float degrees) * (System.Math.PI/180.0) |> float32
    
    let draw (spriteBatch: SpriteBatch) entity = 

        let points = new System.Collections.Generic.List<Vector2>()

        let rotation = Matrix.rotation entity.Orientation
        let scale = Matrix.scale entity.Scale
        let reflection = if entity.Reflection then Matrix.reflection () else Matrix.identity()
        let shear = Matrix.shear entity.Shear

        let transform v = v * rotation * scale * reflection * shear |> toV2

        match entity.Shape with 
        | Triangle(a1, a2, a3) -> 
            points.Add(a1 |> transform)
            points.Add(a2 |> transform)
            points.Add(a3 |> transform)
        | Rectangle(x, y, width, height) ->
            let a1 = Vector.init x y
            let a2 = Vector.init (x + width) y
            let a3 = Vector.init (x + width) (y + height)
            let a4 = Vector.init x (y + height)

            points.Add(a1 |> transform)
            points.Add(a2 |> transform)
            points.Add(a3 |> transform)
            points.Add(a4 |> transform)
        | Circle(x, y, r) ->
            [-100..100] 
                |> List.map float32 
                |> List.map (fun x -> Vector.init x ((pown r 2) - (pown x 2) |> sqrt)) 
                |> List.map transform
                |> List.iter (fun v -> points.Add(v))

            [-100..100] 
                |> List.map float32 
                |> List.map (fun x -> Vector.init x ((pown r 2) - (pown x 2) |> sqrt))
                |> List.map (fun v -> v * Matrix.reflection())
                |> List.map transform                
                |> List.iter (fun v -> points.Add(v))

        spriteBatch.DrawPolygon(entity.Position |> toV2, new Shapes.Polygon(points), entity.Color)

type Screen (context: ScreenContext, spriteBatch: SpriteBatch) =

    let mutable isEscUpPrev = true
    let mutable character = 
        { 
            Position = Vector.init (1920.0f/2.0f + 50.0f) (1080.0f/2.0f)
            Orientation = 0.0f
            Scale = 1.0f
            Reflection = false
            Shear = Vector.zero()
            Shape = Triangle(Vector.init 15.0f -30.0f, Vector.init 0.0f 30.0f, Vector.init -15.0f -30.0f) 
            Color = Color.Aquamarine 
        }  

    let rotationSpin = new SpinButton()
    let scaleSpin = new SpinButton()
    let reflectionCheck = new CheckBox()
    let shearXSpin = new SpinButton()
    let shearYSpin = new SpinButton()
    let shapesList = new ListBox()

    let unwrap (value: _ Nullable) = if value.HasValue then value.Value else 0.0f
    let getShape = 
        function
        | 0 -> Triangle(Vector.init 15.0f -30.0f, Vector.init 0.0f 30.0f, Vector.init -15.0f -30.0f) 
        | 1 -> Rectangle(-15.0f, -15.0f, 30.0f, 30.0f)
        | 2 -> Circle(0.0f, 0.0f, 30.0f)
        | _ -> raise (Exception ("No shape"))

    interface IScreen with 

        member _.Init () =

            Desktop.Widgets.Clear()
            
            let grid = new Grid()
            grid.ColumnSpacing <- 8
            grid.RowSpacing <- 8
            
            let proportion = new Proportion()
            let fillProportion = new Proportion()
            proportion.Type <- Myra.Graphics2D.UI.ProportionType.Auto
            fillProportion.Type <- Myra.Graphics2D.UI.ProportionType.Fill
            grid.DefaultRowProportion <- proportion
            grid.ColumnsProportions.Add(proportion)
            grid.ColumnsProportions.Add(proportion)
            grid.ColumnsProportions.Add(fillProportion)
            
            let rotationLabel = new Label()
            rotationLabel.Text <- "Rotation: "
            rotationLabel.GridRow <- 0
            rotationLabel.GridColumn <- 0
            
            rotationSpin.Value <- new System.Nullable<float32>(0.0f)
            rotationSpin.Width <- new System.Nullable<int>(50)
            rotationSpin.Minimum <- new System.Nullable<float32>(-360.0f)
            rotationSpin.Maximum <- new System.Nullable<float32>(+360.0f)
            rotationSpin.GridRow <- 0
            rotationSpin.GridColumn <- 1

            let scaleLabel = new Label()
            scaleLabel.Text <- "Scale: "
            scaleLabel.GridRow <- 1
            scaleLabel.GridColumn <- 0
            
            scaleSpin.Value <- new System.Nullable<float32>(1.0f)
            scaleSpin.Width <- new System.Nullable<int>(50)
            scaleSpin.Minimum <- new System.Nullable<float32>(1.0f)
            scaleSpin.Maximum <- new System.Nullable<float32>(+30.0f)
            scaleSpin.GridRow <- 1
            scaleSpin.GridColumn <- 1

            let reflectionLabel = new Label()
            reflectionLabel.Text <- "Reflection: "
            reflectionLabel.GridRow <- 2
            reflectionLabel.GridColumn <- 0
            
            reflectionCheck.IsPressed <- false
            reflectionCheck.Width <- new System.Nullable<int>(50)
            reflectionCheck.GridRow <- 2
            reflectionCheck.GridColumn <- 1

            let shearXLabel = new Label()
            shearXLabel.Text <- "Shear X: "
            shearXLabel.GridRow <- 3
            shearXLabel.GridColumn <- 0
                       
            shearXSpin.Value <- new System.Nullable<float32>(0.0f)
            shearXSpin.Increment <- 0.1f
            shearXSpin.Width <- new System.Nullable<int>(50)
            shearXSpin.Minimum <- new System.Nullable<float32>(-5.0f)
            shearXSpin.Maximum <- new System.Nullable<float32>(5.0f)
            shearXSpin.GridRow <- 3
            shearXSpin.GridColumn <- 1

            let shearYLabel = new Label()
            shearYLabel.Text <- "Shear Y: "
            shearYLabel.GridRow <- 4
            shearYLabel.GridColumn <- 0
                       
            shearYSpin.Value <- System.Nullable<float32>(0.0f)
            shearYSpin.Increment <- 0.1f
            shearYSpin.Width <- new System.Nullable<int>(50)
            shearYSpin.Minimum <- new System.Nullable<float32>(-5.0f)
            shearYSpin.Maximum <- new System.Nullable<float32>(5.0f)
            shearYSpin.GridRow <- 4
            shearYSpin.GridColumn <- 1

            let shapesLabel = new Label()
            shapesLabel.Text <- "Shapes: "
            shapesLabel.GridRow <- 5
            shapesLabel.GridColumn <- 0

            let triangleListItem = new ListItem()
            triangleListItem.Text <- "Triangle"

            let rectangleListItem = new ListItem()
            rectangleListItem.Text <- "Rectangle"

            let circleListItem = new ListItem()
            circleListItem.Text <- "Circle"

            shapesList.Items.Add(triangleListItem)
            shapesList.Items.Add(rectangleListItem)
            shapesList.Items.Add(circleListItem)
            shapesList.GridRow <- 5
            shapesList.GridColumn <- 1
            shapesList.SelectedIndex <- System.Nullable<int>(0)

            grid.Widgets.Add(rotationLabel)
            grid.Widgets.Add(rotationSpin)
            grid.Widgets.Add(scaleLabel)
            grid.Widgets.Add(scaleSpin)
            grid.Widgets.Add(reflectionLabel)
            grid.Widgets.Add(reflectionCheck)
            grid.Widgets.Add(shearXLabel)
            grid.Widgets.Add(shearXSpin)
            grid.Widgets.Add(shearYLabel)
            grid.Widgets.Add(shearYSpin)
            grid.Widgets.Add(shapesLabel)
            grid.Widgets.Add(shapesList)
            
            Desktop.Widgets.Add(grid)

        member _.Update(gameTime: GameTime) =
            if not isEscUpPrev && Keyboard.GetState().IsKeyUp(Keys.Escape) then context.Back() else ()
            isEscUpPrev <- Keyboard.GetState().IsKeyUp(Keys.Escape)

            character <- 
                { 
                    character with 
                        Orientation = rotationSpin.Value |> unwrap |> Character.toRad
                        Scale = scaleSpin.Value |> unwrap
                        Reflection = reflectionCheck.IsPressed
                        Shear = Vector.init (shearXSpin.Value |> unwrap) (shearYSpin.Value |> unwrap)
                        Shape = getShape shapesList.SelectedIndex.Value
                }

            
        member _.Draw(gameTime: GameTime) =
            let delta = gameTime.ElapsedGameTime.TotalSeconds |> float32

            Character.draw spriteBatch character

            Desktop.Render()