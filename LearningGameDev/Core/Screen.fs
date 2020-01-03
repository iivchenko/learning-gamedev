namespace LearningGameDev.Core

open Microsoft.Xna.Framework

type IScreen =
    abstract member Update: GameTime -> unit
    abstract member Draw: GameTime -> unit

type EmptyScreen() = 
    interface IScreen with
        member _.Update(_: GameTime) =
            ()
    
        member _.Draw(_: GameTime) =
            ()
    
type ScreenContext() = 
    let empty: IScreen  = EmptyScreen() :> IScreen
    let mutable previousScreen: IScreen option = None
    let mutable currentScreen: IScreen option = None
    
    member _.Screen with get() = 
        match currentScreen with 
        | Some screen -> screen
        | None -> empty
    
    member _.Next(screen: IScreen) =
        previousScreen <- currentScreen
        currentScreen <- Some screen
    
    member _.Back() =
        currentScreen <- previousScreen
        previousScreen <- None