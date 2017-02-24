module Engine

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Content
open Microsoft.Xna.Framework.Input
open MonoGame.Extended.Shapes
open System.Threading
open System.IO
open System.Collections.Generic
open System
open C5

type Mouse = Microsoft.Xna.Framework.Input.Mouse
type MouseState = Microsoft.Xna.Framework.Input.MouseState
type ButtonState = Microsoft.Xna.Framework.Input.ButtonState
type RectangleF = MonoGame.Extended.Shapes.RectangleF
type Vector2 = Microsoft.Xna.Framework.Vector2
type Color = Microsoft.Xna.Framework.Color
type Point = Microsoft.Xna.Framework.Point
type GameTime = Microsoft.Xna.Framework.GameTime

type MList<'a> = System.Collections.Generic.List<'a>

type private Game1 () =
  inherit Game()

  member val initialise = (fun _ -> ()) with get, set
  member val update = (fun _ -> ()) with get, set
  member val draw = (fun _ -> ()) with get, set
  member val load_content = (fun _ -> ()) with get, set

  override x.Initialize() =
    base.Initialize()
    x.initialise ()
    // load the content
  override x.LoadContent() = x.load_content ()
  override x.Update (gameTime) = x.update gameTime
  override x.Draw (gameTime) = x.draw gameTime

type game = private {
    monogame : Game1
    mutable spriteBatch : SpriteBatch
  }
  with
    member this.RenderBounds =
      this.monogame.Window.ClientBounds
    member this.Clear c = this.monogame.GraphicsDevice.Clear c
    member this.FillRectangle (r, c) = this.spriteBatch.FillRectangle (r, c)
    member this.RenderBegin() = this.spriteBatch.Begin()
    member this.RenderEnd() = this.spriteBatch.End()
    member this.DrawLine(x1, y1, x2, y2, c, thickness) = this.spriteBatch.DrawLine(x1, y1, x2, y2, c, thickness)
    member this.DrawCircle(x, y, radius, sides, c, thickness) = this.spriteBatch.DrawCircle(x, y, radius, sides, c, thickness)
 
let get_screen_size (game : game) =
  let bounds = game.RenderBounds
  Vector2(float32 bounds.Width, float32 bounds.Height)

let run_game (width, height, initialize, update, draw) =

  let game = {
    monogame = new Game1 ()
    spriteBatch = null
  }

  let load_content () =
    game.spriteBatch <- new SpriteBatch(game.monogame.GraphicsDevice)

  game.monogame.load_content <- load_content
    

  let graphics = new GraphicsDeviceManager(game.monogame)

  graphics.PreferredBackBufferWidth <- width
  graphics.PreferredBackBufferHeight <- height
  graphics.ApplyChanges()

  game.monogame.initialise <- (fun () -> initialize game)
  game.monogame.update <- (fun gt -> update (game, gt))
  game.monogame.draw <- (fun gt -> draw (game, gt))
  game.monogame.Run()
