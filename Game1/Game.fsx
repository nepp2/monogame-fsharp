
#load "LoadReferences.fsx"

#load "Game1.fs"

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Content
open Microsoft.Xna.Framework.Input
open MonoGame.Extended.Shapes
open System.Threading
open System.IO
open Game

//Microsoft.Xna.Framework.Content.


let game = new Game1 ()

let graphics = new GraphicsDeviceManager(game)
let mutable spriteBatch : SpriteBatch = null


let initialize () = ()
game.initialise <- initialize

let load_content () =
  spriteBatch <- new SpriteBatch(game.GraphicsDevice)

game.load_content <- load_content

do
  let t = new Thread(new ThreadStart(fun _ -> game.Run()))
  t.Start()

// Define a snake
type segment = { x : float ; y : float }
(*
type snake = {
  segments : segment array
  length : float
}
*)
type snake = Vector2

let snake = Vector2(3000.f, 3000.f)

let snake2 = Vector2(3080.f, 3080.f)

let segment_size = 30.f

// Define a camera
let mutable camera = Point(0, 0)

let mutable prev_mouse = new MouseState ()

do // Load update function
  let update gameTime =
    let mouse = Mouse.GetState ()
    if mouse.X > 0 && mouse.X < game.Window.ClientBounds.Width &&
       mouse.Y > 0 && mouse.Y < game.Window.ClientBounds.Height then
      if mouse.LeftButton = ButtonState.Pressed && prev_mouse.LeftButton = ButtonState.Released then
        ()
    prev_mouse <- mouse

  game.update <- update

do // load drawing function
  let draw gameTime =
    let mouse = Mouse.GetState ()
    
    let centre_x = float32 game.Window.ClientBounds.Width / 2.f
    let centre_y = float32 game.Window.ClientBounds.Height / 2.f

    let draw_snake (s : snake) =
      let x = (s.X - snake.X) + centre_x
      let y = (s.Y - snake.Y) + centre_y
      let half_seg = segment_size / 2.f
      let rect = RectangleF(x - half_seg, y - half_seg, segment_size, segment_size)
      spriteBatch.FillRectangle(rect, Color.Aqua)
      ()

    Color(40, 40, 40) |> game.GraphicsDevice.Clear
    spriteBatch.Begin();

    draw_snake snake
    draw_snake snake2

    //spriteBatch.Draw(monstar, new Rectangle(0, 0, 40, 40), Color.White)
    (*
    let t = grid.[i]
    let x = i % len |> float32
    let y = i / len |> float32
    let rect = RectangleF(tile_size * x, tile_size * y, tile_size, tile_size)
    spriteBatch.FillRectangle(rect, tile_colour.[t])
    *)
    // Draw mouse
    spriteBatch.DrawCircle(float32 mouse.X, float32 mouse.Y, 3.f, 10, Color.Red)

    spriteBatch.End();

  game.draw <- draw
