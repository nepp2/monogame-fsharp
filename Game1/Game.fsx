
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

// Define some grid (why?)
let len = 16

let grid =
  let r = new System.Random ()
  Array.init (len * len) (fun _ -> r.Next 2)

let tile_size = 40.f
let tile_colour = [| Color.White ; Color.Black |]

tile_colour.[1] <- Color.Azure

type tiles =
  | Ground = 0
  | Wall = 1


// Define a snake
type segment = { x : float ; y : float }

type snake = {
  segments : segment array
  length : float

}


// Define a camera
let mutable camera = Point(0, 0)

let mutable prev_mouse = new MouseState ()

do // Load update function
  let update gameTime =
    let mouse = Mouse.GetState ()
    if mouse.X > 0 && mouse.X < game.Window.ClientBounds.Width &&
       mouse.Y > 0 && mouse.Y < game.Window.ClientBounds.Height then
      if mouse.LeftButton = ButtonState.Pressed && prev_mouse.LeftButton = ButtonState.Released then
        let x = (mouse.X / int tile_size) % len
        let y = (mouse.Y / int tile_size) % len
        let i = y * len + x
        grid.[i] <- 1 - grid.[i]
  
    prev_mouse <- mouse

  game.update <- update

do // load drawing function
  let draw gameTime =
    let mouse = Mouse.GetState ()
    game.GraphicsDevice.Clear Color.Red
    spriteBatch.Begin();
    //spriteBatch.Draw(monstar, new Rectangle(0, 0, 40, 40), Color.White)
    for i = 0 to grid.Length-1 do
      let t = grid.[i]
      let x = i % len |> float32
      let y = i / len |> float32
      let rect = RectangleF(tile_size * x, tile_size * y, tile_size, tile_size)
      spriteBatch.FillRectangle(rect, tile_colour.[t])

    //spriteBatch.FillRectangle(RectangleF(100.f, 100.f, 40.f, 40.f), tile_colour.[t])

    spriteBatch.DrawCircle(float32 mouse.X, float32 mouse.Y, 3.f, 10, Color.Red)

    spriteBatch.End();

  game.draw <- draw
