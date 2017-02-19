
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

let game = new Game1 ()

let graphics = new GraphicsDeviceManager(game)
let mutable spriteBatch : SpriteBatch = null

let content_dir = Path.Combine (__SOURCE_DIRECTORY__, "Content")

let initialize () =
  // TODO: Add your initialization logic here
  ()

let load_content () =
  do spriteBatch <- new SpriteBatch(game.GraphicsDevice)
  game.Content.RootDirectory <- __SOURCE_DIRECTORY__ + "/Content/bin"
  //let monstar = game.Content.Load<Texture2D>("monstar");
  // TODO: use this.Content to load your game content here   
  //monstar <- x.Content.Load<Texture2D>("monstar.png");
  ()
 
  
game.initialise <- initialize
game.load_content <- load_content
  
//game.Run ()
do
  let t = new Thread(new ThreadStart(fun _ -> game.Run()))
  t.Start()

(*
let monstar =
  let s = Path.Combine (content_dir, "monstar.png") |> File.OpenRead
  Texture2D.FromStream (game.GraphicsDevice, s)
*)
//monstar.SetData()

let len = 16

let array =
  let r = new System.Random ()
  Array.init (len * len) (fun _ -> r.Next 2)

let tile_size = 40.f

let tile_colour = [| Color.White ; Color.Black |]

type tiles =
  | Ground = 0
  | Wall = 1

let mutable prev_mouse = new MouseState ()

let update gameTime =
  let mouse = Mouse.GetState ()
  if mouse.X > 0 && mouse.X < game.Window.ClientBounds.Width &&
     mouse.Y > 0 && mouse.Y < game.Window.ClientBounds.Height then
    if mouse.LeftButton = ButtonState.Pressed && prev_mouse.LeftButton = ButtonState.Released then
      let x = (mouse.X / int tile_size) % len
      let y = (mouse.Y / int tile_size) % len
      let i = y * len + x
      array.[i] <- 1 - array.[i]
  
  prev_mouse <- mouse

game.update <- update

let draw gameTime =
  let mouse = Mouse.GetState ()
  game.GraphicsDevice.Clear Color.BlanchedAlmond
  spriteBatch.Begin();
  //spriteBatch.Draw(monstar, new Rectangle(0, 0, 40, 40), Color.White)
  for i = 0 to array.Length-1 do
    let t = array.[i]
    let x = i % len |> float32
    let y = i / len |> float32
    let rect = RectangleF(tile_size * x, tile_size * y, tile_size, tile_size)
    spriteBatch.FillRectangle(rect, tile_colour.[t])
  //spriteBatch.FillRectangle(RectangleF(100.f, 100.f, 40.f, 40.f), tile_colour.[t])

  spriteBatch.DrawCircle(float32 mouse.X, float32 mouse.Y, 3.f, 10, Color.Red)

  spriteBatch.End();

game.draw <- draw

//let game = new Game1 ()
//game.Content.RootDirectory <- "Content"

(*

Thread.Sleep(500)

let graphics = new GraphicsDeviceManager(game)
let spriteBatch = new SpriteBatch(game.GraphicsDevice)

printf "asasdadsaa"

game.draw <-
  fun gameTime ->
    do game.GraphicsDevice.Clear Color.BlanchedAlmond
    spriteBatch.Begin();
    //spriteBatch.Draw(monstar, new Rectangle(0, 0, 800, 480), Color.White);
    spriteBatch.End();

*)