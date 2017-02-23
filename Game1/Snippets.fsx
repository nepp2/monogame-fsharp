
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

let game : Game = null

let content_dir = Path.Combine (__SOURCE_DIRECTORY__, "Content")

let monstar =
  let s = Path.Combine (content_dir, "monstar.png") |> File.OpenRead
  Texture2D.FromStream (game.GraphicsDevice, s)

//let monstar = game.Content.Load<Texture2D>("monstar");
// TODO: use this.Content to load your game content here   
//monstar <- x.Content.Load<Texture2D>("monstar.png");

Seq.init 50 (float >> ((*) 100.0) >> (fun x -> System.Math.Pow(x, 0.4))) |> Seq.toArray



let f x = 0.31f
let g x = sqrt (x * 100.f)//x * (f x)
let v_100 = g 20.f
let v_100 = g 100.f
let v_1000 = g 3000.f
let v_1500 = g 5000.f


330.f % 70.f