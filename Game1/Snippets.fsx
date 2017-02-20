
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
