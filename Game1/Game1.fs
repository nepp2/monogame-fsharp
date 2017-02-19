module Game

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Content


type Game1 () =
  inherit Game()

  member val initialise = (fun _ -> ()) with get, set
  member val update = (fun _ -> ()) with get, set
  member val draw = (fun _ -> ()) with get, set
  member val load_content = (fun _ -> ()) with get, set

  override x.Initialize() =
    base.Initialize()
    x.initialise ()

  override x.LoadContent() = x.load_content ()
  override x.Update (gameTime) = x.update ()
  override x.Draw (gameTime) = x.draw ()

let run_game () =

  //do x.Content.RootDirectory <- "C:\Users\Andy\Workspace\monogame-fsharp\Content"

  let game = new Game1 ()

  let graphics = new GraphicsDeviceManager(game)
  let mutable spriteBatch = Unchecked.defaultof<SpriteBatch>
  let mutable monstar : Texture2D = null

  let initialize () =
    do spriteBatch <- new SpriteBatch(game.GraphicsDevice)
    // TODO: Add your initialization logic here
    ()

  let load_content () =
    // TODO: use this.Content to load your game content here   
    //monstar <- x.Content.Load<Texture2D>("monstar.png");
    ()
 
  let update (gameTime) =
    // TODO: Add your update logic here
    ()
 
  let draw (gameTime) =
    do game.GraphicsDevice.Clear Color.CornflowerBlue
        
    spriteBatch.Begin();
 
    //spriteBatch.Draw(monstar, new Rectangle(0, 0, 800, 480), Color.White);
 
    spriteBatch.End();

    ()
  
  game.initialise <- initialize
  game.load_content <- load_content
  game.update <- update
  game.draw <- draw
  
  game.Run ()

