module Game

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

type Game1 () =
  inherit Game()

  member val initialise = (fun _ -> ()) with get, set
  member val update = (fun _ -> ()) with get, set
  member val draw = (fun _ -> ()) with get, set
  member val load_content = (fun _ -> ()) with get, set

  override x.Initialize() =
    base.Initialize()
    x.initialise x

  override x.LoadContent() = x.load_content x
  override x.Update (gameTime) = x.update (x, gameTime)
  override x.Draw (gameTime) = x.draw (x, gameTime)

// Define a snake

type MList<'a> = List<'a>

type snake = {
  mutable direction : Vector2
  mutable length : float32
  mutable speed : float32
  mutable head : Vector2
  waypoints : CircularQueue<Vector2>
  mutable colour : Color
}

let create_snake direction head colour =
  {
    direction = direction
    length = 1000.f
    speed = 5.f
    head = head
    waypoints = CircularQueue()
    colour = colour
  }

type System.Random with
  member x.NextFloat() = x.NextDouble() |> float32

type CircularQueue<'t> with
  member x.PeekLast = x.[x.Count-1]
  member x.FromBack i = x.[x.Count - (i+1)]

let random_snake (random : System.Random) =
  let angle = random.NextDouble() * Math.PI * 2.0
  let direction = Vector2(Math.Cos angle |> float32, Math.Sin angle |> float32)
  let head_pos = Vector2(1000.f * random.NextFloat(), 1000.f * random.NextFloat())
  let c () = random.NextFloat() * 0.5f + 0.5f
  let colour = Color(c(), c(), c())
  create_snake direction head_pos colour

let screen_size (game : Game1) =
  Vector2(float32 game.Window.ClientBounds.Width, float32 game.Window.ClientBounds.Height)

// ################## GAME STATE #################

let mutable prev_mouse = new MouseState ()
let mutable spriteBatch : SpriteBatch = null
let random = new System.Random (50)
let snakes = Seq.init 10 (fun i -> random_snake random) |> MList

let waypoint_gap = 30.f
let segment_gap = 20.f
let segment_size = 30.f

// ###############################################

let initialize (game : Game1) = ()

let load_content (game : Game1) =
  spriteBatch <- new SpriteBatch(game.GraphicsDevice)

// Update the game
let update (game : Game1, gameTime) =

  let player = snakes.[0]
  let screen_centre = screen_size(game) / 2.f

  // Update the player direction
  let mouse = Mouse.GetState ()
  if mouse.X > 0 && mouse.X < game.Window.ClientBounds.Width &&
      mouse.Y > 0 && mouse.Y < game.Window.ClientBounds.Height then
    let mouse_world_pos = Vector2(float32 mouse.X, float32 mouse.Y) + player.head - screen_centre
    player.direction <- mouse_world_pos - player.head
    player.direction.Normalize()

  // Update snake positions
  for i = 0 to snakes.Count-1 do
    let s = snakes.[i]
    let dir = s.direction
    dir.Normalize()
    s.head <- s.head + (dir * s.speed)
    // check whether a new waypoint needs to be recorded
    if s.waypoints.Count = 0 then
      s.waypoints.Enqueue(s.head)
    else
      let last_pos = s.waypoints.PeekLast
      let diff = s.head - last_pos
      let len = diff.Length()
      if len >= waypoint_gap then
        let new_pos = (diff / len) * waypoint_gap + last_pos
        s.waypoints.Enqueue(new_pos)
    // check whether some old waypoints should be discarded
    let expected_len = max 1 (int s.length)
    while expected_len < s.waypoints.Count do
      s.waypoints.Dequeue() |> ignore


  prev_mouse <- mouse

// Draw the game
let draw (game : Game1, gameTime) =
  let mouse = Mouse.GetState ()
   
  let screen_centre = screen_size(game) / 2.f

  let player_pos = snakes.[0].head
  let camera = Vector2(screen_centre.X - player_pos.X, screen_centre.Y - player_pos.Y)

  // Draw a segment
  let draw_segment (pos : Vector2, c : Color, segment_size : float32) =
    let half_segment = segment_size / 2.f
    let rect = RectangleF(pos.X - half_segment, pos.Y - half_segment, segment_size, segment_size)
    spriteBatch.FillRectangle(rect, c)

  // Draw a snake
  let draw_snake (camera : Vector2, s : snake) =
    let first_waypoint_to_head = s.head - s.waypoints.PeekLast
    let first_waypoint_to_head_length = first_waypoint_to_head.Length()
    let num_segments = s.length / segment_gap |> int
    let mutable i = 0
    let mutable cont = true
    while cont && i < num_segments do
      let segment_distance = (float32 i * segment_gap)
      let distance_from_first_waypoint_to_segment = segment_distance - first_waypoint_to_head_length
      let pos =
        if distance_from_first_waypoint_to_segment <= 0.f then
          s.head - (first_waypoint_to_head / first_waypoint_to_head_length) * segment_distance
        else
          let waypoint_index = distance_from_first_waypoint_to_segment / waypoint_gap |> int
          if waypoint_index < s.waypoints.Count - 1 then
            let waypoint1 = s.waypoints.FromBack waypoint_index
            let waypoint2 = s.waypoints.FromBack (waypoint_index + 1)
            let waypoint1_offset = float32 waypoint_index * waypoint_gap
            let weight = (distance_from_first_waypoint_to_segment - waypoint1_offset) / waypoint_gap
            waypoint1 * (1.f-weight) + waypoint2 * weight
          else
            cont <- false
            s.waypoints.[0]
      draw_segment (pos + camera, s.colour, segment_size)
      i <- i + 1

    let dot_size = 4.f;
    let half_dot = dot_size / 2.f
    for i = 0 to s.waypoints.Count-2 do
      let waypoint = s.waypoints.[i] + camera
      let rect = RectangleF(waypoint.X - half_dot, waypoint.Y - half_dot, dot_size, dot_size)
      spriteBatch.FillRectangle(rect, Color.White)
    let screen_pos = s.head + camera
    let rect = RectangleF(screen_pos.X - half_dot, screen_pos.Y - half_dot, dot_size, dot_size)
    spriteBatch.FillRectangle(rect, Color.White)
    

  Color(40, 40, 40) |> game.GraphicsDevice.Clear
  spriteBatch.Begin();

  // Draw all of the snakes
  for i = 0 to snakes.Count-1 do
    draw_snake (camera, snakes.[i])

  // Draw mouse
  spriteBatch.DrawCircle(float32 mouse.X, float32 mouse.Y, 3.f, 10, Color.Red)

  spriteBatch.End();


let spin_game_thread (game : Game1) =
  do
    let t = new Thread(new ThreadStart(fun _ -> game.Run()))
    t.Start()

let run_game () =

  let game = new Game1 ()

  let graphics = new GraphicsDeviceManager(game)
  let mutable spriteBatch : SpriteBatch = null

  graphics.PreferredBackBufferWidth <- 1920
  graphics.PreferredBackBufferHeight <- 1080
  graphics.ApplyChanges()

  game.initialise <- initialize
  game.load_content <- load_content
  game.update <- update
  game.draw <- draw
  game.Run()

