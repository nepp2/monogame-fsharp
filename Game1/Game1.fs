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
    mutable segment_gap : float32
    mutable segment_size : float32
  }

type food = {
  mutable pos : Vector2
  mutable colour : Color
  mutable length : float32
  mutable time_to_live : int
}

type CollisionGrid<'a> (collision_tile_size) =
  let grid = Dictionary<Point, List<'a>>()
  with
    member private this.get_tile_index pos =
      // TODO this calculation may be wrong on negative side of axis
      pos / collision_tile_size |> int

    member this.IterateRectangle (pos : Vector2, size : float32) = seq {
      let half_size = size/2.f
      let left = this.get_tile_index (pos.X - half_size)
      let top = this.get_tile_index (pos.Y- half_size)
      let right = this.get_tile_index (pos.X + half_size)
      let bottom = this.get_tile_index (pos.Y + half_size)
      for x = left to right do
        for y = top to bottom do
          let p = Point(x, y)
          if grid.ContainsKey p then yield! grid.[p]
    }

    member private this.add_to_tile (tile, v) =
      if not (grid.ContainsKey tile) then
        grid.[tile] <- List<'a>()
      grid.[tile].Add(v)

    member this.AddPoint (pos : Vector2, v) =
      let x = this.get_tile_index pos.X
      let y = this.get_tile_index pos.Y
      this.add_to_tile(Point(x, y), v)

    member this.AddRectangle (centre : Vector2, size : float32, v) =
      let half_size = size/2.f
      let left = this.get_tile_index (centre.X - half_size)
      let top = this.get_tile_index (centre.Y- half_size)
      let right = this.get_tile_index (centre.X + half_size)
      let bottom = this.get_tile_index (centre.Y + half_size)
      for x = left to right do
        for y = top to bottom do
          this.add_to_tile(Point(x, y), v)
          

[<StructAttribute>]
type Segment =
  val pos : Vector2
  val size : float32
  val id : int
  new(pos, size, id) = 
    { pos = pos
      size = size
      id = id }

let create_snake direction head colour =
  {
    direction = direction
    length = 200.f
    speed = 5.f
    head = head
    waypoints = CircularQueue()
    colour = colour
    segment_size = 30.f
    segment_gap = 20.f
  }

let create_food (colour, pos, length, random : Random) =
  {
    colour = colour
    pos = pos
    length = length
    time_to_live = 1000 + random.Next(2000)
  }

type System.Random with
  member x.NextFloat() = x.NextDouble() |> float32

type CircularQueue<'t> with
  member x.PeekLast = x.[x.Count-1]
  member x.FromBack i = x.[x.Count - (i+1)]

let random_color (random : System.Random) =
  let c () = random.NextFloat() * 0.5f + 0.5f
  Color(c(), c(), c())

let random_snake (random : System.Random) =
  let angle = random.NextDouble() * Math.PI * 2.0
  let direction = Vector2(Math.Cos angle |> float32, Math.Sin angle |> float32)
  let head_pos = Vector2(7000.f * random.NextFloat(), 7000.f * random.NextFloat())
  let colour = random_color random
  create_snake direction head_pos colour

let screen_size (game : Game1) =
  Vector2(float32 game.Window.ClientBounds.Width, float32 game.Window.ClientBounds.Height)

// ################## GAME STATE #################

let mutable prev_mouse = new MouseState ()
let mutable spriteBatch : SpriteBatch = null
let random = new System.Random (50)
let snake_total = 40
let snakes = Seq.init snake_total (fun i -> random_snake random) |> MList
let food = MList<food>()

let waypoint_gap = 30.f
let collision_tile_size = 60.f

// ###############################################

// helper function for iterating over the positions in a snake
let iter_snake s =
  seq {
    let first_waypoint_to_head = s.head - s.waypoints.PeekLast
    let first_waypoint_to_head_length = first_waypoint_to_head.Length()
    let num_segments = s.length / s.segment_gap |> int
    let mutable i = 0
    let mutable cont = true
    while cont && i < num_segments do
      let segment_distance = (float32 i * s.segment_gap)
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
      yield pos
      i <- i + 1
  }

(*
####### MISSING FEATURES #######

- Dead snakes don't turn into food
- There is no boundary or wrap-around
- AI snakes don't do anything (should probably blend a basic grazing behaviour and avoidance behaviour)
- New snakes never appear

####### BUGGY CRAP #######

- tile lookups may be incorrect in negative space

*)

let spawn_dead_snake_food s =
  let spawn_positions = iter_snake s |> Seq.toArray
  let food_per_segment = s.length / float32 spawn_positions.Length
  for pos in spawn_positions do
    let mutable seg_food = food_per_segment
    while seg_food > 0.f do
      let r = random.NextFloat()
      let length = 1.f + (r * r) * 10.f
      seg_food <- seg_food - length
      let food_pos = pos + Vector2(random.NextFloat() * 20.f - 10.f, random.NextFloat() * 20.f - 10.f)
      food.Add (create_food (s.colour, food_pos, length, random))


// initialise
let initialize (game : Game1) = ()

// load the content
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


  // Update the food
  let food_grid = CollisionGrid(60.f)
  let dead_food = MList()
  for i = 0 to food.Count-1 do
    let f = food.[i]
    // Check if food is dead
    f.time_to_live <- f.time_to_live - 1
    if f.time_to_live < 0 then
      dead_food.Add i
    else
      // Add food to collision grid
      food_grid.AddPoint(f.pos, i)

  // Add more snakes
  if snakes.Count < snake_total then
    snakes.Add (random_snake random)

  // Add more food
  let expected_food = snakes.Count * 20
  if expected_food > food.Count then
    let random_head = snakes.[random.Next(snakes.Count)].head
    let f = {
      colour = random_color random
      pos = random_head + Vector2(random.NextFloat() * 1000.f - 500.f, random.NextFloat() * 1000.f - 500.f)
      length = 1.f + random.NextFloat() * 4.f
      time_to_live = 1000 + random.Next(2000)
    }
    food.Add f

  // Update positions and size
  for i = 0 to snakes.Count-1 do
    let s = snakes.[i]
    if i > 0 then
      // Terrible AI
      let fs =
        food_grid.IterateRectangle(s.head, 200.f)
        |> Seq.map (fun i -> food.[i])
        |> Seq.map (fun f -> f, (f.pos - s.head).Length() / f.length)
      let pos =
        if Seq.isEmpty fs |> not then
          (fs |> Seq.minBy snd |> fst).pos
        else
          let enemy = snakes.[(i + 1) % snakes.Count]
          enemy.head + enemy.direction * 100.f
      s.direction <- pos - s.head
      s.direction.Normalize()

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
    let expected_len = max 1 (s.length / waypoint_gap + 1.f |> int)
    while expected_len < s.waypoints.Count do
      s.waypoints.Dequeue() |> ignore
    // Update the snake segment size
    s.segment_size <- 10.f + sqrt(s.length)
    s.segment_gap <- 20.f

  // Add snakes to collision grid
  let snake_grid = CollisionGrid(60.f)
  for i = 0 to snakes.Count-1 do
    let s = snakes.[i]
    for pos in iter_snake s do
      snake_grid.AddRectangle(pos, s.segment_size, Segment(pos, s.segment_size, i))

  // Detect collisions
  let test_collision head id (segment : Segment) =
    segment.id <> id &&
    (head - segment.pos).LengthSquared() < segment.size * segment.size

  let dead_snakes = MList()

  for i = 0 to snakes.Count-1 do
    let s = snakes.[i]
    // Check for collisions with food
    let inline sqr x = x * x
    let segment_size_squared = sqr (s.segment_size * 1.f)
    let suction_squared = sqr (s.segment_size * 10.f)
    for f_index in food_grid.IterateRectangle(s.head, 60.f) do
      let f = food.[f_index]
      let food_vec = (s.head - f.pos)
      let food_distance = food_vec.LengthSquared()
      if food_distance < segment_size_squared then
        s.length <- s.length + f.length
        dead_food.Add f_index
      elif food_distance < suction_squared then
        f.pos <- f.pos + (food_vec * 0.1f)
    // Check for collisions with other snakes
    let collision =
      snake_grid.IterateRectangle(s.head, s.segment_size)
      |> Seq.tryFind (test_collision s.head i)
      |> Option.isSome
    if collision then
      spawn_dead_snake_food s
      dead_snakes.Add i
      s.length <- 1.f

  // Remove dead snakes
  dead_snakes.Sort()
  for i = 1 to dead_snakes.Count do
    let i = dead_snakes.[dead_snakes.Count - i]
    snakes.[i] <- snakes.[snakes.Count-1]
    snakes.RemoveAt(snakes.Count-1)
  // Remove dead food
  dead_food.Sort()
  for i = 1 to dead_food.Count do
    let i = dead_food.[dead_food.Count - i]
    food.[i] <- food.[food.Count-1]
    food.RemoveAt(food.Count-1)

  prev_mouse <- mouse

// Draw the game
let draw (game : Game1, gameTime) =
  let mouse = Mouse.GetState ()
   
  let screen_centre = screen_size(game) / 2.f

  let player_pos = snakes.[0].head
  let camera = Vector2(screen_centre.X - player_pos.X, screen_centre.Y - player_pos.Y)

  // Draw a segment
  let draw_rect (pos : Vector2, c : Color, size : float32) =
    let half_segment = size / 2.f
    let rect = RectangleF(pos.X - half_segment, pos.Y - half_segment, size, size)
    spriteBatch.FillRectangle(rect, c)

  // Draw a snake
  let draw_snake (camera : Vector2, s : snake) =
    (*
    // Draw dot trail
    let dot_size = 4.f;
    for i = 0 to s.waypoints.Count-1 do
      draw_rect (s.waypoints.[i] + camera, Color.White, dot_size)
    *)
    // Draw segments
    for pos in iter_snake s do
      draw_rect (pos + camera, s.colour, s.segment_size)

  
  Color(40, 40, 40) |> game.GraphicsDevice.Clear
  spriteBatch.Begin();

  // Draw all of the snakes
  for i = 0 to snakes.Count-1 do
    draw_snake (camera, snakes.[i])

  // Draw all of the food
  for i = 0 to food.Count-1 do
    let f = food.[i]
    draw_rect (f.pos + camera, f.colour, f.length * 2.f)

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

