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

type movement_mode = NormalMove | FastMove

type snake = {
    mutable direction : Vector2
    mutable energy : float32
    mutable movement_mode : movement_mode
    mutable head : Vector2
    waypoints : CircularQueue<Vector2>
    mutable colour : Color
    mutable segment_gap : float32
    mutable segment_size : float32
  }

[<StructAttribute>]
type Food =
  val pos : Vector2
  val colour : Color
  val energy : float32
  val time_of_expiry : int
  new(pos, colour, energy, time_of_expiry) = 
    { pos = pos
      colour = colour
      energy = energy
      time_of_expiry = time_of_expiry }

type CollisionGrid<'a> (collision_tile_size) =
  let grid = Dictionary<Point, MList<'a>>()
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
        grid.[tile] <- new MList<'a>()
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
    energy = 200.f
    movement_mode = NormalMove
    head = head
    waypoints = CircularQueue()
    colour = colour
    segment_size = 30.f
    segment_gap = 20.f
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
let food = MList<Food>()
let maximum_food = 20000
let mutable total_ticks = 0

let waypoint_gap = 30.f
let collision_tile_size = 60.f

// ###############################################

let energy_to_length energy = sqrt (energy * 100.f)

// helper function for iterating over the positions in a snake
let iter_snake s =
  seq {
    let first_waypoint_to_head = s.head - s.waypoints.PeekLast
    let first_waypoint_to_head_length = first_waypoint_to_head.Length()
    let length = (energy_to_length s.energy)
    let num_segments = length / s.segment_gap |> int
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

- There is no background grid
- There is no boundary or wrap-around
- There should be a maximum turning speed
- AI snakes don't do anything
  - Grazing behaviour
  - Avoidance behaviour
  - Attacking behaviour
  - (Evolve the weightings of these parameters?)
- Snakes are not able to spend energy moving more quickly
- Snakes are a bit ugly (maybe there should be a circle sprite?)
- Food is a bit ugly (should have a graphical effect, or a different sprite?)

####### BUGGY CRAP #######

- tile lookups may be incorrect in negative space

*)

let spawn_food (colour, pos, length) =
  Food(pos, colour, length, total_ticks + 1000 + random.Next(2000))

let spawn_dead_snake_food s =
  let spawn_positions = iter_snake s |> Seq.toArray
  let length_factor = 10.f * (s.segment_size / 30.f)
  let mutable food_remaining = s.energy
  while food_remaining > 0.f do  
    let r = random.NextFloat()
    let length = 1.f + (r * r) * length_factor
    food_remaining <- food_remaining - length
    let pos = spawn_positions.[random.Next(spawn_positions.Length)]
    let size = s.segment_size * 1.2f
    let half = size / 2.f
    let food_pos = pos + Vector2(random.NextFloat() * size - half, random.NextFloat() * size - half)
    food.Add (spawn_food (s.colour, food_pos, length))

let speed mode =
  match mode with NormalMove -> 5.f | FastMove -> 10.f

// initialise
let initialize (game : Game1) = ()

// load the content
let load_content (game : Game1) =
  spriteBatch <- new SpriteBatch(game.GraphicsDevice)

// Update the game
let update (game : Game1, gameTime : GameTime) =
  total_ticks <- total_ticks + 1

  let player = snakes.[0]
  let screen_centre = screen_size(game) / 2.f

  // Update the player direction
  let mouse = Mouse.GetState ()
  if mouse.X > 0 && mouse.X < game.Window.ClientBounds.Width &&
      mouse.Y > 0 && mouse.Y < game.Window.ClientBounds.Height then
    let mouse_world_pos = Vector2(float32 mouse.X, float32 mouse.Y) + player.head - screen_centre
    player.direction <- mouse_world_pos - player.head
    player.direction.Normalize()
  player.movement_mode <- if mouse.LeftButton = ButtonState.Pressed then FastMove else NormalMove

    // Moderate the food level
  do // Make some food
    let random_head = snakes.[random.Next(snakes.Count)].head
    let pos = random_head + Vector2(random.NextFloat() * 1000.f - 500.f, random.NextFloat() * 1000.f - 500.f)
    let f = spawn_food(random_color random, pos, 1.f + random.NextFloat() * 4.f)
    food.Add f
  if maximum_food < food.Count then
    let threshold = float32 maximum_food * 0.9f |> int
    while threshold < food.Count do
      let i = random.Next(food.Count)
      food.[i] <- food.[food.Count-1]
      food.RemoveAt(food.Count-1)

  // Update the food
  let food_grid = CollisionGrid(400.f)
  let dead_food = MList()
  for i = 0 to food.Count-1 do
    let f = food.[i]
    // Check if food is dead
    if f.time_of_expiry < total_ticks then
      dead_food.Add i
    else
      // Add food to collision grid
      food_grid.AddPoint(f.pos, i)

  // Add more snakes
  if snakes.Count < snake_total then
    snakes.Add (random_snake random)

  // Update positions and size
  for i = 0 to snakes.Count-1 do
    let s = snakes.[i]
    // Terrible AI
    if i > 0 then
      let fs =
        food_grid.IterateRectangle(s.head, 200.f)
        |> Seq.map (fun i -> food.[i])
        |> Seq.map (fun f -> f, (f.pos - s.head).Length() / f.energy)
      let pos =
        if Seq.isEmpty fs |> not then
          (fs |> Seq.minBy snd |> fst).pos
        else
          let enemy = snakes.[(i + 1) % snakes.Count]
          enemy.head + enemy.direction * 100.f
      s.direction <- pos - s.head
      s.direction.Normalize()
      if Single.IsNaN s.direction.X || Single.IsNaN s.direction.Y then
        printf "whoopsie"
    // Move the snake
    let dir = s.direction
    dir.Normalize()
    s.head <- s.head + dir * (speed s.movement_mode)
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
    let length = energy_to_length s.energy
    let expected_len = max 1 (length / waypoint_gap + 1.f |> int)
    while expected_len < s.waypoints.Count do
      s.waypoints.Dequeue() |> ignore
    // Update the snake segment size
    s.segment_size <- 20.f + sqrt(s.energy) * 0.3f

  // Add snakes to collision grid
  let snake_grid = CollisionGrid(400.f)
  for i = 0 to snakes.Count-1 do
    let s = snakes.[i]
    for pos in iter_snake s do
      snake_grid.AddRectangle(pos, s.segment_size, Segment(pos, s.segment_size, i))

  // Detect collisions
  let test_collision s id (segment : Segment) =
    if segment.id = id then false
    else
      let radius = (segment.size + s.segment_size) / 4.f
      (s.head - segment.pos).LengthSquared() < radius * radius

  let dead_snakes = MList()

  for i = 0 to snakes.Count-1 do
    let s = snakes.[i]
    // Check for collisions with food
    let inline sqr x = x * x
    let segment_radius = s.segment_size * 0.5f
    let segment_squared = sqr segment_radius
    let suction_radius = s.segment_size * 3.f
    let suction_squared = sqr suction_radius
    for f_index in food_grid.IterateRectangle(s.head, suction_radius * 2.f) do
      let f = food.[f_index]
      let food_vec = (s.head - f.pos)
      let food_distance_squared = food_vec.LengthSquared()
      if food_distance_squared < segment_squared then
        s.energy <- s.energy + f.energy
        dead_food.Add f_index
      elif food_distance_squared < suction_squared then
        let food_distance = sqrt food_distance_squared
        let new_pos = f.pos + (food_vec * (segment_radius / food_distance) * 0.3f) + (s.direction * (speed s.movement_mode))
        food.[f_index] <- Food(new_pos, f.colour, f.energy, f.time_of_expiry)
    // Check for collisions with other snakes
    let collision =
      snake_grid.IterateRectangle(s.head, s.segment_size)
      |> Seq.tryFind (test_collision s i)
      |> Option.isSome
    if collision then
      spawn_dead_snake_food s
      dead_snakes.Add i

  // Remove dead snakes
  if dead_snakes.Count > 0 then
    for i in dead_snakes |> Seq.distinct |> Seq.sortDescending do
      snakes.[i] <- snakes.[snakes.Count-1]
      snakes.RemoveAt(snakes.Count-1)
    // Remove dead food
  if dead_food.Count > 0 then
    for i in dead_food |> Seq.distinct |> Seq.sortDescending do
      food.[i] <- food.[food.Count-1]
      food.RemoveAt(food.Count-1)

  prev_mouse <- mouse

// Draw the game
let draw (game : Game1, gameTime) =
  let mouse = Mouse.GetState ()
   
  let screen = screen_size(game)
  let screen_centre = screen / 2.f

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

  // Draw a grid in the background
  let grid_colour = Color(0.1f, 0.1f, 0.1f)
  let tile_size = 70.f
  let grid_width = screen.X / tile_size |> int |> (+) 1
  let grid_height = screen.Y / tile_size |> int |> (+) 1
  let grid_offset_x = player_pos.X % tile_size
  let grid_offset_y = player_pos.Y % tile_size
  for x = 0 to grid_width do
    let x = (float32 x * tile_size) - grid_offset_x
    spriteBatch.DrawLine(x, 0.f, x, screen.Y, grid_colour, 2.f)
  for y = 0 to grid_height do
    let y = (float32 y * tile_size) - grid_offset_y
    spriteBatch.DrawLine(0.f, y, screen.X, y, grid_colour, 2.f)
      
  // Draw all of the snakes
  for i = 0 to snakes.Count-1 do
    draw_snake (camera, snakes.[i])

  // Draw all of the food
  for i = 0 to food.Count-1 do
    let f = food.[i]
    draw_rect (f.pos + camera, f.colour, f.energy * 2.f)

  // Draw mouse
  spriteBatch.DrawCircle(float32 mouse.X, float32 mouse.Y, 9.f, 4, Color.White, 3.f)

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

