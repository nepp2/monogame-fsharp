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
    mutable speed : float32
    mutable head : Vector2
    waypoints : CircularQueue<Vector2>
    mutable colour : Color
    mutable segment_gap : float32
    mutable segment_size : float32
    mutable poop_cycle : int
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
    member this.Clear () =
      grid.Clear()

    member private this.get_tile_index pos =
      // TODO this calculation may be wrong on negative side of axis
      pos / collision_tile_size |> int

    member this.IterateRectangle (left, top, right, bottom) = seq {
      let left = this.get_tile_index left
      let top = this.get_tile_index top
      let right = this.get_tile_index right
      let bottom = this.get_tile_index bottom
      for x = left to right do
        for y = top to bottom do
          let p = Point(x, y)
          if grid.ContainsKey p then yield! grid.[p]
    }

    member this.IterateSquare (pos : Vector2, size : float32) =
      let half_size = size/2.f
      let left = pos.X - half_size
      let top = pos.Y- half_size
      let right = pos.X + half_size
      let bottom = pos.Y + half_size
      this.IterateRectangle(left, top, right, bottom)

    member private this.add_to_tile (tile, v) =
      if not (grid.ContainsKey tile) then
        grid.[tile] <- new MList<'a>()
      grid.[tile].Add(v)

    member this.AddPoint (pos : Vector2, v) =
      let x = this.get_tile_index pos.X
      let y = this.get_tile_index pos.Y
      this.add_to_tile(Point(x, y), v)

    member this.AddRectangle(left, top, right, bottom, v) =
      let left = this.get_tile_index left
      let top = this.get_tile_index top
      let right = this.get_tile_index right
      let bottom = this.get_tile_index bottom
      for x = left to right do
        for y = top to bottom do
          this.add_to_tile(Point(x, y), v)

    member this.AddSquare (centre : Vector2, size : float32, v) =
      let half_size = size/2.f
      let left = centre.X - half_size
      let top = centre.Y- half_size
      let right = centre.X + half_size
      let bottom = centre.Y + half_size
      this.AddRectangle(left, top, right, bottom, v)

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
    speed = 0.f
    head = head
    waypoints = CircularQueue()
    colour = colour
    segment_size = 30.f
    segment_gap = 20.f
    poop_cycle = 0
  }

type System.Random with
  member x.NextFloat() = x.NextDouble() |> float32

type CircularQueue<'t> with
  member x.PeekLast = x.[x.Count-1]
  member x.FromBack i = x.[x.Count - (i+1)]

let random_color (random : System.Random) =
  let c () = random.NextFloat() * 0.5f + 0.5f
  Color(c(), c(), c())

let get_screen_size (game : Game1) =
  Vector2(float32 game.Window.ClientBounds.Width, float32 game.Window.ClientBounds.Height)

let degrees_to_radians d = d * (float32 Math.PI / 180.f)
let radians_to_degrees r = r * (180.f / float32 Math.PI)

// ################## GAME STATE #################

let mutable prev_mouse = new MouseState ()
let mutable spriteBatch : SpriteBatch = null
let random = new System.Random (50)
let snake_total = 40
let snakes = MList<snake>()
let food = MList<Food>()
let maximum_food = 20000
let mutable total_ticks = 0
let world_size = 7000.f

let max_turn_speed = 5.f

let waypoint_gap = 30.f
let collision_tile_size = 400.f

let boundary_grid = CollisionGrid<RectangleF>(collision_tile_size)
let mutable food_grid = CollisionGrid<int>(collision_tile_size)
let mutable snake_grid = CollisionGrid<Segment>(collision_tile_size)
let dead_food = MList<int>()
let dead_snakes = MList<int>()

// ###############################################

let energy_to_length energy = sqrt (energy * 100.f)

// helper function for interpolating snake body positions
let get_position_at_length_helper (s, length, first_waypoint_to_head, first_waypoint_to_head_length) =
  let distance_from_first_waypoint_to_segment = length - first_waypoint_to_head_length
  if distance_from_first_waypoint_to_segment <= 0.f then
    s.head - (first_waypoint_to_head / first_waypoint_to_head_length) * length
  else
    let waypoint_index = distance_from_first_waypoint_to_segment / waypoint_gap |> int
    if waypoint_index < s.waypoints.Count - 1 then
      let waypoint1 = s.waypoints.FromBack waypoint_index
      let waypoint2 = s.waypoints.FromBack (waypoint_index + 1)
      let waypoint1_offset = float32 waypoint_index * waypoint_gap
      let weight = (distance_from_first_waypoint_to_segment - waypoint1_offset) / waypoint_gap
      waypoint1 * (1.f-weight) + waypoint2 * weight
    else
      s.waypoints.[0]

// interpolate snake body positions
let get_position_at_length (s, length) =
  let first_waypoint_to_head = s.head - s.waypoints.PeekLast
  let first_waypoint_to_head_length = first_waypoint_to_head.Length()
  get_position_at_length_helper(s, length, first_waypoint_to_head, first_waypoint_to_head_length)

// helper function for iterating over the positions in a snake
let iter_snake s =
  seq {
    let first_waypoint_to_head = s.head - s.waypoints.PeekLast
    let first_waypoint_to_head_length = first_waypoint_to_head.Length()
    let length = (energy_to_length s.energy)
    let num_segments = length / s.segment_gap |> int
    let mutable i = 0
    for i = 0 to num_segments-1 do
      let segment_distance = (float32 i * s.segment_gap)
      let pos = get_position_at_length_helper(s, segment_distance, first_waypoint_to_head, first_waypoint_to_head_length)
      yield pos
  }

(*
####### MISSING FEATURES #######

- There should be a maximum turning speed
- AI snakes don't do anything
  - Grazing behaviour
  - Avoidance behaviour
  - Attacking behaviour
  - (Evolve the weightings of these parameters?)

####### BUGGY CRAP #######

- tile lookups may be incorrect in negative space
  - (there is no negative space anymore, so it probably doesn't matter)

*)

let colour_fade (c : Color) =
  let v = c.ToVector3()
  let factor = ((v.X + v.Y + v.Z) / 3.f) / 0.6f
  v / factor |> Color

let rec spawn_random_snake (random : System.Random) =
  let angle = random.NextDouble() * Math.PI * 2.0
  let direction = Vector2(Math.Cos angle |> float32, Math.Sin angle |> float32)
  let head_pos = Vector2(world_size * random.NextFloat(), world_size * random.NextFloat())
  let too_close = snakes.Exists(fun s -> (s.head - head_pos).LengthSquared() < 100.f)
  if too_close then
    spawn_random_snake random
  else
    let colour = random_color random
    create_snake direction head_pos colour

let spawn_food (colour, pos, energy) =
  Food(pos, colour, energy, total_ticks + 1000 + random.Next(2000))

let spawn_dead_snake_food s =
  let spawn_positions = iter_snake s |> Seq.toArray
  let length_factor = 10.f * (s.segment_size / 30.f)
  let colour = colour_fade s.colour
  let mutable food_remaining = s.energy
  while food_remaining > 0.f do  
    let r = random.NextFloat()
    let length = 1.f + (r * r) * length_factor
    food_remaining <- food_remaining - length
    let pos = spawn_positions.[random.Next(spawn_positions.Length)]
    let size = s.segment_size * 1.2f
    let half = size / 2.f
    let food_pos = pos + Vector2(random.NextFloat() * size - half, random.NextFloat() * size - half)
    food.Add (spawn_food (colour, food_pos, length))

// initialise
let initialize (game : Game1) =
  // Spawn some snakes
  Seq.init snake_total (fun i -> spawn_random_snake random) |> snakes.AddRange
  // Initialise the walls
  let width = world_size
  let height = world_size
  let thickness = 1000.f
  let walls =
    [ RectangleF(-thickness, -thickness, width + thickness * 2.f, thickness) // top
      RectangleF(-thickness, height, width + thickness * 2.f, thickness) // bottom
      RectangleF(-thickness, 0.f, thickness, height) // left
      RectangleF(width, 0.f, thickness, height) ] // right
  for w in walls do
    boundary_grid.AddRectangle(w.Left, w.Top, w.Right, w.Bottom, w)

// load the content
let load_content (game : Game1) =
  spriteBatch <- new SpriteBatch(game.GraphicsDevice)

// Handle movement
let handle_movement s =
  s.speed <-
    match s.movement_mode with
      | NormalMove -> 5.f
      | FastMove ->
        let max_poop_size = s.segment_size / 4.f
        let min_energy = 30.f + max_poop_size
        if s.energy > min_energy then
          // Do the pooping
          s.poop_cycle <- (s.poop_cycle + 1) % 3
          if s.poop_cycle = 0 then
            let energy_loss = random.NextFloat() * max_poop_size
            s.energy <- s.energy - energy_loss
            let pos =
              get_position_at_length(s, energy_to_length s.energy) +
              Vector2(random.NextFloat() * 10.f - 5.f, random.NextFloat() * 10.f - 5.f)
            spawn_food (colour_fade s.colour, pos, energy_loss) |> food.Add
          10.f
        else
          5.f

let clean_up_previous_frame () =
  // Remove dead snakes
  if dead_snakes.Count > 0 then
    for i in dead_snakes |> Seq.distinct |> Seq.sortDescending do
      snakes.[i] <- snakes.[snakes.Count-1]
      snakes.RemoveAt(snakes.Count-1)
  dead_snakes.Clear()
  // Remove dead food
  if dead_food.Count > 0 then
    for i in dead_food |> Seq.distinct |> Seq.sortDescending do
      food.[i] <- food.[food.Count-1]
      food.RemoveAt(food.Count-1)
  dead_food.Clear()
  // Clear the collision grids
  food_grid.Clear()
  snake_grid.Clear()

let to_angle (v : Vector2) =
  Math.Atan2 (float v.Y, float v.X) |> float32 |> radians_to_degrees

let change_direction s dir =
  let prev_angle = to_angle s.direction
  let new_angle = to_angle dir
  let mutable a = new_angle - prev_angle
  let sign = a / (abs a)
  while abs a > 180.f do
    a <- a - (360.f * sign)
  let sign = a / (abs a)
  s.direction <-
    if abs a > max_turn_speed then
      let a = prev_angle + max_turn_speed * sign |> degrees_to_radians
      Vector2(cos a, sin a)
    else
      dir

// Update the game
let update (game : Game1, gameTime : GameTime) =
  clean_up_previous_frame ()

  total_ticks <- total_ticks + 1

  let player = snakes.[0]
  let screen_centre = get_screen_size(game) / 2.f

  // Update the player direction
  let mouse = Mouse.GetState ()
  if mouse.X > 0 && mouse.X < game.Window.ClientBounds.Width &&
      mouse.Y > 0 && mouse.Y < game.Window.ClientBounds.Height then
    let mouse_world_pos = Vector2(float32 mouse.X, float32 mouse.Y) + player.head - screen_centre
    change_direction player (mouse_world_pos - player.head)
  player.movement_mode <- if mouse.LeftButton = ButtonState.Pressed then FastMove else NormalMove

  // Moderate the food level
  do // Make some food
    let random_head = snakes.[random.Next(snakes.Count)].head
    let pos = random_head + Vector2(random.NextFloat() * 1000.f - 500.f, random.NextFloat() * 1000.f - 500.f)
    let f = spawn_food(random_color random, pos, 1.f + random.NextFloat() * 2.f)
    food.Add f
  if maximum_food < food.Count then
    let threshold = float32 maximum_food * 0.9f |> int
    while threshold < food.Count do
      let i = random.Next(food.Count)
      food.[i] <- food.[food.Count-1]
      food.RemoveAt(food.Count-1)

  // Update the food
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
    snakes.Add (spawn_random_snake random)

  // Update positions and size
  for i = 0 to snakes.Count-1 do
    let s = snakes.[i]
    // Terrible AI
    if i > 0 then
      let fs =
        food_grid.IterateSquare(s.head, 200.f)
        |> Seq.map (fun i -> food.[i])
        |> Seq.map (fun f -> f, (f.pos - s.head).Length() / f.energy)
      let pos =
        if Seq.isEmpty fs |> not then
          (fs |> Seq.minBy snd |> fst).pos
        else
          let enemy = snakes.[(i + 1) % snakes.Count]
          enemy.head + enemy.direction * 100.f
      change_direction s (pos - s.head)
      if Single.IsNaN s.direction.X || Single.IsNaN s.direction.Y then
        printf "whoopsie"
    // Update the snake's speed
    handle_movement s
    // Move the snake
    let dir = s.direction
    dir.Normalize()
    s.head <- s.head + dir * s.speed
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
  for i = 0 to snakes.Count-1 do
    let s = snakes.[i]
    for pos in iter_snake s do
      snake_grid.AddSquare(pos, s.segment_size, Segment(pos, s.segment_size, i))

  // Detect collisions
  let test_collision s id (segment : Segment) =
    if segment.id = id then false
    else
      let radius = (segment.size + s.segment_size) / 4.f
      (s.head - segment.pos).LengthSquared() < radius * radius

  let boundary_collision s (r : RectangleF) =
    let half_size = s.segment_size / 2.f
    let left = s.head.X - half_size
    let top = s.head.Y- half_size
    let right = s.head.X + half_size
    let bottom = s.head.Y + half_size
    left < r.Right && right > r.Left && top < r.Bottom && bottom > r.Top

  for i = 0 to snakes.Count-1 do
    let s = snakes.[i]
    // Check for collisions with food
    let inline sqr x = x * x
    let segment_radius = s.segment_size * 0.5f
    let segment_squared = sqr segment_radius
    let suction_radius = s.segment_size * 3.f
    let suction_squared = sqr suction_radius
    for f_index in food_grid.IterateSquare(s.head, suction_radius * 2.f) do
      let f = food.[f_index]
      let food_vec = (s.head - f.pos)
      let food_distance_squared = food_vec.LengthSquared()
      if food_distance_squared < segment_squared then
        s.energy <- s.energy + f.energy
        dead_food.Add f_index
      elif food_distance_squared < suction_squared then
        let food_distance = sqrt food_distance_squared
        let dir = s.direction / s.direction.Length()
        let new_pos = f.pos + (food_vec * (segment_radius / food_distance) * 0.3f) + (dir * s.speed * 0.5f)
        food.[f_index] <- Food(new_pos, f.colour, f.energy, f.time_of_expiry)
    let mutable collision =
      // Check for collisions with other snakes
      (snake_grid.IterateSquare(s.head, s.segment_size)
      |> Seq.tryFind (test_collision s i)
      |> Option.isSome)
      // Check for collisions with walls
      ||
      (boundary_grid.IterateSquare(s.head, s.segment_size)
      |> Seq.tryFind (boundary_collision s)
      |> Option.isSome)
    if collision then
      spawn_dead_snake_food s
      dead_snakes.Add i

  prev_mouse <- mouse

// Draw the game
let draw (game : Game1, gameTime) =
  let mouse = Mouse.GetState ()
   
  let screen_size = get_screen_size(game)
  let half_screen = screen_size / 2.f

  let player_pos = snakes.[0].head
  let camera = Vector2(half_screen.X - player_pos.X, half_screen.Y - player_pos.Y)

  // Draw a rectangle
  let draw_square (pos : Vector2, c : Color, size : float32) =
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
      draw_square (pos + camera, s.colour, s.segment_size)
  
  Color(40, 40, 40) |> game.GraphicsDevice.Clear
  spriteBatch.Begin();

  // Draw a grid in the background
  let grid_colour = Color(0.1f, 0.1f, 0.1f)
  let tile_size = 70.f
  let grid_width = screen_size.X / tile_size |> int |> (+) 1
  let grid_height = screen_size.Y / tile_size |> int |> (+) 1
  let grid_offset_x = player_pos.X % tile_size
  let grid_offset_y = player_pos.Y % tile_size
  for x = 0 to grid_width do
    let x = (float32 x * tile_size) - grid_offset_x
    spriteBatch.DrawLine(x, 0.f, x, screen_size.Y, grid_colour, 2.f)
  for y = 0 to grid_height do
    let y = (float32 y * tile_size) - grid_offset_y
    spriteBatch.DrawLine(0.f, y, screen_size.X, y, grid_colour, 2.f)
      
  // Draw all of the snakes
  for i = 0 to snakes.Count-1 do
    draw_snake (camera, snakes.[i])

  let screen = RectangleF(player_pos.X - half_screen.X, player_pos.Y - half_screen.Y, screen_size.X, screen_size.Y)

  // Draw all of the food
  for i in food_grid.IterateRectangle(screen.Left, screen.Top, screen.Right, screen.Bottom) do
    let f = food.[i]
    draw_square (f.pos + camera, f.colour, f.energy * 20.f |> sqrt)

  // Draw the walls
  for w in boundary_grid.IterateRectangle(screen.Left, screen.Top, screen.Right, screen.Bottom) do
    w.Offset(camera)
    spriteBatch.FillRectangle(w, Color.Black)

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

