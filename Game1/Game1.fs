module Game

open System.IO
open System.Collections.Generic
open System

open Engine
open GameTypes
open Utils
open CollisionGrid
open C5

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

// ################## GAME STATE #################

let random = new System.Random (50)
let snake_total = 40
let snakes = MList<snake>()
let food = MList<Food>()
let maximum_food = 20000
let mutable total_ticks = 0L
let world_size = 7000.f

let last_dump_time = 0

let max_turn_speed = 8.f
let normal_speed = 4.f
let fast_speed = 8.f

let waypoint_gap = 30.f
let collision_tile_size = 400.f

let genepool = new CircularQueue<float32[] * float32>()
let genepool_size = 100

let mutable run_fast = false

let mutable last_frame_keyboard = KeyboardState()

let boundary_grid = CollisionGrid<RectangleF>(collision_tile_size)
let mutable food_grid = CollisionGrid<int>(collision_tile_size)
let mutable snake_grid = CollisionGrid<Segment>(collision_tile_size)
let dead_food = MList<int>()
let dead_snakes = MList<int>()

// ################ Helper stuff #################

let energy_to_length energy = sqrt (energy * 100.f)

/// helper function for interpolating snake body positions
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

/// interpolate snake body positions
let get_position_at_length (s, length) =
  if s.waypoints.Count > 0 then
    let first_waypoint_to_head = s.head - s.waypoints.PeekLast
    let first_waypoint_to_head_length = first_waypoint_to_head.Length()
    get_position_at_length_helper(s, length, first_waypoint_to_head, first_waypoint_to_head_length)
  else
    s.head

/// helper function for iterating over the positions in a snake
let iter_snake s =
  seq {
    if s.waypoints.Count > 0 then
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

let json_dump_weights () =
  Directory.CreateDirectory (@"..\..\evolved_weights") |> ignore
  let s = Newtonsoft.Json.JsonConvert.SerializeObject(genepool |> Seq.toArray)
  let path = sprintf @"..\..\evolved_weights\weights_%s.json" (DateTime.Now.ToString("yyyy-MM-dd_HH-mm-ss"))
  File.WriteAllText(path, s)


// ################### GAME AI ###################

module AI =

  let player_control (game : game) s id =
    let screen_centre = get_screen_size(game) / 2.f
    let mouse = Mouse.GetState ()
    let bounds = game.RenderBounds
    let dir =
      if mouse.X > 0 && mouse.X < bounds.Width &&
          mouse.Y > 0 && mouse.Y < bounds.Height then
        let mouse_world_pos = Vector2(float32 mouse.X, float32 mouse.Y) + s.head - screen_centre
        mouse_world_pos - s.head
      else
        s.direction
    let mode = if mouse.LeftButton = ButtonState.Pressed then FastMove else NormalMove
    Action (dir, mode)

  let alpha_slither (weights : float32[]) game s id =
    let radius = 400.f
    let eat_distance_weight = weights.[0]
    let eat_energy_weight = weights.[1]
    let danger_distance_weight = weights.[2]
    let hunt_distance_weight = weights.[3]
    let hunt_orbit_weight = weights.[4]
    let hunt_size_weight = weights.[5]
    let eat_decision_weight = weights.[6]
    let danger_decision_weight = weights.[7]
    let hunt_decision_weight = weights.[8]
    let speed_threshold = weights.[9]

    // grazing
    let eat_vec, eat_weight =
      let mutable v = Vector2()
      let mutable total_weight = 0.f
      for i in food_grid.IterateSquare(s.head, radius * 2.f) do
        let f = food.[i]
        let dir = f.pos - s.head
        let len = dir.Length()
        if len < radius then
          let distance_weight = ((radius - len) / radius) * eat_distance_weight
          let energy_weight = f.energy * eat_energy_weight
          v <- v + (dir / len) * (distance_weight + energy_weight)
          total_weight <- total_weight + distance_weight + energy_weight
      v, total_weight

    // danger
    let danger_vec, danger_weight =
      let mutable v = Vector2()
      let mutable total_weight = 0.f
      for seg in snake_grid.IterateSquare(s.head, radius * 2.f) |> Seq.distinct do
        if seg.id <> id then
          let dir = s.head - seg.pos
          let dir_len = dir.Length()
          let len = dir_len - (s.segment_size + seg.size) / 2.f
          if len < radius then
            let weight = ((radius - len) / radius) * danger_distance_weight
            v <- v + (dir / dir_len) * weight
            total_weight <- total_weight + weight
      let head_rect = square_to_rect s.head s.segment_size
      for r in boundary_grid.IterateSquare(s.head, radius * 2.f) |> Seq.distinct do
        let dir = rect_closest_vector_from_to r head_rect
        let len = dir.Length()
        if len < radius then
          let weight = ((radius - len) / radius) * danger_distance_weight
          v <- v + (dir / len) * weight
          total_weight <- total_weight + weight
      v, total_weight

    // Hunt
    let hunt_vec, hunt_weight =
      let mutable v = Vector2()
      let mutable total_weight = 0.f
      let nearby_snakes =
        snake_grid.IterateSquare(s.head, radius * 2.f)
        |> Seq.map (fun seg -> seg.id) |> Seq.distinct
      let centre_of_mass s samples max_length =
        let snake_length = min max_length (energy_to_length s.energy)
        let mutable sum_pos = Vector2()
        for v = 1 to samples do
          sum_pos <- sum_pos + get_position_at_length (s, (snake_length / float32 samples) * float32 v)
        sum_pos / (float32 samples)
      let snake_centre = centre_of_mass s 5 200.f
      for i in nearby_snakes do
        if i <> id then
          let enemy = snakes.[i]
          let distance = (enemy.head - s.head).Length() - (s.segment_size + enemy.segment_size) / 2.f
          if distance < radius then
            let enemy_centre = centre_of_mass enemy 5 200.f
            let attack_dir = (snake_centre + ((enemy_centre - snake_centre) * 2.f)) - s.head
            let caution_dir = s.head - enemy_centre
            let dir = (attack_dir.Normalized() + caution_dir.Normalized()) / 2.f
            let distance_weight = (distance - hunt_orbit_weight * 400.f |> abs) / radius
            let size_weight = enemy.segment_size * hunt_size_weight
            v <- v + dir * (distance_weight + size_weight)
            total_weight <- total_weight + distance_weight + size_weight
      v, total_weight

    // Otherwise
    let kamikaze_vec =
      let enemy = snakes.[(id+1) % snakes.Count]
      let enemy_pos = enemy.head + enemy.direction.Normalized() * 100.f
      enemy_pos - s.head

    let options = [|
      //eat_weight * eat_decision_weight, eat_vec
      //danger_weight * danger_decision_weight, danger_vec
      hunt_weight * hunt_decision_weight, hunt_vec
      Single.Epsilon, kamikaze_vec
    |]
    let max = options |> Array.maxBy fst
    let direction = snd max
    if direction = Vector2.Zero then
      Action(kamikaze_vec, NormalMove)
    else
      let move = if (fst max) > speed_threshold * 100.f then FastMove else NormalMove
      //Console.WriteLine("Eat: {0}, Danger: {0}, Hunt: {0}", fst options.[0], fst options.[1], fst options.[2])
      Action(direction, move)

  let alpha_slither_random_weights () =
    Array.init 10 (fun _ -> random.NextFloat()) |> AlphaSlither

  let dumb_ai game s id =
    let fs =
      food_grid.IterateSquare(s.head, 200.f)
      |> Seq.map (fun i -> food.[i])
      |> Seq.map (fun f -> f, (f.pos - s.head).Length() / f.energy)
    let pos =
      if Seq.isEmpty fs |> not then
        (fs |> Seq.minBy snd |> fst).pos
      else
        let enemy = snakes.[(id+1) % snakes.Count]
        enemy.head + enemy.direction.Normalized() * 100.f
    let dir = pos - s.head
    Action (dir, NormalMove)

  let run_controller c game s id =
    match c with
    | Human -> player_control game s id
    | AlphaSlither ws -> alpha_slither ws game s id
    | DumbAI -> dumb_ai game s id

// ###############################################

let random_color (random : System.Random) =
  let c () = random.NextFloat() * 0.5f + 0.5f
  Color(c(), c(), c())

/// Used to fade a snake's colour a bit for food created from its body
let colour_fade (c : Color) =
  let v = c.ToVector3()
  let factor = ((v.X + v.Y + v.Z) / 3.f) / 0.6f
  v / factor |> Color

/// Spawn a snake somewhere. Try to avoid other snakes.
let rec spawn_random_snake controller =
  let angle = random.NextDouble() * Math.PI * 2.0
  let direction = Vector2(Math.Cos angle |> float32, Math.Sin angle |> float32)
  let head_pos = Vector2(world_size * random.NextFloat(), world_size * random.NextFloat())
  let too_close = snakes.Exists(fun s -> (s.head - head_pos).LengthSquared() < 150.f * 150.f)
  if too_close then
    spawn_random_snake controller
  else
    let colour = random_color random
    create_snake direction head_pos colour controller

/// Spawn a snake somewhere. Try to avoid other snakes.
let rec spawn_crossover_snake () =
  let choose total_energy =
    let target = random.NextFloat() * total_energy
    let mutable energy = 0.f
    let mutable i = -1
    while energy < target do
      i <- i + 1
      energy <- energy + (snd genepool.[i])
    fst genepool.[i]
  let total_energy = genepool |> Seq.sumBy snd
  let ws1 = choose total_energy
  let ws2 = choose total_energy
  let crossover i =
    if random.Next(2) = 0 then ws1.[i] else ws2.[i]
  let wschild = Array.init ws1.Length crossover
  if random.Next(5) = 0 then
    wschild.[random.Next(wschild.Length)] <- random.NextFloat()
  spawn_random_snake (AlphaSlither wschild)


/// Spawn food with standard time-to-live
let spawn_food (colour, pos, energy) =
  Food(pos, colour, energy, total_ticks + 1000L + (int64(random.Next(2000))))

/// Turn a dead snake's carcass into an appropriately-sized pile of food
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

// Handle snake death
let handle_death s =
  // Salvage AI parameters
  match s.controller with
  | AlphaSlither ws ->
      genepool.Enqueue (ws, s.energy)
      while genepool.Count > genepool_size do
        genepool.Dequeue () |> ignore
  | _ -> ()
  // Spawn food
  spawn_dead_snake_food s


// initialise
let initialize (game : game) =
  // Spawn some snakes
  Seq.init snake_total (fun i -> spawn_random_snake (AI.alpha_slither_random_weights ())) |> snakes.AddRange
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

// Handle movement
let handle_movement_speed s =
  s.speed <-
    match s.movement_mode with
      | NormalMove -> normal_speed
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
          fast_speed
        else
          normal_speed

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
let step_simulation (game : game) =
  clean_up_previous_frame ()

  total_ticks <- total_ticks + 1L

  let player = snakes.[0]

  // Moderate the food level
  do // Make some food
    let random_head = snakes.[random.Next(snakes.Count)].head
    let pos = random_head + Vector2(random.NextFloat() * 1000.f - 500.f, random.NextFloat() * 1000.f - 500.f)
    let f = spawn_food(random_color random, pos, 1.f + random.NextFloat() * 15.f)
    food.Add f
  if maximum_food < food.Count then
    let threshold = float32 maximum_food * 0.9f |> int
    while threshold < food.Count do
      let i = random.Next(food.Count)
      food.[i] <- food.[food.Count-1]
      food.RemoveAt(food.Count-1)

  // Add more snakes
  if snakes.Count < snake_total then
    snakes.Add (spawn_crossover_snake ())

  // Add food to collision grid, and remove old food
  for i = 0 to food.Count-1 do
    let f = food.[i]
    if f.time_of_expiry < total_ticks then
      dead_food.Add i
    else
      food_grid.AddPoint(f.pos, i)

  // Add snakes to collision grid
  for i = 0 to snakes.Count-1 do
    let s = snakes.[i]
    for pos in iter_snake s do
      snake_grid.AddSquare(pos, s.segment_size, Segment(pos, s.segment_size, i))

  // Update snake positions
  for i = 0 to snakes.Count-1 do
    let s = snakes.[i]
    // Run AI
    let action = AI.run_controller s.controller game s i
    change_direction s action.dir
    s.movement_mode <- action.mode
    // Update the snake's speed
    handle_movement_speed s
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
      handle_death s
      dead_snakes.Add i

// Update
let update (game, gameTime : GameTime) =
  // input
  let keystate = Keyboard.GetState()
  if keystate.IsKeyDown Keys.Space && (last_frame_keyboard.IsKeyUp Keys.Space) then
    run_fast <- not run_fast
  if keystate.IsKeyDown Keys.Enter && (last_frame_keyboard.IsKeyDown Keys.Enter) then
    let player = snakes.[0]
    player.controller <-
      match player.controller with Human -> AI.alpha_slither_random_weights () | _ -> Human
  last_frame_keyboard <- keystate
  // JSON
  if gameTime.ElapsedGameTime.Minutes > last_dump_time + 10 then
    json_dump_weights ()
  // simulation
  if run_fast then
    let watch = System.Diagnostics.Stopwatch()
    watch.Start()
    while watch.ElapsedMilliseconds < 12L do
      step_simulation game
    watch.Stop()
  else
    step_simulation game

// Draw the game
let draw (game : game, gameTime : GameTime) =
  let mouse = Mouse.GetState ()
   
  let screen_size = get_screen_size(game)
  let half_screen = screen_size / 2.f

  let player_pos = snakes.[0].head
  let camera = Vector2(half_screen.X - player_pos.X, half_screen.Y - player_pos.Y)

  // Draw a rectangle
  let draw_square (pos : Vector2, c : Color, size : float32) =
    let half_segment = size / 2.f
    let rect = RectangleF(pos.X - half_segment, pos.Y - half_segment, size, size)
    game.FillRectangle(rect, c)

  // Draw a snake
  let draw_snake (camera : Vector2, s : snake) =
    // Draw segments
    for pos in iter_snake s do
      draw_square (pos + camera, s.colour, s.segment_size)
  
  Color(40, 40, 40) |> game.Clear
  game.RenderBegin();

  // Draw a grid in the background
  let grid_colour = Color(0.1f, 0.1f, 0.1f)
  let tile_size = 70.f
  let grid_width = screen_size.X / tile_size |> int |> (+) 1
  let grid_height = screen_size.Y / tile_size |> int |> (+) 1
  let grid_offset_x = player_pos.X % tile_size
  let grid_offset_y = player_pos.Y % tile_size
  for x = 0 to grid_width do
    let x = (float32 x * tile_size) - grid_offset_x
    game.DrawLine(x, 0.f, x, screen_size.Y, grid_colour, 2.f)
  for y = 0 to grid_height do
    let y = (float32 y * tile_size) - grid_offset_y
    game.DrawLine(0.f, y, screen_size.X, y, grid_colour, 2.f)
      
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
    game.FillRectangle(w, Color.Black)

  // Draw mouse
  game.DrawCircle(float32 mouse.X, float32 mouse.Y, 9.f, 4, Color.White, 3.f)

  game.RenderEnd();


let run_game () =
  Engine.run_game (1280, 768, initialize, update, draw)
