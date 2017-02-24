module GameTypes

open Engine

open C5

type movement_mode = NormalMove | FastMove

type System.Random with
  member x.NextFloat() = x.NextDouble() |> float32

type CircularQueue<'t> with
  member x.PeekLast = x.[x.Count-1]
  member x.FromBack i = x.[x.Count - (i+1)]

[<StructAttribute>]
type Action =
  val dir : Vector2
  val mode : movement_mode
  new(dir, mode) = 
    { dir = dir
      mode = mode }

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
    mutable ai : game -> snake -> int -> Action
  }

let create_snake direction head colour ai =
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
    ai = ai
  }

[<StructAttribute>]
type Segment =
  val pos : Vector2
  val size : float32
  val id : int
  new(pos, size, id) = 
    { pos = pos
      size = size
      id = id }

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
