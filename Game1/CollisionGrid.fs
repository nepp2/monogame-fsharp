module CollisionGrid

open Engine
open System.Collections.Generic

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