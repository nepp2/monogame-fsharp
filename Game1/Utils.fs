module Utils

open System
open Engine

let degrees_to_radians d = d * (float32 Math.PI / 180.f)
let radians_to_degrees r = r * (180.f / float32 Math.PI)

let square_to_rect (pos : Vector2) (size : float32) =
  let half_size = size/2.f
  RectangleF(pos.X - half_size, pos.Y- half_size, size, size)

let rect_closest_vector_from_to (a : RectangleF) (b : RectangleF) =
  let vec = b.Center - a.Center
  let sign_x = vec.X / (abs vec.X)
  let sign_y = vec.Y / (abs vec.Y)
  if a.Bottom > b.Top && a.Top < b.Bottom then
    let size = min (a.Left - b.Right |> abs) (a.Right - b.Left |> abs)
    Vector2(size * sign_x, 0.f)
  elif a.Right > b.Left && a.Left < b.Right then
    let size = min (a.Top - b.Bottom |> abs) (a.Bottom - b.Top |> abs)
    Vector2(0.f, size * sign_y)
  else
    let size_x = min (a.Left - b.Right |> abs) (a.Right - b.Left |> abs)
    let size_y = min (a.Top - b.Bottom |> abs) (a.Bottom - b.Top |> abs)
    Vector2(size_x * sign_x, size_y * sign_y)
      

