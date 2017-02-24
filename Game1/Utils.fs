module Utils

open System

let degrees_to_radians d = d * (float32 Math.PI / 180.f)
let radians_to_degrees r = r * (180.f / float32 Math.PI)
