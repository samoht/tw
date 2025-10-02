(** Clipping utilities *)

open Utility
open Style
open Css

type t = Clip_polygon of (float * float) list
type Utility.base += Clipping of t

let wrap x = Clipping x
let unwrap = function Clipping x -> Some x | _ -> None

let clip_polygon' points =
  let coords =
    points
    |> List.map (fun (x, y) -> Printf.sprintf "%g%% %g%%" x y)
    |> String.concat ", "
  in
  let func = "polygon(" ^ coords ^ ")" in
  let class_name = "clip-[" ^ func ^ "]" in
  style class_name [ clip func ]

let clip_polygon points = base (Clipping (Clip_polygon points))
let to_style = function Clip_polygon points -> clip_polygon' points
let suborder = function Clip_polygon _ -> 0
let of_string _ = Error (`Msg "Not a clipping utility")
let priority = 99
let handler = { Utility.of_string; priority; suborder; to_style }
let () = Utility.register ~wrap ~unwrap handler
