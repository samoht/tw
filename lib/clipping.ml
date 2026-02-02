(** Clipping utilities *)

module Handler = struct
  open Style
  open Css

  type t = Clip_polygon of (float * float) list
  type Utility.base += Self of t

  let name = "clipping"
  let priority = 26

  (** Convert float percentage pairs to typed length pairs for clip-path polygon
  *)
  let points_to_lengths points : (Css.length * Css.length) list =
    List.map
      (fun (x, y) ->
        let x' : Css.length = Pct x in
        let y' : Css.length = Pct y in
        (x', y'))
      points

  let clip_polygon' points =
    style [ clip_path (Css.Clip_path_polygon (points_to_lengths points)) ]

  (** Format a float without trailing zeros for class name generation *)
  let format_float f =
    let s = string_of_float f in
    (* Remove trailing dot if present (e.g., "5." -> "5") *)
    if String.length s > 0 && s.[String.length s - 1] = '.' then
      String.sub s 0 (String.length s - 1)
    else s

  let to_class = function
    | Clip_polygon points ->
        let coords =
          points
          |> List.map (fun (x, y) ->
              format_float x ^ "% " ^ format_float y ^ "%")
          |> String.concat ", "
        in
        "clip-[polygon(" ^ coords ^ ")]"

  let to_style = function Clip_polygon points -> clip_polygon' points
  let suborder = function Clip_polygon _ -> 0
  let of_class _class_name = Error (`Msg "Not a clipping utility")
end

open Handler

let () = Utility.register (module Handler)
let utility x = Utility.base (Self x)
let clip_polygon points = utility (Clip_polygon points)
