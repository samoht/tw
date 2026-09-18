(** Clipping utilities *)

module Css = Cascade.Css

module Handler = struct
  open Style

  (* Capture the project Pp before [open Css] shadows it with Css.Pp. *)
  let format_float = Pp.float

  open Css

  type t = Clip_polygon of (float * float) list

  let name = "clipping"
  let priority _ = 28

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
    style
      [
        clip_path
          (Css.Clip_path_polygon
             {
               fill_rule = None;
               points = points_to_lengths points;
               spaced = false;
             });
      ]

  let to_class = function
    | Clip_polygon points ->
        let coords =
          points
          |> List.map (fun (x, y) ->
              format_float x ^ "% " ^ format_float y ^ "%")
          |> String.concat ", "
        in
        "clip-[polygon(" ^ coords ^ ")]"

  let to_style _theme = function Clip_polygon points -> clip_polygon' points
  let suborder = function Clip_polygon _ -> 0
  let of_class _theme _class_name = Error (`Msg "Not a clipping utility")
  let examples = [ Clip_polygon [] ]
end

open Handler
module Utility_factory = Utility.Make (Handler)

let utility = Utility_factory.v
let clip_polygon points = utility (Clip_polygon points)
