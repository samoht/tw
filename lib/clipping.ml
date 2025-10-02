(** Clipping utilities *)

module Handler = struct
  open Style
  open Css

  type t = Clip_polygon of (float * float) list
  type Utility.base += Self of t

  let priority = 99

  let clip_polygon' points =
    let coords =
      points
      |> List.map (fun (x, y) -> Printf.sprintf "%g%% %g%%" x y)
      |> String.concat ", "
    in
    let func = "polygon(" ^ coords ^ ")" in
    let class_name = "clip-[" ^ func ^ "]" in
    style class_name [ clip func ]

  let to_style = function Clip_polygon points -> clip_polygon' points
  let suborder = function Clip_polygon _ -> 0
  let of_string _ = Error (`Msg "Not a clipping utility")
end

open Handler

let () = Utility.register (module Handler)
let utility x = Utility.base (Self x)
let clip_polygon points = utility (Clip_polygon points)
