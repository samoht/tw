(** Clipping utilities *)

open Core

let clip_polygon points =
  let coords =
    points
    |> List.map (fun (x, y) -> Pp.str [ Pp.float x; "% "; Pp.float y; "%" ])
    |> Pp.sep ", "
  in
  let func = Pp.str [ "polygon("; coords; ")" ] in
  let class_name = Pp.str [ "clip-["; func; "]" ] in
  style class_name [ Css.clip func ]
