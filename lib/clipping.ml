(** Clipping utilities *)

open Core

let clip_polygon points =
  let coords =
    points
    |> List.map (fun (x, y) ->
           String.concat "" [ Pp.float x; "% "; Pp.float y; "%" ])
    |> Pp.sep ", "
  in
  let func = String.concat "" [ "polygon("; coords; ")" ] in
  let class_name = String.concat "" [ "clip-["; func; "]" ] in
  style class_name [ Css.clip func ]
