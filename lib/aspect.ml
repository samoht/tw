(** Aspect ratio utilities *)

open Css
open Core

let aspect_auto = style "aspect-auto" [ Css.aspect_ratio Auto ]
let aspect_square = style "aspect-square" [ Css.aspect_ratio (Ratio (1, 1)) ]
let aspect_video = style "aspect-video" [ Css.aspect_ratio (Ratio (16, 9)) ]

let aspect_ratio w h =
  let class_name =
    Pp.str [ "aspect-["; string_of_int w; "/"; string_of_int h; "]" ]
  in
  style class_name [ Css.aspect_ratio (Ratio (w, h)) ]
