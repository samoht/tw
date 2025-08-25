(** Filter utilities for visual effects like blur, brightness, and backdrop
    filters

    What's included:
    - `blur-*` and `backdrop-blur-*` presets.
    - Scalar filters: `brightness`, `contrast`, `grayscale`, `saturate`,
      `sepia`, `invert`, `hue-rotate` and their `backdrop-*` counterparts.

    What's not:
    - Filter composition chains beyond the provided helpers. Use `style` with
      `Css.filter` if you need complex pipelines.

    Parsing contract (`of_string`):
    - Accepts tokens like ["blur"; "sm"], ["brightness"; n],
      ["backdrop"; "hue"; "rotate"; n], etc. Unknown tokens yield `Error (`Msg
      "Not a filter utility")`. *)

open Core
open Css

(** {1 Filter Utilities} *)

let blur_internal = function
  | `None -> style "blur-none" [ filter "blur(0)" ]
  | `Xs -> style "blur-xs" [ filter "blur(2px)" ]
  | `Sm -> style "blur-sm" [ filter "blur(4px)" ]
  | `Md -> style "blur" [ filter "blur(8px)" ]
  | `Lg -> style "blur-lg" [ filter "blur(16px)" ]
  | `Xl -> style "blur-xl" [ filter "blur(24px)" ]
  | `Xl_2 -> style "blur-2xl" [ filter "blur(40px)" ]
  | `Xl_3 -> style "blur-3xl" [ filter "blur(64px)" ]
  | `Full -> style "blur-full" [ filter "blur(9999px)" ]

let blur_none = blur_internal `None
let blur_xs = blur_internal `Xs
let blur_sm = blur_internal `Sm
let blur = blur_internal `Md (* Default blur *)
let blur_md = blur_internal `Md
let blur_lg = blur_internal `Lg
let blur_xl = blur_internal `Xl
let blur_2xl = blur_internal `Xl_2
let blur_3xl = blur_internal `Xl_3

let brightness n =
  let class_name = "brightness-" ^ string_of_int n in
  let value = Pp.float (float_of_int n /. 100.0) in
  style class_name [ filter ("brightness(" ^ value ^ ")") ]

let contrast n =
  let class_name = "contrast-" ^ string_of_int n in
  let value = Pp.float (float_of_int n /. 100.0) in
  style class_name [ filter ("contrast(" ^ value ^ ")") ]

let grayscale n =
  let class_name = if n = 0 then "grayscale-0" else "grayscale" in
  let value = Pp.float (float_of_int n /. 100.0) in
  style class_name [ filter ("grayscale(" ^ value ^ ")") ]

let saturate n =
  let class_name = "saturate-" ^ string_of_int n in
  let value = Pp.float (float_of_int n /. 100.0) in
  style class_name [ filter ("saturate(" ^ value ^ ")") ]

let sepia n =
  let class_name = if n = 0 then "sepia-0" else "sepia" in
  let value = Pp.float (float_of_int n /. 100.0) in
  style class_name [ filter ("sepia(" ^ value ^ ")") ]

let invert n =
  let class_name = if n = 0 then "invert-0" else "invert" in
  let value = Pp.float (float_of_int n /. 100.0) in
  style class_name [ filter ("invert(" ^ value ^ ")") ]

let hue_rotate n =
  let class_name = "hue-rotate-" ^ string_of_int n in
  let value = string_of_int n ^ "deg" in
  style class_name [ filter ("hue-rotate(" ^ value ^ ")") ]

(** {1 Backdrop Filter Utilities} *)

let backdrop_blur_internal = function
  | `None -> style "backdrop-blur-none" [ backdrop_filter "blur(0)" ]
  | `Xs -> style "backdrop-blur-xs" [ backdrop_filter "blur(2px)" ]
  | `Sm -> style "backdrop-blur-sm" [ backdrop_filter "blur(4px)" ]
  | `Md -> style "backdrop-blur" [ backdrop_filter "blur(8px)" ]
  | `Lg -> style "backdrop-blur-lg" [ backdrop_filter "blur(12px)" ]
  | `Xl -> style "backdrop-blur-xl" [ backdrop_filter "blur(24px)" ]
  | `Xl_2 -> style "backdrop-blur-2xl" [ backdrop_filter "blur(40px)" ]
  | `Xl_3 -> style "backdrop-blur-3xl" [ backdrop_filter "blur(64px)" ]
  | `Full -> style "backdrop-blur-full" [ backdrop_filter "blur(9999px)" ]

let backdrop_blur_none = backdrop_blur_internal `None
let backdrop_blur_xs = backdrop_blur_internal `Xs
let backdrop_blur_sm = backdrop_blur_internal `Sm
let backdrop_blur = backdrop_blur_internal `Md (* Default backdrop blur *)
let backdrop_blur_md = backdrop_blur_internal `Md
let backdrop_blur_lg = backdrop_blur_internal `Lg
let backdrop_blur_xl = backdrop_blur_internal `Xl
let backdrop_blur_2xl = backdrop_blur_internal `Xl_2
let backdrop_blur_3xl = backdrop_blur_internal `Xl_3

let backdrop_brightness n =
  let class_name = Pp.str [ "backdrop-brightness-"; string_of_int n ] in
  style class_name
    [
      backdrop_filter
        (Pp.str [ "brightness("; Pp.float (float_of_int n /. 100.); ")" ]);
    ]

let backdrop_contrast n =
  let class_name = Pp.str [ "backdrop-contrast-"; string_of_int n ] in
  style class_name
    [
      backdrop_filter
        (Pp.str [ "contrast("; Pp.float (float_of_int n /. 100.); ")" ]);
    ]

let backdrop_opacity n =
  let class_name = Pp.str [ "backdrop-opacity-"; string_of_int n ] in
  style class_name
    [
      backdrop_filter
        (Pp.str [ "opacity("; Pp.float (float_of_int n /. 100.); ")" ]);
    ]

let backdrop_saturate n =
  let class_name = Pp.str [ "backdrop-saturate-"; string_of_int n ] in
  style class_name
    [
      backdrop_filter
        (Pp.str [ "saturate("; Pp.float (float_of_int n /. 100.); ")" ]);
    ]

let backdrop_grayscale n =
  let class_name = Pp.str [ "backdrop-grayscale-"; string_of_int n ] in
  style class_name
    [
      backdrop_filter
        (Pp.str [ "grayscale("; Pp.float (float_of_int n /. 100.); ")" ]);
    ]

let backdrop_invert n =
  let class_name = Pp.str [ "backdrop-invert-"; string_of_int n ] in
  style class_name
    [
      backdrop_filter
        (Pp.str [ "invert("; Pp.float (float_of_int n /. 100.); ")" ]);
    ]

let backdrop_sepia n =
  let class_name = Pp.str [ "backdrop-sepia-"; string_of_int n ] in
  style class_name
    [
      backdrop_filter
        (Pp.str [ "sepia("; Pp.float (float_of_int n /. 100.); ")" ]);
    ]

let backdrop_hue_rotate n =
  let class_name = Pp.str [ "backdrop-hue-rotate-"; string_of_int n ] in
  style class_name
    [ backdrop_filter (Pp.str [ "hue-rotate("; string_of_int n; "deg)" ]) ]

(** {1 Parsing Functions} *)

let int_of_string_positive name s =
  match int_of_string_opt s with
  | None -> Error (`Msg ("Invalid " ^ name ^ " value: " ^ s))
  | Some n when n >= 0 -> Ok n
  | Some _ -> Error (`Msg (name ^ " must be non-negative: " ^ s))

let int_of_string_with_sign s =
  match int_of_string_opt s with
  | None -> Error (`Msg ("Invalid number: " ^ s))
  | Some n -> Ok n

let ( >|= ) r f = Result.map f r

let of_string = function
  | [ "blur"; "none" ] -> Ok blur_none
  | [ "blur"; "xs" ] -> Ok blur_xs
  | [ "blur"; "sm" ] -> Ok blur_sm
  | [ "blur" ] -> Ok blur
  | [ "blur"; "md" ] -> Ok blur_md
  | [ "blur"; "lg" ] -> Ok blur_lg
  | [ "blur"; "xl" ] -> Ok blur_xl
  | [ "blur"; "2xl" ] -> Ok blur_2xl
  | [ "blur"; "3xl" ] -> Ok blur_3xl
  | [ "brightness"; n ] -> int_of_string_positive "brightness" n >|= brightness
  | [ "contrast"; n ] -> int_of_string_positive "contrast" n >|= contrast
  | [ "grayscale"; n ] -> int_of_string_positive "grayscale" n >|= grayscale
  | [ "grayscale" ] -> Ok (grayscale 100)
  | [ "saturate"; n ] -> int_of_string_positive "saturate" n >|= saturate
  | [ "sepia"; n ] -> int_of_string_positive "sepia" n >|= sepia
  | [ "sepia" ] -> Ok (sepia 100)
  | [ "invert"; n ] -> int_of_string_positive "invert" n >|= invert
  | [ "invert" ] -> Ok (invert 100)
  | [ "hue"; "rotate"; n ] -> int_of_string_with_sign n >|= hue_rotate
  | [ "backdrop"; "blur"; "none" ] -> Ok backdrop_blur_none
  | [ "backdrop"; "blur"; "xs" ] -> Ok backdrop_blur_xs
  | [ "backdrop"; "blur"; "sm" ] -> Ok backdrop_blur_sm
  | [ "backdrop"; "blur" ] -> Ok backdrop_blur
  | [ "backdrop"; "blur"; "md" ] -> Ok backdrop_blur_md
  | [ "backdrop"; "blur"; "lg" ] -> Ok backdrop_blur_lg
  | [ "backdrop"; "blur"; "xl" ] -> Ok backdrop_blur_xl
  | [ "backdrop"; "blur"; "2xl" ] -> Ok backdrop_blur_2xl
  | [ "backdrop"; "blur"; "3xl" ] -> Ok backdrop_blur_3xl
  | [ "backdrop"; "brightness"; n ] ->
      int_of_string_positive "backdrop-brightness" n >|= backdrop_brightness
  | [ "backdrop"; "contrast"; n ] ->
      int_of_string_positive "backdrop-contrast" n >|= backdrop_contrast
  | [ "backdrop"; "opacity"; n ] ->
      int_of_string_positive "backdrop-opacity" n >|= backdrop_opacity
  | [ "backdrop"; "saturate"; n ] ->
      int_of_string_positive "backdrop-saturate" n >|= backdrop_saturate
  | [ "backdrop"; "grayscale"; n ] ->
      int_of_string_positive "backdrop-grayscale" n >|= backdrop_grayscale
  | [ "backdrop"; "grayscale" ] -> Ok (backdrop_grayscale 100)
  | [ "backdrop"; "invert"; n ] ->
      int_of_string_positive "backdrop-invert" n >|= backdrop_invert
  | [ "backdrop"; "invert" ] -> Ok (backdrop_invert 100)
  | [ "backdrop"; "sepia"; n ] ->
      int_of_string_positive "backdrop-sepia" n >|= backdrop_sepia
  | [ "backdrop"; "sepia" ] -> Ok (backdrop_sepia 100)
  | [ "backdrop"; "hue"; "rotate"; n ] ->
      int_of_string_with_sign n >|= backdrop_hue_rotate
  | _ -> Error (`Msg "Not a filter utility")
