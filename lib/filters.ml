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

open Style
open Css
module Parse = Parse

(** {1 Utility Type} *)

type utility =
  | Blur_none
  | Blur_xs
  | Blur_sm
  | Blur
  | Blur_md
  | Blur_lg
  | Blur_xl
  | Blur_2xl
  | Blur_3xl
  | Brightness of int
  | Contrast of int
  | Grayscale of int
  | Saturate of int
  | Sepia of int
  | Invert of int
  | Hue_rotate of int
  | Backdrop_blur_none
  | Backdrop_blur_xs
  | Backdrop_blur_sm
  | Backdrop_blur
  | Backdrop_blur_md
  | Backdrop_blur_lg
  | Backdrop_blur_xl
  | Backdrop_blur_2xl
  | Backdrop_blur_3xl
  | Backdrop_brightness of int
  | Backdrop_contrast of int
  | Backdrop_opacity of int
  | Backdrop_saturate of int
  | Backdrop_grayscale of int
  | Backdrop_invert of int
  | Backdrop_sepia of int
  | Backdrop_hue_rotate of int

(** {1 Filter Utilities} *)

let blur_internal = function
  | `None -> style "blur-none" [ filter (Blur (Px 0.)) ]
  | `Xs -> style "blur-xs" [ filter (Blur (Px 2.)) ]
  | `Sm -> style "blur-sm" [ filter (Blur (Px 4.)) ]
  | `Md -> style "blur" [ filter (Blur (Px 8.)) ]
  | `Lg -> style "blur-lg" [ filter (Blur (Px 16.)) ]
  | `Xl -> style "blur-xl" [ filter (Blur (Px 24.)) ]
  | `Xl_2 -> style "blur-2xl" [ filter (Blur (Px 40.)) ]
  | `Xl_3 -> style "blur-3xl" [ filter (Blur (Px 64.)) ]
  | `Full -> style "blur-full" [ filter (Blur (Px 9999.)) ]

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
  let value : Css.number = Num (float_of_int n /. 100.0) in
  style class_name [ filter (Brightness value) ]

let contrast n =
  let class_name = "contrast-" ^ string_of_int n in
  let value : Css.number = Num (float_of_int n /. 100.0) in
  style class_name [ filter (Contrast value) ]

let grayscale n =
  let class_name = if n = 0 then "grayscale-0" else "grayscale" in
  let value : Css.number = Num (float_of_int n /. 100.0) in
  style class_name [ filter (Grayscale value) ]

let saturate n =
  let class_name = "saturate-" ^ string_of_int n in
  let value : Css.number = Num (float_of_int n /. 100.0) in
  style class_name [ filter (Saturate value) ]

let sepia n =
  let class_name = if n = 0 then "sepia-0" else "sepia" in
  let value : Css.number = Num (float_of_int n /. 100.0) in
  style class_name [ filter (Sepia value) ]

let invert n =
  let class_name = if n = 0 then "invert-0" else "invert" in
  let value : Css.number = Num (float_of_int n /. 100.0) in
  style class_name [ filter (Invert value) ]

let hue_rotate n =
  let class_name = "hue-rotate-" ^ string_of_int n in
  style class_name [ filter (Hue_rotate (Deg (float_of_int n))) ]

(** {1 Backdrop Filter Utilities} *)

let backdrop_blur_internal = function
  | `None -> style "backdrop-blur-none" [ backdrop_filter (Blur (Px 0.)) ]
  | `Xs -> style "backdrop-blur-xs" [ backdrop_filter (Blur (Px 2.)) ]
  | `Sm -> style "backdrop-blur-sm" [ backdrop_filter (Blur (Px 4.)) ]
  | `Md -> style "backdrop-blur" [ backdrop_filter (Blur (Px 8.)) ]
  | `Lg -> style "backdrop-blur-lg" [ backdrop_filter (Blur (Px 12.)) ]
  | `Xl -> style "backdrop-blur-xl" [ backdrop_filter (Blur (Px 24.)) ]
  | `Xl_2 -> style "backdrop-blur-2xl" [ backdrop_filter (Blur (Px 40.)) ]
  | `Xl_3 -> style "backdrop-blur-3xl" [ backdrop_filter (Blur (Px 64.)) ]
  | `Full -> style "backdrop-blur-full" [ backdrop_filter (Blur (Px 9999.)) ]

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
  let class_name =
    String.concat "" [ "backdrop-brightness-"; string_of_int n ]
  in
  style class_name
    [ backdrop_filter (Brightness (Num (float_of_int n /. 100.0))) ]

let backdrop_contrast n =
  let class_name = String.concat "" [ "backdrop-contrast-"; string_of_int n ] in
  style class_name
    [ backdrop_filter (Contrast (Num (float_of_int n /. 100.0))) ]

let backdrop_opacity n =
  let class_name = String.concat "" [ "backdrop-opacity-"; string_of_int n ] in
  style class_name [ backdrop_filter (Opacity (Num (float_of_int n /. 100.0))) ]

let backdrop_saturate n =
  let class_name = String.concat "" [ "backdrop-saturate-"; string_of_int n ] in
  style class_name
    [ backdrop_filter (Saturate (Num (float_of_int n /. 100.0))) ]

let backdrop_grayscale_default =
  style "backdrop-grayscale" [ backdrop_filter (Grayscale (Num 1.0)) ]

let backdrop_grayscale n =
  let class_name =
    String.concat "" [ "backdrop-grayscale-"; string_of_int n ]
  in
  style class_name
    [ backdrop_filter (Grayscale (Num (float_of_int n /. 100.0))) ]

let backdrop_invert_default =
  style "backdrop-invert" [ backdrop_filter (Invert (Num 1.0)) ]

let backdrop_invert n =
  let class_name = String.concat "" [ "backdrop-invert-"; string_of_int n ] in
  style class_name [ backdrop_filter (Invert (Num (float_of_int n /. 100.0))) ]

let backdrop_sepia_default =
  style "backdrop-sepia" [ backdrop_filter (Sepia (Num 1.0)) ]

let backdrop_sepia n =
  let class_name = String.concat "" [ "backdrop-sepia-"; string_of_int n ] in
  style class_name [ backdrop_filter (Sepia (Num (float_of_int n /. 100.0))) ]

let backdrop_hue_rotate n =
  let class_name =
    String.concat "" [ "backdrop-hue-rotate-"; string_of_int n ]
  in
  style class_name [ backdrop_filter (Hue_rotate (Deg (float_of_int n))) ]

(** {1 Conversion Functions} *)

let to_style = function
  | Blur_none -> blur_none
  | Blur_xs -> blur_xs
  | Blur_sm -> blur_sm
  | Blur -> blur
  | Blur_md -> blur_md
  | Blur_lg -> blur_lg
  | Blur_xl -> blur_xl
  | Blur_2xl -> blur_2xl
  | Blur_3xl -> blur_3xl
  | Brightness n -> brightness n
  | Contrast n -> contrast n
  | Grayscale n -> grayscale n
  | Saturate n -> saturate n
  | Sepia n -> sepia n
  | Invert n -> invert n
  | Hue_rotate n -> hue_rotate n
  | Backdrop_blur_none -> backdrop_blur_none
  | Backdrop_blur_xs -> backdrop_blur_xs
  | Backdrop_blur_sm -> backdrop_blur_sm
  | Backdrop_blur -> backdrop_blur
  | Backdrop_blur_md -> backdrop_blur_md
  | Backdrop_blur_lg -> backdrop_blur_lg
  | Backdrop_blur_xl -> backdrop_blur_xl
  | Backdrop_blur_2xl -> backdrop_blur_2xl
  | Backdrop_blur_3xl -> backdrop_blur_3xl
  | Backdrop_brightness n -> backdrop_brightness n
  | Backdrop_contrast n -> backdrop_contrast n
  | Backdrop_opacity n -> backdrop_opacity n
  | Backdrop_saturate n -> backdrop_saturate n
  | Backdrop_grayscale n -> backdrop_grayscale n
  | Backdrop_invert n -> backdrop_invert n
  | Backdrop_sepia n -> backdrop_sepia n
  | Backdrop_hue_rotate n -> backdrop_hue_rotate n

(** {1 Parsing Functions} *)

let ( >|= ) = Parse.( >|= )

let of_string = function
  | [ "blur"; "none" ] -> Ok Blur_none
  | [ "blur"; "xs" ] -> Ok Blur_xs
  | [ "blur"; "sm" ] -> Ok Blur_sm
  | [ "blur" ] -> Ok Blur
  | [ "blur"; "md" ] -> Ok Blur_md
  | [ "blur"; "lg" ] -> Ok Blur_lg
  | [ "blur"; "xl" ] -> Ok Blur_xl
  | [ "blur"; "2xl" ] -> Ok Blur_2xl
  | [ "blur"; "3xl" ] -> Ok Blur_3xl
  | [ "brightness"; n ] -> Parse.int_pos ~name:"brightness" n >|= fun x -> Brightness x
  | [ "contrast"; n ] -> Parse.int_pos ~name:"contrast" n >|= fun x -> Contrast x
  | [ "grayscale"; n ] -> Parse.int_pos ~name:"grayscale" n >|= fun x -> Grayscale x
  | [ "grayscale" ] -> Ok (Grayscale 100)
  | [ "saturate"; n ] -> Parse.int_pos ~name:"saturate" n >|= fun x -> Saturate x
  | [ "sepia"; n ] -> Parse.int_pos ~name:"sepia" n >|= fun x -> Sepia x
  | [ "sepia" ] -> Ok (Sepia 100)
  | [ "invert"; n ] -> Parse.int_pos ~name:"invert" n >|= fun x -> Invert x
  | [ "invert" ] -> Ok (Invert 100)
  | [ "hue"; "rotate"; n ] -> Parse.int_any n >|= fun x -> Hue_rotate x
  | [ "backdrop"; "blur"; "none" ] -> Ok Backdrop_blur_none
  | [ "backdrop"; "blur"; "xs" ] -> Ok Backdrop_blur_xs
  | [ "backdrop"; "blur"; "sm" ] -> Ok Backdrop_blur_sm
  | [ "backdrop"; "blur" ] -> Ok Backdrop_blur
  | [ "backdrop"; "blur"; "md" ] -> Ok Backdrop_blur_md
  | [ "backdrop"; "blur"; "lg" ] -> Ok Backdrop_blur_lg
  | [ "backdrop"; "blur"; "xl" ] -> Ok Backdrop_blur_xl
  | [ "backdrop"; "blur"; "2xl" ] -> Ok Backdrop_blur_2xl
  | [ "backdrop"; "blur"; "3xl" ] -> Ok Backdrop_blur_3xl
  | [ "backdrop"; "brightness"; n ] ->
      Parse.int_pos ~name:"backdrop-brightness" n >|= fun x -> Backdrop_brightness x
  | [ "backdrop"; "contrast"; n ] ->
      Parse.int_pos ~name:"backdrop-contrast" n >|= fun x -> Backdrop_contrast x
  | [ "backdrop"; "opacity"; n ] ->
      Parse.int_pos ~name:"backdrop-opacity" n >|= fun x -> Backdrop_opacity x
  | [ "backdrop"; "saturate"; n ] ->
      Parse.int_pos ~name:"backdrop-saturate" n >|= fun x -> Backdrop_saturate x
  | [ "backdrop"; "grayscale"; n ] ->
      Parse.int_pos ~name:"backdrop-grayscale" n >|= fun x -> Backdrop_grayscale x
  | [ "backdrop"; "grayscale" ] -> Ok (Backdrop_grayscale 100)
  | [ "backdrop"; "invert"; n ] ->
      Parse.int_pos ~name:"backdrop-invert" n >|= fun x -> Backdrop_invert x
  | [ "backdrop"; "invert" ] -> Ok (Backdrop_invert 100)
  | [ "backdrop"; "sepia"; n ] ->
      Parse.int_pos ~name:"backdrop-sepia" n >|= fun x -> Backdrop_sepia x
  | [ "backdrop"; "sepia" ] -> Ok (Backdrop_sepia 100)
  | [ "backdrop"; "hue"; "rotate"; n ] ->
      Parse.int_any n >|= fun x -> Backdrop_hue_rotate x
  | _ -> Error (`Msg "Not a filter utility")

(** {1 Suborder Function} *)

let suborder = function
  | Blur_none -> 0
  | Blur_xs -> 1
  | Blur_sm -> 2
  | Blur -> 3
  | Blur_md -> 4
  | Blur_lg -> 5
  | Blur_xl -> 6
  | Blur_2xl -> 7
  | Blur_3xl -> 8
  | Brightness n -> 100 + n
  | Contrast n -> 10000 + n
  | Grayscale n -> 20000 + n
  | Saturate n -> 30000 + n
  | Sepia n -> 40000 + n
  | Invert n -> 50000 + n
  | Hue_rotate n -> 60000 + n
  | Backdrop_blur_none -> 100000
  | Backdrop_blur_xs -> 100001
  | Backdrop_blur_sm -> 100002
  | Backdrop_blur -> 100003
  | Backdrop_blur_md -> 100004
  | Backdrop_blur_lg -> 100005
  | Backdrop_blur_xl -> 100006
  | Backdrop_blur_2xl -> 100007
  | Backdrop_blur_3xl -> 100008
  | Backdrop_brightness n -> 110000 + n
  | Backdrop_contrast n -> 120000 + n
  | Backdrop_opacity n -> 130000 + n
  | Backdrop_saturate n -> 140000 + n
  | Backdrop_grayscale n -> 150000 + n
  | Backdrop_invert n -> 160000 + n
  | Backdrop_sepia n -> 170000 + n
  | Backdrop_hue_rotate n -> 180000 + n
