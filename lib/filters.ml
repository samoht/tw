(** Filter utilities for visual effects like blur, brightness, and backdrop
    filters. *)

module Handler = struct
  open Style
  open Css

  type t =
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

  type Utility.base += Self of t

  let name = "filters"
  let priority = 25

  let blur_internal = function
    | `None -> style [ filter (Blur (Px 0.)) ]
    | `Xs -> style [ filter (Blur (Px 2.)) ]
    | `Sm -> style [ filter (Blur (Px 4.)) ]
    | `Md -> style [ filter (Blur (Px 8.)) ]
    | `Lg -> style [ filter (Blur (Px 16.)) ]
    | `Xl -> style [ filter (Blur (Px 24.)) ]
    | `Xl_2 -> style [ filter (Blur (Px 40.)) ]
    | `Xl_3 -> style [ filter (Blur (Px 64.)) ]
    | `Full -> style [ filter (Blur (Px 9999.)) ]

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
    let value : Css.number = Num (float_of_int n /. 100.0) in
    style [ filter (Brightness value) ]

  let contrast n =
    let value : Css.number = Num (float_of_int n /. 100.0) in
    style [ filter (Contrast value) ]

  let grayscale n =
    let value : Css.number = Num (float_of_int n /. 100.0) in
    style [ filter (Grayscale value) ]

  let saturate n =
    let value : Css.number = Num (float_of_int n /. 100.0) in
    style [ filter (Saturate value) ]

  let sepia n =
    let value : Css.number = Num (float_of_int n /. 100.0) in
    style [ filter (Sepia value) ]

  let invert n =
    let value : Css.number = Num (float_of_int n /. 100.0) in
    style [ filter (Invert value) ]

  let hue_rotate n = style [ filter (Hue_rotate (Deg (float_of_int n))) ]

  let backdrop_blur_internal = function
    | `None -> style [ backdrop_filter (Blur (Px 0.)) ]
    | `Xs -> style [ backdrop_filter (Blur (Px 2.)) ]
    | `Sm -> style [ backdrop_filter (Blur (Px 4.)) ]
    | `Md -> style [ backdrop_filter (Blur (Px 8.)) ]
    | `Lg -> style [ backdrop_filter (Blur (Px 12.)) ]
    | `Xl -> style [ backdrop_filter (Blur (Px 24.)) ]
    | `Xl_2 -> style [ backdrop_filter (Blur (Px 40.)) ]
    | `Xl_3 -> style [ backdrop_filter (Blur (Px 64.)) ]
    | `Full -> style [ backdrop_filter (Blur (Px 9999.)) ]

  let backdrop_blur_none = backdrop_blur_internal `None
  let backdrop_blur_xs = backdrop_blur_internal `Xs
  let backdrop_blur_sm = backdrop_blur_internal `Sm
  let backdrop_blur = backdrop_blur_internal `Md
  let backdrop_blur_md = backdrop_blur_internal `Md
  let backdrop_blur_lg = backdrop_blur_internal `Lg
  let backdrop_blur_xl = backdrop_blur_internal `Xl
  let backdrop_blur_2xl = backdrop_blur_internal `Xl_2
  let backdrop_blur_3xl = backdrop_blur_internal `Xl_3

  let backdrop_brightness n =
    style [ backdrop_filter (Brightness (Num (float_of_int n /. 100.0))) ]

  let backdrop_contrast n =
    style [ backdrop_filter (Contrast (Num (float_of_int n /. 100.0))) ]

  let backdrop_opacity n =
    style [ backdrop_filter (Opacity (Num (float_of_int n /. 100.0))) ]

  let backdrop_saturate n =
    style [ backdrop_filter (Saturate (Num (float_of_int n /. 100.0))) ]

  let backdrop_grayscale_default =
    style [ backdrop_filter (Grayscale (Num 1.0)) ]

  let backdrop_grayscale n =
    style [ backdrop_filter (Grayscale (Num (float_of_int n /. 100.0))) ]

  let backdrop_invert_default = style [ backdrop_filter (Invert (Num 1.0)) ]

  let backdrop_invert n =
    style [ backdrop_filter (Invert (Num (float_of_int n /. 100.0))) ]

  let backdrop_sepia_default = style [ backdrop_filter (Sepia (Num 1.0)) ]

  let backdrop_sepia n =
    style [ backdrop_filter (Sepia (Num (float_of_int n /. 100.0))) ]

  let backdrop_hue_rotate n =
    style [ backdrop_filter (Hue_rotate (Deg (float_of_int n))) ]

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
    | Backdrop_grayscale n ->
        if n = 100 then backdrop_grayscale_default else backdrop_grayscale n
    | Backdrop_invert n ->
        if n = 100 then backdrop_invert_default else backdrop_invert n
    | Backdrop_sepia n ->
        if n = 100 then backdrop_sepia_default else backdrop_sepia n
    | Backdrop_hue_rotate n -> backdrop_hue_rotate n

  let ( >|= ) = Parse.( >|= )
  let err_not_utility = Error (`Msg "Not a filter utility")

  let suborder = function
    (* Alphabetical order by class name *)
    | Blur -> 0 (* blur *)
    | Blur_2xl -> 1 (* blur-2xl *)
    | Blur_3xl -> 2 (* blur-3xl *)
    | Blur_lg -> 3 (* blur-lg *)
    | Blur_md -> 4 (* blur-md *)
    | Blur_none -> 5 (* blur-none *)
    | Blur_sm -> 6 (* blur-sm *)
    | Blur_xl -> 7 (* blur-xl *)
    | Blur_xs -> 8 (* blur-xs *)
    | Brightness n -> 100 + n
    | Contrast n -> 10000 + n
    | Grayscale n -> 20000 + n
    | Saturate n -> 30000 + n
    | Sepia n -> 40000 + n
    | Invert n -> 50000 + n
    | Hue_rotate n -> 60000 + n
    (* Alphabetical order by class name *)
    | Backdrop_blur -> 100000 (* backdrop-blur *)
    | Backdrop_blur_2xl -> 100001 (* backdrop-blur-2xl *)
    | Backdrop_blur_3xl -> 100002 (* backdrop-blur-3xl *)
    | Backdrop_blur_lg -> 100003 (* backdrop-blur-lg *)
    | Backdrop_blur_md -> 100004 (* backdrop-blur-md *)
    | Backdrop_blur_none -> 100005 (* backdrop-blur-none *)
    | Backdrop_blur_sm -> 100006 (* backdrop-blur-sm *)
    | Backdrop_blur_xl -> 100007 (* backdrop-blur-xl *)
    | Backdrop_blur_xs -> 100008 (* backdrop-blur-xs *)
    | Backdrop_brightness n -> 110000 + n
    | Backdrop_contrast n -> 120000 + n
    | Backdrop_opacity n -> 130000 + n
    | Backdrop_saturate n -> 140000 + n
    | Backdrop_grayscale n -> 150000 + n
    | Backdrop_invert n -> 160000 + n
    | Backdrop_sepia n -> 170000 + n
    | Backdrop_hue_rotate n -> 180000 + n

  let of_class class_name =
    let parts = String.split_on_char '-' class_name in
    match parts with
    | [ "blur"; "none" ] -> Ok Blur_none
    | [ "blur"; "xs" ] -> Ok Blur_xs
    | [ "blur"; "sm" ] -> Ok Blur_sm
    | [ "blur" ] -> Ok Blur
    | [ "blur"; "md" ] -> Ok Blur_md
    | [ "blur"; "lg" ] -> Ok Blur_lg
    | [ "blur"; "xl" ] -> Ok Blur_xl
    | [ "blur"; "2xl" ] -> Ok Blur_2xl
    | [ "blur"; "3xl" ] -> Ok Blur_3xl
    | [ "brightness"; n ] ->
        Parse.int_pos ~name:"brightness" n >|= fun x -> Brightness x
    | [ "contrast"; n ] ->
        Parse.int_pos ~name:"contrast" n >|= fun x -> Contrast x
    | [ "grayscale"; n ] ->
        Parse.int_pos ~name:"grayscale" n >|= fun x -> Grayscale x
    | [ "grayscale" ] -> Ok (Grayscale 100)
    | [ "saturate"; n ] ->
        Parse.int_pos ~name:"saturate" n >|= fun x -> Saturate x
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
        Parse.int_pos ~name:"backdrop-brightness" n >|= fun x ->
        Backdrop_brightness x
    | [ "backdrop"; "contrast"; n ] ->
        Parse.int_pos ~name:"backdrop-contrast" n >|= fun x ->
        Backdrop_contrast x
    | [ "backdrop"; "opacity"; n ] ->
        Parse.int_pos ~name:"backdrop-opacity" n >|= fun x -> Backdrop_opacity x
    | [ "backdrop"; "saturate"; n ] ->
        Parse.int_pos ~name:"backdrop-saturate" n >|= fun x ->
        Backdrop_saturate x
    | [ "backdrop"; "grayscale"; n ] ->
        Parse.int_pos ~name:"backdrop-grayscale" n >|= fun x ->
        Backdrop_grayscale x
    | [ "backdrop"; "grayscale" ] -> Ok (Backdrop_grayscale 100)
    | [ "backdrop"; "invert"; n ] ->
        Parse.int_pos ~name:"backdrop-invert" n >|= fun x -> Backdrop_invert x
    | [ "backdrop"; "invert" ] -> Ok (Backdrop_invert 100)
    | [ "backdrop"; "sepia"; n ] ->
        Parse.int_pos ~name:"backdrop-sepia" n >|= fun x -> Backdrop_sepia x
    | [ "backdrop"; "sepia" ] -> Ok (Backdrop_sepia 100)
    | [ "backdrop"; "hue"; "rotate"; n ] ->
        Parse.int_any n >|= fun x -> Backdrop_hue_rotate x
    | _ -> err_not_utility

  let to_class = function
    | Blur_none -> "blur-none"
    | Blur_xs -> "blur-xs"
    | Blur_sm -> "blur-sm"
    | Blur -> "blur"
    | Blur_md -> "blur-md"
    | Blur_lg -> "blur-lg"
    | Blur_xl -> "blur-xl"
    | Blur_2xl -> "blur-2xl"
    | Blur_3xl -> "blur-3xl"
    | Brightness n -> "brightness-" ^ string_of_int n
    | Contrast n -> "contrast-" ^ string_of_int n
    | Grayscale 0 -> "grayscale-0"
    | Grayscale 100 -> "grayscale"
    | Grayscale n -> "grayscale-" ^ string_of_int n
    | Saturate n -> "saturate-" ^ string_of_int n
    | Sepia 0 -> "sepia-0"
    | Sepia 100 -> "sepia"
    | Sepia n -> "sepia-" ^ string_of_int n
    | Invert 0 -> "invert-0"
    | Invert 100 -> "invert"
    | Invert n -> "invert-" ^ string_of_int n
    | Hue_rotate n ->
        let prefix = if n < 0 then "-" else "" in
        prefix ^ "hue-rotate-" ^ string_of_int (abs n)
    | Backdrop_blur_none -> "backdrop-blur-none"
    | Backdrop_blur_xs -> "backdrop-blur-xs"
    | Backdrop_blur_sm -> "backdrop-blur-sm"
    | Backdrop_blur -> "backdrop-blur"
    | Backdrop_blur_md -> "backdrop-blur-md"
    | Backdrop_blur_lg -> "backdrop-blur-lg"
    | Backdrop_blur_xl -> "backdrop-blur-xl"
    | Backdrop_blur_2xl -> "backdrop-blur-2xl"
    | Backdrop_blur_3xl -> "backdrop-blur-3xl"
    | Backdrop_brightness n -> "backdrop-brightness-" ^ string_of_int n
    | Backdrop_contrast n -> "backdrop-contrast-" ^ string_of_int n
    | Backdrop_opacity n -> "backdrop-opacity-" ^ string_of_int n
    | Backdrop_saturate n -> "backdrop-saturate-" ^ string_of_int n
    | Backdrop_grayscale 0 -> "backdrop-grayscale-0"
    | Backdrop_grayscale 100 -> "backdrop-grayscale"
    | Backdrop_grayscale n -> "backdrop-grayscale-" ^ string_of_int n
    | Backdrop_invert 0 -> "backdrop-invert-0"
    | Backdrop_invert 100 -> "backdrop-invert"
    | Backdrop_invert n -> "backdrop-invert-" ^ string_of_int n
    | Backdrop_sepia 0 -> "backdrop-sepia-0"
    | Backdrop_sepia 100 -> "backdrop-sepia"
    | Backdrop_sepia n -> "backdrop-sepia-" ^ string_of_int n
    | Backdrop_hue_rotate n ->
        let prefix = if n < 0 then "-" else "" in
        prefix ^ "backdrop-hue-rotate-" ^ string_of_int (abs n)
end

open Handler

let () = Utility.register (module Handler)

(** {1 Public API - Utility Values} *)

let utility x = Utility.base (Self x)
let blur_none = utility Blur_none
let blur_xs = utility Blur_xs
let blur_sm = utility Blur_sm
let blur = utility Blur
let blur_md = utility Blur_md
let blur_lg = utility Blur_lg
let blur_xl = utility Blur_xl
let blur_2xl = utility Blur_2xl
let blur_3xl = utility Blur_3xl
let brightness n = utility (Brightness n)
let contrast n = utility (Contrast n)
let grayscale n = utility (Grayscale n)
let saturate n = utility (Saturate n)
let sepia n = utility (Sepia n)
let invert n = utility (Invert n)
let hue_rotate n = utility (Hue_rotate n)
let backdrop_blur_none = utility Backdrop_blur_none
let backdrop_blur_xs = utility Backdrop_blur_xs
let backdrop_blur_sm = utility Backdrop_blur_sm
let backdrop_blur = utility Backdrop_blur
let backdrop_blur_md = utility Backdrop_blur_md
let backdrop_blur_lg = utility Backdrop_blur_lg
let backdrop_blur_xl = utility Backdrop_blur_xl
let backdrop_blur_2xl = utility Backdrop_blur_2xl
let backdrop_blur_3xl = utility Backdrop_blur_3xl
let backdrop_brightness n = utility (Backdrop_brightness n)
let backdrop_contrast n = utility (Backdrop_contrast n)
let backdrop_opacity n = utility (Backdrop_opacity n)
let backdrop_saturate n = utility (Backdrop_saturate n)
let backdrop_grayscale ?(n = 100) () = utility (Backdrop_grayscale n)
let backdrop_invert ?(n = 100) () = utility (Backdrop_invert n)
let backdrop_sepia ?(n = 100) () = utility (Backdrop_sepia n)
let backdrop_hue_rotate n = utility (Backdrop_hue_rotate n)
