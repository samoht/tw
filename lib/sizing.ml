(** Sizing utilities for width and height

    What's included:
    - Width/height: `w-*`, `h-*` including fractions (`1/2`, `2/3`, ...),
      spacing-scale values, `auto/full/screen/min/max/fit`.
    - Min/max width/height: `min-w-*`, `max-w-*`, `min-h-*`, `max-h-*`,
      including screen breakpoints and prose width.
    - Square sizing: `size-*` (applies to both width and height).

    What's not:
    - Arbitrary values not on the spacing scale. Extend via `style` with raw
      `Css` lengths if needed.

    Parsing contract (`of_string`):
    - Grammar examples: * ["w"; v], ["h"; v] where v ∈ spacing scale | fraction
      | special keywords * ["min"; "w"; v], ["min"; "h"; v] * ["max"; "w"; v],
      ["max"; "w"; "screen"; bk], ["max"; "h"; v] * ["size"; v]
    - Errors: returns `Error (`Msg ..)` with a short reason, or `Error (`Msg
      "Not a sizing utility")` on unknown prefixes. *)

open Core
open Css

(* Get the spacing variable from the Spacing module *)
let spacing_def, spacing_var = Var.theme Var.Spacing (Rem 0.25)

(** Helper to create spacing-based utilities with consistent pattern *)
let spacing_utility prefix css_prop n =
  let class_name = prefix ^ string_of_int n in
  let spacing_value =
    Calc Calc.(length (Var spacing_var) * float (float_of_int n))
  in
  style class_name [ spacing_def; css_prop spacing_value ]

(** {1 Width Utilities} *)

let w' size =
  match size with
  | `None -> style "w-0" [ width (Px 0) ]
  | `Xs -> style "w-xs" [ width (Rem 0.5) ]
  | `Sm -> style "w-sm" [ width (Rem 1.0) ]
  | `Md -> style "w-md" [ width (Rem 1.5) ]
  | `Lg -> style "w-lg" [ width (Rem 2.0) ]
  | `Xl -> style "w-xl" [ width (Rem 3.0) ]
  | `Xl_2 -> style "w-2xl" [ width (Rem 4.0) ]
  | `Xl_3 -> style "w-3xl" [ width (Rem 6.0) ]
  | `Full -> style "w-full" [ width (Pct 100.0) ]

let w_auto = style "w-auto" [ width Auto ]
let w_full = style "w-full" [ width (Pct 100.0) ]
let w_screen = style "w-screen" [ width (Vw 100.0) ]
let w_min = style "w-min" [ width Min_content ]
let w_max = style "w-max" [ width Max_content ]
let w_fit = style "w-fit" [ width Fit_content ]
let w_1_2 = style "w-1/2" [ width (Pct 50.0) ]
let w_1_3 = style "w-1/3" [ width (Pct 33.333333) ]
let w_2_3 = style "w-2/3" [ width (Pct 66.666667) ]
let w_1_4 = style "w-1/4" [ width (Pct 25.0) ]
let w_3_4 = style "w-3/4" [ width (Pct 75.0) ]
let w_1_5 = style "w-1/5" [ width (Pct 20.0) ]
let w_2_5 = style "w-2/5" [ width (Pct 40.0) ]
let w_3_5 = style "w-3/5" [ width (Pct 60.0) ]
let w_4_5 = style "w-4/5" [ width (Pct 80.0) ]

(* Int-based width function for Tailwind scale *)
let w n = spacing_utility "w-" width n

(** {1 Height Utilities} *)

let h' size =
  match size with
  | `None -> style "h-0" [ height (Px 0) ]
  | `Xs -> style "h-xs" [ height (Rem 0.5) ]
  | `Sm -> style "h-sm" [ height (Rem 1.0) ]
  | `Md -> style "h-md" [ height (Rem 1.5) ]
  | `Lg -> style "h-lg" [ height (Rem 2.0) ]
  | `Xl -> style "h-xl" [ height (Rem 3.0) ]
  | `Xl_2 -> style "h-2xl" [ height (Rem 4.0) ]
  | `Xl_3 -> style "h-3xl" [ height (Rem 6.0) ]
  | `Full -> style "h-full" [ height (Pct 100.0) ]

let h_auto = style "h-auto" [ height Auto ]
let h_full = style "h-full" [ height (Pct 100.0) ]
let h_screen = style "h-screen" [ height (Vh 100.0) ]
let h_min = style "h-min" [ height Min_content ]
let h_max = style "h-max" [ height Max_content ]
let h_fit = style "h-fit" [ height Fit_content ]
let h_1_2 = style "h-1/2" [ height (Pct 50.0) ]
let h_1_3 = style "h-1/3" [ height (Pct 33.333333) ]
let h_2_3 = style "h-2/3" [ height (Pct 66.666667) ]
let h_1_4 = style "h-1/4" [ height (Pct 25.0) ]
let h_3_4 = style "h-3/4" [ height (Pct 75.0) ]
let h_1_5 = style "h-1/5" [ height (Pct 20.0) ]
let h_2_5 = style "h-2/5" [ height (Pct 40.0) ]
let h_3_5 = style "h-3/5" [ height (Pct 60.0) ]
let h_4_5 = style "h-4/5" [ height (Pct 80.0) ]

(* Int-based height function for Tailwind scale *)
let h n = spacing_utility "h-" height n

(** {1 Min Width Utilities} *)

let min_w' size =
  match size with
  | `None -> style "min-w-0" [ min_width (Px 0) ]
  | `Xs -> style "min-w-xs" [ min_width (Rem 0.5) ]
  | `Sm -> style "min-w-sm" [ min_width (Rem 1.0) ]
  | `Md -> style "min-w-md" [ min_width (Rem 1.5) ]
  | `Lg -> style "min-w-lg" [ min_width (Rem 2.0) ]
  | `Xl -> style "min-w-xl" [ min_width (Rem 3.0) ]
  | `Xl_2 -> style "min-w-2xl" [ min_width (Rem 4.0) ]
  | `Xl_3 -> style "min-w-3xl" [ min_width (Rem 6.0) ]
  | `Full -> style "min-w-full" [ min_width (Pct 100.0) ]

let min_w_0 = style "min-w-0" [ min_width (Px 0) ]
let min_w_full = style "min-w-full" [ min_width (Pct 100.0) ]
let min_w_min = style "min-w-min" [ min_width Min_content ]
let min_w_max = style "min-w-max" [ min_width Max_content ]
let min_w_fit = style "min-w-fit" [ min_width Fit_content ]

(* Int-based min-width function for Tailwind scale *)
let min_w n = spacing_utility "min-w-" min_width n

(** {1 Max Width Utilities} *)

let max_w' size =
  match size with
  | `None -> style "max-w-none" [ (* max-width: none not directly supported *) ]
  | `Xs -> style "max-w-xs" [ max_width (Rem 20.0) ]
  | `Sm -> style "max-w-sm" [ max_width (Rem 24.0) ]
  | `Md -> style "max-w-md" [ max_width (Rem 28.0) ]
  | `Lg -> style "max-w-lg" [ max_width (Rem 32.0) ]
  | `Xl -> style "max-w-xl" [ max_width (Rem 36.0) ]
  | `Xl_2 -> style "max-w-2xl" [ max_width (Rem 42.0) ]
  | `Xl_3 -> style "max-w-3xl" [ max_width (Rem 48.0) ]
  | `Full -> style "max-w-full" [ max_width (Pct 100.0) ]

let max_w_none =
  style "max-w-none" [ (* max-width: none not directly supported *) ]

let max_w_xs = style "max-w-xs" [ max_width (Rem 20.0) ]
let max_w_sm = style "max-w-sm" [ max_width (Rem 24.0) ]
let max_w_md = style "max-w-md" [ max_width (Rem 28.0) ]
let max_w_lg = style "max-w-lg" [ max_width (Rem 32.0) ]
let max_w_xl = style "max-w-xl" [ max_width (Rem 36.0) ]
let max_w_2xl = style "max-w-2xl" [ max_width (Rem 42.0) ]
let max_w_3xl = style "max-w-3xl" [ max_width (Rem 48.0) ]
let max_w_4xl = style "max-w-4xl" [ max_width (Rem 56.0) ]
let max_w_5xl = style "max-w-5xl" [ max_width (Rem 64.0) ]
let max_w_6xl = style "max-w-6xl" [ max_width (Rem 72.0) ]
let max_w_7xl = style "max-w-7xl" [ max_width (Rem 80.0) ]
let max_w_full = style "max-w-full" [ max_width (Pct 100.0) ]
let max_w_min = style "max-w-min" [ max_width Min_content ]
let max_w_max = style "max-w-max" [ max_width Max_content ]
let max_w_fit = style "max-w-fit" [ max_width Fit_content ]
let max_w_prose = style "max-w-prose" [ max_width (Ch 65.0) ]
let max_w_screen_sm = style "max-w-screen-sm" [ max_width (Px 640) ]
let max_w_screen_md = style "max-w-screen-md" [ max_width (Px 768) ]
let max_w_screen_lg = style "max-w-screen-lg" [ max_width (Px 1024) ]
let max_w_screen_xl = style "max-w-screen-xl" [ max_width (Px 1280) ]
let max_w_screen_2xl = style "max-w-screen-2xl" [ max_width (Px 1536) ]

(* Int-based max-width function for Tailwind scale *)
let max_w n = spacing_utility "max-w-" max_width n

(** {1 Min Height Utilities} *)

let min_h' size =
  match size with
  | `None -> style "min-h-0" [ min_height (Px 0) ]
  | `Xs -> style "min-h-xs" [ min_height (Rem 0.5) ]
  | `Sm -> style "min-h-sm" [ min_height (Rem 1.0) ]
  | `Md -> style "min-h-md" [ min_height (Rem 1.5) ]
  | `Lg -> style "min-h-lg" [ min_height (Rem 2.0) ]
  | `Xl -> style "min-h-xl" [ min_height (Rem 3.0) ]
  | `Xl_2 -> style "min-h-2xl" [ min_height (Rem 4.0) ]
  | `Xl_3 -> style "min-h-3xl" [ min_height (Rem 6.0) ]
  | `Full -> style "min-h-full" [ min_height (Pct 100.0) ]

let min_h_0 = style "min-h-0" [ min_height (Px 0) ]
let min_h_full = style "min-h-full" [ min_height (Pct 100.0) ]
let min_h_screen = style "min-h-screen" [ min_height (Vh 100.0) ]
let min_h_min = style "min-h-min" [ min_height Min_content ]
let min_h_max = style "min-h-max" [ min_height Max_content ]
let min_h_fit = style "min-h-fit" [ min_height Fit_content ]

(* Int-based min-height function for Tailwind scale *)
let min_h n = spacing_utility "min-h-" min_height n

(** {1 Max Height Utilities} *)

let max_h' size =
  match size with
  | `None ->
      style "max-h-none" [ (* max-height: none not directly supported *) ]
  | `Xs -> style "max-h-xs" [ max_height (Rem 0.5) ]
  | `Sm -> style "max-h-sm" [ max_height (Rem 1.0) ]
  | `Md -> style "max-h-md" [ max_height (Rem 1.5) ]
  | `Lg -> style "max-h-lg" [ max_height (Rem 2.0) ]
  | `Xl -> style "max-h-xl" [ max_height (Rem 3.0) ]
  | `Xl_2 -> style "max-h-2xl" [ max_height (Rem 4.0) ]
  | `Xl_3 -> style "max-h-3xl" [ max_height (Rem 6.0) ]
  | `Full -> style "max-h-full" [ max_height (Pct 100.0) ]

let max_h_none =
  style "max-h-none" [ (* max-height: none not directly supported *) ]

let max_h_full = style "max-h-full" [ max_height (Pct 100.0) ]
let max_h_screen = style "max-h-screen" [ max_height (Vh 100.0) ]
let max_h_min = style "max-h-min" [ max_height Min_content ]
let max_h_max = style "max-h-max" [ max_height Max_content ]
let max_h_fit = style "max-h-fit" [ max_height Fit_content ]

(* Int-based max-height function for Tailwind scale *)
let max_h n = spacing_utility "max-h-" max_height n

(** {1 String Parsing} *)

let fraction_of_string = function
  | "1/2" -> Ok (style "w-1/2" [ width (Pct 50.0) ])
  | "1/3" -> Ok (style "w-1/3" [ width (Pct 33.333333) ])
  | "2/3" -> Ok (style "w-2/3" [ width (Pct 66.666667) ])
  | "1/4" -> Ok (style "w-1/4" [ width (Pct 25.0) ])
  | "3/4" -> Ok (style "w-3/4" [ width (Pct 75.0) ])
  | "1/5" -> Ok (style "w-1/5" [ width (Pct 20.0) ])
  | "2/5" -> Ok (style "w-2/5" [ width (Pct 40.0) ])
  | "3/5" -> Ok (style "w-3/5" [ width (Pct 60.0) ])
  | "4/5" -> Ok (style "w-4/5" [ width (Pct 80.0) ])
  | "1/6" -> Ok (style "w-1/6" [ width (Pct 16.666667) ])
  | "5/6" -> Ok (style "w-5/6" [ width (Pct 83.333333) ])
  | s -> Error (`Msg ("Invalid fraction: " ^ s))

let h_fraction_of_string = function
  | "1/2" -> Ok (style "h-1/2" [ height (Pct 50.0) ])
  | "1/3" -> Ok (style "h-1/3" [ height (Pct 33.333333) ])
  | "2/3" -> Ok (style "h-2/3" [ height (Pct 66.666667) ])
  | "1/4" -> Ok (style "h-1/4" [ height (Pct 25.0) ])
  | "3/4" -> Ok (style "h-3/4" [ height (Pct 75.0) ])
  | "1/5" -> Ok (style "h-1/5" [ height (Pct 20.0) ])
  | "2/5" -> Ok (style "h-2/5" [ height (Pct 40.0) ])
  | "3/5" -> Ok (style "h-3/5" [ height (Pct 60.0) ])
  | "4/5" -> Ok (style "h-4/5" [ height (Pct 80.0) ])
  | "1/6" -> Ok (style "h-1/6" [ height (Pct 16.666667) ])
  | "5/6" -> Ok (style "h-5/6" [ height (Pct 83.333333) ])
  | s -> Error (`Msg ("Invalid fraction: " ^ s))

let size_fraction_of_string = function
  | "1/2" -> Ok (style "size-1/2" [ width (Pct 50.0); height (Pct 50.0) ])
  | "1/3" ->
      Ok (style "size-1/3" [ width (Pct 33.333333); height (Pct 33.333333) ])
  | "2/3" ->
      Ok (style "size-2/3" [ width (Pct 66.666667); height (Pct 66.666667) ])
  | "1/4" -> Ok (style "size-1/4" [ width (Pct 25.0); height (Pct 25.0) ])
  | "3/4" -> Ok (style "size-3/4" [ width (Pct 75.0); height (Pct 75.0) ])
  | s -> Error (`Msg ("Invalid fraction: " ^ s))

(** {1 Aspect Ratio Utilities} *)

let aspect_auto = style "aspect-auto" [ Css.aspect_ratio Auto ]
let aspect_square = style "aspect-square" [ Css.aspect_ratio (Ratio (1, 1)) ]
let aspect_video = style "aspect-video" [ Css.aspect_ratio (Ratio (16, 9)) ]

let aspect_ratio w h =
  let class_name =
    Pp.str [ "aspect-["; string_of_int w; "/"; string_of_int h; "]" ]
  in
  style class_name [ Css.aspect_ratio (Ratio (w, h)) ]

let spacing_value_of_string = function
  | "0" -> Ok (Px 0)
  | "px" -> Ok (Px 1)
  | "0.5" -> Ok (Rem 0.125)
  | "1" -> Ok (Rem 0.25)
  | "1.5" -> Ok (Rem 0.375)
  | "2" -> Ok (Rem 0.5)
  | "2.5" -> Ok (Rem 0.625)
  | "3" -> Ok (Rem 0.75)
  | "3.5" -> Ok (Rem 0.875)
  | "4" -> Ok (Rem 1.0)
  | "5" -> Ok (Rem 1.25)
  | "6" -> Ok (Rem 1.5)
  | "7" -> Ok (Rem 1.75)
  | "8" -> Ok (Rem 2.0)
  | "9" -> Ok (Rem 2.25)
  | "10" -> Ok (Rem 2.5)
  | "11" -> Ok (Rem 2.75)
  | "12" -> Ok (Rem 3.0)
  | "14" -> Ok (Rem 3.5)
  | "16" -> Ok (Rem 4.0)
  | "20" -> Ok (Rem 5.0)
  | "24" -> Ok (Rem 6.0)
  | "28" -> Ok (Rem 7.0)
  | "32" -> Ok (Rem 8.0)
  | "36" -> Ok (Rem 9.0)
  | "40" -> Ok (Rem 10.0)
  | "44" -> Ok (Rem 11.0)
  | "48" -> Ok (Rem 12.0)
  | "52" -> Ok (Rem 13.0)
  | "56" -> Ok (Rem 14.0)
  | "60" -> Ok (Rem 15.0)
  | "64" -> Ok (Rem 16.0)
  | "72" -> Ok (Rem 18.0)
  | "80" -> Ok (Rem 20.0)
  | "96" -> Ok (Rem 24.0)
  | s -> Error (`Msg ("Invalid spacing value: " ^ s))

let of_string parts =
  let parse_w = function
    | "auto" -> Ok w_auto
    | "full" -> Ok w_full
    | "screen" -> Ok w_screen
    | "min" -> Ok w_min
    | "max" -> Ok w_max
    | "fit" -> Ok w_fit
    | frac when String.contains frac '/' -> fraction_of_string frac
    | v -> (
        match spacing_value_of_string v with
        | Ok css_val -> Ok (style ("w-" ^ v) [ width css_val ])
        | Error _ -> Error (`Msg ("Invalid width value: " ^ v)))
  in
  let parse_h = function
    | "auto" -> Ok h_auto
    | "full" -> Ok h_full
    | "screen" -> Ok h_screen
    | "min" -> Ok h_min
    | "max" -> Ok h_max
    | "fit" -> Ok h_fit
    | frac when String.contains frac '/' -> h_fraction_of_string frac
    | v -> (
        match spacing_value_of_string v with
        | Ok css_val -> Ok (style ("h-" ^ v) [ height css_val ])
        | Error _ -> Error (`Msg ("Invalid height value: " ^ v)))
  in
  let parse_min_w = function
    | "0" -> Ok min_w_0
    | "full" -> Ok min_w_full
    | "min" -> Ok min_w_min
    | "max" -> Ok min_w_max
    | "fit" -> Ok min_w_fit
    | v -> (
        match spacing_value_of_string v with
        | Ok css_val -> Ok (style ("min-w-" ^ v) [ min_width css_val ])
        | Error _ -> Error (`Msg ("Invalid min-width value: " ^ v)))
  in
  let parse_min_h = function
    | "0" -> Ok min_h_0
    | "full" -> Ok min_h_full
    | "screen" -> Ok min_h_screen
    | v -> (
        match spacing_value_of_string v with
        | Ok css_val -> Ok (style ("min-h-" ^ v) [ min_height css_val ])
        | Error _ -> Error (`Msg ("Invalid min-height value: " ^ v)))
  in
  let parse_max_w = function
    | "none" -> Ok max_w_none
    | "xs" -> Ok max_w_xs
    | "sm" -> Ok max_w_sm
    | "md" -> Ok max_w_md
    | "lg" -> Ok max_w_lg
    | "xl" -> Ok max_w_xl
    | "2xl" -> Ok max_w_2xl
    | "3xl" -> Ok max_w_3xl
    | "4xl" -> Ok max_w_4xl
    | "5xl" -> Ok max_w_5xl
    | "6xl" -> Ok max_w_6xl
    | "7xl" -> Ok max_w_7xl
    | "full" -> Ok max_w_full
    | "min" -> Ok max_w_min
    | "max" -> Ok max_w_max
    | "fit" -> Ok max_w_fit
    | "prose" -> Ok max_w_prose
    | v -> Error (`Msg ("Invalid max-width value: " ^ v))
  in
  let parse_max_w_screen = function
    | "sm" -> Ok max_w_screen_sm
    | "md" -> Ok max_w_screen_md
    | "lg" -> Ok max_w_screen_lg
    | "xl" -> Ok max_w_screen_xl
    | "2xl" -> Ok max_w_screen_2xl
    | s -> Error (`Msg ("Invalid max-width screen size: " ^ s))
  in
  let parse_max_h = function
    | "none" -> Ok max_h_none
    | "full" -> Ok max_h_full
    | "screen" -> Ok max_h_screen
    | "min" -> Ok max_h_min
    | "max" -> Ok max_h_max
    | "fit" -> Ok max_h_fit
    | v -> (
        match spacing_value_of_string v with
        | Ok css_val -> Ok (style ("max-h-" ^ v) [ max_height css_val ])
        | Error _ -> Error (`Msg ("Invalid max-height value: " ^ v)))
  in
  let parse_size = function
    | "auto" -> Ok (style "size-auto" [ width Auto; height Auto ])
    | "full" -> Ok (style "size-full" [ width (Pct 100.0); height (Pct 100.0) ])
    | "min" -> Ok (style "size-min" [ width Min_content; height Min_content ])
    | "max" -> Ok (style "size-max" [ width Max_content; height Max_content ])
    | "fit" -> Ok (style "size-fit" [ width Fit_content; height Fit_content ])
    | frac when String.contains frac '/' -> size_fraction_of_string frac
    | v -> (
        match spacing_value_of_string v with
        | Ok css_val ->
            Ok (style ("size-" ^ v) [ width css_val; height css_val ])
        | Error _ -> Error (`Msg ("Invalid size value: " ^ v)))
  in
  match parts with
  | [ "w"; value ] -> parse_w value
  | [ "h"; value ] -> parse_h value
  | [ "min"; "w"; value ] -> parse_min_w value
  | [ "min"; "h"; value ] -> parse_min_h value
  | [ "max"; "w"; value ] -> parse_max_w value
  | [ "max"; "w"; "screen"; size ] -> parse_max_w_screen size
  | [ "max"; "h"; value ] -> parse_max_h value
  | [ "size"; value ] -> parse_size value
  | [ "aspect"; "auto" ] -> Ok aspect_auto
  | [ "aspect"; "square" ] -> Ok aspect_square
  | [ "aspect"; "video" ] -> Ok aspect_video
  | _ -> Error (`Msg "Not a sizing utility")
