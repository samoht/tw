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

open Style
open Css

(* Use shared spacing variable from Theme *)
let spacing_var = Theme.spacing_var

(** {1 Utility Types} *)

type utility =
  (* Width utilities *)
  | W_auto
  | W_full
  | W_screen
  | W_min
  | W_max
  | W_fit
  | W_px
  | W_spacing of float
  | W_fraction of string
  (* Height utilities *)
  | H_auto
  | H_full
  | H_screen
  | H_min
  | H_max
  | H_fit
  | H_px
  | H_spacing of float
  | H_fraction of string
  (* Min-width utilities *)
  | Min_w_0
  | Min_w_full
  | Min_w_min
  | Min_w_max
  | Min_w_fit
  | Min_w_spacing of float
  (* Max-width utilities *)
  | Max_w_none
  | Max_w_xs
  | Max_w_sm
  | Max_w_md
  | Max_w_lg
  | Max_w_xl
  | Max_w_2xl
  | Max_w_3xl
  | Max_w_4xl
  | Max_w_5xl
  | Max_w_6xl
  | Max_w_7xl
  | Max_w_full
  | Max_w_min
  | Max_w_max
  | Max_w_fit
  | Max_w_prose
  | Max_w_screen_sm
  | Max_w_screen_md
  | Max_w_screen_lg
  | Max_w_screen_xl
  | Max_w_screen_2xl
  | Max_w_spacing of float
  (* Min-height utilities *)
  | Min_h_0
  | Min_h_full
  | Min_h_screen
  | Min_h_spacing of float
  (* Max-height utilities *)
  | Max_h_none
  | Max_h_full
  | Max_h_screen
  | Max_h_min
  | Max_h_max
  | Max_h_fit
  | Max_h_spacing of float
  (* Size utilities (both width and height) *)
  | Size_auto
  | Size_full
  | Size_min
  | Size_max
  | Size_fit
  | Size_spacing of float
  | Size_fraction of string
  (* Aspect utilities *)
  | Aspect_auto
  | Aspect_square
  | Aspect_video

(** Helper to create spacing-based utilities with consistent pattern *)
let spacing_utility prefix css_prop n =
  let class_name = prefix ^ Css.Pp.to_string Css.Pp.float n in
  let decl, spacing_ref = Var.binding spacing_var (Rem 0.25) in
  let spacing_value : Css.length =
    Calc Calc.(mul (length (Var spacing_ref)) (float n))
  in
  style class_name (decl :: [ css_prop spacing_value ])

(** {1 Width Utilities} *)

let w' size =
  match size with
  | `None -> style "w-0" [ width (Px 0.) ]
  | `Xs -> style "w-xs" [ width (Rem 0.5) ]
  | `Sm -> style "w-sm" [ width (Rem 1.0) ]
  | `Md -> style "w-md" [ width (Rem 1.5) ]
  | `Lg -> style "w-lg" [ width (Rem 2.0) ]
  | `Xl -> style "w-xl" [ width (Rem 3.0) ]
  | `Xl_2 -> style "w-2xl" [ width (Rem 4.0) ]
  | `Xl_3 -> style "w-3xl" [ width (Rem 6.0) ]
  | `Full -> style "w-full" [ width (Pct 100.0) ]
  | `Rem n -> spacing_utility "w-" width n

let w_auto = style "w-auto" [ width Auto ]
let w_px = style "w-px" [ width (Px 1.0) ]
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

(* Int-based width function for Tailwind scale (n * 0.25rem) *)
let w n = w' (`Rem (float_of_int n))

(** {1 Height Utilities} *)

let h' size =
  match size with
  | `None -> style "h-0" [ height (Px 0.) ]
  | `Xs -> style "h-xs" [ height (Rem 0.5) ]
  | `Sm -> style "h-sm" [ height (Rem 1.0) ]
  | `Md -> style "h-md" [ height (Rem 1.5) ]
  | `Lg -> style "h-lg" [ height (Rem 2.0) ]
  | `Xl -> style "h-xl" [ height (Rem 3.0) ]
  | `Xl_2 -> style "h-2xl" [ height (Rem 4.0) ]
  | `Xl_3 -> style "h-3xl" [ height (Rem 6.0) ]
  | `Full -> style "h-full" [ height (Pct 100.0) ]
  | `Rem n -> spacing_utility "h-" height n

let h_auto = style "h-auto" [ height Auto ]
let h_px = style "h-px" [ height (Px 1.0) ]
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

(* Int-based height function for Tailwind scale (n * 0.25rem) *)
let h n = h' (`Rem (float_of_int n))

(** {1 Min Width Utilities} *)

let min_w' size =
  match size with
  | `None -> style "min-w-0" [ min_width (Px 0.) ]
  | `Xs -> style "min-w-xs" [ min_width (Rem 0.5) ]
  | `Sm -> style "min-w-sm" [ min_width (Rem 1.0) ]
  | `Md -> style "min-w-md" [ min_width (Rem 1.5) ]
  | `Lg -> style "min-w-lg" [ min_width (Rem 2.0) ]
  | `Xl -> style "min-w-xl" [ min_width (Rem 3.0) ]
  | `Xl_2 -> style "min-w-2xl" [ min_width (Rem 4.0) ]
  | `Xl_3 -> style "min-w-3xl" [ min_width (Rem 6.0) ]
  | `Full -> style "min-w-full" [ min_width (Pct 100.0) ]
  | `Rem n -> spacing_utility "min-w-" min_width n

let min_w_0 = style "min-w-0" [ min_width (Px 0.) ]
let min_w_full = style "min-w-full" [ min_width (Pct 100.0) ]
let min_w_min = style "min-w-min" [ min_width Min_content ]
let min_w_max = style "min-w-max" [ min_width Max_content ]
let min_w_fit = style "min-w-fit" [ min_width Fit_content ]

(* Int-based min-width function for Tailwind scale (n * 0.25rem) *)
let min_w n = min_w' (`Rem (float_of_int n))

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
  | `Rem n -> spacing_utility "max-w-" max_width n

(* Container size theme variables *)
let container_xs = Var.theme Css.Length "container-xs" ~order:(5, 0)
let container_sm = Var.theme Css.Length "container-sm" ~order:(5, 1)
let container_md = Var.theme Css.Length "container-md" ~order:(5, 2)
let container_lg = Var.theme Css.Length "container-lg" ~order:(5, 3)
let container_xl = Var.theme Css.Length "container-xl" ~order:(5, 4)
let container_2xl = Var.theme Css.Length "container-2xl" ~order:(5, 5)
let container_3xl = Var.theme Css.Length "container-3xl" ~order:(5, 6)
let container_4xl = Var.theme Css.Length "container-4xl" ~order:(5, 7)
let container_5xl = Var.theme Css.Length "container-5xl" ~order:(5, 8)
let container_6xl = Var.theme Css.Length "container-6xl" ~order:(5, 9)
let container_7xl = Var.theme Css.Length "container-7xl" ~order:(5, 10)

let max_w_none =
  style "max-w-none" [ (* max-width: none not directly supported *) ]

let max_w_xs =
  let decl, ref_ = Var.binding container_xs (Rem 20.0) in
  style "max-w-xs" [ decl; max_width (Var ref_) ]

let max_w_sm =
  let decl, ref_ = Var.binding container_sm (Rem 24.0) in
  style "max-w-sm" [ decl; max_width (Var ref_) ]

let max_w_md =
  let decl, ref_ = Var.binding container_md (Rem 28.0) in
  style "max-w-md" [ decl; max_width (Var ref_) ]

let max_w_lg =
  let decl, ref_ = Var.binding container_lg (Rem 32.0) in
  style "max-w-lg" [ decl; max_width (Var ref_) ]

let max_w_xl =
  let decl, ref_ = Var.binding container_xl (Rem 36.0) in
  style "max-w-xl" [ decl; max_width (Var ref_) ]

let max_w_2xl =
  let decl, ref_ = Var.binding container_2xl (Rem 42.0) in
  style "max-w-2xl" [ decl; max_width (Var ref_) ]

let max_w_3xl =
  let decl, ref_ = Var.binding container_3xl (Rem 48.0) in
  style "max-w-3xl" [ decl; max_width (Var ref_) ]

let max_w_4xl =
  let decl, ref_ = Var.binding container_4xl (Rem 56.0) in
  style "max-w-4xl" [ decl; max_width (Var ref_) ]

let max_w_5xl =
  let decl, ref_ = Var.binding container_5xl (Rem 64.0) in
  style "max-w-5xl" [ decl; max_width (Var ref_) ]

let max_w_6xl =
  let decl, ref_ = Var.binding container_6xl (Rem 72.0) in
  style "max-w-6xl" [ decl; max_width (Var ref_) ]

let max_w_7xl =
  let decl, ref_ = Var.binding container_7xl (Rem 80.0) in
  style "max-w-7xl" [ decl; max_width (Var ref_) ]

let max_w_full = style "max-w-full" [ max_width (Pct 100.0) ]
let max_w_min = style "max-w-min" [ max_width Min_content ]
let max_w_max = style "max-w-max" [ max_width Max_content ]
let max_w_fit = style "max-w-fit" [ max_width Fit_content ]
let max_w_prose = style "max-w-prose" [ max_width (Ch 65.0) ]
let max_w_screen_sm = style "max-w-screen-sm" [ max_width (Px 640.) ]
let max_w_screen_md = style "max-w-screen-md" [ max_width (Px 768.) ]
let max_w_screen_lg = style "max-w-screen-lg" [ max_width (Px 1024.) ]
let max_w_screen_xl = style "max-w-screen-xl" [ max_width (Px 1280.) ]
let max_w_screen_2xl = style "max-w-screen-2xl" [ max_width (Px 1536.) ]

(* Int-based max-width function for Tailwind scale (n * 0.25rem) *)
let max_w n = max_w' (`Rem (float_of_int n))

(** {1 Min Height Utilities} *)

let min_h' size =
  match size with
  | `None -> style "min-h-0" [ min_height (Px 0.) ]
  | `Xs -> style "min-h-xs" [ min_height (Rem 0.5) ]
  | `Sm -> style "min-h-sm" [ min_height (Rem 1.0) ]
  | `Md -> style "min-h-md" [ min_height (Rem 1.5) ]
  | `Lg -> style "min-h-lg" [ min_height (Rem 2.0) ]
  | `Xl -> style "min-h-xl" [ min_height (Rem 3.0) ]
  | `Xl_2 -> style "min-h-2xl" [ min_height (Rem 4.0) ]
  | `Xl_3 -> style "min-h-3xl" [ min_height (Rem 6.0) ]
  | `Full -> style "min-h-full" [ min_height (Pct 100.0) ]
  | `Rem n -> spacing_utility "min-h-" min_height n

let min_h_0 = style "min-h-0" [ min_height (Px 0.) ]
let min_h_full = style "min-h-full" [ min_height (Pct 100.0) ]
let min_h_screen = style "min-h-screen" [ min_height (Vh 100.0) ]
let min_h_min = style "min-h-min" [ min_height Min_content ]
let min_h_max = style "min-h-max" [ min_height Max_content ]
let min_h_fit = style "min-h-fit" [ min_height Fit_content ]

(* Int-based min-height function for Tailwind scale (n * 0.25rem) *)
let min_h n = min_h' (`Rem (float_of_int n))

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
  | `Rem n -> spacing_utility "max-h-" max_height n

let max_h_none =
  style "max-h-none" [ (* max-height: none not directly supported *) ]

let max_h_full = style "max-h-full" [ max_height (Pct 100.0) ]
let max_h_screen = style "max-h-screen" [ max_height (Vh 100.0) ]
let max_h_min = style "max-h-min" [ max_height Min_content ]
let max_h_max = style "max-h-max" [ max_height Max_content ]
let max_h_fit = style "max-h-fit" [ max_height Fit_content ]

(* Int-based max-height function for Tailwind scale (n * 0.25rem) *)
let max_h n = max_h' (`Rem (float_of_int n))

(** {1 String Parsing} *)

let fraction_table =
  [
    ("1/2", 50.0);
    ("1/3", 33.333333);
    ("2/3", 66.666667);
    ("1/4", 25.0);
    ("3/4", 75.0);
    ("1/5", 20.0);
    ("2/5", 40.0);
    ("3/5", 60.0);
    ("4/5", 80.0);
    ("1/6", 16.666667);
    ("5/6", 83.333333);
  ]

(** {1 Aspect Ratio Utilities} *)

let aspect_auto = style "aspect-auto" [ Css.aspect_ratio Auto ]
let aspect_square = style "aspect-square" [ Css.aspect_ratio (Ratio (1., 1.)) ]
let aspect_video = style "aspect-video" [ Css.aspect_ratio (Ratio (16., 9.)) ]

let aspect_ratio w h =
  let class_name =
    String.concat "" [ "aspect-["; string_of_int w; "/"; string_of_int h; "]" ]
  in
  style class_name [ Css.aspect_ratio (Ratio (float_of_int w, float_of_int h)) ]

(** {1 Conversion Functions} *)

let to_style = function
  (* Width utilities *)
  | W_auto -> w_auto
  | W_px -> w_px
  | W_full -> w_full
  | W_screen -> w_screen
  | W_min -> w_min
  | W_max -> w_max
  | W_fit -> w_fit
  | W_spacing n -> w' (`Rem n)
  | W_fraction f -> (
      match f with
      | "1/2" -> w_1_2
      | "1/3" -> w_1_3
      | "2/3" -> w_2_3
      | "1/4" -> w_1_4
      | "3/4" -> w_3_4
      | "1/5" -> w_1_5
      | "2/5" -> w_2_5
      | "3/5" -> w_3_5
      | "4/5" -> w_4_5
      | _ -> failwith ("Unknown width fraction: " ^ f))
  (* Height utilities *)
  | H_auto -> h_auto
  | H_px -> h_px
  | H_full -> h_full
  | H_screen -> h_screen
  | H_min -> h_min
  | H_max -> h_max
  | H_fit -> h_fit
  | H_spacing n -> h' (`Rem n)
  | H_fraction f -> (
      match f with
      | "1/2" -> h_1_2
      | "1/3" -> h_1_3
      | "2/3" -> h_2_3
      | "1/4" -> h_1_4
      | "3/4" -> h_3_4
      | "1/5" -> h_1_5
      | "2/5" -> h_2_5
      | "3/5" -> h_3_5
      | "4/5" -> h_4_5
      | _ -> failwith ("Unknown height fraction: " ^ f))
  (* Min-width utilities *)
  | Min_w_0 -> min_w_0
  | Min_w_full -> min_w_full
  | Min_w_min -> min_w_min
  | Min_w_max -> min_w_max
  | Min_w_fit -> min_w_fit
  | Min_w_spacing n -> min_w' (`Rem n)
  (* Max-width utilities *)
  | Max_w_none -> max_w_none
  | Max_w_xs -> max_w_xs
  | Max_w_sm -> max_w_sm
  | Max_w_md -> max_w_md
  | Max_w_lg -> max_w_lg
  | Max_w_xl -> max_w_xl
  | Max_w_2xl -> max_w_2xl
  | Max_w_3xl -> max_w_3xl
  | Max_w_4xl -> max_w_4xl
  | Max_w_5xl -> max_w_5xl
  | Max_w_6xl -> max_w_6xl
  | Max_w_7xl -> max_w_7xl
  | Max_w_full -> max_w_full
  | Max_w_min -> max_w_min
  | Max_w_max -> max_w_max
  | Max_w_fit -> max_w_fit
  | Max_w_prose -> max_w_prose
  | Max_w_screen_sm -> max_w_screen_sm
  | Max_w_screen_md -> max_w_screen_md
  | Max_w_screen_lg -> max_w_screen_lg
  | Max_w_screen_xl -> max_w_screen_xl
  | Max_w_screen_2xl -> max_w_screen_2xl
  | Max_w_spacing n -> max_w' (`Rem n)
  (* Min-height utilities *)
  | Min_h_0 -> min_h_0
  | Min_h_full -> min_h_full
  | Min_h_screen -> min_h_screen
  | Min_h_spacing n -> min_h' (`Rem n)
  (* Max-height utilities *)
  | Max_h_none -> max_h_none
  | Max_h_full -> max_h_full
  | Max_h_screen -> max_h_screen
  | Max_h_min -> max_h_min
  | Max_h_max -> max_h_max
  | Max_h_fit -> max_h_fit
  | Max_h_spacing n -> max_h' (`Rem n)
  (* Size utilities *)
  | Size_auto -> style "size-auto" [ width Auto; height Auto ]
  | Size_full -> style "size-full" [ width (Pct 100.0); height (Pct 100.0) ]
  | Size_min -> style "size-min" [ width Min_content; height Min_content ]
  | Size_max -> style "size-max" [ width Max_content; height Max_content ]
  | Size_fit -> style "size-fit" [ width Fit_content; height Fit_content ]
  | Size_spacing n ->
      let class_name = "size-" ^ Css.Pp.to_string Css.Pp.float n in
      let decl, spacing_ref = Var.binding spacing_var (Rem 0.25) in
      let spacing_value : Css.length =
        Calc Calc.(mul (length (Var spacing_ref)) (float n))
      in
      style class_name (decl :: [ width spacing_value; height spacing_value ])
  | Size_fraction f -> (
      match
        List.assoc_opt f
          [
            ("1/2", 50.0);
            ("1/3", 33.333333);
            ("2/3", 66.666667);
            ("1/4", 25.0);
            ("3/4", 75.0);
            ("1/5", 20.0);
            ("2/5", 40.0);
            ("3/5", 60.0);
            ("4/5", 80.0);
            ("1/6", 16.666667);
            ("5/6", 83.333333);
          ]
      with
      | Some pct -> style ("size-" ^ f) [ width (Pct pct); height (Pct pct) ]
      | None -> failwith ("Unknown size fraction: " ^ f))
  (* Aspect utilities *)
  | Aspect_auto -> aspect_auto
  | Aspect_square -> aspect_square
  | Aspect_video -> aspect_video

let of_string parts =
  let parse_w = function
    | "auto" -> Ok W_auto
    | "px" -> Ok W_px
    | "full" -> Ok W_full
    | "screen" -> Ok W_screen
    | "min" -> Ok W_min
    | "max" -> Ok W_max
    | "fit" -> Ok W_fit
    | frac when String.contains frac '/' ->
        if List.mem_assoc frac fraction_table then Ok (W_fraction frac)
        else Error (`Msg ("Invalid width fraction: " ^ frac))
    | v -> (
        match float_of_string_opt v with
        | Some n when n >= 0. -> Ok (W_spacing n)
        | _ -> Error (`Msg ("Invalid width value: " ^ v)))
  in
  let parse_h = function
    | "auto" -> Ok H_auto
    | "px" -> Ok H_px
    | "full" -> Ok H_full
    | "screen" -> Ok H_screen
    | "min" -> Ok H_min
    | "max" -> Ok H_max
    | "fit" -> Ok H_fit
    | frac when String.contains frac '/' ->
        if List.mem_assoc frac fraction_table then Ok (H_fraction frac)
        else Error (`Msg ("Invalid height fraction: " ^ frac))
    | v -> (
        match float_of_string_opt v with
        | Some n when n >= 0. -> Ok (H_spacing n)
        | _ -> Error (`Msg ("Invalid height value: " ^ v)))
  in
  let parse_min_w = function
    | "0" -> Ok Min_w_0
    | "full" -> Ok Min_w_full
    | "min" -> Ok Min_w_min
    | "max" -> Ok Min_w_max
    | "fit" -> Ok Min_w_fit
    | v -> (
        match float_of_string_opt v with
        | Some n when n >= 0. -> Ok (Min_w_spacing n)
        | _ -> Error (`Msg ("Invalid min-width value: " ^ v)))
  in
  let parse_min_h = function
    | "0" -> Ok Min_h_0
    | "full" -> Ok Min_h_full
    | "screen" -> Ok Min_h_screen
    | v -> (
        match float_of_string_opt v with
        | Some n when n >= 0. -> Ok (Min_h_spacing n)
        | _ -> Error (`Msg ("Invalid min-height value: " ^ v)))
  in
  let parse_max_w = function
    | "none" -> Ok Max_w_none
    | "xs" -> Ok Max_w_xs
    | "sm" -> Ok Max_w_sm
    | "md" -> Ok Max_w_md
    | "lg" -> Ok Max_w_lg
    | "xl" -> Ok Max_w_xl
    | "2xl" -> Ok Max_w_2xl
    | "3xl" -> Ok Max_w_3xl
    | "4xl" -> Ok Max_w_4xl
    | "5xl" -> Ok Max_w_5xl
    | "6xl" -> Ok Max_w_6xl
    | "7xl" -> Ok Max_w_7xl
    | "full" -> Ok Max_w_full
    | "min" -> Ok Max_w_min
    | "max" -> Ok Max_w_max
    | "fit" -> Ok Max_w_fit
    | "prose" -> Ok Max_w_prose
    | v -> (
        match float_of_string_opt v with
        | Some n when n >= 0. -> Ok (Max_w_spacing n)
        | _ -> Error (`Msg ("Invalid max-width value: " ^ v)))
  in
  let parse_max_w_screen = function
    | "sm" -> Ok Max_w_screen_sm
    | "md" -> Ok Max_w_screen_md
    | "lg" -> Ok Max_w_screen_lg
    | "xl" -> Ok Max_w_screen_xl
    | "2xl" -> Ok Max_w_screen_2xl
    | s -> Error (`Msg ("Invalid max-width screen size: " ^ s))
  in
  let parse_max_h = function
    | "none" -> Ok Max_h_none
    | "full" -> Ok Max_h_full
    | "screen" -> Ok Max_h_screen
    | "min" -> Ok Max_h_min
    | "max" -> Ok Max_h_max
    | "fit" -> Ok Max_h_fit
    | v -> (
        match float_of_string_opt v with
        | Some n when n >= 0. -> Ok (Max_h_spacing n)
        | _ -> Error (`Msg ("Invalid max-height value: " ^ v)))
  in
  let parse_size = function
    | "auto" -> Ok Size_auto
    | "full" -> Ok Size_full
    | "min" -> Ok Size_min
    | "max" -> Ok Size_max
    | "fit" -> Ok Size_fit
    | frac when String.contains frac '/' ->
        if List.mem_assoc frac fraction_table then Ok (Size_fraction frac)
        else Error (`Msg ("Invalid size fraction: " ^ frac))
    | v -> (
        match float_of_string_opt v with
        | Some n when n >= 0. -> Ok (Size_spacing n)
        | _ -> Error (`Msg ("Invalid size value: " ^ v)))
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
  | [ "aspect"; "auto" ] -> Ok Aspect_auto
  | [ "aspect"; "square" ] -> Ok Aspect_square
  | [ "aspect"; "video" ] -> Ok Aspect_video
  | _ -> Error (`Msg "Not a sizing utility")

(** Suborder function for sorting sizing utilities within their priority group.
    Heights come before widths, with numeric values sorted numerically. *)
let suborder = function
  (* Height utilities (0-9999) *)
  | H_auto -> 0
  | H_full -> 1
  | H_screen -> 2
  | H_min -> 3
  | H_max -> 4
  | H_fit -> 5
  | H_spacing n -> 100 + int_of_float (n *. 100.)
  | H_px -> 10000
  | H_fraction _ -> 50000
  (* Width utilities (100000-199999) *)
  | W_auto -> 100000
  | W_full -> 100001
  | W_screen -> 100002
  | W_min -> 100003
  | W_max -> 100004
  | W_fit -> 100005
  | W_spacing n -> 100100 + int_of_float (n *. 100.)
  | W_px -> 110000
  | W_fraction _ -> 150000
  (* Min-width utilities (200000-299999) - comes after width *)
  | Min_w_0 -> 200000
  | Min_w_full -> 200001
  | Min_w_min -> 200002
  | Min_w_max -> 200003
  | Min_w_fit -> 200004
  | Min_w_spacing n -> 200100 + int_of_float (n *. 100.)
  (* Min-height utilities (300000-399999) *)
  | Min_h_0 -> 300000
  | Min_h_full -> 300001
  | Min_h_screen -> 300002
  | Min_h_spacing n -> 300100 + int_of_float (n *. 100.)
  (* Max-width utilities (400000-499999) - comes after min-* *)
  | Max_w_none -> 400000
  | Max_w_xs -> 400001
  | Max_w_sm -> 400002
  | Max_w_md -> 400003
  | Max_w_lg -> 400004
  | Max_w_xl -> 400005
  | Max_w_2xl -> 400006
  | Max_w_3xl -> 400007
  | Max_w_4xl -> 400008
  | Max_w_5xl -> 400009
  | Max_w_6xl -> 400010
  | Max_w_7xl -> 400011
  | Max_w_full -> 400012
  | Max_w_min -> 400013
  | Max_w_max -> 400014
  | Max_w_fit -> 400015
  | Max_w_prose -> 400016
  | Max_w_screen_sm -> 400020
  | Max_w_screen_md -> 400021
  | Max_w_screen_lg -> 400022
  | Max_w_screen_xl -> 400023
  | Max_w_screen_2xl -> 400024
  | Max_w_spacing n -> 400100 + int_of_float (n *. 100.)
  (* Max-height utilities (500000-599999) *)
  | Max_h_none -> 500000
  | Max_h_full -> 500001
  | Max_h_screen -> 500002
  | Max_h_min -> 500003
  | Max_h_max -> 500004
  | Max_h_fit -> 500005
  | Max_h_spacing n -> 500100 + int_of_float (n *. 100.)
  (* Size utilities (600000-699999) *)
  | Size_auto -> 600000
  | Size_full -> 600001
  | Size_min -> 600002
  | Size_max -> 600003
  | Size_fit -> 600004
  | Size_spacing n -> 600100 + int_of_float (n *. 100.)
  | Size_fraction _ -> 650000
  (* Aspect utilities (700000-) *)
  | Aspect_auto -> 700000
  | Aspect_square -> 700001
  | Aspect_video -> 700002
