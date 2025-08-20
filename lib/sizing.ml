(** Sizing utilities for width and height *)

open Core
open Css

(** {1 Width Utilities} *)

let w size =
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

(** {1 Height Utilities} *)

let h size =
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

(** {1 Min Width Utilities} *)

let min_w size =
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

(** {1 Max Width Utilities} *)

let max_w size =
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

(** {1 Min Height Utilities} *)

let min_h size =
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

(** {1 Max Height Utilities} *)

let max_h size =
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
