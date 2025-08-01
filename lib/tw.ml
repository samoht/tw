(** A type-safe, ergonomic DSL for Tailwind CSS using nominal types. *)

open Css

(** A Tailwind utility modifier *)
type modifier =
  | Hover
  | Focus
  | Active
  | Disabled
  | Group_hover
  | Dark
  | Responsive of string
  | Peer_hover
  | Peer_focus
  | Peer_checked
  | Group_focus
  | Aria_checked
  | Aria_expanded
  | Aria_selected
  | Aria_disabled
  | Data_state of string (* data-state="value" *)
  | Data_variant of string (* data-variant="value" *)
  | Data_active (* data-active="true" or data-active *)
  | Data_inactive (* data-inactive="true" or data-inactive *)
  | Data_custom of string * string (* data-{key}="{value}" *)

(** A Tailwind utility class with its name and CSS properties *)
type t =
  | Style of string * Css.property list
  | Prose of Prose.t
  | Modified of modifier * t
  | Group of t list

(** {1 Core Types} *)

(* Abstract color type *)
type color =
  | Black
  | White
  | Gray
  | Slate
  | Zinc
  | Red
  | Orange
  | Amber
  | Yellow
  | Lime
  | Green
  | Emerald
  | Teal
  | Cyan
  | Sky
  | Blue
  | Indigo
  | Violet
  | Purple
  | Fuchsia
  | Pink
  | Rose

(* Common size variants used across multiple utilities *)
type size = [ `None | `Xs | `Sm | `Md | `Lg | `Xl | `Xl_2 | `Xl_3 | `Full ]

(* Polymorphic variant types for composable sizing *)
type spacing = [ `Px | `Full | `Val of float ]
type margin = [ spacing | `Auto ]
type scale = [ spacing | size | `Screen | `Min | `Max | `Fit ]
type max_scale = [ scale | `Xl_4 | `Xl_5 | `Xl_6 | `Xl_7 ]
type shadow = [ size | `Inner ]

(* Re-export CSS module *)
module Css = Css

(** {1 Helper Functions} *)

(** Convert hex color to rgb format *)
let hex_to_rgb hex =
  let hex = if String.get hex 0 = '#' then String.sub hex 1 6 else hex in
  let r = int_of_string ("0x" ^ String.sub hex 0 2) in
  let g = int_of_string ("0x" ^ String.sub hex 2 2) in
  let b = int_of_string ("0x" ^ String.sub hex 4 2) in
  Pp.str [ string_of_int r; " "; string_of_int g; " "; string_of_int b ]

let color_to_hex color shade =
  match (color, shade) with
  (* Basic colors *)
  | Black, _ -> "#000000"
  | White, _ -> "#ffffff"
  | Gray, 50 -> "#f9fafb"
  | Gray, 100 -> "#f3f4f6"
  | Gray, 200 -> "#e5e7eb"
  | Gray, 300 -> "#d1d5db"
  | Gray, 400 -> "#9ca3af"
  | Gray, 500 -> "#6b7280"
  | Gray, 600 -> "#4b5563"
  | Gray, 700 -> "#374151"
  | Gray, 800 -> "#1f2937"
  | Gray, 900 -> "#111827"
  (* Extended color palette *)
  | Slate, 50 -> "#f8fafc"
  | Slate, 100 -> "#f1f5f9"
  | Slate, 200 -> "#e2e8f0"
  | Slate, 300 -> "#cbd5e1"
  | Slate, 400 -> "#94a3b8"
  | Slate, 500 -> "#64748b"
  | Slate, 600 -> "#475569"
  | Slate, 700 -> "#334155"
  | Slate, 800 -> "#1e293b"
  | Slate, 900 -> "#0f172a"
  | Zinc, 50 -> "#fafafa"
  | Zinc, 100 -> "#f4f4f5"
  | Zinc, 200 -> "#e4e4e7"
  | Zinc, 300 -> "#d4d4d8"
  | Zinc, 400 -> "#a1a1aa"
  | Zinc, 500 -> "#71717a"
  | Zinc, 600 -> "#52525b"
  | Zinc, 700 -> "#3f3f46"
  | Zinc, 800 -> "#27272a"
  | Zinc, 900 -> "#18181b"
  | Red, 50 -> "#fef2f2"
  | Red, 100 -> "#fee2e2"
  | Red, 200 -> "#fecaca"
  | Red, 300 -> "#fca5a5"
  | Red, 400 -> "#f87171"
  | Red, 500 -> "#ef4444"
  | Red, 600 -> "#dc2626"
  | Red, 700 -> "#b91c1c"
  | Red, 800 -> "#991b1b"
  | Red, 900 -> "#7f1d1d"
  | Orange, 50 -> "#fff7ed"
  | Orange, 100 -> "#ffedd5"
  | Orange, 200 -> "#fed7aa"
  | Orange, 300 -> "#fdba74"
  | Orange, 400 -> "#fb923c"
  | Orange, 500 -> "#f97316"
  | Orange, 600 -> "#ea580c"
  | Orange, 700 -> "#c2410c"
  | Orange, 800 -> "#9a3412"
  | Orange, 900 -> "#7c2d12"
  | Amber, 50 -> "#fffbeb"
  | Amber, 100 -> "#fef3c7"
  | Amber, 200 -> "#fde68a"
  | Amber, 300 -> "#fcd34d"
  | Amber, 400 -> "#fbbf24"
  | Amber, 500 -> "#f59e0b"
  | Amber, 600 -> "#d97706"
  | Amber, 700 -> "#b45309"
  | Amber, 800 -> "#92400e"
  | Amber, 900 -> "#78350f"
  | Yellow, 50 -> "#fefce8"
  | Yellow, 100 -> "#fef9c3"
  | Yellow, 200 -> "#fef08a"
  | Yellow, 300 -> "#fde047"
  | Yellow, 400 -> "#facc15"
  | Yellow, 500 -> "#eab308"
  | Yellow, 600 -> "#ca8a04"
  | Yellow, 700 -> "#a16207"
  | Yellow, 800 -> "#854d0e"
  | Yellow, 900 -> "#713f12"
  | Lime, 50 -> "#f7fee7"
  | Lime, 100 -> "#ecfccb"
  | Lime, 200 -> "#d9f99d"
  | Lime, 300 -> "#bef264"
  | Lime, 400 -> "#a3e635"
  | Lime, 500 -> "#84cc16"
  | Lime, 600 -> "#65a30d"
  | Lime, 700 -> "#4d7c0f"
  | Lime, 800 -> "#365314"
  | Lime, 900 -> "#1a2e05"
  | Green, 50 -> "#f0fdf4"
  | Green, 100 -> "#dcfce7"
  | Green, 200 -> "#bbf7d0"
  | Green, 300 -> "#86efac"
  | Green, 400 -> "#4ade80"
  | Green, 500 -> "#22c55e"
  | Green, 600 -> "#16a34a"
  | Green, 700 -> "#15803d"
  | Green, 800 -> "#166534"
  | Green, 900 -> "#14532d"
  | Emerald, 50 -> "#ecfdf5"
  | Emerald, 100 -> "#d1fae5"
  | Emerald, 200 -> "#a7f3d0"
  | Emerald, 300 -> "#6ee7b7"
  | Emerald, 400 -> "#34d399"
  | Emerald, 500 -> "#10b981"
  | Emerald, 600 -> "#059669"
  | Emerald, 700 -> "#047857"
  | Emerald, 800 -> "#065f46"
  | Emerald, 900 -> "#064e3b"
  | Teal, 50 -> "#f0fdfa"
  | Teal, 100 -> "#ccfbf1"
  | Teal, 200 -> "#99f6e4"
  | Teal, 300 -> "#5eead4"
  | Teal, 400 -> "#2dd4bf"
  | Teal, 500 -> "#14b8a6"
  | Teal, 600 -> "#0d9488"
  | Teal, 700 -> "#0f766e"
  | Teal, 800 -> "#115e59"
  | Teal, 900 -> "#134e4a"
  | Cyan, 50 -> "#ecfeff"
  | Cyan, 100 -> "#cffafe"
  | Cyan, 200 -> "#a5f3fc"
  | Cyan, 300 -> "#67e8f9"
  | Cyan, 400 -> "#22d3ee"
  | Cyan, 500 -> "#06b6d4"
  | Cyan, 600 -> "#0891b2"
  | Cyan, 700 -> "#0e7490"
  | Cyan, 800 -> "#155e75"
  | Cyan, 900 -> "#164e63"
  | Sky, 50 -> "#f0f9ff"
  | Sky, 100 -> "#e0f2fe"
  | Sky, 200 -> "#bae6fd"
  | Sky, 300 -> "#7dd3fc"
  | Sky, 400 -> "#38bdf8"
  | Sky, 500 -> "#0ea5e9"
  | Sky, 600 -> "#0284c7"
  | Sky, 700 -> "#0369a1"
  | Sky, 800 -> "#075985"
  | Sky, 900 -> "#0c4a6e"
  | Blue, 50 -> "#eff6ff"
  | Blue, 100 -> "#dbeafe"
  | Blue, 200 -> "#bfdbfe"
  | Blue, 300 -> "#93c5fd"
  | Blue, 400 -> "#60a5fa"
  | Blue, 500 -> "#3b82f6"
  | Blue, 600 -> "#2563eb"
  | Blue, 700 -> "#1d4ed8"
  | Blue, 800 -> "#1e40af"
  | Blue, 900 -> "#1e3a8a"
  | Indigo, 50 -> "#eef2ff"
  | Indigo, 100 -> "#e0e7ff"
  | Indigo, 200 -> "#c7d2fe"
  | Indigo, 300 -> "#a5b4fc"
  | Indigo, 400 -> "#818cf8"
  | Indigo, 500 -> "#6366f1"
  | Indigo, 600 -> "#4f46e5"
  | Indigo, 700 -> "#4338ca"
  | Indigo, 800 -> "#3730a3"
  | Indigo, 900 -> "#312e81"
  | Violet, 50 -> "#f5f3ff"
  | Violet, 100 -> "#ede9fe"
  | Violet, 200 -> "#ddd6fe"
  | Violet, 300 -> "#c4b5fd"
  | Violet, 400 -> "#a78bfa"
  | Violet, 500 -> "#8b5cf6"
  | Violet, 600 -> "#7c3aed"
  | Violet, 700 -> "#6d28d9"
  | Violet, 800 -> "#5b21b6"
  | Violet, 900 -> "#4c1d95"
  | Purple, 50 -> "#faf5ff"
  | Purple, 100 -> "#f3e8ff"
  | Purple, 200 -> "#e9d5ff"
  | Purple, 300 -> "#d8b4fe"
  | Purple, 400 -> "#c084fc"
  | Purple, 500 -> "#a855f7"
  | Purple, 600 -> "#9333ea"
  | Purple, 700 -> "#7e22ce"
  | Purple, 800 -> "#6b21a8"
  | Purple, 900 -> "#581c87"
  | Fuchsia, 50 -> "#fdf4ff"
  | Fuchsia, 100 -> "#fae8ff"
  | Fuchsia, 200 -> "#f5d0fe"
  | Fuchsia, 300 -> "#f0abfc"
  | Fuchsia, 400 -> "#e879f9"
  | Fuchsia, 500 -> "#d946ef"
  | Fuchsia, 600 -> "#c026d3"
  | Fuchsia, 700 -> "#a21caf"
  | Fuchsia, 800 -> "#86198f"
  | Fuchsia, 900 -> "#701a75"
  | Pink, 50 -> "#fdf2f8"
  | Pink, 100 -> "#fce7f3"
  | Pink, 200 -> "#fbcfe8"
  | Pink, 300 -> "#f9a8d4"
  | Pink, 400 -> "#f472b6"
  | Pink, 500 -> "#ec4899"
  | Pink, 600 -> "#db2777"
  | Pink, 700 -> "#be185d"
  | Pink, 800 -> "#9d174d"
  | Pink, 900 -> "#831843"
  | Rose, 50 -> "#fff1f2"
  | Rose, 100 -> "#ffe4e6"
  | Rose, 200 -> "#fecdd3"
  | Rose, 300 -> "#fda4af"
  | Rose, 400 -> "#fb7185"
  | Rose, 500 -> "#f43f5e"
  | Rose, 600 -> "#e11d48"
  | Rose, 700 -> "#be123c"
  | Rose, 800 -> "#9f1239"
  | Rose, 900 -> "#881337"
  | color, shade ->
      let color_name =
        match color with
        | Black -> "Black"
        | White -> "White"
        | Gray -> "Gray"
        | Slate -> "Slate"
        | Zinc -> "Zinc"
        | Red -> "Red"
        | Orange -> "Orange"
        | Amber -> "Amber"
        | Yellow -> "Yellow"
        | Lime -> "Lime"
        | Green -> "Green"
        | Emerald -> "Emerald"
        | Teal -> "Teal"
        | Cyan -> "Cyan"
        | Sky -> "Sky"
        | Blue -> "Blue"
        | Indigo -> "Indigo"
        | Violet -> "Violet"
        | Purple -> "Purple"
        | Fuchsia -> "Fuchsia"
        | Pink -> "Pink"
        | Rose -> "Rose"
      in
      let err_unknown_color color shade =
        Pp.str
          [ "Unknown color combination: "; color; " "; string_of_int shade ]
      in
      failwith (err_unknown_color color_name shade)

let spacing_to_rem = function
  | 0 -> "0"
  | 1 -> "0.25rem"
  | 2 -> "0.5rem"
  | 3 -> "0.75rem"
  | 4 -> "1rem"
  | 6 -> "1.5rem"
  | 8 -> "2rem"
  | 10 -> "2.5rem"
  | 12 -> "3rem"
  | 16 -> "4rem"
  | 20 -> "5rem"
  | 24 -> "6rem"
  | 56 -> "14rem"
  | n -> string_of_int n ^ "rem"

(** {1 Public API} *)

(** {1 Colors} *)

let color_name = function
  | Black -> "black"
  | White -> "white"
  | Gray -> "gray"
  | Slate -> "slate"
  | Zinc -> "zinc"
  | Red -> "red"
  | Orange -> "orange"
  | Amber -> "amber"
  | Yellow -> "yellow"
  | Lime -> "lime"
  | Green -> "green"
  | Emerald -> "emerald"
  | Teal -> "teal"
  | Cyan -> "cyan"
  | Sky -> "sky"
  | Blue -> "blue"
  | Indigo -> "indigo"
  | Violet -> "violet"
  | Purple -> "purple"
  | Fuchsia -> "fuchsia"
  | Pink -> "pink"
  | Rose -> "rose"

(* Color constructors *)
let black = Black
let white = White
let gray = Gray
let slate = Slate
let zinc = Zinc
let red = Red
let orange = Orange
let amber = Amber
let yellow = Yellow
let lime = Lime
let green = Green
let emerald = Emerald
let teal = Teal
let cyan = Cyan
let sky = Sky
let blue = Blue
let indigo = Indigo
let violet = Violet
let purple = Purple
let fuchsia = Fuchsia
let pink = Pink
let rose = Rose

(* Value constructors *)

(* Spacing constructors *)
let int n = `Val (float_of_int n *. 0.25)
let one_px = `Px
let full = `Full
let rem f = `Val f

(* Margin constructors *)
let auto = `Auto

(* Size constructors *)
let screen = `Screen
let min = `Min
let max = `Max
let fit = `Fit

(* Max width constructors *)
let none = `None
let xs = `Xs
let sm = `Sm
let md = `Md
let lg = `Lg
let xl = `Xl
let xl_2 = `Xl_2
let xl_3 = `Xl_3
let xl_4 = `Xl_4
let xl_5 = `Xl_5
let xl_6 = `Xl_6
let xl_7 = `Xl_7

(* Value constructors *)
let inner = `Inner

let bg ?(shade = 500) color =
  let class_name =
    match color with
    | Black | White -> Pp.str [ "bg-"; color_name color ]
    | _ -> Pp.str [ "bg-"; color_name color; "-"; string_of_int shade ]
  in
  let hex = color_to_hex color shade in
  let rgb = hex_to_rgb hex in
  Style
    ( class_name,
      [
        property "--tw-bg-opacity" "1";
        background_color (Pp.str [ "rgb("; rgb; " / var(--tw-bg-opacity))" ]);
      ] )

let bg_transparent = Style ("bg-transparent", [ background_color "transparent" ])
let bg_current = Style ("bg-current", [ background_color "currentColor" ])

let text ?(shade = 500) color =
  let class_name =
    match color with
    | Black | White -> Pp.str [ "text-"; color_name color ]
    | _ -> Pp.str [ "text-"; color_name color; "-"; string_of_int shade ]
  in
  let hex = color_to_hex color shade in
  let rgb = hex_to_rgb hex in
  Style
    ( class_name,
      [
        property "--tw-text-opacity" "1";
        Css.color (Pp.str [ "rgb("; rgb; " / var(--tw-text-opacity))" ]);
      ] )

let text_transparent = Style ("text-transparent", [ Css.color "transparent" ])
let text_current = Style ("text-current", [ Css.color "currentColor" ])

let border_color ?(shade = 500) color =
  let class_name =
    match color with
    | Black | White -> Pp.str [ "border-"; color_name color ]
    | _ -> Pp.str [ "border-"; color_name color; "-"; string_of_int shade ]
  in
  let hex = color_to_hex color shade in
  let rgb = hex_to_rgb hex in
  Style
    ( class_name,
      [
        property "--tw-border-opacity" "1";
        Css.border_color
          (Pp.str [ "rgb("; rgb; " / var(--tw-border-opacity))" ]);
      ] )

let border_transparent =
  Style ("border-transparent", [ Css.border_color "transparent" ])

let border_current =
  Style ("border-current", [ Css.border_color "currentColor" ])

let pp_spacing_suffix : spacing -> string = function
  | `Px -> "px"
  | `Full -> "full"
  | `Val f ->
      (* Convert rem values back to Tailwind scale *)
      let n = int_of_float (f /. 0.25) in
      string_of_int n

let pp_size_suffix : size -> string = function
  | `None -> "none"
  | `Xs -> "xs"
  | `Sm -> "sm"
  | `Md -> "md"
  | `Lg -> "lg"
  | `Xl -> "xl"
  | `Xl_2 -> "2xl"
  | `Xl_3 -> "3xl"
  | `Full -> "full"

let pp_scale_suffix : scale -> string = function
  | `Screen -> "screen"
  | `Min -> "min"
  | `Max -> "max"
  | `Fit -> "fit"
  | #spacing as s -> pp_spacing_suffix s
  | #size as s -> pp_size_suffix s

let pp_margin_suffix : margin -> string = function
  | `Auto -> "auto"
  | #spacing as s -> pp_spacing_suffix s

let pp_max_scale_suffix : max_scale -> string = function
  | #size as s -> pp_size_suffix s
  | `Xl_4 -> "4xl"
  | `Xl_5 -> "5xl"
  | `Xl_6 -> "6xl"
  | `Xl_7 -> "7xl"
  | #scale as s -> pp_scale_suffix s

(** Format float for CSS - ensures leading zero and removes trailing dot *)
let css_float f =
  let s = string_of_float f in
  let s = if String.starts_with ~prefix:"." s then "0" ^ s else s in
  if String.ends_with ~suffix:"." s then String.sub s 0 (String.length s - 1)
  else s

let pp_spacing : spacing -> string = function
  | `Px -> "1px"
  | `Full -> "100%"
  | `Val f -> if f = 0.0 then "0" else Pp.str [ css_float f; "rem" ]

let pp_margin : margin -> string = function
  | `Auto -> "auto"
  | #spacing as s -> pp_spacing s

let pp_size : size -> string = function
  | `None -> "0"
  | `Xs -> "0.125rem"
  | `Sm -> "0.25rem"
  | `Md -> "0.375rem"
  | `Lg -> "0.5rem"
  | `Xl -> "0.75rem"
  | `Xl_2 -> "1rem"
  | `Xl_3 -> "1.5rem"
  | `Full -> "100%"

let pp_scale : scale -> string = function
  | `Screen -> "100vh"
  | `Min -> "min-content"
  | `Max -> "max-content"
  | `Fit -> "fit-content"
  | #spacing as s -> pp_spacing s
  | #size as s -> pp_size s

let pp_max_scale : max_scale -> string = function
  | `None -> "none"
  | `Xs -> "20rem"
  | `Sm -> "24rem"
  | `Md -> "28rem"
  | `Lg -> "32rem"
  | `Xl -> "36rem"
  | `Xl_2 -> "42rem"
  | `Xl_3 -> "48rem"
  | `Xl_4 -> "56rem"
  | `Xl_5 -> "64rem"
  | `Xl_6 -> "72rem"
  | `Xl_7 -> "80rem"
  | #scale as s -> pp_scale s

(** {1 Spacing} *)

let p s =
  let class_name = "p-" ^ pp_spacing_suffix s in
  Style (class_name, [ padding (pp_spacing s) ])

let px s =
  let v = pp_spacing s in
  let class_name = "px-" ^ pp_spacing_suffix s in
  Style (class_name, [ padding_left v; padding_right v ])

let py s =
  let v = pp_spacing s in
  let class_name = "py-" ^ pp_spacing_suffix s in
  Style (class_name, [ padding_bottom v; padding_top v ])

let pt s =
  let class_name = "pt-" ^ pp_spacing_suffix s in
  Style (class_name, [ padding_top (pp_spacing s) ])

let pr s =
  let class_name = "pr-" ^ pp_spacing_suffix s in
  Style (class_name, [ padding_right (pp_spacing s) ])

let pb s =
  let class_name = "pb-" ^ pp_spacing_suffix s in
  Style (class_name, [ padding_bottom (pp_spacing s) ])

let pl s =
  let class_name = "pl-" ^ pp_spacing_suffix s in
  Style (class_name, [ padding_left (pp_spacing s) ])

let m m =
  let class_name = "m-" ^ pp_margin_suffix m in
  Style (class_name, [ margin (pp_margin m) ])

let mx m =
  let v = pp_margin m in
  let class_name = "mx-" ^ pp_margin_suffix m in
  Style (class_name, [ margin_left v; margin_right v ])

let my m =
  let v = pp_margin m in
  let class_name = "my-" ^ pp_margin_suffix m in
  Style (class_name, [ margin_bottom v; margin_top v ])

let mt m =
  let class_name = "mt-" ^ pp_margin_suffix m in
  Style (class_name, [ margin_top (pp_margin m) ])

let mr m =
  let class_name = "mr-" ^ pp_margin_suffix m in
  Style (class_name, [ margin_right (pp_margin m) ])

let mb m =
  let class_name = "mb-" ^ pp_margin_suffix m in
  Style (class_name, [ margin_bottom (pp_margin m) ])

let ml m =
  let class_name = "ml-" ^ pp_margin_suffix m in
  Style (class_name, [ margin_left (pp_margin m) ])

let gap s =
  let class_name = "gap-" ^ pp_spacing_suffix s in
  Style (class_name, [ gap (pp_spacing s) ])

let gap_x s =
  let class_name = "gap-x-" ^ pp_spacing_suffix s in
  Style (class_name, [ column_gap (pp_spacing s) ])

let gap_y s =
  let class_name = "gap-y-" ^ pp_spacing_suffix s in
  Style (class_name, [ row_gap (pp_spacing s) ])

(** {1 Sizing} *)

let w (s : scale) =
  let class_name = "w-" ^ pp_scale_suffix s in
  Style (class_name, [ width (pp_scale s) ])

let h (s : scale) =
  let class_name = "h-" ^ pp_scale_suffix s in
  match s with
  | `Screen -> Style (class_name, [ height "100vh" ])
  | _ -> Style (class_name, [ height (pp_scale s) ])

let min_w (s : scale) =
  let class_name = "min-w-" ^ pp_scale_suffix s in
  Style (class_name, [ min_width (pp_scale s) ])

let min_h (s : scale) =
  let class_name = "min-h-" ^ pp_scale_suffix s in
  match s with
  | `Screen -> Style (class_name, [ min_height "100vh" ])
  | _ -> Style (class_name, [ min_height (pp_scale s) ])

let max_w (mw : max_scale) =
  let class_name = "max-w-" ^ pp_max_scale_suffix mw in
  Style (class_name, [ max_width (pp_max_scale mw) ])

(** {1 Typography} *)

let text_xs = Style ("text-xs", [ font_size "0.75rem"; line_height "1rem" ])
let text_sm = Style ("text-sm", [ font_size "0.875rem"; line_height "1.25rem" ])
let text_xl = Style ("text-xl", [ font_size "1.25rem"; line_height "1.75rem" ])
let text_2xl = Style ("text-2xl", [ font_size "1.5rem"; line_height "2rem" ])
let text_center = Style ("text-center", [ text_align "center" ])

(** {1 Layout} *)

let block = Style ("block", [ display "block" ])
let inline = Style ("inline", [ display "inline" ])
let inline_block = Style ("inline-block", [ display "inline-block" ])
let hidden = Style ("hidden", [ display "none" ])

(** {1 Flexbox} *)

let flex = Style ("flex", [ display "flex" ])
let flex_shrink_0 = Style ("flex-shrink-0", [ Css.flex_shrink "0" ])
let flex_col = Style ("flex-col", [ flex_direction "column" ])
let flex_row = Style ("flex-row", [ flex_direction "row" ])
let flex_wrap = Style ("flex-wrap", [ Css.flex_wrap "wrap" ])

let flex_row_reverse =
  Style ("flex-row-reverse", [ flex_direction "row-reverse" ])

let flex_col_reverse =
  Style ("flex-col-reverse", [ flex_direction "column-reverse" ])

let flex_wrap_reverse =
  Style ("flex-wrap-reverse", [ Css.flex_wrap "wrap-reverse" ])

let flex_nowrap = Style ("flex-nowrap", [ Css.flex_wrap "nowrap" ])
let flex_1 = Style ("flex-1", [ Css.flex "1 1 0%" ])
let flex_auto = Style ("flex-auto", [ Css.flex "1 1 auto" ])
let flex_initial = Style ("flex-initial", [ Css.flex "0 1 auto" ])
let flex_none = Style ("flex-none", [ Css.flex "none" ])
let flex_grow = Style ("flex-grow", [ Css.flex_grow "1" ])
let flex_grow_0 = Style ("flex-grow-0", [ Css.flex_grow "0" ])
let flex_shrink = Style ("flex-shrink", [ Css.flex_shrink "1" ])

(* center *)

let items_center = Style ("items-center", [ align_items "center" ])

let justify_between =
  Style ("justify-between", [ justify_content "space-between" ])

(** {1 Positioning} *)

let relative = Style ("relative", [ position "relative" ])
let absolute = Style ("absolute", [ position "absolute" ])
let fixed = Style ("fixed", [ position "fixed" ])
let sticky = Style ("sticky", [ position "sticky" ])

(* Borders *)

(* Modifiers *)

(** {1 CSS Generation} *)

(* Internal helper to extract CSS properties from a style *)
let rec to_css_properties = function
  | Style (_class_name, props) -> props
  | Prose variant ->
      (* For inline styles, we can only use the base prose properties, not the
         descendant selectors like .prose h1 *)
      Prose.to_base_properties variant
  | Modified (_modifier, t) -> to_css_properties t
  | Group styles -> List.concat_map to_css_properties styles

(* Helper to get breakpoint for responsive prefix *)
let responsive_breakpoint = function
  | "sm" -> "640px"
  | "md" -> "768px"
  | "lg" -> "1024px"
  | "xl" -> "1280px"
  | "2xl" -> "1536px"
  | _ -> "0px"

(* Extract selector and properties from a single Tw style *)
let extract_selector_props tw =
  let rec extract = function
    | Style (class_name, props) -> [ ("." ^ class_name, props) ]
    | Prose variant ->
        (* Convert prose rules to selector/props pairs *)
        Prose.to_css_rules variant
        |> List.map (fun rule -> (Css.selector rule, Css.properties rule))
    | Modified (modifier, t) ->
        let base = extract t in
        List.map
          (fun (selector, props) ->
            match modifier with
            | Hover -> (selector ^ ":hover", props)
            | Focus -> (selector ^ ":focus", props)
            | Active -> (selector ^ ":active", props)
            | Disabled -> (selector ^ ":disabled", props)
            | Group_hover -> (".group:hover " ^ selector, props)
            | Group_focus -> (".group:focus " ^ selector, props)
            | Peer_hover -> (".peer:hover ~ " ^ selector, props)
            | Peer_focus -> (".peer:focus ~ " ^ selector, props)
            | Peer_checked -> (".peer:checked ~ " ^ selector, props)
            | Aria_checked -> (selector ^ "[aria-checked=\"true\"]", props)
            | Aria_expanded -> (selector ^ "[aria-expanded=\"true\"]", props)
            | Aria_selected -> (selector ^ "[aria-selected=\"true\"]", props)
            | Aria_disabled -> (selector ^ "[aria-disabled=\"true\"]", props)
            | Data_state value ->
                (selector ^ "[data-state=\"" ^ value ^ "\"]", props)
            | Data_variant value ->
                (selector ^ "[data-variant=\"" ^ value ^ "\"]", props)
            | Data_active -> (selector ^ "[data-active]", props)
            | Data_inactive -> (selector ^ "[data-inactive]", props)
            | Data_custom (key, value) ->
                (selector ^ "[data-" ^ key ^ "=\"" ^ value ^ "\"]", props)
            | Dark ->
                ("@media (prefers-color-scheme: dark) { " ^ selector, props)
            | Responsive prefix ->
                ( "@media (min-width: "
                  ^ responsive_breakpoint prefix
                  ^ ") { " ^ selector,
                  props ))
          base
    | Group styles -> List.concat_map extract styles
  in
  extract tw

(* Group properties by selector *)
let group_by_selector rules =
  List.fold_left
    (fun acc (selector, props) ->
      let existing = try List.assoc selector acc with Not_found -> [] in
      (selector, existing @ props) :: List.remove_assoc selector acc)
    [] rules

(* Base reset CSS rules *)
let reset_rules =
  [
    rule ~selector:"*" [ margin "0"; padding "0"; box_sizing "border-box" ];
    rule ~selector:"body"
      [
        font_size "16px";
        line_height "1.5";
        color "#374151";
        font_family
          "-apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, 'Helvetica \
           Neue', Arial, sans-serif";
      ];
  ]

(* Check if prose styles are being used *)
let uses_prose tw_classes =
  let rec check = function
    | Style (_, _) -> false
    | Prose _ -> true
    | Modified (_, t) -> check t
    | Group styles -> List.exists check styles
  in
  List.exists check tw_classes

(* Generate CSS rules for all used Tw classes *)
let to_css ?(reset = true) tw_classes =
  let all_rules =
    tw_classes |> List.concat_map extract_selector_props |> group_by_selector
  in
  let rules =
    List.map
      (fun (selector, props) -> rule ~selector (deduplicate_properties props))
      all_rules
  in
  let final_rules =
    if reset then
      (* Include prose CSS variables in reset if prose is being used *)
      let prose_reset =
        if uses_prose tw_classes then
          [ rule ~selector:":root" Prose.css_variables ]
        else []
      in
      reset_rules @ prose_reset @ rules
    else rules
  in
  stylesheet final_rules

let stylesheet_to_string ?minify stylesheet = Css.to_string ?minify stylesheet

let neg_mt s =
  let class_name = "-mt-" ^ pp_spacing_suffix s in
  Style (class_name, [ margin_top ("-" ^ pp_spacing s) ])

let neg_mr s =
  let class_name = "-mr-" ^ pp_spacing_suffix s in
  Style (class_name, [ margin_right ("-" ^ pp_spacing s) ])

let neg_mb s =
  let class_name = "-mb-" ^ pp_spacing_suffix s in
  Style (class_name, [ margin_bottom ("-" ^ pp_spacing s) ])

let neg_ml s =
  let class_name = "-ml-" ^ pp_spacing_suffix s in
  Style (class_name, [ margin_left ("-" ^ pp_spacing s) ])

let max_h s =
  let class_name = "max-h-" ^ pp_scale_suffix s in
  match s with
  | `Screen -> Style (class_name, [ max_height "100vh" ])
  | _ -> Style (class_name, [ max_height (pp_scale s) ])

let text_base = Style ("text-base", [ font_size "1rem"; line_height "1.5rem" ])
let text_lg = Style ("text-lg", [ font_size "1.125rem"; line_height "1.75rem" ])

let text_3xl =
  Style ("text-3xl", [ font_size "1.875rem"; line_height "2.25rem" ])

let text_4xl = Style ("text-4xl", [ font_size "2.25rem"; line_height "2.5rem" ])
let text_5xl = Style ("text-5xl", [ font_size "3rem"; line_height "1" ])
let font_thin = Style ("font-thin", [ font_weight "100" ])
let font_light = Style ("font-light", [ font_weight "300" ])
let font_normal = Style ("font-normal", [ font_weight "400" ])
let font_medium = Style ("font-medium", [ font_weight "500" ])
let font_semibold = Style ("font-semibold", [ font_weight "600" ])
let font_bold = Style ("font-bold", [ font_weight "700" ])
let font_extrabold = Style ("font-extrabold", [ font_weight "800" ])
let font_black = Style ("font-black", [ font_weight "900" ])

(* Font family utilities *)
let font_sans =
  Style
    ( "font-sans",
      [
        font_family
          "ui-sans-serif, system-ui, -apple-system, BlinkMacSystemFont, 'Segoe \
           UI', Roboto, 'Helvetica Neue', Arial, 'Noto Sans', sans-serif, \
           'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto \
           Color Emoji'";
      ] )

let font_serif =
  Style
    ( "font-serif",
      [
        font_family
          "ui-serif, Georgia, Cambria, 'Times New Roman', Times, serif";
      ] )

let font_mono =
  Style
    ( "font-mono",
      [
        font_family
          "ui-monospace, SFMono-Regular, 'SF Mono', Consolas, 'Liberation \
           Mono', Menlo, monospace";
      ] )

let italic = Style ("italic", [ font_style "italic" ])
let not_italic = Style ("not-italic", [ font_style "normal" ])
let underline = Style ("underline", [ text_decoration "underline" ])
let line_through = Style ("line-through", [ text_decoration "line-through" ])
let no_underline = Style ("no-underline", [ text_decoration "none" ])
let text_left = Style ("text-left", [ text_align "left" ])
let text_right = Style ("text-right", [ text_align "right" ])
let text_justify = Style ("text-justify", [ text_align "justify" ])
let leading_none = Style ("leading-none", [ line_height "1" ])
let leading_tight = Style ("leading-tight", [ line_height "1.25" ])
let leading_snug = Style ("leading-snug", [ line_height "1.375" ])
let leading_normal = Style ("leading-normal", [ line_height "1.5" ])
let leading_relaxed = Style ("leading-relaxed", [ line_height "1.625" ])
let leading_loose = Style ("leading-loose", [ line_height "2" ])
let leading_6 = Style ("leading-6", [ line_height "1.5rem" ])
let tracking_tighter = Style ("tracking-tighter", [ letter_spacing "-0.05em" ])
let tracking_tight = Style ("tracking-tight", [ letter_spacing "-0.025em" ])
let tracking_normal = Style ("tracking-normal", [ letter_spacing "0" ])
let tracking_wide = Style ("tracking-wide", [ letter_spacing "0.025em" ])
let tracking_wider = Style ("tracking-wider", [ letter_spacing "0.05em" ])
let tracking_widest = Style ("tracking-widest", [ letter_spacing "0.1em" ])
let whitespace_normal = Style ("whitespace-normal", [ white_space "normal" ])
let whitespace_nowrap = Style ("whitespace-nowrap", [ white_space "nowrap" ])
let whitespace_pre = Style ("whitespace-pre", [ white_space "pre" ])

let whitespace_pre_line =
  Style ("whitespace-pre-line", [ white_space "pre-line" ])

let whitespace_pre_wrap =
  Style ("whitespace-pre-wrap", [ white_space "pre-wrap" ])

let inline_flex = Style ("inline-flex", [ display "inline-flex" ])
let grid = Style ("grid", [ display "grid" ])
let inline_grid = Style ("inline-grid", [ display "inline-grid" ])
let items_start = Style ("items-start", [ align_items "flex-start" ])
let items_end = Style ("items-end", [ align_items "flex-end" ])
let items_baseline = Style ("items-baseline", [ align_items "baseline" ])
let items_stretch = Style ("items-stretch", [ align_items "stretch" ])
let justify_start = Style ("justify-start", [ justify_content "flex-start" ])
let justify_end = Style ("justify-end", [ justify_content "flex-end" ])
let justify_center = Style ("justify-center", [ justify_content "center" ])
let justify_around = Style ("justify-around", [ justify_content "space-around" ])
let justify_evenly = Style ("justify-evenly", [ justify_content "space-evenly" ])

let grid_cols n =
  let class_name = "grid-cols-" ^ string_of_int n in
  Style
    ( class_name,
      [
        grid_template_columns ("repeat(" ^ string_of_int n ^ ", minmax(0, 1fr))");
      ] )

let grid_rows n =
  let class_name = "grid-rows-" ^ string_of_int n in
  Style
    ( class_name,
      [ grid_template_rows ("repeat(" ^ string_of_int n ^ ", minmax(0, 1fr))") ]
    )

let static = Style ("static", [ position "static" ])
let inset_0 = Style ("inset-0", [ top "0"; right "0"; bottom "0"; left "0" ])
let inset_x_0 = Style ("inset-x-0", [ left "0"; right "0" ])
let inset_y_0 = Style ("inset-y-0", [ bottom "0"; top "0" ])

let top n =
  let class_name = "top-" ^ string_of_int n in
  Style (class_name, [ top (spacing_to_rem n) ])

let right n =
  let class_name = "right-" ^ string_of_int n in
  Style (class_name, [ right (spacing_to_rem n) ])

let bottom n =
  let class_name = "bottom-" ^ string_of_int n in
  Style (class_name, [ bottom (spacing_to_rem n) ])

let left n =
  let class_name = "left-" ^ string_of_int n in
  Style (class_name, [ left (spacing_to_rem n) ])

let z n =
  let class_name = "z-" ^ string_of_int n in
  Style (class_name, [ z_index (string_of_int n) ])

type width = [ size | `Default ]

let border (w : width) =
  let width_px, class_suffix =
    match w with
    | `None -> ("0", "-0")
    | `Xs | `Default -> ("1px", "" (* Default border is 1px *))
    | `Sm -> ("2px", "-2")
    | `Md -> ("4px", "-4" (* For borders, Md maps to 4px *))
    | `Lg -> ("4px", "-4")
    | `Xl -> ("8px", "-8")
    | `Xl_2 -> ("8px", "-8")
    | `Xl_3 -> ("8px", "-8")
    | `Full -> ("8px", "-8")
  in
  let class_name = "border" ^ class_suffix in
  Style (class_name, [ border_width width_px ])

let border_t = Style ("border-t", [ border_top_width "1px" ])
let border_r = Style ("border-r", [ border_right_width "1px" ])
let border_b = Style ("border-b", [ border_bottom_width "1px" ])
let border_l = Style ("border-l", [ border_left_width "1px" ])

let rounded_value : size -> string = function
  | `None -> "0"
  | `Sm -> "0.125rem"
  | `Md -> "0.375rem"
  | `Lg -> "0.5rem"
  | `Xl -> "0.75rem"
  | `Xl_2 -> "1rem"
  | `Xl_3 -> "1.5rem"
  | `Full -> "9999px"
  | `Xs -> "0.0625rem"

let pp_rounded_suffix : size -> string = function
  | `None -> "none"
  | `Sm -> "sm"
  | `Md -> "md"
  | `Lg -> "lg"
  | `Xl -> "xl"
  | `Xl_2 -> "2xl"
  | `Xl_3 -> "3xl"
  | `Full -> "full"
  | `Xs -> "xs"

let rounded r =
  let class_name = "rounded-" ^ pp_rounded_suffix r in
  Style (class_name, [ border_radius (rounded_value r) ])

let shadow_value : shadow -> string = function
  | `None -> "0 0 #0000"
  | `Sm -> "0 1px 2px 0 #0000000d" (* rgba(0,0,0,0.05) = #0000000d *)
  | `Md ->
      "0 4px 6px -1px #0000001a,0 2px 4px -2px #0000000f" (* 0.1 and 0.06 *)
  | `Lg ->
      "0 10px 15px -3px #0000001a,0 4px 6px -4px #0000000d" (* 0.1 and 0.05 *)
  | `Xl ->
      "0 20px 25px -5px #0000001a,0 8px 10px -6px #0000000a" (* 0.1 and 0.04 *)
  | `Xl_2 -> "0 25px 50px -12px #00000040" (* 0.25 *)
  | `Inner -> "inset 0 2px 4px 0 #0000000f" (* 0.06 *)
  | `Xs -> "0 1px 1px 0 #0000000a" (* extra small *)
  | `Xl_3 -> "0 35px 60px -15px #00000059" (* extra extra large *)
  | `Full -> "0 0 0 0 #0000" (* no shadow, same as none *)

let pp_shadow_suffix : shadow -> string = function
  | `None -> "none"
  | `Sm -> "sm"
  | `Md -> "md"
  | `Lg -> "lg"
  | `Xl -> "xl"
  | `Xl_2 -> "2xl"
  | `Inner -> "inner"
  | `Xs -> "xs"
  | `Xl_3 -> "3xl"
  | `Full -> "full"

let shadow_colored_value : shadow -> string = function
  | `None -> "0 0 #0000"
  | `Sm -> "0 1px 2px 0 var(--tw-shadow-color)"
  | `Md ->
      "0 4px 6px -1px var(--tw-shadow-color),0 2px 4px -2px \
       var(--tw-shadow-color)"
  | `Lg ->
      "0 10px 15px -3px var(--tw-shadow-color),0 4px 6px -4px \
       var(--tw-shadow-color)"
  | `Xl ->
      "0 20px 25px -5px var(--tw-shadow-color),0 8px 10px -6px \
       var(--tw-shadow-color)"
  | `Xl_2 -> "0 25px 50px -12px var(--tw-shadow-color)"
  | `Inner -> "inset 0 2px 4px 0 var(--tw-shadow-color)"
  | `Xs -> "0 1px 1px 0 var(--tw-shadow-color)"
  | `Xl_3 -> "0 35px 60px -15px var(--tw-shadow-color)"
  | `Full -> "0 0 #0000"

let shadow s =
  let class_name = "shadow-" ^ pp_shadow_suffix s in
  (* For modern Tailwind, shadows use CSS custom properties *)
  (* Note: Tailwind will automatically merge the box-shadow property for classes that share it *)
  let custom_props =
    [
      property "--tw-shadow" (shadow_value s);
      property "--tw-shadow-colored" (shadow_colored_value s);
    ]
  in
  let box_shadow_prop =
    box_shadow
      "var(--tw-ring-offset-shadow,0 0 #0000),var(--tw-ring-shadow,0 0 \
       #0000),var(--tw-shadow)"
  in
  Style (class_name, custom_props @ [ box_shadow_prop ])

let opacity n =
  let class_name = "opacity-" ^ string_of_int n in
  let value =
    if n = 0 then "0"
    else if n = 100 then "1"
    else string_of_float (float_of_int n /. 100.0)
  in
  Style (class_name, [ opacity value ])

let transition_none = Style ("transition-none", [ transition "none" ])

let transition_all =
  Style
    ("transition-all", [ transition "all 150ms cubic-bezier(0.4, 0, 0.2, 1)" ])

let transition_colors =
  Style
    ( "transition-colors",
      [
        transition
          "background-color, border-color, color, fill, stroke 150ms \
           cubic-bezier(0.4, 0, 0.2, 1)";
      ] )

let transition_opacity =
  Style
    ( "transition-opacity",
      [ transition "opacity 150ms cubic-bezier(0.4, 0, 0.2, 1)" ] )

let transition_shadow =
  Style
    ( "transition-shadow",
      [ transition "box-shadow 150ms cubic-bezier(0.4, 0, 0.2, 1)" ] )

let transition_transform =
  Style
    ( "transition-transform",
      [ transition "transform 150ms cubic-bezier(0.4, 0, 0.2, 1)" ] )

let rotate n =
  let class_name = "rotate-" ^ string_of_int n in
  Style (class_name, [ transform ("rotate(" ^ string_of_int n ^ "deg)") ])

let translate_x n =
  let class_name = "translate-x-" ^ string_of_int n in
  Style (class_name, [ transform ("translateX(" ^ spacing_to_rem n ^ ")") ])

let translate_y n =
  let class_name = "translate-y-" ^ string_of_int n in
  Style (class_name, [ transform ("translateY(" ^ spacing_to_rem n ^ ")") ])

let cursor_auto = Style ("cursor-auto", [ cursor "auto" ])
let cursor_default = Style ("cursor-default", [ cursor "default" ])
let cursor_pointer = Style ("cursor-pointer", [ cursor "pointer" ])
let cursor_wait = Style ("cursor-wait", [ cursor "wait" ])
let cursor_move = Style ("cursor-move", [ cursor "move" ])
let cursor_not_allowed = Style ("cursor-not-allowed", [ cursor "not-allowed" ])
let select_none = Style ("select-none", [ user_select "none" ])
let select_text = Style ("select-text", [ user_select "text" ])
let select_all = Style ("select-all", [ user_select "all" ])
let select_auto = Style ("select-auto", [ user_select "auto" ])

let pointer_events_none =
  Style ("pointer-events-none", [ pointer_events "none" ])

let pointer_events_auto =
  Style ("pointer-events-auto", [ pointer_events "auto" ])

let outline_none = Style ("outline-none", [ outline "none" ])

let ring (w : width) =
  let width, class_suffix =
    match w with
    | `None -> ("0", "0")
    | `Xs -> ("1px", "1")
    | `Sm -> ("2px", "2")
    | `Default -> ("3px", "")
    | `Md -> ("3px", "3")
    | `Lg -> ("4px", "4")
    | `Xl -> ("8px", "8")
    | `Xl_2 -> ("8px", "8" (* Map Xl_2 to 8px as well *))
    | `Xl_3 -> ("8px", "8" (* Map Xl_3 to 8px as well *))
    | `Full -> ("8px", "8" (* Map Full to 8px as well *))
  in
  let class_name =
    if class_suffix = "" then "ring" else "ring-" ^ class_suffix
  in
  let shadow_value =
    if width = "0" then "0 0 #0000"
    else "0 0 0 " ^ width ^ " var(--tw-ring-color)"
  in
  Style
    ( class_name,
      [
        property "--tw-ring-color" "rgb(59 130 246 / 0.5)";
        box_shadow
          "var(--tw-ring-offset-shadow,0 0 #0000),var(--tw-ring-shadow,0 0 \
           #0000),var(--tw-shadow,0 0 #0000)";
        property "--tw-ring-shadow" shadow_value;
      ] )

let ring_color ?(shade = 500) color =
  let class_name =
    match color with
    | Black | White -> Pp.str [ "ring-"; color_name color ]
    | _ -> Pp.str [ "ring-"; color_name color; "-"; string_of_int shade ]
  in
  Style (class_name, [])

let isolate = Style ("isolate", [ display "isolate" ])
let overflow_auto = Style ("overflow-auto", [ overflow "auto" ])
let overflow_hidden = Style ("overflow-hidden", [ overflow "hidden" ])
let overflow_visible = Style ("overflow-visible", [ overflow "visible" ])
let overflow_scroll = Style ("overflow-scroll", [ overflow "scroll" ])

(* Overflow variants *)
let overflow_x_auto = Style ("overflow-x-auto", [ overflow_x "auto" ])
let overflow_x_hidden = Style ("overflow-x-hidden", [ overflow_x "hidden" ])
let overflow_x_visible = Style ("overflow-x-visible", [ overflow_x "visible" ])
let overflow_x_scroll = Style ("overflow-x-scroll", [ overflow_x "scroll" ])
let overflow_y_auto = Style ("overflow-y-auto", [ overflow_y "auto" ])
let overflow_y_hidden = Style ("overflow-y-hidden", [ overflow_y "hidden" ])
let overflow_y_visible = Style ("overflow-y-visible", [ overflow_y "visible" ])
let overflow_y_scroll = Style ("overflow-y-scroll", [ overflow_y "scroll" ])

(* Scroll snap utilities *)
let snap_none = Style ("snap-none", [ scroll_snap_type "none" ])

let snap_x =
  Style ("snap-x", [ scroll_snap_type "x var(--tw-scroll-snap-strictness)" ])

let snap_y =
  Style ("snap-y", [ scroll_snap_type "y var(--tw-scroll-snap-strictness)" ])

let snap_both =
  Style
    ("snap-both", [ scroll_snap_type "both var(--tw-scroll-snap-strictness)" ])

let snap_mandatory =
  Style
    ("snap-mandatory", [ property "--tw-scroll-snap-strictness" "mandatory" ])

let snap_proximity =
  Style
    ("snap-proximity", [ property "--tw-scroll-snap-strictness" "proximity" ])

let snap_start = Style ("snap-start", [ scroll_snap_align "start" ])
let snap_end = Style ("snap-end", [ scroll_snap_align "end" ])
let snap_center = Style ("snap-center", [ scroll_snap_align "center" ])
let snap_align_none = Style ("snap-align-none", [ scroll_snap_align "none" ])
let snap_normal = Style ("snap-normal", [ scroll_snap_stop "normal" ])
let snap_always = Style ("snap-always", [ scroll_snap_stop "always" ])
let scroll_auto = Style ("scroll-auto", [ scroll_behavior "auto" ])
let scroll_smooth = Style ("scroll-smooth", [ scroll_behavior "smooth" ])
let object_contain = Style ("object-contain", [ object_fit "contain" ])
let object_cover = Style ("object-cover", [ object_fit "cover" ])
let object_fill = Style ("object-fill", [ object_fit "fill" ])
let object_none = Style ("object-none", [ object_fit "none" ])
let object_scale_down = Style ("object-scale-down", [ object_fit "scale-down" ])

let sr_only =
  Style
    ( "sr-only",
      [
        position "absolute";
        width "1px";
        height "1px";
        padding "0";
        margin "-1px";
        overflow "hidden";
        clip "rect(0, 0, 0, 0)";
        white_space "nowrap";
        border_width "0";
      ] )

let not_sr_only =
  Style
    ( "not-sr-only",
      [
        position "static";
        width "auto";
        height "auto";
        padding "0";
        margin "0";
        overflow "visible";
        clip "auto";
        white_space "normal";
      ] )

(* Responsive and state modifiers *)

let focus_visible =
  Style
    ("focus-visible", [ outline "2px solid transparent"; outline_offset "2px" ])

let active t = Modified (Active, t)
let disabled t = Modified (Disabled, t)
let dark t = Modified (Dark, t)

(* New on_ style modifiers that take lists *)
let on_hover styles = Group (List.map (fun t -> Modified (Hover, t)) styles)
let on_focus styles = Group (List.map (fun t -> Modified (Focus, t)) styles)
let on_active styles = Group (List.map (fun t -> Modified (Active, t)) styles)

let on_disabled styles =
  Group (List.map (fun t -> Modified (Disabled, t)) styles)

let on_group_hover styles =
  Group (List.map (fun t -> Modified (Group_hover, t)) styles)

let on_group_focus styles =
  Group (List.map (fun t -> Modified (Group_focus, t)) styles)

let on_dark styles = Group (List.map (fun t -> Modified (Dark, t)) styles)

(* Additional on_* functions for consistency *)
let on_peer_hover styles =
  Group (List.map (fun t -> Modified (Peer_hover, t)) styles)

let on_peer_focus styles =
  Group (List.map (fun t -> Modified (Peer_focus, t)) styles)

let on_aria_disabled styles =
  Group (List.map (fun t -> Modified (Aria_disabled, t)) styles)

let on_data_active styles =
  Group (List.map (fun t -> Modified (Data_active, t)) styles)

let on_data_inactive styles =
  Group (List.map (fun t -> Modified (Data_inactive, t)) styles)

(* Check if a style already has a responsive modifier *)
let rec has_responsive_modifier = function
  | Style _ -> false
  | Prose _ -> false
  | Modified (Responsive _, _) -> true
  | Modified (_, t) -> has_responsive_modifier t
  | Group styles -> List.exists has_responsive_modifier styles

let validate_no_nested_responsive styles =
  List.iter
    (fun style ->
      if has_responsive_modifier style then
        failwith
          "Cannot apply responsive modifiers to styles that already have \
           responsive modifiers")
    styles

let on_sm styles =
  validate_no_nested_responsive styles;
  Group (List.map (fun t -> Modified (Responsive "sm", t)) styles)

let on_md styles =
  validate_no_nested_responsive styles;
  Group (List.map (fun t -> Modified (Responsive "md", t)) styles)

let on_lg styles =
  validate_no_nested_responsive styles;
  Group (List.map (fun t -> Modified (Responsive "lg", t)) styles)

let on_xl styles =
  validate_no_nested_responsive styles;
  Group (List.map (fun t -> Modified (Responsive "xl", t)) styles)

let on_2xl styles =
  validate_no_nested_responsive styles;
  Group (List.map (fun t -> Modified (Responsive "2xl", t)) styles)

let bg_gradient_to_b =
  Style
    ( "bg-gradient-to-b",
      [
        background_image "linear-gradient(to bottom, var(--tw-gradient-stops))";
      ] )

let bg_gradient_to_br =
  Style
    ( "bg-gradient-to-br",
      [
        background_image
          "linear-gradient(to bottom right, var(--tw-gradient-stops))";
      ] )

let bg_gradient_to_t =
  Style
    ( "bg-gradient-to-t",
      [ background_image "linear-gradient(to top, var(--tw-gradient-stops))" ]
    )

let bg_gradient_to_tr =
  Style
    ( "bg-gradient-to-tr",
      [
        background_image
          "linear-gradient(to top right, var(--tw-gradient-stops))";
      ] )

let bg_gradient_to_r =
  Style
    ( "bg-gradient-to-r",
      [ background_image "linear-gradient(to right, var(--tw-gradient-stops))" ]
    )

let bg_gradient_to_bl =
  Style
    ( "bg-gradient-to-bl",
      [
        background_image
          "linear-gradient(to bottom left, var(--tw-gradient-stops))";
      ] )

let bg_gradient_to_l =
  Style
    ( "bg-gradient-to-l",
      [ background_image "linear-gradient(to left, var(--tw-gradient-stops))" ]
    )

let bg_gradient_to_tl =
  Style
    ( "bg-gradient-to-tl",
      [
        background_image
          "linear-gradient(to top left, var(--tw-gradient-stops))";
      ] )

let antialiased =
  Style
    ( "antialiased",
      [
        webkit_font_smoothing "antialiased"; moz_osx_font_smoothing "grayscale";
      ] )

(* Additional functions needed *)
let aspect_ratio width height =
  let class_name =
    Pp.str
      [ "aspect-["; string_of_float width; "/"; string_of_float height; "]" ]
  in
  (* aspect-ratio isn't widely supported in CSS yet, skip for now *)
  Style (class_name, [])

let clip_path _value =
  (* clip-path is a modern CSS property, skip for now *)
  Style ("clip-path-custom", [])

let transform =
  Style
    ( "transform",
      [
        property "--tw-translate-x" "0";
        property "--tw-translate-y" "0";
        property "--tw-rotate" "0";
        property "--tw-skew-x" "0";
        property "--tw-skew-y" "0";
        property "--tw-scale-x" "1";
        property "--tw-scale-y" "1";
        transform
          "translateX(var(--tw-translate-x)) translateY(var(--tw-translate-y)) \
           rotate(var(--tw-rotate)) skewX(var(--tw-skew-x)) \
           skewY(var(--tw-skew-y)) scaleX(var(--tw-scale-x)) \
           scaleY(var(--tw-scale-y))";
      ] )

let transform_none = Style ("transform-none", [ Css.transform "none" ])
let transform_gpu = Style ("transform-gpu", [ Css.transform "translateZ(0)" ])

let brightness n =
  let class_name = "brightness-" ^ string_of_int n in
  let value = string_of_float (float_of_int n /. 100.0) in
  Style (class_name, [ filter ("brightness(" ^ value ^ ")") ])

let contrast n =
  let class_name = "contrast-" ^ string_of_int n in
  let value = string_of_float (float_of_int n /. 100.0) in
  Style (class_name, [ filter ("contrast(" ^ value ^ ")") ])

let blur = function
  | `None -> Style ("blur-none", [ filter "blur(0)" ])
  | `Xs -> Style ("blur-xs", [ filter "blur(2px)" ])
  | `Sm -> Style ("blur-sm", [ filter "blur(4px)" ])
  | `Md -> Style ("blur", [ filter "blur(8px)" ])
  | `Lg -> Style ("blur-lg", [ filter "blur(16px)" ])
  | `Xl -> Style ("blur-xl", [ filter "blur(24px)" ])
  | `Xl_2 -> Style ("blur-2xl", [ filter "blur(40px)" ])
  | `Xl_3 -> Style ("blur-3xl", [ filter "blur(64px)" ])
  | `Full -> Style ("blur-full", [ filter "blur(9999px)" ])

let grayscale n =
  let class_name = if n = 0 then "grayscale-0" else "grayscale" in
  let value = string_of_float (float_of_int n /. 100.0) in
  Style (class_name, [ filter ("grayscale(" ^ value ^ ")") ])

let saturate n =
  let class_name = "saturate-" ^ string_of_int n in
  let value = string_of_float (float_of_int n /. 100.0) in
  Style (class_name, [ filter ("saturate(" ^ value ^ ")") ])

let sepia n =
  let class_name = if n = 0 then "sepia-0" else "sepia" in
  let value = string_of_float (float_of_int n /. 100.0) in
  Style (class_name, [ filter ("sepia(" ^ value ^ ")") ])

let invert n =
  let class_name = if n = 0 then "invert-0" else "invert" in
  let value = string_of_float (float_of_int n /. 100.0) in
  Style (class_name, [ filter ("invert(" ^ value ^ ")") ])

let hue_rotate n =
  let class_name = "hue-rotate-" ^ string_of_int n in
  let value = string_of_int n ^ "deg" in
  Style (class_name, [ filter ("hue-rotate(" ^ value ^ ")") ])

let backdrop_brightness n =
  let class_name = Pp.str [ "backdrop-brightness-"; string_of_int n ] in
  Style
    ( class_name,
      [
        backdrop_filter
          (Pp.str
             [ "brightness("; string_of_float (float_of_int n /. 100.); ")" ]);
      ] )

let backdrop_contrast n =
  let class_name = Pp.str [ "backdrop-contrast-"; string_of_int n ] in
  Style
    ( class_name,
      [
        backdrop_filter
          (Pp.str
             [ "contrast("; string_of_float (float_of_int n /. 100.); ")" ]);
      ] )

let backdrop_opacity n =
  let class_name = Pp.str [ "backdrop-opacity-"; string_of_int n ] in
  Style
    ( class_name,
      [
        backdrop_filter
          (Pp.str [ "opacity("; string_of_float (float_of_int n /. 100.); ")" ]);
      ] )

let backdrop_saturate n =
  let class_name = Pp.str [ "backdrop-saturate-"; string_of_int n ] in
  Style
    ( class_name,
      [
        backdrop_filter
          (Pp.str
             [ "saturate("; string_of_float (float_of_int n /. 100.); ")" ]);
      ] )

let backdrop_blur = function
  | `None -> Style ("backdrop-blur-none", [ backdrop_filter "blur(0)" ])
  | `Xs -> Style ("backdrop-blur-xs", [ backdrop_filter "blur(2px)" ])
  | `Sm -> Style ("backdrop-blur-sm", [ backdrop_filter "blur(4px)" ])
  | `Md -> Style ("backdrop-blur", [ backdrop_filter "blur(8px)" ])
  | `Lg -> Style ("backdrop-blur-lg", [ backdrop_filter "blur(12px)" ])
  | `Xl -> Style ("backdrop-blur-xl", [ backdrop_filter "blur(16px)" ])
  | `Xl_2 -> Style ("backdrop-blur-2xl", [ backdrop_filter "blur(24px)" ])
  | `Xl_3 -> Style ("backdrop-blur-3xl", [ backdrop_filter "blur(40px)" ])
  | `Full -> Style ("backdrop-blur-full", [ backdrop_filter "blur(9999px)" ])

(* Animation utilities *)
let animate_none = Style ("animate-none", [ animation "none" ])

let animate_spin =
  Style ("animate-spin", [ animation "spin 1s linear infinite" ])

let animate_ping =
  Style
    ("animate-ping", [ animation "ping 1s cubic-bezier(0, 0, 0.2, 1) infinite" ])

let animate_pulse =
  Style
    ( "animate-pulse",
      [ animation "pulse 2s cubic-bezier(0.4, 0, 0.6, 1) infinite" ] )

let animate_bounce = Style ("animate-bounce", [ animation "bounce 1s infinite" ])

(* Transition utilities *)
let duration n =
  let class_name = "duration-" ^ string_of_int n in
  let value = string_of_int n ^ "ms" in
  Style (class_name, [ property "transition-duration" value ])

let ease_linear =
  Style ("ease-linear", [ property "transition-timing-function" "linear" ])

let ease_in =
  Style
    ( "ease-in",
      [ property "transition-timing-function" "cubic-bezier(0.4, 0, 1, 1)" ] )

let ease_out =
  Style
    ( "ease-out",
      [ property "transition-timing-function" "cubic-bezier(0, 0, 0.2, 1)" ] )

let ease_in_out =
  Style
    ( "ease-in-out",
      [ property "transition-timing-function" "cubic-bezier(0.4, 0, 0.2, 1)" ]
    )

(** Transform utilities *)
let scale n =
  let value = float_of_int n /. 100.0 in
  let class_name = "scale-" ^ string_of_int n in
  Style
    ( class_name,
      [
        property "--tw-scale-x" (string_of_float value);
        property "--tw-scale-y" (string_of_float value);
        Css.transform
          "translate(var(--tw-translate-x), var(--tw-translate-y)) \
           rotate(var(--tw-rotate)) skewX(var(--tw-skew-x)) \
           skewY(var(--tw-skew-y)) scaleX(var(--tw-scale-x)) \
           scaleY(var(--tw-scale-y))";
      ] )

(* Appearance utilities *)
let appearance_none = Style ("appearance-none", [ appearance "none" ])

(* Resize utilities *)
let resize_none = Style ("resize-none", [ Css.resize "none" ])
let resize_y = Style ("resize-y", [ Css.resize "vertical" ])
let resize_x = Style ("resize-x", [ Css.resize "horizontal" ])
let resize = Style ("resize", [ Css.resize "both" ])

(* Will-change utilities *)
let will_change_auto =
  Style ("will-change-auto", [ property "will-change" "auto" ])

let will_change_scroll =
  Style ("will-change-scroll", [ property "will-change" "scroll-position" ])

let will_change_contents =
  Style ("will-change-contents", [ property "will-change" "contents" ])

let will_change_transform =
  Style ("will-change-transform", [ property "will-change" "transform" ])

(* Contain utilities *)
let contain_none = Style ("contain-none", [ property "contain" "none" ])
let contain_content = Style ("contain-content", [ property "contain" "content" ])
let contain_layout = Style ("contain-layout", [ property "contain" "layout" ])
let contain_paint = Style ("contain-paint", [ property "contain" "paint" ])
let contain_size = Style ("contain-size", [ property "contain" "size" ])

(* Object position utilities *)
let object_top = Style ("object-top", [ property "object-position" "top" ])
let object_right = Style ("object-right", [ property "object-position" "right" ])

let object_bottom =
  Style ("object-bottom", [ property "object-position" "bottom" ])

let object_left = Style ("object-left", [ property "object-position" "left" ])

let object_center =
  Style ("object-center", [ property "object-position" "center" ])

(* Table utilities *)
let table_auto = Style ("table-auto", [ table_layout "auto" ])
let table_fixed = Style ("table-fixed", [ table_layout "fixed" ])

let border_collapse =
  Style ("border-collapse", [ Css.border_collapse "collapse" ])

let border_separate =
  Style ("border-separate", [ Css.border_collapse "separate" ])

let border_spacing n =
  let value = spacing_to_rem n in
  Style ("border-spacing-" ^ string_of_int n, [ border_spacing value ])

(* Form utilities - equivalent to @tailwindcss/forms plugin *)
let form_input =
  Style
    ( "form-input",
      [
        appearance "none";
        background_color "white";
        Css.border_color "rgb(209 213 219)";
        border_width "1px";
        border_radius "0.375rem";
        padding_top "0.5rem";
        padding_right "0.75rem";
        padding_bottom "0.5rem";
        padding_left "0.75rem";
        font_size "1rem";
        line_height "1.5rem";
        property "outline" "2px solid transparent";
        property "outline-offset" "2px";
      ] )

let form_textarea =
  Style
    ( "form-textarea",
      [
        appearance "none";
        background_color "white";
        Css.border_color "rgb(209 213 219)";
        border_width "1px";
        border_radius "0.375rem";
        padding_top "0.5rem";
        padding_right "0.75rem";
        padding_bottom "0.5rem";
        padding_left "0.75rem";
        font_size "1rem";
        line_height "1.5rem";
        Css.resize "vertical";
      ] )

let form_select =
  Style
    ( "form-select",
      [
        appearance "none";
        background_color "white";
        Css.border_color "rgb(209 213 219)";
        border_width "1px";
        border_radius "0.375rem";
        padding_top "0.5rem";
        padding_right "2.5rem";
        padding_bottom "0.5rem";
        padding_left "0.75rem";
        font_size "1rem";
        line_height "1.5rem";
        background_image
          "url(\"data:image/svg+xml,%3csvg xmlns='http://www.w3.org/2000/svg' \
           fill='none' viewBox='0 0 20 20'%3e%3cpath stroke='%236b7280' \
           stroke-linecap='round' stroke-linejoin='round' stroke-width='1.5' \
           d='M6 8l4 4 4-4'/%3e%3c/svg%3e\")";
        background_position "right 0.5rem center";
        background_repeat "no-repeat";
        background_size "1.5em 1.5em";
      ] )

let form_checkbox =
  Style
    ( "form-checkbox",
      [
        appearance "none";
        width "1rem";
        height "1rem";
        background_color "white";
        Css.border_color "rgb(209 213 219)";
        border_width "1px";
        border_radius "0.25rem";
        Css.color "rgb(59 130 246)";
        Css.flex_shrink "0";
        display "inline-block";
        vertical_align "middle";
      ] )

let form_radio =
  Style
    ( "form-radio",
      [
        appearance "none";
        width "1rem";
        height "1rem";
        background_color "white";
        Css.border_color "rgb(209 213 219)";
        border_width "1px";
        border_radius "100%";
        Css.color "rgb(59 130 246)";
        Css.flex_shrink "0";
        display "inline-block";
        vertical_align "middle";
      ] )

(* Peer and group utilities *)
let peer = Style ("peer", []) (* Marker class for peer relationships *)
let group = Style ("group", []) (* Marker class for group relationships *)

(* Peer modifiers *)
let peer_checked style = Modified (Peer_checked, style)

let on_peer_checked styles =
  Group (List.map (fun s -> Modified (Peer_checked, s)) styles)

(* ARIA state modifiers *)
let aria_checked style = Modified (Aria_checked, style)
let aria_expanded style = Modified (Aria_expanded, style)
let aria_selected style = Modified (Aria_selected, style)

let on_aria_checked styles =
  Group (List.map (fun s -> Modified (Aria_checked, s)) styles)

let on_aria_expanded styles =
  Group (List.map (fun s -> Modified (Aria_expanded, s)) styles)

let on_aria_selected styles =
  Group (List.map (fun s -> Modified (Aria_selected, s)) styles)

(* Data attribute modifiers *)
let data_state value style = Modified (Data_state value, style)
let data_variant value style = Modified (Data_variant value, style)
let data_custom key value style = Modified (Data_custom (key, value), style)

let from_color ?(shade = 500) color =
  let class_name =
    match color with
    | Black | White -> "from-" ^ color_name color
    | _ -> Pp.str [ "from-"; color_name color; "-"; string_of_int shade ]
  in
  (* CSS variables for gradients, skip for now *)
  Style (class_name, [])

let to_color ?(shade = 500) color =
  let class_name =
    match color with
    | Black | White -> "to-" ^ color_name color
    | _ -> Pp.str [ "to-"; color_name color; "-"; string_of_int shade ]
  in
  (* CSS variables for gradients, skip for now *)
  Style (class_name, [])

let color_to_string = color_name

(* Class generation functions *)
let rec to_class = function
  | Style (class_name, _) -> class_name
  | Prose variant -> Prose.to_class variant
  | Modified (modifier, t) -> (
      let base_class = to_class t in
      match modifier with
      | Hover -> "hover:" ^ base_class
      | Focus -> "focus:" ^ base_class
      | Active -> "active:" ^ base_class
      | Disabled -> "disabled:" ^ base_class
      | Group_hover -> "group-hover:" ^ base_class
      | Group_focus -> "group-focus:" ^ base_class
      | Peer_hover -> "peer-hover:" ^ base_class
      | Peer_focus -> "peer-focus:" ^ base_class
      | Peer_checked -> "peer-checked:" ^ base_class
      | Aria_checked -> "aria-checked:" ^ base_class
      | Aria_expanded -> "aria-expanded:" ^ base_class
      | Aria_selected -> "aria-selected:" ^ base_class
      | Aria_disabled -> "aria-disabled:" ^ base_class
      | Data_state value -> "data-[state=" ^ value ^ "]:" ^ base_class
      | Data_variant value -> "data-[variant=" ^ value ^ "]:" ^ base_class
      | Data_active -> "data-[active]:" ^ base_class
      | Data_inactive -> "data-[inactive]:" ^ base_class
      | Data_custom (key, value) ->
          "data-[" ^ key ^ "=" ^ value ^ "]:" ^ base_class
      | Dark -> "dark:" ^ base_class
      | Responsive prefix -> prefix ^ ":" ^ base_class)
  | Group styles -> styles |> List.map to_class |> String.concat " "

let to_classes styles = styles |> List.map to_class |> String.concat " "
let to_string t = to_class t
let classes_to_string = to_classes

(* Pretty printing *)
let pp t = to_string t

(* Convert Tw styles to inline style attribute value *)
let to_inline_style styles =
  let all_props = List.concat_map to_css_properties styles in
  let deduped = Css.deduplicate_properties all_props in
  Css.properties_to_inline_style deduped

(* Prose utilities for beautiful typography *)
let prose = Prose Base
let prose_sm = Prose Sm
let prose_lg = Prose Lg
let prose_xl = Prose Xl
let prose_2xl = Prose Xl2
let prose_gray = Prose Gray
let prose_slate = Prose Slate

(* Expose Prose module for convenient access *)
module Prose = Prose

(* Generate complete prose stylesheet *)
let prose_stylesheet () =
  (* Generate CSS for all prose variants *)
  let all_variants = [Prose.Base; Prose.Sm; Prose.Lg; Prose.Xl; Prose.Xl2; 
                      Prose.Gray; Prose.Slate] in
  let all_rules = List.concat_map Prose.to_css_rules all_variants in
  (* Add CSS variables to root *)
  let root_rule = Css.rule ~selector:":root" Prose.css_variables in
  Css.stylesheet (root_rule :: all_rules)

(* Line clamp utility function *)
let line_clamp n =
  let class_name = "line-clamp-" ^ string_of_int n in
  if n = 0 then Style ("line-clamp-none", [ webkit_line_clamp "none" ])
  else Style (class_name, [ webkit_line_clamp (string_of_int n) ])

(* Opacity utilities *)

(* Helper parsing functions *)
let color_of_string = function
  | "black" -> Ok black
  | "white" -> Ok white
  | "gray" -> Ok gray
  | "slate" -> Ok slate
  | "zinc" -> Ok zinc
  | "red" -> Ok red
  | "orange" -> Ok orange
  | "amber" -> Ok amber
  | "yellow" -> Ok yellow
  | "lime" -> Ok lime
  | "green" -> Ok green
  | "emerald" -> Ok emerald
  | "teal" -> Ok teal
  | "cyan" -> Ok cyan
  | "sky" -> Ok sky
  | "blue" -> Ok blue
  | "indigo" -> Ok indigo
  | "violet" -> Ok violet
  | "purple" -> Ok purple
  | "fuchsia" -> Ok fuchsia
  | "pink" -> Ok pink
  | "rose" -> Ok rose
  | color -> Error (`Msg ("Unknown color: " ^ color))

let spacing_of_string s =
  match s with
  | "px" -> Ok one_px
  | "full" -> Ok full
  | n -> (
      match int_of_string_opt n with
      | None -> Error (`Msg ("Invalid spacing: " ^ s))
      | Some n -> Ok (int n))

let margin_of_string s =
  match s with
  | "auto" -> Ok auto
  | _ -> (
      match spacing_of_string s with
      | Ok sp -> Ok (sp :> margin)
      | Error _ as e -> e)

let size_of_string = function
  | "none" -> Ok none
  | "xs" -> Ok xs
  | "sm" -> Ok sm
  | "md" -> Ok md
  | "lg" -> Ok lg
  | "xl" -> Ok xl
  | "2xl" -> Ok xl_2
  | "3xl" -> Ok xl_3
  | "full" -> Ok full
  | s -> Error (`Msg ("Unknown size: " ^ s))

let scale_of_string s =
  match s with
  | "full" -> Ok full
  | "screen" -> Ok screen
  | "min" -> Ok min
  | "max" -> Ok max
  | "fit" -> Ok fit
  | n -> (
      (* Try to parse as spacing (integer) first *)
      match spacing_of_string n with
      | Ok sp -> Ok (sp :> scale)
      | Error _ -> Error (`Msg ("Invalid scale value: " ^ s)))

let max_scale_of_string s =
  match scale_of_string s with
  | Ok sc -> Ok (sc :> max_scale)
  | Error _ -> (
      (* Try additional max_scale values *)
      match s with
      | "2xl" -> Ok xl_2
      | "3xl" -> Ok xl_3
      | "4xl" -> Ok xl_4
      | "5xl" -> Ok xl_5
      | "6xl" -> Ok xl_6
      | "7xl" -> Ok xl_7
      | _ -> Error (`Msg ("Invalid max scale value: " ^ s)))

let text_size_of_string = function
  | "xs" -> Ok text_xs
  | "sm" -> Ok text_sm
  | "base" -> Ok text_base
  | "lg" -> Ok text_lg
  | "xl" -> Ok text_xl
  | "2xl" -> Ok text_2xl
  | "3xl" -> Ok text_3xl
  | "4xl" -> Ok text_4xl
  | "5xl" -> Ok text_5xl
  | s -> Error (`Msg ("Unknown text size: " ^ s))

let shadow_of_string = function
  | "none" -> Ok (shadow none)
  | "sm" -> Ok (shadow sm)
  | "md" -> Ok (shadow md)
  | "lg" -> Ok (shadow lg)
  | "xl" -> Ok (shadow xl)
  | "2xl" -> Ok (shadow xl_2)
  | "inner" -> Ok (shadow inner)
  | "" -> Ok (shadow md) (* default shadow *)
  | s -> Error (`Msg ("Unknown shadow size: " ^ s))

let parse_positive_int name s =
  match int_of_string_opt s with
  | None -> Error (`Msg ("Invalid " ^ name ^ " value: " ^ s))
  | Some n when n >= 0 -> Ok n
  | Some _ -> Error (`Msg (name ^ " must be non-negative: " ^ s))

let parse_bounded_int name min max s =
  match int_of_string_opt s with
  | None -> Error (`Msg ("Invalid " ^ name ^ " value: " ^ s))
  | Some n when n >= min && n <= max -> Ok n
  | Some _ ->
      Error
        (`Msg
           (Pp.str
              [
                name;
                " must be between ";
                string_of_int min;
                " and ";
                string_of_int max;
                ": ";
                s;
              ]))

let leading_of_string n =
  match float_of_string_opt n with
  | None -> Error (`Msg ("Invalid leading value: " ^ n))
  | Some value ->
      let rem_value = value /. 4.0 in
      let class_name = Pp.str [ "leading-"; n ] in
      let css_value = Pp.str [ string_of_float rem_value; "rem" ] in
      Ok (Style (class_name, [ line_height css_value ]))

(* Helper for Result.bind-like operation *)
let ( >>= ) r f = match r with Error _ as e -> e | Ok x -> f x

(* Helper for "try this or else try that" *)
let ( <|> ) r1 r2 = match r1 with Ok _ -> r1 | Error _ -> r2

(* Helper for Result.map-like operation *)
let ( >|= ) r f = match r with Error _ as e -> e | Ok x -> Ok (f x)

(* Helper to parse shade from string *)
let shade_of_string s =
  match int_of_string_opt s with
  | None -> Error (`Msg ("Invalid shade: " ^ s))
  | Some shade -> Ok shade

(* Parse modifiers (responsive, states) from class string *)
let parse_modifiers class_str =
  let parts = String.split_on_char ':' class_str in
  match List.rev parts with
  | [] -> ([], class_str)
  | base_class :: modifiers -> (List.rev modifiers, base_class)

(* Apply modifiers to a base style *)
let apply_modifiers modifiers base_style =
  List.fold_left
    (fun acc modifier ->
      match modifier with
      | "sm" -> (
          match acc with
          | Group styles -> on_sm styles
          | single -> on_sm [ single ])
      | "md" -> (
          match acc with
          | Group styles -> on_md styles
          | single -> on_md [ single ])
      | "lg" -> (
          match acc with
          | Group styles -> on_lg styles
          | single -> on_lg [ single ])
      | "xl" -> (
          match acc with
          | Group styles -> on_xl styles
          | single -> on_xl [ single ])
      | "2xl" -> (
          match acc with
          | Group styles -> on_2xl styles
          | single -> on_2xl [ single ])
      | "hover" -> (
          match acc with
          | Group styles -> on_hover styles
          | single -> on_hover [ single ])
      | "focus" -> (
          match acc with
          | Group styles -> on_focus styles
          | single -> on_focus [ single ])
      | "active" -> (
          match acc with
          | Group styles -> on_active styles
          | single -> on_active [ single ])
      | "disabled" -> (
          match acc with
          | Group styles -> on_disabled styles
          | single -> on_disabled [ single ])
      | "dark" -> (
          match acc with
          | Group styles -> on_dark styles
          | single -> on_dark [ single ])
      | _ -> acc (* ignore unknown modifiers for now *))
    base_style modifiers

(* Parse a single class string into a Tw.t *)
let of_string class_str =
  let modifiers, base_class = parse_modifiers class_str in
  let parts = String.split_on_char '-' base_class in
  let base_result =
    match parts with
    | [ "bg"; "transparent" ] -> Ok bg_transparent
    | [ "bg"; "current" ] -> Ok bg_current
    | [ "bg"; color; shade ] ->
        color_of_string color >>= fun color ->
        shade_of_string shade >|= fun shade -> bg ~shade color
    | [ "bg"; color ] -> color_of_string color >|= bg
    | [ "text"; "transparent" ] -> Ok text_transparent
    | [ "text"; "current" ] -> Ok text_current
    | [ "text"; "center" ] -> Ok text_center
    | [ "text"; "left" ] -> Ok text_left
    | [ "text"; "right" ] -> Ok text_right
    | [ "text"; "justify" ] -> Ok text_justify
    | [ "text"; color; shade ] ->
        color_of_string color >>= fun color ->
        shade_of_string shade >|= fun shade -> text ~shade color
    | [ "text"; single ] ->
        (* Try size first, then color *)
        text_size_of_string single <|> (color_of_string single >|= text)
    | [ "border" ] -> Ok (border `Default)
    | [ "border"; color; shade ] ->
        color_of_string color >>= fun color ->
        shade_of_string shade >|= fun shade -> border_color ~shade color
    | [ "border"; "transparent" ] -> Ok border_transparent
    | [ "border"; "current" ] -> Ok border_current
    | [ "border"; color ] -> color_of_string color >|= border_color
    | [ "p"; n ] -> spacing_of_string n >|= p
    | [ "px"; n ] -> spacing_of_string n >|= px
    | [ "py"; n ] -> spacing_of_string n >|= py
    | [ "pt"; n ] -> spacing_of_string n >|= pt
    | [ "pr"; n ] -> spacing_of_string n >|= pr
    | [ "pb"; n ] -> spacing_of_string n >|= pb
    | [ "pl"; n ] -> spacing_of_string n >|= pl
    | [ "m"; n ] -> margin_of_string n >|= m
    | [ "mx"; n ] -> margin_of_string n >|= mx
    | [ "my"; n ] -> margin_of_string n >|= my
    | [ "mt"; n ] -> margin_of_string n >|= mt
    | [ "mr"; n ] -> margin_of_string n >|= mr
    | [ "mb"; n ] -> margin_of_string n >|= mb
    | [ "ml"; n ] -> margin_of_string n >|= ml
    | [ "w"; n ] -> scale_of_string n >|= w
    | [ "h"; n ] -> scale_of_string n >|= h
    | [ "min"; "w"; n ] -> scale_of_string n >|= min_w
    | [ "min"; "h"; n ] -> scale_of_string n >|= min_h
    | [ "max"; "w"; n ] -> max_scale_of_string n >|= max_w
    | [ "max"; "h"; n ] -> scale_of_string n >|= max_h
    | [ "gap"; n ] -> spacing_of_string n >|= gap
    | [ "gap"; "x"; n ] -> spacing_of_string n >|= gap_x
    | [ "gap"; "y"; n ] -> spacing_of_string n >|= gap_y
    | [ "flex" ] -> Ok flex
    | [ "flex"; "col" ] -> Ok flex_col
    | [ "flex"; "row" ] -> Ok flex_row
    | [ "flex"; "wrap" ] -> Ok flex_wrap
    | [ "flex"; "nowrap" ] -> Ok flex_nowrap
    | [ "flex"; "1" ] -> Ok flex_1
    | [ "flex"; "auto" ] -> Ok flex_auto
    | [ "flex"; "initial" ] -> Ok flex_initial
    | [ "flex"; "none" ] -> Ok flex_none
    | [ "block" ] -> Ok block
    | [ "inline" ] -> Ok inline
    | [ "inline"; "block" ] -> Ok inline_block
    | [ "inline"; "grid" ] -> Ok inline_grid
    | [ "grid" ] -> Ok grid
    | [ "grid"; "cols"; n ] -> parse_positive_int "grid cols" n >|= grid_cols
    | [ "grid"; "rows"; n ] -> parse_positive_int "grid rows" n >|= grid_rows
    | [ "hidden" ] -> Ok hidden
    | [ "rounded" ] -> Ok (rounded md)
    | [ "rounded"; size ] -> size_of_string size >|= rounded
    | [ "shadow" ] -> shadow_of_string ""
    | [ "shadow"; size ] -> shadow_of_string size
    | [ "ring" ] -> Ok (ring `Default)
    | [ "ring"; "0" ] -> Ok (ring `None)
    | [ "ring"; "1" ] -> Ok (ring `Xs)
    | [ "ring"; "2" ] -> Ok (ring `Sm)
    | [ "ring"; "3" ] -> Ok (ring `Md)
    | [ "ring"; "4" ] -> Ok (ring `Lg)
    | [ "ring"; "8" ] -> Ok (ring `Xl)
    | [ "items"; "center" ] -> Ok items_center
    | [ "items"; "start" ] -> Ok items_start
    | [ "items"; "end" ] -> Ok items_end
    | [ "items"; "stretch" ] -> Ok items_stretch
    | [ "items"; "baseline" ] -> Ok items_baseline
    | [ "justify"; "center" ] -> Ok justify_center
    | [ "justify"; "start" ] -> Ok justify_start
    | [ "justify"; "end" ] -> Ok justify_end
    | [ "justify"; "between" ] -> Ok justify_between
    | [ "justify"; "around" ] -> Ok justify_around
    | [ "justify"; "evenly" ] -> Ok justify_evenly
    | [ "font"; "thin" ] -> Ok font_thin
    | [ "font"; "light" ] -> Ok font_light
    | [ "font"; "normal" ] -> Ok font_normal
    | [ "font"; "medium" ] -> Ok font_medium
    | [ "font"; "semibold" ] -> Ok font_semibold
    | [ "font"; "bold" ] -> Ok font_bold
    | [ "font"; "extrabold" ] -> Ok font_extrabold
    | [ "font"; "black" ] -> Ok font_black
    | [ "font"; "sans" ] -> Ok font_sans
    | [ "font"; "serif" ] -> Ok font_serif
    | [ "font"; "mono" ] -> Ok font_mono
    | [ "italic" ] -> Ok italic
    | [ "not"; "italic" ] -> Ok not_italic
    | [ "underline" ] -> Ok underline
    | [ "no"; "underline" ] -> Ok no_underline
    | [ "leading"; "none" ] -> Ok leading_none
    | [ "leading"; "tight" ] -> Ok leading_tight
    | [ "leading"; "snug" ] -> Ok leading_snug
    | [ "leading"; "normal" ] -> Ok leading_normal
    | [ "leading"; "relaxed" ] -> Ok leading_relaxed
    | [ "leading"; "loose" ] -> Ok leading_loose
    | [ "leading"; n ] -> leading_of_string n
    | [ "relative" ] -> Ok relative
    | [ "absolute" ] -> Ok absolute
    | [ "fixed" ] -> Ok fixed
    | [ "sticky" ] -> Ok sticky
    | [ "static" ] -> Ok static
    | [ "opacity"; n ] -> parse_bounded_int "Opacity" 0 100 n >|= opacity
    | [ "transition" ] -> Ok transition_all
    | [ "transition"; "none" ] -> Ok transition_none
    | [ "transition"; "all" ] -> Ok transition_all
    | [ "transition"; "colors" ] -> Ok transition_colors
    | [ "transition"; "opacity" ] -> Ok transition_opacity
    | [ "transition"; "shadow" ] -> Ok transition_shadow
    | [ "transition"; "transform" ] -> Ok transition_transform
    | [ "duration"; n ] -> parse_positive_int "duration" n >|= duration
    | [ "scale"; n ] -> parse_positive_int "scale" n >|= scale
    (* Prose classes *)
    | [ "prose" ] -> Ok prose
    | [ "prose"; "sm" ] -> Ok prose_sm
    | [ "prose"; "lg" ] -> Ok prose_lg  
    | [ "prose"; "xl" ] -> Ok prose_xl
    | [ "prose"; "2xl" ] -> Ok prose_2xl
    | [ "prose"; "gray" ] -> Ok prose_gray
    | [ "prose"; "slate" ] -> Ok prose_slate
    | _ -> Error (`Msg ("Unknown class: " ^ class_str))
  in
  match base_result with
  | Error _ as e -> e
  | Ok base_style -> Ok (apply_modifiers modifiers base_style)
