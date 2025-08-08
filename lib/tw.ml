(** A type-safe, ergonomic DSL for Tailwind CSS using nominal types. *)

open Css

(** {1 Core Types} *)

type breakpoint = [ `Sm | `Md | `Lg | `Xl | `Xl_2 ]
(** Responsive breakpoints *)

(** A Tailwind utility modifier *)
type modifier =
  | Hover
  | Focus
  | Active
  | Disabled
  | Group_hover
  | Dark
  | Responsive of breakpoint
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
type spacing = [ `Px | `Full | `Rem of float ]
type margin = [ spacing | `Auto ]
type scale = [ spacing | size | `Screen | `Min | `Max | `Fit ]
type max_scale = [ scale | `Xl_4 | `Xl_5 | `Xl_6 | `Xl_7 ]
type shadow = [ size | `Inner ]

(* Re-export CSS module *)
module Css = Css

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

(* Helper to convert breakpoint to string *)
let string_of_breakpoint = function
  | `Sm -> "sm"
  | `Md -> "md"
  | `Lg -> "lg"
  | `Xl -> "xl"
  | `Xl_2 -> "2xl"

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
            | Responsive breakpoint ->
                let prefix = string_of_breakpoint breakpoint in
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
    Css.rule ~selector:"*"
      [ Css.margin "0"; Css.padding "0"; Css.box_sizing "border-box" ];
    Css.rule ~selector:"body"
      [
        Css.font_size "16px";
        Css.line_height "1.5";
        Css.color "#374151";
        Css.font_family
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
      (fun (selector, props) ->
        Css.rule ~selector (Css.deduplicate_properties props))
      all_rules
  in
  let final_rules =
    if reset then
      (* Include prose CSS variables in reset if prose is being used *)
      let prose_reset =
        if uses_prose tw_classes then
          [ Css.rule ~selector:":root" Prose.css_variables ]
        else []
      in
      reset_rules @ prose_reset @ rules
    else rules
  in
  Css.stylesheet final_rules

(* Convert Tw styles to inline style attribute value *)
let to_inline_style styles =
  let all_props = List.concat_map to_css_properties styles in
  let deduped = Css.deduplicate_properties all_props in
  Css.properties_to_inline_style deduped

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
  | n -> Pp.float (float_of_int n *. 0.25) ^ "rem"

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
let rem f = `Rem f
let int n = rem (float_of_int n *. 0.25)
let one_px = `Px
let full = `Full

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

let bg color shade =
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

(* Default color backgrounds - using shade 500 *)
let bg_black = bg Black 500
let bg_white = bg White 500
let bg_gray = bg Gray 500
let bg_slate = bg Slate 500
let bg_zinc = bg Zinc 500
let bg_red = bg Red 500
let bg_orange = bg Orange 500
let bg_amber = bg Amber 500
let bg_yellow = bg Yellow 500
let bg_lime = bg Lime 500
let bg_green = bg Green 500
let bg_emerald = bg Emerald 500
let bg_teal = bg Teal 500
let bg_cyan = bg Cyan 500
let bg_sky = bg Sky 500
let bg_blue = bg Blue 500
let bg_indigo = bg Indigo 500
let bg_violet = bg Violet 500
let bg_purple = bg Purple 500
let bg_fuchsia = bg Fuchsia 500
let bg_pink = bg Pink 500
let bg_rose = bg Rose 500

let text color shade =
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

(* Default text colors - using shade 500 *)
let text_black = text Black 500
let text_white = text White 500
let text_gray = text Gray 500
let text_slate = text Slate 500
let text_zinc = text Zinc 500
let text_red = text Red 500
let text_orange = text Orange 500
let text_amber = text Amber 500
let text_yellow = text Yellow 500
let text_lime = text Lime 500
let text_green = text Green 500
let text_emerald = text Emerald 500
let text_teal = text Teal 500
let text_cyan = text Cyan 500
let text_sky = text Sky 500
let text_blue = text Blue 500
let text_indigo = text Indigo 500
let text_violet = text Violet 500
let text_purple = text Purple 500
let text_fuchsia = text Fuchsia 500
let text_pink = text Pink 500
let text_rose = text Rose 500

let border_color color shade =
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

(* Default border colors - using shade 500 *)
let border_black = border_color Black 500
let border_white = border_color White 500
let border_gray = border_color Gray 500
let border_slate = border_color Slate 500
let border_zinc = border_color Zinc 500
let border_red = border_color Red 500
let border_orange = border_color Orange 500
let border_amber = border_color Amber 500
let border_yellow = border_color Yellow 500
let border_lime = border_color Lime 500
let border_green = border_color Green 500
let border_emerald = border_color Emerald 500
let border_teal = border_color Teal 500
let border_cyan = border_color Cyan 500
let border_sky = border_color Sky 500
let border_blue = border_color Blue 500
let border_indigo = border_color Indigo 500
let border_violet = border_color Violet 500
let border_purple = border_color Purple 500
let border_fuchsia = border_color Fuchsia 500
let border_pink = border_color Pink 500
let border_rose = border_color Rose 500

let pp_spacing_suffix : spacing -> string = function
  | `Px -> "px"
  | `Full -> "full"
  | `Rem f ->
      (* Convert rem values back to Tailwind scale *)
      let n = int_of_float (f /. 0.25) in
      string_of_int (abs n)

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

let pp_max_scale_suffix : max_scale -> string = function
  | `Xl_4 -> "4xl"
  | `Xl_5 -> "5xl"
  | `Xl_6 -> "6xl"
  | `Xl_7 -> "7xl"
  | #scale as s -> pp_scale_suffix s

let pp_margin_suffix : margin -> string = function
  | `Auto -> "auto"
  | #spacing as s -> pp_spacing_suffix s

let pp_spacing : spacing -> string = function
  | `Px -> "1px"
  | `Full -> "100%"
  | `Rem f -> if f = 0.0 then "0" else Pp.str [ Pp.float f; "rem" ]

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
  | `Xl_4 -> "56rem"
  | `Xl_5 -> "64rem"
  | `Xl_6 -> "72rem"
  | `Xl_7 -> "80rem"
  | #scale as s -> pp_scale s

(** {1 Spacing} *)

(* Typed spacing functions with ' suffix *)
let p' (s : spacing) =
  let class_name = "p-" ^ pp_spacing_suffix s in
  Style (class_name, [ padding (pp_spacing s) ])

let px' (s : spacing) =
  let v = pp_spacing s in
  let class_name = "px-" ^ pp_spacing_suffix s in
  Style (class_name, [ padding_left v; padding_right v ])

let py' (s : spacing) =
  let v = pp_spacing s in
  let class_name = "py-" ^ pp_spacing_suffix s in
  Style (class_name, [ padding_bottom v; padding_top v ])

let pt' (s : spacing) =
  let class_name = "pt-" ^ pp_spacing_suffix s in
  Style (class_name, [ padding_top (pp_spacing s) ])

let pr' (s : spacing) =
  let class_name = "pr-" ^ pp_spacing_suffix s in
  Style (class_name, [ padding_right (pp_spacing s) ])

let pb' (s : spacing) =
  let class_name = "pb-" ^ pp_spacing_suffix s in
  Style (class_name, [ padding_bottom (pp_spacing s) ])

let pl' (s : spacing) =
  let class_name = "pl-" ^ pp_spacing_suffix s in
  Style (class_name, [ padding_left (pp_spacing s) ])

(* Int-based spacing functions (convenience wrappers) *)
let p n = p' (int n)
let px n = px' (int n)
let py n = py' (int n)
let pt n = pt' (int n)
let pr n = pr' (int n)
let pb n = pb' (int n)
let pl n = pl' (int n)

(* Special padding values *)
let p_px = p' `Px
let p_full = p' `Full
let px_px = px' `Px
let px_full = px' `Full
let py_px = py' `Px
let py_full = py' `Full
let pt_px = pt' `Px
let pt_full = pt' `Full
let pr_px = pr' `Px
let pr_full = pr' `Full
let pb_px = pb' `Px
let pb_full = pb' `Full
let pl_px = pl' `Px
let pl_full = pl' `Full

(* Typed margin functions with ' suffix *)
let m' (m : margin) =
  let class_name = "m-" ^ pp_margin_suffix m in
  Style (class_name, [ margin (pp_margin m) ])

let mx' (m : margin) =
  let v = pp_margin m in
  let class_name = "mx-" ^ pp_margin_suffix m in
  Style (class_name, [ margin_left v; margin_right v ])

let my' (m : margin) =
  let v = pp_margin m in
  let class_name = "my-" ^ pp_margin_suffix m in
  Style (class_name, [ margin_bottom v; margin_top v ])

let mt' (m : margin) =
  let class_name = "mt-" ^ pp_margin_suffix m in
  Style (class_name, [ margin_top (pp_margin m) ])

let mr' (m : margin) =
  let class_name = "mr-" ^ pp_margin_suffix m in
  Style (class_name, [ margin_right (pp_margin m) ])

let mb' (m : margin) =
  let class_name = "mb-" ^ pp_margin_suffix m in
  Style (class_name, [ margin_bottom (pp_margin m) ])

let ml' (m : margin) =
  let class_name = "ml-" ^ pp_margin_suffix m in
  Style (class_name, [ margin_left (pp_margin m) ])

(* Int-based margin functions - now support negative values *)
let m n =
  let s = int n in
  let prefix = if n < 0 then "-" else "" in
  let class_name = prefix ^ "m-" ^ pp_spacing_suffix s in
  Style (class_name, [ margin (pp_margin s) ])

let mx n =
  let s = int n in
  let v = pp_margin s in
  let prefix = if n < 0 then "-" else "" in
  let class_name = prefix ^ "mx-" ^ pp_spacing_suffix s in
  Style (class_name, [ margin_left v; margin_right v ])

let my n =
  let s = int n in
  let v = pp_margin s in
  let prefix = if n < 0 then "-" else "" in
  let class_name = prefix ^ "my-" ^ pp_spacing_suffix s in
  Style (class_name, [ margin_bottom v; margin_top v ])

let mt n =
  let s = int n in
  let prefix = if n < 0 then "-" else "" in
  let class_name = prefix ^ "mt-" ^ pp_spacing_suffix s in
  Style (class_name, [ margin_top (pp_margin s) ])

let mr n =
  let s = int n in
  let prefix = if n < 0 then "-" else "" in
  let class_name = prefix ^ "mr-" ^ pp_spacing_suffix s in
  Style (class_name, [ margin_right (pp_margin s) ])

let mb n =
  let s = int n in
  let prefix = if n < 0 then "-" else "" in
  let class_name = prefix ^ "mb-" ^ pp_spacing_suffix s in
  Style (class_name, [ margin_bottom (pp_margin s) ])

let ml n =
  let s = int n in
  let prefix = if n < 0 then "-" else "" in
  let class_name = prefix ^ "ml-" ^ pp_spacing_suffix s in
  Style (class_name, [ margin_left (pp_margin s) ])

(* Common margin utilities *)
let m_auto = m' `Auto
let mx_auto = mx' `Auto (* Very common for centering *)
let my_auto = my' `Auto
let mt_auto = mt' `Auto
let mr_auto = mr' `Auto
let mb_auto = mb' `Auto
let ml_auto = ml' `Auto

(* Typed gap functions with ' suffix *)
let gap' (s : spacing) =
  let class_name = "gap-" ^ pp_spacing_suffix s in
  Style (class_name, [ gap (pp_spacing s) ])

let gap_x' (s : spacing) =
  let class_name = "gap-x-" ^ pp_spacing_suffix s in
  Style (class_name, [ column_gap (pp_spacing s) ])

let gap_y' (s : spacing) =
  let class_name = "gap-y-" ^ pp_spacing_suffix s in
  Style (class_name, [ row_gap (pp_spacing s) ])

(* Int-based gap functions (convenience wrappers) *)
let gap n = gap' (int n)
let gap_x n = gap_x' (int n)
let gap_y n = gap_y' (int n)

(* Special gap values *)
let gap_px = gap' `Px
let gap_full = gap' `Full

(** {1 Sizing} *)

(* Typed scale functions with ' suffix *)
let w' (s : scale) =
  let class_name = "w-" ^ pp_scale_suffix s in
  Style (class_name, [ width (pp_scale s) ])

let h' (s : scale) =
  let class_name = "h-" ^ pp_scale_suffix s in
  Style (class_name, [ height (pp_scale s) ])

let min_w' (s : scale) =
  let class_name = "min-w-" ^ pp_scale_suffix s in
  Style (class_name, [ min_width (pp_scale s) ])

let min_h' (s : scale) =
  let class_name = "min-h-" ^ pp_scale_suffix s in
  Style (class_name, [ min_height (pp_scale s) ])

let max_w' (s : max_scale) =
  let class_name = "max-w-" ^ pp_max_scale_suffix s in
  Style (class_name, [ max_width (pp_max_scale s) ])

let max_h' (s : max_scale) =
  let class_name = "max-h-" ^ pp_max_scale_suffix s in
  Style (class_name, [ max_height (pp_max_scale s) ])

(* Int-based scale functions (convenience wrappers) *)
let w n = w' (int n)
let h n = h' (int n)
let min_w n = min_w' (int n)
let min_h n = min_h' (int n)
let max_w n = max_w' (int n)
let max_h n = max_h' (int n)

(* Common size utilities *)
let w_full = w' `Full (* Very common for full width *)
let h_full = h' `Full (* Very common for full height *)
let w_fit = w' `Fit (* Common for fit-content sizing *)
let h_fit = h' `Fit (* Common for fit-content sizing *)
let w_screen = w' `Screen (* Full viewport width *)
let h_screen = h' `Screen (* Full viewport height *)
let w_min = w' `Min (* Min-content width *)
let h_min = h' `Min (* Min-content height *)
let w_max = w' `Max (* Max-content width *)
let h_max = h' `Max (* Max-content height *)
let min_h_screen = min_h' `Screen (* Common for full viewport height *)
let min_w_full = min_w' `Full (* Minimum width 100% *)
let min_h_full = min_h' `Full (* Minimum height 100% *)
let max_w_2xl = max_w' `Xl_2 (* Common for article text *)
let max_w_3xl = max_w' `Xl_3 (* Common for content width *)
let max_w_4xl = max_w' `Xl_4 (* Common for content width *)
let max_w_none = max_w' `None (* No maximum width *)
let max_w_full = max_w' `Full (* Maximum width 100% *)
let max_h_full = max_h' `Full (* Maximum height 100% *)

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

let neg_mt n =
  let s = int n in
  let class_name = "-mt-" ^ pp_spacing_suffix s in
  Style (class_name, [ margin_top ("-" ^ pp_spacing s) ])

let neg_mr n =
  let s = int n in
  let class_name = "-mr-" ^ pp_spacing_suffix s in
  Style (class_name, [ margin_right ("-" ^ pp_spacing s) ])

let neg_mb n =
  let s = int n in
  let class_name = "-mb-" ^ pp_spacing_suffix s in
  Style (class_name, [ margin_bottom ("-" ^ pp_spacing s) ])

let neg_ml n =
  let s = int n in
  let class_name = "-ml-" ^ pp_spacing_suffix s in
  Style (class_name, [ margin_left ("-" ^ pp_spacing s) ])

(* Negative margin special variants *)
let neg_mt_px = Style ("-mt-px", [ margin_top "-1px" ])
let neg_mr_px = Style ("-mr-px", [ margin_right "-1px" ])
let neg_mb_px = Style ("-mb-px", [ margin_bottom "-1px" ])
let neg_ml_px = Style ("-ml-px", [ margin_left "-1px" ])
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

(* Align content utilities - for multi-line flex/grid containers *)
let content_start = Style ("content-start", [ Css.align_content "flex-start" ])
let content_end = Style ("content-end", [ Css.align_content "flex-end" ])
let content_center = Style ("content-center", [ Css.align_content "center" ])

let content_between =
  Style ("content-between", [ Css.align_content "space-between" ])

let content_around =
  Style ("content-around", [ Css.align_content "space-around" ])

let content_evenly =
  Style ("content-evenly", [ Css.align_content "space-evenly" ])

let content_stretch = Style ("content-stretch", [ Css.align_content "stretch" ])

(* Place content utilities - shorthand for align-content and justify-content in
   Grid *)
let place_content_start =
  Style ("place-content-start", [ Css.place_content "start" ])

let place_content_end = Style ("place-content-end", [ Css.place_content "end" ])

let place_content_center =
  Style ("place-content-center", [ Css.place_content "center" ])

let place_content_between =
  Style ("place-content-between", [ Css.place_content "space-between" ])

let place_content_around =
  Style ("place-content-around", [ Css.place_content "space-around" ])

let place_content_evenly =
  Style ("place-content-evenly", [ Css.place_content "space-evenly" ])

let place_content_stretch =
  Style ("place-content-stretch", [ Css.place_content "stretch" ])

(* Place items utilities - shorthand for align-items and justify-items in
   Grid *)
let place_items_start = Style ("place-items-start", [ Css.place_items "start" ])
let place_items_end = Style ("place-items-end", [ Css.place_items "end" ])

let place_items_center =
  Style ("place-items-center", [ Css.place_items "center" ])

let place_items_stretch =
  Style ("place-items-stretch", [ Css.place_items "stretch" ])

(* Place self utilities - shorthand for align-self and justify-self *)
let place_self_auto = Style ("place-self-auto", [ Css.place_self "auto" ])
let place_self_start = Style ("place-self-start", [ Css.place_self "start" ])
let place_self_end = Style ("place-self-end", [ Css.place_self "end" ])
let place_self_center = Style ("place-self-center", [ Css.place_self "center" ])

let place_self_stretch =
  Style ("place-self-stretch", [ Css.place_self "stretch" ])

(* Align self utilities *)
let self_auto = Style ("self-auto", [ Css.align_self "auto" ])
let self_start = Style ("self-start", [ Css.align_self "flex-start" ])
let self_end = Style ("self-end", [ Css.align_self "flex-end" ])
let self_center = Style ("self-center", [ Css.align_self "center" ])
let self_baseline = Style ("self-baseline", [ Css.align_self "baseline" ])
let self_stretch = Style ("self-stretch", [ Css.align_self "stretch" ])

(* Justify self utilities - for Grid items *)
let justify_self_auto = Style ("justify-self-auto", [ Css.justify_self "auto" ])

let justify_self_start =
  Style ("justify-self-start", [ Css.justify_self "start" ])

let justify_self_end = Style ("justify-self-end", [ Css.justify_self "end" ])

let justify_self_center =
  Style ("justify-self-center", [ Css.justify_self "center" ])

let justify_self_stretch =
  Style ("justify-self-stretch", [ Css.justify_self "stretch" ])

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
  let prefix = if n < 0 then "-" else "" in
  let class_name = prefix ^ "top-" ^ string_of_int (abs n) in
  Style (class_name, [ top (spacing_to_rem n) ])

let right n =
  let prefix = if n < 0 then "-" else "" in
  let class_name = prefix ^ "right-" ^ string_of_int (abs n) in
  Style (class_name, [ right (spacing_to_rem n) ])

let bottom n =
  let prefix = if n < 0 then "-" else "" in
  let class_name = prefix ^ "bottom-" ^ string_of_int (abs n) in
  Style (class_name, [ bottom (spacing_to_rem n) ])

let left n =
  let prefix = if n < 0 then "-" else "" in
  let class_name = prefix ^ "left-" ^ string_of_int (abs n) in
  Style (class_name, [ left (spacing_to_rem n) ])

let z n =
  let class_name = "z-" ^ string_of_int n in
  Style (class_name, [ z_index (string_of_int n) ])

type width = size

let border_internal (w : width) =
  let width_px, class_suffix =
    match w with
    | `None -> ("0", "-0")
    | `Xs -> ("1px", "" (* Default border is 1px *))
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

let border_none = border_internal `None
let border_xs = border_internal `Xs
let border = border_internal `Xs (* Default border is xs/1px *)
let border_sm = border_internal `Sm
let border_md = border_internal `Md
let border_lg = border_internal `Lg
let border_xl = border_internal `Xl
let border_2xl = border_internal `Xl_2
let border_3xl = border_internal `Xl_3
let border_full = border_internal `Full
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

let rounded_internal r =
  let class_name = "rounded-" ^ pp_rounded_suffix r in
  Style (class_name, [ border_radius (rounded_value r) ])

let rounded_none = rounded_internal `None
let rounded_sm = rounded_internal `Sm
let rounded = rounded_internal `Md (* Default rounded *)
let rounded_md = rounded_internal `Md
let rounded_lg = rounded_internal `Lg
let rounded_xl = rounded_internal `Xl
let rounded_2xl = rounded_internal `Xl_2
let rounded_3xl = rounded_internal `Xl_3
let rounded_full = rounded_internal `Full

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

let shadow_internal s =
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

let shadow_none = shadow_internal `None
let shadow_sm = shadow_internal `Sm
let shadow = shadow_internal `Md (* Default shadow *)
let shadow_md = shadow_internal `Md
let shadow_lg = shadow_internal `Lg
let shadow_xl = shadow_internal `Xl
let shadow_2xl = shadow_internal `Xl_2
let shadow_inner = shadow_internal `Inner

let opacity n =
  let class_name = "opacity-" ^ string_of_int n in
  let value =
    if n = 0 then "0"
    else if n = 100 then "1"
    else Pp.float (float_of_int n /. 100.0)
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
  let prefix = if n < 0 then "-" else "" in
  let class_name = prefix ^ "translate-x-" ^ string_of_int (abs n) in
  Style (class_name, [ transform ("translateX(" ^ spacing_to_rem n ^ ")") ])

let translate_y n =
  let prefix = if n < 0 then "-" else "" in
  let class_name = prefix ^ "translate-y-" ^ string_of_int (abs n) in
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

let ring_internal (w : width) =
  let width, class_suffix =
    match w with
    | `None -> ("0", "0")
    | `Xs -> ("1px", "1")
    | `Sm -> ("2px", "2")
    | `Md -> ("3px", "" (* Default ring width is 3px *))
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

let ring_none = ring_internal `None
let ring_xs = ring_internal `Xs
let ring_sm = ring_internal `Sm
let ring = ring_internal `Md (* Default ring *)
let ring_md = ring_internal `Md
let ring_lg = ring_internal `Lg
let ring_xl = ring_internal `Xl

let ring_color color shade =
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
  Group (List.map (fun t -> Modified (Responsive `Sm, t)) styles)

let on_md styles =
  validate_no_nested_responsive styles;
  Group (List.map (fun t -> Modified (Responsive `Md, t)) styles)

let on_lg styles =
  validate_no_nested_responsive styles;
  Group (List.map (fun t -> Modified (Responsive `Lg, t)) styles)

let on_xl styles =
  validate_no_nested_responsive styles;
  Group (List.map (fun t -> Modified (Responsive `Xl, t)) styles)

let on_2xl styles =
  validate_no_nested_responsive styles;
  Group (List.map (fun t -> Modified (Responsive `Xl_2, t)) styles)

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

(* Text transformation utilities *)
let uppercase = Style ("uppercase", [ Css.text_transform "uppercase" ])
let lowercase = Style ("lowercase", [ Css.text_transform "lowercase" ])
let capitalize = Style ("capitalize", [ Css.text_transform "capitalize" ])
let normal_case = Style ("normal-case", [ Css.text_transform "none" ])

(* Text decoration style utilities *)
let underline_solid =
  Style ("underline-solid", [ Css.text_decoration_style "solid" ])

let underline_double =
  Style ("underline-double", [ Css.text_decoration_style "double" ])

let underline_dotted =
  Style ("underline-dotted", [ Css.text_decoration_style "dotted" ])

let underline_dashed =
  Style ("underline-dashed", [ Css.text_decoration_style "dashed" ])

let underline_wavy =
  Style ("underline-wavy", [ Css.text_decoration_style "wavy" ])

(* Text underline offset utilities *)
let underline_offset_auto =
  Style ("underline-offset-auto", [ Css.text_underline_offset "auto" ])

let underline_offset_0 =
  Style ("underline-offset-0", [ Css.text_underline_offset "0" ])

let underline_offset_1 =
  Style ("underline-offset-1", [ Css.text_underline_offset "1px" ])

let underline_offset_2 =
  Style ("underline-offset-2", [ Css.text_underline_offset "2px" ])

let underline_offset_4 =
  Style ("underline-offset-4", [ Css.text_underline_offset "4px" ])

let underline_offset_8 =
  Style ("underline-offset-8", [ Css.text_underline_offset "8px" ])

(* Additional functions needed *)
let aspect_ratio width height =
  let class_name =
    Pp.str [ "aspect-["; Pp.float width; "/"; Pp.float height; "]" ]
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
  let value = Pp.float (float_of_int n /. 100.0) in
  Style (class_name, [ filter ("brightness(" ^ value ^ ")") ])

let contrast n =
  let class_name = "contrast-" ^ string_of_int n in
  let value = Pp.float (float_of_int n /. 100.0) in
  Style (class_name, [ filter ("contrast(" ^ value ^ ")") ])

let blur_internal = function
  | `None -> Style ("blur-none", [ filter "blur(0)" ])
  | `Xs -> Style ("blur-xs", [ filter "blur(2px)" ])
  | `Sm -> Style ("blur-sm", [ filter "blur(4px)" ])
  | `Md -> Style ("blur", [ filter "blur(8px)" ])
  | `Lg -> Style ("blur-lg", [ filter "blur(16px)" ])
  | `Xl -> Style ("blur-xl", [ filter "blur(24px)" ])
  | `Xl_2 -> Style ("blur-2xl", [ filter "blur(40px)" ])
  | `Xl_3 -> Style ("blur-3xl", [ filter "blur(64px)" ])
  | `Full -> Style ("blur-full", [ filter "blur(9999px)" ])

let blur_none = blur_internal `None
let blur_xs = blur_internal `Xs
let blur_sm = blur_internal `Sm
let blur = blur_internal `Md (* Default blur *)
let blur_md = blur_internal `Md
let blur_lg = blur_internal `Lg
let blur_xl = blur_internal `Xl
let blur_2xl = blur_internal `Xl_2
let blur_3xl = blur_internal `Xl_3

let grayscale n =
  let class_name = if n = 0 then "grayscale-0" else "grayscale" in
  let value = Pp.float (float_of_int n /. 100.0) in
  Style (class_name, [ filter ("grayscale(" ^ value ^ ")") ])

let saturate n =
  let class_name = "saturate-" ^ string_of_int n in
  let value = Pp.float (float_of_int n /. 100.0) in
  Style (class_name, [ filter ("saturate(" ^ value ^ ")") ])

let sepia n =
  let class_name = if n = 0 then "sepia-0" else "sepia" in
  let value = Pp.float (float_of_int n /. 100.0) in
  Style (class_name, [ filter ("sepia(" ^ value ^ ")") ])

let invert n =
  let class_name = if n = 0 then "invert-0" else "invert" in
  let value = Pp.float (float_of_int n /. 100.0) in
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
          (Pp.str [ "brightness("; Pp.float (float_of_int n /. 100.); ")" ]);
      ] )

let backdrop_contrast n =
  let class_name = Pp.str [ "backdrop-contrast-"; string_of_int n ] in
  Style
    ( class_name,
      [
        backdrop_filter
          (Pp.str [ "contrast("; Pp.float (float_of_int n /. 100.); ")" ]);
      ] )

let backdrop_opacity n =
  let class_name = Pp.str [ "backdrop-opacity-"; string_of_int n ] in
  Style
    ( class_name,
      [
        backdrop_filter
          (Pp.str [ "opacity("; Pp.float (float_of_int n /. 100.); ")" ]);
      ] )

let backdrop_saturate n =
  let class_name = Pp.str [ "backdrop-saturate-"; string_of_int n ] in
  Style
    ( class_name,
      [
        backdrop_filter
          (Pp.str [ "saturate("; Pp.float (float_of_int n /. 100.); ")" ]);
      ] )

let backdrop_blur_internal = function
  | `None -> Style ("backdrop-blur-none", [ backdrop_filter "blur(0)" ])
  | `Xs -> Style ("backdrop-blur-xs", [ backdrop_filter "blur(2px)" ])
  | `Sm -> Style ("backdrop-blur-sm", [ backdrop_filter "blur(4px)" ])
  | `Md -> Style ("backdrop-blur", [ backdrop_filter "blur(8px)" ])
  | `Lg -> Style ("backdrop-blur-lg", [ backdrop_filter "blur(12px)" ])
  | `Xl -> Style ("backdrop-blur-xl", [ backdrop_filter "blur(16px)" ])
  | `Xl_2 -> Style ("backdrop-blur-2xl", [ backdrop_filter "blur(24px)" ])
  | `Xl_3 -> Style ("backdrop-blur-3xl", [ backdrop_filter "blur(40px)" ])
  | `Full -> Style ("backdrop-blur-full", [ backdrop_filter "blur(9999px)" ])

let backdrop_blur_none = backdrop_blur_internal `None
let backdrop_blur_xs = backdrop_blur_internal `Xs
let backdrop_blur_sm = backdrop_blur_internal `Sm
let backdrop_blur = backdrop_blur_internal `Md (* Default backdrop blur *)
let backdrop_blur_md = backdrop_blur_internal `Md
let backdrop_blur_lg = backdrop_blur_internal `Lg
let backdrop_blur_xl = backdrop_blur_internal `Xl
let backdrop_blur_2xl = backdrop_blur_internal `Xl_2
let backdrop_blur_3xl = backdrop_blur_internal `Xl_3

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
        property "--tw-scale-x" (Pp.float value);
        property "--tw-scale-y" (Pp.float value);
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
let on_peer_checked styles =
  Group (List.map (fun s -> Modified (Peer_checked, s)) styles)

(* ARIA state modifiers *)
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

let from_color color shade =
  let class_name =
    match color with
    | Black | White -> "from-" ^ color_name color
    | _ -> Pp.str [ "from-"; color_name color; "-"; string_of_int shade ]
  in
  (* CSS variables for gradients, skip for now *)
  Style (class_name, [])

let to_color color shade =
  let class_name =
    match color with
    | Black | White -> "to-" ^ color_name color
    | _ -> Pp.str [ "to-"; color_name color; "-"; string_of_int shade ]
  in
  (* CSS variables for gradients, skip for now *)
  Style (class_name, [])

let color_to_string = color_name

(* Class generation functions *)
let rec pp = function
  | Style (class_name, _) -> class_name
  | Prose variant -> Prose.to_class variant
  | Modified (modifier, t) -> (
      let base_class = pp t in
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
      | Responsive breakpoint ->
          string_of_breakpoint breakpoint ^ ":" ^ base_class)
  | Group styles -> styles |> List.map pp |> String.concat " "

let to_classes styles = styles |> List.map pp |> String.concat " "
let classes_to_string = to_classes

(* to_inline_style is now included from Core module *)

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
  let all_variants =
    [
      Prose.Base;
      Prose.Sm;
      Prose.Lg;
      Prose.Xl;
      Prose.Xl2;
      Prose.Gray;
      Prose.Slate;
    ]
  in
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
  | "none" -> Ok shadow_none
  | "sm" -> Ok shadow_sm
  | "md" -> Ok shadow_md
  | "lg" -> Ok shadow_lg
  | "xl" -> Ok shadow_xl
  | "2xl" -> Ok shadow_2xl
  | "inner" -> Ok shadow_inner
  | "" -> Ok shadow (* default shadow *)
  | s -> Error (`Msg ("Unknown shadow size: " ^ s))

let int_of_string_positive name s =
  match int_of_string_opt s with
  | None -> Error (`Msg ("Invalid " ^ name ^ " value: " ^ s))
  | Some n when n >= 0 -> Ok n
  | Some _ -> Error (`Msg (name ^ " must be non-negative: " ^ s))

let int_of_string_bounded name min max s =
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
      let css_value = Pp.str [ Pp.float rem_value; "rem" ] in
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
let modifiers_of_string class_str =
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

(* Helper functions for parsing *)
let spacing_of_string prefix px_var full_var int_fn = function
  | [ p; "px" ] when p = prefix -> Ok px_var
  | [ p; "full" ] when p = prefix -> Ok full_var
  | [ p; n ] when p = prefix ->
      let name =
        if prefix = "p" then "padding"
        else if prefix = "px" then "padding-x"
        else if prefix = "py" then "padding-y"
        else "padding-" ^ String.sub prefix 1 (String.length prefix - 1)
      in
      int_of_string_positive name n >|= int_fn
  | _ -> Error (`Msg "")

let margin_of_string prefix auto_var int_fn = function
  | [ p; "auto" ] when p = prefix -> Ok auto_var
  | [ p; n ] when p = prefix ->
      let name =
        if prefix = "m" then "margin"
        else if prefix = "mx" then "margin-x"
        else if prefix = "my" then "margin-y"
        else "margin-" ^ String.sub prefix 1 (String.length prefix - 1)
      in
      int_of_string_positive name n >|= int_fn
  | _ -> Error (`Msg "")

let width_of_string = function
  | [ "w"; "full" ] -> Ok w_full
  | [ "w"; "screen" ] -> Ok (w' screen)
  | [ "w"; "min" ] -> Ok (w' min)
  | [ "w"; "max" ] -> Ok (w' max)
  | [ "w"; "fit" ] -> Ok w_fit
  | [ "w"; "px" ] -> Ok (w' one_px)
  | [ "w"; "auto" ] -> Ok (w' none)
  | [ "w"; n ] -> int_of_string_positive "width" n >|= w
  | _ -> Error (`Msg "")

let height_of_string = function
  | [ "h"; "full" ] -> Ok h_full
  | [ "h"; "screen" ] -> Ok (h' screen)
  | [ "h"; "min" ] -> Ok (h' min)
  | [ "h"; "max" ] -> Ok (h' max)
  | [ "h"; "fit" ] -> Ok (h' fit)
  | [ "h"; "px" ] -> Ok (h' one_px)
  | [ "h"; "auto" ] -> Ok (h' none)
  | [ "h"; n ] -> int_of_string_positive "height" n >|= h
  | _ -> Error (`Msg "")

let gap_of_string = function
  | [ "gap"; "px" ] -> Ok (gap' `Px)
  | [ "gap"; "full" ] -> Ok (gap' `Full)
  | [ "gap"; n ] -> int_of_string_positive "gap" n >|= gap
  | [ "gap"; "x"; "px" ] -> Ok (gap_x' `Px)
  | [ "gap"; "x"; "full" ] -> Ok (gap_x' `Full)
  | [ "gap"; "x"; n ] -> int_of_string_positive "gap-x" n >|= gap_x
  | [ "gap"; "y"; "px" ] -> Ok (gap_y' `Px)
  | [ "gap"; "y"; "full" ] -> Ok (gap_y' `Full)
  | [ "gap"; "y"; n ] -> int_of_string_positive "gap-y" n >|= gap_y
  | _ -> Error (`Msg "")

let min_width_of_string = function
  | [ "min"; "w"; "full" ] -> Ok (min_w' full)
  | [ "min"; "w"; "min" ] -> Ok (min_w' min)
  | [ "min"; "w"; "max" ] -> Ok (min_w' max)
  | [ "min"; "w"; "fit" ] -> Ok (min_w' fit)
  | [ "min"; "w"; "px" ] -> Ok (min_w' one_px)
  | [ "min"; "w"; n ] -> int_of_string_positive "min-width" n >|= min_w
  | _ -> Error (`Msg "")

let min_height_of_string = function
  | [ "min"; "h"; "full" ] -> Ok (min_h' full)
  | [ "min"; "h"; "screen" ] -> Ok (min_h' screen)
  | [ "min"; "h"; "min" ] -> Ok (min_h' min)
  | [ "min"; "h"; "max" ] -> Ok (min_h' max)
  | [ "min"; "h"; "fit" ] -> Ok (min_h' fit)
  | [ "min"; "h"; "px" ] -> Ok (min_h' one_px)
  | [ "min"; "h"; n ] -> int_of_string_positive "min-height" n >|= min_h
  | _ -> Error (`Msg "")

let max_width_of_string = function
  | [ "max"; "w"; "none" ] -> Ok (max_w' none)
  | [ "max"; "w"; "xs" ] -> Ok (max_w' xs)
  | [ "max"; "w"; "sm" ] -> Ok (max_w' sm)
  | [ "max"; "w"; "md" ] -> Ok (max_w' md)
  | [ "max"; "w"; "lg" ] -> Ok (max_w' lg)
  | [ "max"; "w"; "xl" ] -> Ok (max_w' xl)
  | [ "max"; "w"; "2xl" ] -> Ok (max_w' xl_2)
  | [ "max"; "w"; "3xl" ] -> Ok (max_w' xl_3)
  | [ "max"; "w"; "4xl" ] -> Ok (max_w' xl_4)
  | [ "max"; "w"; "5xl" ] -> Ok (max_w' xl_5)
  | [ "max"; "w"; "6xl" ] -> Ok (max_w' xl_6)
  | [ "max"; "w"; "7xl" ] -> Ok (max_w' xl_7)
  | [ "max"; "w"; "full" ] -> Ok (max_w' full)
  | [ "max"; "w"; "min" ] -> Ok (max_w' min)
  | [ "max"; "w"; "max" ] -> Ok (max_w' max)
  | [ "max"; "w"; "fit" ] -> Ok (max_w' fit)
  | [ "max"; "w"; "px" ] -> Ok (max_w' one_px)
  | [ "max"; "w"; n ] -> int_of_string_positive "max-width" n >|= max_w
  | _ -> Error (`Msg "")

let max_height_of_string = function
  | [ "max"; "h"; "full" ] -> Ok (max_h' full)
  | [ "max"; "h"; "screen" ] -> Ok (max_h' screen)
  | [ "max"; "h"; "min" ] -> Ok (max_h' min)
  | [ "max"; "h"; "max" ] -> Ok (max_h' max)
  | [ "max"; "h"; "fit" ] -> Ok (max_h' fit)
  | [ "max"; "h"; "px" ] -> Ok (max_h' one_px)
  | [ "max"; "h"; "none" ] -> Ok (max_h' none)
  | [ "max"; "h"; n ] -> int_of_string_positive "max-height" n >|= max_h
  | _ -> Error (`Msg "")

(* Parse color-related classes *)
let color_classes_of_string = function
  | [ "bg"; "transparent" ] -> Ok bg_transparent
  | [ "bg"; "current" ] -> Ok bg_current
  | [ "bg"; color; shade ] ->
      color_of_string color >>= fun color ->
      shade_of_string shade >|= fun shade -> bg color shade
  | [ "bg"; color ] -> color_of_string color >|= fun color -> bg color 500
  | [ "text"; "transparent" ] -> Ok text_transparent
  | [ "text"; "current" ] -> Ok text_current
  | [ "text"; "center" ] -> Ok text_center
  | [ "text"; "left" ] -> Ok text_left
  | [ "text"; "right" ] -> Ok text_right
  | [ "text"; "justify" ] -> Ok text_justify
  | [ "text"; color; shade ] ->
      color_of_string color >>= fun color ->
      shade_of_string shade >|= fun shade -> text color shade
  | [ "text"; single ] ->
      (* Try size first, then color *)
      text_size_of_string single
      <|> (color_of_string single >|= fun color -> text color 500)
  | [ "border" ] -> Ok border
  | [ "border"; color; shade ] ->
      color_of_string color >>= fun color ->
      shade_of_string shade >|= fun shade -> border_color color shade
  | [ "border"; "transparent" ] -> Ok border_transparent
  | [ "border"; "current" ] -> Ok border_current
  | [ "border"; color ] -> color_of_string color >|= fun c -> border_color c 500
  | _ -> Error (`Msg "")

(* Parse layout and typography classes *)
let layout_typography_of_string = function
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
  | [ "grid"; "cols"; n ] -> int_of_string_positive "grid cols" n >|= grid_cols
  | [ "grid"; "rows"; n ] -> int_of_string_positive "grid rows" n >|= grid_rows
  | [ "hidden" ] -> Ok hidden
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
  | _ -> Error (`Msg "")

(* Parse utility and effect classes *)
let utility_classes_of_string = function
  | [ "rounded" ] -> Ok rounded
  | [ "rounded"; size ] -> (
      match size with
      | "none" -> Ok rounded_none
      | "sm" -> Ok rounded_sm
      | "md" -> Ok rounded_md
      | "lg" -> Ok rounded_lg
      | "xl" -> Ok rounded_xl
      | "2xl" -> Ok rounded_2xl
      | "3xl" -> Ok rounded_3xl
      | "full" -> Ok rounded_full
      | s -> Error (`Msg ("Unknown rounded size: " ^ s)))
  | [ "shadow" ] -> shadow_of_string ""
  | [ "shadow"; size ] -> shadow_of_string size
  | [ "ring" ] -> Ok ring
  | [ "ring"; "0" ] -> Ok ring_none
  | [ "ring"; "1" ] -> Ok ring_xs
  | [ "ring"; "2" ] -> Ok ring_sm
  | [ "ring"; "3" ] -> Ok ring_md
  | [ "ring"; "4" ] -> Ok ring_lg
  | [ "ring"; "8" ] -> Ok ring_xl
  | [ "relative" ] -> Ok relative
  | [ "absolute" ] -> Ok absolute
  | [ "fixed" ] -> Ok fixed
  | [ "sticky" ] -> Ok sticky
  | [ "static" ] -> Ok static
  | [ "opacity"; n ] -> int_of_string_bounded "Opacity" 0 100 n >|= opacity
  | [ "transition" ] -> Ok transition_all
  | [ "transition"; "none" ] -> Ok transition_none
  | [ "transition"; "all" ] -> Ok transition_all
  | [ "transition"; "colors" ] -> Ok transition_colors
  | [ "transition"; "opacity" ] -> Ok transition_opacity
  | [ "transition"; "shadow" ] -> Ok transition_shadow
  | [ "transition"; "transform" ] -> Ok transition_transform
  | [ "duration"; n ] -> int_of_string_positive "duration" n >|= duration
  | [ "scale"; n ] -> int_of_string_positive "scale" n >|= scale
  | [ "prose" ] -> Ok prose
  | [ "prose"; "sm" ] -> Ok prose_sm
  | [ "prose"; "lg" ] -> Ok prose_lg
  | [ "prose"; "xl" ] -> Ok prose_xl
  | [ "prose"; "2xl" ] -> Ok prose_2xl
  | [ "prose"; "gray" ] -> Ok prose_gray
  | [ "prose"; "slate" ] -> Ok prose_slate
  | _ -> Error (`Msg "")

(* Parse a single class string into a Tw.t *)
let of_string class_str =
  let modifiers, base_class = modifiers_of_string class_str in
  let parts = String.split_on_char '-' base_class in
  let base_result =
    (* Try color classes first *)
    color_classes_of_string parts
    <|>
    (* Try spacing/sizing with helper functions *)
    (match parts with
    | "p" :: _ -> spacing_of_string "p" p_px p_full p parts
    | "px" :: _ -> spacing_of_string "px" px_px px_full px parts
    | "py" :: _ -> spacing_of_string "py" py_px py_full py parts
    | "pt" :: _ -> spacing_of_string "pt" pt_px pt_full pt parts
    | "pr" :: _ -> spacing_of_string "pr" pr_px pr_full pr parts
    | "pb" :: _ -> spacing_of_string "pb" pb_px pb_full pb parts
    | "pl" :: _ -> spacing_of_string "pl" pl_px pl_full pl parts
    | "m" :: _ -> margin_of_string "m" m_auto m parts
    | "mx" :: _ -> margin_of_string "mx" mx_auto mx parts
    | "my" :: _ -> margin_of_string "my" my_auto my parts
    | "mt" :: _ -> margin_of_string "mt" mt_auto mt parts
    | "mr" :: _ -> margin_of_string "mr" mr_auto mr parts
    | "mb" :: _ -> margin_of_string "mb" mb_auto mb parts
    | "ml" :: _ -> margin_of_string "ml" ml_auto ml parts
    | "w" :: _ -> width_of_string parts
    | "h" :: _ -> height_of_string parts
    | "gap" :: _ -> gap_of_string parts
    | "min" :: "w" :: _ -> min_width_of_string parts
    | "min" :: "h" :: _ -> min_height_of_string parts
    | "max" :: "w" :: _ -> max_width_of_string parts
    | "max" :: "h" :: _ -> max_height_of_string parts
    | _ -> Error (`Msg ""))
    <|>
    (* Try layout and typography classes *)
    layout_typography_of_string parts
    <|>
    (* Try utility classes *)
    utility_classes_of_string parts
    <|>
    (* Unknown class *)
    Error (`Msg ("Unknown class: " ^ class_str))
  in
  match base_result with
  | Error _ as e -> e
  | Ok base_style -> Ok (apply_modifiers modifiers base_style)
