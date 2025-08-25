(** A type-safe, ergonomic DSL for Tailwind CSS using nominal types.

    This library takes inspiration from Tailwind CSS v3's utility-first approach
    while leveraging OCaml's type system for compile-time safety. We cherry-pick
    concepts that work well with OCaml and add our own innovations where
    appropriate.

    Key design decisions:
    - Pure OCaml implementation without external CSS dependencies
    - Type-safe API that prevents invalid CSS at compile time
    - Simplified spacing functions that accept integers directly
    - Support for modern CSS features like container queries and 3D transforms
    - Minimal bundle size for js_of_ocaml by avoiding Format module *)

open Css

(** {1 Core Types} *)

(* Import and re-export core types from Core module *)
include Core

(* CSS rule generation from Rules module *)
let to_css = Rules.to_css
let to_inline_style = Rules.to_inline_style

type color = Color.t

(* Re-export Color module *)
module Color = Color

(* Backwards compatibility - keep scale type that hasn't been moved yet *)
type scale = [ spacing | size | `Screen | `Min | `Max | `Fit ]
type max_scale = [ scale | `Xl_4 | `Xl_5 | `Xl_6 | `Xl_7 ]

(* Re-export CSS module *)
module Css = Css

(* Import color constructors from Color module *)
let black = Color.black
let white = Color.white
let gray = Color.gray
let slate = Color.slate
let zinc = Color.zinc
let neutral = Color.neutral
let stone = Color.stone
let red = Color.red
let orange = Color.orange
let amber = Color.amber
let yellow = Color.yellow
let lime = Color.lime
let green = Color.green
let emerald = Color.emerald
let teal = Color.teal
let cyan = Color.cyan
let sky = Color.sky
let blue = Color.blue
let indigo = Color.indigo
let violet = Color.violet
let purple = Color.purple
let fuchsia = Color.fuchsia
let pink = Color.pink
let rose = Color.rose
let hex = Color.hex
let rgb = Color.rgb

(** {1 Helpers} *)

(* These are now imported from Core *)
let style = Core.style
let style_with_vars = Core.style_with_vars

(** {1 Helper Functions} *)

(** Convert hex color to rgb format - now only handles hex strings *)

(** Convert any color to RGB space-separated string format (e.g., "255 0 0") *)

(* int_to_length removed - no longer used *)

(** {1 Public API} *)

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

(* Include only the color application utilities we need *)
include struct
  open Color

  let bg c s = bg c s
  let bg_transparent = bg_transparent
  let bg_current = bg_current
  let bg_black = bg_black
  let bg_white = bg_white
  let bg_gray = bg_gray
  let bg_slate = bg_slate
  let bg_zinc = bg_zinc
  let bg_neutral = bg_neutral
  let bg_stone = bg_stone
  let bg_red = bg_red
  let bg_orange = bg_orange
  let bg_amber = bg_amber
  let bg_yellow = bg_yellow
  let bg_lime = bg_lime
  let bg_green = bg_green
  let bg_emerald = bg_emerald
  let bg_teal = bg_teal
  let bg_cyan = bg_cyan
  let bg_sky = bg_sky
  let bg_blue = bg_blue
  let bg_indigo = bg_indigo
  let bg_violet = bg_violet
  let bg_purple = bg_purple
  let bg_fuchsia = bg_fuchsia
  let bg_pink = bg_pink
  let bg_rose = bg_rose
  let text c s = text c s
  let text_transparent = text_transparent
  let text_current = text_current
  let text_black = text_black
  let text_white = text_white
  let text_gray = text_gray
  let text_slate = text_slate
  let text_zinc = text_zinc
  let text_neutral = text_neutral
  let text_stone = text_stone
  let text_red = text_red
  let text_orange = text_orange
  let text_amber = text_amber
  let text_yellow = text_yellow
  let text_lime = text_lime
  let text_green = text_green
  let text_emerald = text_emerald
  let text_teal = text_teal
  let text_cyan = text_cyan
  let text_sky = text_sky
  let text_blue = text_blue
  let text_indigo = text_indigo
  let text_violet = text_violet
  let text_purple = text_purple
  let text_fuchsia = text_fuchsia
  let text_pink = text_pink
  let text_rose = text_rose
  let border_color c s = border_color c s
  let border_transparent = border_transparent
  let border_current = border_current
  let border_black = border_black
  let border_white = border_white
  let border_gray = border_gray
  let border_slate = border_slate
  let border_zinc = border_zinc
  let border_neutral = border_neutral
  let border_stone = border_stone
  let border_red = border_red
  let border_orange = border_orange
  let border_amber = border_amber
  let border_yellow = border_yellow
  let border_lime = border_lime
  let border_green = border_green
  let border_emerald = border_emerald
  let border_teal = border_teal
  let border_cyan = border_cyan
  let border_sky = border_sky
  let border_blue = border_blue
  let border_indigo = border_indigo
  let border_violet = border_violet
  let border_purple = border_purple
  let border_fuchsia = border_fuchsia
  let border_pink = border_pink
  let border_rose = border_rose
end

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

(* pp_margin_suffix removed - no longer used *)

(* margin_to_length removed - no longer used *)

let rec scale_to_length : scale -> Css.length = function
  | `Screen -> Css.Vh 100.0
  | `Min | `Max | `Fit ->
      Css.Auto (* These need special handling, using Auto for now *)
  | #spacing as s -> Spacing.spacing_to_length s
  | #size as s -> size_to_length s

and size_to_length : size -> Css.length = function
  | `None -> Zero
  | `Xs -> Rem 0.125
  | `Sm -> Rem 0.25
  | `Md -> Rem 0.375
  | `Lg -> Rem 0.5
  | `Xl -> Rem 0.75
  | `Xl_2 -> Rem 1.0
  | `Xl_3 -> Rem 1.5
  | `Full -> Pct 100.0

let max_scale_to_length : max_scale -> Css.length = function
  | `Xl_4 -> Rem 56.0
  | `Xl_5 -> Rem 64.0
  | `Xl_6 -> Rem 72.0
  | `Xl_7 -> Rem 80.0
  | #scale as s -> scale_to_length s

(** {1 Spacing} *)

(* Helper to extract spacing variables from scale types - still needed for
   sizing *)
let scale_vars = function
  | `Rem _ ->
      [] (* The --spacing variable is handled via string parsing in all_vars *)
  | _ -> []

(* Include all spacing utilities *)
include Spacing

(* Special padding values for backward compatibility *)
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

(* Common margin utilities for backward compatibility *)
let m_auto = m' `Auto
let mx_auto = mx' `Auto
let my_auto = my' `Auto
let mt_auto = mt' `Auto
let mr_auto = mr' `Auto
let mb_auto = mb' `Auto
let ml_auto = ml' `Auto

(* Special gap values *)
let gap_px = gap' `Px
let gap_full = gap' `Full

(** {1 Sizing} *)

(* Typed scale functions with ' suffix *)
let w' (s : scale) =
  let class_name = "w-" ^ pp_scale_suffix s in
  let decl =
    match s with
    | `Fit -> Css.width Fit_content
    | `Min -> Css.width Min_content
    | `Max -> Css.width Max_content
    | _ -> Css.width (scale_to_length s)
  in
  style_with_vars class_name [ decl ] (scale_vars s)

let h' (s : scale) =
  let class_name = "h-" ^ pp_scale_suffix s in
  let decl =
    match s with
    | `Fit -> Css.height Fit_content
    | `Min -> Css.height Min_content
    | `Max -> Css.height Max_content
    | _ -> Css.height (scale_to_length s)
  in
  style_with_vars class_name [ decl ] (scale_vars s)

let min_w' (s : scale) =
  let class_name = "min-w-" ^ pp_scale_suffix s in
  let len = scale_to_length s in
  style_with_vars class_name [ Css.min_width len ] (scale_vars s)

let min_h' (s : scale) =
  let class_name = "min-h-" ^ pp_scale_suffix s in
  let len = scale_to_length s in
  style_with_vars class_name [ Css.min_height len ] (scale_vars s)

let max_w' (s : max_scale) =
  let class_name = "max-w-" ^ pp_max_scale_suffix s in
  let len = max_scale_to_length s in
  let vars = match s with #scale as sc -> scale_vars sc | _ -> [] in
  style_with_vars class_name [ Css.max_width len ] vars

let max_h' (s : max_scale) =
  let class_name = "max-h-" ^ pp_max_scale_suffix s in
  let len = max_scale_to_length s in
  let vars = match s with #scale as sc -> scale_vars sc | _ -> [] in
  style_with_vars class_name [ Css.max_height len ] vars

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

(* Include all typography utilities *)
include Typography

(** {1 Layout} *)

(* Include all layout utilities *)
include Layout

(** {1 Borders} *)

(* Include all border utilities *)
include Borders

(** {1 Effects} *)

(* Include all effect utilities *)
include Effects

(** {1 Transforms} *)

(* Include all transform utilities *)
include Transforms

(** {1 Interactivity} *)

(* Include all interactivity utilities *)
include Interactivity

(** {1 Containers} *)

(* Include all container query utilities *)
include Containers

(** {1 Filters} *)

(* Include all filter utilities *)
include Filters

(* Include positioning utilities *)
include Positioning

(* Include animation and transition utilities *)
include Animations

(* Include form utilities *)
include Forms

(** {1 Sizing} *)

(* Sizing utilities are provided through wrapper functions, not included
   directly to avoid naming conflicts *)

(* Modifiers *)

(** {1 Additional Utilities} *)

(* Grid utilities that depend on dynamic values *)
let grid_cols n =
  let class_name = "grid-cols-" ^ string_of_int n in
  style class_name
    [
      Css.Grid.template_columns
        (Css.Repeat (n, Css.Min_max (Css.Px 0, Css.Fr 1.)));
    ]

let grid_rows n =
  let class_name = "grid-rows-" ^ string_of_int n in
  style class_name
    [
      Css.Grid.template_rows (Css.Repeat (n, Css.Min_max (Css.Px 0, Css.Fr 1.)));
    ]

(* Opacity wrapper for int-based API *)
let opacity n =
  let class_name = "opacity-" ^ string_of_int n in
  let value = float_of_int n /. 100.0 in
  style class_name [ Css.opacity value ]

(* These utilities haven't been moved to separate modules yet *)

(* Place content utilities - shorthand for align-content and justify-content in
   Grid *)
let content_stretch = style "content-stretch" [ Css.align_content Stretch ]

let place_content_start =
  style "place-content-start" [ Css.place_content "start" ]

let place_content_end = style "place-content-end" [ Css.place_content "end" ]

let place_content_center =
  style "place-content-center" [ Css.place_content "center" ]

let place_content_between =
  style "place-content-between" [ Css.place_content "space-between" ]

let place_content_around =
  style "place-content-around" [ Css.place_content "space-around" ]

let place_content_evenly =
  style "place-content-evenly" [ Css.place_content "space-evenly" ]

let place_content_stretch =
  style "place-content-stretch" [ Css.place_content "stretch" ]

(* Place items utilities - shorthand for align-items and justify-items in
   Grid *)
let place_items_start = style "place-items-start" [ Css.place_items "start" ]
let place_items_end = style "place-items-end" [ Css.place_items "end" ]
let place_items_center = style "place-items-center" [ Css.place_items "center" ]

let place_items_stretch =
  style "place-items-stretch" [ Css.place_items "stretch" ]

(* Place self utilities - shorthand for align-self and justify-self *)
let place_self_auto = style "place-self-auto" [ Css.place_self "auto" ]
let place_self_start = style "place-self-start" [ Css.place_self "start" ]
let place_self_end = style "place-self-end" [ Css.place_self "end" ]
let place_self_center = style "place-self-center" [ Css.place_self "center" ]
let place_self_stretch = style "place-self-stretch" [ Css.place_self "stretch" ]

(* Justify self utilities - for Grid items *)
let justify_self_auto = style "justify-self-auto" [ Css.justify_self Auto ]
let justify_self_start = style "justify-self-start" [ Css.justify_self Start ]
let justify_self_end = style "justify-self-end" [ Css.justify_self End ]

let justify_self_center =
  style "justify-self-center" [ Css.justify_self Center ]

let justify_self_stretch =
  style "justify-self-stretch" [ Css.justify_self Stretch ]

(* Additional typography utilities *)
let leading_6 = style "leading-6" [ Css.line_height (Rem 1.5) ]
let whitespace_normal = style "whitespace-normal" [ Css.white_space Normal ]
let whitespace_nowrap = style "whitespace-nowrap" [ Css.white_space Nowrap ]
let whitespace_pre = style "whitespace-pre" [ Css.white_space Pre ]

let whitespace_pre_line =
  style "whitespace-pre-line" [ Css.white_space Pre_line ]

let whitespace_pre_wrap =
  style "whitespace-pre-wrap" [ Css.white_space Pre_wrap ]

(* Additional border utilities with custom implementations *)
let border_internal (w : size) =
  let width_len, class_suffix =
    match w with
    | `None -> (Css.Zero, "-0")
    | `Xs -> (Css.Px 1, "" (* Default border is 1px *))
    | `Sm -> (Css.Px 2, "-2")
    | `Md -> (Css.Px 4, "-4" (* For borders, Md maps to 4px *))
    | `Lg -> (Css.Px 4, "-4")
    | `Xl -> (Css.Px 8, "-8")
    | `Xl_2 -> (Css.Px 8, "-8")
    | `Xl_3 -> (Css.Px 8, "-8")
    | `Full -> (Css.Px 8, "-8")
  in
  let class_name = "border" ^ class_suffix in
  style class_name
    [
      Css.border_style (Var (Css.var "tw-border-style"));
      Css.border_width width_len;
    ]

let border_xs = border_internal `Xs
let border_sm = border_internal `Sm
let border_md = border_internal `Md
let border_lg = border_internal `Lg
let border_xl = border_internal `Xl
let border_2xl = border_internal `Xl_2
let border_3xl = border_internal `Xl_3
let border_full = border_internal `Full

let border_none_style =
  style "border-none"
    [
      Css.custom_property "--tw-border-style" "none"; Css.border_style Css.None;
    ]

(* Table utilities *)
let border_collapse = style "border-collapse" [ Css.border_collapse Collapse ]
let border_separate = style "border-separate" [ Css.border_collapse Separate ]

let border_spacing n =
  let class_name = "border-spacing-" ^ string_of_int n in
  style class_name [ Css.border_spacing (Rem (float_of_int n *. 0.25)) ]

let table_auto = style "table-auto" [ Css.table_layout Auto ]
let table_fixed = style "table-fixed" [ Css.table_layout Fixed ]

(* Appearance utilities *)
let appearance_none = style "appearance-none" [ Css.appearance None ]

(* Will-change utilities *)
let will_change_auto = style "will-change-auto" [ Css.will_change "auto" ]

let will_change_scroll =
  style "will-change-scroll" [ Css.will_change "scroll-position" ]

let will_change_contents =
  style "will-change-contents" [ Css.will_change "contents" ]

let will_change_transform =
  style "will-change-transform" [ Css.will_change "transform" ]

(* Contain utilities *)
let contain_none = style "contain-none" [ Css.contain "none" ]
let contain_content = style "contain-content" [ Css.contain "content" ]
let contain_layout = style "contain-layout" [ Css.contain "layout" ]
let contain_paint = style "contain-paint" [ Css.contain "paint" ]
let contain_size = style "contain-size" [ Css.contain "size" ]

type width = size

let pointer_events_none =
  style "pointer-events-none"
    [ Css.pointer_events (None : Css.pointer_events_value) ]

let pointer_events_auto =
  style "pointer-events-auto"
    [ Css.pointer_events (Auto : Css.pointer_events_value) ]

let outline_none = style "outline-none" [ Css.outline "none" ]

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
  style class_name
    [
      Css.custom_property "--tw-ring-color" "rgb(59 130 246 / 0.5)";
      box_shadow
        "var(--tw-ring-offset-shadow,0 0 #0000),var(--tw-ring-shadow,0 0 \
         #0000),var(--tw-shadow,0 0 #0000)";
      Css.custom_property "--tw-ring-shadow" shadow_value;
    ]

let ring_none = ring_internal `None
let ring_xs = ring_internal `Xs
let ring_sm = ring_internal `Sm
let ring = ring_internal `Md (* Default ring *)
let ring_md = ring_internal `Md
let ring_lg = ring_internal `Lg
let ring_xl = ring_internal `Xl

let ring_color color shade =
  let class_name =
    if Color.is_base_color color then Pp.str [ "ring-"; Color.pp color ]
    else Pp.str [ "ring-"; Color.pp color; "-"; string_of_int shade ]
  in
  style class_name []

let isolate = style "isolate" [ Css.isolation Css.Isolate ]

(* Scroll snap utilities *)
let snap_none = style "snap-none" [ Css.scroll_snap_type Css.None ]

let snap_x =
  style "snap-x"
    [ Css.scroll_snap_type (X_var "var(--tw-scroll-snap-strictness)") ]

let snap_y =
  style "snap-y"
    [ Css.scroll_snap_type (Y_var "var(--tw-scroll-snap-strictness)") ]

let snap_both =
  style "snap-both"
    [ Css.scroll_snap_type (Both_var "var(--tw-scroll-snap-strictness)") ]

let snap_mandatory =
  style "snap-mandatory"
    [ Css.custom_property "--tw-scroll-snap-strictness" "mandatory" ]

let snap_proximity =
  style "snap-proximity"
    [ Css.custom_property "--tw-scroll-snap-strictness" "proximity" ]

let snap_start = style "snap-start" [ Css.scroll_snap_align Css.Start ]
let snap_end = style "snap-end" [ Css.scroll_snap_align Css.End ]
let snap_center = style "snap-center" [ Css.scroll_snap_align Css.Center ]
let snap_align_none = style "snap-align-none" [ Css.scroll_snap_align Css.None ]
let snap_normal = style "snap-normal" [ Css.scroll_snap_stop Css.Normal ]
let snap_always = style "snap-always" [ Css.scroll_snap_stop Css.Always ]

let sr_only =
  style "sr-only"
    [
      Css.position Css.Absolute;
      Css.width (Css.Px 1);
      Css.height (Css.Px 1);
      Css.padding Css.Zero;
      Css.margin (Css.Px (-1));
      Css.overflow Css.Hidden;
      Css.clip "rect(0, 0, 0, 0)";
      Css.white_space Nowrap;
      Css.border_width Css.Zero;
    ]

let not_sr_only =
  style "not-sr-only"
    [
      Css.position Css.Static;
      Css.width Css.Auto;
      Css.height Css.Auto;
      Css.padding Css.Zero;
      Css.margin Css.Zero;
      Css.overflow Css.Visible;
      Css.clip "auto";
      Css.white_space Normal;
    ]

(* Responsive and state modifiers *)

let focus_visible =
  style "focus-visible"
    [ outline "2px solid transparent"; outline_offset (Px 2) ]

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

(* New Tailwind v4 modifiers *)
(* The not_ function should be used with other modifiers, not standalone *)
let not_ modifier_fn =
  match modifier_fn with
  | Modified (mod_type, inner) -> Modified (Not mod_type, inner)
  | _ -> modifier_fn (* If not a modifier, return as-is *)

let has selector styles =
  Group (List.map (fun t -> Modified (Has selector, t)) styles)

let group_has selector styles =
  Group (List.map (fun t -> Modified (Group_has selector, t)) styles)

let peer_has selector styles =
  Group (List.map (fun t -> Modified (Peer_has selector, t)) styles)

let on_focus_within styles =
  Group (List.map (fun t -> Modified (Focus_within, t)) styles)

let on_focus_visible styles =
  Group (List.map (fun t -> Modified (Focus_visible, t)) styles)

let motion_safe styles =
  Group (List.map (fun t -> Modified (Motion_safe, t)) styles)

let motion_reduce styles =
  Group (List.map (fun t -> Modified (Motion_reduce, t)) styles)

let contrast_more styles =
  Group (List.map (fun t -> Modified (Contrast_more, t)) styles)

let contrast_less styles =
  Group (List.map (fun t -> Modified (Contrast_less, t)) styles)

let starting styles = Group (List.map (fun t -> Modified (Starting, t)) styles)

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
  style "bg-gradient-to-b"
    [
      Css.background_image
        "linear-gradient(to bottom, var(--tw-gradient-stops))";
    ]

let bg_gradient_to_br =
  style "bg-gradient-to-br"
    [
      Css.background_image
        "linear-gradient(to bottom right, var(--tw-gradient-stops))";
    ]

let bg_gradient_to_t =
  style "bg-gradient-to-t"
    [ Css.background_image "linear-gradient(to top, var(--tw-gradient-stops))" ]

let bg_gradient_to_tr =
  style "bg-gradient-to-tr"
    [
      Css.background_image
        "linear-gradient(to top right, var(--tw-gradient-stops))";
    ]

let bg_gradient_to_r =
  style "bg-gradient-to-r"
    [
      Css.background_image "linear-gradient(to right, var(--tw-gradient-stops))";
    ]

let bg_gradient_to_bl =
  style "bg-gradient-to-bl"
    [
      Css.background_image
        "linear-gradient(to bottom left, var(--tw-gradient-stops))";
    ]

let bg_gradient_to_l =
  style "bg-gradient-to-l"
    [
      Css.background_image "linear-gradient(to left, var(--tw-gradient-stops))";
    ]

let bg_gradient_to_tl =
  style "bg-gradient-to-tl"
    [
      Css.background_image
        "linear-gradient(to top left, var(--tw-gradient-stops))";
    ]

(** Gradient color stops *)
let from_color ?(shade = 500) color =
  let class_name =
    if Color.is_base_color color || Color.is_custom_color color then
      "from-" ^ Color.pp color
    else Pp.str [ "from-"; Color.pp color; "-"; string_of_int shade ]
  in
  let var_str =
    if Color.is_base_color color || Color.is_custom_color color then
      Pp.str [ "var(--color-"; Color.pp color; ")" ]
    else
      Pp.str [ "var(--color-"; Color.pp color; "-"; string_of_int shade; ")" ]
  in
  style class_name
    [
      Css.custom_property "--tw-gradient-from" var_str;
      Css.custom_property "--tw-gradient-stops"
        "var(--tw-gradient-via-stops,var(--tw-gradient-position),var(--tw-gradient-from)var(--tw-gradient-from-position),var(--tw-gradient-to)var(--tw-gradient-to-position))";
    ]

let via_color ?(shade = 500) color =
  let class_name =
    if Color.is_base_color color || Color.is_custom_color color then
      "via-" ^ Color.pp color
    else Pp.str [ "via-"; Color.pp color; "-"; string_of_int shade ]
  in
  let var_str =
    if Color.is_base_color color || Color.is_custom_color color then
      Pp.str [ "var(--color-"; Color.pp color; ")" ]
    else
      Pp.str [ "var(--color-"; Color.pp color; "-"; string_of_int shade; ")" ]
  in
  style class_name
    [
      Css.custom_property "--tw-gradient-via" var_str;
      Css.custom_property "--tw-gradient-via-stops"
        "var(--tw-gradient-position),var(--tw-gradient-from)var(--tw-gradient-from-position),var(--tw-gradient-via)var(--tw-gradient-via-position),var(--tw-gradient-to)var(--tw-gradient-to-position)";
      Css.custom_property "--tw-gradient-stops" "var(--tw-gradient-via-stops)";
    ]

let to_color ?(shade = 500) color =
  let class_name =
    if Color.is_base_color color || Color.is_custom_color color then
      "to-" ^ Color.pp color
    else Pp.str [ "to-"; Color.pp color; "-"; string_of_int shade ]
  in
  let var_str =
    if Color.is_base_color color || Color.is_custom_color color then
      Pp.str [ "var(--color-"; Color.pp color; ")" ]
    else
      Pp.str [ "var(--color-"; Color.pp color; "-"; string_of_int shade; ")" ]
  in
  style class_name
    [
      Css.custom_property "--tw-gradient-to" var_str;
      Css.custom_property "--tw-gradient-stops"
        "var(--tw-gradient-via-stops,var(--tw-gradient-position),var(--tw-gradient-from)var(--tw-gradient-from-position),var(--tw-gradient-to)var(--tw-gradient-to-position))";
    ]

let antialiased =
  style "antialiased"
    [
      Css.webkit_font_smoothing Antialiased;
      Css.moz_osx_font_smoothing Grayscale;
    ]

(* Text transformation utilities *)
let uppercase = style "uppercase" [ Css.text_transform Uppercase ]
let lowercase = style "lowercase" [ Css.text_transform Lowercase ]
let capitalize = style "capitalize" [ Css.text_transform Capitalize ]
let normal_case = style "normal-case" [ Css.text_transform None ]

(* Text decoration style utilities *)
let underline_solid =
  style "underline-solid" [ Css.text_decoration_style Solid ]

let underline_double =
  style "underline-double" [ Css.text_decoration_style Double ]

let underline_dotted =
  style "underline-dotted" [ Css.text_decoration_style Dotted ]

let underline_dashed =
  style "underline-dashed" [ Css.text_decoration_style Dashed ]

let underline_wavy = style "underline-wavy" [ Css.text_decoration_style Wavy ]

(* Text underline offset utilities *)
let underline_offset_auto =
  style "underline-offset-auto" [ Css.text_underline_offset "auto" ]

let underline_offset_0 =
  style "underline-offset-0" [ Css.text_underline_offset "0" ]

let underline_offset_1 =
  style "underline-offset-1" [ Css.text_underline_offset "1px" ]

let underline_offset_2 =
  style "underline-offset-2" [ Css.text_underline_offset "2px" ]

let underline_offset_4 =
  style "underline-offset-4" [ Css.text_underline_offset "4px" ]

let underline_offset_8 =
  style "underline-offset-8" [ Css.text_underline_offset "8px" ]

(* Additional functions needed *)
let aspect_ratio width height =
  let class_name =
    Pp.str [ "aspect-["; Pp.float width; "/"; Pp.float height; "]" ]
  in
  (* aspect-ratio isn't widely supported in CSS yet, skip for now *)
  style class_name []

let clip_path _value =
  (* clip-path is a modern CSS property, skip for now *)
  style "clip-path-custom" []

(* Peer and group utilities *)
let peer = style "peer" [] (* Marker class for peer relationships *)
let group = style "group" [] (* Marker class for group relationships *)

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
let color_to_string = Color.to_name

(* Helper function for breakpoint conversion *)
let string_of_breakpoint = function
  | `Sm -> "sm"
  | `Md -> "md"
  | `Lg -> "lg"
  | `Xl -> "xl"
  | `Xl_2 -> "2xl"

(* Class generation functions *)
let rec pp = function
  | Style { name = class_name; _ } -> class_name
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
          string_of_breakpoint breakpoint ^ ":" ^ base_class
      | Container query ->
          Containers.container_query_to_class_prefix query ^ ":" ^ base_class
      (* New v4 modifiers *)
      | Not _modifier -> "not-" ^ base_class (* Simplified for class names *)
      | Has selector -> "has-[" ^ selector ^ "]:" ^ base_class
      | Group_has selector -> "group-has-[" ^ selector ^ "]:" ^ base_class
      | Peer_has selector -> "peer-has-[" ^ selector ^ "]:" ^ base_class
      | Starting -> "starting:" ^ base_class
      | Focus_within -> "focus-within:" ^ base_class
      | Focus_visible -> "focus-visible:" ^ base_class
      | Motion_safe -> "motion-safe:" ^ base_class
      | Motion_reduce -> "motion-reduce:" ^ base_class
      | Contrast_more -> "contrast-more:" ^ base_class
      | Contrast_less -> "contrast-less:" ^ base_class)
  | Group styles -> styles |> List.map pp |> String.concat " "

let to_classes styles = styles |> List.map pp |> String.concat " "
let classes_to_string = to_classes

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
  Css.stylesheet (List.map (fun r -> Css.Rule r) (root_rule :: all_rules))

(* Line clamp utility function *)
let line_clamp n =
  let class_name = "line-clamp-" ^ string_of_int n in
  if n = 0 then style "line-clamp-none" [ Css.webkit_line_clamp "none" ]
  else style class_name [ Css.webkit_line_clamp (string_of_int n) ]

(* Opacity utilities *)

(* Helper parsing functions *)

let int_of_string_positive name s =
  match int_of_string_opt s with
  | None -> Error (`Msg ("Invalid " ^ name ^ " value: " ^ s))
  | Some n when n >= 0 -> Ok n
  | Some _ -> Error (`Msg (name ^ " must be non-negative: " ^ s))

(* Helper for Result.bind-like operation *)

(* Helper for "try this or else try that" *)
let ( <|> ) r1 r2 = match r1 with Ok _ -> r1 | Error _ -> r2

(* Helper for Result.map-like operation *)
let ( >|= ) r f = match r with Error _ as e -> e | Ok x -> Ok (f x)

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

(* Parse utility classes *)
let utility_classes_of_string = function
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
    Color.color_classes_of_string parts
    <|>
    (* Try spacing utilities *)
    Spacing.of_string parts
    <|>
    (* Try sizing utilities *)
    (match parts with
    | "w" :: _ -> width_of_string parts
    | "h" :: _ -> height_of_string parts
    | "min" :: "w" :: _ -> min_width_of_string parts
    | "min" :: "h" :: _ -> min_height_of_string parts
    | "max" :: "w" :: _ -> max_width_of_string parts
    | "max" :: "h" :: _ -> max_height_of_string parts
    | _ -> Error (`Msg ""))
    <|>
    (* Try layout utilities *)
    Layout.of_string parts
    <|>
    (* Try typography utilities *)
    Typography.of_string parts
    <|>
    (* Try border utilities *)
    Borders.of_string parts
    <|>
    (* Try effects utilities *)
    Effects.of_string parts
    <|>
    (* Try transform utilities *)
    Transforms.of_string parts
    <|>
    (* Try interactivity utilities *)
    Interactivity.of_string parts
    <|>
    (* Try container utilities *)
    Containers.of_string parts
    <|>
    (* Try filter utilities *)
    Filters.of_string parts
    <|>
    (* Try positioning utilities *)
    Positioning.of_string parts
    <|>
    (* Try animation and transition utilities *)
    Animations.of_string parts
    <|>
    (* Try form utilities *)
    Forms.of_string parts
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

(** {1 Module Exports} *)

module Core = Core
module Spacing = Spacing
module Borders = Borders
module Sizing = Sizing
module Layout = Layout
module Typography = Typography
module Effects = Effects
module Transforms = Transforms
module Interactivity = Interactivity
module Containers = Containers
module Filters = Filters
module Positioning = Positioning
module Animations = Animations
module Forms = Forms
