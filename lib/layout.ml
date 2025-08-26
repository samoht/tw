(** Layout utilities for display, flexbox, grid, and positioning

    What's included:
    - Display: `block`, `inline`, `inline-block`, `hidden`, `flex`,
      `inline-flex`, `grid`, `inline-grid`.
    - Flexbox: direction, wrap, flex, grow/shrink, alignment.
    - Grid: dynamic template columns/rows via `grid_cols`, `grid_rows`.
    - Position shorthands: see Positioning module for offsets.
    - Object-fit/position, overflow, z-index.

    What's not:
    - Some niche shorthands are omitted; prefer the typed helpers exposed in
      `Css` (including `place_*` variants) rather than raw properties.

    Parsing contract (`of_string`):
    - Accepted tokens include a subset of Tailwind layout utilities, e.g.: *
      ["flex"], ["flex"; "row"], ["flex"; "wrap"], ["flex"; "1"],
      ["flex"; "none"] * ["items"; {"start"|"center"|...}], ["justify"; {...}],
      ["content"; {...}] * ["grid"], ["grid"; "cols"; n], ["grid"; "rows"; n]
      with non‑negative ints * ["hidden"], ["block"], ["inline"; "grid"], etc.
    - Errors: returns `Error (`Msg "Not a layout utility")` for unknown
      patterns. *)

open Core
open Css
module Parse = Parse

(** {1 Display Utilities} *)

let block = style "block" [ display Block ]
let inline = style "inline" [ display Inline ]
let inline_block = style "inline-block" [ display Inline_block ]
let hidden = style "hidden" [ display None ]
let flex = style "flex" [ display Flex ]
let inline_flex = style "inline-flex" [ display Inline_flex ]
let grid = style "grid" [ display Grid ]
let inline_grid = style "inline-grid" [ display Inline_grid ]
(* flow_root not supported by Css module *)

(** {1 Flexbox Utilities} *)

(* Flex Direction *)
let flex_row = style "flex-row" [ Flex.direction Row ]
let flex_row_reverse = style "flex-row-reverse" [ Flex.direction Row_reverse ]
let flex_col = style "flex-col" [ Flex.direction Column ]

let flex_col_reverse =
  style "flex-col-reverse" [ Flex.direction Column_reverse ]

(* Flex Wrap *)
let flex_wrap = style "flex-wrap" [ Flex.wrap Wrap ]
let flex_wrap_reverse = style "flex-wrap-reverse" [ Flex.wrap Wrap_reverse ]
let flex_nowrap = style "flex-nowrap" [ Flex.wrap Nowrap ]

(* Flex *)
let flex_1 = style "flex-1" [ Flex.flex (Grow 1.0) ]
let flex_auto = style "flex-auto" [ Flex.flex Auto ]
let flex_initial = style "flex-initial" [ Flex.flex Initial ]
let flex_none = style "flex-none" [ Flex.flex None ]

(* Flex Grow *)
let flex_grow = style "flex-grow" [ Flex.grow 1.0 ]
let flex_grow_0 = style "flex-grow-0" [ Flex.grow 0.0 ]

(* Flex Shrink *)
let flex_shrink = style "flex-shrink" [ Flex.shrink 1.0 ]
let flex_shrink_0 = style "flex-shrink-0" [ Flex.shrink 0.0 ]

(** {1 Alignment Utilities} *)

(* Align Items *)
let items_start = style "items-start" [ align_items Flex_start ]
let items_end = style "items-end" [ align_items Flex_end ]
let items_center = style "items-center" [ align_items Center ]
let items_baseline = style "items-baseline" [ align_items Baseline ]
let items_stretch = style "items-stretch" [ align_items Stretch ]

(* Justify Content *)
let justify_start = style "justify-start" [ justify_content Flex_start ]
let justify_end = style "justify-end" [ justify_content Flex_end ]
let justify_center = style "justify-center" [ justify_content Center ]
let justify_between = style "justify-between" [ justify_content Space_between ]
let justify_around = style "justify-around" [ justify_content Space_around ]
let justify_evenly = style "justify-evenly" [ justify_content Space_evenly ]

(* Align Content *)
let content_start = style "content-start" [ align_content Flex_start ]
let content_end = style "content-end" [ align_content Flex_end ]
let content_center = style "content-center" [ align_content Center ]
let content_between = style "content-between" [ align_content Space_between ]
let content_around = style "content-around" [ align_content Space_around ]
let content_evenly = style "content-evenly" [ align_content Space_evenly ]
let content_stretch = style "content-stretch" [ align_content Stretch ]

(* Align Self *)
let self_auto = style "self-auto" [ align_self Auto ]
let self_start = style "self-start" [ align_self Flex_start ]
let self_end = style "self-end" [ align_self Flex_end ]
let self_center = style "self-center" [ align_self Center ]
let self_baseline = style "self-baseline" [ align_self Baseline ]
let self_stretch = style "self-stretch" [ align_self Stretch ]

(* Justify Self - not supported by Css module *)
let justify_self_auto = style "justify-self-auto" [ justify_self Auto ]
let justify_self_start = style "justify-self-start" [ justify_self Start ]
let justify_self_end = style "justify-self-end" [ justify_self End ]
let justify_self_center = style "justify-self-center" [ justify_self Center ]
let justify_self_stretch = style "justify-self-stretch" [ justify_self Stretch ]

(* Place Content *)
let place_content_start = style "place-content-start" [ place_content Start ]
let place_content_end = style "place-content-end" [ place_content End ]
let place_content_center = style "place-content-center" [ place_content Center ]

let place_content_between =
  style "place-content-between" [ place_content Space_between ]

let place_content_around =
  style "place-content-around" [ place_content Space_around ]

let place_content_evenly =
  style "place-content-evenly" [ place_content Space_evenly ]

let place_content_stretch =
  style "place-content-stretch" [ place_content Stretch ]

(* Place Items *)
let place_items_start = style "place-items-start" [ place_items Start ]
let place_items_end = style "place-items-end" [ place_items End ]
let place_items_center = style "place-items-center" [ place_items Center ]
let place_items_stretch = style "place-items-stretch" [ place_items Stretch ]

(* Place Self *)
let place_self_auto = style "place-self-auto" [ place_self_v `Auto ]
let place_self_start = style "place-self-start" [ place_self_v `Start ]
let place_self_end = style "place-self-end" [ place_self_v `End ]
let place_self_center = style "place-self-center" [ place_self_v `Center ]
let place_self_stretch = style "place-self-stretch" [ place_self_v `Stretch ]

(** {1 Positioning Utilities} *)

let static = style "static" [ position Static ]
let relative = style "relative" [ position Relative ]
let absolute = style "absolute" [ position Absolute ]
let fixed = style "fixed" [ position Fixed ]
let sticky = style "sticky" [ position Sticky ]

(** {1 Overflow Utilities} *)

let overflow_auto = style "overflow-auto" [ overflow Auto ]
let overflow_hidden = style "overflow-hidden" [ overflow Hidden ]
let overflow_visible = style "overflow-visible" [ overflow Visible ]
let overflow_scroll = style "overflow-scroll" [ overflow Scroll ]
let overflow_x_auto = style "overflow-x-auto" [ overflow_x Auto ]
let overflow_x_hidden = style "overflow-x-hidden" [ overflow_x Hidden ]
let overflow_x_visible = style "overflow-x-visible" [ overflow_x Visible ]
let overflow_x_scroll = style "overflow-x-scroll" [ overflow_x Scroll ]
let overflow_y_auto = style "overflow-y-auto" [ overflow_y Auto ]
let overflow_y_hidden = style "overflow-y-hidden" [ overflow_y Hidden ]
let overflow_y_visible = style "overflow-y-visible" [ overflow_y Visible ]
let overflow_y_scroll = style "overflow-y-scroll" [ overflow_y Scroll ]

(** {1 Z-Index Utilities} *)

let z_0 = style "z-0" [ z_index 0 ]
let z_10 = style "z-10" [ z_index 10 ]
let z_20 = style "z-20" [ z_index 20 ]
let z_30 = style "z-30" [ z_index 30 ]
let z_40 = style "z-40" [ z_index 40 ]
let z_50 = style "z-50" [ z_index 50 ]
(* z_auto not directly supported - would need z-index: auto property *)

(** {1 Object Fit Utilities} *)

let object_contain = style "object-contain" [ object_fit Contain ]
let object_cover = style "object-cover" [ object_fit Cover ]
let object_fill = style "object-fill" [ object_fit Fill ]
let object_none = style "object-none" [ object_fit None ]
let object_scale_down = style "object-scale-down" [ object_fit Scale_down ]

(** {1 Object Position Utilities} *)

let object_center = style "object-center" [ object_position "center" ]
let object_top = style "object-top" [ object_position "top" ]
let object_bottom = style "object-bottom" [ object_position "bottom" ]
let object_left = style "object-left" [ object_position "left" ]
let object_right = style "object-right" [ object_position "right" ]

(** {1 Table Utilities} *)

let border_collapse = style "border-collapse" [ Css.border_collapse Collapse ]
let border_separate = style "border-separate" [ Css.border_collapse Separate ]

let border_spacing n =
  let class_name = "border-spacing-" ^ string_of_int n in
  style class_name [ Css.border_spacing (Rem (float_of_int n *. 0.25)) ]

let table_auto = style "table-auto" [ Css.table_layout Auto ]
let table_fixed = style "table-fixed" [ Css.table_layout Fixed ]

(** {1 Grid Utilities} *)

let grid_cols n =
  let class_name = "grid-cols-" ^ string_of_int n in
  style class_name
    [
      custom_property "--grid-cols" (string_of_int n);
      Grid.template_columns (Repeat (n, Min_max (Px 0, Fr 1.0)));
    ]

let grid_rows n =
  let class_name = "grid-rows-" ^ string_of_int n in
  style class_name
    [
      custom_property "--grid-rows" (string_of_int n);
      Grid.template_rows (Repeat (n, Min_max (Px 0, Fr 1.0)));
    ]

(** {1 Parsing Functions} *)

let ( >|= ) = Parse.( >|= )

let of_string = function
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
  | [ "grid"; "cols"; n ] -> Parse.int_pos ~name:"grid cols" n >|= grid_cols
  | [ "grid"; "rows"; n ] -> Parse.int_pos ~name:"grid rows" n >|= grid_rows
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
  | [ "static" ] -> Ok static
  | [ "relative" ] -> Ok relative
  | [ "absolute" ] -> Ok absolute
  | [ "fixed" ] -> Ok fixed
  | [ "sticky" ] -> Ok sticky
  | [ "overflow"; "auto" ] -> Ok overflow_auto
  | [ "overflow"; "hidden" ] -> Ok overflow_hidden
  | [ "overflow"; "visible" ] -> Ok overflow_visible
  | [ "overflow"; "scroll" ] -> Ok overflow_scroll
  | [ "overflow"; "x"; "auto" ] -> Ok overflow_x_auto
  | [ "overflow"; "x"; "hidden" ] -> Ok overflow_x_hidden
  | [ "overflow"; "x"; "visible" ] -> Ok overflow_x_visible
  | [ "overflow"; "x"; "scroll" ] -> Ok overflow_x_scroll
  | [ "overflow"; "y"; "auto" ] -> Ok overflow_y_auto
  | [ "overflow"; "y"; "hidden" ] -> Ok overflow_y_hidden
  | [ "overflow"; "y"; "visible" ] -> Ok overflow_y_visible
  | [ "overflow"; "y"; "scroll" ] -> Ok overflow_y_scroll
  | [ "z"; "0" ] -> Ok z_0
  | [ "z"; "10" ] -> Ok z_10
  | [ "z"; "20" ] -> Ok z_20
  | [ "z"; "30" ] -> Ok z_30
  | [ "z"; "40" ] -> Ok z_40
  | [ "z"; "50" ] -> Ok z_50
  | [ "object"; "contain" ] -> Ok object_contain
  | [ "object"; "cover" ] -> Ok object_cover
  | [ "object"; "fill" ] -> Ok object_fill
  | [ "object"; "none" ] -> Ok object_none
  | [ "object"; "scale"; "down" ] -> Ok object_scale_down
  | [ "object"; "center" ] -> Ok object_center
  | [ "object"; "top" ] -> Ok object_top
  | [ "object"; "bottom" ] -> Ok object_bottom
  | [ "object"; "left" ] -> Ok object_left
  | [ "object"; "right" ] -> Ok object_right
  | _ -> Error (`Msg "Not a layout utility")
