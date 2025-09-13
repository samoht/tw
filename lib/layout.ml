(** Layout utilities for basic display, positioning, and object properties

    What's included:
    - Display: `block`, `inline`, `inline-block`, `hidden`.
    - Position utilities: `static`, `relative`, `absolute`, `fixed`, `sticky`.
    - Object-fit/position, overflow, z-index.
    - Table utilities for border-collapse, border-spacing, table-layout.

    What's not:
    - Flex/grid utilities: see Flow module.
    - Some niche shorthands are omitted; prefer the typed helpers exposed in
      `Css` rather than raw properties.

    Parsing contract (`of_string`):
    - Accepted tokens include a subset of Tailwind layout utilities, e.g.:
      ["block"], ["inline"], ["hidden"], ["static"], ["relative"], etc.
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
(* flow_root not supported by Css module *)

(* Screen-reader utilities placed in Display section per request *)
let sr_only =
  style "sr-only"
    [
      position Absolute;
      width (Px 1.);
      height (Px 1.);
      padding [ Zero ];
      margin [ Px (-1.) ];
      overflow Hidden;
      clip "rect(0, 0, 0, 0)";
      white_space Nowrap;
      border_width Zero;
    ]

let not_sr_only =
  style "not-sr-only"
    [
      position Static;
      width Auto;
      height Auto;
      padding [ Zero ];
      margin [ Zero ];
      overflow Visible;
      clip "auto";
      white_space Normal;
    ]

(** {1 Positioning Utilities} *)

let static = style "static" [ position Static ]
let relative = style "relative" [ position Relative ]
let absolute = style "absolute" [ position Absolute ]
let fixed = style "fixed" [ position Fixed ]
let sticky = style "sticky" [ position Sticky ]

(** {1 Visibility Utilities} *)

let visible = style "visible" [ visibility Visible ]
let invisible = style "invisible" [ visibility Hidden ]
let collapse = style "collapse" [ visibility Collapse ]

(** {1 Isolation} *)

let isolate = style "isolate" [ isolation Isolate ]

(** {1 Overflow Utilities} *)

let overflow_auto = style "overflow-auto" [ overflow Auto ]
let overflow_hidden = style "overflow-hidden" [ overflow Hidden ]
let overflow_clip = style "overflow-clip" [ overflow Clip ]
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

let z_0 = style "z-0" [ z_index (Index 0) ]
let z_10 = style "z-10" [ z_index (Index 10) ]
let z_20 = style "z-20" [ z_index (Index 20) ]
let z_30 = style "z-30" [ z_index (Index 30) ]
let z_40 = style "z-40" [ z_index (Index 40) ]
let z_50 = style "z-50" [ z_index (Index 50) ]
let z_auto = style "z-auto" [ z_index_auto ]

(** {1 Object Fit Utilities} *)

let object_contain = style "object-contain" [ object_fit Contain ]
let object_cover = style "object-cover" [ object_fit Cover ]
let object_fill = style "object-fill" [ object_fit Fill ]
let object_none = style "object-none" [ object_fit None ]
let object_scale_down = style "object-scale-down" [ object_fit Scale_down ]

(** {1 Object Position Utilities} *)

let object_center =
  style "object-center" [ object_position (XY (Center, Center)) ]

let object_top = style "object-top" [ object_position (XY (Center, Top)) ]

let object_bottom =
  style "object-bottom" [ object_position (XY (Center, Bottom)) ]

let object_left = style "object-left" [ object_position (XY (Left, Center)) ]
let object_right = style "object-right" [ object_position (XY (Right, Center)) ]

(** {1 Table Utilities} *)

let border_collapse = style "border-collapse" [ Css.border_collapse Collapse ]
let border_separate = style "border-separate" [ Css.border_collapse Separate ]

let border_spacing n =
  let class_name = "border-spacing-" ^ string_of_int n in
  style class_name [ Css.border_spacing (Rem (float_of_int n *. 0.25)) ]

let table_auto = style "table-auto" [ Css.table_layout Auto ]
let table_fixed = style "table-fixed" [ Css.table_layout Fixed ]

(** {1 Parsing Functions} *)

let of_string = function
  | [ "block" ] -> Ok block
  | [ "inline" ] -> Ok inline
  | [ "inline"; "block" ] -> Ok inline_block
  | [ "hidden" ] -> Ok hidden
  | [ "static" ] -> Ok static
  | [ "relative" ] -> Ok relative
  | [ "absolute" ] -> Ok absolute
  | [ "fixed" ] -> Ok fixed
  | [ "sticky" ] -> Ok sticky
  | [ "visible" ] -> Ok visible
  | [ "invisible" ] -> Ok invisible
  | [ "collapse" ] -> Ok collapse
  | [ "isolate" ] -> Ok isolate
  | [ "overflow"; "auto" ] -> Ok overflow_auto
  | [ "overflow"; "hidden" ] -> Ok overflow_hidden
  | [ "overflow"; "clip" ] -> Ok overflow_clip
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
  | [ "z"; "auto" ] -> Ok z_auto
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
  | [ "sr-only" ] -> Ok sr_only
  | [ "not-sr-only" ] -> Ok not_sr_only
  | _ -> Error (`Msg "Not a layout utility")
