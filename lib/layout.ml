(** Layout utilities for basic display, positioning, and object properties *)

open Style
open Css

type t =
  | (* Display *)
    Block
  | Inline
  | Inline_block
  | Hidden
  | Sr_only
  | Not_sr_only
  | (* Visibility *)
    Visible
  | Invisible
  | Collapse
  | (* Isolation *)
    Isolate
  | (* Overflow *)
    Overflow_auto
  | Overflow_hidden
  | Overflow_clip
  | Overflow_visible
  | Overflow_scroll
  | Overflow_x_auto
  | Overflow_x_hidden
  | Overflow_x_visible
  | Overflow_x_scroll
  | Overflow_y_auto
  | Overflow_y_hidden
  | Overflow_y_visible
  | Overflow_y_scroll
  | (* Z-index *)
    Z_0
  | Z_10
  | Z_20
  | Z_30
  | Z_40
  | Z_50
  | Z_auto
  | (* Object fit *)
    Object_contain
  | Object_cover
  | Object_fill
  | Object_none
  | Object_scale_down
  | (* Object position *)
    Object_center
  | Object_top
  | Object_bottom
  | Object_left
  | Object_right
  | (* Table *)
    Border_collapse
  | Border_separate
  | Border_spacing of int
  | Table_auto
  | Table_fixed

type Utility.base += Layout of t

let wrap x = Layout x
let unwrap = function Layout x -> Some x | _ -> None
let base x = Utility.base (wrap x)

let suborder = function
  (* Display utilities - alphabetically ordered *)
  | Block -> 0
  | Inline -> 1
  | Inline_block -> 2
  | Hidden -> 3
  (* Overflow utilities - alphabetically ordered *)
  | Overflow_auto -> 4
  | Overflow_clip -> 5
  | Overflow_hidden -> 6
  | Overflow_scroll -> 7
  | Overflow_visible -> 8
  | Overflow_x_auto -> 9
  | Overflow_x_hidden -> 10
  | Overflow_x_scroll -> 11
  | Overflow_x_visible -> 12
  | Overflow_y_auto -> 13
  | Overflow_y_hidden -> 14
  | Overflow_y_scroll -> 15
  | Overflow_y_visible -> 16
  (* Visibility *)
  | Visible -> 100
  | Invisible -> 101
  | Collapse -> 102
  (* Isolation *)
  | Isolate -> 200
  (* Z-index *)
  | Z_0 -> 300
  | Z_10 -> 301
  | Z_20 -> 302
  | Z_30 -> 303
  | Z_40 -> 304
  | Z_50 -> 305
  | Z_auto -> 306
  (* Object fit *)
  | Object_contain -> 400
  | Object_cover -> 401
  | Object_fill -> 402
  | Object_none -> 403
  | Object_scale_down -> 404
  (* Object position *)
  | Object_center -> 500
  | Object_top -> 501
  | Object_bottom -> 502
  | Object_left -> 503
  | Object_right -> 504
  (* Table *)
  | Border_collapse -> 600
  | Border_separate -> 601
  | Border_spacing _ -> 602
  | Table_auto -> 603
  | Table_fixed -> 604
  (* Screen reader *)
  | Sr_only -> 700
  | Not_sr_only -> 701

(** {1 Style Generation} *)

let to_style = function
  | Block -> style "block" [ display Block ]
  | Inline -> style "inline" [ display Inline ]
  | Inline_block -> style "inline-block" [ display Inline_block ]
  | Hidden -> style "hidden" [ display None ]
  | Sr_only ->
      style "sr-only"
        [
          Css.position Absolute;
          width (Px 1.);
          height (Px 1.);
          padding [ Zero ];
          margin [ Px (-1.) ];
          overflow Hidden;
          clip "rect(0, 0, 0, 0)";
          white_space Nowrap;
          border_width Zero;
        ]
  | Not_sr_only ->
      style "not-sr-only"
        [
          Css.position Static;
          width Auto;
          height Auto;
          padding [ Zero ];
          margin [ Zero ];
          overflow Visible;
          clip "auto";
          white_space Normal;
        ]
  | Visible -> style "visible" [ visibility Visible ]
  | Invisible -> style "invisible" [ visibility Hidden ]
  | Collapse -> style "collapse" [ visibility Collapse ]
  | Isolate -> style "isolate" [ isolation Isolate ]
  | Overflow_auto -> style "overflow-auto" [ overflow Auto ]
  | Overflow_hidden -> style "overflow-hidden" [ overflow Hidden ]
  | Overflow_clip -> style "overflow-clip" [ overflow Clip ]
  | Overflow_visible -> style "overflow-visible" [ overflow Visible ]
  | Overflow_scroll -> style "overflow-scroll" [ overflow Scroll ]
  | Overflow_x_auto -> style "overflow-x-auto" [ overflow_x Auto ]
  | Overflow_x_hidden -> style "overflow-x-hidden" [ overflow_x Hidden ]
  | Overflow_x_visible -> style "overflow-x-visible" [ overflow_x Visible ]
  | Overflow_x_scroll -> style "overflow-x-scroll" [ overflow_x Scroll ]
  | Overflow_y_auto -> style "overflow-y-auto" [ overflow_y Auto ]
  | Overflow_y_hidden -> style "overflow-y-hidden" [ overflow_y Hidden ]
  | Overflow_y_visible -> style "overflow-y-visible" [ overflow_y Visible ]
  | Overflow_y_scroll -> style "overflow-y-scroll" [ overflow_y Scroll ]
  | Z_0 -> style "z-0" [ z_index (Index 0) ]
  | Z_10 -> style "z-10" [ z_index (Index 10) ]
  | Z_20 -> style "z-20" [ z_index (Index 20) ]
  | Z_30 -> style "z-30" [ z_index (Index 30) ]
  | Z_40 -> style "z-40" [ z_index (Index 40) ]
  | Z_50 -> style "z-50" [ z_index (Index 50) ]
  | Z_auto -> style "z-auto" [ z_index_auto ]
  | Object_contain -> style "object-contain" [ object_fit Contain ]
  | Object_cover -> style "object-cover" [ object_fit Cover ]
  | Object_fill -> style "object-fill" [ object_fit Fill ]
  | Object_none -> style "object-none" [ object_fit None ]
  | Object_scale_down -> style "object-scale-down" [ object_fit Scale_down ]
  | Object_center -> style "object-center" [ object_position Center ]
  | Object_top -> style "object-top" [ object_position Center_top ]
  | Object_bottom -> style "object-bottom" [ object_position Center_bottom ]
  | Object_left -> style "object-left" [ object_position Left_center ]
  | Object_right -> style "object-right" [ object_position Right_center ]
  | Border_collapse -> style "border-collapse" [ Css.border_collapse Collapse ]
  | Border_separate -> style "border-separate" [ Css.border_collapse Separate ]
  | Border_spacing n ->
      let class_name = "border-spacing-" ^ string_of_int n in
      style class_name [ Css.border_spacing (Rem (float_of_int n *. 0.25)) ]
  | Table_auto -> style "table-auto" [ Css.table_layout Auto ]
  | Table_fixed -> style "table-fixed" [ Css.table_layout Fixed ]

(** {1 Parsing Functions} *)

let err_not_utility = Error (`Msg "Not a layout utility")

let of_string = function
  | [ "block" ] -> Ok Block
  | [ "inline" ] -> Ok Inline
  | [ "inline"; "block" ] -> Ok Inline_block
  | [ "hidden" ] -> Ok Hidden
  | [ "visible" ] -> Ok Visible
  | [ "invisible" ] -> Ok Invisible
  | [ "collapse" ] -> Ok Collapse
  | [ "isolate" ] -> Ok Isolate
  | [ "overflow"; "auto" ] -> Ok Overflow_auto
  | [ "overflow"; "hidden" ] -> Ok Overflow_hidden
  | [ "overflow"; "clip" ] -> Ok Overflow_clip
  | [ "overflow"; "visible" ] -> Ok Overflow_visible
  | [ "overflow"; "scroll" ] -> Ok Overflow_scroll
  | [ "overflow"; "x"; "auto" ] -> Ok Overflow_x_auto
  | [ "overflow"; "x"; "hidden" ] -> Ok Overflow_x_hidden
  | [ "overflow"; "x"; "visible" ] -> Ok Overflow_x_visible
  | [ "overflow"; "x"; "scroll" ] -> Ok Overflow_x_scroll
  | [ "overflow"; "y"; "auto" ] -> Ok Overflow_y_auto
  | [ "overflow"; "y"; "hidden" ] -> Ok Overflow_y_hidden
  | [ "overflow"; "y"; "visible" ] -> Ok Overflow_y_visible
  | [ "overflow"; "y"; "scroll" ] -> Ok Overflow_y_scroll
  | [ "z"; "0" ] -> Ok Z_0
  | [ "z"; "10" ] -> Ok Z_10
  | [ "z"; "20" ] -> Ok Z_20
  | [ "z"; "30" ] -> Ok Z_30
  | [ "z"; "40" ] -> Ok Z_40
  | [ "z"; "50" ] -> Ok Z_50
  | [ "z"; "auto" ] -> Ok Z_auto
  | [ "object"; "contain" ] -> Ok Object_contain
  | [ "object"; "cover" ] -> Ok Object_cover
  | [ "object"; "fill" ] -> Ok Object_fill
  | [ "object"; "none" ] -> Ok Object_none
  | [ "object"; "scale"; "down" ] -> Ok Object_scale_down
  | [ "object"; "center" ] -> Ok Object_center
  | [ "object"; "top" ] -> Ok Object_top
  | [ "object"; "bottom" ] -> Ok Object_bottom
  | [ "object"; "left" ] -> Ok Object_left
  | [ "object"; "right" ] -> Ok Object_right
  | [ "sr-only" ] -> Ok Sr_only
  | [ "not-sr-only" ] -> Ok Not_sr_only
  | [ "border"; "collapse" ] -> Ok Border_collapse
  | [ "border"; "separate" ] -> Ok Border_separate
  | [ "border"; "spacing"; n ] -> (
      match int_of_string_opt n with
      | Some i -> Ok (Border_spacing i)
      | None -> err_not_utility)
  | [ "table"; "auto" ] -> Ok Table_auto
  | [ "table"; "fixed" ] -> Ok Table_fixed
  | _ -> err_not_utility

(** {1 Public API - Utility Values} *)

(* These provide the public API for layout utilities *)
let block = base Block
let inline = base Inline
let inline_block = base Inline_block
let hidden = base Hidden
let visible = base Visible
let invisible = base Invisible
let collapse = base Collapse
let isolate = base Isolate
let overflow_auto = base Overflow_auto
let overflow_hidden = base Overflow_hidden
let overflow_clip = base Overflow_clip
let overflow_visible = base Overflow_visible
let overflow_scroll = base Overflow_scroll
let overflow_x_auto = base Overflow_x_auto
let overflow_x_hidden = base Overflow_x_hidden
let overflow_x_visible = base Overflow_x_visible
let overflow_x_scroll = base Overflow_x_scroll
let overflow_y_auto = base Overflow_y_auto
let overflow_y_hidden = base Overflow_y_hidden
let overflow_y_visible = base Overflow_y_visible
let overflow_y_scroll = base Overflow_y_scroll
let z_0 = base Z_0
let z_10 = base Z_10
let z_20 = base Z_20
let z_30 = base Z_30
let z_40 = base Z_40
let z_50 = base Z_50
let z_auto = base Z_auto
let object_contain = base Object_contain
let object_cover = base Object_cover
let object_fill = base Object_fill
let object_none = base Object_none
let object_scale_down = base Object_scale_down
let object_center = base Object_center
let object_top = base Object_top
let object_bottom = base Object_bottom
let object_left = base Object_left
let object_right = base Object_right
let sr_only = base Sr_only
let not_sr_only = base Not_sr_only
let border_collapse = base Border_collapse
let border_separate = base Border_separate
let border_spacing n = base (Border_spacing n)
let table_auto = base Table_auto
let table_fixed = base Table_fixed

(** Priority for layout utilities *)
let priority = 15

let () =
  Utility.register ~wrap ~unwrap { to_style; priority; suborder; of_string }

module Handler = struct
  type nonrec t = t

  let of_string = of_string
  let suborder = suborder
  let to_style = to_style
  let order x = (priority, suborder x)
end

module Private = Handler
