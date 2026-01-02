(** Layout utilities for basic display, positioning, and object properties *)

(** Screen reader utilities handler - priority 0 to appear first *)
module Screen_reader_handler = struct
  open Style
  open Css

  type t = Sr_only | Not_sr_only
  type Utility.base += Self of t

  let name = "screen_reader"
  let priority = 0

  let suborder = function
    (* Negative suborders to appear before absolute (suborder 0) *)
    | Sr_only -> -2
    | Not_sr_only -> -1

  let to_class = function Sr_only -> "sr-only" | Not_sr_only -> "not-sr-only"

  let to_style = function
    | Sr_only ->
        (* Property order matches Tailwind: clip, white-space, border-width,
           width, height, margin, padding, position, overflow *)
        style
          [
            clip (Css.Clip_rect (Zero, Zero, Zero, Zero));
            white_space Nowrap;
            border_width Zero;
            width (Px 1.);
            height (Px 1.);
            margin [ Px (-1.) ];
            padding [ Zero ];
            Css.position Absolute;
            overflow Hidden;
          ]
    | Not_sr_only ->
        (* Property order matches Tailwind: clip, white-space, width, height,
           margin, padding, position, overflow *)
        style
          [
            clip Css.Clip_auto;
            white_space Normal;
            width Auto;
            height Auto;
            margin [ Zero ];
            padding [ Zero ];
            Css.position Static;
            overflow Visible;
          ]

  let of_class class_name =
    let parts = String.split_on_char '-' class_name in
    match parts with
    | [ "sr"; "only" ] -> Ok Sr_only
    | [ "not"; "sr"; "only" ] -> Ok Not_sr_only
    | _ -> Error (`Msg "Not a screen reader utility")
end

module Handler = struct
  open Style
  open Css

  type t =
    | (* Display *)
      Block
    | Inline
    | Inline_block
    | Table
    | Hidden
    | (* Visibility *)
      Visible
    | Invisible
    | Collapse
    | (* Isolation *)
      Isolate
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

  type Utility.base += Self of t

  (** Priority for layout utilities. Set to 4 for display utilities (block,
      inline, inline-block, hidden). All display utilities share priority 4 to
      group together and sort alphabetically. *)
  let name = "layout"

  let priority = 4

  let suborder = function
    (* Display utilities - ordered to match Tailwind: block(3), flex(4),
       grid(5), hidden(6), inline(7), inline-block(8), inline-flex(9),
       inline-grid(10), table(11) *)
    | Block -> 3
    | Hidden -> 6
    | Inline -> 7
    | Inline_block -> 8
    | Table -> 11
    (* Visibility *)
    | Visible -> 100
    | Invisible -> 101
    | Collapse -> 102
    (* Isolation *)
    | Isolate -> 200
    (* Z-index *)
    | Z_0 -> 500
    | Z_10 -> 501
    | Z_20 -> 502
    | Z_30 -> 503
    | Z_40 -> 504
    | Z_50 -> 505
    | Z_auto -> 506
    (* Object fit *)
    | Object_contain -> 600
    | Object_cover -> 601
    | Object_fill -> 602
    | Object_none -> 603
    | Object_scale_down -> 604
    (* Object position *)
    | Object_center -> 700
    | Object_top -> 701
    | Object_bottom -> 702
    | Object_left -> 703
    | Object_right -> 704

  (** {1 Style Generation} *)

  let to_class = function
    | Block -> "block"
    | Inline -> "inline"
    | Inline_block -> "inline-block"
    | Table -> "table"
    | Hidden -> "hidden"
    | Visible -> "visible"
    | Invisible -> "invisible"
    | Collapse -> "collapse"
    | Isolate -> "isolate"
    | Z_0 -> "z-0"
    | Z_10 -> "z-10"
    | Z_20 -> "z-20"
    | Z_30 -> "z-30"
    | Z_40 -> "z-40"
    | Z_50 -> "z-50"
    | Z_auto -> "z-auto"
    | Object_contain -> "object-contain"
    | Object_cover -> "object-cover"
    | Object_fill -> "object-fill"
    | Object_none -> "object-none"
    | Object_scale_down -> "object-scale-down"
    | Object_center -> "object-center"
    | Object_top -> "object-top"
    | Object_bottom -> "object-bottom"
    | Object_left -> "object-left"
    | Object_right -> "object-right"

  let to_style = function
    | Block -> style [ display Block ]
    | Inline -> style [ display Inline ]
    | Inline_block -> style [ display Inline_block ]
    | Table -> style [ display Table ]
    | Hidden -> style [ display None ]
    | Visible -> style [ visibility Visible ]
    | Invisible -> style [ visibility Hidden ]
    | Collapse -> style [ visibility Collapse ]
    | Isolate -> style [ isolation Isolate ]
    | Z_0 -> style [ z_index (Index 0) ]
    | Z_10 -> style [ z_index (Index 10) ]
    | Z_20 -> style [ z_index (Index 20) ]
    | Z_30 -> style [ z_index (Index 30) ]
    | Z_40 -> style [ z_index (Index 40) ]
    | Z_50 -> style [ z_index (Index 50) ]
    | Z_auto -> style [ z_index_auto ]
    | Object_contain -> style [ object_fit Contain ]
    | Object_cover -> style [ object_fit Cover ]
    | Object_fill -> style [ object_fit Fill ]
    | Object_none -> style [ object_fit None ]
    | Object_scale_down -> style [ object_fit Scale_down ]
    | Object_center -> style [ object_position Center ]
    | Object_top -> style [ object_position Center_top ]
    | Object_bottom -> style [ object_position Center_bottom ]
    | Object_left -> style [ object_position Left_center ]
    | Object_right -> style [ object_position Right_center ]

  (** {1 Parsing Functions} *)

  let of_class class_name =
    let parts = String.split_on_char '-' class_name in
    match parts with
    | [ "block" ] -> Ok Block
    | [ "inline" ] -> Ok Inline
    | [ "inline"; "block" ] -> Ok Inline_block
    | [ "table" ] -> Ok Table
    | [ "hidden" ] -> Ok Hidden
    | [ "visible" ] -> Ok Visible
    | [ "invisible" ] -> Ok Invisible
    | [ "collapse" ] -> Ok Collapse
    | [ "isolate" ] -> Ok Isolate
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
    | _ -> Error (`Msg "Not a layout utility")
end

open Handler

(** Register both handlers with Utility system *)
let () = Utility.register (module Screen_reader_handler)

let () = Utility.register (module Handler)

(** {1 Public API - Utility Values} *)

(* Layout utilities *)
let utility x = Utility.base (Self x)
let block = utility Block
let inline = utility Inline
let inline_block = utility Inline_block
let table = utility Table
let hidden = utility Hidden
let visible = utility Visible
let invisible = utility Invisible
let collapse = utility Collapse
let isolate = utility Isolate
let z_0 = utility Z_0
let z_10 = utility Z_10
let z_20 = utility Z_20
let z_30 = utility Z_30
let z_40 = utility Z_40
let z_50 = utility Z_50
let z_auto = utility Z_auto
let object_contain = utility Object_contain
let object_cover = utility Object_cover
let object_fill = utility Object_fill
let object_none = utility Object_none
let object_scale_down = utility Object_scale_down
let object_center = utility Object_center
let object_top = utility Object_top
let object_bottom = utility Object_bottom
let object_left = utility Object_left
let object_right = utility Object_right

(* Screen reader utilities *)
let sr_utility x = Utility.base (Screen_reader_handler.Self x)
let sr_only = sr_utility Screen_reader_handler.Sr_only
let not_sr_only = sr_utility Screen_reader_handler.Not_sr_only
