(** Layout utilities for basic display, positioning, and object properties *)

module Handler = struct
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

  type Utility.base += Self of t

  (** Priority for layout utilities. Set to 4 for display utilities (block,
      inline, inline-block, hidden). All display utilities share priority 4 to
      group together and sort alphabetically. *)
  let name = "layout"

  let priority = 4

  let suborder = function
    | Sr_only -> 0
    | Not_sr_only -> 1
    | Block -> 2
    | Hidden -> 3
    | Inline -> 4
    | Inline_block -> 5
    (* Overflow utilities *)
    | Overflow_auto -> 10
    | Overflow_clip -> 11
    | Overflow_hidden -> 12
    | Overflow_scroll -> 13
    | Overflow_visible -> 14
    | Overflow_x_auto -> 15
    | Overflow_x_hidden -> 16
    | Overflow_x_scroll -> 17
    | Overflow_x_visible -> 18
    | Overflow_y_auto -> 19
    | Overflow_y_hidden -> 20
    | Overflow_y_scroll -> 21
    | Overflow_y_visible -> 22
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
    | Hidden -> "hidden"
    | Sr_only -> "sr-only"
    | Not_sr_only -> "not-sr-only"
    | Visible -> "visible"
    | Invisible -> "invisible"
    | Collapse -> "collapse"
    | Isolate -> "isolate"
    | Overflow_auto -> "overflow-auto"
    | Overflow_hidden -> "overflow-hidden"
    | Overflow_clip -> "overflow-clip"
    | Overflow_visible -> "overflow-visible"
    | Overflow_scroll -> "overflow-scroll"
    | Overflow_x_auto -> "overflow-x-auto"
    | Overflow_x_hidden -> "overflow-x-hidden"
    | Overflow_x_visible -> "overflow-x-visible"
    | Overflow_x_scroll -> "overflow-x-scroll"
    | Overflow_y_auto -> "overflow-y-auto"
    | Overflow_y_hidden -> "overflow-y-hidden"
    | Overflow_y_visible -> "overflow-y-visible"
    | Overflow_y_scroll -> "overflow-y-scroll"
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
    | Hidden -> style [ display None ]
    | Sr_only ->
        style
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
        style
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
    | Visible -> style [ visibility Visible ]
    | Invisible -> style [ visibility Hidden ]
    | Collapse -> style [ visibility Collapse ]
    | Isolate -> style [ isolation Isolate ]
    | Overflow_auto -> style [ overflow Auto ]
    | Overflow_hidden -> style [ overflow Hidden ]
    | Overflow_clip -> style [ overflow Clip ]
    | Overflow_visible -> style [ overflow Visible ]
    | Overflow_scroll -> style [ overflow Scroll ]
    | Overflow_x_auto -> style [ overflow_x Auto ]
    | Overflow_x_hidden -> style [ overflow_x Hidden ]
    | Overflow_x_visible -> style [ overflow_x Visible ]
    | Overflow_x_scroll -> style [ overflow_x Scroll ]
    | Overflow_y_auto -> style [ overflow_y Auto ]
    | Overflow_y_hidden -> style [ overflow_y Hidden ]
    | Overflow_y_visible -> style [ overflow_y Visible ]
    | Overflow_y_scroll -> style [ overflow_y Scroll ]
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
    | [ "sr"; "only" ] -> Ok Sr_only
    | [ "not"; "sr"; "only" ] -> Ok Not_sr_only
    | _ -> Error (`Msg "Not a layout utility")
end

open Handler

(** Register layout handler with Utility system *)
let () = Utility.register (module Handler)

(** {1 Public API - Utility Values} *)

(* These provide the public API for layout utilities *)
let utility x = Utility.base (Self x)
let block = utility Block
let inline = utility Inline
let inline_block = utility Inline_block
let hidden = utility Hidden
let visible = utility Visible
let invisible = utility Invisible
let collapse = utility Collapse
let isolate = utility Isolate
let overflow_auto = utility Overflow_auto
let overflow_hidden = utility Overflow_hidden
let overflow_clip = utility Overflow_clip
let overflow_visible = utility Overflow_visible
let overflow_scroll = utility Overflow_scroll
let overflow_x_auto = utility Overflow_x_auto
let overflow_x_hidden = utility Overflow_x_hidden
let overflow_x_visible = utility Overflow_x_visible
let overflow_x_scroll = utility Overflow_x_scroll
let overflow_y_auto = utility Overflow_y_auto
let overflow_y_hidden = utility Overflow_y_hidden
let overflow_y_visible = utility Overflow_y_visible
let overflow_y_scroll = utility Overflow_y_scroll
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
let sr_only = utility Sr_only
let not_sr_only = utility Not_sr_only
