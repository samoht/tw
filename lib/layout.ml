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
        (* Property order matches Tailwind: clip-path, white-space,
           border-width, width, height, margin, padding, position, overflow *)
        style
          [
            clip_path (Css.Clip_path_inset (Pct 50., None, None, None));
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
        (* Property order matches Tailwind: clip-path, white-space, width,
           height, margin, padding, position, overflow *)
        style
          [
            clip_path Css.Clip_path_none;
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
    | Inline_table
    | Table
    | Table_caption
    | Table_cell
    | Table_column
    | Table_column_group
    | Table_footer_group
    | Table_header_group
    | Table_row
    | Table_row_group
    | List_item
    | Flow_root
    | Contents
    | Hidden
    | (* Visibility *)
      Visible
    | Invisible
    | Collapse
    | (* Isolation *)
      Isolate
    | Isolation_auto
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
    | Object_bottom_left
    | Object_bottom_right
    | Object_left_bottom
    | Object_left_top
    | Object_right_bottom
    | Object_right_top
    | Object_top_left
    | Object_top_right
    | (* Float *)
      Float_left
    | Float_right
    | Float_none
    | Float_start
    | Float_end
    | (* Clear *)
      Clear_left
    | Clear_right
    | Clear_none
    | Clear_both
    | Clear_start
    | Clear_end
    | (* Box decoration break *)
      Box_decoration_clone
    | Box_decoration_slice
    | (* Break before/after/inside - page/column breaks *)
      Break_before_all
    | Break_before_auto
    | Break_before_avoid
    | Break_before_avoid_page
    | Break_before_column
    | Break_before_left
    | Break_before_page
    | Break_before_right
    | Break_after_all
    | Break_after_auto
    | Break_after_avoid
    | Break_after_avoid_page
    | Break_after_column
    | Break_after_left
    | Break_after_page
    | Break_after_right
    | Break_inside_auto
    | Break_inside_avoid
    | Break_inside_avoid_column
    | Break_inside_avoid_page

  type Utility.base += Self of t

  (** Priority for layout utilities. Set to 4 for display utilities (block,
      inline, inline-block, hidden). All display utilities share priority 4 to
      group together and sort alphabetically. *)
  let name = "layout"

  let priority = 4

  let suborder = function
    (* Display utilities - ordered alphabetically per Tailwind. Gaps left for
       flex(3), grid(5), inline-flex(9), inline-grid(10) which live in flex.ml
       and grid.ml with the same priority. *)
    | Block -> 1
    | Contents -> 2
    (* flex = 3, in flex.ml *)
    | Flow_root -> 4
    (* grid = 5, in grid.ml *)
    | Hidden -> 6
    | Inline -> 7
    | Inline_block -> 8
    (* inline-flex = 9, in flex.ml *)
    (* inline-grid = 10, in grid.ml *)
    | Inline_table -> 11
    | List_item -> 12
    | Table -> 13
    | Table_caption -> 15
    | Table_cell -> 16
    | Table_column -> 17
    | Table_column_group -> 18
    | Table_footer_group -> 20
    | Table_header_group -> 21
    | Table_row -> 22
    | Table_row_group -> 23
    (* Visibility - alphabetical order: collapse, invisible, visible *)
    | Collapse -> 100
    | Invisible -> 101
    | Visible -> 102
    (* Isolation - order: isolate, isolation-auto *)
    | Isolate -> 200
    | Isolation_auto -> 201
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
    (* Object position - alphabetical: bottom, bottom-left, ..., top-right *)
    | Object_bottom -> 700
    | Object_bottom_left -> 701
    | Object_bottom_right -> 702
    | Object_center -> 703
    | Object_left -> 704
    | Object_left_bottom -> 705
    | Object_left_top -> 706
    | Object_right -> 707
    | Object_right_bottom -> 708
    | Object_right_top -> 709
    | Object_top -> 710
    | Object_top_left -> 711
    | Object_top_right -> 712
    (* Float - alphabetical order: end, left, none, right, start *)
    | Float_end -> 800
    | Float_left -> 801
    | Float_none -> 802
    | Float_right -> 803
    | Float_start -> 804
    (* Clear - alphabetical order: both, end, left, none, right, start *)
    | Clear_both -> 900
    | Clear_end -> 901
    | Clear_left -> 902
    | Clear_none -> 903
    | Clear_right -> 904
    | Clear_start -> 905
    (* Box decoration break - alphabetical: clone, slice *)
    | Box_decoration_clone -> 1000
    | Box_decoration_slice -> 1001
    (* Break before - alphabetical order (Tailwind: break-before < break-inside
       < break-after) *)
    | Break_before_all -> 1100
    | Break_before_auto -> 1101
    | Break_before_avoid -> 1102
    | Break_before_avoid_page -> 1103
    | Break_before_column -> 1104
    | Break_before_left -> 1105
    | Break_before_page -> 1106
    | Break_before_right -> 1107
    (* Break inside - alphabetical order *)
    | Break_inside_auto -> 1200
    | Break_inside_avoid -> 1201
    | Break_inside_avoid_column -> 1202
    | Break_inside_avoid_page -> 1203
    (* Break after - alphabetical order *)
    | Break_after_all -> 1300
    | Break_after_auto -> 1301
    | Break_after_avoid -> 1302
    | Break_after_avoid_page -> 1303
    | Break_after_column -> 1304
    | Break_after_left -> 1305
    | Break_after_page -> 1306
    | Break_after_right -> 1307

  (** {1 Style Generation} *)

  let to_class = function
    | Block -> "block"
    | Inline -> "inline"
    | Inline_block -> "inline-block"
    | Inline_table -> "inline-table"
    | Table -> "table"
    | Table_caption -> "table-caption"
    | Table_cell -> "table-cell"
    | Table_column -> "table-column"
    | Table_column_group -> "table-column-group"
    | Table_footer_group -> "table-footer-group"
    | Table_header_group -> "table-header-group"
    | Table_row -> "table-row"
    | Table_row_group -> "table-row-group"
    | List_item -> "list-item"
    | Flow_root -> "flow-root"
    | Contents -> "contents"
    | Hidden -> "hidden"
    | Visible -> "visible"
    | Invisible -> "invisible"
    | Collapse -> "collapse"
    | Isolate -> "isolate"
    | Isolation_auto -> "isolation-auto"
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
    | Object_bottom_left -> "object-bottom-left"
    | Object_bottom_right -> "object-bottom-right"
    | Object_left_bottom -> "object-left-bottom"
    | Object_left_top -> "object-left-top"
    | Object_right_bottom -> "object-right-bottom"
    | Object_right_top -> "object-right-top"
    | Object_top_left -> "object-top-left"
    | Object_top_right -> "object-top-right"
    | Float_left -> "float-left"
    | Float_right -> "float-right"
    | Float_none -> "float-none"
    | Float_start -> "float-start"
    | Float_end -> "float-end"
    | Clear_left -> "clear-left"
    | Clear_right -> "clear-right"
    | Clear_none -> "clear-none"
    | Clear_both -> "clear-both"
    | Clear_start -> "clear-start"
    | Clear_end -> "clear-end"
    | Box_decoration_clone -> "box-decoration-clone"
    | Box_decoration_slice -> "box-decoration-slice"
    | Break_before_all -> "break-before-all"
    | Break_before_auto -> "break-before-auto"
    | Break_before_avoid -> "break-before-avoid"
    | Break_before_avoid_page -> "break-before-avoid-page"
    | Break_before_column -> "break-before-column"
    | Break_before_left -> "break-before-left"
    | Break_before_page -> "break-before-page"
    | Break_before_right -> "break-before-right"
    | Break_after_all -> "break-after-all"
    | Break_after_auto -> "break-after-auto"
    | Break_after_avoid -> "break-after-avoid"
    | Break_after_avoid_page -> "break-after-avoid-page"
    | Break_after_column -> "break-after-column"
    | Break_after_left -> "break-after-left"
    | Break_after_page -> "break-after-page"
    | Break_after_right -> "break-after-right"
    | Break_inside_auto -> "break-inside-auto"
    | Break_inside_avoid -> "break-inside-avoid"
    | Break_inside_avoid_column -> "break-inside-avoid-column"
    | Break_inside_avoid_page -> "break-inside-avoid-page"

  let to_style = function
    | Block -> style [ display Block ]
    | Inline -> style [ display Inline ]
    | Inline_block -> style [ display Inline_block ]
    | Inline_table -> style [ display Inline_table ]
    | Table -> style [ display Table ]
    | Table_caption -> style [ display Table_caption ]
    | Table_cell -> style [ display Table_cell ]
    | Table_column -> style [ display Table_column ]
    | Table_column_group -> style [ display Table_column_group ]
    | Table_footer_group -> style [ display Table_footer_group ]
    | Table_header_group -> style [ display Table_header_group ]
    | Table_row -> style [ display Table_row ]
    | Table_row_group -> style [ display Table_row_group ]
    | List_item -> style [ display List_item ]
    | Flow_root -> style [ display Flow_root ]
    | Contents -> style [ display Contents ]
    | Hidden -> style [ display None ]
    | Visible -> style [ visibility Visible ]
    | Invisible -> style [ visibility Hidden ]
    | Collapse -> style [ visibility Collapse ]
    | Isolate -> style [ isolation Isolate ]
    | Isolation_auto -> style [ isolation Auto ]
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
    | Object_top -> style [ object_position Top ]
    | Object_bottom -> style [ object_position Bottom ]
    | Object_left -> style [ object_position Left ]
    | Object_right -> style [ object_position Right ]
    | Object_bottom_left -> style [ object_position Bottom_left ]
    | Object_bottom_right -> style [ object_position Bottom_right ]
    | Object_left_bottom -> style [ object_position Left_bottom ]
    | Object_left_top -> style [ object_position Left_top ]
    | Object_right_bottom -> style [ object_position Right_bottom ]
    | Object_right_top -> style [ object_position Right_top ]
    | Object_top_left -> style [ object_position Top_left ]
    | Object_top_right -> style [ object_position Top_right ]
    | Float_left -> style [ Css.float Left ]
    | Float_right -> style [ Css.float Right ]
    | Float_none -> style [ Css.float None ]
    | Float_start -> style [ Css.float Inline_start ]
    | Float_end -> style [ Css.float Inline_end ]
    | Clear_left -> style [ Css.clear Left ]
    | Clear_right -> style [ Css.clear Right ]
    | Clear_none -> style [ Css.clear None ]
    | Clear_both -> style [ Css.clear Both ]
    | Clear_start -> style [ Css.clear Inline_start ]
    | Clear_end -> style [ Css.clear Inline_end ]
    | Box_decoration_clone ->
        style
          [
            Css.webkit_box_decoration_break Clone;
            Css.box_decoration_break Clone;
          ]
    | Box_decoration_slice ->
        style
          [
            Css.webkit_box_decoration_break Slice;
            Css.box_decoration_break Slice;
          ]
    (* Break before *)
    | Break_before_all -> style [ Css.break_before All ]
    | Break_before_auto -> style [ Css.break_before Auto ]
    | Break_before_avoid -> style [ Css.break_before Avoid ]
    | Break_before_avoid_page -> style [ Css.break_before Avoid_page ]
    | Break_before_column -> style [ Css.break_before Column ]
    | Break_before_left -> style [ Css.break_before Left ]
    | Break_before_page -> style [ Css.break_before Page ]
    | Break_before_right -> style [ Css.break_before Right ]
    (* Break after *)
    | Break_after_all -> style [ Css.break_after All ]
    | Break_after_auto -> style [ Css.break_after Auto ]
    | Break_after_avoid -> style [ Css.break_after Avoid ]
    | Break_after_avoid_page -> style [ Css.break_after Avoid_page ]
    | Break_after_column -> style [ Css.break_after Column ]
    | Break_after_left -> style [ Css.break_after Left ]
    | Break_after_page -> style [ Css.break_after Page ]
    | Break_after_right -> style [ Css.break_after Right ]
    (* Break inside *)
    | Break_inside_auto -> style [ Css.break_inside Auto ]
    | Break_inside_avoid -> style [ Css.break_inside Avoid ]
    | Break_inside_avoid_column -> style [ Css.break_inside Avoid_column ]
    | Break_inside_avoid_page -> style [ Css.break_inside Avoid_page ]

  (** {1 Parsing Functions} *)

  let of_class class_name =
    let parts = String.split_on_char '-' class_name in
    match parts with
    | [ "block" ] -> Ok Block
    | [ "contents" ] -> Ok Contents
    | [ "flow"; "root" ] -> Ok Flow_root
    | [ "inline" ] -> Ok Inline
    | [ "inline"; "block" ] -> Ok Inline_block
    | [ "inline"; "table" ] -> Ok Inline_table
    | [ "list"; "item" ] -> Ok List_item
    | [ "table" ] -> Ok Table
    | [ "table"; "caption" ] -> Ok Table_caption
    | [ "table"; "cell" ] -> Ok Table_cell
    | [ "table"; "column" ] -> Ok Table_column
    | [ "table"; "column"; "group" ] -> Ok Table_column_group
    | [ "table"; "footer"; "group" ] -> Ok Table_footer_group
    | [ "table"; "header"; "group" ] -> Ok Table_header_group
    | [ "table"; "row" ] -> Ok Table_row
    | [ "table"; "row"; "group" ] -> Ok Table_row_group
    | [ "hidden" ] -> Ok Hidden
    | [ "visible" ] -> Ok Visible
    | [ "invisible" ] -> Ok Invisible
    | [ "collapse" ] -> Ok Collapse
    | [ "isolate" ] -> Ok Isolate
    | [ "isolation"; "auto" ] -> Ok Isolation_auto
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
    | [ "object"; "bottom"; "left" ] -> Ok Object_bottom_left
    | [ "object"; "bottom"; "right" ] -> Ok Object_bottom_right
    | [ "object"; "left"; "bottom" ] -> Ok Object_left_bottom
    | [ "object"; "left"; "top" ] -> Ok Object_left_top
    | [ "object"; "right"; "bottom" ] -> Ok Object_right_bottom
    | [ "object"; "right"; "top" ] -> Ok Object_right_top
    | [ "object"; "top"; "left" ] -> Ok Object_top_left
    | [ "object"; "top"; "right" ] -> Ok Object_top_right
    | [ "float"; "left" ] -> Ok Float_left
    | [ "float"; "right" ] -> Ok Float_right
    | [ "float"; "none" ] -> Ok Float_none
    | [ "float"; "start" ] -> Ok Float_start
    | [ "float"; "end" ] -> Ok Float_end
    | [ "clear"; "left" ] -> Ok Clear_left
    | [ "clear"; "right" ] -> Ok Clear_right
    | [ "clear"; "none" ] -> Ok Clear_none
    | [ "clear"; "both" ] -> Ok Clear_both
    | [ "clear"; "start" ] -> Ok Clear_start
    | [ "clear"; "end" ] -> Ok Clear_end
    | [ "box"; "decoration"; "clone" ] -> Ok Box_decoration_clone
    | [ "box"; "decoration"; "slice" ] -> Ok Box_decoration_slice
    (* Break before *)
    | [ "break"; "before"; "all" ] -> Ok Break_before_all
    | [ "break"; "before"; "auto" ] -> Ok Break_before_auto
    | [ "break"; "before"; "avoid" ] -> Ok Break_before_avoid
    | [ "break"; "before"; "avoid"; "page" ] -> Ok Break_before_avoid_page
    | [ "break"; "before"; "column" ] -> Ok Break_before_column
    | [ "break"; "before"; "left" ] -> Ok Break_before_left
    | [ "break"; "before"; "page" ] -> Ok Break_before_page
    | [ "break"; "before"; "right" ] -> Ok Break_before_right
    (* Break after *)
    | [ "break"; "after"; "all" ] -> Ok Break_after_all
    | [ "break"; "after"; "auto" ] -> Ok Break_after_auto
    | [ "break"; "after"; "avoid" ] -> Ok Break_after_avoid
    | [ "break"; "after"; "avoid"; "page" ] -> Ok Break_after_avoid_page
    | [ "break"; "after"; "column" ] -> Ok Break_after_column
    | [ "break"; "after"; "left" ] -> Ok Break_after_left
    | [ "break"; "after"; "page" ] -> Ok Break_after_page
    | [ "break"; "after"; "right" ] -> Ok Break_after_right
    (* Break inside *)
    | [ "break"; "inside"; "auto" ] -> Ok Break_inside_auto
    | [ "break"; "inside"; "avoid" ] -> Ok Break_inside_avoid
    | [ "break"; "inside"; "avoid"; "column" ] -> Ok Break_inside_avoid_column
    | [ "break"; "inside"; "avoid"; "page" ] -> Ok Break_inside_avoid_page
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
let contents = utility Contents
let flow_root = utility Flow_root
let inline = utility Inline
let inline_block = utility Inline_block
let inline_table = utility Inline_table
let list_item = utility List_item
let table = utility Table
let table_caption = utility Table_caption
let table_cell = utility Table_cell
let table_column = utility Table_column
let table_column_group = utility Table_column_group
let table_footer_group = utility Table_footer_group
let table_header_group = utility Table_header_group
let table_row = utility Table_row
let table_row_group = utility Table_row_group
let hidden = utility Hidden
let visible = utility Visible
let invisible = utility Invisible
let collapse = utility Collapse
let isolate = utility Isolate
let isolation_auto = utility Isolation_auto
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
let object_bottom_left = utility Object_bottom_left
let object_bottom_right = utility Object_bottom_right
let object_left_bottom = utility Object_left_bottom
let object_left_top = utility Object_left_top
let object_right_bottom = utility Object_right_bottom
let object_right_top = utility Object_right_top
let object_top_left = utility Object_top_left
let object_top_right = utility Object_top_right
let float_left = utility Float_left
let float_right = utility Float_right
let float_none = utility Float_none
let float_start = utility Float_start
let float_end = utility Float_end

(* Screen reader utilities *)
let sr_utility x = Utility.base (Screen_reader_handler.Self x)
let sr_only = sr_utility Screen_reader_handler.Sr_only
let not_sr_only = sr_utility Screen_reader_handler.Not_sr_only

(* Box decoration break utilities *)
let box_decoration_clone = utility Box_decoration_clone
let box_decoration_slice = utility Box_decoration_slice

(* Break before/after/inside utilities - page/column breaks *)
let break_before_all = utility Break_before_all
let break_before_auto = utility Break_before_auto
let break_before_avoid = utility Break_before_avoid
let break_before_avoid_page = utility Break_before_avoid_page
let break_before_column = utility Break_before_column
let break_before_left = utility Break_before_left
let break_before_page = utility Break_before_page
let break_before_right = utility Break_before_right
let break_after_all = utility Break_after_all
let break_after_auto = utility Break_after_auto
let break_after_avoid = utility Break_after_avoid
let break_after_avoid_page = utility Break_after_avoid_page
let break_after_column = utility Break_after_column
let break_after_left = utility Break_after_left
let break_after_page = utility Break_after_page
let break_after_right = utility Break_after_right
let break_inside_auto = utility Break_inside_auto
let break_inside_avoid = utility Break_inside_avoid
let break_inside_avoid_column = utility Break_inside_avoid_column
let break_inside_avoid_page = utility Break_inside_avoid_page
