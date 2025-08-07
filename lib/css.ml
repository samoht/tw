(** CSS generation utilities *)

type property_name =
  | Background_color
  | Color
  | Border_color
  | Padding
  | Padding_left
  | Padding_right
  | Padding_bottom
  | Padding_top
  | Margin
  | Margin_left
  | Margin_right
  | Margin_top
  | Margin_bottom
  | Gap
  | Column_gap
  | Row_gap
  | Width
  | Height
  | Min_width
  | Min_height
  | Max_width
  | Max_height
  | Font_size
  | Line_height
  | Font_weight
  | Font_style
  | Text_align
  | Text_decoration
  | Text_decoration_style
  | Text_underline_offset
  | Text_transform
  | Letter_spacing
  | White_space
  | Display
  | Position
  | Flex_direction
  | Flex_wrap
  | Flex
  | Flex_grow
  | Flex_shrink
  | Align_items
  | Justify_content
  | Align_content
  | Align_self
  | Justify_self
  | Place_content
  | Place_items
  | Place_self
  | Grid_template_columns
  | Grid_template_rows
  | Border_width
  | Border_radius
  | Box_shadow
  | Opacity
  | Transition
  | Transform
  | Cursor
  | Table_layout
  | Border_collapse
  | Border_spacing
  | User_select
  | Pointer_events
  | Overflow
  | Object_fit
  | Top
  | Right
  | Bottom
  | Left
  | Z_index
  | Border_top_width
  | Border_right_width
  | Border_bottom_width
  | Border_left_width
  | Outline
  | Outline_offset
  | Clip
  | Filter
  | Background_image
  | Animation
  | Appearance
  | Overflow_x
  | Overflow_y
  | Resize
  | Vertical_align
  | Box_sizing
  | Font_family
  | Background_position
  | Background_repeat
  | Background_size
  | Webkit_font_smoothing
  | Moz_osx_font_smoothing
  | Webkit_line_clamp
  | Backdrop_filter
  | Scroll_snap_type
  | Scroll_snap_align
  | Scroll_snap_stop
  | Scroll_behavior
  | Custom of string  (** CSS property names as a variant type *)

type property = property_name * string
(** A CSS property as (name, value) pair *)

(* Property constructor functions *)
let background_color value = (Background_color, value)
let color value = (Color, value)
let border_color value = (Border_color, value)
let padding value = (Padding, value)
let padding_left value = (Padding_left, value)
let padding_right value = (Padding_right, value)
let padding_bottom value = (Padding_bottom, value)
let padding_top value = (Padding_top, value)
let margin value = (Margin, value)
let margin_left value = (Margin_left, value)
let margin_right value = (Margin_right, value)
let margin_top value = (Margin_top, value)
let margin_bottom value = (Margin_bottom, value)
let gap value = (Gap, value)
let column_gap value = (Column_gap, value)
let row_gap value = (Row_gap, value)
let width value = (Width, value)
let height value = (Height, value)
let min_width value = (Min_width, value)
let min_height value = (Min_height, value)
let max_width value = (Max_width, value)
let max_height value = (Max_height, value)
let font_size value = (Font_size, value)
let line_height value = (Line_height, value)
let font_weight value = (Font_weight, value)
let font_style value = (Font_style, value)
let text_align value = (Text_align, value)
let text_decoration value = (Text_decoration, value)
let text_decoration_style value = (Text_decoration_style, value)
let text_underline_offset value = (Text_underline_offset, value)
let text_transform value = (Text_transform, value)
let letter_spacing value = (Letter_spacing, value)
let white_space value = (White_space, value)
let display value = (Display, value)
let position value = (Position, value)
let flex_direction value = (Flex_direction, value)
let flex_wrap value = (Flex_wrap, value)
let flex value = (Flex, value)
let flex_grow value = (Flex_grow, value)
let flex_shrink value = (Flex_shrink, value)
let align_items value = (Align_items, value)
let justify_content value = (Justify_content, value)
let align_content value = (Align_content, value)
let align_self value = (Align_self, value)
let justify_self value = (Justify_self, value)
let place_content value = (Place_content, value)
let place_items value = (Place_items, value)
let place_self value = (Place_self, value)
let grid_template_columns value = (Grid_template_columns, value)
let grid_template_rows value = (Grid_template_rows, value)
let border_width value = (Border_width, value)
let border_radius value = (Border_radius, value)
let box_shadow value = (Box_shadow, value)
let opacity value = (Opacity, value)
let transition value = (Transition, value)
let transform value = (Transform, value)
let cursor value = (Cursor, value)
let table_layout value = (Table_layout, value)
let border_collapse value = (Border_collapse, value)
let border_spacing value = (Border_spacing, value)
let user_select value = (User_select, value)
let pointer_events value = (Pointer_events, value)
let overflow value = (Overflow, value)
let object_fit value = (Object_fit, value)
let top value = (Top, value)
let right value = (Right, value)
let bottom value = (Bottom, value)
let left value = (Left, value)
let z_index value = (Z_index, value)
let border_top_width value = (Border_top_width, value)
let border_right_width value = (Border_right_width, value)
let border_bottom_width value = (Border_bottom_width, value)
let border_left_width value = (Border_left_width, value)
let outline value = (Outline, value)
let outline_offset value = (Outline_offset, value)
let clip value = (Clip, value)
let filter value = (Filter, value)
let background_image value = (Background_image, value)
let animation value = (Animation, value)
let appearance value = (Appearance, value)
let overflow_x value = (Overflow_x, value)
let overflow_y value = (Overflow_y, value)
let resize value = (Resize, value)
let vertical_align value = (Vertical_align, value)
let box_sizing value = (Box_sizing, value)
let font_family value = (Font_family, value)
let background_position value = (Background_position, value)
let background_repeat value = (Background_repeat, value)
let background_size value = (Background_size, value)
let webkit_font_smoothing value = (Webkit_font_smoothing, value)
let moz_osx_font_smoothing value = (Moz_osx_font_smoothing, value)
let webkit_line_clamp value = (Webkit_line_clamp, value)
let backdrop_filter value = (Backdrop_filter, value)
let scroll_snap_type value = (Scroll_snap_type, value)
let scroll_snap_align value = (Scroll_snap_align, value)
let scroll_snap_stop value = (Scroll_snap_stop, value)
let scroll_behavior value = (Scroll_behavior, value)
let property name value = (Custom name, value)

type rule = { selector : string; properties : property list }
type media_query = { condition : string; rules : rule list }
type stylesheet = { rules : rule list; media_queries : media_query list }

(** {1 Creation} *)

let property_name_to_string = function
  | Background_color -> "background-color"
  | Color -> "color"
  | Border_color -> "border-color"
  | Padding -> "padding"
  | Padding_left -> "padding-left"
  | Padding_right -> "padding-right"
  | Padding_bottom -> "padding-bottom"
  | Padding_top -> "padding-top"
  | Margin -> "margin"
  | Margin_left -> "margin-left"
  | Margin_right -> "margin-right"
  | Margin_top -> "margin-top"
  | Margin_bottom -> "margin-bottom"
  | Gap -> "gap"
  | Column_gap -> "column-gap"
  | Row_gap -> "row-gap"
  | Width -> "width"
  | Height -> "height"
  | Min_width -> "min-width"
  | Min_height -> "min-height"
  | Max_width -> "max-width"
  | Max_height -> "max-height"
  | Font_size -> "font-size"
  | Line_height -> "line-height"
  | Font_weight -> "font-weight"
  | Font_style -> "font-style"
  | Text_align -> "text-align"
  | Text_decoration -> "text-decoration"
  | Text_decoration_style -> "text-decoration-style"
  | Text_underline_offset -> "text-underline-offset"
  | Text_transform -> "text-transform"
  | Letter_spacing -> "letter-spacing"
  | White_space -> "white-space"
  | Display -> "display"
  | Position -> "position"
  | Flex_direction -> "flex-direction"
  | Flex_wrap -> "flex-wrap"
  | Flex -> "flex"
  | Flex_grow -> "flex-grow"
  | Flex_shrink -> "flex-shrink"
  | Align_items -> "align-items"
  | Justify_content -> "justify-content"
  | Align_content -> "align-content"
  | Align_self -> "align-self"
  | Justify_self -> "justify-self"
  | Place_content -> "place-content"
  | Place_items -> "place-items"
  | Place_self -> "place-self"
  | Grid_template_columns -> "grid-template-columns"
  | Grid_template_rows -> "grid-template-rows"
  | Border_width -> "border-width"
  | Border_radius -> "border-radius"
  | Box_shadow -> "box-shadow"
  | Opacity -> "opacity"
  | Transition -> "transition"
  | Transform -> "transform"
  | Cursor -> "cursor"
  | Table_layout -> "table-layout"
  | Border_collapse -> "border-collapse"
  | Border_spacing -> "border-spacing"
  | User_select -> "user-select"
  | Pointer_events -> "pointer-events"
  | Overflow -> "overflow"
  | Object_fit -> "object-fit"
  | Top -> "top"
  | Right -> "right"
  | Bottom -> "bottom"
  | Left -> "left"
  | Z_index -> "z-index"
  | Border_top_width -> "border-top-width"
  | Border_right_width -> "border-right-width"
  | Border_bottom_width -> "border-bottom-width"
  | Border_left_width -> "border-left-width"
  | Outline -> "outline"
  | Outline_offset -> "outline-offset"
  | Clip -> "clip"
  | Filter -> "filter"
  | Background_image -> "background-image"
  | Animation -> "animation"
  | Appearance -> "appearance"
  | Overflow_x -> "overflow-x"
  | Overflow_y -> "overflow-y"
  | Resize -> "resize"
  | Vertical_align -> "vertical-align"
  | Box_sizing -> "box-sizing"
  | Font_family -> "font-family"
  | Background_position -> "background-position"
  | Background_repeat -> "background-repeat"
  | Background_size -> "background-size"
  | Webkit_font_smoothing -> "-webkit-font-smoothing"
  | Moz_osx_font_smoothing -> "-moz-osx-font-smoothing"
  | Webkit_line_clamp -> "-webkit-line-clamp"
  | Backdrop_filter -> "backdrop-filter"
  | Scroll_snap_type -> "scroll-snap-type"
  | Scroll_snap_align -> "scroll-snap-align"
  | Scroll_snap_stop -> "scroll-snap-stop"
  | Scroll_behavior -> "scroll-behavior"
  | Custom s -> s

let rule ~selector properties = { selector; properties }
let selector rule = rule.selector
let properties rule = rule.properties
let media ~condition rules = { condition; rules }
let stylesheet ?(media_queries = []) rules = { rules; media_queries }

(** {1 Utilities} *)

let deduplicate_properties props =
  (* Keep last occurrence of each property while preserving order *)
  let seen = Hashtbl.create 16 in
  List.fold_right
    (fun (prop_name, value) acc ->
      if Hashtbl.mem seen prop_name then acc
      else (
        Hashtbl.add seen prop_name ();
        (prop_name, value) :: acc))
    props []

let properties_to_inline_style props =
  props
  |> List.map (fun (prop_name, value) ->
         property_name_to_string prop_name ^ ": " ^ value)
  |> String.concat "; "

let merge_rules rules =
  (* Group rules by selector *)
  let tbl = Hashtbl.create 16 in
  List.iter
    (fun rule ->
      let existing =
        try Hashtbl.find tbl rule.selector with Not_found -> []
      in
      Hashtbl.replace tbl rule.selector (existing @ rule.properties))
    rules;

  (* Create merged rules *)
  Hashtbl.fold
    (fun selector properties acc ->
      { selector; properties = deduplicate_properties properties } :: acc)
    tbl []
  |> List.sort (fun a b -> String.compare a.selector b.selector)

(* Merge rules with identical properties into combined selectors *)
let merge_by_properties rules =
  (* Create a hash of properties for comparison *)
  let properties_hash props =
    props
    |> List.map (fun (name, value) ->
           property_name_to_string name ^ ":" ^ value)
    |> List.sort String.compare |> String.concat ";"
  in

  (* Group rules by their properties *)
  let groups = Hashtbl.create 16 in
  List.iter
    (fun rule ->
      let hash = properties_hash rule.properties in
      let existing = try Hashtbl.find groups hash with Not_found -> [] in
      Hashtbl.replace groups hash (rule :: existing))
    rules;

  (* Create merged rules *)
  Hashtbl.fold
    (fun _hash rules_with_same_props acc ->
      match rules_with_same_props with
      | [] -> acc
      | [ rule ] -> rule :: acc
      | multiple ->
          let selectors =
            multiple
            |> List.map (fun r -> r.selector)
            |> List.sort String.compare |> String.concat ","
          in
          let properties = (List.hd multiple).properties in
          { selector = selectors; properties } :: acc)
    groups []
  |> List.sort (fun a b -> String.compare a.selector b.selector)

let minify_selector s =
  (* Remove unnecessary whitespace in selectors *)
  s
  |> Re.replace_string
       (Re.compile (Re.seq [ Re.rep Re.space; Re.char '>'; Re.rep Re.space ]))
       ~by:">"
  |> Re.replace_string
       (Re.compile (Re.seq [ Re.rep Re.space; Re.char '+'; Re.rep Re.space ]))
       ~by:"+"
  |> Re.replace_string
       (Re.compile (Re.seq [ Re.rep Re.space; Re.char '~'; Re.rep Re.space ]))
       ~by:"~"
  |> Re.replace_string
       (Re.compile (Re.seq [ Re.rep Re.space; Re.char ','; Re.rep Re.space ]))
       ~by:","
  |> Re.replace_string
       (Re.compile (Re.seq [ Re.rep Re.space; Re.char ':'; Re.rep Re.space ]))
       ~by:":"
  |> String.trim

let minify_value v =
  let v = String.trim v in
  (* Special case: plain "0" or "1" should stay as is *)
  if v = "0" || v = "1" then v
  else
    (* Remove leading 0 from decimals but preserve units - e.g., "0.5rem" ->
       ".5rem" *)
    let decimal_re =
      Re.compile
        (Re.seq
           [
             Re.bos;
             Re.str "0.";
             Re.group (Re.rep1 Re.digit);
             Re.group (Re.rep Re.any);
           ])
    in
    match Re.exec_opt decimal_re v with
    | Some m -> "." ^ Re.Group.get m 1 ^ Re.Group.get m 2
    | None -> v

(** {1 Rendering} *)

let render_minified_rule rule =
  let selector = minify_selector rule.selector in
  let props =
    rule.properties
    |> List.map (fun (prop_name, value) ->
           property_name_to_string prop_name ^ ":" ^ minify_value value)
  in
  Pp.str [ selector; "{"; Pp.sep ";" props; "}" ]

let render_formatted_rule rule =
  let props =
    rule.properties
    |> List.map (fun (prop_name, value) ->
           Pp.str [ "  "; property_name_to_string prop_name; ": "; value; ";" ])
  in
  Pp.lines [ Pp.str [ rule.selector; " {" ]; Pp.lines props; "}" ]

let render_formatted_media_rule rule =
  let props =
    rule.properties
    |> List.map (fun (prop_name, value) ->
           Pp.str
             [ "    "; property_name_to_string prop_name; ": "; value; ";" ])
  in
  Pp.str [ "  "; rule.selector; " {\n"; Pp.lines props; "\n  }" ]

let to_string ?(minify = false) stylesheet =
  if minify then
    let rules = merge_rules stylesheet.rules |> merge_by_properties in
    let rule_strings = List.map render_minified_rule rules in
    let media_strings =
      stylesheet.media_queries
      |> List.map (fun (mq : media_query) ->
             let rules_str =
               mq.rules |> merge_rules |> merge_by_properties
               |> List.map render_minified_rule
               |> String.concat ""
             in
             "@media " ^ mq.condition ^ "{" ^ rules_str ^ "}")
    in
    String.concat "" (rule_strings @ media_strings)
  else
    let rule_strings = List.map render_formatted_rule stylesheet.rules in
    let media_strings =
      stylesheet.media_queries
      |> List.map (fun (mq : media_query) ->
             let rules_str =
               mq.rules
               |> List.map render_formatted_media_rule
               |> String.concat "\n"
             in
             Pp.lines
               [
                 Pp.str [ "@media "; mq.condition; " {" ];
                 Pp.indent 2 rules_str;
                 "}";
               ])
    in
    String.concat "\n\n" (rule_strings @ media_strings)
