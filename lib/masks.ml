(** Mask utilities for CSS masking.

    Provides utilities for mask-image, mask-composite, mask-mode, mask-type,
    mask-size, mask-position, mask-repeat, mask-clip, and mask-origin. *)

module Css = Cascade.Css

module Handler = struct
  open Style
  open Css

  (* The position keywords [mask-<position>] accepts. *)
  module Keyword = struct
    type t =
      | Bottom
      | Bottom_left
      | Bottom_right
      | Center
      | Left
      | Right
      | Top
      | Top_left
      | Top_right
  end

  type t =
    | No_mask
    | Add
    | Exclude
    | Intersect
    | Subtract
    | Alpha
    | Luminance
    | Match
    | Type_alpha
    | Type_luminance
    | Auto
    | Contain
    | Cover
    | Position of Keyword.t
    | No_repeat
    | Repeat
    | Repeat_round
    | Repeat_space
    | Repeat_x
    | Repeat_y
    | Clip_border
    | Clip_padding
    | Clip_content
    | Clip_fill
    | Clip_stroke
    | Clip_view
    | No_clip
    | Origin_border
    | Origin_padding
    | Origin_content
    | Origin_fill
    | Origin_stroke
    | Origin_view
    (* Bracket notation *)
    | Bracket_contain
    | Bracket_cover
    | Bracket_size of string
    | Bracket_length of string
    | Bracket_position of string * Css.position_value list
    | Bracket_typed_position of string * Css.position_value list
    | Bracket_image_var of string
    | Bracket_url of string
    | Bracket_url_var of string
    | Bracket_var of string
    | Bracket_image of string
    (* Sub-property bracket notation: mask-position-[...], mask-size-[...] *)
    | Position_bracket of string * Css.position_value list
    | Position_bracket_var of string
    | Size_bracket of string
    | Size_bracket_var of string

  let name = "masks"

  (* After the backgrounds and the mask-gradient utilities, before fill and
     stroke and before padding - Tailwind's own order. *)
  let priority _ = 21

  (* Helper to create webkit + standard declarations for mask properties *)

  let mask_none =
    style [ Css.webkit_mask_image Css.None; Css.mask_image Css.None ]

  let mask_add =
    style
      [
        Css.webkit_mask_composite Source_over;
        Css.webkit_mask_composite Source_over;
        Css.mask_composite Css.Add;
      ]

  let mask_exclude =
    style
      [
        Css.webkit_mask_composite Xor;
        Css.webkit_mask_composite Xor;
        Css.mask_composite Css.Exclude;
      ]

  let mask_intersect =
    style
      [
        Css.webkit_mask_composite Source_in;
        Css.webkit_mask_composite Source_in;
        Css.mask_composite Css.Intersect;
      ]

  let mask_subtract =
    style
      [
        Css.webkit_mask_composite Source_out;
        Css.webkit_mask_composite Source_out;
        Css.mask_composite Css.Subtract;
      ]

  let mask_alpha =
    style
      [
        Css.webkit_mask_source_type Css.Alpha;
        Css.webkit_mask_source_type Css.Alpha;
        Css.mask_mode Css.Alpha;
      ]

  let mask_luminance =
    style
      [
        Css.webkit_mask_source_type Css.Luminance;
        Css.webkit_mask_source_type Css.Luminance;
        Css.mask_mode Css.Luminance;
      ]

  let mask_match =
    style
      [
        Css.webkit_mask_source_type Css.Auto;
        Css.webkit_mask_source_type Css.Auto;
        Css.mask_mode Match_source;
      ]

  let mask_type_alpha = style [ Css.mask_type Css.Alpha ]
  let mask_type_luminance = style [ Css.mask_type Css.Luminance ]

  (* mask-size *)
  let mask_auto =
    style [ Css.webkit_mask_size Css.Auto; Css.mask_size Css.Auto ]

  let mask_contain =
    style [ Css.webkit_mask_size Css.Contain; Css.mask_size Css.Contain ]

  let mask_cover =
    style [ Css.webkit_mask_size Css.Cover; Css.mask_size Css.Cover ]

  (* mask-position *)
  let mask_position' pos =
    let pos_val : Css.position_value list =
      match pos with
      | Keyword.Bottom -> [ Center_bottom ]
      | Keyword.Bottom_left -> [ XY (Pct 0., Pct 100.) ]
      | Keyword.Bottom_right -> [ XY (Pct 100., Pct 100.) ]
      | Keyword.Center -> [ Center ]
      | Keyword.Left -> [ Single (Pct 0.) ]
      | Keyword.Right -> [ Single (Pct 100.) ]
      | Keyword.Top -> [ Center_top ]
      | Keyword.Top_left -> [ XY (Pct 0., Pct 0.) ]
      | Keyword.Top_right -> [ XY (Pct 100., Pct 0.) ]
    in
    style [ Css.webkit_mask_position pos_val; Css.mask_position pos_val ]

  (* mask-repeat *)
  let mask_no_repeat' =
    style
      [ Css.webkit_mask_repeat Css.No_repeat; Css.mask_repeat Css.No_repeat ]

  let mask_repeat' =
    style [ Css.webkit_mask_repeat Css.Repeat; Css.mask_repeat Css.Repeat ]

  let mask_repeat_round' =
    style [ Css.webkit_mask_repeat Round; Css.mask_repeat Round ]

  let mask_repeat_space' =
    style [ Css.webkit_mask_repeat Space; Css.mask_repeat Space ]

  let mask_repeat_x' =
    style [ Css.webkit_mask_repeat Css.Repeat_x; Css.mask_repeat Css.Repeat_x ]

  let mask_repeat_y' =
    style [ Css.webkit_mask_repeat Css.Repeat_y; Css.mask_repeat Css.Repeat_y ]

  (* mask-clip utilities *)
  let mask_clip_border =
    style [ Css.webkit_mask_clip Border_box; Css.mask_clip Border_box ]

  let mask_clip_padding =
    style [ Css.webkit_mask_clip Padding_box; Css.mask_clip Padding_box ]

  let mask_clip_content =
    style [ Css.webkit_mask_clip Content_box; Css.mask_clip Content_box ]

  let mask_clip_fill =
    style [ Css.webkit_mask_clip Fill_box; Css.mask_clip Fill_box ]

  let mask_clip_stroke =
    style [ Css.webkit_mask_clip Stroke_box; Css.mask_clip Stroke_box ]

  let mask_clip_view =
    style [ Css.webkit_mask_clip View_box; Css.mask_clip View_box ]

  let mask_no_clip =
    style [ Css.webkit_mask_clip Css.No_clip; Css.mask_clip Css.No_clip ]

  (* mask-origin utilities *)
  let mask_origin_border =
    style [ Css.webkit_mask_origin Border_box; Css.mask_origin Border_box ]

  let mask_origin_padding =
    style [ Css.webkit_mask_origin Padding_box; Css.mask_origin Padding_box ]

  let mask_origin_content =
    style [ Css.webkit_mask_origin Content_box; Css.mask_origin Content_box ]

  let mask_origin_fill =
    style [ Css.webkit_mask_origin Fill_box; Css.mask_origin Fill_box ]

  let mask_origin_stroke =
    style [ Css.webkit_mask_origin Stroke_box; Css.mask_origin Stroke_box ]

  let mask_origin_view =
    style [ Css.webkit_mask_origin View_box; Css.mask_origin View_box ]

  (* Bracket notation helpers *)

  (* [mask-size-[...]], [mask-[size:...]] and [mask-[length:...]] take any CSS
     length, so a token is read with the value parser rather than a hand-picked
     unit table. *)
  let parse_bracket_len s = Parse.arbitrary_length s

  let parse_bracket_size inner =
    let parts =
      String.split_on_char '_' inner |> List.filter (fun s -> s <> "")
    in
    match parts with
    | [ w; h ] -> (
        match (parse_bracket_len w, parse_bracket_len h) with
        | Some wl, Some hl ->
            Some
              [
                Css.webkit_mask_size (Size (wl, hl));
                Css.mask_size (Size (wl, hl));
              ]
        | _ -> None)
    | [ v ] ->
        Option.map
          (fun l ->
            let size : Css.background_size = Length l in
            [ Css.webkit_mask_size size; Css.mask_size size ])
          (parse_bracket_len v)
    | _ -> None

  (* A bracket mask-position: the whole [<position>] grammar, one position per
     mask layer, comma-separated. [None] means the bracket is not a position,
     which [of_class] rejects: [mask-[position:top]] used to fall through the
     hand-rolled reading to a plausible-looking [center]. *)
  let parse_bracket_position inner : Css.position_value list option =
    let one entry : Css.position_value option =
      let cursor = Cascade.Cursor.of_string (Parse.decode_underscores entry) in
      match
        Cascade.Cursor.try_parse_full_err Css.Properties.read_position_value
          cursor
      with
      | Ok pos -> Some pos
      | Error _ -> None
    in
    let entries = String.split_on_char ',' inner |> List.map String.trim in
    let positions = List.map one entries in
    if List.exists Option.is_none positions then None
    else Some (List.filter_map Fun.id positions)

  let mask_position_style positions =
    style [ Css.webkit_mask_position positions; Css.mask_position positions ]

  let to_style _theme = function
    | No_mask -> mask_none
    | Add -> mask_add
    | Exclude -> mask_exclude
    | Intersect -> mask_intersect
    | Subtract -> mask_subtract
    | Alpha -> mask_alpha
    | Luminance -> mask_luminance
    | Match -> mask_match
    | Type_alpha -> mask_type_alpha
    | Type_luminance -> mask_type_luminance
    | Auto -> mask_auto
    | Contain -> mask_contain
    | Cover -> mask_cover
    | Position pos -> mask_position' pos
    | No_repeat -> mask_no_repeat'
    | Repeat -> mask_repeat'
    | Repeat_round -> mask_repeat_round'
    | Repeat_space -> mask_repeat_space'
    | Repeat_x -> mask_repeat_x'
    | Repeat_y -> mask_repeat_y'
    | Clip_border -> mask_clip_border
    | Clip_padding -> mask_clip_padding
    | Clip_content -> mask_clip_content
    | Clip_fill -> mask_clip_fill
    | Clip_stroke -> mask_clip_stroke
    | Clip_view -> mask_clip_view
    | No_clip -> mask_no_clip
    | Origin_border -> mask_origin_border
    | Origin_padding -> mask_origin_padding
    | Origin_content -> mask_origin_content
    | Origin_fill -> mask_origin_fill
    | Origin_stroke -> mask_origin_stroke
    | Origin_view -> mask_origin_view
    (* Bracket notation *)
    | Bracket_contain ->
        style [ Css.webkit_mask_size Css.Contain; Css.mask_size Css.Contain ]
    | Bracket_cover ->
        style [ Css.webkit_mask_size Css.Cover; Css.mask_size Css.Cover ]
    | Bracket_size inner -> (
        match parse_bracket_size inner with
        | Some decls -> style decls
        | None ->
            style [ Css.webkit_mask_size Css.Auto; Css.mask_size Css.Auto ])
    | Bracket_length inner -> (
        match parse_bracket_size inner with
        | Some decls -> style decls
        | None ->
            style [ Css.webkit_mask_size Css.Auto; Css.mask_size Css.Auto ])
    | Bracket_position (_, positions) -> mask_position_style positions
    | Bracket_typed_position (_, positions) -> mask_position_style positions
    | Bracket_image_var v ->
        let bare = Parse.extract_var_name v in
        let var_ref : Css.background_image Css.var = Var.bracket bare in
        style
          [
            Css.webkit_mask_image (Var var_ref);
            Css.webkit_mask_image (Var var_ref);
            Css.mask_image (Var var_ref);
          ]
    (* The whole [url()] is kept, so cascade reads the token rather than masks
       slicing the file name out of it: the quotes are the tokeniser's, and an
       escape in it stands for one character of the URL. [of_class] refuses a
       value the token reader will not take, so [None] names nothing. *)
    | Bracket_url v -> (
        match Parse.url_token (Parse.decode_arbitrary_value v) with
        | Some url ->
            style [ Css.webkit_mask_image (Url url); Css.mask_image (Url url) ]
        | None -> style [])
    | Bracket_url_var v ->
        let bare = Parse.extract_var_name v in
        let var_ref : Css.background_image Css.var = Var.bracket bare in
        style
          [
            Css.webkit_mask_image (Var var_ref);
            Css.webkit_mask_image (Var var_ref);
            Css.mask_image (Var var_ref);
          ]
    | Bracket_var v ->
        let bare = Parse.extract_var_name v in
        let var_ref : Css.background_image Css.var = Var.bracket bare in
        style
          [
            Css.webkit_mask_image (Var var_ref);
            Css.webkit_mask_image (Var var_ref);
            Css.mask_image (Var var_ref);
          ]
    | Bracket_image v -> (
        match Css.parse_background_image (Parse.decode_underscores v) with
        | Some (img :: _) ->
            style [ Css.webkit_mask_image img; Css.mask_image img ]
        | _ ->
            invalid_arg ("mask-[" ^ v ^ "]: not a valid background-image value")
        )
    (* Sub-property bracket notation *)
    | Position_bracket (_, positions) -> mask_position_style positions
    | Position_bracket_var v ->
        let bare = Parse.extract_var_name v in
        let var_ref : Css.position_value Css.var = Var.bracket bare in
        style
          [
            Css.webkit_mask_position [ Var var_ref ];
            Css.webkit_mask_position [ Var var_ref ];
            Css.mask_position [ Var var_ref ];
          ]
    | Size_bracket inner -> (
        match parse_bracket_size inner with
        | Some decls -> style decls
        | None ->
            style [ Css.webkit_mask_size Css.Auto; Css.mask_size Css.Auto ])
    | Size_bracket_var v ->
        let bare = Parse.extract_var_name v in
        let var_ref : Css.background_size Css.var = Var.bracket bare in
        style
          [
            Css.webkit_mask_size (Var var_ref);
            Css.webkit_mask_size (Var var_ref);
            Css.mask_size (Var var_ref);
          ]

  (* Tailwind sorts by the property a utility sets, at the rank its property
     table gives it: mask-image (209), then mask-composite (257) through
     mask-origin (264). Utilities that set the same property share a slot, where
     the class name breaks the tie. *)
  let property_rank = function
    | Bracket_image_var _ | Bracket_image _ | Bracket_url _ | Bracket_url_var _
    | Bracket_var _ | No_mask ->
        209
    | Add | Exclude | Intersect | Subtract -> 257
    | Alpha | Luminance | Match -> 258
    | Type_alpha | Type_luminance -> 259
    | Bracket_contain | Bracket_cover | Bracket_length _ | Bracket_size _ | Auto
    | Contain | Cover | Size_bracket _ | Size_bracket_var _ ->
        260
    | Clip_border | Clip_content | Clip_fill | Clip_padding | Clip_stroke
    | Clip_view | No_clip ->
        261
    | Bracket_position _ | Bracket_typed_position _ | Position _
    | Position_bracket _ | Position_bracket_var _ ->
        262
    | No_repeat | Repeat | Repeat_round | Repeat_space | Repeat_x | Repeat_y ->
        263
    | Origin_border | Origin_content | Origin_fill | Origin_padding
    | Origin_stroke | Origin_view ->
        264

  (* Each of these writes one property and stops there, so it closes that
     property's slot: the mask-gradient utilities that write mask-image and
     carry on sort inside mask-image's, and the background sizing utilities that
     share this priority take the slots between the two families. *)
  let suborder t = Utility.Property_order.last (property_rank t)

  (* [mask-[<image>]] takes any background-image value, so what makes one is
     whether the value parser accepts it, not which gradient function it
     names. *)
  let is_image_value inner =
    match Css.parse_background_image (Parse.decode_underscores inner) with
    | Some (_ :: _) -> true
    | _ -> false

  let of_class _theme class_name =
    let parts = Parse.split_class class_name in
    match parts with
    | [ "mask"; "none" ] -> Ok No_mask
    | [ "mask"; "add" ] -> Ok Add
    | [ "mask"; "exclude" ] -> Ok Exclude
    | [ "mask"; "intersect" ] -> Ok Intersect
    | [ "mask"; "subtract" ] -> Ok Subtract
    | [ "mask"; "alpha" ] -> Ok Alpha
    | [ "mask"; "luminance" ] -> Ok Luminance
    | [ "mask"; "match" ] -> Ok Match
    | [ "mask"; "type"; "alpha" ] -> Ok Type_alpha
    | [ "mask"; "type"; "luminance" ] -> Ok Type_luminance
    | [ "mask"; "auto" ] -> Ok Auto
    | [ "mask"; "contain" ] -> Ok Contain
    | [ "mask"; "cover" ] -> Ok Cover
    (* mask-position *)
    | [ "mask"; "bottom" ] -> Ok (Position Keyword.Bottom)
    | [ "mask"; "bottom"; "left" ] -> Ok (Position Keyword.Bottom_left)
    | [ "mask"; "bottom"; "right" ] -> Ok (Position Keyword.Bottom_right)
    | [ "mask"; "center" ] -> Ok (Position Keyword.Center)
    | [ "mask"; "left" ] -> Ok (Position Keyword.Left)
    | [ "mask"; "right" ] -> Ok (Position Keyword.Right)
    | [ "mask"; "top" ] -> Ok (Position Keyword.Top)
    | [ "mask"; "top"; "left" ] -> Ok (Position Keyword.Top_left)
    | [ "mask"; "top"; "right" ] -> Ok (Position Keyword.Top_right)
    (* mask-repeat *)
    | [ "mask"; "no"; "repeat" ] -> Ok No_repeat
    | [ "mask"; "repeat" ] -> Ok Repeat
    | [ "mask"; "repeat"; "round" ] -> Ok Repeat_round
    | [ "mask"; "repeat"; "space" ] -> Ok Repeat_space
    | [ "mask"; "repeat"; "x" ] -> Ok Repeat_x
    | [ "mask"; "repeat"; "y" ] -> Ok Repeat_y
    (* mask-clip *)
    | [ "mask"; "clip"; "border" ] -> Ok Clip_border
    | [ "mask"; "clip"; "padding" ] -> Ok Clip_padding
    | [ "mask"; "clip"; "content" ] -> Ok Clip_content
    | [ "mask"; "clip"; "fill" ] -> Ok Clip_fill
    | [ "mask"; "clip"; "stroke" ] -> Ok Clip_stroke
    | [ "mask"; "clip"; "view" ] -> Ok Clip_view
    | [ "mask"; "no"; "clip" ] -> Ok No_clip
    (* mask-origin *)
    | [ "mask"; "origin"; "border" ] -> Ok Origin_border
    | [ "mask"; "origin"; "padding" ] -> Ok Origin_padding
    | [ "mask"; "origin"; "content" ] -> Ok Origin_content
    | [ "mask"; "origin"; "fill" ] -> Ok Origin_fill
    | [ "mask"; "origin"; "stroke" ] -> Ok Origin_stroke
    | [ "mask"; "origin"; "view" ] -> Ok Origin_view
    (* Sub-property bracket notation: mask-position-[...], mask-size-[...] *)
    | [ "mask"; "position"; bracket ] when Parse.is_bracket_value bracket -> (
        let inner = Parse.bracket_inner bracket in
        if Parse.is_var inner then Ok (Position_bracket_var inner)
        else
          match parse_bracket_position inner with
          | Some positions -> Ok (Position_bracket (inner, positions))
          | None -> Error (`Msg "Invalid mask-position value"))
    | [ "mask"; "size"; bracket ] when Parse.is_bracket_value bracket ->
        let inner = Parse.bracket_inner bracket in
        if Parse.is_var inner then Ok (Size_bracket_var inner)
        else if parse_bracket_size inner = None then
          Error (`Msg "Invalid mask-size value")
        else Ok (Size_bracket inner)
    (* Bracket notation: mask-[...] *)
    | [ "mask"; bracket ] when Parse.is_bracket_value bracket -> (
        let inner = Parse.bracket_inner bracket in
        match inner with
        | "contain" -> Ok Bracket_contain
        | "cover" -> Ok Bracket_cover
        | _ when String.length inner > 7 && String.sub inner 0 7 = "length:" ->
            (* The [length:] hint forces a mask-size; a value the size grammar
               cannot take is not a utility. It used to fall through to a
               plausible-looking [auto]. *)
            let v = String.sub inner 7 (String.length inner - 7) in
            if parse_bracket_size v = None then
              Error (`Msg ("Unknown mask bracket length: " ^ v))
            else Ok (Bracket_length v)
        | _ when String.length inner > 5 && String.sub inner 0 5 = "size:" ->
            let v = String.sub inner 5 (String.length inner - 5) in
            if parse_bracket_size v = None then
              Error (`Msg ("Unknown mask bracket size: " ^ v))
            else Ok (Bracket_size v)
        | _ when String.length inner > 9 && String.sub inner 0 9 = "position:"
          -> (
            (* The [position:] data-type hint forces a mask-position; a value
               the grammar rejects is not a utility. It used to fall through to
               a plausible-looking [center]. *)
            let v = String.sub inner 9 (String.length inner - 9) in
            match parse_bracket_position v with
            | Some positions -> Ok (Bracket_typed_position (v, positions))
            | None -> Error (`Msg ("Unknown mask bracket position: " ^ v)))
        | _ when String.length inner > 6 && String.sub inner 0 6 = "image:" ->
            Ok
              (Bracket_image_var (String.sub inner 6 (String.length inner - 6)))
        | _ when String.length inner > 4 && String.sub inner 0 4 = "url:" ->
            Ok (Bracket_url_var (String.sub inner 4 (String.length inner - 4)))
        (* Before the [url(...)] reading below, which takes one whole token and
           so has no answer for the comma of a layer list. *)
        | _ when is_image_value inner -> Ok (Bracket_image inner)
        | _ when String.starts_with ~prefix:"url(" inner -> (
            match Parse.url_token (Parse.decode_arbitrary_value inner) with
            | Some _ -> Ok (Bracket_url inner)
            | None -> Error (`Msg ("Unknown mask bracket url: " ^ inner)))
        | _ when Parse.is_var inner -> Ok (Bracket_var inner)
        | _ -> (
            match parse_bracket_position inner with
            | Some positions -> Ok (Bracket_position (inner, positions))
            | None -> Error (`Msg ("Unknown mask bracket value: " ^ inner))))
    | _ -> Error (`Msg "Not a mask utility")

  let to_class = function
    | No_mask -> "mask-none"
    | Add -> "mask-add"
    | Exclude -> "mask-exclude"
    | Intersect -> "mask-intersect"
    | Subtract -> "mask-subtract"
    | Alpha -> "mask-alpha"
    | Luminance -> "mask-luminance"
    | Match -> "mask-match"
    | Type_alpha -> "mask-type-alpha"
    | Type_luminance -> "mask-type-luminance"
    | Auto -> "mask-auto"
    | Contain -> "mask-contain"
    | Cover -> "mask-cover"
    | Position Keyword.Bottom -> "mask-bottom"
    | Position Keyword.Bottom_left -> "mask-bottom-left"
    | Position Keyword.Bottom_right -> "mask-bottom-right"
    | Position Keyword.Center -> "mask-center"
    | Position Keyword.Left -> "mask-left"
    | Position Keyword.Right -> "mask-right"
    | Position Keyword.Top -> "mask-top"
    | Position Keyword.Top_left -> "mask-top-left"
    | Position Keyword.Top_right -> "mask-top-right"
    | No_repeat -> "mask-no-repeat"
    | Repeat -> "mask-repeat"
    | Repeat_round -> "mask-repeat-round"
    | Repeat_space -> "mask-repeat-space"
    | Repeat_x -> "mask-repeat-x"
    | Repeat_y -> "mask-repeat-y"
    | Clip_border -> "mask-clip-border"
    | Clip_padding -> "mask-clip-padding"
    | Clip_content -> "mask-clip-content"
    | Clip_fill -> "mask-clip-fill"
    | Clip_stroke -> "mask-clip-stroke"
    | Clip_view -> "mask-clip-view"
    | No_clip -> "mask-no-clip"
    | Origin_border -> "mask-origin-border"
    | Origin_padding -> "mask-origin-padding"
    | Origin_content -> "mask-origin-content"
    | Origin_fill -> "mask-origin-fill"
    | Origin_stroke -> "mask-origin-stroke"
    | Origin_view -> "mask-origin-view"
    | Bracket_contain -> "mask-[contain]"
    | Bracket_cover -> "mask-[cover]"
    | Bracket_size v -> "mask-[size:" ^ v ^ "]"
    | Bracket_length v -> "mask-[length:" ^ v ^ "]"
    | Bracket_position (v, _) -> "mask-[" ^ v ^ "]"
    | Bracket_typed_position (v, _) -> "mask-[position:" ^ v ^ "]"
    | Bracket_image_var v -> "mask-[image:" ^ v ^ "]"
    | Bracket_url v -> "mask-[" ^ v ^ "]"
    | Bracket_url_var v -> "mask-[url:" ^ v ^ "]"
    | Bracket_var v -> "mask-[" ^ v ^ "]"
    | Bracket_image v -> "mask-[" ^ v ^ "]"
    | Position_bracket (v, _) -> "mask-position-[" ^ v ^ "]"
    | Position_bracket_var v -> "mask-position-[" ^ v ^ "]"
    | Size_bracket v -> "mask-size-[" ^ v ^ "]"
    | Size_bracket_var v -> "mask-size-[" ^ v ^ "]"

  let examples =
    [ No_mask; Clip_border; Origin_border; Repeat; Type_alpha; Add; Alpha ]
end

open Handler
module Utility_factory = Utility.Make (Handler)

let utility = Utility_factory.v
let mask_none = utility No_mask
let mask_add = utility Add
let mask_exclude = utility Exclude
let mask_intersect = utility Intersect
let mask_subtract = utility Subtract
let mask_alpha = utility Alpha
let mask_luminance = utility Luminance
let mask_match = utility Match
let mask_type_alpha = utility Type_alpha
let mask_type_luminance = utility Type_luminance
let mask_auto = utility Auto
let mask_contain = utility Contain
let mask_cover = utility Cover
let mask_bottom = utility (Position Keyword.Bottom)
let mask_bottom_left = utility (Position Keyword.Bottom_left)
let mask_bottom_right = utility (Position Keyword.Bottom_right)
let mask_center = utility (Position Keyword.Center)
let mask_left = utility (Position Keyword.Left)
let mask_right = utility (Position Keyword.Right)
let mask_top = utility (Position Keyword.Top)
let mask_top_left = utility (Position Keyword.Top_left)
let mask_top_right = utility (Position Keyword.Top_right)
let mask_no_repeat = utility No_repeat
let mask_repeat = utility Repeat
let mask_repeat_round = utility Repeat_round
let mask_repeat_space = utility Repeat_space
let mask_repeat_x = utility Repeat_x
let mask_repeat_y = utility Repeat_y
let mask_clip_border = utility Clip_border
let mask_clip_padding = utility Clip_padding
let mask_clip_content = utility Clip_content
let mask_clip_fill = utility Clip_fill
let mask_clip_stroke = utility Clip_stroke
let mask_clip_view = utility Clip_view
let mask_no_clip = utility No_clip
let mask_origin_border = utility Origin_border
let mask_origin_padding = utility Origin_padding
let mask_origin_content = utility Origin_content
let mask_origin_fill = utility Origin_fill
let mask_origin_stroke = utility Origin_stroke
let mask_origin_view = utility Origin_view
