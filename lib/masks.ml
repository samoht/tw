(** Mask utilities for CSS masking.

    Provides utilities for mask-image, mask-composite, mask-mode, mask-type,
    mask-size, mask-position, mask-repeat, mask-clip, and mask-origin. *)

open Cascade

module Handler = struct
  open Style
  open Css

  type position_keyword =
    | Pos_bottom
    | Pos_bottom_left
    | Pos_bottom_right
    | Pos_center
    | Pos_left
    | Pos_right
    | Pos_top
    | Pos_top_left
    | Pos_top_right

  type t =
    | Mask_none
    | Mask_add
    | Mask_exclude
    | Mask_intersect
    | Mask_subtract
    | Mask_alpha
    | Mask_luminance
    | Mask_match
    | Mask_type_alpha
    | Mask_type_luminance
    | Mask_auto
    | Mask_contain
    | Mask_cover
    | Mask_position of position_keyword
    | Mask_no_repeat
    | Mask_repeat
    | Mask_repeat_round
    | Mask_repeat_space
    | Mask_repeat_x
    | Mask_repeat_y
    | Mask_clip_border
    | Mask_clip_padding
    | Mask_clip_content
    | Mask_clip_fill
    | Mask_clip_stroke
    | Mask_clip_view
    | Mask_no_clip
    | Mask_origin_border
    | Mask_origin_padding
    | Mask_origin_content
    | Mask_origin_fill
    | Mask_origin_stroke
    | Mask_origin_view
    (* Bracket notation *)
    | Mask_bracket_contain
    | Mask_bracket_cover
    | Mask_bracket_size of string
    | Mask_bracket_length of string
    | Mask_bracket_position of string
    | Mask_bracket_typed_position of string
    | Mask_bracket_image_var of string
    | Mask_bracket_url of string
    | Mask_bracket_url_var of string
    | Mask_bracket_var of string
    | Mask_bracket_linear_gradient of string
    (* Sub-property bracket notation: mask-position-[...], mask-size-[...] *)
    | Mask_position_bracket of string
    | Mask_position_bracket_var of string
    | Mask_size_bracket of string
    | Mask_size_bracket_var of string

  type Utility.base += Self of t

  let name = "masks"
  let priority = 21 (* After backgrounds, before filters *)

  (* Helper to create webkit + standard declarations for mask properties *)

  let mask_none =
    style [ Css.webkit_mask_image Css.None; Css.mask_image Css.None ]

  let mask_add =
    style
      [
        Css.webkit_mask_composite Source_over;
        Css.webkit_mask_composite Source_over;
        Css.mask_composite Add;
      ]

  let mask_exclude =
    style
      [
        Css.webkit_mask_composite Xor;
        Css.webkit_mask_composite Xor;
        Css.mask_composite Exclude;
      ]

  let mask_intersect =
    style
      [
        Css.webkit_mask_composite Source_in;
        Css.webkit_mask_composite Source_in;
        Css.mask_composite Intersect;
      ]

  let mask_subtract =
    style
      [
        Css.webkit_mask_composite Source_out;
        Css.webkit_mask_composite Source_out;
        Css.mask_composite Subtract;
      ]

  let mask_alpha =
    style
      [
        Css.webkit_mask_source_type Alpha;
        Css.webkit_mask_source_type Alpha;
        Css.mask_mode Alpha;
      ]

  let mask_luminance =
    style
      [
        Css.webkit_mask_source_type Luminance;
        Css.webkit_mask_source_type Luminance;
        Css.mask_mode Luminance;
      ]

  let mask_match =
    style
      [
        Css.webkit_mask_source_type Auto;
        Css.webkit_mask_source_type Auto;
        Css.mask_mode Match_source;
      ]

  let mask_type_alpha = style [ Css.mask_type Alpha ]
  let mask_type_luminance = style [ Css.mask_type Luminance ]

  (* mask-size *)
  let mask_auto = style [ Css.webkit_mask_size Auto; Css.mask_size Auto ]

  let mask_contain =
    style [ Css.webkit_mask_size Contain; Css.mask_size Contain ]

  let mask_cover = style [ Css.webkit_mask_size Cover; Css.mask_size Cover ]

  (* mask-position *)
  let mask_position' pos =
    let pos_val : Css.position_value list =
      match pos with
      | Pos_bottom -> [ Center_bottom ]
      | Pos_bottom_left -> [ XY (Pct 0., Pct 100.) ]
      | Pos_bottom_right -> [ XY (Pct 100., Pct 100.) ]
      | Pos_center -> [ Center ]
      | Pos_left -> [ Single (Pct 0.) ]
      | Pos_right -> [ Single (Pct 100.) ]
      | Pos_top -> [ Center_top ]
      | Pos_top_left -> [ XY (Pct 0., Pct 0.) ]
      | Pos_top_right -> [ XY (Pct 100., Pct 0.) ]
    in
    style [ Css.webkit_mask_position pos_val; Css.mask_position pos_val ]

  (* mask-repeat *)
  let mask_no_repeat' =
    style [ Css.webkit_mask_repeat No_repeat; Css.mask_repeat No_repeat ]

  let mask_repeat' =
    style [ Css.webkit_mask_repeat Repeat; Css.mask_repeat Repeat ]

  let mask_repeat_round' =
    style [ Css.webkit_mask_repeat Round; Css.mask_repeat Round ]

  let mask_repeat_space' =
    style [ Css.webkit_mask_repeat Space; Css.mask_repeat Space ]

  let mask_repeat_x' =
    style [ Css.webkit_mask_repeat Repeat_x; Css.mask_repeat Repeat_x ]

  let mask_repeat_y' =
    style [ Css.webkit_mask_repeat Repeat_y; Css.mask_repeat Repeat_y ]

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
    style [ Css.webkit_mask_clip No_clip; Css.mask_clip No_clip ]

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

  let parse_bracket_len s =
    if String.ends_with ~suffix:"px" s then
      let n = String.sub s 0 (String.length s - 2) |> float_of_string_opt in
      Option.map (fun f -> (Css.Px f : Css.length)) n
    else if String.ends_with ~suffix:"%" s then
      let n = String.sub s 0 (String.length s - 1) |> float_of_string_opt in
      Option.map (fun f -> (Css.Pct f : Css.length)) n
    else if String.ends_with ~suffix:"rem" s then
      let n = String.sub s 0 (String.length s - 3) |> float_of_string_opt in
      Option.map (fun f -> (Css.Rem f : Css.length)) n
    else None

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
        let parse_size_val s : Css.background_size option =
          if String.ends_with ~suffix:"px" s then
            let n =
              String.sub s 0 (String.length s - 2) |> float_of_string_opt
            in
            Option.map (fun f -> (Css.Px f : Css.background_size)) n
          else if String.ends_with ~suffix:"%" s then
            let n =
              String.sub s 0 (String.length s - 1) |> float_of_string_opt
            in
            Option.map (fun f -> (Css.Pct f : Css.background_size)) n
          else if String.ends_with ~suffix:"rem" s then
            let n =
              String.sub s 0 (String.length s - 3) |> float_of_string_opt
            in
            Option.map (fun f -> (Css.Rem f : Css.background_size)) n
          else None
        in
        Option.map
          (fun s -> [ Css.webkit_mask_size s; Css.mask_size s ])
          (parse_size_val v)
    | _ -> None

  let parse_bracket_position inner =
    let parts =
      String.split_on_char '_' inner |> List.filter (fun s -> s <> "")
    in
    let parse_pos_val s : Css.position_value option =
      if String.ends_with ~suffix:"px" s then
        let n = String.sub s 0 (String.length s - 2) |> float_of_string_opt in
        Option.map (fun f -> (Css.Single (Px f) : Css.position_value)) n
      else if String.ends_with ~suffix:"%" s then
        let n = String.sub s 0 (String.length s - 1) |> float_of_string_opt in
        Option.map (fun f -> (Css.Single (Pct f) : Css.position_value)) n
      else None
    in
    match parts with
    | [ x; y ] -> (
        match (parse_bracket_len x, parse_bracket_len y) with
        | Some xv, Some yv ->
            Some
              [
                Css.webkit_mask_position [ Css.XY (xv, yv) ];
                Css.mask_position [ Css.XY (xv, yv) ];
              ]
        | _ -> None)
    | [ v ] ->
        Option.map
          (fun pv ->
            [ Css.webkit_mask_position [ pv ]; Css.mask_position [ pv ] ])
          (parse_pos_val v)
    | _ -> None

  let to_style = function
    | Mask_none -> mask_none
    | Mask_add -> mask_add
    | Mask_exclude -> mask_exclude
    | Mask_intersect -> mask_intersect
    | Mask_subtract -> mask_subtract
    | Mask_alpha -> mask_alpha
    | Mask_luminance -> mask_luminance
    | Mask_match -> mask_match
    | Mask_type_alpha -> mask_type_alpha
    | Mask_type_luminance -> mask_type_luminance
    | Mask_auto -> mask_auto
    | Mask_contain -> mask_contain
    | Mask_cover -> mask_cover
    | Mask_position pos -> mask_position' pos
    | Mask_no_repeat -> mask_no_repeat'
    | Mask_repeat -> mask_repeat'
    | Mask_repeat_round -> mask_repeat_round'
    | Mask_repeat_space -> mask_repeat_space'
    | Mask_repeat_x -> mask_repeat_x'
    | Mask_repeat_y -> mask_repeat_y'
    | Mask_clip_border -> mask_clip_border
    | Mask_clip_padding -> mask_clip_padding
    | Mask_clip_content -> mask_clip_content
    | Mask_clip_fill -> mask_clip_fill
    | Mask_clip_stroke -> mask_clip_stroke
    | Mask_clip_view -> mask_clip_view
    | Mask_no_clip -> mask_no_clip
    | Mask_origin_border -> mask_origin_border
    | Mask_origin_padding -> mask_origin_padding
    | Mask_origin_content -> mask_origin_content
    | Mask_origin_fill -> mask_origin_fill
    | Mask_origin_stroke -> mask_origin_stroke
    | Mask_origin_view -> mask_origin_view
    (* Bracket notation *)
    | Mask_bracket_contain ->
        style [ Css.webkit_mask_size Contain; Css.mask_size Contain ]
    | Mask_bracket_cover ->
        style [ Css.webkit_mask_size Cover; Css.mask_size Cover ]
    | Mask_bracket_size inner -> (
        match parse_bracket_size inner with
        | Some decls -> style decls
        | None -> style [ Css.webkit_mask_size Auto; Css.mask_size Auto ])
    | Mask_bracket_length inner -> (
        match parse_bracket_size inner with
        | Some decls -> style decls
        | None -> style [ Css.webkit_mask_size Auto; Css.mask_size Auto ])
    | Mask_bracket_position inner -> (
        match parse_bracket_position inner with
        | Some decls -> style decls
        | None ->
            style
              [
                Css.webkit_mask_position [ Center ];
                Css.mask_position [ Center ];
              ])
    | Mask_bracket_typed_position inner -> (
        match parse_bracket_position inner with
        | Some decls -> style decls
        | None ->
            style
              [
                Css.webkit_mask_position [ Center ];
                Css.mask_position [ Center ];
              ])
    | Mask_bracket_image_var v ->
        let bare = Parse.extract_var_name v in
        let var_ref : Css.background_image Css.var = Var.bracket bare in
        style
          [
            Css.webkit_mask_image (Var var_ref);
            Css.webkit_mask_image (Var var_ref);
            Css.mask_image (Var var_ref);
          ]
    | Mask_bracket_url url ->
        style [ Css.webkit_mask_image (Url url); Css.mask_image (Url url) ]
    | Mask_bracket_url_var v ->
        let bare = Parse.extract_var_name v in
        let var_ref : Css.background_image Css.var = Var.bracket bare in
        style
          [
            Css.webkit_mask_image (Var var_ref);
            Css.webkit_mask_image (Var var_ref);
            Css.mask_image (Var var_ref);
          ]
    | Mask_bracket_var v ->
        let bare = Parse.extract_var_name v in
        let var_ref : Css.background_image Css.var = Var.bracket bare in
        style
          [
            Css.webkit_mask_image (Var var_ref);
            Css.webkit_mask_image (Var var_ref);
            Css.mask_image (Var var_ref);
          ]
    | Mask_bracket_linear_gradient v ->
        let css_str = String.map (fun c -> if c = '_' then ' ' else c) v in
        let reader = Css.Reader.of_string css_str in
        let img = Css.read_background_image reader in
        let img = Css.minify_background_image img in
        style [ Css.webkit_mask_image img; Css.mask_image img ]
    (* Sub-property bracket notation *)
    | Mask_position_bracket inner -> (
        match parse_bracket_position inner with
        | Some decls -> style decls
        | None ->
            style
              [
                Css.webkit_mask_position [ Center ];
                Css.mask_position [ Center ];
              ])
    | Mask_position_bracket_var v ->
        let bare = Parse.extract_var_name v in
        let var_ref : Css.position_value Css.var = Var.bracket bare in
        style
          [
            Css.webkit_mask_position [ Var var_ref ];
            Css.webkit_mask_position [ Var var_ref ];
            Css.mask_position [ Var var_ref ];
          ]
    | Mask_size_bracket inner -> (
        match parse_bracket_size inner with
        | Some decls -> style decls
        | None -> style [ Css.webkit_mask_size Auto; Css.mask_size Auto ])
    | Mask_size_bracket_var v ->
        let bare = Parse.extract_var_name v in
        let var_ref : Css.background_size Css.var = Var.bracket bare in
        style
          [
            Css.webkit_mask_size (Var var_ref);
            Css.webkit_mask_size (Var var_ref);
            Css.mask_size (Var var_ref);
          ]

  let suborder = function
    | Mask_bracket_image_var _ -> 100
    | Mask_bracket_linear_gradient _ -> 101
    | Mask_bracket_url _ -> 102
    | Mask_bracket_url_var _ -> 103
    | Mask_bracket_var _ -> 104
    | Mask_none -> 105
    | Mask_add -> 200
    | Mask_exclude -> 201
    | Mask_intersect -> 202
    | Mask_subtract -> 203
    | Mask_alpha -> 300
    | Mask_luminance -> 301
    | Mask_match -> 302
    | Mask_type_alpha -> 303
    | Mask_type_luminance -> 304
    | Mask_bracket_contain -> 400
    | Mask_bracket_cover -> 401
    | Mask_bracket_length _ -> 402
    | Mask_bracket_size _ -> 403
    | Mask_auto -> 404
    | Mask_contain -> 405
    | Mask_cover -> 406
    | Mask_bracket_position _ -> 500
    | Mask_bracket_typed_position _ -> 501
    | Mask_position Pos_bottom -> 502
    | Mask_position Pos_bottom_left -> 503
    | Mask_position Pos_bottom_right -> 504
    | Mask_position Pos_center -> 505
    | Mask_position Pos_left -> 506
    | Mask_position Pos_right -> 507
    | Mask_position Pos_top -> 508
    | Mask_position Pos_top_left -> 509
    | Mask_position Pos_top_right -> 510
    | Mask_no_repeat -> 600
    | Mask_repeat -> 601
    | Mask_repeat_round -> 602
    | Mask_repeat_space -> 603
    | Mask_repeat_x -> 604
    | Mask_repeat_y -> 605
    | Mask_clip_border -> 700
    | Mask_clip_content -> 701
    | Mask_clip_fill -> 702
    | Mask_clip_padding -> 703
    | Mask_clip_stroke -> 704
    | Mask_clip_view -> 705
    | Mask_no_clip -> 706
    | Mask_origin_border -> 800
    | Mask_origin_content -> 801
    | Mask_origin_fill -> 802
    | Mask_origin_padding -> 803
    | Mask_origin_stroke -> 804
    | Mask_origin_view -> 805
    | Mask_position_bracket _ -> 511
    | Mask_position_bracket_var _ -> 512
    | Mask_size_bracket _ -> 407
    | Mask_size_bracket_var _ -> 408

  let of_class class_name =
    let parts = Parse.split_class class_name in
    match parts with
    | [ "mask"; "none" ] -> Ok Mask_none
    | [ "mask"; "add" ] -> Ok Mask_add
    | [ "mask"; "exclude" ] -> Ok Mask_exclude
    | [ "mask"; "intersect" ] -> Ok Mask_intersect
    | [ "mask"; "subtract" ] -> Ok Mask_subtract
    | [ "mask"; "alpha" ] -> Ok Mask_alpha
    | [ "mask"; "luminance" ] -> Ok Mask_luminance
    | [ "mask"; "match" ] -> Ok Mask_match
    | [ "mask"; "type"; "alpha" ] -> Ok Mask_type_alpha
    | [ "mask"; "type"; "luminance" ] -> Ok Mask_type_luminance
    | [ "mask"; "auto" ] -> Ok Mask_auto
    | [ "mask"; "contain" ] -> Ok Mask_contain
    | [ "mask"; "cover" ] -> Ok Mask_cover
    (* mask-position *)
    | [ "mask"; "bottom" ] -> Ok (Mask_position Pos_bottom)
    | [ "mask"; "bottom"; "left" ] -> Ok (Mask_position Pos_bottom_left)
    | [ "mask"; "bottom"; "right" ] -> Ok (Mask_position Pos_bottom_right)
    | [ "mask"; "center" ] -> Ok (Mask_position Pos_center)
    | [ "mask"; "left" ] -> Ok (Mask_position Pos_left)
    | [ "mask"; "right" ] -> Ok (Mask_position Pos_right)
    | [ "mask"; "top" ] -> Ok (Mask_position Pos_top)
    | [ "mask"; "top"; "left" ] -> Ok (Mask_position Pos_top_left)
    | [ "mask"; "top"; "right" ] -> Ok (Mask_position Pos_top_right)
    (* mask-repeat *)
    | [ "mask"; "no"; "repeat" ] -> Ok Mask_no_repeat
    | [ "mask"; "repeat" ] -> Ok Mask_repeat
    | [ "mask"; "repeat"; "round" ] -> Ok Mask_repeat_round
    | [ "mask"; "repeat"; "space" ] -> Ok Mask_repeat_space
    | [ "mask"; "repeat"; "x" ] -> Ok Mask_repeat_x
    | [ "mask"; "repeat"; "y" ] -> Ok Mask_repeat_y
    (* mask-clip *)
    | [ "mask"; "clip"; "border" ] -> Ok Mask_clip_border
    | [ "mask"; "clip"; "padding" ] -> Ok Mask_clip_padding
    | [ "mask"; "clip"; "content" ] -> Ok Mask_clip_content
    | [ "mask"; "clip"; "fill" ] -> Ok Mask_clip_fill
    | [ "mask"; "clip"; "stroke" ] -> Ok Mask_clip_stroke
    | [ "mask"; "clip"; "view" ] -> Ok Mask_clip_view
    | [ "mask"; "no"; "clip" ] -> Ok Mask_no_clip
    (* mask-origin *)
    | [ "mask"; "origin"; "border" ] -> Ok Mask_origin_border
    | [ "mask"; "origin"; "padding" ] -> Ok Mask_origin_padding
    | [ "mask"; "origin"; "content" ] -> Ok Mask_origin_content
    | [ "mask"; "origin"; "fill" ] -> Ok Mask_origin_fill
    | [ "mask"; "origin"; "stroke" ] -> Ok Mask_origin_stroke
    | [ "mask"; "origin"; "view" ] -> Ok Mask_origin_view
    (* Sub-property bracket notation: mask-position-[...], mask-size-[...] *)
    | [ "mask"; "position"; bracket ] when Parse.is_bracket_value bracket ->
        let inner = Parse.bracket_inner bracket in
        if Parse.is_var inner then Ok (Mask_position_bracket_var inner)
        else Ok (Mask_position_bracket inner)
    | [ "mask"; "size"; bracket ] when Parse.is_bracket_value bracket ->
        let inner = Parse.bracket_inner bracket in
        if Parse.is_var inner then Ok (Mask_size_bracket_var inner)
        else Ok (Mask_size_bracket inner)
    (* Bracket notation: mask-[...] *)
    | [ "mask"; bracket ] when Parse.is_bracket_value bracket -> (
        let inner = Parse.bracket_inner bracket in
        match inner with
        | "contain" -> Ok Mask_bracket_contain
        | "cover" -> Ok Mask_bracket_cover
        | _ when String.length inner > 7 && String.sub inner 0 7 = "length:" ->
            Ok
              (Mask_bracket_length
                 (String.sub inner 7 (String.length inner - 7)))
        | _ when String.length inner > 5 && String.sub inner 0 5 = "size:" ->
            Ok
              (Mask_bracket_size (String.sub inner 5 (String.length inner - 5)))
        | _ when String.length inner > 9 && String.sub inner 0 9 = "position:"
          ->
            Ok
              (Mask_bracket_typed_position
                 (String.sub inner 9 (String.length inner - 9)))
        | _ when String.length inner > 6 && String.sub inner 0 6 = "image:" ->
            Ok
              (Mask_bracket_image_var
                 (String.sub inner 6 (String.length inner - 6)))
        | _ when String.length inner > 4 && String.sub inner 0 4 = "url:" ->
            Ok
              (Mask_bracket_url_var
                 (String.sub inner 4 (String.length inner - 4)))
        | _ when String.length inner > 4 && String.sub inner 0 4 = "url(" ->
            let url_content = String.sub inner 4 (String.length inner - 5) in
            Ok (Mask_bracket_url url_content)
        | _
          when String.length inner > 16
               && String.sub inner 0 16 = "linear-gradient(" ->
            Ok (Mask_bracket_linear_gradient inner)
        | _ when Parse.is_var inner -> Ok (Mask_bracket_var inner)
        | _ ->
            if parse_bracket_position inner <> None then
              Ok (Mask_bracket_position inner)
            else Error (`Msg ("Unknown mask bracket value: " ^ inner)))
    | _ -> Error (`Msg "Not a mask utility")

  let to_class = function
    | Mask_none -> "mask-none"
    | Mask_add -> "mask-add"
    | Mask_exclude -> "mask-exclude"
    | Mask_intersect -> "mask-intersect"
    | Mask_subtract -> "mask-subtract"
    | Mask_alpha -> "mask-alpha"
    | Mask_luminance -> "mask-luminance"
    | Mask_match -> "mask-match"
    | Mask_type_alpha -> "mask-type-alpha"
    | Mask_type_luminance -> "mask-type-luminance"
    | Mask_auto -> "mask-auto"
    | Mask_contain -> "mask-contain"
    | Mask_cover -> "mask-cover"
    | Mask_position Pos_bottom -> "mask-bottom"
    | Mask_position Pos_bottom_left -> "mask-bottom-left"
    | Mask_position Pos_bottom_right -> "mask-bottom-right"
    | Mask_position Pos_center -> "mask-center"
    | Mask_position Pos_left -> "mask-left"
    | Mask_position Pos_right -> "mask-right"
    | Mask_position Pos_top -> "mask-top"
    | Mask_position Pos_top_left -> "mask-top-left"
    | Mask_position Pos_top_right -> "mask-top-right"
    | Mask_no_repeat -> "mask-no-repeat"
    | Mask_repeat -> "mask-repeat"
    | Mask_repeat_round -> "mask-repeat-round"
    | Mask_repeat_space -> "mask-repeat-space"
    | Mask_repeat_x -> "mask-repeat-x"
    | Mask_repeat_y -> "mask-repeat-y"
    | Mask_clip_border -> "mask-clip-border"
    | Mask_clip_padding -> "mask-clip-padding"
    | Mask_clip_content -> "mask-clip-content"
    | Mask_clip_fill -> "mask-clip-fill"
    | Mask_clip_stroke -> "mask-clip-stroke"
    | Mask_clip_view -> "mask-clip-view"
    | Mask_no_clip -> "mask-no-clip"
    | Mask_origin_border -> "mask-origin-border"
    | Mask_origin_padding -> "mask-origin-padding"
    | Mask_origin_content -> "mask-origin-content"
    | Mask_origin_fill -> "mask-origin-fill"
    | Mask_origin_stroke -> "mask-origin-stroke"
    | Mask_origin_view -> "mask-origin-view"
    | Mask_bracket_contain -> "mask-[contain]"
    | Mask_bracket_cover -> "mask-[cover]"
    | Mask_bracket_size v -> "mask-[size:" ^ v ^ "]"
    | Mask_bracket_length v -> "mask-[length:" ^ v ^ "]"
    | Mask_bracket_position v -> "mask-[" ^ v ^ "]"
    | Mask_bracket_typed_position v -> "mask-[position:" ^ v ^ "]"
    | Mask_bracket_image_var v -> "mask-[image:" ^ v ^ "]"
    | Mask_bracket_url url -> "mask-[url(" ^ url ^ ")]"
    | Mask_bracket_url_var v -> "mask-[url:" ^ v ^ "]"
    | Mask_bracket_var v -> "mask-[" ^ v ^ "]"
    | Mask_bracket_linear_gradient v -> "mask-[" ^ v ^ "]"
    | Mask_position_bracket v -> "mask-position-[" ^ v ^ "]"
    | Mask_position_bracket_var v -> "mask-position-[" ^ v ^ "]"
    | Mask_size_bracket v -> "mask-size-[" ^ v ^ "]"
    | Mask_size_bracket_var v -> "mask-size-[" ^ v ^ "]"
end

open Handler

let () = Utility.register (module Handler)
let utility x = Utility.base (Self x)
let mask_none = utility Mask_none
let mask_add = utility Mask_add
let mask_exclude = utility Mask_exclude
let mask_intersect = utility Mask_intersect
let mask_subtract = utility Mask_subtract
let mask_alpha = utility Mask_alpha
let mask_luminance = utility Mask_luminance
let mask_match = utility Mask_match
let mask_type_alpha = utility Mask_type_alpha
let mask_type_luminance = utility Mask_type_luminance
let mask_auto = utility Mask_auto
let mask_contain = utility Mask_contain
let mask_cover = utility Mask_cover
let mask_bottom = utility (Mask_position Pos_bottom)
let mask_bottom_left = utility (Mask_position Pos_bottom_left)
let mask_bottom_right = utility (Mask_position Pos_bottom_right)
let mask_center = utility (Mask_position Pos_center)
let mask_left = utility (Mask_position Pos_left)
let mask_right = utility (Mask_position Pos_right)
let mask_top = utility (Mask_position Pos_top)
let mask_top_left = utility (Mask_position Pos_top_left)
let mask_top_right = utility (Mask_position Pos_top_right)
let mask_no_repeat = utility Mask_no_repeat
let mask_repeat = utility Mask_repeat
let mask_repeat_round = utility Mask_repeat_round
let mask_repeat_space = utility Mask_repeat_space
let mask_repeat_x = utility Mask_repeat_x
let mask_repeat_y = utility Mask_repeat_y
let mask_clip_border = utility Mask_clip_border
let mask_clip_padding = utility Mask_clip_padding
let mask_clip_content = utility Mask_clip_content
let mask_clip_fill = utility Mask_clip_fill
let mask_clip_stroke = utility Mask_clip_stroke
let mask_clip_view = utility Mask_clip_view
let mask_no_clip = utility Mask_no_clip
let mask_origin_border = utility Mask_origin_border
let mask_origin_padding = utility Mask_origin_padding
let mask_origin_content = utility Mask_origin_content
let mask_origin_fill = utility Mask_origin_fill
let mask_origin_stroke = utility Mask_origin_stroke
let mask_origin_view = utility Mask_origin_view
