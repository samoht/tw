(** Background utilities

    What's included:
    - `bg-<color>` - Background colour, with an opacity modifier, keywords and
      bracket values.
    - `bg-none`, `bg-[url(...)]`, `bg-[image:...]` - Background images other
      than the named gradients.
    - `bg-fixed`, `bg-clip-*`, `bg-origin-*`, `bg-<position>`, `bg-repeat-*`,
      `bg-auto`/`bg-cover`/`bg-contain` - Attachment, clip, origin, position,
      repeat and size, with their bracket forms.

    The named gradients and their colour stops are in [Gradients]. *)

module Css = Cascade.Css

module Handler = struct
  (* The named background positions, as [bg-<position>] spells them. *)
  module Position = struct
    type t =
      | Bottom
      | Bottom_left
      | Bottom_right
      | Center
      | Left
      | Left_bottom
      | Left_top
      | Right
      | Right_bottom
      | Right_top
      | Top
      | Top_left
      | Top_right
  end

  type t =
    | Bg of Color.color * int
    | Bg_origin_border
    | Bg_origin_padding
    | Bg_origin_content
    | Bg_clip_border
    | Bg_clip_padding
    | Bg_clip_content
    | Bg_clip_text
    (* Background color keywords *)
    | Bg_inherit
    (* Background image *)
    | Bg_none
    (* Background size *)
    | Bg_auto
    | Bg_cover
    | Bg_contain
    (* Background attachment *)
    | Bg_fixed
    | Bg_local
    | Bg_scroll
    (* Background repeat *)
    | Bg_repeat
    | Bg_no_repeat
    | Bg_repeat_x
    | Bg_repeat_y
    | Bg_repeat_round
    | Bg_repeat_space
    (* Background position *)
    | Bg_position of Position.t
    (* Bracket notation: bg-[contain], bg-[cover] → background-size *)
    | Bg_bracket_contain
    | Bg_bracket_cover
    (* Bracket notation: bg-[length:...] or bg-[size:...] → background-size *)
    | Bg_bracket_size of string
    (* Bracket notation: bg-[50%], bg-[120px], bg-[120px_120px] →
       background-position *)
    | Bg_bracket_position of string * Css.position_value
    (* Bracket notation: bg-[position:...] and bg-[percentage:...] →
       background-position; the whole inner text is kept, hint included *)
    | Bg_bracket_typed_position of string * Css.position_value
    (* Bracket notation: bg-[color:var(--x)] → background-color *)
    | Bg_bracket_color_var of string
    (* Bracket notation: bg-[var(--x)] → background-color *)
    | Bg_bracket_var of string
    (* Bracket notation: bg-[image:var(--x)] → background-image *)
    | Bg_bracket_image_var of string
    (* Bracket notation: bg-[image:<gradient/literal>] → background-image, with
       the image: hint kept in the class name *)
    | Bg_bracket_image of string * Css.background_image
    (* Bracket notation: bg-[url(...)] → background-image, holding the whole
       url() as written so the token reader, not tw, resolves its quotes and
       escapes *)
    | Bg_bracket_url of string
    (* Bracket notation: bg-[image:url(...)] → background-image (image: hint
       forces background-image; selector keeps the image: form) *)
    | Bg_bracket_image_url of string
    (* Bracket notation: bg-[url:var(--x)] → background-image *)
    | Bg_bracket_url_var of string
    (* Bracket notation: bg-[linear-gradient(...)] → background-image *)
    | Bg_bracket_linear_gradient of string * Css.background_image
    (* Bracket color/var with opacity *)
    | Bg_bracket_color_var_opacity of string * Color.opacity_modifier
    | Bg_bracket_var_opacity of string * Color.opacity_modifier
    (* Bracket color: bg-[#0088cc], bg-[rgba(48,163,0,0.14)], etc. Stores
       original string for class name roundtrip and typed Css.color *)
    | Bg_bracket_color of string * Css.color
    (* Bracket color with opacity: bg-[#0088cc]/50, bg-[rgba(...)]/50, etc. *)
    | Bg_bracket_color_opacity of string * Css.color * Color.opacity_modifier
    (* bg-current *)
    | Bg_current
    (* bg-current with opacity: bg-current/50, bg-current/[0.5] *)
    | Bg_current_opacity of Color.opacity_modifier
    (* bg-transparent *)
    | Bg_transparent
    (* Named color with opacity: bg-red-500/50, bg-blue-500/[0.5] *)
    | Bg_opacity of Color.color * int * Color.opacity_modifier
    (* bg-[length:...] - explicit length prefix for bracket size *)
    | Bg_bracket_length of string
    | (* A bracket no reader took. The hint chooses the longhand and the value
         is forwarded verbatim, which is Tailwind's token-stream contract; with
         no hint, or one this family does not know, the colour is the last
         resort. The browser discards the declaration; the selector is what the
         class was for. *)
      Bg_bracket_raw_color of string * string
    | Bg_bracket_raw_image of string * string
    | Bg_bracket_raw_position of string * string
    (* bg-position-[...] bracket notation *)
    | Bg_position_bracket of string * Css.position_value
    | Bg_position_raw of string * string
    (* bg-position-[foo]: one longhand, so the value is forwarded verbatim. *)
    (* bg-size-[...] bracket notation *)
    | Bg_size_bracket of string

  let to_class (t : t) =
    match t with
    | Bg (color, shade) ->
        if Color.is_shadeless color then "bg-" ^ Color.pp color
        else "bg-" ^ Color.pp color ^ "-" ^ string_of_int shade
    | Bg_origin_border -> "bg-origin-border"
    | Bg_origin_padding -> "bg-origin-padding"
    | Bg_origin_content -> "bg-origin-content"
    | Bg_clip_border -> "bg-clip-border"
    | Bg_clip_padding -> "bg-clip-padding"
    | Bg_clip_content -> "bg-clip-content"
    | Bg_clip_text -> "bg-clip-text"
    | Bg_inherit -> "bg-inherit"
    | Bg_none -> "bg-none"
    | Bg_auto -> "bg-auto"
    | Bg_cover -> "bg-cover"
    | Bg_contain -> "bg-contain"
    | Bg_fixed -> "bg-fixed"
    | Bg_local -> "bg-local"
    | Bg_scroll -> "bg-scroll"
    | Bg_repeat -> "bg-repeat"
    | Bg_no_repeat -> "bg-no-repeat"
    | Bg_repeat_x -> "bg-repeat-x"
    | Bg_repeat_y -> "bg-repeat-y"
    | Bg_repeat_round -> "bg-repeat-round"
    | Bg_repeat_space -> "bg-repeat-space"
    | Bg_position Position.Bottom -> "bg-bottom"
    | Bg_position Position.Bottom_left -> "bg-bottom-left"
    | Bg_position Position.Bottom_right -> "bg-bottom-right"
    | Bg_position Position.Center -> "bg-center"
    | Bg_position Position.Left -> "bg-left"
    | Bg_position Position.Left_bottom -> "bg-left-bottom"
    | Bg_position Position.Left_top -> "bg-left-top"
    | Bg_position Position.Right -> "bg-right"
    | Bg_position Position.Right_bottom -> "bg-right-bottom"
    | Bg_position Position.Right_top -> "bg-right-top"
    | Bg_position Position.Top -> "bg-top"
    | Bg_position Position.Top_left -> "bg-top-left"
    | Bg_position Position.Top_right -> "bg-top-right"
    | Bg_bracket_contain -> "bg-[contain]"
    | Bg_bracket_cover -> "bg-[cover]"
    | Bg_bracket_size v -> "bg-[size:" ^ v ^ "]"
    | Bg_bracket_length v -> "bg-[length:" ^ v ^ "]"
    | Bg_bracket_raw_color (v, _)
    | Bg_bracket_raw_image (v, _)
    | Bg_bracket_raw_position (v, _) ->
        "bg-[" ^ v ^ "]"
    | Bg_bracket_position (v, _) -> "bg-[" ^ v ^ "]"
    | Bg_bracket_typed_position (inner, _) -> "bg-[" ^ inner ^ "]"
    | Bg_bracket_color_var v -> "bg-[color:" ^ v ^ "]"
    | Bg_bracket_var v -> "bg-[" ^ v ^ "]"
    | Bg_bracket_image_var v -> "bg-[image:" ^ v ^ "]"
    | Bg_bracket_image (v, _) -> "bg-[image:" ^ v ^ "]"
    | Bg_bracket_url v -> "bg-[" ^ v ^ "]"
    | Bg_bracket_image_url v -> "bg-[image:" ^ v ^ "]"
    | Bg_bracket_url_var v -> "bg-[url:" ^ v ^ "]"
    | Bg_bracket_linear_gradient (v, _) -> "bg-[" ^ v ^ "]"
    | Bg_bracket_color_var_opacity (v, opacity) ->
        "bg-[color:" ^ v ^ "]" ^ Color.opacity_suffix opacity
    | Bg_bracket_var_opacity (v, opacity) ->
        "bg-[" ^ v ^ "]" ^ Color.opacity_suffix opacity
    | Bg_bracket_color (orig, _) -> "bg-[" ^ orig ^ "]"
    | Bg_bracket_color_opacity (orig, _, opacity) ->
        "bg-[" ^ orig ^ "]" ^ Color.opacity_suffix opacity
    | Bg_current -> "bg-current"
    | Bg_current_opacity opacity -> "bg-current" ^ Color.opacity_suffix opacity
    | Bg_transparent -> "bg-transparent"
    | Bg_opacity (color, shade, opacity) ->
        let base =
          if Color.is_shadeless color then "bg-" ^ Color.pp color
          else "bg-" ^ Color.pp color ^ "-" ^ string_of_int shade
        in
        base ^ Color.opacity_suffix opacity
    | Bg_position_bracket (v, _) | Bg_position_raw (v, _) ->
        "bg-position-[" ^ v ^ "]"
    | Bg_size_bracket v -> "bg-size-[" ^ v ^ "]"

  open Style
  open Css

  let name = "backgrounds"

  (* Tailwind sorts a layer by the rank its property table gives the property
     each rule writes. The background utilities take two stretches of that
     table: background-color (198) and background-image (199), which the
     gradient variables (202-208) follow, then background-size (251) through
     background-origin (256). The second stretch falls between the mask-image
     utilities and mask-composite, so it sorts with the masks. *)
  let property_rank = function
    | Bg _ | Bg_opacity _ | Bg_inherit | Bg_current | Bg_current_opacity _
    | Bg_transparent | Bg_bracket_color _ | Bg_bracket_color_opacity _
    | Bg_bracket_color_var _ | Bg_bracket_color_var_opacity _ | Bg_bracket_var _
    | Bg_bracket_var_opacity _ | Bg_bracket_raw_color _ ->
        198
    | Bg_bracket_image_var _ | Bg_bracket_image _ | Bg_bracket_linear_gradient _
    | Bg_bracket_url _ | Bg_bracket_image_url _ | Bg_bracket_url_var _
    | Bg_bracket_raw_image _ | Bg_none ->
        199
    | Bg_auto | Bg_cover | Bg_contain | Bg_bracket_contain | Bg_bracket_cover
    | Bg_bracket_size _ | Bg_bracket_length _ | Bg_size_bracket _ ->
        251
    | Bg_fixed | Bg_local | Bg_scroll -> 252
    | Bg_clip_border | Bg_clip_padding | Bg_clip_content | Bg_clip_text -> 253
    | Bg_position _ | Bg_bracket_position _ | Bg_bracket_typed_position _
    | Bg_position_bracket _ | Bg_position_raw _ | Bg_bracket_raw_position _ ->
        254
    | Bg_repeat | Bg_no_repeat | Bg_repeat_x | Bg_repeat_y | Bg_repeat_round
    | Bg_repeat_space ->
        255
    | Bg_origin_border | Bg_origin_padding | Bg_origin_content -> 256

  (* The masks open their band at mask-image; every rank past it belongs to
     them, and the background properties in that stretch interleave. *)
  let mask_image_rank = 209
  let priority t = if property_rank t > mask_image_rank then 21 else 20

  (* Rules sharing a slot sort by declaration count, most first, then by class
     name. A background utility writes nothing past its own property, so each
     closes its slot: in background-image's, after the gradients that carry
     more. *)
  let suborder t = Utility.Property_order.last (property_rank t)

  (* A palette colour's value with the theme token it reads, or the value alone
     for an arbitrary colour, which has no token. *)
  let bg' ?theme ?(shade = 500) color =
    let bg_var_name =
      let base = Color.pp color in
      if Color.is_base_color color then "background-color-" ^ base
      else "background-color-" ^ base ^ "-" ^ string_of_int shade
    in
    match Scheme.theme_value theme bg_var_name with
    | Some theme_val ->
        (* Property-scoped bg color: --background-color-<name> *)
        let tv = Var.theme Css.Color bg_var_name ~order:(5, 50) in
        let d, r = Var.binding tv (Css.hex theme_val) in
        style [ d; Css.background_color (Var r) ]
    | None ->
        let theme_decls, color_value = Color.bound_color ?theme ~shade color in
        style (theme_decls @ [ Css.background_color color_value ])

  let bg_origin_border = style [ Css.background_origin Border_box ]
  let bg_origin_padding = style [ Css.background_origin Padding_box ]
  let bg_origin_content = style [ Css.background_origin Content_box ]
  let bg_clip_border = style [ Css.background_clip Border_box ]
  let bg_clip_padding = style [ Css.background_clip Padding_box ]
  let bg_clip_content = style [ Css.background_clip Content_box ]

  let bg_clip_text =
    style [ Css.webkit_background_clip Text; Css.background_clip Text ]

  let bg_inherit' = style [ Css.background_color Inherit ]
  let bg_none' = style [ Css.background_image None ]
  let bg_auto' = style [ Css.background_size Auto ]
  let bg_cover' = style [ Css.background_size Cover ]
  let bg_contain' = style [ Css.background_size Contain ]
  let bg_fixed' = style [ Css.background_attachment Fixed ]
  let bg_local' = style [ Css.background_attachment Local ]
  let bg_scroll' = style [ Css.background_attachment Scroll ]
  let bg_repeat' = style [ Css.background_repeat Repeat ]
  let bg_no_repeat' = style [ Css.background_repeat No_repeat ]
  let bg_repeat_x' = style [ Css.background_repeat Repeat_x ]
  let bg_repeat_y' = style [ Css.background_repeat Repeat_y ]
  let bg_repeat_round' = style [ Css.background_repeat Round ]
  let bg_repeat_space' = style [ Css.background_repeat Space ]
  let bg_position' pos = style [ Css.background_position pos ]

  (* Parse a bracket size value like "120px_120px" or "120px" *)
  (* [bg-size-[8rem]] and [bg-[length:120px_120px]] take any CSS length, so read
     them with the value parser rather than a hand-rolled unit table. *)
  let parse_bracket_size inner =
    let parts =
      String.split_on_char '_' inner |> List.filter (fun s -> s <> "")
    in
    match parts with
    | [ w; h ] -> (
        match (Css.parse_length w, Css.parse_length h) with
        | Some w, Some h -> Some (Css.background_size (Size (w, h)))
        | _ -> None)
    | [ v ] -> (
        (* Keywords are valid background-size values even under a [length:...]
           hint (Tailwind emits e.g. bg-[length:cover] as
           background-size:cover). *)
        match v with
        | "cover" -> Some (Css.background_size Cover)
        | "contain" -> Some (Css.background_size Contain)
        | "auto" -> Some (Css.background_size Auto)
        | _ ->
            Option.map
              (fun l -> Css.background_size (Length l))
              (Css.parse_length v))
    | _ -> None

  (* A bracket background-position: [120px_120px], [top], [left_10px_top_20px].
     The whole [<position>] grammar, so the hand-rolled reading that only knew
     lengths and two-keyword pairs no longer drops [top] on the floor. [None]
     means the bracket is not a position, which [of_class] rejects. *)
  let parse_bracket_position inner : Css.position_value option =
    let cursor = Cascade.Cursor.of_string (Parse.decode_underscores inner) in
    match
      Cascade.Cursor.try_parse_full_err Css.Properties.read_position_value
        cursor
    with
    | Ok pos -> Some pos
    | Error _ -> None

  (* A data-type hint chooses which longhand a bracket lands in and says nothing
     about the value. [bg-position-] and [bg-size-] each write one longhand, so
     every hint lands there and the readers are handed what follows it. The
     constructors keep the bracket whole, because the class name is what the
     markup carries, so the peel has to happen again wherever one re-reads it.
     [None] is a bracket whose hint is empty, which names no utility. *)
  let bracket_value_after_hint = Parse.value_after_hint

  (* The bracket forms that also accept a var() reference read it as one. *)
  let bracket_position_value inner : Css.position_value option =
    let read value : Css.position_value option =
      if Parse.is_var value then
        let ref : Css.position_value Css.var =
          Var.bracket (Parse.extract_var_name value)
        in
        Some (Css.Var ref)
      else parse_bracket_position value
    in
    Option.bind (bracket_value_after_hint inner) read

  (* A bracket background-image: a gradient, a url(), or a comma-separated layer
     list. [None] means the bracket is not an image, which [of_class] rejects:
     [bg-[image:nope]] used to parse and then emit an empty rule. *)
  let image_of_text css_str : Css.background_image option =
    match Css.parse_background_image css_str with
    | Some [ img ] -> Some (Css.minify_background_image img)
    | Some (_ :: _ as imgs) ->
        Some (Css.List (List.map Css.minify_background_image imgs))
    | Some [] | None -> None

  let parse_bracket_image v = image_of_text (Parse.decode_arbitrary_value v)

  (* A bracket image, with the pair Tailwind's polyfill writes when a stop reads
     a custom property or [currentcolor] through a [color-mix()]: what a browser
     without [color-mix()] reads in the open, the image as written behind the
     guard. [v] is the bracket's text after any hint. *)
  let bracket_image_style ~theme v img =
    let decl i = Css.background_image i in
    match
      Option.bind
        (Parse.color_mix_fallback ~resolve:(Color.theme_token theme)
           (Parse.decode_arbitrary_value v))
        image_of_text
    with
    | Stdlib.Option.None -> style [ decl img ]
    | Stdlib.Option.Some in_the_open ->
        style
          ~rules:(Some [ Color.color_mix_supports [ decl img ] ])
          [ decl in_the_open ]

  (** Bracket color var with opacity: bg-[color:var(--x)]/50 *)
  let bg_bracket_color_var_opacity' var_str opacity =
    let bare = Parse.extract_var_name var_str in
    let var_ref : Css.color Css.var = Var.bracket bare in
    let fallback_decl = Css.background_color (Var var_ref) in
    let oklab_color = Color.mix_alpha opacity (Css.Var var_ref) in
    let oklab_decl = Css.background_color oklab_color in
    let supports_rule = Color.color_mix_supports [ oklab_decl ] in
    style ~rules:(Some [ supports_rule ]) [ fallback_decl ]

  (* A [color:] hint says how to read the value written after it; only a var()
     reference there names a custom property, so any other spelling has to be a
     colour the reader takes. *)
  (* [position:] and [percentage:] both name the background-position longhand;
     Tailwind knows the two spellings for the one hint. *)
  let has_bracket_position_hint inner =
    match Parse.data_type_hint inner with
    | Some (("position" | "percentage"), _) -> true
    | Some _ | None -> false

  let to_style theme =
    let bg_with_opacity c shade opacity =
      Color.bg_with_opacity ~theme c shade opacity
    in
    let bg' ?(shade = 500) color = bg' ~theme ~shade color in
    function
    | Bg (color, shade) -> bg' ~shade color
    | Bg_origin_border -> bg_origin_border
    | Bg_origin_padding -> bg_origin_padding
    | Bg_origin_content -> bg_origin_content
    | Bg_clip_border -> bg_clip_border
    | Bg_clip_padding -> bg_clip_padding
    | Bg_clip_content -> bg_clip_content
    | Bg_clip_text -> bg_clip_text
    | Bg_inherit -> bg_inherit'
    | Bg_none -> bg_none'
    | Bg_auto -> bg_auto'
    | Bg_cover -> bg_cover'
    | Bg_contain -> bg_contain'
    | Bg_fixed -> bg_fixed'
    | Bg_local -> bg_local'
    | Bg_scroll -> bg_scroll'
    | Bg_repeat -> bg_repeat'
    | Bg_no_repeat -> bg_no_repeat'
    | Bg_repeat_x -> bg_repeat_x'
    | Bg_repeat_y -> bg_repeat_y'
    | Bg_repeat_round -> bg_repeat_round'
    | Bg_repeat_space -> bg_repeat_space'
    | Bg_position pos ->
        let pos_val : Css.position_value list =
          match pos with
          | Position.Bottom -> [ Center_bottom ]
          | Position.Bottom_left -> [ XY (Px 0., Pct 100.) ]
          | Position.Bottom_right -> [ XY (Pct 100., Pct 100.) ]
          | Position.Center -> [ Center ]
          | Position.Left -> [ Single (Px 0.) ]
          | Position.Left_bottom -> [ XY (Px 0., Pct 100.) ]
          | Position.Left_top -> [ XY (Px 0., Px 0.) ]
          | Position.Right -> [ Single (Pct 100.) ]
          | Position.Right_bottom -> [ XY (Pct 100., Pct 100.) ]
          | Position.Right_top -> [ XY (Pct 100., Px 0.) ]
          | Position.Top -> [ Center_top ]
          | Position.Top_left -> [ XY (Px 0., Px 0.) ]
          | Position.Top_right -> [ XY (Pct 100., Px 0.) ]
        in
        bg_position' pos_val
    | Bg_bracket_contain -> style [ Css.background_size Contain ]
    | Bg_bracket_cover -> style [ Css.background_size Cover ]
    | Bg_bracket_size inner -> (
        (* A value the size grammar declines is forwarded verbatim, not replaced
           by [auto]: the hint chose the longhand, and substituting a value the
           class never asked for paints something rather than nothing. *)
        match parse_bracket_size inner with
        | Some decl -> style [ decl ]
        | None ->
            style
              (Option.to_list
                 (Parse.opaque_declaration "background-size" inner)))
    | Bg_bracket_position (_, pos) -> style [ Css.background_position [ pos ] ]
    | Bg_bracket_typed_position (_, pos) ->
        style [ Css.background_position [ pos ] ]
    | Bg_bracket_color_var v ->
        let bare = Parse.extract_var_name v in
        let var_ref : Css.color Css.var = Var.bracket bare in
        style [ Css.background_color (Var var_ref) ]
    | Bg_bracket_var v ->
        let bare = Parse.extract_var_name v in
        let var_ref : Css.color Css.var = Var.bracket bare in
        style [ Css.background_color (Var var_ref) ]
    | Bg_bracket_image_var v ->
        let bare = Parse.extract_var_name v in
        let var_ref : Css.background_image Css.var = Var.bracket bare in
        style [ Css.background_image (Var var_ref) ]
    | Bg_bracket_image (v, img) -> bracket_image_style ~theme v img
    (* The whole [url()] is kept, so cascade reads the token rather than tw
       slicing the file name out of it: the quotes are the tokeniser's, and an
       escape in it stands for one character of the URL. [of_class] refuses a
       value the token reader will not take, so [None] names nothing. *)
    | Bg_bracket_url v | Bg_bracket_image_url v -> (
        match Parse.url_token (Parse.decode_arbitrary_value v) with
        | Some url -> style [ Css.background_image (Url url) ]
        | None -> style [])
    | Bg_bracket_url_var v when not (Parse.is_var v) -> (
        match Parse.url_token (Parse.decode_arbitrary_value v) with
        | Some url -> style [ Css.background_image (Url url) ]
        | None -> (
            match parse_bracket_image v with
            | Some img -> style [ Css.background_image img ]
            | None -> style []))
    | Bg_bracket_url_var v ->
        let bare = Parse.extract_var_name v in
        let var_ref : Css.background_image Css.var = Var.bracket bare in
        style [ Css.background_image (Var var_ref) ]
    | Bg_bracket_linear_gradient (v, img) -> bracket_image_style ~theme v img
    | Bg_bracket_color_var_opacity (v, opacity) ->
        bg_bracket_color_var_opacity' v opacity
    | Bg_bracket_var_opacity (v, opacity) ->
        bg_bracket_color_var_opacity' v opacity
    | Bg_bracket_color (_, css_color) ->
        Color.bracket_color_style ~theme ~property:Css.background_color
          css_color
    | Bg_bracket_color_opacity (_, css_color, opacity) ->
        Color.bracket_color_opacity_style ~theme ~property:Css.background_color
          css_color opacity
    | Bg_current -> style [ Css.background_color Css.Current ]
    | Bg_current_opacity opacity -> Color.bg_current_with_opacity opacity
    | Bg_transparent -> style [ Css.background_color (Css.hex "#0000") ]
    | Bg_opacity (color, shade, opacity) -> bg_with_opacity color shade opacity
    | Bg_bracket_raw_color (_, value) ->
        style
          (Option.to_list (Parse.opaque_declaration "background-color" value))
    | Bg_bracket_raw_image (_, value) ->
        style
          (Option.to_list (Parse.opaque_declaration "background-image" value))
    | Bg_bracket_raw_position (_, value) ->
        style
          (Option.to_list
             (Parse.opaque_declaration "background-position" value))
    | Bg_bracket_length inner -> (
        match parse_bracket_size inner with
        | Some decl -> style [ decl ]
        | None ->
            style
              (Option.to_list
                 (Parse.opaque_declaration "background-size" inner)))
    | Bg_position_bracket (_, pos) -> style [ Css.background_position [ pos ] ]
    | Bg_position_raw (_, v) ->
        style
          (Option.to_list (Parse.opaque_declaration "background-position" v))
    | Bg_size_bracket inner -> (
        (* [of_class] refused an empty hint, so the peel succeeds here. *)
        let value =
          Option.value (bracket_value_after_hint inner) ~default:inner
        in
        match parse_bracket_size value with
        | Some decl -> style [ decl ]
        | None when Parse.is_var value ->
            let var_name = Parse.extract_var_name value in
            let ref : Css.background_size Css.var = Var.bracket var_name in
            style [ Css.background_size (Var ref) ]
        | None ->
            (* A value the size grammar declines is forwarded verbatim, not
               replaced by [auto]: substituting a value the class never asked
               for paints something rather than nothing. *)
            style
              (Option.to_list
                 (Parse.opaque_declaration "background-size" value)))

  let of_class theme class_name =
    let parts = Parse.split_class class_name in
    match parts with
    (* A [bg-] class shaped like a gradient is the gradients' to accept or
       refuse: a theme colour spelled the same way does not make it a background
       colour. *)
    | _ when Gradients.Handler.owns_bg_class class_name ->
        Error (`Msg "A gradient utility, not a background one")
    | [ "bg"; "origin"; "border" ] -> Ok Bg_origin_border
    | [ "bg"; "origin"; "padding" ] -> Ok Bg_origin_padding
    | [ "bg"; "origin"; "content" ] -> Ok Bg_origin_content
    | [ "bg"; "clip"; "border" ] -> Ok Bg_clip_border
    | [ "bg"; "clip"; "padding" ] -> Ok Bg_clip_padding
    | [ "bg"; "clip"; "content" ] -> Ok Bg_clip_content
    | [ "bg"; "clip"; "text" ] -> Ok Bg_clip_text
    (* Background color keywords *)
    | [ "bg"; "inherit" ] -> Ok Bg_inherit
    | [ "bg"; "transparent" ] -> Ok Bg_transparent
    | [ "bg"; current_str ]
      when String.starts_with ~prefix:"current" current_str -> (
        let base, opacity = Color.parse_opacity_modifier ~theme current_str in
        match opacity with
        | Color.No_opacity when base = "current" -> Ok Bg_current
        | Color.No_opacity -> Error (`Msg ("Invalid bg: " ^ current_str))
        | _ -> Ok (Bg_current_opacity opacity))
    (* Background image *)
    | [ "bg"; "none" ] -> Ok Bg_none
    (* Background size *)
    | [ "bg"; "auto" ] -> Ok Bg_auto
    | [ "bg"; "cover" ] -> Ok Bg_cover
    | [ "bg"; "contain" ] -> Ok Bg_contain
    (* Background attachment *)
    | [ "bg"; "fixed" ] -> Ok Bg_fixed
    | [ "bg"; "local" ] -> Ok Bg_local
    | [ "bg"; "scroll" ] -> Ok Bg_scroll
    (* Background repeat *)
    | [ "bg"; "repeat" ] -> Ok Bg_repeat
    | [ "bg"; "no"; "repeat" ] -> Ok Bg_no_repeat
    | [ "bg"; "repeat"; "x" ] -> Ok Bg_repeat_x
    | [ "bg"; "repeat"; "y" ] -> Ok Bg_repeat_y
    | [ "bg"; "repeat"; "round" ] -> Ok Bg_repeat_round
    | [ "bg"; "repeat"; "space" ] -> Ok Bg_repeat_space
    (* Background position *)
    | [ "bg"; "bottom" ] -> Ok (Bg_position Position.Bottom)
    | [ "bg"; "bottom"; "left" ] -> Ok (Bg_position Position.Bottom_left)
    | [ "bg"; "bottom"; "right" ] -> Ok (Bg_position Position.Bottom_right)
    | [ "bg"; "center" ] -> Ok (Bg_position Position.Center)
    | [ "bg"; "left" ] -> Ok (Bg_position Position.Left)
    | [ "bg"; "left"; "bottom" ] -> Ok (Bg_position Position.Left_bottom)
    | [ "bg"; "left"; "top" ] -> Ok (Bg_position Position.Left_top)
    | [ "bg"; "right" ] -> Ok (Bg_position Position.Right)
    | [ "bg"; "right"; "bottom" ] -> Ok (Bg_position Position.Right_bottom)
    | [ "bg"; "right"; "top" ] -> Ok (Bg_position Position.Right_top)
    | [ "bg"; "top" ] -> Ok (Bg_position Position.Top)
    | [ "bg"; "top"; "left" ] -> Ok (Bg_position Position.Top_left)
    | [ "bg"; "top"; "right" ] -> Ok (Bg_position Position.Top_right)
    (* bg-position-[...] bracket notation *)
    | [ "bg"; "position"; bracket ] when Parse.is_bracket_value bracket -> (
        let inner = Parse.bracket_inner bracket in
        match bracket_position_value inner with
        | Some pos -> Ok (Bg_position_bracket (inner, pos))
        | None -> (
            match Parse.arbitrary_declaration_value inner with
            | Some raw -> Ok (Bg_position_raw (inner, raw))
            | None -> Error (`Msg "Invalid background-position value")))
    (* bg-size-[...] bracket notation *)
    | [ "bg"; "size"; bracket ] when Parse.is_bracket_value bracket ->
        let inner = Parse.bracket_inner bracket in
        let readable value =
          parse_bracket_size value <> None || Parse.is_var value
        in
        if
          Option.fold ~none:false ~some:readable
            (bracket_value_after_hint inner)
        then Ok (Bg_size_bracket inner)
        else if
          (* The size reader keeps its own spelling; a value it declines still
             names background-size, which Bg_size_bracket forwards verbatim. *)
          Parse.arbitrary_declaration_value inner <> None
        then Ok (Bg_size_bracket inner)
        else Error (`Msg "Invalid background-size value")
    (* Bracket notation: bg-[...] and bg-[...]/opacity *)
    | [ "bg"; bracket_stuff ] when String.starts_with ~prefix:"[" bracket_stuff
      -> (
        (* Find the matching ] for the first [ *)
        let len = String.length bracket_stuff in
        let close = ref (-1) in
        let depth = ref 0 in
        for i = 0 to len - 1 do
          if !close < 0 then
            if bracket_stuff.[i] = '[' then incr depth
            else if bracket_stuff.[i] = ']' then (
              decr depth;
              if !depth = 0 then close := i)
        done;
        let parse_opacity s = Color.opacity_of_string ~theme s in
        (* The last resort forwards a value verbatim, so it has to be handed one
           value: [bg-[10px][20px]] is two brackets glued together, which is no
           utility to Tailwind either. Every reader above answers for a shape it
           recognises; only the fall-through needs this. *)
        let last_resort inner =
          if Parse.is_bracket_value bracket_stuff then
            Parse.arbitrary_declaration_value inner
          else None
        in
        if !close >= 0 && !close + 1 < len && bracket_stuff.[!close + 1] = '/'
        then
          (* Bracket with opacity: [color:var(--x)]/50 *)
          let bracket = String.sub bracket_stuff 0 (!close + 1) in
          let opacity_str =
            String.sub bracket_stuff (!close + 2) (len - !close - 2)
          in
          let inner = Parse.bracket_inner bracket in
          match parse_opacity opacity_str with
          | Some opacity -> (
              match Color.bracket_color_hint inner with
              | Some v -> (
                  if
                    (* The hint says how to read the value written after it;
                       only a var() reference there names a custom property. *)
                    Parse.is_var v
                  then Ok (Bg_bracket_color_var_opacity (v, opacity))
                  else
                    match Color.parse_bracket_color v with
                    | Some css_color ->
                        Ok
                          (Bg_bracket_color_opacity (inner, css_color, opacity))
                    | None -> Error (`Msg ("Unknown bg bracket color: " ^ v)))
              | None when Parse.is_var inner ->
                  (* A var() holds an unknown colour, so the alpha has to be
                     applied at run time by color-mix, not folded here. *)
                  Ok (Bg_bracket_var_opacity (inner, opacity))
              | None -> (
                  match Color.parse_bracket_color inner with
                  | Some css_color ->
                      Ok (Bg_bracket_color_opacity (inner, css_color, opacity))
                  | None ->
                      Error
                        (`Msg ("Unknown bg bracket value: " ^ bracket_stuff))))
          | None -> Error (`Msg ("Invalid opacity: " ^ bracket_stuff))
        else
          (* Regular bracket notation: bg-[...] *)
          let inner = Parse.bracket_inner bracket_stuff in
          (* A hint with nothing after it names no value: it is read below as
             the position hint or the last resort would read it. *)
          let hint =
            Option.bind (Parse.data_type_hint inner) (fun (h, v) ->
                if v = "" then None else Some (h, v))
          in
          match (inner, hint) with
          | "contain", _ -> Ok Bg_bracket_contain
          | "cover", _ -> Ok Bg_bracket_cover
          | _, Some ("length", v) -> Ok (Bg_bracket_length v)
          | _, Some ("size", v) -> Ok (Bg_bracket_size v)
          | _ when has_bracket_position_hint inner -> (
              (* The hint forces a background-position; a value the grammar
                 rejects is not a utility. It used to fall through to a
                 plausible-looking [center]. *)
              let v =
                Option.value (Parse.value_after_hint inner) ~default:inner
              in
              match bracket_position_value v with
              | Some pos -> Ok (Bg_bracket_typed_position (inner, pos))
              | None -> (
                  (* The hint chose the longhand, so the value goes there
                     whatever the position grammar makes of it. *)
                  match last_resort inner with
                  | Some raw -> Ok (Bg_bracket_raw_position (inner, raw))
                  | None -> Error (`Msg ("Unknown bg bracket position: " ^ v))))
          | _, Some ("color", v) when Color.is_bracket_color v -> (
              if
                (* The hint says how to read the value written after it; only a
                   var() reference there names a custom property. *)
                Parse.is_var v
              then Ok (Bg_bracket_color_var v)
              else
                match Color.parse_bracket_color v with
                | Some css_color -> Ok (Bg_bracket_color (inner, css_color))
                | None -> (
                    match last_resort inner with
                    | Some raw -> Ok (Bg_bracket_raw_color (inner, raw))
                    | None -> Error (`Msg ("Unknown bg bracket color: " ^ v))))
          | _, Some ("image", v) -> (
              (* The [image:] data-type hint forces a background-image. The
                 value is a [url(...)] literal, a [var(...)] reference, or a
                 literal image (e.g. a gradient). *)
              let raw_image () =
                match last_resort inner with
                | Some raw -> Ok (Bg_bracket_raw_image (inner, raw))
                | None -> Error (`Msg ("Unknown bg bracket image: " ^ v))
              in
              if String.starts_with ~prefix:"url(" v then
                match Parse.url_token (Parse.decode_arbitrary_value v) with
                | Some _ -> Ok (Bg_bracket_image_url v)
                | None -> raw_image ()
              else if Parse.is_var v then Ok (Bg_bracket_image_var v)
              else
                match parse_bracket_image v with
                | Some img -> Ok (Bg_bracket_image (v, img))
                | None -> raw_image ())
          | _, Some ("url", v) -> (
              if
                (* The [url:] data-type hint forces a background-image the way
                   [image:] does. Only a var() reference names a custom
                   property; any other spelling is the image itself. *)
                Parse.is_var v
              then Ok (Bg_bracket_url_var v)
              else if
                Parse.url_token (Parse.decode_arbitrary_value v) <> None
                || parse_bracket_image v <> None
              then Ok (Bg_bracket_url_var v)
              else
                match last_resort inner with
                | Some raw -> Ok (Bg_bracket_raw_image (inner, raw))
                | None -> Error (`Msg ("Unknown bg bracket url: " ^ v)))
          | _ when String.starts_with ~prefix:"url(" inner -> (
              match Parse.url_token (Parse.decode_arbitrary_value inner) with
              | Some _ -> Ok (Bg_bracket_url inner)
              | None -> (
                  (* No hint, so the colour is the last resort even though the
                     text opens with [url(]. *)
                  match last_resort inner with
                  | Some raw -> Ok (Bg_bracket_raw_color (inner, raw))
                  | None -> Error (`Msg ("Unknown bg bracket url: " ^ inner))))
          | _ when Parse.is_var inner -> Ok (Bg_bracket_var inner)
          | _ -> (
              (* Try parsing as background-image (gradients, urls,
                 comma-separated) *)
              match parse_bracket_image inner with
              | Some img -> Ok (Bg_bracket_linear_gradient (inner, img))
              | None -> (
                  match Color.parse_bracket_color inner with
                  | Some css_color -> Ok (Bg_bracket_color (inner, css_color))
                  | None -> (
                      match parse_bracket_position inner with
                      | Some pos -> Ok (Bg_bracket_position (inner, pos))
                      | None -> (
                          (* The colour is this family's last resort, which an
                             unknown hint reaches as well as no hint at all. *)
                          match last_resort inner with
                          | Some raw -> Ok (Bg_bracket_raw_color (inner, raw))
                          | None ->
                              Error
                                (`Msg ("Unknown bg bracket value: " ^ inner)))))
              ))
    | "bg" :: rest when List.exists Color.has_opacity rest -> (
        match Color.shade_and_opacity_of_strings ~theme rest with
        | Ok (color, shade, opacity) -> Ok (Bg_opacity (color, shade, opacity))
        | Error e -> Error e)
    | "bg" :: rest -> (
        match Color.shade_of_strings ~theme rest with
        | Ok (color, shade) -> Ok (Bg (color, shade))
        | Error _ -> Error (`Msg "Invalid background color"))
    | _ -> Error (`Msg "Unknown background class")

  let examples =
    [
      Bg_none;
      Bg_auto;
      Bg_no_repeat;
      Bg_fixed;
      Bg_clip_border;
      Bg_origin_border;
      Bg_current;
      Bg_position Position.Center;
    ]
end

open Handler
module Utility_factory = Utility.Make (Handler)

let utility = Utility_factory.v

let bg ?opacity ?(shade = 500) color =
  Color.check_shade ~utility:"bg" color shade;
  match opacity with
  | None -> utility (Bg (color, shade))
  | Some pct -> utility (Bg_opacity (color, shade, Color.opacity_of_int pct))

let bg_transparent = utility Bg_transparent
let bg_current = utility Bg_current
let bg_fixed = utility Bg_fixed
let bg_local = utility Bg_local
let bg_scroll = utility Bg_scroll
let bg_clip_border = utility Bg_clip_border
let bg_clip_padding = utility Bg_clip_padding
let bg_clip_content = utility Bg_clip_content
let bg_clip_text = utility Bg_clip_text
let bg_origin_border = utility Bg_origin_border
let bg_origin_padding = utility Bg_origin_padding
let bg_origin_content = utility Bg_origin_content
let bg_bottom = utility (Bg_position Position.Bottom)
let bg_bottom_left = utility (Bg_position Position.Bottom_left)
let bg_bottom_right = utility (Bg_position Position.Bottom_right)
let bg_center = utility (Bg_position Position.Center)
let bg_left = utility (Bg_position Position.Left)
let bg_left_bottom = utility (Bg_position Position.Left_bottom)
let bg_left_top = utility (Bg_position Position.Left_top)
let bg_right = utility (Bg_position Position.Right)
let bg_right_bottom = utility (Bg_position Position.Right_bottom)
let bg_right_top = utility (Bg_position Position.Right_top)
let bg_top = utility (Bg_position Position.Top)
let bg_top_left = utility (Bg_position Position.Top_left)
let bg_top_right = utility (Bg_position Position.Top_right)
let bg_repeat = utility Bg_repeat
let bg_no_repeat = utility Bg_no_repeat
let bg_repeat_x = utility Bg_repeat_x
let bg_repeat_y = utility Bg_repeat_y
let bg_repeat_round = utility Bg_repeat_round
let bg_repeat_space = utility Bg_repeat_space
let bg_auto = utility Bg_auto
let bg_cover = utility Bg_cover
let bg_contain = utility Bg_contain
