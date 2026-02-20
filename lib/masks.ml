(** Mask utilities for CSS masking.

    Provides utilities for mask-image, mask-composite, mask-mode, mask-type,
    mask-size, mask-clip, and mask-origin. *)

module Handler = struct
  open Style
  open Css

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
  let mask_auto = style [ Css.webkit_mask_size Auto; Css.mask_size Auto ]

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

  let suborder = function
    (* Alphabetical order *)
    | Mask_add -> 0
    | Mask_alpha -> 1
    | Mask_auto -> 2
    | Mask_clip_border -> 3
    | Mask_clip_content -> 4
    | Mask_clip_fill -> 5
    | Mask_clip_padding -> 6
    | Mask_clip_stroke -> 7
    | Mask_clip_view -> 8
    | Mask_exclude -> 9
    | Mask_intersect -> 10
    | Mask_luminance -> 11
    | Mask_match -> 12
    | Mask_no_clip -> 13
    | Mask_none -> 14
    | Mask_origin_border -> 15
    | Mask_origin_content -> 16
    | Mask_origin_fill -> 17
    | Mask_origin_padding -> 18
    | Mask_origin_stroke -> 19
    | Mask_origin_view -> 20
    | Mask_subtract -> 21
    | Mask_type_alpha -> 22
    | Mask_type_luminance -> 23

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
    | [ "mask"; "clip"; "border" ] -> Ok Mask_clip_border
    | [ "mask"; "clip"; "padding" ] -> Ok Mask_clip_padding
    | [ "mask"; "clip"; "content" ] -> Ok Mask_clip_content
    | [ "mask"; "clip"; "fill" ] -> Ok Mask_clip_fill
    | [ "mask"; "clip"; "stroke" ] -> Ok Mask_clip_stroke
    | [ "mask"; "clip"; "view" ] -> Ok Mask_clip_view
    | [ "mask"; "no"; "clip" ] -> Ok Mask_no_clip
    | [ "mask"; "origin"; "border" ] -> Ok Mask_origin_border
    | [ "mask"; "origin"; "padding" ] -> Ok Mask_origin_padding
    | [ "mask"; "origin"; "content" ] -> Ok Mask_origin_content
    | [ "mask"; "origin"; "fill" ] -> Ok Mask_origin_fill
    | [ "mask"; "origin"; "stroke" ] -> Ok Mask_origin_stroke
    | [ "mask"; "origin"; "view" ] -> Ok Mask_origin_view
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
