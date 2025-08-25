(** Background and gradient utilities *)

open Core

type direction =
  | Bottom
  | Bottom_right
  | Right
  | Top_right
  | Top
  | Top_left
  | Left
  | Bottom_left

let bg_gradient_to = function
  | Bottom ->
      style "bg-gradient-to-b"
        [
          Css.background_image
            "linear-gradient(to bottom, var(--tw-gradient-stops))";
        ]
  | Bottom_right ->
      style "bg-gradient-to-br"
        [
          Css.background_image
            "linear-gradient(to bottom right, var(--tw-gradient-stops))";
        ]
  | Right ->
      style "bg-gradient-to-r"
        [
          Css.background_image
            "linear-gradient(to right, var(--tw-gradient-stops))";
        ]
  | Top_right ->
      style "bg-gradient-to-tr"
        [
          Css.background_image
            "linear-gradient(to top right, var(--tw-gradient-stops))";
        ]
  | Top ->
      style "bg-gradient-to-t"
        [
          Css.background_image
            "linear-gradient(to top, var(--tw-gradient-stops))";
        ]
  | Top_left ->
      style "bg-gradient-to-tl"
        [
          Css.background_image
            "linear-gradient(to top left, var(--tw-gradient-stops))";
        ]
  | Left ->
      style "bg-gradient-to-l"
        [
          Css.background_image
            "linear-gradient(to left, var(--tw-gradient-stops))";
        ]
  | Bottom_left ->
      style "bg-gradient-to-bl"
        [
          Css.background_image
            "linear-gradient(to bottom left, var(--tw-gradient-stops))";
        ]

(* Legacy fixed-direction helpers removed in favor of bg_gradient_to *)

let from_color ?(shade = 500) color =
  let class_name =
    if Color.is_base_color color || Color.is_custom_color color then
      "from-" ^ Color.pp color
    else Pp.str [ "from-"; Color.pp color; "-"; string_of_int shade ]
  in
  let var_str =
    if Color.is_base_color color || Color.is_custom_color color then
      Pp.str [ "var(--color-"; Color.pp color; ")" ]
    else
      Pp.str [ "var(--color-"; Color.pp color; "-"; string_of_int shade; ")" ]
  in
  style class_name
    [
      Css.custom_property "--tw-gradient-from" var_str;
      Css.custom_property "--tw-gradient-stops"
        "var(--tw-gradient-via-stops,var(--tw-gradient-position),var(--tw-gradient-from)var(--tw-gradient-from-position),var(--tw-gradient-to)var(--tw-gradient-to-position))";
    ]

let via_color ?(shade = 500) color =
  let class_name =
    if Color.is_base_color color || Color.is_custom_color color then
      "via-" ^ Color.pp color
    else Pp.str [ "via-"; Color.pp color; "-"; string_of_int shade ]
  in
  let var_str =
    if Color.is_base_color color || Color.is_custom_color color then
      Pp.str [ "var(--color-"; Color.pp color; ")" ]
    else
      Pp.str [ "var(--color-"; Color.pp color; "-"; string_of_int shade; ")" ]
  in
  style class_name
    [
      Css.custom_property "--tw-gradient-via" var_str;
      Css.custom_property "--tw-gradient-via-stops"
        "var(--tw-gradient-position),var(--tw-gradient-from)var(--tw-gradient-from-position),var(--tw-gradient-via)var(--tw-gradient-via-position),var(--tw-gradient-to)var(--tw-gradient-to-position)";
      Css.custom_property "--tw-gradient-stops" "var(--tw-gradient-via-stops)";
    ]

let to_color ?(shade = 500) color =
  let class_name =
    if Color.is_base_color color || Color.is_custom_color color then
      "to-" ^ Color.pp color
    else Pp.str [ "to-"; Color.pp color; "-"; string_of_int shade ]
  in
  let var_str =
    if Color.is_base_color color || Color.is_custom_color color then
      Pp.str [ "var(--color-"; Color.pp color; ")" ]
    else
      Pp.str [ "var(--color-"; Color.pp color; "-"; string_of_int shade; ")" ]
  in
  style class_name
    [
      Css.custom_property "--tw-gradient-to" var_str;
      Css.custom_property "--tw-gradient-stops"
        "var(--tw-gradient-via-stops,var(--tw-gradient-position),var(--tw-gradient-from)var(--tw-gradient-from-position),var(--tw-gradient-to)var(--tw-gradient-to-position))";
    ]
