(** Form element utilities *)

open Core
open Css

(** {1 Form Input Utilities} *)

(* Basic form input placeholder - actual styling would be plugin-specific *)
let form_input = style "form-input" []

let form_textarea =
  style "form-textarea"
    [
      Css.appearance None;
      Css.background_color (Rgb { r = 255; g = 255; b = 255 });
      Css.border_color (Rgb { r = 209; g = 213; b = 219 });
      Css.border_width (Px 1);
      Css.border_radius (Rem 0.375);
      Css.padding_top (Rem 0.5);
      Css.padding_right (Rem 0.75);
      Css.padding_bottom (Rem 0.5);
      Css.padding_left (Rem 0.75);
      Css.font_size (Rem 1.0);
      Css.line_height (Rem 1.5);
      Css.resize Vertical;
    ]

let form_select =
  style "form-select"
    [
      Css.appearance None;
      Css.background_color (Rgb { r = 255; g = 255; b = 255 });
      Css.border_color (Rgb { r = 209; g = 213; b = 219 });
      Css.border_width (Px 1);
      Css.border_radius (Rem 0.375);
      Css.padding_top (Rem 0.5);
      Css.padding_right (Rem 2.5);
      Css.padding_bottom (Rem 0.5);
      Css.padding_left (Rem 0.75);
      Css.font_size (Rem 1.0);
      Css.line_height (Rem 1.5);
      Css.background_image
        (Css.url
           "data:image/svg+xml,%3csvg xmlns='http://www.w3.org/2000/svg' \
            fill='none' viewBox='0 0 20 20'%3e%3cpath stroke='%236b7280' \
            stroke-linecap='round' stroke-linejoin='round' stroke-width='1.5' \
            d='M6 8l4 4 4-4'/%3e%3c/svg%3e");
      Css.background_position "right 0.5rem center";
      Css.background_repeat No_repeat;
      Css.background_size "1.5em 1.5em";
    ]

let form_checkbox =
  style "form-checkbox"
    [
      Css.appearance None;
      Css.width (Rem 1.0);
      Css.height (Rem 1.0);
      Css.background_color (Rgb { r = 255; g = 255; b = 255 });
      Css.border_color (Rgb { r = 209; g = 213; b = 219 });
      Css.border_width (Px 1);
      Css.border_radius (Rem 0.25);
      Css.color (Rgb { r = 59; g = 130; b = 246 });
      Css.Flex.shrink 0.0;
      Css.display Inline_block;
      Css.vertical_align Middle;
    ]

let form_radio =
  style "form-radio"
    [
      Css.appearance None;
      Css.width (Rem 1.0);
      Css.height (Rem 1.0);
      Css.background_color (Rgb { r = 255; g = 255; b = 255 });
      Css.border_color (Rgb { r = 209; g = 213; b = 219 });
      Css.border_width (Px 1);
      Css.border_radius (Pct 100.0);
      Css.color (Rgb { r = 59; g = 130; b = 246 });
      Css.Flex.shrink 0.0;
      Css.display Inline_block;
      Css.vertical_align Middle;
    ]

(** {1 Parsing Functions} *)

let of_string = function
  | [ "form"; "input" ] -> Ok form_input
  | [ "form"; "textarea" ] -> Ok form_textarea
  | [ "form"; "select" ] -> Ok form_select
  | [ "form"; "checkbox" ] -> Ok form_checkbox
  | [ "form"; "radio" ] -> Ok form_radio
  | _ -> Error (`Msg "Not a form utility")
