(** Form element utilities. *)

module Handler = struct
  open Style
  open Css

  type t =
    | Form_input
    | Form_textarea
    | Form_select
    | Form_checkbox
    | Form_radio

  type Utility.base += Self of t

  let priority = 800
  let form_input = style "form-input" []

  let form_textarea =
    style "form-textarea"
      [
        Css.appearance None;
        Css.background_color (Css.rgb 255 255 255);
        Css.border_color (Css.rgb 209 213 219);
        Css.border_width (Px 1.);
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
        Css.background_color (Css.rgb 255 255 255);
        Css.border_color (Css.rgb 209 213 219);
        Css.border_width (Px 1.);
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
              stroke-linecap='round' stroke-linejoin='round' \
              stroke-width='1.5' d='M6 8l4 4 4-4'/%3e%3c/svg%3e");
        Css.background_position [ Right_center ];
        Css.background_repeat No_repeat;
        Css.background_size (Size (Em 1.5, Em 1.5));
      ]

  let form_checkbox =
    style "form-checkbox"
      [
        Css.appearance None;
        Css.width (Rem 1.0);
        Css.height (Rem 1.0);
        Css.background_color (Css.rgb 255 255 255);
        Css.border_color (Css.rgb 209 213 219);
        Css.border_width (Px 1.);
        Css.border_radius (Rem 0.25);
        Css.color (Css.rgb 59 130 246);
        Css.flex_shrink 0.0;
        Css.display Inline_block;
        Css.vertical_align Middle;
      ]

  let form_radio =
    style "form-radio"
      [
        Css.appearance None;
        Css.width (Rem 1.0);
        Css.height (Rem 1.0);
        Css.background_color (Css.rgb 255 255 255);
        Css.border_color (Css.rgb 209 213 219);
        Css.border_width (Px 1.);
        Css.border_radius (Pct 100.0);
        Css.color (Css.rgb 59 130 246);
        Css.flex_shrink 0.0;
        Css.display Inline_block;
        Css.vertical_align Middle;
      ]

  let to_style = function
    | Form_input -> form_input
    | Form_textarea -> form_textarea
    | Form_select -> form_select
    | Form_checkbox -> form_checkbox
    | Form_radio -> form_radio

  let suborder = function
    | Form_input -> 0
    | Form_textarea -> 1
    | Form_select -> 2
    | Form_checkbox -> 3
    | Form_radio -> 4

  let of_string =
    let err_not_utility = Error (`Msg "Not a form utility") in
    function
    | [ "form"; "input" ] -> Ok Form_input
    | [ "form"; "textarea" ] -> Ok Form_textarea
    | [ "form"; "select" ] -> Ok Form_select
    | [ "form"; "checkbox" ] -> Ok Form_checkbox
    | [ "form"; "radio" ] -> Ok Form_radio
    | _ -> err_not_utility
end

open Handler

(** Register handler with Utility system *)
let () = Utility.register (module Handler)

(** Public API *)
let utility x = Utility.base (Self x)

let form_input = utility Form_input
let form_textarea = utility Form_textarea
let form_select = utility Form_select
let form_checkbox = utility Form_checkbox
let form_radio = utility Form_radio
