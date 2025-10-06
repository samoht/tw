(** Form element utilities. *)

(* Define form-specific variables using the Var system *)
let tw_shadow_var =
  Var.property_default Css.Shadow
    ~initial:
      (Css.shadow ~h_offset:Css.Zero ~v_offset:Css.Zero ~color:(Css.hex "#0000")
         ())
    "tw-shadow"

let tw_ring_inset_var = Var.channel Css.String "tw-ring-inset"
let tw_ring_offset_width_var = Var.channel Css.String "tw-ring-offset-width"
let tw_ring_offset_color_var = Var.channel Css.String "tw-ring-offset-color"
let tw_ring_color_var = Var.channel Css.String "tw-ring-color"
let tw_ring_offset_shadow_var = Var.channel Css.String "tw-ring-offset-shadow"
let tw_ring_shadow_var = Var.channel Css.String "tw-ring-shadow"

module Handler = struct
  open Style

  type t =
    | Form_input
    | Form_textarea
    | Form_select
    | Form_checkbox
    | Form_radio

  type Utility.base += Self of t

  let name = "forms"
  let priority = 26

  let form_input =
    let open Css in
    let open Css.Selector in
    (* Base class selector *)
    let base_sel = class_ "form-input" in

    (* Create all rules including base as first rule *)
    let rules =
      [
        (* Base state - MUST be first *)
        rule ~selector:base_sel
          [
            appearance None;
            background_color (hex "#fff");
            border_color (oklch 55.1 0.027 264.364);
            border_width (Px 1.);
            border_radius (Px 0.);
            padding_top (Rem 0.5);
            padding_bottom (Rem 0.5);
            padding_left (Rem 0.75);
            padding_right (Rem 0.75);
            font_size (Rem 1.);
            line_height (Rem 1.5);
          ];
        (* :focus state *)
        rule
          ~selector:(compound [ base_sel; Focus ])
          [
            outline_offset (Px 2.);
            custom_property "--tw-ring-inset" "var(--tw-empty, )";
            custom_property "--tw-ring-offset-width" "2px";
            custom_property "--tw-ring-offset-color" "#fff";
            custom_property "--tw-ring-color" "oklch(54.6% 0.245 262.881)";
            custom_property "--tw-ring-offset-shadow"
              "var(--tw-ring-inset) 0 0 0 var(--tw-ring-offset-width) \
               var(--tw-ring-offset-color)";
            custom_property "--tw-ring-shadow"
              "var(--tw-ring-inset) 0 0 0 calc(2px + \
               var(--tw-ring-offset-width)) var(--tw-ring-color)";
            outline "2px solid #0000";
          ];
        (* ::placeholder *)
        rule
          ~selector:(compound [ base_sel; Placeholder ])
          [ color (oklch 55.1 0.027 264.364); opacity 1. ];
        (* Webkit datetime pseudo-elements - all with padding: 0 *)
        rule
          ~selector:(compound [ base_sel; Webkit_datetime_edit_fields_wrapper ])
          [ padding [ Px 0. ] ];
        rule
          ~selector:(compound [ base_sel; Webkit_date_and_time_value ])
          [ min_height (Rem 1.5) ];
        rule
          ~selector:(compound [ base_sel; Webkit_date_and_time_value ])
          [ text_align Inherit ];
        rule
          ~selector:(compound [ base_sel; Webkit_datetime_edit ])
          [ display Inline_flex ];
        (* webkit-datetime-edit padding *)
        rule
          ~selector:(compound [ base_sel; Webkit_datetime_edit ])
          [ padding [ Px 0. ] ];
        (* All datetime field selectors with padding *)
        rule
          ~selector:(compound [ base_sel; Webkit_datetime_edit_year_field ])
          [ padding [ Px 0. ] ];
        rule
          ~selector:(compound [ base_sel; Webkit_datetime_edit_month_field ])
          [ padding [ Px 0. ] ];
        rule
          ~selector:(compound [ base_sel; Webkit_datetime_edit_day_field ])
          [ padding [ Px 0. ] ];
        rule
          ~selector:(compound [ base_sel; Webkit_datetime_edit_hour_field ])
          [ padding [ Px 0. ] ];
        rule
          ~selector:(compound [ base_sel; Webkit_datetime_edit_minute_field ])
          [ padding [ Px 0. ] ];
        rule
          ~selector:(compound [ base_sel; Webkit_datetime_edit_second_field ])
          [ padding [ Px 0. ] ];
        rule
          ~selector:
            (compound [ base_sel; Webkit_datetime_edit_millisecond_field ])
          [ padding [ Px 0. ] ];
        rule
          ~selector:(compound [ base_sel; Webkit_datetime_edit_meridiem_field ])
          [ padding [ Px 0. ] ];
      ]
    in

    (* Return style with only rules, no base declarations *)
    style ~rules:(Some rules) []

  let form_textarea =
    let open Css in
    let open Css.Selector in
    (* Base class selector *)
    let base_sel = class_ "form-textarea" in

    (* Create all rules including base as first rule *)
    let rules =
      [
        (* Base state - MUST be first *)
        rule ~selector:base_sel
          [
            appearance None;
            background_color (hex "#fff");
            border_color (oklch 55.1 0.027 264.364);
            border_width (Px 1.);
            border_radius (Px 0.);
            padding_top (Rem 0.5);
            padding_bottom (Rem 0.5);
            padding_left (Rem 0.75);
            padding_right (Rem 0.75);
            font_size (Rem 1.);
            line_height (Rem 1.5);
            resize Vertical;
          ];
        (* :focus state *)
        rule
          ~selector:(compound [ base_sel; Focus ])
          [
            outline_offset (Px 2.);
            custom_property "--tw-ring-inset" "var(--tw-empty, )";
            custom_property "--tw-ring-offset-width" "2px";
            custom_property "--tw-ring-offset-color" "#fff";
            custom_property "--tw-ring-color" "oklch(54.6% 0.245 262.881)";
            custom_property "--tw-ring-offset-shadow"
              "var(--tw-ring-inset) 0 0 0 var(--tw-ring-offset-width) \
               var(--tw-ring-offset-color)";
            custom_property "--tw-ring-shadow"
              "var(--tw-ring-inset) 0 0 0 calc(2px + \
               var(--tw-ring-offset-width)) var(--tw-ring-color)";
            outline "2px solid #0000";
          ];
        (* ::placeholder *)
        rule
          ~selector:(compound [ base_sel; Placeholder ])
          [ color (oklch 55.1 0.027 264.364); opacity 1. ];
      ]
    in

    (* Return style with only rules, no base declarations *)
    style ~rules:(Some rules) []

  let form_select =
    let open Css in
    let open Css.Selector in
    (* Base class selector *)
    let base_sel = class_ "form-select" in

    (* Create all rules including base as first rule *)
    let rules =
      [
        (* Base state - MUST be first *)
        rule ~selector:base_sel
          [
            appearance None;
            background_color (hex "#fff");
            border_color (oklch 55.1 0.027 264.364);
            border_width (Px 1.);
            border_radius (Px 0.);
            padding_top (Rem 0.5);
            padding_bottom (Rem 0.5);
            padding_left (Rem 0.75);
            padding_right (Rem 2.5);
            font_size (Rem 1.);
            line_height (Rem 1.5);
          ];
        (* :focus state *)
        rule
          ~selector:(compound [ base_sel; Focus ])
          [
            outline_offset (Px 2.);
            custom_property "--tw-ring-inset" "var(--tw-empty, )";
            custom_property "--tw-ring-offset-width" "2px";
            custom_property "--tw-ring-offset-color" "#fff";
            custom_property "--tw-ring-color" "oklch(54.6% 0.245 262.881)";
            custom_property "--tw-ring-offset-shadow"
              "var(--tw-ring-inset) 0 0 0 var(--tw-ring-offset-width) \
               var(--tw-ring-offset-color)";
            custom_property "--tw-ring-shadow"
              "var(--tw-ring-inset) 0 0 0 calc(2px + \
               var(--tw-ring-offset-width)) var(--tw-ring-color)";
            outline "2px solid #0000";
          ];
        (* Base with background image - for single select *)
        rule ~selector:base_sel
          [
            background_image
              (Url
                 "data:image/svg+xml,%3csvg xmlns='http://www.w3.org/2000/svg' \
                  fill='none' viewBox='0 0 20 20'%3e%3cpath stroke='%236b7280' \
                  stroke-linecap='round' stroke-linejoin='round' \
                  stroke-width='1.5' d='M6 8l4 4 4-4'/%3e%3c/svg%3e");
            background_position [ Right_center ];
            background_repeat No_repeat;
            background_size (Size (Em 1.5, Em 1.5));
          ];
        (* Multi-select override - :where([size]:not([size="1"])) *)
        rule
          ~selector:
            (compound
               [
                 base_sel;
                 Where
                   [
                     Compound
                       [
                         Attribute (None, "size", Presence, None);
                         Not [ Attribute (None, "size", Exact "1", None) ];
                       ];
                   ];
               ])
          [ background_image None; padding_right (Rem 0.75) ];
      ]
    in

    (* Return style with only rules, no base declarations *)
    style ~rules:(Some rules) []

  let form_checkbox =
    let open Css in
    let open Css.Selector in
    (* Base class selector *)
    let base_sel = class_ "form-checkbox" in

    (* Create all rules including base as first rule *)
    let rules =
      [
        (* Base state - MUST be first *)
        rule ~selector:base_sel
          [
            appearance None;
            vertical_align Middle;
            user_select None;
            color (oklch 54.6 0.245 262.881);
            custom_property "--tw-shadow" "0 0 #0000";
            background_color (hex "#fff");
            border_width (Px 1.);
            border_color (oklch 55.1 0.027 264.364);
            border_radius (Px 0.);
            flex_shrink 0.0;
            width (Rem 1.);
            height (Rem 1.);
            display Inline_block;
          ];
        (* :focus state *)
        rule
          ~selector:(compound [ base_sel; Focus ])
          [
            outline_offset (Px 2.);
            custom_property "--tw-ring-inset" "var(--tw-empty, )";
            custom_property "--tw-ring-offset-width" "2px";
            custom_property "--tw-ring-offset-color" "#fff";
            custom_property "--tw-ring-color" "oklch(54.6% 0.245 262.881)";
            custom_property "--tw-ring-offset-shadow"
              "var(--tw-ring-inset) 0 0 0 var(--tw-ring-offset-width) \
               var(--tw-ring-offset-color)";
            custom_property "--tw-ring-shadow"
              "var(--tw-ring-inset) 0 0 0 calc(2px + \
               var(--tw-ring-offset-width)) var(--tw-ring-color)";
            outline "2px solid #0000";
          ];
        (* :checked state *)
        rule
          ~selector:(compound [ base_sel; Checked ])
          [
            background_color Current;
            background_image
              (Url
                 "data:image/svg+xml,%3csvg viewBox='0 0 16 16' fill='white' \
                  xmlns='http://www.w3.org/2000/svg'%3e%3cpath d='M12.207 \
                  4.793a1 1 0 010 1.414l-5 5a1 1 0 01-1.414 0l-2-2a1 1 0 \
                  011.414-1.414L6.5 9.086l4.293-4.293a1 1 0 011.414 \
                  0z'/%3e%3c/svg%3e");
            background_position [ Center ];
            background_repeat No_repeat;
            background_size Cover;
            border_color (hex "#0000");
          ];
        (* :checked:hover, :checked:focus state *)
        rule
          ~selector:
            (list
               [
                 compound [ base_sel; Checked; Hover ];
                 compound [ base_sel; Checked; Focus ];
               ])
          [ border_color (hex "#0000"); background_color Current ];
        (* :indeterminate state *)
        rule
          ~selector:(compound [ base_sel; Indeterminate ])
          [
            background_color Current;
            background_image
              (Url
                 "data:image/svg+xml,%3csvg xmlns='http://www.w3.org/2000/svg' \
                  fill='none' viewBox='0 0 16 16'%3e%3cpath stroke='white' \
                  stroke-linecap='round' stroke-linejoin='round' \
                  stroke-width='2' d='M4 8h8'/%3e%3c/svg%3e");
            background_position [ Center ];
            background_repeat No_repeat;
            background_size (Size (Pct 100., Pct 100.));
            border_color (hex "#0000");
          ];
        (* :indeterminate:hover, :indeterminate:focus state *)
        rule
          ~selector:
            (list
               [
                 compound [ base_sel; Indeterminate; Hover ];
                 compound [ base_sel; Indeterminate; Focus ];
               ])
          [ border_color (hex "#0000"); background_color Current ];
      ]
    in

    (* Return style with only rules, no base declarations *)
    style ~rules:(Some rules) []

  let form_radio =
    let open Css in
    let open Css.Selector in
    (* Base class selector *)
    let base_sel = class_ "form-radio" in

    (* Create all rules including base as first rule *)
    let rules =
      [
        (* Base state - MUST be first *)
        rule ~selector:base_sel
          [
            appearance None;
            vertical_align Middle;
            user_select None;
            color (oklch 54.6 0.245 262.881);
            custom_property "--tw-shadow" "0 0 #0000";
            background_color (hex "#fff");
            border_width (Px 1.);
            border_color (oklch 55.1 0.027 264.364);
            border_radius (Pct 100.0);
            (* Circular for radio buttons *)
            flex_shrink 0.0;
            width (Rem 1.);
            height (Rem 1.);
            display Inline_block;
          ];
        (* :focus state *)
        rule
          ~selector:(compound [ base_sel; Focus ])
          [
            outline_offset (Px 2.);
            custom_property "--tw-ring-inset" "var(--tw-empty, )";
            custom_property "--tw-ring-offset-width" "2px";
            custom_property "--tw-ring-offset-color" "#fff";
            custom_property "--tw-ring-color" "oklch(54.6% 0.245 262.881)";
            custom_property "--tw-ring-offset-shadow"
              "var(--tw-ring-inset) 0 0 0 var(--tw-ring-offset-width) \
               var(--tw-ring-offset-color)";
            custom_property "--tw-ring-shadow"
              "var(--tw-ring-inset) 0 0 0 calc(2px + \
               var(--tw-ring-offset-width)) var(--tw-ring-color)";
            outline "2px solid #0000";
          ];
        (* :checked state *)
        rule
          ~selector:(compound [ base_sel; Checked ])
          [
            background_color Current;
            background_image
              (Url
                 "data:image/svg+xml,%3csvg viewBox='0 0 16 16' fill='white' \
                  xmlns='http://www.w3.org/2000/svg'%3e%3ccircle cx='8' cy='8' \
                  r='3'/%3e%3c/svg%3e");
            background_position [ Center ];
            background_repeat No_repeat;
            background_size Cover;
            border_color (hex "#0000");
          ];
        (* :checked:hover, :checked:focus state *)
        rule
          ~selector:
            (list
               [
                 compound [ base_sel; Checked; Hover ];
                 compound [ base_sel; Checked; Focus ];
               ])
          [ border_color (hex "#0000"); background_color Current ];
      ]
    in

    (* Return style with only rules, no base declarations *)
    style ~rules:(Some rules) []

  let to_style = function
    | Form_input -> form_input
    | Form_textarea -> form_textarea
    | Form_select -> form_select
    | Form_checkbox -> form_checkbox
    | Form_radio -> form_radio

  let suborder = function
    | Form_checkbox -> 0
    | Form_radio -> 1
    | Form_input -> 2
    | Form_select -> 3
    | Form_textarea -> 4

  let of_class = function
    | "form-input" -> Ok Form_input
    | "form-textarea" -> Ok Form_textarea
    | "form-select" -> Ok Form_select
    | "form-checkbox" -> Ok Form_checkbox
    | "form-radio" -> Ok Form_radio
    | _ -> Error (`Msg "Not a form utility")

  let to_class = function
    | Form_input -> "form-input"
    | Form_textarea -> "form-textarea"
    | Form_select -> "form-select"
    | Form_checkbox -> "form-checkbox"
    | Form_radio -> "form-radio"
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
