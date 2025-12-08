(** Form element utilities - matching Tailwind v4's [@tailwindcss/forms] plugin.

    These utilities use the ring/shadow variable system from Effects for focus
    states, ensuring proper type-safety and variable composition. *)

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

  (* Priority 5 places forms after margins (2) but before sizing (6) and other
     layout utilities, matching Tailwind's output ordering *)
  let priority = 5

  (* Colors used in forms *)
  let blue_600 = Css.oklch 54.6 0.245 262.881
  let gray_500 = Css.oklch 55.1 0.027 264.364

  (* Focus state declarations using the ring system from Effects. We use typed
     shadow values with Var references and inset_var to build the ring shadows
     matching Tailwind's format: var(--tw-ring-inset)0 0 0
     var(--tw-ring-offset-width)var(--tw-ring-offset-color) *)
  let focus_ring_decls ~offset_width ~ring_width_px =
    let open Css in
    (* Set --tw-ring-inset to var(--tw-empty, ) - evaluates to empty by
       default *)
    let d_ring_inset, _ =
      Var.binding Effects.ring_inset_var "var(--tw-empty, )"
    in

    (* Set ring offset width *)
    let d_offset_width, _ =
      Var.binding Effects.ring_offset_width_var offset_width
    in

    (* Set ring offset color *)
    let d_offset_color, _ =
      Var.binding Effects.ring_offset_color_var (hex "#fff")
    in

    (* Set ring color *)
    let d_ring_color, _ = Var.binding Effects.ring_color_var blue_600 in

    (* Build ring-offset-shadow with inset_var prefix: var(--tw-ring-inset)0 0 0
       var(--tw-ring-offset-width)var(--tw-ring-offset-color) *)
    let offset_shadow_spread : length = Var (var_ref "tw-ring-offset-width") in
    let offset_shadow_color : color = Var (var_ref "tw-ring-offset-color") in
    let ring_offset_shadow_value =
      shadow ~inset:false ~inset_var:"tw-ring-inset" ~h_offset:Zero
        ~v_offset:Zero ~blur:Zero ~spread:offset_shadow_spread
        ~color:offset_shadow_color ()
    in
    let d_ring_offset_shadow, _ =
      Var.binding Effects.ring_offset_shadow_var ring_offset_shadow_value
    in

    (* Build ring-shadow with inset_var prefix: var(--tw-ring-inset)0 0 0
       calc(Npx + var(--tw-ring-offset-width))var(--tw-ring-color) *)
    let offset_width_default : length = Px 0. in
    let ring_shadow_spread : length =
      Calc
        Calc.(
          add
            (px (float_of_int ring_width_px))
            (var ~default:offset_width_default "tw-ring-offset-width"))
    in
    let ring_shadow_color : color = Var (var_ref "tw-ring-color") in
    let ring_shadow_value =
      shadow ~inset:false ~inset_var:"tw-ring-inset" ~h_offset:Zero
        ~v_offset:Zero ~blur:Zero ~spread:ring_shadow_spread
        ~color:ring_shadow_color ()
    in
    let d_ring_shadow, _ =
      Var.binding Effects.ring_shadow_var ring_shadow_value
    in

    (* Compose box-shadow from variable references *)
    let v_ring_offset = Var.reference Effects.ring_offset_shadow_var in
    let v_ring = Var.reference Effects.ring_shadow_var in
    let v_shadow = Var.reference Effects.shadow_var in

    let box_shadow_vars : shadow list =
      [ Var v_ring_offset; Var v_ring; Var v_shadow ]
    in

    [
      outline_offset (Px 2.);
      d_ring_inset;
      d_offset_width;
      d_offset_color;
      d_ring_color;
      d_ring_offset_shadow;
      d_ring_shadow;
      box_shadows box_shadow_vars;
      outline "2px solid #0000";
    ]

  (* Focus state for input/textarea/select (1px ring, no offset) *)
  let input_focus_decls =
    let open Css in
    focus_ring_decls ~offset_width:(Px 0.) ~ring_width_px:1
    @ [ border_color blue_600 ]

  (* Focus state for checkbox/radio (2px ring, 2px offset) *)
  let checkbox_focus_decls =
    focus_ring_decls ~offset_width:(Css.Px 2.) ~ring_width_px:2

  let form_input =
    let open Css in
    let open Css.Selector in
    let base_sel = class_ "form-input" in

    (* Shadow initial value for base state *)
    let d_shadow, _ =
      Var.binding Effects.shadow_var
        (shadow ~h_offset:Zero ~v_offset:Zero ~color:(hex "#0000") ())
    in

    let rules =
      [
        rule ~selector:base_sel
          [
            appearance None;
            d_shadow;
            background_color (hex "#fff");
            border_width (Px 1.);
            border_color gray_500;
            border_radius (Px 0.);
            padding [ Rem 0.5; Rem 0.75 ];
            font_size (Rem 1.);
            line_height (Rem 1.5);
          ];
        rule ~selector:(compound [ base_sel; Focus ]) input_focus_decls;
        rule
          ~selector:(compound [ base_sel; Placeholder ])
          [ color gray_500; opacity 1. ];
        rule
          ~selector:(compound [ base_sel; Webkit_datetime_edit_fields_wrapper ])
          [ padding [ Px 0. ] ];
        rule
          ~selector:(compound [ base_sel; Webkit_date_and_time_value ])
          [ min_height (Em 1.5) ];
        rule
          ~selector:(compound [ base_sel; Webkit_date_and_time_value ])
          [ text_align Inherit ];
        rule
          ~selector:(compound [ base_sel; Webkit_datetime_edit ])
          [ display Inline_flex ];
        rule
          ~selector:(compound [ base_sel; Webkit_datetime_edit ])
          [ padding_top (Px 0.); padding_bottom (Px 0.) ];
        rule
          ~selector:(compound [ base_sel; Webkit_datetime_edit_year_field ])
          [ padding_top (Px 0.); padding_bottom (Px 0.) ];
        rule
          ~selector:(compound [ base_sel; Webkit_datetime_edit_month_field ])
          [ padding_top (Px 0.); padding_bottom (Px 0.) ];
        rule
          ~selector:(compound [ base_sel; Webkit_datetime_edit_day_field ])
          [ padding_top (Px 0.); padding_bottom (Px 0.) ];
        rule
          ~selector:(compound [ base_sel; Webkit_datetime_edit_hour_field ])
          [ padding_top (Px 0.); padding_bottom (Px 0.) ];
        rule
          ~selector:(compound [ base_sel; Webkit_datetime_edit_minute_field ])
          [ padding_top (Px 0.); padding_bottom (Px 0.) ];
        rule
          ~selector:(compound [ base_sel; Webkit_datetime_edit_second_field ])
          [ padding_top (Px 0.); padding_bottom (Px 0.) ];
        rule
          ~selector:
            (compound [ base_sel; Webkit_datetime_edit_millisecond_field ])
          [ padding_top (Px 0.); padding_bottom (Px 0.) ];
        rule
          ~selector:(compound [ base_sel; Webkit_datetime_edit_meridiem_field ])
          [ padding_top (Px 0.); padding_bottom (Px 0.) ];
      ]
    in
    style ~rules:(Some rules) []

  let form_textarea =
    let open Css in
    let open Css.Selector in
    let base_sel = class_ "form-textarea" in

    let d_shadow, _ =
      Var.binding Effects.shadow_var
        (shadow ~h_offset:Zero ~v_offset:Zero ~color:(hex "#0000") ())
    in

    let rules =
      [
        rule ~selector:base_sel
          [
            appearance None;
            d_shadow;
            background_color (hex "#fff");
            border_width (Px 1.);
            border_color gray_500;
            border_radius (Px 0.);
            padding [ Rem 0.5; Rem 0.75 ];
            font_size (Rem 1.);
            line_height (Rem 1.5);
          ];
        rule ~selector:(compound [ base_sel; Focus ]) input_focus_decls;
        rule
          ~selector:(compound [ base_sel; Placeholder ])
          [ color gray_500; opacity 1. ];
      ]
    in
    style ~rules:(Some rules) []

  let form_select =
    let open Css in
    let open Css.Selector in
    let base_sel = class_ "form-select" in

    let d_shadow, _ =
      Var.binding Effects.shadow_var
        (shadow ~h_offset:Zero ~v_offset:Zero ~color:(hex "#0000") ())
    in

    let rules =
      [
        rule ~selector:base_sel
          [
            appearance None;
            d_shadow;
            background_color (hex "#fff");
            border_width (Px 1.);
            border_color gray_500;
            border_radius (Px 0.);
            padding [ Rem 0.5; Rem 0.75 ];
            font_size (Rem 1.);
            line_height (Rem 1.5);
          ];
        rule ~selector:(compound [ base_sel; Focus ]) input_focus_decls;
        rule ~selector:base_sel
          [
            background_image
              (Url
                 "data:image/svg+xml,%3csvg xmlns='http://www.w3.org/2000/svg' \
                  fill='none' viewBox='0 0 20 20'%3e%3cpath \
                  stroke='oklch(55.1%25 0.027 264.364)' stroke-linecap='round' \
                  stroke-linejoin='round' stroke-width='1.5' d='M6 8l4 4 \
                  4-4'/%3e%3c/svg%3e");
            background_position
              [ Edge_offset_axis ("right", Rem 0.5, "center") ];
            background_repeat No_repeat;
            background_size (Size (Em 1.5, Em 1.5));
            padding_right (Rem 2.5);
            print_color_adjust Exact;
          ];
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
          [
            background_image Initial;
            background_position [ Initial ];
            background_repeat Unset;
            background_size Initial;
            padding_right (Rem 0.75);
            print_color_adjust Unset;
          ];
      ]
    in
    style ~rules:(Some rules) []

  let form_checkbox =
    let open Css in
    let open Css.Selector in
    let base_sel = class_ "form-checkbox" in

    let d_shadow, _ =
      Var.binding Effects.shadow_var
        (shadow ~h_offset:Zero ~v_offset:Zero ~color:(hex "#0000") ())
    in

    let rules =
      [
        (* Base state - order matches Tailwind output *)
        rule ~selector:base_sel
          [
            appearance None;
            print_color_adjust Exact;
            vertical_align Middle;
            webkit_user_select None;
            user_select None;
            color blue_600;
            d_shadow;
            background_color (hex "#fff");
            background_origin Border_box;
            border_width (Px 1.);
            border_color gray_500;
            border_radius (Px 0.);
            flex_shrink 0.0;
            width (Rem 1.);
            height (Rem 1.);
            padding [ Px 0. ];
            display Inline_block;
          ];
        (* :focus state *)
        rule ~selector:(compound [ base_sel; Focus ]) checkbox_focus_decls;
        (* :checked state - order matches Tailwind output *)
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
            background_position [ XY (Pct 50., Pct 50.) ];
            background_repeat No_repeat;
            background_size (Size (Pct 100., Pct 100.));
            border_color (hex "#0000");
          ];
        (* :checked:hover, :checked:focus - combined selector *)
        rule
          ~selector:
            (Selector.list
               [
                 compound [ base_sel; Checked; Hover ];
                 compound [ base_sel; Checked; Focus ];
               ])
          [ border_color (hex "#0000"); background_color Current ];
        (* @media (forced-colors:active) for checked *)
        media ~condition:"(forced-colors:active)"
          [
            rule ~selector:(compound [ base_sel; Checked ]) [ appearance Auto ];
          ];
        (* :indeterminate state *)
        rule
          ~selector:(compound [ base_sel; Indeterminate ])
          [
            background_image
              (Url
                 "data:image/svg+xml,%3csvg xmlns='http://www.w3.org/2000/svg' \
                  fill='none' viewBox='0 0 16 16'%3e%3cpath stroke='white' \
                  stroke-linecap='round' stroke-linejoin='round' \
                  stroke-width='2' d='M4 8h8'/%3e%3c/svg%3e");
            border_color (hex "#0000");
            background_color Current;
            background_size (Size (Pct 100., Pct 100.));
            background_position [ XY (Pct 50., Pct 50.) ];
            background_repeat No_repeat;
          ];
        (* :indeterminate:hover, :indeterminate:focus - combined selector *)
        rule
          ~selector:
            (Selector.list
               [
                 compound [ base_sel; Indeterminate; Hover ];
                 compound [ base_sel; Indeterminate; Focus ];
               ])
          [ background_color Current; border_color (hex "#0000") ];
        (* @media (forced-colors:active) for indeterminate *)
        media ~condition:"(forced-colors:active)"
          [
            rule
              ~selector:(compound [ base_sel; Indeterminate ])
              [ appearance Auto ];
          ];
      ]
    in
    style ~rules:(Some rules) []

  let form_radio =
    let open Css in
    let open Css.Selector in
    let base_sel = class_ "form-radio" in

    let d_shadow, _ =
      Var.binding Effects.shadow_var
        (shadow ~h_offset:Zero ~v_offset:Zero ~color:(hex "#0000") ())
    in

    let rules =
      [
        (* Base state - order matches Tailwind output *)
        rule ~selector:base_sel
          [
            appearance None;
            print_color_adjust Exact;
            vertical_align Middle;
            webkit_user_select None;
            user_select None;
            color blue_600;
            d_shadow;
            background_color (hex "#fff");
            background_origin Border_box;
            border_width (Px 1.);
            border_color gray_500;
            border_radius (Pct 100.0);
            flex_shrink 0.0;
            width (Rem 1.);
            height (Rem 1.);
            padding [ Px 0. ];
            display Inline_block;
          ];
        (* :focus state *)
        rule ~selector:(compound [ base_sel; Focus ]) checkbox_focus_decls;
        (* :checked state - order matches Tailwind output *)
        rule
          ~selector:(compound [ base_sel; Checked ])
          [
            background_color Current;
            background_image
              (Url
                 "data:image/svg+xml,%3csvg viewBox='0 0 16 16' fill='white' \
                  xmlns='http://www.w3.org/2000/svg'%3e%3ccircle cx='8' cy='8' \
                  r='3'/%3e%3c/svg%3e");
            background_position [ XY (Pct 50., Pct 50.) ];
            background_repeat No_repeat;
            background_size (Size (Pct 100., Pct 100.));
            border_color (hex "#0000");
          ];
        (* :checked:hover, :checked:focus - combined selector *)
        rule
          ~selector:
            (Selector.list
               [
                 compound [ base_sel; Checked; Hover ];
                 compound [ base_sel; Checked; Focus ];
               ])
          [ border_color (hex "#0000"); background_color Current ];
        (* @media (forced-colors:active) for checked *)
        media ~condition:"(forced-colors:active)"
          [
            rule ~selector:(compound [ base_sel; Checked ]) [ appearance Auto ];
          ];
      ]
    in
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
