(** Form element utilities - matching Tailwind v4's [\@tailwindcss/forms]
    plugin.

    These utilities use the ring/shadow variable system from Effects for focus
    states, ensuring proper type-safety and variable composition.

    Form utilities are split into two priority groups to match Tailwind's
    output:
    - checkbox, radio, input: priority 3 (before layout at 4)
    - select, textarea: priority 7 (after sizing at 6) *)

(* Shared colors and focus ring helpers *)
let blue_600 = Css.oklch 54.6 0.245 262.881
let gray_500 = Css.oklch 55.1 0.027 264.364

let focus_ring_decls ~offset_width ~ring_width_px =
  let open Css in
  let d_ring_inset, _ =
    Var.binding Effects.ring_inset_var "var(--tw-empty, )"
  in
  let d_offset_width, _ =
    Var.binding Effects.ring_offset_width_var offset_width
  in
  let d_offset_color, _ =
    Var.binding Effects.ring_offset_color_var (hex "#fff")
  in
  let d_ring_color, _ = Var.binding Effects.ring_color_var blue_600 in
  let offset_shadow_spread : length = Var (var_ref "tw-ring-offset-width") in
  let offset_shadow_color : color = Var (var_ref "tw-ring-offset-color") in
  let ring_offset_shadow_value =
    shadow ~inset:false ~inset_var:"tw-ring-inset" ~h_offset:Zero ~v_offset:Zero
      ~blur:Zero ~spread:offset_shadow_spread ~color:offset_shadow_color ()
  in
  let d_ring_offset_shadow, _ =
    Var.binding Effects.ring_offset_shadow_var ring_offset_shadow_value
  in
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
    shadow ~inset:false ~inset_var:"tw-ring-inset" ~h_offset:Zero ~v_offset:Zero
      ~blur:Zero ~spread:ring_shadow_spread ~color:ring_shadow_color ()
  in
  let d_ring_shadow, _ =
    Var.binding Effects.ring_shadow_var ring_shadow_value
  in
  let v_ring_offset = Var.reference Effects.ring_offset_shadow_var in
  let v_ring = Var.reference Effects.ring_shadow_var in
  let v_shadow = Var.reference Effects.shadow_var in
  let box_shadow_vars : Css.shadow list =
    [ Var v_ring_offset; Var v_ring; Var v_shadow ]
  in
  (* Note: does NOT include outline - that goes last, after optional
     border_color *)
  [
    outline_offset (Px 2.);
    d_ring_inset;
    d_offset_width;
    d_offset_color;
    d_ring_color;
    d_ring_offset_shadow;
    d_ring_shadow;
    box_shadows box_shadow_vars;
  ]

let input_focus_decls =
  let open Css in
  focus_ring_decls ~offset_width:(Px 0.) ~ring_width_px:1
  @ [
      border_color blue_600;
      outline
        (Shorthand
           {
             width = Some (Px 2.);
             style = Some Solid;
             color = Some (hex "#0000");
           });
    ]

let checkbox_focus_decls =
  let open Css in
  focus_ring_decls ~offset_width:(Px 2.) ~ring_width_px:2
  @ [
      outline
        (Shorthand
           {
             width = Some (Px 2.);
             style = Some Solid;
             color = Some (hex "#0000");
           });
    ]

(* Handler for checkbox/radio/input - priority 3 *)
module Handler = struct
  open Style

  type t = Form_input | Form_checkbox | Form_radio
  type Utility.base += Self of t

  let name = "forms"
  let priority = 3

  let form_input =
    let open Css in
    let open Css.Selector in
    let base_sel = class_ "form-input" in
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
          [ color gray_500; opacity (Opacity_number 1.) ];
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
        rule ~selector:(compound [ base_sel; Focus ]) checkbox_focus_decls;
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
        media ~condition:(Media.Forced_colors `Active)
          [
            rule ~selector:(compound [ base_sel; Checked ]) [ appearance Auto ];
          ];
        rule
          ~selector:
            (Selector.list
               [
                 compound [ base_sel; Checked; Hover ];
                 compound [ base_sel; Checked; Focus ];
               ])
          [ background_color Current; border_color (hex "#0000") ];
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
            background_position [ XY (Pct 50., Pct 50.) ];
            background_repeat No_repeat;
            background_size (Size (Pct 100., Pct 100.));
            border_color (hex "#0000");
          ];
        media ~condition:(Media.Forced_colors `Active)
          [
            rule
              ~selector:(compound [ base_sel; Indeterminate ])
              [ appearance Auto ];
          ];
        rule
          ~selector:
            (Selector.list
               [
                 compound [ base_sel; Indeterminate; Hover ];
                 compound [ base_sel; Indeterminate; Focus ];
               ])
          [ background_color Current; border_color (hex "#0000") ];
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
        rule ~selector:(compound [ base_sel; Focus ]) checkbox_focus_decls;
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
        media ~condition:(Media.Forced_colors `Active)
          [
            rule ~selector:(compound [ base_sel; Checked ]) [ appearance Auto ];
          ];
        rule
          ~selector:
            (Selector.list
               [
                 compound [ base_sel; Checked; Hover ];
                 compound [ base_sel; Checked; Focus ];
               ])
          [ background_color Current; border_color (hex "#0000") ];
      ]
    in
    style ~rules:(Some rules) []

  let to_style = function
    | Form_input -> form_input
    | Form_checkbox -> form_checkbox
    | Form_radio -> form_radio

  let suborder = function
    | Form_checkbox -> 0
    | Form_radio -> 1
    | Form_input -> 2

  let of_class = function
    | "form-input" -> Ok Form_input
    | "form-checkbox" -> Ok Form_checkbox
    | "form-radio" -> Ok Form_radio
    | _ -> Error (`Msg "Not a form utility")

  let to_class = function
    | Form_input -> "form-input"
    | Form_checkbox -> "form-checkbox"
    | Form_radio -> "form-radio"
end

(* Handler for select/textarea - priority 7 *)
module Select = struct
  open Style

  type t = Form_select | Form_textarea
  type Utility.base += Self of t

  let name = "forms_select"
  let priority = 8 (* After sizing (6), flex_props (7) *)

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
          [ color gray_500; opacity (Opacity_number 1.) ];
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
            print_color_adjust Exact;
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
                         Attribute (None, Regular "size", Presence, None);
                         Not
                           [ Attribute (None, Regular "size", Exact "1", None) ];
                       ];
                   ];
               ])
          [
            background_image Initial;
            background_position [ Initial ];
            background_repeat Unset;
            background_size Initial;
            print_color_adjust Unset;
            padding_right (Rem 0.75);
          ];
      ]
    in
    style ~rules:(Some rules) []

  let to_style = function
    | Form_select -> form_select
    | Form_textarea -> form_textarea

  let suborder = function Form_select -> 0 | Form_textarea -> 1

  let of_class = function
    | "form-select" -> Ok Form_select
    | "form-textarea" -> Ok Form_textarea
    | _ -> Error (`Msg "Not a form select utility")

  let to_class = function
    | Form_select -> "form-select"
    | Form_textarea -> "form-textarea"
end

(* Register both handlers *)
let () = Utility.register (module Handler)
let () = Utility.register (module Select)

(* Public API *)
let form_input = Utility.base (Handler.Self Handler.Form_input)
let form_checkbox = Utility.base (Handler.Self Handler.Form_checkbox)
let form_radio = Utility.base (Handler.Self Handler.Form_radio)
let form_select = Utility.base (Select.Self Select.Form_select)
let form_textarea = Utility.base (Select.Self Select.Form_textarea)

(* ======================================================================== Base
   layer stylesheet - rules that apply to native HTML form elements when the
   @tailwindcss/forms plugin is used
   ======================================================================== *)

(** Type selectors for form elements - wrapped with input:where() to match
    Tailwind v4 *)
let type_text =
  Css.Selector.(element "input" && where [ attribute "type" (Exact "text") ])

let type_email =
  Css.Selector.(element "input" && where [ attribute "type" (Exact "email") ])

let type_url =
  Css.Selector.(element "input" && where [ attribute "type" (Exact "url") ])

let type_password =
  Css.Selector.(
    element "input" && where [ attribute "type" (Exact "password") ])

let type_number =
  Css.Selector.(element "input" && where [ attribute "type" (Exact "number") ])

let type_date =
  Css.Selector.(element "input" && where [ attribute "type" (Exact "date") ])

let type_datetime_local =
  Css.Selector.(
    element "input" && where [ attribute "type" (Exact "datetime-local") ])

let type_month =
  Css.Selector.(element "input" && where [ attribute "type" (Exact "month") ])

let type_search =
  Css.Selector.(element "input" && where [ attribute "type" (Exact "search") ])

let type_tel =
  Css.Selector.(element "input" && where [ attribute "type" (Exact "tel") ])

let type_time =
  Css.Selector.(element "input" && where [ attribute "type" (Exact "time") ])

let type_week =
  Css.Selector.(element "input" && where [ attribute "type" (Exact "week") ])

let type_checkbox =
  Css.Selector.(
    element "input" && where [ attribute "type" (Exact "checkbox") ])

let type_radio =
  Css.Selector.(element "input" && where [ attribute "type" (Exact "radio") ])

let type_file =
  Css.Selector.(element "input" && where [ attribute "type" (Exact "file") ])

let input_no_type =
  Css.Selector.(element "input" && where [ not [ attribute "type" Presence ] ])

let multiple =
  Css.Selector.(element "select" && where [ attribute "multiple" Presence ])

let size = Css.Selector.(attribute "size" Presence)
let size_1 = Css.Selector.attribute "size" (Exact "1")

(** Combined text input selector - matches all text-like inputs *)
let text_inputs =
  Css.Selector.list
    [
      type_text;
      input_no_type;
      type_email;
      type_url;
      type_password;
      type_number;
      type_date;
      type_datetime_local;
      type_month;
      type_search;
      type_tel;
      type_time;
      type_week;
      multiple;
      Css.Selector.element "textarea";
      Css.Selector.element "select";
    ]

(** Focus ring declarations for text inputs (1px ring, no offset) *)
let text_input_focus_ring_decls =
  let open Css in
  focus_ring_decls ~offset_width:(Px 0.) ~ring_width_px:1
  @ [
      border_color blue_600;
      outline
        (Shorthand
           {
             width = Some (Px 2.);
             style = Some Solid;
             color = Some (hex "#0000");
           });
    ]

(** Focus ring declarations for checkboxes/radios (2px ring, 2px offset) *)
let checkbox_focus_ring_decls =
  let open Css in
  focus_ring_decls ~offset_width:(Px 2.) ~ring_width_px:2
  @ [
      outline
        (Shorthand
           {
             width = Some (Px 2.);
             style = Some Solid;
             color = Some (hex "#0000");
           });
    ]

(** Text inputs base styles *)
let text_inputs_base () =
  let open Css in
  let d_shadow, _ =
    Var.binding Effects.shadow_var
      (shadow ~h_offset:Zero ~v_offset:Zero ~color:(hex "#0000") ())
  in
  let text_input_items =
    [
      type_text;
      input_no_type;
      type_email;
      type_url;
      type_password;
      type_number;
      type_date;
      type_datetime_local;
      type_month;
      type_search;
      type_tel;
      type_time;
      type_week;
      multiple;
      Selector.element "textarea";
      Selector.element "select";
    ]
  in
  [
    rule ~selector:text_inputs
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
    rule
      ~selector:Selector.(is_ text_input_items && Focus)
      text_input_focus_ring_decls;
    rule
      ~selector:
        Selector.(
          list
            [
              element "input" && Placeholder; element "textarea" && Placeholder;
            ])
      [ color gray_500; opacity (Opacity_number 1.) ];
    (* Webkit datetime edit rules - forms plugin specific. These provide
       additional styling beyond preflight *)
    rule ~selector:Selector.Webkit_datetime_edit_fields_wrapper
      [ padding [ Px 0. ] ];
    rule ~selector:Selector.Webkit_date_and_time_value [ min_height (Em 1.5) ];
    rule ~selector:Selector.Webkit_date_and_time_value [ text_align Inherit ];
    rule ~selector:Selector.Webkit_datetime_edit [ display Inline_flex ];
    rule ~selector:Selector.Webkit_datetime_edit
      [ padding_top (Px 0.); padding_bottom (Px 0.) ];
    rule ~selector:Selector.Webkit_datetime_edit_year_field
      [ padding_top (Px 0.); padding_bottom (Px 0.) ];
    rule ~selector:Selector.Webkit_datetime_edit_month_field
      [ padding_top (Px 0.); padding_bottom (Px 0.) ];
    rule ~selector:Selector.Webkit_datetime_edit_day_field
      [ padding_top (Px 0.); padding_bottom (Px 0.) ];
    rule ~selector:Selector.Webkit_datetime_edit_hour_field
      [ padding_top (Px 0.); padding_bottom (Px 0.) ];
    rule ~selector:Selector.Webkit_datetime_edit_minute_field
      [ padding_top (Px 0.); padding_bottom (Px 0.) ];
    rule ~selector:Selector.Webkit_datetime_edit_second_field
      [ padding_top (Px 0.); padding_bottom (Px 0.) ];
    rule ~selector:Selector.Webkit_datetime_edit_millisecond_field
      [ padding_top (Px 0.); padding_bottom (Px 0.) ];
    rule ~selector:Selector.Webkit_datetime_edit_meridiem_field
      [ padding_top (Px 0.); padding_bottom (Px 0.) ];
  ]

(** Select dropdown base styles *)
let select_base () =
  let open Css in
  [
    rule
      ~selector:(Selector.element "select")
      [
        print_color_adjust Exact;
        background_image
          (Url
             "data:image/svg+xml,%3csvg xmlns='http://www.w3.org/2000/svg' \
              fill='none' viewBox='0 0 20 20'%3e%3cpath stroke='oklch(55.1%25 \
              0.027 264.364)' stroke-linecap='round' stroke-linejoin='round' \
              stroke-width='1.5' d='M6 8l4 4 4-4'/%3e%3c/svg%3e");
        background_position [ Edge_offset_axis ("right", Rem 0.5, "center") ];
        background_repeat No_repeat;
        background_size (Size (Em 1.5, Em 1.5));
        padding_right (Rem 2.5);
      ];
    rule
      ~selector:
        Selector.(
          list
            [
              multiple;
              Selector.element "select" && where [ size && not [ size_1 ] ];
            ])
      [
        background_image Initial;
        background_position [ Initial ];
        background_repeat Unset;
        background_size Initial;
        print_color_adjust Unset;
        padding_right (Rem 0.75);
      ];
  ]

(** Checkbox and radio base styles - order matches Tailwind exactly with
    interleaved forced-colors media queries *)
let checkbox_radio_base () =
  let open Css in
  let d_shadow, _ =
    Var.binding Effects.shadow_var
      (shadow ~h_offset:Zero ~v_offset:Zero ~color:(hex "#0000") ())
  in
  [
    (* 1. Base styles for both *)
    rule
      ~selector:Selector.(list [ type_checkbox; type_radio ])
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
        flex_shrink 0.;
        width (Rem 1.);
        height (Rem 1.);
        padding [ Px 0. ];
        display Inline_block;
      ];
    (* 2. Checkbox border-radius: 0 *)
    rule ~selector:type_checkbox [ border_radius (Px 0.) ];
    (* 3. Radio border-radius: 100% *)
    rule ~selector:type_radio [ border_radius (Pct 100.) ];
    (* 4. Focus ring for both *)
    rule
      ~selector:Selector.(list [ type_checkbox && Focus; type_radio && Focus ])
      checkbox_focus_ring_decls;
    (* 5. Checked state for both - common properties *)
    rule
      ~selector:
        Selector.(list [ type_checkbox && Checked; type_radio && Checked ])
      [
        background_color Current;
        background_position [ XY (Pct 50., Pct 50.) ];
        background_repeat No_repeat;
        background_size (Size (Pct 100., Pct 100.));
        border_color (hex "#0000");
      ];
    (* 6. checkbox:checked background-image (checkmark) *)
    rule
      ~selector:Selector.(type_checkbox && Checked)
      [
        background_image
          (Url
             "data:image/svg+xml,%3csvg viewBox='0 0 16 16' fill='white' \
              xmlns='http://www.w3.org/2000/svg'%3e%3cpath d='M12.207 4.793a1 \
              1 0 010 1.414l-5 5a1 1 0 01-1.414 0l-2-2a1 1 0 011.414-1.414L6.5 \
              9.086l4.293-4.293a1 1 0 011.414 0z'/%3e%3c/svg%3e");
      ];
    (* 7. @media (forced-colors:active) checkbox:checked *)
    media ~condition:(Media.Forced_colors `Active)
      [ rule ~selector:Selector.(type_checkbox && Checked) [ appearance Auto ] ];
    (* 8. radio:checked background-image (circle) *)
    rule
      ~selector:Selector.(type_radio && Checked)
      [
        background_image
          (Url
             "data:image/svg+xml,%3csvg viewBox='0 0 16 16' fill='white' \
              xmlns='http://www.w3.org/2000/svg'%3e%3ccircle cx='8' cy='8' \
              r='3'/%3e%3c/svg%3e");
      ];
    (* 9. @media (forced-colors:active) radio:checked *)
    media ~condition:(Media.Forced_colors `Active)
      [ rule ~selector:Selector.(type_radio && Checked) [ appearance Auto ] ];
    (* 10. Checked hover/focus for both *)
    rule
      ~selector:
        Selector.(
          list
            [
              type_checkbox && Checked && Hover;
              type_checkbox && Checked && Focus;
              type_radio && Checked && Hover;
              type_radio && Checked && Focus;
            ])
      [ background_color Current; border_color (hex "#0000") ];
    (* 11. checkbox:indeterminate - all properties *)
    rule
      ~selector:Selector.(type_checkbox && Indeterminate)
      [
        background_color Current;
        background_image
          (Url
             "data:image/svg+xml,%3csvg xmlns='http://www.w3.org/2000/svg' \
              fill='none' viewBox='0 0 16 16'%3e%3cpath stroke='white' \
              stroke-linecap='round' stroke-linejoin='round' stroke-width='2' \
              d='M4 8h8'/%3e%3c/svg%3e");
        background_position [ XY (Pct 50., Pct 50.) ];
        background_repeat No_repeat;
        background_size (Size (Pct 100., Pct 100.));
        border_color (hex "#0000");
      ];
    (* 12. @media (forced-colors:active) checkbox:indeterminate *)
    media ~condition:(Media.Forced_colors `Active)
      [
        rule
          ~selector:Selector.(type_checkbox && Indeterminate)
          [ appearance Auto ];
      ];
    (* 13. Indeterminate hover/focus *)
    rule
      ~selector:
        Selector.(
          list
            [
              type_checkbox && Indeterminate && Hover;
              type_checkbox && Indeterminate && Focus;
            ])
      [ background_color Current; border_color (hex "#0000") ];
  ]

(** File input base styles *)
let file_input_base () =
  let open Css in
  [
    rule ~selector:type_file
      [
        background Unset;
        border_color Inherit;
        font_size Unset;
        line_height Inherit;
        border_width (Px 0.);
        border_radius (Px 0.);
        padding [ Px 0. ];
      ];
    rule
      ~selector:Selector.(type_file && Focus)
      [
        (* Dual outline for webkit fallback - both must be preserved *)
        outline
          (Shorthand
             {
               width = Some (Px 1.);
               style = Some Solid;
               color = Some (System ButtonText);
             });
        outline
          (Shorthand
             {
               width = Some (Px 1.);
               style = Some Auto;
               color = Some (System Webkit_focus_ring_color);
             });
      ];
  ]

(** Complete base layer stylesheet for forms plugin *)
let base_stylesheet () =
  Css.v
    (List.concat
       [
         text_inputs_base ();
         select_base ();
         checkbox_radio_base ();
         file_input_base ();
       ])
