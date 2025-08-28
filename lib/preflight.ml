(** Preflight and reset rules *)

open Css

(** Box model resets *)
let box_resets () =
  [
    rule ~selector:"*, :after, :before, ::backdrop"
      [ box_sizing Border_box; border "0 solid"; margin Zero; padding Zero ];
    rule ~selector:"::file-selector-button"
      [ box_sizing Border_box; border "0 solid"; margin Zero; padding Zero ];
  ]

(** HTML and body defaults *)
let root_resets () =
  let _, font_feature_var =
    Var.theme Var.Default_font_feature_settings Normal ~fallback:Normal
  in
  let _, font_variation_var =
    Var.theme Var.Default_font_variation_settings Normal ~fallback:Normal
  in
  [
    rule ~selector:"html, :host"
      [
        webkit_text_size_adjust "100%";
        tab_size 4;
        line_height (Num 1.5);
        font_family [ Var Typography.default_font_family_var ];
        font_feature_settings (Var font_feature_var);
        font_variation_settings (Var font_variation_var);
        webkit_tap_highlight_color Transparent;
      ];
  ]

(** Structural elements *)
let structural_resets () =
  [
    rule ~selector:"hr" [ height Zero; color Inherit; border_top_width (Px 1) ];
    rule ~selector:"abbr:where([title])"
      [
        webkit_text_decoration "underline dotted";
        text_decoration Underline_dotted;
      ];
  ]

(** Typography resets *)
let typography_resets () =
  [
    rule ~selector:"h1, h2, h3, h4, h5, h6"
      [ font_size Inherit; font_weight Inherit ];
    rule ~selector:"a"
      [
        color Inherit;
        webkit_text_decoration "inherit";
        webkit_text_decoration "inherit";
        webkit_text_decoration "inherit";
        text_decoration Inherit;
      ];
    rule ~selector:"b, strong" [ font_weight Bolder ];
  ]

(** Code and monospace resets *)
let code_resets () =
  (* Create font feature/variation variables with fallback for monospace *)
  let _, font_feature_var =
    Var.theme Var.Default_mono_font_feature_settings Normal ~fallback:Normal
  in
  let _, font_variation_var =
    Var.theme Var.Default_mono_font_variation_settings Normal ~fallback:Normal
  in
  [
    rule ~selector:"code, kbd, samp, pre"
      [
        font_family [ Var Typography.default_mono_font_family_var ];
        font_feature_settings (Var font_feature_var);
        font_variation_settings (Var font_variation_var);
        font_size (Em 1.0);
      ];
  ]

(** Text-level semantics *)
let text_level_resets () =
  [
    rule ~selector:"small" [ font_size (Pct 80.0) ];
    rule ~selector:"sub, sup"
      [
        vertical_align Baseline;
        font_size (Pct 75.0);
        line_height Zero;
        position Relative;
      ];
    rule ~selector:"sub" [ bottom (Em (-0.25)) ];
    rule ~selector:"sup" [ top (Em (-0.5)) ];
  ]

(** Table resets *)
let table_resets () =
  [
    rule ~selector:"table"
      [ text_indent Zero; border_color Inherit; border_collapse Collapse ];
  ]

(** Interactive elements *)
let interactive_resets () =
  [
    rule ~selector:":-moz-focusring" [ outline "auto" ];
    rule ~selector:"progress" [ vertical_align Baseline ];
    rule ~selector:"summary" [ display List_item ];
  ]

(** List resets *)
let list_resets () = [ rule ~selector:"ol, ul, menu" [ list_style "none" ] ]

(** Media resets *)
let media_resets () =
  [
    rule ~selector:"img, svg, video, canvas, audio, iframe, embed, object"
      [ vertical_align Middle; display Block ];
    rule ~selector:"img, video" [ max_width (Pct 100.0); height Auto ];
  ]

(** Form control resets *)
let form_control_resets () =
  [
    rule ~selector:"button, input, select, optgroup, textarea"
      [
        font "inherit";
        font_feature_settings Inherit;
        font_variation_settings Inherit;
        letter_spacing Inherit;
        color Inherit;
        opacity 1.0;
        background_color Transparent;
        border_radius Zero;
      ];
    rule ~selector:"::file-selector-button"
      [
        font "inherit";
        font_feature_settings Inherit;
        font_variation_settings Inherit;
        letter_spacing Inherit;
        color Inherit;
        opacity 1.0;
        background_color Transparent;
        border_radius Zero;
      ];
  ]

(** Select and option resets *)
let select_resets () =
  [
    rule ~selector:":where(select:is([multiple], [size])) optgroup"
      [ font_weight Bolder ];
    rule ~selector:":where(select:is([multiple], [size])) optgroup option"
      [ padding_inline_start (Px 20) ];
    rule ~selector:"::file-selector-button" [ margin_inline_end (Px 4) ];
  ]

(** Form placeholder and textarea resets *)
let form_misc_resets () =
  [
    rule ~selector:"::placeholder" [ opacity 1.0 ];
    rule ~selector:"textarea" [ resize Vertical ];
  ]

(** Webkit-specific form resets *)
let webkit_form_resets () =
  [
    rule ~selector:"::-webkit-search-decoration" [ webkit_appearance None ];
    rule ~selector:"::-webkit-date-and-time-value"
      [ min_height (Lh 1.0); text_align Inherit ];
    rule ~selector:"::-webkit-datetime-edit" [ display Inline_flex ];
    rule ~selector:"::-webkit-datetime-edit-fields-wrapper" [ padding Zero ];
    rule ~selector:"::-webkit-datetime-edit" [ padding_block Zero ];
    rule ~selector:"::-webkit-datetime-edit-year-field" [ padding_block Zero ];
    rule ~selector:"::-webkit-datetime-edit-month-field" [ padding_block Zero ];
    rule ~selector:"::-webkit-datetime-edit-day-field" [ padding_block Zero ];
    rule ~selector:"::-webkit-datetime-edit-hour-field" [ padding_block Zero ];
    rule ~selector:"::-webkit-datetime-edit-minute-field" [ padding_block Zero ];
    rule ~selector:"::-webkit-datetime-edit-second-field" [ padding_block Zero ];
    rule ~selector:"::-webkit-datetime-edit-millisecond-field"
      [ padding_block Zero ];
    rule ~selector:"::-webkit-datetime-edit-meridiem-field"
      [ padding_block Zero ];
  ]

(** Firefox-specific form resets *)
let firefox_form_resets () =
  [ rule ~selector:":-moz-ui-invalid" [ box_shadow None ] ]

(** Buttons need specific styles *)
let button_specific_resets () =
  [
    rule
      ~selector:
        "button, input:where([type=button], [type=reset], [type=submit])"
      [ appearance Button ];
  ]

(** Button appearance resets *)
let button_resets () =
  [
    rule ~selector:"::file-selector-button" [ appearance Button ];
    rule ~selector:"::-webkit-inner-spin-button" [ height Auto ];
    rule ~selector:"::-webkit-outer-spin-button" [ height Auto ];
  ]

(** Hidden elements *)
let hidden_resets () =
  [
    rule ~selector:"[hidden]:where(:not([hidden=until-found]))"
      [ important (display None) ];
  ]

let stylesheet () =
  List.concat
    [
      box_resets ();
      root_resets ();
      structural_resets ();
      typography_resets ();
      code_resets ();
      text_level_resets ();
      table_resets ();
      interactive_resets ();
      list_resets ();
      media_resets ();
      form_control_resets ();
      select_resets ();
      form_misc_resets ();
      webkit_form_resets ();
      firefox_form_resets ();
      button_specific_resets ();
      button_resets ();
      hidden_resets ();
    ]
