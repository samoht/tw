(** Preflight and reset rules *)

open Css
open Selector

(* Base element selectors *)
let abbr = element "abbr"
let button = element "button"
let input = element "input"
let select = element "select"
let optgroup = element "optgroup"
let option = element "option"

(* Attribute selectors *)
let title = attribute "title" Presence
let multiple = attribute "multiple" Presence
let size = attribute "size" Presence
let type_button = attribute "type" (Exact "button")
let type_reset = attribute "type" (Exact "reset")
let type_submit = attribute "type" (Exact "submit")
let hidden = attribute "hidden" Presence
let hidden_until_found = attribute "hidden" (Exact "until-found")

(* Complex selectors *)
let abbr_with_title = abbr && where [ title ]
let select_is_multiple_size = select && fun_ "is" [ multiple; size ]
let input_button_types = input && where [ type_button; type_reset; type_submit ]
let hidden_not_until_found = hidden && where [ not [ hidden_until_found ] ]

(** Box model resets *)
let box_resets () =
  let open Selector in
  [
    rule
      ~selector:
        (list
           [
             universal;
             pseudo_class "after";
             pseudo_class "before";
             pseudo_element "backdrop";
           ])
      [ box_sizing Border_box; border "0 solid"; margin Zero; padding Zero ];
    rule
      ~selector:(pseudo_element "file-selector-button")
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
    rule
      ~selector:Selector.(list [ element "html"; pseudo_class "host" ])
      [
        webkit_text_size_adjust "100%";
        tab_size 4;
        line_height (Number 1.5);
        font_family [ Var Typography.default_font_family_var ];
        font_feature_settings (Var font_feature_var);
        font_variation_settings (Var font_variation_var);
        webkit_tap_highlight_color Transparent;
      ];
  ]

(** Structural elements *)
let structural_resets () =
  [
    rule ~selector:(Selector.element "hr")
      [ height Zero; color Inherit; border_top_width (Px 1) ];
    rule ~selector:abbr_with_title
      [
        webkit_text_decoration Underline_dotted;
        text_decoration Underline_dotted;
      ];
  ]

(** Typography resets *)
let typography_resets () =
  [
    rule
      ~selector:
        Selector.(
          list
            [
              element "h1";
              element "h2";
              element "h3";
              element "h4";
              element "h5";
              element "h6";
            ])
      [ font_size Inherit; font_weight Inherit ];
    rule ~selector:(Selector.element "a")
      [ color Inherit; webkit_text_decoration Inherit; text_decoration Inherit ];
    rule
      ~selector:Selector.(list [ element "b"; element "strong" ])
      [ font_weight Bolder ];
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
    rule
      ~selector:
        Selector.(
          list [ element "code"; element "kbd"; element "samp"; element "pre" ])
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
    rule ~selector:(Selector.element "small") [ font_size (Pct 80.0) ];
    rule
      ~selector:Selector.(list [ element "sub"; element "sup" ])
      [
        vertical_align Baseline;
        font_size (Pct 75.0);
        line_height (Number 0.);
        position Relative;
      ];
    rule ~selector:(Selector.element "sub") [ bottom (Em (-0.25)) ];
    rule ~selector:(Selector.element "sup") [ top (Em (-0.5)) ];
  ]

(** Table resets *)
let table_resets () =
  [
    rule ~selector:(Selector.element "table")
      [ text_indent Zero; border_color Inherit; border_collapse Collapse ];
  ]

(** Interactive elements *)
let interactive_resets () =
  [
    rule ~selector:(Selector.pseudo_class "-moz-focusring") [ outline "auto" ];
    rule ~selector:(Selector.element "progress") [ vertical_align Baseline ];
    rule ~selector:(Selector.element "summary") [ display List_item ];
  ]

(** List resets *)
let list_resets () =
  [
    rule
      ~selector:Selector.(list [ element "ol"; element "ul"; element "menu" ])
      [ list_style "none" ];
  ]

(** Media resets *)
let media_resets () =
  [
    rule
      ~selector:
        Selector.(
          list
            [
              element "img";
              element "svg";
              element "video";
              element "canvas";
              element "audio";
              element "iframe";
              element "embed";
              element "object";
            ])
      [ vertical_align Middle; display Block ];
    rule
      ~selector:Selector.(list [ element "img"; element "video" ])
      [ max_width (Pct 100.0); height Auto ];
  ]

(** Form control resets *)
let form_control_resets () =
  [
    rule
      ~selector:
        Selector.(
          list
            [
              element "button";
              element "input";
              element "select";
              element "optgroup";
              element "textarea";
            ])
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
    rule
      ~selector:(Selector.pseudo_element "file-selector-button")
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
    rule
      ~selector:(where [ select_is_multiple_size ] ++ optgroup)
      [ font_weight Bolder ];
    rule
      ~selector:(where [ select_is_multiple_size ] ++ optgroup ++ option)
      [ padding_inline_start (Px 20) ];
    rule
      ~selector:(Selector.pseudo_element "file-selector-button")
      [ margin_inline_end (Px 4) ];
  ]

(** Form placeholder and textarea resets *)
let form_misc_resets () =
  [
    rule ~selector:(Selector.pseudo_element "placeholder") [ opacity 1.0 ];
    rule ~selector:(Selector.element "textarea") [ resize Vertical ];
  ]

(** Webkit-specific form resets *)
let webkit_form_resets () =
  [
    rule
      ~selector:(Selector.pseudo_element "-webkit-search-decoration")
      [ webkit_appearance None ];
    rule
      ~selector:(Selector.pseudo_element "-webkit-date-and-time-value")
      [ min_height (Lh 1.0); text_align Inherit ];
    rule
      ~selector:(Selector.pseudo_element "-webkit-datetime-edit")
      [ display Inline_flex ];
    rule
      ~selector:(Selector.pseudo_element "-webkit-datetime-edit-fields-wrapper")
      [ padding Zero ];
    rule
      ~selector:(Selector.pseudo_element "-webkit-datetime-edit")
      [ padding_block Zero ];
    rule
      ~selector:(Selector.pseudo_element "-webkit-datetime-edit-year-field")
      [ padding_block Zero ];
    rule
      ~selector:(Selector.pseudo_element "-webkit-datetime-edit-month-field")
      [ padding_block Zero ];
    rule
      ~selector:(Selector.pseudo_element "-webkit-datetime-edit-day-field")
      [ padding_block Zero ];
    rule
      ~selector:(Selector.pseudo_element "-webkit-datetime-edit-hour-field")
      [ padding_block Zero ];
    rule
      ~selector:(Selector.pseudo_element "-webkit-datetime-edit-minute-field")
      [ padding_block Zero ];
    rule
      ~selector:(Selector.pseudo_element "-webkit-datetime-edit-second-field")
      [ padding_block Zero ];
    rule
      ~selector:
        (Selector.pseudo_element "-webkit-datetime-edit-millisecond-field")
      [ padding_block Zero ];
    rule
      ~selector:(Selector.pseudo_element "-webkit-datetime-edit-meridiem-field")
      [ padding_block Zero ];
  ]

(** Firefox-specific form resets *)
let firefox_form_resets () =
  [
    rule ~selector:(Selector.pseudo_class "-moz-ui-invalid") [ box_shadow None ];
  ]

(** Buttons need specific styles *)
let button_specific_resets () =
  [
    (* Complex selector - temporary workaround *)
    rule
      ~selector:Selector.(list [ button; input_button_types ])
      [ appearance Button ];
  ]

(** Button appearance resets *)
let button_resets () =
  [
    rule
      ~selector:(Selector.pseudo_element "file-selector-button")
      [ appearance Button ];
    rule
      ~selector:(Selector.pseudo_element "-webkit-inner-spin-button")
      [ height Auto ];
    rule
      ~selector:(Selector.pseudo_element "-webkit-outer-spin-button")
      [ height Auto ];
  ]

(** Hidden elements *)
let hidden_resets () =
  [ rule ~selector:hidden_not_until_found [ important (display None) ] ]

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
