(** Preflight and reset rules *)

open Css

(* Extend variable kinds for preflight-specific variables *)
type _ Var.kind +=
  | (* Default font settings for preflight *)
      Default_font_family :
      Css.font_family list Var.kind
  | Default_mono_font_family : Css.font_family list Var.kind
  | Default_font_feature_settings : Css.font_feature_settings Var.kind
  | Default_font_variation_settings : Css.font_variation_settings Var.kind
  | Default_mono_font_feature_settings : Css.font_feature_settings Var.kind
  | Default_mono_font_variation_settings : Css.font_variation_settings Var.kind

(* Base element selectors *)
let abbr = Selector.element "abbr"
let button = Selector.element "button"
let input = Selector.element "input"
let select = Selector.element "select"
let optgroup = Selector.element "optgroup"
let option = Selector.element "option"

(* Attribute selectors *)
let title = Selector.attribute "title" Presence
let multiple = Selector.attribute "multiple" Presence
let size = Selector.attribute "size" Presence
let type_button = Selector.attribute "type" (Exact "button")
let type_reset = Selector.attribute "type" (Exact "reset")
let type_submit = Selector.attribute "type" (Exact "submit")
let hidden = Selector.attribute "hidden" Presence
let hidden_until_found = Selector.attribute "hidden" (Exact "until-found")

(* Complex selectors *)
let abbr_with_title = Selector.(abbr && where [ title ])
let select_is_multiple_size = Selector.(select && is_ [ multiple; size ])

let input_button_types =
  Selector.(input && where [ type_button; type_reset; type_submit ])

let hidden_not_until_found =
  Selector.(hidden && where [ not [ hidden_until_found ] ])

(** Box model resets *)
let box_resets () =
  let open Selector in
  [
    rule
      ~selector:(Selector.list [ Selector.universal; After; Before; Backdrop ])
      [
        box_sizing Border_box;
        border ~width:Zero ~style:Solid ();
        margin [ Zero ];
        padding [ Zero ];
      ];
    rule ~selector:File_selector_button
      [
        box_sizing Border_box;
        border ~width:Zero ~style:Solid ();
        margin [ Zero ];
        padding [ Zero ];
      ];
  ]

(* Create theme variables for font settings *)
let font_feature_var =
  Var.create Default_font_feature_settings "default-font-feature-settings"
    ~layer:Theme ~order:7 ~fallback:Normal

let font_variation_var =
  Var.create Default_font_variation_settings "default-font-variation-settings"
    ~layer:Theme ~order:8 ~fallback:Normal

(** HTML and body defaults *)
let root_resets () =
  [
    rule
      ~selector:Selector.(list [ element "html"; host () ])
      [
        webkit_text_size_adjust (Pct 100.);
        tab_size 4;
        line_height (Num 1.5);
        (* Use default-font-family with full font stack as fallback *)
        font_family
          (Css.Var
             (var_ref
                ~fallback:
                  (Css.Fallback
                     (Css.List
                        [
                          Css.Ui_sans_serif;
                          Css.System_ui;
                          Css.Sans_serif;
                          Css.Apple_color_emoji;
                          Css.Segoe_ui_emoji;
                          Css.Segoe_ui_symbol;
                          Css.Noto_color_emoji;
                        ]
                       : Css.font_family))
                "default-font-family"));
        font_feature_settings (Var (Var.use font_feature_var));
        font_variation_settings (Var (Var.use font_variation_var));
        webkit_tap_highlight_color Transparent;
      ];
  ]

(** Structural elements *)
let structural_resets () =
  [
    rule ~selector:(Selector.element "hr")
      [ height Zero; color Inherit; border_top_width (Px 1.) ];
    rule ~selector:abbr_with_title
      [
        webkit_text_decoration
          (Shorthand
             {
               lines = [ Underline ];
               style = Some Dotted;
               color = None;
               thickness = None;
             });
        text_decoration
          (Shorthand
             {
               lines = [ Underline ];
               style = Some Dotted;
               color = None;
               thickness = None;
             });
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
              Selector.element "h1";
              Selector.element "h2";
              Selector.element "h3";
              Selector.element "h4";
              Selector.element "h5";
              Selector.element "h6";
            ])
      [ font_size Inherit; font_weight Inherit ];
    rule ~selector:(Selector.element "a")
      [ color Inherit; webkit_text_decoration Inherit; text_decoration Inherit ];
    rule
      ~selector:
        Selector.(list [ Selector.element "b"; Selector.element "strong" ])
      [ font_weight Bolder ];
  ]

(** Code and monospace resets *)
let code_resets () =
  (* Create font feature/variation variables with fallback for monospace *)
  let font_feature_var =
    Var.create Default_mono_font_feature_settings
      "default-mono-font-feature-settings" ~layer:Theme ~order:9
      ~fallback:Normal
  in
  let font_variation_var =
    Var.create Default_mono_font_variation_settings
      "default-mono-font-variation-settings" ~layer:Theme ~order:10
      ~fallback:Normal
  in
  [
    rule
      ~selector:
        Selector.(
          list [ element "code"; element "kbd"; element "samp"; element "pre" ])
      [
        (* Use default-mono-font-family with full monospace font stack as
           fallback *)
        font_family
          (Css.Var
             (var_ref
                ~fallback:
                  (Css.Fallback
                     (Css.List
                        [
                          Css.Ui_monospace;
                          Css.SFMono_regular;
                          Css.Menlo;
                          Css.Monaco;
                          Css.Consolas;
                          Css.Liberation_mono;
                          Css.Courier_new;
                          Css.Monospace;
                        ]
                       : Css.font_family))
                "default-mono-font-family"));
        font_feature_settings (Var (Var.use font_feature_var));
        font_variation_settings (Var (Var.use font_variation_var));
        font_size (Em 1.0);
      ];
  ]

(** Text-level semantics *)
let text_level_resets () =
  [
    rule ~selector:(Selector.element "small") [ font_size (Pct 80.0) ];
    rule
      ~selector:
        Selector.(list [ Selector.element "sub"; Selector.element "sup" ])
      [
        vertical_align Baseline;
        font_size (Pct 75.0);
        line_height (Num 0.);
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
    rule ~selector:Moz_focusring [ outline "auto" ];
    rule ~selector:(Selector.element "progress") [ vertical_align Baseline ];
    rule ~selector:(Selector.element "summary") [ display List_item ];
  ]

(** List resets *)
let list_resets () =
  [
    rule
      ~selector:
        Selector.(
          list
            [
              Selector.element "ol";
              Selector.element "ul";
              Selector.element "menu";
            ])
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
              Selector.element "img";
              Selector.element "svg";
              Selector.element "video";
              Selector.element "canvas";
              Selector.element "audio";
              Selector.element "iframe";
              Selector.element "embed";
              Selector.element "object";
            ])
      [ vertical_align Middle; display Block ];
    rule
      ~selector:
        Selector.(list [ Selector.element "img"; Selector.element "video" ])
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
              Selector.element "button";
              Selector.element "input";
              Selector.element "select";
              Selector.element "optgroup";
              Selector.element "textarea";
            ])
      [
        font "inherit";
        font_feature_settings Inherit;
        font_variation_settings Inherit;
        letter_spacing Inherit;
        color Inherit;
        opacity 1.0;
        background_color (hex "#0000");
        border_radius Zero;
      ];
    rule ~selector:File_selector_button
      [
        font "inherit";
        font_feature_settings Inherit;
        font_variation_settings Inherit;
        letter_spacing Inherit;
        color Inherit;
        opacity 1.0;
        background_color (hex "#0000");
        border_radius Zero;
      ];
  ]

(** Select and option resets *)
let select_resets () =
  [
    rule
      ~selector:Selector.(where [ select_is_multiple_size ] ++ optgroup)
      [ font_weight Bolder ];
    rule
      ~selector:
        Selector.(where [ select_is_multiple_size ] ++ optgroup ++ option)
      [ padding_inline_start (Px 20.) ];
    rule ~selector:File_selector_button [ margin_inline_end (Px 4.) ];
  ]

(** Form placeholder and textarea resets *)
let form_misc_resets () =
  [
    rule ~selector:Placeholder [ opacity 1.0 ];
    rule ~selector:(Selector.element "textarea") [ resize Vertical ];
  ]

(** Webkit-specific form resets *)
let webkit_form_resets () =
  [
    rule ~selector:(Selector.Pseudo_element "-webkit-search-decoration")
      [ webkit_appearance None ];
    rule ~selector:(Selector.Pseudo_element "-webkit-date-and-time-value")
      [ min_height (Lh 1.0); text_align Inherit ];
    rule ~selector:(Selector.Pseudo_element "-webkit-datetime-edit")
      [ display Inline_flex ];
    rule
      ~selector:(Selector.Pseudo_element "-webkit-datetime-edit-fields-wrapper")
      [ padding [ Zero ] ];
    rule ~selector:(Selector.Pseudo_element "-webkit-datetime-edit")
      [ padding_block Zero ];
    rule ~selector:(Selector.Pseudo_element "-webkit-datetime-edit-year-field")
      [ padding_block Zero ];
    rule ~selector:(Selector.Pseudo_element "-webkit-datetime-edit-month-field")
      [ padding_block Zero ];
    rule ~selector:(Selector.Pseudo_element "-webkit-datetime-edit-day-field")
      [ padding_block Zero ];
    rule ~selector:(Selector.Pseudo_element "-webkit-datetime-edit-hour-field")
      [ padding_block Zero ];
    rule
      ~selector:(Selector.Pseudo_element "-webkit-datetime-edit-minute-field")
      [ padding_block Zero ];
    rule
      ~selector:(Selector.Pseudo_element "-webkit-datetime-edit-second-field")
      [ padding_block Zero ];
    rule
      ~selector:
        (Selector.Pseudo_element "-webkit-datetime-edit-millisecond-field")
      [ padding_block Zero ];
    rule
      ~selector:(Selector.Pseudo_element "-webkit-datetime-edit-meridiem-field")
      [ padding_block Zero ];
  ]

(** Firefox-specific form resets *)
let firefox_form_resets () =
  [ rule ~selector:Moz_ui_invalid [ box_shadow None ] ]

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
    rule ~selector:File_selector_button [ appearance Button ];
    rule ~selector:(Selector.Pseudo_element "-webkit-inner-spin-button")
      [ height Auto ];
    rule ~selector:(Selector.Pseudo_element "-webkit-outer-spin-button")
      [ height Auto ];
  ]

(** Hidden elements *)
let hidden_resets () =
  [ rule ~selector:hidden_not_until_found [ important (display None) ] ]

let stylesheet ?placeholder_supports () =
  let base_rules =
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
  in

  match placeholder_supports with
  | None -> Css.v base_rules
  | Some supports ->
      (* Split the rules at placeholder and insert the @supports *)
      let rec split_after_placeholder acc = function
        | [] -> (List.rev acc, [])
        | h :: t -> (
            match Css.statement_selector h with
            | Some sel when Css.Selector.to_string sel = "::placeholder" ->
                (List.rev (h :: acc), t)
            | _ -> split_after_placeholder (h :: acc) t)
      in
      let before, after = split_after_placeholder [] base_rules in
      Css.concat [ Css.v before; supports; Css.v after ]
