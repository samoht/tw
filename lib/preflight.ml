(** Preflight and reset rules *)

open Css

let base_reset () =
  [
    rule ~selector:"*, :after, :before, ::backdrop"
      [ box_sizing Border_box; border "0 solid"; margin Zero; padding Zero ];
    rule ~selector:"::file-selector-button"
      [ box_sizing Border_box; border "0 solid"; margin Zero; padding Zero ];
    rule ~selector:"html, :host"
      [
        webkit_text_size_adjust "100%";
        tab_size 4;
        line_height (Num 1.5);
        font_family
          [
            Var
              {
                name = "default-font-family";
                fallback =
                  Some
                    [
                      Ui_sans_serif;
                      System_ui;
                      Sans_serif;
                      Apple_color_emoji;
                      Segoe_ui_emoji;
                      Segoe_ui_symbol;
                      Noto_color_emoji;
                    ];
              };
          ];
        font_feature_settings "var(--default-font-feature-settings, normal)";
        font_variation_settings "var(--default-font-variation-settings, normal)";
        webkit_tap_highlight_color Transparent;
      ];
    rule ~selector:"hr" [ height Zero; color Inherit; border_top_width (Px 1) ];
    rule ~selector:"abbr:where([title])"
      [
        webkit_text_decoration "underline dotted";
        text_decoration Underline_dotted;
      ];
    rule ~selector:"table"
      [ text_indent Zero; border_color Inherit; border_collapse Collapse ];
    rule ~selector:":-moz-focusring" [ outline "auto" ];
    rule ~selector:"progress" [ vertical_align Baseline ];
    rule ~selector:"summary" [ display List_item ];
    rule ~selector:"ol, ul, menu" [ list_style "none" ];
    rule ~selector:"[hidden]:where(:not([hidden=until-found]))"
      [ important (display None) ];
  ]

let typography_reset () =
  [
    rule ~selector:"h1, h2, h3, h4, h5, h6"
      [ font_size Inherit; font_weight Inherit ];
    rule ~selector:"a"
      [
        color Inherit; webkit_text_decoration "inherit"; text_decoration Inherit;
      ];
    rule ~selector:"b, strong" [ font_weight Bolder ];
    rule ~selector:"code, kbd, samp, pre"
      [
        font_family
          [
            Var
              {
                name = "default-mono-font-family";
                fallback =
                  Some
                    [
                      Ui_monospace;
                      SFMono_regular;
                      Menlo;
                      Monaco;
                      Consolas;
                      Liberation_mono;
                      Courier_new;
                      Monospace;
                    ];
              };
          ];
        font_feature_settings
          "var(--default-mono-font-feature-settings, normal)";
        font_variation_settings
          "var(--default-mono-font-variation-settings, normal)";
        font_size (Em 1.0);
      ];
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

let media_reset () =
  [
    rule ~selector:"img, svg, video, canvas, audio, iframe, embed, object"
      [ vertical_align Middle; display Block ];
    rule ~selector:"img, video" [ max_width (Pct 100.0); height Auto ];
  ]

let forms_reset () =
  [
    rule ~selector:"button, input, select, optgroup, textarea"
      [
        font "inherit";
        font_feature_settings "inherit";
        font_variation_settings "inherit";
        letter_spacing Inherit;
        color Inherit;
        opacity 1.0;
        background_color Transparent;
        border_radius Zero;
      ];
    rule ~selector:"::file-selector-button"
      [
        font "inherit";
        font_feature_settings "inherit";
        font_variation_settings "inherit";
        letter_spacing Inherit;
        color Inherit;
        opacity 1.0;
        background_color Transparent;
        border_radius Zero;
      ];
    rule ~selector:":where(select:is([multiple], [size])) optgroup"
      [ font_weight Bolder ];
    rule ~selector:":where(select:is([multiple], [size])) optgroup option"
      [ padding_inline_start (Px 20) ];
    rule ~selector:"::file-selector-button" [ margin_inline_end (Px 4) ];
    rule ~selector:"::placeholder" [ opacity 1.0 ];
    rule ~selector:"textarea" [ resize Vertical ];
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
    rule ~selector:":-moz-ui-invalid" [ box_shadow "none" ];
    rule
      ~selector:
        "button, input:where([type=button], [type=reset], [type=submit])"
      [ appearance Button ];
    rule ~selector:"::file-selector-button" [ appearance Button ];
    rule ~selector:"::-webkit-inner-spin-button" [ height Auto ];
    rule ~selector:"::-webkit-outer-spin-button" [ height Auto ];
  ]

let stylesheet () =
  List.concat
    [ base_reset (); typography_reset (); media_reset (); forms_reset () ]
