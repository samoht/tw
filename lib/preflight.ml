(** Preflight and reset rules *)

open Css

let stylesheet () =
  [
    (* Universal reset *)
    rule ~selector:"*, :after, :before, ::backdrop"
      [ box_sizing Border_box; border "0 solid"; margin Zero; padding Zero ];
    (* File selector button - first occurrence *)
    rule ~selector:"::file-selector-button"
      [ box_sizing Border_box; border "0 solid"; margin Zero; padding Zero ];
    (* HTML and host *)
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
    (* Horizontal rule *)
    rule ~selector:"hr" [ height Zero; color Inherit; border_top_width (Px 1) ];
    (* Abbreviations *)
    rule ~selector:"abbr:where([title])"
      [
        webkit_text_decoration "underline dotted";
        text_decoration Underline_dotted;
      ];
    (* Headings *)
    rule ~selector:"h1, h2, h3, h4, h5, h6"
      [ font_size Inherit; font_weight Inherit ];
    (* Links *)
    rule ~selector:"a"
      [
        color Inherit; webkit_text_decoration "inherit"; text_decoration Inherit;
      ];
    (* Bold elements *)
    rule ~selector:"b, strong" [ font_weight Bolder ];
    (* Code elements *)
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
    (* Small text *)
    rule ~selector:"small" [ font_size (Pct 80.0) ];
    (* Sub and sup *)
    rule ~selector:"sub, sup"
      [
        vertical_align Baseline;
        font_size (Pct 75.0);
        line_height Zero;
        position Relative;
      ];
    rule ~selector:"sub" [ bottom (Em (-0.25)) ];
    rule ~selector:"sup" [ top (Em (-0.5)) ];
    (* Table *)
    rule ~selector:"table"
      [ text_indent Zero; border_color Inherit; border_collapse Collapse ];
    (* Firefox focusring *)
    rule ~selector:":-moz-focusring" [ outline "auto" ];
    (* Progress *)
    rule ~selector:"progress" [ vertical_align Baseline ];
    (* Summary *)
    rule ~selector:"summary" [ display List_item ];
    (* Lists *)
    rule ~selector:"ol, ul, menu" [ list_style "none" ];
    (* Media elements *)
    rule ~selector:"img, svg, video, canvas, audio, iframe, embed, object"
      [ vertical_align Middle; display Block ];
    rule ~selector:"img, video" [ max_width (Pct 100.0); height Auto ];
    (* Form elements *)
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
    (* File selector button - second occurrence with font properties *)
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
    (* Select with optgroup *)
    rule ~selector:":where(select:is([multiple], [size])) optgroup"
      [ font_weight Bolder ];
    rule ~selector:":where(select:is([multiple], [size])) optgroup option"
      [ padding_inline_start (Px 20) ];
    (* File selector button - third occurrence with margin *)
    rule ~selector:"::file-selector-button" [ margin_inline_end (Px 4) ];
    (* Placeholder - basic *)
    rule ~selector:"::placeholder" [ opacity 1.0 ];
    (* Textarea *)
    rule ~selector:"textarea" [ resize Vertical ];
    (* Search decoration *)
    rule ~selector:"::-webkit-search-decoration" [ webkit_appearance None ];
    (* Webkit datetime inputs *)
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
    (* Firefox-specific *)
    rule ~selector:":-moz-ui-invalid" [ box_shadow "none" ];
    (* Button-like inputs *)
    rule
      ~selector:
        "button, input:where([type=button], [type=reset], [type=submit])"
      [ appearance Button ];
    (* File selector button - fourth occurrence with appearance *)
    rule ~selector:"::file-selector-button" [ appearance Button ];
    (* Webkit spin buttons *)
    rule ~selector:"::-webkit-inner-spin-button" [ height Auto ];
    rule ~selector:"::-webkit-outer-spin-button" [ height Auto ];
    (* Hidden elements *)
    rule ~selector:"[hidden]:where(:not([hidden=until-found]))"
      [ important (display None) ];
  ]
