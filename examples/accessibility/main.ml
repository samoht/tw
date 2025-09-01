(** Feature: Accessibility preferences demo

    Shows high contrast (forced-colors) and reduced motion preferences. *)

open Tw_html

let contrast_card =
  div
    ~tw:
      Tw.
        [
          p 4;
          rounded_lg;
          border;
          text gray 700;
          contrast_more [ border_lg; text_black ];
        ]
    [
      h3 ~tw:Tw.[ font_semibold; mb 2 ] [ txt "High Contrast" ];
      p [ txt "Increases contrast for accessibility." ];
    ]

let motion_card =
  div
    ~tw:Tw.[ bg_white; p 4; rounded_lg; shadow_sm ]
    [
      h3 ~tw:Tw.[ font_semibold; mb 2 ] [ txt "Motion Preferences" ];
      div
        ~tw:
          Tw.
            [
              w 8;
              h 8;
              bg blue 500;
              rounded_full;
              mx_auto;
              motion_safe [ animate_pulse ];
              motion_reduce [ transition_none ];
            ]
        [];
      p
        ~tw:Tw.[ text gray 600; text_sm; mt 2 ]
        [ txt "Pulses unless user prefers reduced motion." ];
    ]

let page_view =
  page ~title:"Accessibility Demo" ~tw_css:"accessibility.css" []
    [
      div
        ~tw:Tw.[ max_w_4xl; mx_auto; p 8; flex; flex_col; gap 6 ]
        [
          h1
            ~tw:Tw.[ text_4xl; font_bold; mb 4; text_center ]
            [ txt "Accessibility" ];
          contrast_card;
          motion_card;
        ];
    ]

let () =
  let html_str = html page_view in
  let css_file, css_stylesheet = css page_view in
  let css_str = Tw.Css.to_string ~minify:true ~optimize:true css_stylesheet in
  let oc_html = open_out "index.html" in
  output_string oc_html html_str;
  close_out oc_html;
  let oc_css = open_out css_file in
  output_string oc_css css_str;
  close_out oc_css;
  ()
