(** Feature: Dark mode demo

    Shows dark mode color scheme using the `dark` modifier. *)

open Tw_html

let dark_mode_card =
  div
    ~tw:Tw.[ bg_white; dark [ bg gray 800 ]; p 4; rounded_lg; shadow_sm ]
    [
      h3
        ~tw:Tw.[ font_semibold; mb 2; text gray 900; dark [ text_white ] ]
        [ txt "Dark Mode" ];
      p
        ~tw:Tw.[ text gray 600; dark [ text gray 300 ]; text_sm ]
        [ txt "Adapts to the user's color scheme preference." ];
    ]

let page_view =
  page ~title:"Dark Mode Demo" ~tw_css:"dark_mode.css" []
    [
      div
        ~tw:Tw.[ max_w_4xl; mx_auto; p 8; flex; flex_col; gap 6 ]
        [
          h1
            ~tw:Tw.[ text_4xl; font_bold; mb 4; text_center ]
            [ txt "Dark Mode" ];
          dark_mode_card;
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
