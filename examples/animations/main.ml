(** Feature: Animations demo

    Shows starting styles and a gradient background transition. *)

open Tw_html

let animated_element =
  div
    ~tw:
      Tw.
        [
          starting [ opacity 0; scale 95 ];
          opacity 100;
          scale 100;
          transition_all;
          duration 500;
          p 4;
          bg_gradient_to Right;
          from_color ~shade:500 blue;
          to_color ~shade:600 purple;
          text_white;
          rounded_lg;
        ]
    [ txt "Smooth entry animation with @starting-style" ]

let page_view =
  page ~title:"Animations Demo" ~tw_css:"animations.css" []
    [
      div
        ~tw:Tw.[ max_w_4xl; mx_auto; p 8; flex; flex_col; gap 6 ]
        [
          h1
            ~tw:Tw.[ text_4xl; font_bold; mb 4; text_center ]
            [ txt "Animations" ];
          animated_element;
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
