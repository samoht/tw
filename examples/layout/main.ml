(** Feature: Layout demo

    Shows Flexbox and Grid utilities with responsive examples. *)

open Tw_html

let card txt_content =
  div
    ~tw:Tw.[ p 4; bg_white; rounded_md; shadow_sm; text_center ]
    [ txt txt_content ]

let flex_showcase =
  div
    ~tw:Tw.[ p 4; bg gray 50; rounded_lg; shadow_sm ]
    [
      h2 ~tw:Tw.[ text_2xl; font_semibold; mb 4 ] [ txt "Flexbox" ];
      (* Direction *)
      p ~tw:Tw.[ text_sm; text gray 600; mb 2 ] [ txt "flex-row" ];
      div
        ~tw:Tw.[ flex; flex_row; gap 2; mb 4 ]
        [ card "1"; card "2"; card "3" ];
      p ~tw:Tw.[ text_sm; text gray 600; mb 2 ] [ txt "flex-col" ];
      div
        ~tw:Tw.[ flex; flex_col; gap 2; mb 4 ]
        [ card "A"; card "B"; card "C" ];
      (* Justify *)
      p ~tw:Tw.[ text_sm; text gray 600; mb 2 ] [ txt "justify-between" ];
      div
        ~tw:Tw.[ flex; justify_between; gap 2; mb 4 ]
        [ card "Left"; card "Right" ];
      (* Align *)
      p ~tw:Tw.[ text_sm; text gray 600; mb 2 ] [ txt "items-center" ];
      div
        ~tw:Tw.[ flex; items_center; h 24; gap 2 ]
        [ card "Top"; card "Middle"; card "Bottom" ];
    ]

let grid_showcase =
  div
    ~tw:Tw.[ p 4; bg gray 50; rounded_lg; shadow_sm ]
    [
      h2 ~tw:Tw.[ text_2xl; font_semibold; mb 4 ] [ txt "Grid" ];
      (* Fixed columns *)
      p ~tw:Tw.[ text_sm; text gray 600; mb 2 ] [ txt "grid-cols-3" ];
      div
        ~tw:Tw.[ grid; grid_cols 3; gap 2; mb 4 ]
        [ card "1"; card "2"; card "3"; card "4"; card "5"; card "6" ];
      (* Responsive *)
      p ~tw:Tw.[ text_sm; text gray 600; mb 2 ] [ txt "responsive columns" ];
      div
        ~tw:
          Tw.
            [ grid; grid_cols 1; sm [ grid_cols 2 ]; lg [ grid_cols 4 ]; gap 2 ]
        [ card "A"; card "B"; card "C"; card "D"; card "E"; card "F" ];
    ]

let page_view =
  page ~title:"Layout Demo" ~tw_css:"layout.css" []
    [
      div
        ~tw:Tw.[ max_w_6xl; mx_auto; p 8; flex; flex_col; gap 8 ]
        [
          h1 ~tw:Tw.[ text_4xl; font_bold; text_center ] [ txt "Layout" ];
          flex_showcase;
          grid_showcase;
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
