(** Component: Cards grid

    Demonstrates simple content cards in a responsive grid. *)

open Tw_html

let card ~title ~desc =
  div
    ~tw:Tw.[ bg_white; p 6; rounded_lg; shadow_sm ]
    [
      h3 ~tw:Tw.[ font_semibold; mb 2 ] [ txt title ];
      p ~tw:Tw.[ text gray 600 ] [ txt desc ];
    ]

let page_view =
  page ~title:"Cards" ~tw_css:"cards.css" []
    [
      div
        ~tw:Tw.[ max_w_5xl; mx_auto; p 8 ]
        [
          h1 ~tw:Tw.[ text_3xl; font_bold; mb 6 ] [ txt "Cards" ];
          div
            ~tw:
              Tw.
                [
                  grid;
                  grid_cols 1;
                  sm [ grid_cols 2 ];
                  lg [ grid_cols 3 ];
                  gap 6;
                ]
            [
              card ~title:"Fast" ~desc:"Generates only the CSS you use.";
              card ~title:"Type-safe"
                ~desc:"Prevent invalid utilities at compile time.";
              card ~title:"Portable" ~desc:"Pure OCaml, no external CSS deps.";
            ];
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
