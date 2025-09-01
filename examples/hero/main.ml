(** Component: Hero section with CTA

    Demonstrates a centered hero with heading, subtext, and CTA buttons. *)

open Tw_html

let hero =
  section
    ~tw:Tw.[ bg_white ]
    [
      div
        ~tw:Tw.[ max_w_4xl; mx_auto; px 6; py 24; text_center ]
        [
          h1
            ~tw:Tw.[ text_5xl; font_bold; tracking_tight; text gray 900 ]
            [ txt "Build modern UIs with type-safe utilities" ];
          p
            ~tw:Tw.[ mt 6; text_lg; leading_relaxed; text gray 600 ]
            [ txt "Compose styles in OCaml and generate optimized CSS." ];
          div
            ~tw:Tw.[ mt 10; flex; items_center; justify_center; gap 4 ]
            [
              a
                ~tw:
                  Tw.
                    [
                      inline_flex;
                      items_center;
                      px 5;
                      py 3;
                      rounded_md;
                      bg blue 600;
                      text_white;
                      hover [ bg blue 700 ];
                    ]
                ~at:[ At.href "#get-started" ]
                [ txt "Get started" ];
              a
                ~tw:
                  Tw.
                    [
                      inline_flex;
                      items_center;
                      px 5;
                      py 3;
                      rounded_md;
                      border;
                      border_color gray 300;
                      text gray 700;
                      hover [ bg gray 50 ];
                    ]
                ~at:[ At.href "#learn-more" ]
                [ txt "Learn more" ];
            ];
        ];
    ]

let page_view = page ~title:"Hero" ~tw_css:"hero.css" [] [ hero ]

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
