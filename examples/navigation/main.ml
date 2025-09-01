(** Component: Responsive navigation bar

    Demonstrates a simple responsive navbar layout. *)

open Tw_html

let nav_link href label =
  a
    ~tw:Tw.[ text gray 600; hover [ text gray 900 ]; px 3; py 2; rounded_md ]
    ~at:[ At.href href ]
    [ txt label ]

let navbar =
  nav
    ~tw:Tw.[ bg_white; border_b; border_color gray 200 ]
    [
      div
        ~tw:Tw.[ max_w_6xl; mx_auto; px 4 ]
        [
          div
            ~tw:Tw.[ flex; h 16; items_center; justify_between ]
            [
              (* Brand *)
              a
                ~tw:Tw.[ text_xl; font_bold; text gray 900 ]
                ~at:[ At.href "#" ]
                [ txt "Brand" ];
              (* Links *)
              div
                ~tw:Tw.[ hidden; md [ flex ]; gap 2 ]
                [
                  nav_link "#features" "Features";
                  nav_link "#pricing" "Pricing";
                  nav_link "#about" "About";
                ];
              (* Call to action *)
              a
                ~tw:
                  Tw.
                    [
                      hidden;
                      md [ inline_flex ];
                      items_center;
                      px 4;
                      py 2;
                      rounded_md;
                      bg blue 600;
                      text_white;
                      hover [ bg blue 700 ];
                    ]
                ~at:[ At.href "#get-started" ]
                [ txt "Get started" ];
            ];
        ];
    ]

let page_view = page ~title:"Navigation" ~tw_css:"navigation.css" [] [ navbar ]

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
