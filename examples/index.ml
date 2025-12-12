(** Top-level manual index for examples Uses Tw.Prose for readable documentation
    and embeds sub-examples via iframes. *)

open Tw_html

let example_card ~title ~path ~height =
  section
    ~tw:Tw.[ mb 10 ]
    [
      h2 ~tw:Tw.[ text_2xl; font_semibold; mb 2 ] [ txt title ];
      p ~tw:Tw.[ text gray 600; mb 4 ] [ txt path ];
      iframe
        ~tw:Tw.[ w_full; h height; border; rounded_md; bg_white ]
        ~at:[ At.src path ]
        [];
    ]

let content =
  [
    h1 [ txt "Examples Manual" ];
    p
      ~tw:Tw.[ prose_lead ]
      [ txt "Explore focused, feature-based examples. Each is embedded below." ];
    ul
      [
        li
          [
            a
              ~at:[ At.href "landing/" ]
              [ txt "Landing — Marketing page with gradients and CTA" ];
          ];
        li
          [
            a
              ~at:[ At.href "dashboard/" ]
              [ txt "Dashboard — Responsive admin layout" ];
          ];
        li
          [
            a ~at:[ At.href "prose/" ] [ txt "Prose — Typography for articles" ];
          ];
        li
          [
            a
              ~at:[ At.href "forms/" ]
              [ txt "Forms — Input states and validation" ];
          ];
        li
          [
            a
              ~at:[ At.href "colors/" ]
              [ txt "Colors — Full palette and alpha variants" ];
          ];
        li
          [
            a
              ~at:[ At.href "layout/" ]
              [ txt "Layout — Flexbox and Grid patterns" ];
          ];
        li
          [
            a
              ~at:[ At.href "modifiers/" ]
              [ txt "Modifiers — State, group, peer, and ARIA" ];
          ];
        li
          [
            a
              ~at:[ At.href "animations/" ]
              [ txt "Animations — Keyframes and transitions" ];
          ];
        li
          [
            a
              ~at:[ At.href "accessibility/" ]
              [ txt "Accessibility — Contrast, motion, focus" ];
          ];
      ];
    hr ();
    example_card ~title:"Landing" ~path:"landing/" ~height:700;
    example_card ~title:"Dashboard" ~path:"dashboard/" ~height:800;
    example_card ~title:"Prose" ~path:"prose/" ~height:1200;
    example_card ~title:"Forms" ~path:"forms/" ~height:600;
    example_card ~title:"Colors" ~path:"colors/" ~height:900;
    example_card ~title:"Layout" ~path:"layout/" ~height:1800;
    example_card ~title:"Modifiers" ~path:"modifiers/" ~height:1600;
    example_card ~title:"Animations" ~path:"animations/" ~height:1500;
    example_card ~title:"Accessibility" ~path:"accessibility/" ~height:1400;
  ]

let page_view =
  page ~title:"Examples Manual" ~tw_css:"manual.css" []
    [ div ~tw:Tw.[ prose; max_w_4xl; mx_auto; p 8 ] content ]

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
