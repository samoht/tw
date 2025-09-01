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
        li [ a ~at:[ At.href "landing/" ] [ txt "Landing" ] ];
        li [ a ~at:[ At.href "prose/" ] [ txt "Prose" ] ];
        li [ a ~at:[ At.href "colors/" ] [ txt "Colors" ] ];
        li [ a ~at:[ At.href "layout/" ] [ txt "Layout" ] ];
        li [ a ~at:[ At.href "navigation/" ] [ txt "Navigation" ] ];
        li [ a ~at:[ At.href "hero/" ] [ txt "Hero" ] ];
        li [ a ~at:[ At.href "cards/" ] [ txt "Cards" ] ];
        li [ a ~at:[ At.href "modifiers/" ] [ txt "Modifiers" ] ];
        li [ a ~at:[ At.href "dark_mode/" ] [ txt "Dark Mode" ] ];
        li [ a ~at:[ At.href "accessibility/" ] [ txt "Accessibility" ] ];
        li [ a ~at:[ At.href "animations/" ] [ txt "Animations" ] ];
      ];
    hr ();
    example_card ~title:"Landing" ~path:"landing/" ~height:700;
    example_card ~title:"Prose" ~path:"prose/" ~height:1200;
    example_card ~title:"Colors" ~path:"colors/" ~height:900;
    example_card ~title:"Layout" ~path:"layout/" ~height:700;
    example_card ~title:"Navigation" ~path:"navigation/" ~height:120;
    example_card ~title:"Hero" ~path:"hero/" ~height:400;
    example_card ~title:"Cards" ~path:"cards/" ~height:500;
    example_card ~title:"Modifiers" ~path:"modifiers/" ~height:500;
    example_card ~title:"Dark Mode" ~path:"dark_mode/" ~height:250;
    example_card ~title:"Accessibility" ~path:"accessibility/" ~height:300;
    example_card ~title:"Animations" ~path:"animations/" ~height:220;
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
