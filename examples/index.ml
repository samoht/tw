(** Top-level manual index for examples Uses Tw.Prose for readable documentation
    and embeds sub-examples via iframes. *)

open Tw_html

let example_card ~title ~path ~height =
  section
    ~tw:Tw.[ mb 10 ]
    [
      h2 ~tw:Tw.[ text_2xl; font_semibold; mb 2 ] [ txt title ];
      p ~tw:Tw.[ text ~shade:600 gray; mb 4 ] [ txt path ];
      iframe
        ~tw:Tw.[ w_full; h height; border; rounded_md; bg white ]
        ~at:[ At.src path ]
        [];
    ]

(* Each example: title, iframe path, one-line blurb, and embed height. *)
let examples =
  [
    ("Landing", "landing/", "Marketing page with gradients and CTA", 700);
    ("Dashboard", "dashboard/", "Responsive admin layout", 800);
    ("Prose", "prose/", "Typography for articles", 1200);
    ("Forms", "forms/", "Input states and validation", 600);
    ("Colors", "colors/", "Full palette and alpha variants", 900);
    ("Layout", "layout/", "Flexbox and Grid patterns", 1800);
    ("Modifiers", "modifiers/", "State, group, peer, and ARIA", 1600);
    ("Animations", "animations/", "Keyframes and transitions", 1500);
    ("Accessibility", "accessibility/", "Contrast, motion, focus", 1400);
  ]

let nav_link (title, path, blurb, _) =
  li [ a ~at:[ At.href path ] [ txt (title ^ " — " ^ blurb) ] ]

let content =
  [
    h1 [ txt "Examples Manual" ];
    p
      ~tw:Tw.[ prose_lead ]
      [ txt "Explore focused, feature-based examples. Each is embedded below." ];
    ul (List.map nav_link examples);
    hr ();
  ]
  @ List.map
      (fun (title, path, _, height) -> example_card ~title ~path ~height)
      examples

let page_view =
  page ~title:"Examples Manual" ~tw_css:(Link "manual.css") []
    [ div ~tw:Tw.[ prose; max_w_4xl; mx_auto; p 8 ] content ]

let () =
  let html_str = html page_view in
  let css_file, css_stylesheet = css page_view in
  let css_str = Tw.Css.to_string ~minify:true css_stylesheet in
  let oc_html = open_out "index.html" in
  output_string oc_html html_str;
  close_out oc_html;
  Option.iter
    (fun file ->
      let oc_css = open_out file in
      output_string oc_css css_str;
      close_out oc_css)
    css_file;
  ()
