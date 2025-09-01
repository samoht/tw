open Tw_html
(** Landing page example demonstrating tw.html with automatic CSS generation *)

(* Header component *)
let header_section =
  header
    ~tw:Tw.[ bg_white; shadow_sm; py 4 ]
    [
      div
        ~tw:Tw.[ max_w_4xl; mx_auto; px 4 ]
        [
          h1
            ~tw:Tw.[ text_2xl; font_bold; text gray 800 ]
            [ txt "Welcome to tw.html" ];
        ];
    ]

(* Hero section *)
let hero_section =
  section
    ~tw:Tw.[ text_center; mb 12 ]
    [
      h2
        ~tw:Tw.[ text_4xl; font_bold; text gray 900; mb 4 ]
        [ txt "Type-Safe HTML with Tw" ];
      p
        ~tw:Tw.[ text_lg; text gray 600; max_w_2xl; mx_auto; leading_relaxed ]
        [
          txt
            "Write HTML and CSS using OCaml's type system. Get compile-time \
             guarantees and auto-completion for free.";
        ];
    ]

(* Card component *)
let feature_card ~icon ~title ~description =
  div
    ~tw:Tw.[ bg_white; rounded_lg; shadow_md; p 6 ]
    [
      div
        ~tw:Tw.[ flex; items_center; mb 4 ]
        [
          span
            ~tw:
              Tw.
                [
                  w 12;
                  h 12;
                  bg blue 100;
                  rounded_full;
                  flex;
                  items_center;
                  justify_center;
                  text blue 600;
                  font_semibold;
                  text_xl;
                ]
            [ txt icon ];
          h3
            ~tw:Tw.[ ml 4; text_xl; font_semibold; text gray 800 ]
            [ txt title ];
        ];
      p ~tw:Tw.[ text gray 600 ] [ txt description ];
    ]

(* Features section *)
let features_section =
  section
    ~tw:Tw.[ mb 12 ]
    [
      h2
        ~tw:Tw.[ text_3xl; font_bold; text_center; text gray 900; mb 8 ]
        [ txt "Features" ];
      div
        ~tw:Tw.[ grid; grid_cols 1; md [ grid_cols 2 ]; gap 6 ]
        [
          feature_card ~icon:"🎨" ~title:"Tailwind-Compatible"
            ~description:
              "Use familiar Tw utility classes with compile-time safety. No \
               more typos in class names!";
          feature_card ~icon:"⚡" ~title:"Automatic CSS Generation"
            ~description:
              "Generates optimized CSS containing only the classes you \
               actually use. No dead code!";
          feature_card ~icon:"🔒" ~title:"Type-Safe"
            ~description:
              "OCaml's type system ensures your HTML is well-formed and your \
               CSS classes exist.";
          feature_card ~icon:"🚀" ~title:"Fast"
            ~description:
              "Compile-time generation means zero runtime overhead. Your pages \
               load instantly.";
        ];
    ]

(* Code example *)
let code_example_section =
  section
    ~tw:Tw.[ bg gray 100; rounded_lg; p 6; mb 12 ]
    [
      h3
        ~tw:Tw.[ text_xl; font_semibold; text gray 800; mb 4 ]
        [ txt "Code Example" ];
      pre
        ~tw:
          Tw.
            [
              p 4;
              rounded_md;
              overflow_x_auto;
              bg gray 900;
              text_white;
              font_mono;
              text_sm;
            ]
        [
          code
            [
              txt
                "let button ~tw label =\n\
                \  Html.button ~at:[ Html.At.class_ (Tw.to_classes tw) ] [ \
                 Html.txt label ]";
            ];
        ];
    ]

(* Call-to-action section *)
let cta_section =
  section
    ~tw:Tw.[ text_center ]
    [
      h2 ~tw:Tw.[ text_2xl; font_bold; mb 2 ] [ txt "Get Started" ];
      p ~tw:Tw.[ text gray 600; mb 4 ] [ txt "Install and start styling now." ];
      a
        ~tw:
          Tw.
            [
              inline_flex;
              items_center;
              px 6;
              py 3;
              rounded_md;
              bg blue 600;
              text_white;
              hover [ bg blue 700 ];
            ]
        ~at:[ At.href "#" ]
        [ txt "Read the docs" ];
    ]

let hover_effect_card =
  div
    ~tw:Tw.[ transform; transition_all; duration 300; hover [ scale 105 ] ]
    [
      div
        ~tw:Tw.[ bg_white; p 6; rounded_lg; shadow_md; text_center ]
        [ txt "Hover over this card to see it scale and lift." ];
    ]

let dark_mode_card =
  div
    ~tw:Tw.[ bg_white; dark [ bg gray 800 ]; p 4; rounded_lg; shadow_sm ]
    [
      h3
        ~tw:Tw.[ font_semibold; mb 2; text gray 900; dark [ text_white ] ]
        [ txt "Dark Mode" ];
      p
        ~tw:Tw.[ text gray 600; dark [ text gray 300 ]; text_sm ]
        [ txt "This card adapts to your system's dark mode preference." ];
    ]

let animation_card =
  div
    ~tw:Tw.[ bg_white; p 4; rounded_lg; shadow_sm ]
    [
      h3 ~tw:Tw.[ font_semibold; mb 2 ] [ txt "Animation" ];
      div
        ~tw:Tw.[ w 8; h 8; bg blue 500; rounded_full; mx_auto; animate_pulse ]
        [];
      p
        ~tw:Tw.[ text gray 600; text_sm; mt 2 ]
        [ txt "A pulsing indicator using CSS animations." ];
    ]

(* Interactive examples section *)
let interactive_section =
  section
    ~tw:Tw.[ mb 12 ]
    [
      h2
        ~tw:Tw.[ text_3xl; font_bold; text_center; text gray 900; mb 8 ]
        [ txt "Interactive Examples" ];
      div
        ~tw:Tw.[ grid; grid_cols 1; lg [ grid_cols 3 ]; gap 4 ]
        [ hover_effect_card; dark_mode_card; animation_card ];
    ]

(* Footer component *)
let footer_section =
  footer
    ~tw:Tw.[ bg gray 800; text_white; py 8; mt_auto ]
    [
      div
        ~tw:Tw.[ max_w_4xl; mx_auto; px 4; text_center ]
        [ p ~tw:Tw.[ text gray 500 ] [ txt "Built with ❤️ using OCaml and Tw" ] ];
    ]

(* Main page assembly *)
let main =
  page ~title:"Landing Page"
    ~meta:
      [
        ( "description",
          "A landing page demonstrating Tw CSS generation with OCaml" );
        ("viewport", "width=device-width, initial-scale=1.0");
      ]
    ~tw_css:"landing.css"
    (* Head content *)
    [ meta ~at:[ At.name "author"; At.content "tw.html" ] () ]
    (* Body content *)
    [
      div
        ~tw:Tw.[ min_h_screen; bg gray 50 ]
        [
          header_section;
          (* Main content *)
          main
            ~tw:Tw.[ flex_1; py 8 ]
            [
              div
                ~tw:Tw.[ max_w_2xl; mx_auto; px 4 ]
                [
                  hero_section;
                  features_section;
                  code_example_section;
                  interactive_section;
                  cta_section;
                ];
            ];
          footer_section;
        ];
    ]

(* Generate the page *)
let () =
  let html_str = html main in
  let css_filename, css_stylesheet = css main in
  let css_str = Tw.Css.to_string ~minify:true ~optimize:true css_stylesheet in

  (* Write HTML file *)
  let oc_html = open_out "index.html" in
  output_string oc_html html_str;
  close_out oc_html;

  (* Write CSS file *)
  let oc_css = open_out css_filename in
  output_string oc_css css_str;
  close_out oc_css;

  ()
