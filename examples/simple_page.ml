(* Simple page example demonstrating tw.html with automatic CSS generation *)
open Tw_html

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
        ~tw:Tw.[ grid; grid_cols 1; on_md [ grid_cols 2 ]; gap 6 ]
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
              bg gray 900;
              text gray 100;
              p 4;
              rounded_md;
              overflow_x_auto;
              font_mono;
              text_sm;
            ]
        [
          code
            [
              txt
                {|div
  ~tw:Tw.[ bg_white; p 4; rounded_lg; shadow_md ]
  [
    h1 ~tw:Tw.[ text_2xl; font_bold; text_blue_600 ] 
      [ txt "Hello, World!" ];
    p ~tw:Tw.[ text_gray_700; mt 2 ] 
      [ txt "Type-safe HTML generation with Tailwind." ];
  ]|};
            ];
        ];
    ]

(* CTA section *)
let cta_section =
  section
    ~tw:Tw.[ text_center; mb 12 ]
    [
      h2
        ~tw:Tw.[ text_3xl; font_bold; text gray 900; mb 4 ]
        [ txt "Get Started" ];
      p
        ~tw:Tw.[ text_lg; text gray 600; mb 8 ]
        [
          txt
            "Install tw.html from opam and start building type-safe web pages \
             today!";
        ];
      div
        ~tw:Tw.[ flex; justify_center; gap 4 ]
        [
          button
            ~tw:
              Tw.
                [
                  bg blue 600;
                  text_white;
                  px 6;
                  py 3;
                  rounded_lg;
                  font_semibold;
                  on_hover [ bg blue 700 ];
                  transition_colors;
                ]
            [ txt "Documentation" ];
          button
            ~tw:
              Tw.
                [
                  bg gray 200;
                  text gray 800;
                  px 6;
                  py 3;
                  rounded_lg;
                  font_semibold;
                  on_hover [ bg gray 300 ];
                  transition_colors;
                ]
            [ txt "View on GitHub" ];
        ];
    ]

(* Interactive example components *)
let hover_effect_card =
  div
    ~tw:
      Tw.
        [
          bg_white;
          p 4;
          rounded_lg;
          shadow_sm;
          on_hover [ shadow_lg; scale 105 ];
          transition_all;
          duration 200;
          cursor_pointer;
        ]
    [
      h3 ~tw:Tw.[ font_semibold; mb 2 ] [ txt "Hover Effect" ];
      p
        ~tw:Tw.[ text gray 600; text_sm ]
        [ txt "Hover over this card to see it scale and lift." ];
    ]

let dark_mode_card =
  div
    ~tw:Tw.[ bg_white; on_dark [ bg gray 800 ]; p 4; rounded_lg; shadow_sm ]
    [
      h3
        ~tw:Tw.[ font_semibold; mb 2; text gray 900; on_dark [ text_white ] ]
        [ txt "Dark Mode" ];
      p
        ~tw:Tw.[ text gray 600; on_dark [ text gray 300 ]; text_sm ]
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
        ~tw:Tw.[ grid; grid_cols 1; on_lg [ grid_cols 3 ]; gap 4 ]
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
  page ~title:"Simple Styled Page"
    ~meta:
      [
        ( "description",
          "A simple page demonstrating Tw CSS generation with OCaml" );
        ("viewport", "width=device-width, initial-scale=1.0");
      ]
    ~tw_css:"simple.css"
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
  let css_str = Tw.Css.to_string ~minify:true css_stylesheet in

  (* Write HTML file *)
  let oc_html = open_out "simple_page.html" in
  output_string oc_html html_str;
  close_out oc_html;

  (* Write CSS file *)
  let oc_css = open_out css_filename in
  output_string oc_css css_str;
  close_out oc_css;

  (* Use simpler output *)
  print_endline "Generated:";
  print_endline "  - simple_page.html";
  print_string "  - ";
  print_endline css_filename;
  print_endline "\nOpen simple_page.html in your browser to see the result!"
