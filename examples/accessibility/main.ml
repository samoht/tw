(** Feature: Accessibility preferences and semantic HTML demo

    Shows high contrast, reduced motion, dark mode, and demonstrates focus
    states and ARIA patterns. *)

open Tw_html

(* Header with skip link for keyboard navigation *)
let page_header =
  header
    ~tw:Tw.[ bg_white; shadow_sm; dark [ bg gray 900 ] ]
    [
      a
        ~at:[ At.href "#main-content" ]
        ~tw:
          Tw.
            [
              sr_only;
              focus [ not_sr_only; absolute; p 4; bg blue 600; text_white ];
            ]
        [ txt "Skip to main content" ];
      div
        ~tw:Tw.[ max_w_7xl; mx_auto; px 4; py 4 ]
        [
          h1
            ~tw:Tw.[ text_2xl; font_bold; text gray 900; dark [ text_white ] ]
            [ txt "Accessibility Demo" ];
        ];
    ]

(* High contrast section *)
let contrast_section =
  section
    ~at:[ At.v "aria-labelledby" "contrast-heading" ]
    ~tw:
      Tw.
        [
          bg_white;
          p 6;
          rounded_xl;
          shadow_sm;
          border;
          border_color gray 200;
          dark [ bg gray 800; border_color gray 700 ];
          contrast_more [ border_color gray 900; border_lg ];
        ]
    [
      h2
        ~at:[ At.id "contrast-heading" ]
        ~tw:
          Tw.
            [
              text_xl;
              font_bold;
              text gray 900;
              mb 4;
              dark [ text_white ];
              contrast_more [ underline ];
            ]
        [ txt "High Contrast Mode" ];
      p
        ~tw:
          Tw.
            [
              text gray 600;
              dark [ text gray 300 ];
              contrast_more [ text_black; dark [ text_white ] ];
            ]
        [
          txt
            "When you enable high contrast in your OS settings, this content \
             adapts automatically. Borders become thicker, and colors become \
             more distinct.";
        ];
      div
        ~tw:Tw.[ mt 6; flex; gap 4; flex_wrap ]
        [
          button
            ~tw:
              Tw.
                [
                  px 4;
                  py 2;
                  rounded_lg;
                  bg blue 600;
                  text_white;
                  hover [ bg blue 700 ];
                  focus [ ring_md; ring_color blue 500 ];
                  contrast_more [ border_md; border_color blue 900 ];
                ]
            [ txt "Primary Action" ];
          button
            ~tw:
              Tw.
                [
                  px 4;
                  py 2;
                  rounded_lg;
                  border;
                  border_color gray 300;
                  text gray 700;
                  hover [ bg gray 100 ];
                  focus [ ring_md; ring_color gray 500 ];
                  contrast_more [ border_lg; border_color gray 900; text_black ];
                  dark [ border_color gray 600; text gray 300 ];
                ]
            [ txt "Secondary" ];
        ];
    ]

(* Motion preferences section *)
let motion_section =
  section
    ~at:[ At.v "aria-labelledby" "motion-heading" ]
    ~tw:Tw.[ bg_white; p 6; rounded_xl; shadow_sm; dark [ bg gray 800 ] ]
    [
      h2
        ~at:[ At.id "motion-heading" ]
        ~tw:Tw.[ text_xl; font_bold; text gray 900; mb 4; dark [ text_white ] ]
        [ txt "Motion Preferences" ];
      p
        ~tw:Tw.[ text gray 600; dark [ text gray 300 ]; mb 6 ]
        [
          txt
            "These animations respect the prefers-reduced-motion media query. \
             Users who prefer reduced motion will see static elements instead.";
        ];
      div
        ~tw:Tw.[ flex; gap 8; items_center; justify_center; flex_wrap ]
        [
          (* Pulsing indicator *)
          div
            ~tw:Tw.[ flex; flex_col; items_center; gap 2 ]
            [
              div
                ~tw:
                  Tw.
                    [
                      w 12;
                      h 12;
                      bg blue 500;
                      rounded_full;
                      motion_safe [ animate_pulse ];
                      motion_reduce [ opacity 100 ];
                    ]
                [];
              span
                ~tw:Tw.[ text_sm; text gray 500; dark [ text gray 400 ] ]
                [ txt "Pulse" ];
            ];
          (* Spinning loader *)
          div
            ~tw:Tw.[ flex; flex_col; items_center; gap 2 ]
            [
              div
                ~tw:
                  Tw.
                    [
                      w 12;
                      h 12;
                      border_lg;
                      border_color blue 600;
                      rounded_full;
                      motion_safe [ animate_spin ];
                    ]
                [];
              span
                ~tw:Tw.[ text_sm; text gray 500; dark [ text gray 400 ] ]
                [ txt "Spinner" ];
            ];
          (* Bouncing element *)
          div
            ~tw:Tw.[ flex; flex_col; items_center; gap 2 ]
            [
              div
                ~tw:
                  Tw.
                    [
                      w 12;
                      h 12;
                      bg green 500;
                      rounded_lg;
                      motion_safe [ animate_bounce ];
                      motion_reduce [ opacity 100 ];
                    ]
                [];
              span
                ~tw:Tw.[ text_sm; text gray 500; dark [ text gray 400 ] ]
                [ txt "Bounce" ];
            ];
        ];
    ]

(* Color scheme section *)
let color_section =
  section
    ~at:[ At.v "aria-labelledby" "color-heading" ]
    ~tw:Tw.[ bg_white; p 6; rounded_xl; shadow_sm; dark [ bg gray 800 ] ]
    [
      h2
        ~at:[ At.id "color-heading" ]
        ~tw:Tw.[ text_xl; font_bold; text gray 900; mb 4; dark [ text_white ] ]
        [ txt "Dark Mode Support" ];
      p
        ~tw:Tw.[ text gray 600; dark [ text gray 300 ]; mb 6 ]
        [ txt "These elements adapt to your system's color scheme preference." ];
      div
        ~tw:Tw.[ grid; grid_cols 2; gap 4; lg [ grid_cols 4 ] ]
        (List.map
           (fun (color_name, bg_style, text_style) ->
             div
               ~tw:
                 Tw.
                   [
                     p 4;
                     rounded_lg;
                     text_center;
                     bg_style;
                     text_style;
                     transition_colors;
                     duration 200;
                   ]
               [ txt color_name ])
           [
             ("Light", Tw.(bg gray 100), Tw.(text gray 800));
             ("Primary", Tw.(bg blue 600), Tw.text_white);
             ("Success", Tw.(bg green 600), Tw.text_white);
             ("Warning", Tw.(bg amber 500), Tw.text_black);
           ]);
    ]

(* Focus states demonstration *)
let focus_section =
  section
    ~at:[ At.v "aria-labelledby" "focus-heading" ]
    ~tw:Tw.[ bg_white; p 6; rounded_xl; shadow_sm; dark [ bg gray 800 ] ]
    [
      h2
        ~at:[ At.id "focus-heading" ]
        ~tw:Tw.[ text_xl; font_bold; text gray 900; mb 4; dark [ text_white ] ]
        [ txt "Focus States for Keyboard Navigation" ];
      p
        ~tw:Tw.[ text gray 600; dark [ text gray 300 ]; mb 6 ]
        [
          txt
            "Use Tab to navigate these interactive elements. Each has a \
             visible focus indicator for keyboard users.";
        ];
      div
        ~tw:Tw.[ flex; flex_wrap; gap 4 ]
        [
          (* Text input with focus ring *)
          input
            ~at:
              [
                At.v "type" "text";
                At.v "placeholder" "Focus me with Tab";
                At.v "aria-label" "Example text input";
              ]
            ~tw:
              Tw.
                [
                  px 4;
                  py 2;
                  rounded_lg;
                  border;
                  border_color gray 300;
                  focus [ ring_md; ring_color blue 500; outline_none ];
                  dark [ bg gray 700; border_color gray 600; text_white ];
                ]
            ();
          (* Link with focus styling *)
          a
            ~at:[ At.href "#" ]
            ~tw:
              Tw.
                [
                  px 4;
                  py 2;
                  text blue 600;
                  underline;
                  rounded;
                  focus [ ring_md; ring_color blue 500 ];
                  dark [ text blue 400 ];
                ]
            [ txt "Focusable Link" ];
          (* Button with focus *)
          button
            ~tw:
              Tw.
                [
                  px 4;
                  py 2;
                  bg blue 600;
                  text_white;
                  rounded_lg;
                  focus [ ring_md; ring_color blue 400 ];
                ]
            [ txt "Button" ];
        ];
    ]

(* Screen reader content section *)
let sr_section =
  section
    ~at:[ At.v "aria-labelledby" "sr-heading" ]
    ~tw:Tw.[ bg blue 50; p 6; rounded_xl; dark [ bg gray 800 ] ]
    [
      h2
        ~at:[ At.id "sr-heading" ]
        ~tw:Tw.[ text_xl; font_bold; text gray 900; mb 4; dark [ text_white ] ]
        [ txt "Screen Reader Utilities" ];
      p
        ~tw:Tw.[ text gray 600; dark [ text gray 300 ]; mb 6 ]
        [
          txt
            "The sr-only class hides content visually while keeping it \
             accessible to screen readers.";
        ];
      div
        ~tw:Tw.[ flex; gap 4; flex_wrap ]
        [
          button
            ~tw:
              Tw.
                [
                  flex;
                  items_center;
                  gap 2;
                  px 4;
                  py 2;
                  bg_white;
                  rounded_lg;
                  shadow_sm;
                  hover [ shadow_md ];
                  focus [ ring_md; ring_color blue 500 ];
                  dark [ bg gray 800 ];
                ]
            [
              span ~tw:Tw.[ text_xl ] [ txt "!" ];
              span ~tw:Tw.[ sr_only ] [ txt "Important notification" ];
              span
                ~tw:Tw.[ text gray 700; dark [ text gray 300 ] ]
                [ txt "Alert" ];
            ];
          button
            ~tw:
              Tw.
                [
                  flex;
                  items_center;
                  gap 2;
                  px 4;
                  py 2;
                  bg_white;
                  rounded_lg;
                  shadow_sm;
                  hover [ shadow_md ];
                  focus [ ring_md; ring_color blue 500 ];
                  dark [ bg gray 800 ];
                ]
            [
              span ~tw:Tw.[ text_xl ] [ txt "i" ];
              span ~tw:Tw.[ sr_only ] [ txt "More information" ];
              span
                ~tw:Tw.[ text gray 700; dark [ text gray 300 ] ]
                [ txt "Info" ];
            ];
        ];
      p
        ~tw:Tw.[ text_sm; text gray 500; mt 4; dark [ text gray 400 ] ]
        [
          txt
            "Screen reader users will hear 'Important notification Alert' and \
             'More information Info' for these buttons.";
        ];
    ]

(* Main page *)
let page_view =
  page ~title:"Accessibility Demo" ~tw_css:"accessibility.css" []
    [
      page_header;
      main
        ~at:[ At.id "main-content" ]
        ~tw:Tw.[ max_w_5xl; mx_auto; p 6; md [ p 8 ] ]
        [
          div
            ~tw:Tw.[ flex; flex_col; gap 8 ]
            [
              (* Introduction *)
              div
                ~tw:Tw.[ text_center; py 8 ]
                [
                  h1
                    ~tw:
                      Tw.
                        [
                          text_3xl;
                          md [ text_4xl ];
                          font_bold;
                          text gray 900;
                          mb 4;
                          dark [ text_white ];
                        ]
                    [ txt "Building Accessible Interfaces" ];
                  p
                    ~tw:
                      Tw.
                        [
                          text_lg;
                          text gray 600;
                          max_w_2xl;
                          mx_auto;
                          dark [ text gray 400 ];
                        ]
                    [
                      txt
                        "This demo showcases Tailwind accessibility utilities \
                         for contrast, motion, focus states, and screen reader \
                         support.";
                    ];
                ];
              contrast_section;
              motion_section;
              color_section;
              focus_section;
              sr_section;
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
