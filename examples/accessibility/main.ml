(** Feature: Accessibility preferences and semantic HTML demo

    Shows high contrast, reduced motion, dark mode, and demonstrates focus
    states and ARIA patterns. *)

open Tw_html

(* Header with skip link for keyboard navigation *)
let page_header =
  header
    ~tw:Tw.[ bg white; shadow_sm; dark [ bg ~shade:900 gray ] ]
    [
      a
        ~at:[ At.href "#main-content" ]
        ~tw:
          Tw.
            [
              sr_only;
              focus
                [ not_sr_only; absolute; p 4; bg ~shade:600 blue; text white ];
            ]
        [ txt "Skip to main content" ];
      div
        ~tw:Tw.[ max_w_7xl; mx_auto; px 4; py 4 ]
        [
          h1
            ~tw:
              Tw.
                [
                  text_2xl; font_bold; text ~shade:900 gray; dark [ text white ];
                ]
            [ txt "Accessibility Demo" ];
        ];
    ]

(* High contrast section *)
let contrast_buttons =
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
              bg ~shade:600 blue;
              text white;
              hover [ bg ~shade:700 blue ];
              focus [ ring_md; ring_color blue ];
              contrast_more [ border_md; border_color ~shade:900 blue ];
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
              border_color ~shade:300 gray;
              text ~shade:700 gray;
              hover [ bg ~shade:100 gray ];
              focus [ ring_md; ring_color gray ];
              contrast_more
                [ border_lg; border_color ~shade:900 gray; text black ];
              dark [ border_color ~shade:600 gray; text ~shade:300 gray ];
            ]
        [ txt "Secondary" ];
    ]

let contrast_section =
  section
    ~at:[ At.v "aria-labelledby" "contrast-heading" ]
    ~tw:
      Tw.
        [
          bg white;
          p 6;
          rounded_xl;
          shadow_sm;
          border;
          border_color ~shade:200 gray;
          dark [ bg ~shade:800 gray; border_color ~shade:700 gray ];
          contrast_more [ border_color ~shade:900 gray; border_lg ];
        ]
    [
      h2
        ~at:[ At.id "contrast-heading" ]
        ~tw:
          Tw.
            [
              text_xl;
              font_bold;
              text ~shade:900 gray;
              mb 4;
              dark [ text white ];
              contrast_more [ underline ];
            ]
        [ txt "High Contrast Mode" ];
      p
        ~tw:
          Tw.
            [
              text ~shade:600 gray;
              dark [ text ~shade:300 gray ];
              contrast_more [ text black; dark [ text white ] ];
            ]
        [
          txt
            "When you enable high contrast in your OS settings, this content \
             adapts automatically. Borders become thicker, and colors become \
             more distinct.";
        ];
      contrast_buttons;
    ]

(* Motion preferences section *)
let motion_demo_label = Tw.[ text_sm; text gray; dark [ text ~shade:400 gray ] ]

let motion_demo_item ~styles label =
  div
    ~tw:Tw.[ flex; flex_col; items_center; gap 2 ]
    [ div ~tw:styles []; span ~tw:motion_demo_label [ txt label ] ]

let motion_demos =
  div
    ~tw:Tw.[ flex; gap 8; items_center; justify_center; flex_wrap ]
    [
      motion_demo_item
        ~styles:
          Tw.
            [
              w 12;
              h 12;
              bg blue;
              rounded_full;
              motion_safe [ animate_pulse ];
              motion_reduce [ opacity 100 ];
            ]
        "Pulse";
      motion_demo_item
        ~styles:
          Tw.
            [
              w 12;
              h 12;
              border_lg;
              border_color ~shade:600 blue;
              rounded_full;
              motion_safe [ animate_spin ];
            ]
        "Spinner";
      motion_demo_item
        ~styles:
          Tw.
            [
              w 12;
              h 12;
              bg green;
              rounded_lg;
              motion_safe [ animate_bounce ];
              motion_reduce [ opacity 100 ];
            ]
        "Bounce";
    ]

let motion_section =
  section
    ~at:[ At.v "aria-labelledby" "motion-heading" ]
    ~tw:Tw.[ bg white; p 6; rounded_xl; shadow_sm; dark [ bg ~shade:800 gray ] ]
    [
      h2
        ~at:[ At.id "motion-heading" ]
        ~tw:
          Tw.
            [
              text_xl;
              font_bold;
              text ~shade:900 gray;
              mb 4;
              dark [ text white ];
            ]
        [ txt "Motion Preferences" ];
      p
        ~tw:Tw.[ text ~shade:600 gray; dark [ text ~shade:300 gray ]; mb 6 ]
        [
          txt
            "These animations respect the prefers-reduced-motion media query. \
             Users who prefer reduced motion will see still elements instead.";
        ];
      motion_demos;
    ]

(* Color scheme section *)
let color_section =
  section
    ~at:[ At.v "aria-labelledby" "color-heading" ]
    ~tw:Tw.[ bg white; p 6; rounded_xl; shadow_sm; dark [ bg ~shade:800 gray ] ]
    [
      h2
        ~at:[ At.id "color-heading" ]
        ~tw:
          Tw.
            [
              text_xl;
              font_bold;
              text ~shade:900 gray;
              mb 4;
              dark [ text white ];
            ]
        [ txt "Dark Mode Support" ];
      p
        ~tw:Tw.[ text ~shade:600 gray; dark [ text ~shade:300 gray ]; mb 6 ]
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
             ("Light", Tw.(bg ~shade:100 gray), Tw.(text ~shade:800 gray));
             ("Primary", Tw.(bg ~shade:600 blue), Tw.(text white));
             ("Success", Tw.(bg ~shade:600 green), Tw.(text white));
             ("Warning", Tw.(bg amber), Tw.(text black));
           ]);
    ]

(* Focus states demonstration *)
let focus_examples =
  div
    ~tw:Tw.[ flex; flex_wrap; gap 4 ]
    [
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
              border_color ~shade:300 gray;
              focus [ ring_md; ring_color blue; outline_none ];
              dark
                [ bg ~shade:700 gray; border_color ~shade:600 gray; text white ];
            ]
        ();
      a
        ~at:[ At.href "#" ]
        ~tw:
          Tw.
            [
              px 4;
              py 2;
              text ~shade:600 blue;
              underline;
              rounded;
              focus [ ring_md; ring_color blue ];
              dark [ text ~shade:400 blue ];
            ]
        [ txt "Focusable Link" ];
      button
        ~tw:
          Tw.
            [
              px 4;
              py 2;
              bg ~shade:600 blue;
              text white;
              rounded_lg;
              focus [ ring_md; ring_color ~shade:400 blue ];
            ]
        [ txt "Button" ];
    ]

let focus_section =
  section
    ~at:[ At.v "aria-labelledby" "focus-heading" ]
    ~tw:Tw.[ bg white; p 6; rounded_xl; shadow_sm; dark [ bg ~shade:800 gray ] ]
    [
      h2
        ~at:[ At.id "focus-heading" ]
        ~tw:
          Tw.
            [
              text_xl;
              font_bold;
              text ~shade:900 gray;
              mb 4;
              dark [ text white ];
            ]
        [ txt "Focus States for Keyboard Navigation" ];
      p
        ~tw:Tw.[ text ~shade:600 gray; dark [ text ~shade:300 gray ]; mb 6 ]
        [
          txt
            "Use Tab to navigate these interactive elements. Each has a clear \
             focus indicator for keyboard users.";
        ];
      focus_examples;
    ]

(* Screen reader content section *)
let sr_button_style =
  Tw.
    [
      flex;
      items_center;
      gap 2;
      px 4;
      py 2;
      bg white;
      rounded_lg;
      shadow_sm;
      hover [ shadow_md ];
      focus [ ring_md; ring_color blue ];
      dark [ bg ~shade:800 gray ];
    ]

let sr_buttons =
  div
    ~tw:Tw.[ flex; gap 4; flex_wrap ]
    [
      button ~tw:sr_button_style
        [
          span ~tw:Tw.[ text_xl ] [ txt "!" ];
          span ~tw:Tw.[ sr_only ] [ txt "Important notification" ];
          span
            ~tw:Tw.[ text ~shade:700 gray; dark [ text ~shade:300 gray ] ]
            [ txt "Alert" ];
        ];
      button ~tw:sr_button_style
        [
          span ~tw:Tw.[ text_xl ] [ txt "i" ];
          span ~tw:Tw.[ sr_only ] [ txt "More information" ];
          span
            ~tw:Tw.[ text ~shade:700 gray; dark [ text ~shade:300 gray ] ]
            [ txt "Info" ];
        ];
    ]

let sr_section =
  section
    ~at:[ At.v "aria-labelledby" "sr-heading" ]
    ~tw:Tw.[ bg ~shade:50 blue; p 6; rounded_xl; dark [ bg ~shade:800 gray ] ]
    [
      h2
        ~at:[ At.id "sr-heading" ]
        ~tw:
          Tw.
            [
              text_xl;
              font_bold;
              text ~shade:900 gray;
              mb 4;
              dark [ text white ];
            ]
        [ txt "Screen Reader Utilities" ];
      p
        ~tw:Tw.[ text ~shade:600 gray; dark [ text ~shade:300 gray ]; mb 6 ]
        [
          txt
            "The sr-only class hides content visually while keeping it \
             accessible to screen readers.";
        ];
      sr_buttons;
      p
        ~tw:Tw.[ text_sm; text gray; mt 4; dark [ text ~shade:400 gray ] ]
        [
          txt
            "Screen reader users will hear 'Important notification Alert' and \
             'More information Info' for these buttons.";
        ];
    ]

(* Main page *)
let page_intro =
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
              text ~shade:900 gray;
              mb 4;
              dark [ text white ];
            ]
        [ txt "Building Accessible Interfaces" ];
      p
        ~tw:
          Tw.
            [
              text_lg;
              text ~shade:600 gray;
              max_w_2xl;
              mx_auto;
              dark [ text ~shade:400 gray ];
            ]
        [
          txt
            "This demo showcases Tailwind accessibility utilities for \
             contrast, motion, focus states, and screen reader support.";
        ];
    ]

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
              page_intro;
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
