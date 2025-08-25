(** Tailwind CSS v3 Core Features Demo

    This example showcases all the core features available in Tailwind v3:
    - Complete utility classes (spacing, colors, typography, etc.)
    - Flexbox and Grid layouts
    - Responsive design breakpoints
    - Pseudo-class variants (hover, focus, active, etc.)
    - Dark mode support
    - Animations and transitions
    - Forms and components
    - Filters and effects *)

open Tw_html

(* Typography showcase *)
let typography_showcase =
  div
    ~tw:Tw.[ p 6; bg_white; rounded_lg; shadow_md ]
    [
      h3 ~tw:Tw.[ text_2xl; font_bold; mb 4 ] [ txt "Typography" ];
      (* Font sizes *)
      div
        ~tw:Tw.[ flex; flex_col; gap 2 ]
        [
          p ~tw:Tw.[ text_xs ] [ txt "text-xs: Extra small text" ];
          p ~tw:Tw.[ text_sm ] [ txt "text-sm: Small text" ];
          p ~tw:Tw.[ text_base ] [ txt "text-base: Base text" ];
          p ~tw:Tw.[ text_lg ] [ txt "text-lg: Large text" ];
          p ~tw:Tw.[ text_xl ] [ txt "text-xl: Extra large text" ];
          p ~tw:Tw.[ text_2xl ] [ txt "text-2xl: 2X large text" ];
        ];
      (* Font weights *)
      div
        ~tw:Tw.[ flex; flex_col; gap 2; mt 4 ]
        [
          p ~tw:Tw.[ font_thin ] [ txt "font-thin: Thin weight" ];
          p ~tw:Tw.[ font_light ] [ txt "font-light: Light weight" ];
          p ~tw:Tw.[ font_normal ] [ txt "font-normal: Normal weight" ];
          p ~tw:Tw.[ font_medium ] [ txt "font-medium: Medium weight" ];
          p ~tw:Tw.[ font_semibold ] [ txt "font-semibold: Semibold weight" ];
          p ~tw:Tw.[ font_bold ] [ txt "font-bold: Bold weight" ];
        ];
      (* Text alignment *)
      div
        ~tw:Tw.[ flex; flex_col; gap 2; mt 4 ]
        [
          p ~tw:Tw.[ text_left ] [ txt "text-left: Left aligned" ];
          p ~tw:Tw.[ text_center ] [ txt "text-center: Center aligned" ];
          p ~tw:Tw.[ text_right ] [ txt "text-right: Right aligned" ];
        ];
      (* Text decoration *)
      div
        ~tw:Tw.[ flex; flex_col; gap 2; mt 4 ]
        [
          p ~tw:Tw.[ underline ] [ txt "underline: Underlined text" ];
          p ~tw:Tw.[ line_through ] [ txt "line-through: Strike through" ];
          p ~tw:Tw.[ uppercase ] [ txt "uppercase: Uppercase text" ];
          p ~tw:Tw.[ lowercase ] [ txt "LOWERCASE: Lowercase text" ];
          p ~tw:Tw.[ capitalize ] [ txt "capitalize: capitalized text" ];
        ];
    ]

(* Color palette showcase *)
let color_showcase =
  div
    ~tw:Tw.[ p 6; bg_white; rounded_lg; shadow_md ]
    [
      h3 ~tw:Tw.[ text_2xl; font_bold; mb 4 ] [ txt "Color Palette" ];
      (* Basic colors *)
      div
        ~tw:
          Tw.
            [
              grid;
              grid_cols 2;
              on_sm [ grid_cols 4 ];
              on_lg [ grid_cols 8 ];
              gap 2;
            ]
        [
          div ~tw:Tw.[ h 16; bg red 500; rounded_md ] [];
          div ~tw:Tw.[ h 16; bg orange 500; rounded_md ] [];
          div ~tw:Tw.[ h 16; bg yellow 500; rounded_md ] [];
          div ~tw:Tw.[ h 16; bg green 500; rounded_md ] [];
          div ~tw:Tw.[ h 16; bg blue 500; rounded_md ] [];
          div ~tw:Tw.[ h 16; bg indigo 500; rounded_md ] [];
          div ~tw:Tw.[ h 16; bg purple 500; rounded_md ] [];
          div ~tw:Tw.[ h 16; bg pink 500; rounded_md ] [];
        ];
      (* Gray scale *)
      div
        ~tw:Tw.[ grid; grid_cols 5; on_lg [ grid_cols 10 ]; gap 2; mt 4 ]
        [
          div ~tw:Tw.[ h 12; bg gray 50; border ] [];
          div ~tw:Tw.[ h 12; bg gray 100 ] [];
          div ~tw:Tw.[ h 12; bg gray 200 ] [];
          div ~tw:Tw.[ h 12; bg gray 300 ] [];
          div ~tw:Tw.[ h 12; bg gray 400 ] [];
          div ~tw:Tw.[ h 12; bg gray 500 ] [];
          div ~tw:Tw.[ h 12; bg gray 600 ] [];
          div ~tw:Tw.[ h 12; bg gray 700 ] [];
          div ~tw:Tw.[ h 12; bg gray 800 ] [];
          div ~tw:Tw.[ h 12; bg gray 900 ] [];
        ];
    ]

(* Spacing showcase *)
let spacing_showcase =
  div
    ~tw:Tw.[ p 6; bg_white; rounded_lg; shadow_md ]
    [
      h3 ~tw:Tw.[ text_2xl; font_bold; mb 4 ] [ txt "Spacing System" ];
      (* Padding examples *)
      div
        ~tw:Tw.[ flex; flex_col; gap 2 ]
        [
          div ~tw:Tw.[ p 0; bg blue 100; border ] [ txt "p-0" ];
          div ~tw:Tw.[ p 1; bg blue 100; border ] [ txt "p-1" ];
          div ~tw:Tw.[ p 2; bg blue 100; border ] [ txt "p-2" ];
          div ~tw:Tw.[ p 4; bg blue 100; border ] [ txt "p-4" ];
          div ~tw:Tw.[ p 8; bg blue 100; border ] [ txt "p-8" ];
        ];
      (* Margin examples *)
      div
        ~tw:Tw.[ bg gray 100; p 4; mt 4 ]
        [
          div ~tw:Tw.[ m 0; bg green 100; p 2 ] [ txt "m-0" ];
          div ~tw:Tw.[ m 2; bg green 100; p 2 ] [ txt "m-2" ];
          div ~tw:Tw.[ m 4; bg green 100; p 2 ] [ txt "m-4" ];
        ];
    ]

(* Flexbox layout *)
let flexbox_showcase =
  div
    ~tw:Tw.[ p 6; bg_white; rounded_lg; shadow_md ]
    [
      h3 ~tw:Tw.[ text_2xl; font_bold; mb 4 ] [ txt "Flexbox Layout" ];
      (* Flex direction *)
      div
        [
          p ~tw:Tw.[ text_sm; text gray 600; mb 2 ] [ txt "flex-row:" ];
          div
            ~tw:Tw.[ flex; flex_row; gap 2; mb 4 ]
            [
              div ~tw:Tw.[ p 2; bg blue 200 ] [ txt "1" ];
              div ~tw:Tw.[ p 2; bg blue 300 ] [ txt "2" ];
              div ~tw:Tw.[ p 2; bg blue 400 ] [ txt "3" ];
            ];
        ];
      div
        [
          p ~tw:Tw.[ text_sm; text gray 600; mb 2 ] [ txt "flex-col:" ];
          div
            ~tw:Tw.[ flex; flex_col; gap 2; mb 4 ]
            [
              div ~tw:Tw.[ p 2; bg green 200 ] [ txt "1" ];
              div ~tw:Tw.[ p 2; bg green 300 ] [ txt "2" ];
              div ~tw:Tw.[ p 2; bg green 400 ] [ txt "3" ];
            ];
        ];
      (* Justify content *)
      div
        [
          p ~tw:Tw.[ text_sm; text gray 600; mb 2 ] [ txt "justify-between:" ];
          div
            ~tw:Tw.[ flex; justify_between; bg gray 100; p 2 ]
            [
              div ~tw:Tw.[ p 2; bg purple 200 ] [ txt "Start" ];
              div ~tw:Tw.[ p 2; bg purple 400 ] [ txt "End" ];
            ];
        ];
      (* Align items *)
      div
        ~tw:Tw.[ mt 4 ]
        [
          p ~tw:Tw.[ text_sm; text gray 600; mb 2 ] [ txt "items-center:" ];
          div
            ~tw:Tw.[ flex; items_center; h 20; bg gray 100 ]
            [
              div ~tw:Tw.[ p 2; bg red 200 ] [ txt "Centered" ];
              div ~tw:Tw.[ p 4; bg red 300 ] [ txt "Items" ];
            ];
        ];
    ]

(* Grid layout *)
let grid_showcase =
  div
    ~tw:Tw.[ p 6; bg_white; rounded_lg; shadow_md ]
    [
      h3 ~tw:Tw.[ text_2xl; font_bold; mb 4 ] [ txt "Grid Layout" ];
      (* Basic grid *)
      div
        ~tw:Tw.[ grid; grid_cols 3; gap 2; mb 4 ]
        [
          div ~tw:Tw.[ p 4; bg indigo 100; text_center ] [ txt "1" ];
          div ~tw:Tw.[ p 4; bg indigo 200; text_center ] [ txt "2" ];
          div ~tw:Tw.[ p 4; bg indigo 300; text_center ] [ txt "3" ];
          div ~tw:Tw.[ p 4; bg indigo 400; text_center ] [ txt "4" ];
          div ~tw:Tw.[ p 4; bg indigo 500; text_center; text_white ] [ txt "5" ];
          div ~tw:Tw.[ p 4; bg indigo 600; text_center; text_white ] [ txt "6" ];
        ];
      (* Responsive grid *)
      p
        ~tw:Tw.[ text_sm; text gray 600; mb 2 ]
        [ txt "Responsive grid (resize window):" ];
      div
        ~tw:
          Tw.
            [
              grid;
              grid_cols 1;
              on_sm [ grid_cols 2 ];
              on_lg [ grid_cols 4 ];
              gap 4;
            ]
        [
          div ~tw:Tw.[ p 4; bg teal 200; text_center ] [ txt "A" ];
          div ~tw:Tw.[ p 4; bg teal 300; text_center ] [ txt "B" ];
          div ~tw:Tw.[ p 4; bg teal 400; text_center ] [ txt "C" ];
          div ~tw:Tw.[ p 4; bg teal 500; text_center; text_white ] [ txt "D" ];
        ];
    ]

(* Borders and shadows *)
let borders_shadows_showcase =
  div
    ~tw:Tw.[ p 6; bg_white; rounded_lg; shadow_md ]
    [
      h3 ~tw:Tw.[ text_2xl; font_bold; mb 4 ] [ txt "Borders & Shadows" ];
      (* Border widths *)
      div
        ~tw:Tw.[ grid; grid_cols 2; on_sm [ grid_cols 4 ]; gap 4; mb 4 ]
        [
          div ~tw:Tw.[ p 4; border; text_center ] [ txt "border" ];
          div ~tw:Tw.[ p 4; border_md; text_center ] [ txt "border-2" ];
          div ~tw:Tw.[ p 4; border_lg; text_center ] [ txt "border-4" ];
          div ~tw:Tw.[ p 4; border_xl; text_center ] [ txt "border-8" ];
        ];
      (* Border styles *)
      div
        ~tw:Tw.[ grid; grid_cols 2; on_sm [ grid_cols 3 ]; gap 4; mb 4 ]
        [
          div
            ~tw:Tw.[ p 4; border_md; border_solid; text_center ]
            [ txt "solid" ];
          div
            ~tw:Tw.[ p 4; border_md; border_dashed; text_center ]
            [ txt "dashed" ];
          div
            ~tw:Tw.[ p 4; border_md; border_dotted; text_center ]
            [ txt "dotted" ];
        ];
      (* Border radius *)
      div
        ~tw:Tw.[ grid; grid_cols 2; on_sm [ grid_cols 4 ]; gap 4; mb 4 ]
        [
          div ~tw:Tw.[ p 4; bg gray 200; text_center ] [ txt "none" ];
          div ~tw:Tw.[ p 4; bg gray 200; rounded_sm; text_center ] [ txt "sm" ];
          div ~tw:Tw.[ p 4; bg gray 200; rounded_md; text_center ] [ txt "md" ];
          div ~tw:Tw.[ p 4; bg gray 200; rounded_lg; text_center ] [ txt "lg" ];
        ];
      (* Shadows *)
      div
        ~tw:Tw.[ grid; grid_cols 2; on_sm [ grid_cols 3 ]; gap 4 ]
        [
          div
            ~tw:Tw.[ p 4; bg_white; shadow_sm; text_center ]
            [ txt "shadow-sm" ];
          div
            ~tw:Tw.[ p 4; bg_white; shadow_md; text_center ]
            [ txt "shadow-md" ];
          div
            ~tw:Tw.[ p 4; bg_white; shadow_lg; text_center ]
            [ txt "shadow-lg" ];
        ];
    ]

(* Interactive states *)
let interactive_showcase =
  div
    ~tw:Tw.[ p 6; bg_white; rounded_lg; shadow_md ]
    [
      h3 ~tw:Tw.[ text_2xl; font_bold; mb 4 ] [ txt "Interactive States" ];
      (* Hover effects *)
      div
        ~tw:Tw.[ flex; flex_col; gap 4 ]
        [
          button
            ~tw:
              Tw.
                [
                  px 4;
                  py 2;
                  bg blue 500;
                  text_white;
                  rounded_md;
                  on_hover [ bg blue 600 ];
                  transition_colors;
                ]
            [ txt "Hover me" ];
          (* Focus states *)
          input
            ~tw:
              Tw.
                [
                  w_full;
                  px 3;
                  py 2;
                  border;
                  rounded_md;
                  on_focus
                    [ border_color blue 500; ring_md; ring_color blue 200 ];
                ]
            ~at:[ At.type' "text"; At.placeholder "Focus me" ]
            ();
          (* Active states *)
          button
            ~tw:
              Tw.
                [
                  px 4;
                  py 2;
                  bg green 500;
                  text_white;
                  rounded_md;
                  on_active [ bg green 700 ];
                  transition_colors;
                ]
            [ txt "Click and hold" ];
          (* Disabled states *)
          button
            ~tw:
              Tw.
                [
                  px 4;
                  py 2;
                  bg gray 500;
                  text_white;
                  rounded_md;
                  opacity 50;
                  cursor_not_allowed;
                ]
            ~at:[ At.disabled ]
            [ txt "Disabled" ];
        ];
    ]

(* Transitions and animations *)
let animations_showcase =
  div
    ~tw:Tw.[ p 6; bg_white; rounded_lg; shadow_md ]
    [
      h3 ~tw:Tw.[ text_2xl; font_bold; mb 4 ] [ txt "Animations & Transitions" ];
      (* Transitions *)
      div
        ~tw:Tw.[ grid; grid_cols 2; on_sm [ grid_cols 3 ]; gap 4; mb 4 ]
        [
          div
            ~tw:
              Tw.
                [
                  p 4;
                  bg purple 500;
                  text_white;
                  text_center;
                  rounded_md;
                  on_hover [ scale 110 ];
                  transition_transform;
                  duration 300;
                ]
            [ txt "Scale" ];
          div
            ~tw:
              Tw.
                [
                  p 4;
                  bg pink 500;
                  text_white;
                  text_center;
                  rounded_md;
                  on_hover [ rotate 45 ];
                  transition_transform;
                  duration 300;
                ]
            [ txt "Rotate" ];
          div
            ~tw:
              Tw.
                [
                  p 4;
                  bg yellow 500;
                  text_center;
                  rounded_md;
                  on_hover [ opacity 50 ];
                  transition_opacity;
                  duration 300;
                ]
            [ txt "Fade" ];
        ];
      (* Built-in animations *)
      div
        ~tw:Tw.[ grid; grid_cols 2; on_sm [ grid_cols 4 ]; gap 4 ]
        [
          div
            ~tw:
              Tw.
                [
                  p 4;
                  bg blue 500;
                  text_white;
                  text_center;
                  rounded_md;
                  animate_pulse;
                ]
            [ txt "Pulse" ];
          div
            ~tw:
              Tw.
                [
                  p 4;
                  bg green 500;
                  text_white;
                  text_center;
                  rounded_md;
                  animate_bounce;
                ]
            [ txt "Bounce" ];
          div
            ~tw:
              Tw.
                [
                  p 4;
                  bg red 500;
                  text_white;
                  text_center;
                  rounded_md;
                  animate_spin;
                ]
            [ txt "Spin" ];
          div
            ~tw:
              Tw.
                [
                  p 4;
                  bg purple 500;
                  text_white;
                  text_center;
                  rounded_md;
                  animate_ping;
                ]
            [ txt "Ping" ];
        ];
    ]

(* Forms *)
let forms_showcase =
  div
    ~tw:Tw.[ p 6; bg_white; rounded_lg; shadow_md ]
    [
      h3 ~tw:Tw.[ text_2xl; font_bold; mb 4 ] [ txt "Form Elements" ];
      form
        ~tw:Tw.[ flex; flex_col; gap 4 ]
        [
          (* Text input *)
          div
            [
              label
                ~tw:Tw.[ block; text_sm; font_medium; mb 1 ]
                [ txt "Text Input:" ];
              input
                ~tw:
                  Tw.
                    [
                      w_full;
                      px 3;
                      py 2;
                      border;
                      rounded_md;
                      on_focus
                        [ border_color blue 500; ring_sm; ring_color blue 200 ];
                    ]
                ~at:[ At.type' "text"; At.placeholder "Enter text" ]
                ();
            ];
          (* Select *)
          div
            [
              label
                ~tw:Tw.[ block; text_sm; font_medium; mb 1 ]
                [ txt "Select:" ];
              select
                ~tw:Tw.[ w_full; px 3; py 2; border; rounded_md ]
                [
                  option [ txt "Option 1" ];
                  option [ txt "Option 2" ];
                  option [ txt "Option 3" ];
                ];
            ];
          (* Checkbox *)
          div
            ~tw:Tw.[ flex; items_center ]
            [
              input
                ~tw:Tw.[ mr 2 ]
                ~at:[ At.type' "checkbox"; At.id "check1" ]
                ();
              label ~at:[ At.for' "check1" ] [ txt "Checkbox option" ];
            ];
          (* Radio buttons *)
          div
            ~tw:Tw.[ flex; flex_col; gap 2 ]
            [
              div
                ~tw:Tw.[ flex; items_center ]
                [
                  input
                    ~tw:Tw.[ mr 2 ]
                    ~at:[ At.type' "radio"; At.name "radio"; At.id "radio1" ]
                    ();
                  label ~at:[ At.for' "radio1" ] [ txt "Radio option 1" ];
                ];
              div
                ~tw:Tw.[ flex; items_center ]
                [
                  input
                    ~tw:Tw.[ mr 2 ]
                    ~at:[ At.type' "radio"; At.name "radio"; At.id "radio2" ]
                    ();
                  label ~at:[ At.for' "radio2" ] [ txt "Radio option 2" ];
                ];
            ];
          (* Textarea *)
          div
            [
              label
                ~tw:Tw.[ block; text_sm; font_medium; mb 1 ]
                [ txt "Textarea:" ];
              textarea
                ~tw:Tw.[ w_full; px 3; py 2; border; rounded_md ]
                ~at:[ At.rows 3; At.placeholder "Enter multiple lines" ]
                [];
            ];
          (* Submit button *)
          button
            ~tw:
              Tw.
                [
                  w_full;
                  px 4;
                  py 2;
                  bg blue 600;
                  text_white;
                  rounded_md;
                  on_hover [ bg blue 700 ];
                  transition_colors;
                ]
            ~at:[ At.type' "submit" ]
            [ txt "Submit" ];
        ];
    ]

(* Responsive utilities *)
let responsive_showcase =
  div
    ~tw:Tw.[ p 6; bg_white; rounded_lg; shadow_md ]
    [
      h3 ~tw:Tw.[ text_2xl; font_bold; mb 4 ] [ txt "Responsive Design" ];
      p
        ~tw:Tw.[ text gray 600; mb 4 ]
        [ txt "Resize your browser to see changes:" ];
      (* Responsive text *)
      p
        ~tw:
          Tw.
            [
              text_xs;
              on_sm [ text_sm ];
              on_md [ text_base ];
              on_lg [ text_lg ];
              on_xl [ text_xl ];
              on_2xl [ text_2xl ];
            ]
        [ txt "This text grows with screen size" ];
      (* Responsive colors *)
      div
        ~tw:
          Tw.
            [
              p 4;
              mt 4;
              rounded_md;
              bg red 500;
              on_sm [ bg yellow 500 ];
              on_md [ bg green 500 ];
              on_lg [ bg blue 500 ];
              on_xl [ bg purple 500 ];
            ]
        [
          p
            ~tw:Tw.[ text_white; text_center ]
            [ txt "Background changes with screen size" ];
        ];
      (* Show/hide elements *)
      div
        ~tw:Tw.[ mt 4; flex; flex_col; gap 2 ]
        [
          p
            ~tw:Tw.[ block; on_sm [ hidden ] ]
            [ txt "📱 Visible only on mobile" ];
          p
            ~tw:Tw.[ hidden; on_sm [ block ]; on_lg [ hidden ] ]
            [ txt "💻 Visible on small to medium screens" ];
          p
            ~tw:Tw.[ hidden; on_lg [ block ] ]
            [ txt "🖥️ Visible on large screens and up" ];
        ];
    ]

(* Filters and effects *)
let filters_showcase =
  div
    ~tw:Tw.[ p 6; bg_white; rounded_lg; shadow_md ]
    [
      h3 ~tw:Tw.[ text_2xl; font_bold; mb 4 ] [ txt "Filters & Effects" ];
      (* Blur *)
      div
        ~tw:Tw.[ grid; grid_cols 2; on_sm [ grid_cols 3 ]; gap 4; mb 4 ]
        [
          img
            ~tw:Tw.[ w_full; h 24; bg gray 300; rounded_md; object_cover ]
            ~at:[ At.src "https://via.placeholder.com/150"; At.alt "Normal" ]
            ();
          img
            ~tw:
              Tw.
                [ w_full; h 24; bg gray 300; rounded_md; object_cover; blur_sm ]
            ~at:[ At.src "https://via.placeholder.com/150"; At.alt "Blur sm" ]
            ();
          img
            ~tw:
              Tw.
                [ w_full; h 24; bg gray 300; rounded_md; object_cover; blur_md ]
            ~at:[ At.src "https://via.placeholder.com/150"; At.alt "Blur md" ]
            ();
        ];
      (* Opacity *)
      div
        ~tw:Tw.[ grid; grid_cols 4; gap 2; mb 4 ]
        [
          div
            ~tw:Tw.[ p 4; bg blue 500; text_white; text_center; opacity 100 ]
            [ txt "100%" ];
          div
            ~tw:Tw.[ p 4; bg blue 500; text_white; text_center; opacity 75 ]
            [ txt "75%" ];
          div
            ~tw:Tw.[ p 4; bg blue 500; text_white; text_center; opacity 50 ]
            [ txt "50%" ];
          div
            ~tw:Tw.[ p 4; bg blue 500; text_white; text_center; opacity 25 ]
            [ txt "25%" ];
        ];
      (* Mix blend modes *)
      div
        ~tw:
          Tw.
            [
              relative;
              h 32;
              bg_gradient_to Right;
              from_color blue ~shade:500;
              to_color purple ~shade:500;
            ]
        [
          div
            ~tw:
              Tw.
                [
                  absolute;
                  top 4;
                  left 4;
                  right 4;
                  bottom 4;
                  bg_white;
                  flex;
                  items_center;
                  justify_center;
                ]
            [ txt "Mix Blend" ];
        ];
    ]

(* Gradients *)
let gradients_showcase =
  div
    ~tw:Tw.[ p 6; bg_white; rounded_lg; shadow_md ]
    [
      h3 ~tw:Tw.[ text_2xl; font_bold; mb 4 ] [ txt "Gradients" ];
      div
        ~tw:Tw.[ flex; flex_col; gap 4 ]
        [
          (* Linear gradients *)
          div
            ~tw:
              Tw.
                [
                  p 4;
                  rounded_md;
                  text_white;
                  text_center;
                  bg_gradient_to Right;
                  from_color blue ~shade:500;
                  to_color purple ~shade:600;
                ]
            [ txt "Linear gradient (to-r)" ];
          div
            ~tw:
              Tw.
                [
                  p 4;
                  rounded_md;
                  text_white;
                  text_center;
                  bg_gradient_to Bottom_right;
                  from_color green ~shade:400;
                  to_color blue ~shade:600;
                ]
            [ txt "Diagonal gradient (to-br)" ];
          div
            ~tw:
              Tw.
                [
                  p 4;
                  rounded_md;
                  text_white;
                  text_center;
                  bg_gradient_to Top;
                  from_color pink ~shade:500;
                  to_color yellow ~shade:500;
                ]
            [ txt "Vertical gradient (to-t)" ];
        ];
    ]

(* Position utilities *)
let position_showcase =
  div
    ~tw:Tw.[ p 6; bg_white; rounded_lg; shadow_md ]
    [
      h3 ~tw:Tw.[ text_2xl; font_bold; mb 4 ] [ txt "Position & Z-Index" ];
      (* Relative/Absolute positioning *)
      div
        ~tw:Tw.[ relative; h 32; bg gray 100; rounded_md; mb 4 ]
        [
          div
            ~tw:
              Tw.
                [
                  absolute; top 2; left 2; p 2; bg blue 500; text_white; text_sm;
                ]
            [ txt "Top Left" ];
          div
            ~tw:
              Tw.
                [
                  absolute;
                  top 2;
                  right 2;
                  p 2;
                  bg green 500;
                  text_white;
                  text_sm;
                ]
            [ txt "Top Right" ];
          div
            ~tw:
              Tw.
                [
                  absolute;
                  bottom 2;
                  left 2;
                  p 2;
                  bg red 500;
                  text_white;
                  text_sm;
                ]
            [ txt "Bottom Left" ];
          div
            ~tw:
              Tw.
                [
                  absolute;
                  bottom 2;
                  right 2;
                  p 2;
                  bg purple 500;
                  text_white;
                  text_sm;
                ]
            [ txt "Bottom Right" ];
          div
            ~tw:
              Tw.
                [
                  absolute;
                  top_1_2;
                  left_1_2;
                  transform;
                  neg_translate_x_1_2;
                  neg_translate_y_1_2;
                  p 2;
                  bg yellow 500;
                  text_sm;
                ]
            [ txt "Center" ];
        ];
      (* Z-index layers *)
      div
        ~tw:Tw.[ relative; h 24 ]
        [
          div ~tw:Tw.[ absolute; inset_0; bg red 300; z 10; p 2 ] [ txt "z-10" ];
          div
            ~tw:
              Tw.
                [
                  absolute;
                  top 2;
                  left 2;
                  right 2;
                  bottom 2;
                  bg blue 300;
                  z 20;
                  p 2;
                ]
            [ txt "z-20" ];
          div
            ~tw:
              Tw.
                [
                  absolute;
                  top 4;
                  left 4;
                  right 4;
                  bottom 4;
                  bg green 300;
                  z 30;
                  p 2;
                ]
            [ txt "z-30" ];
        ];
    ]

(* Utilities showcase *)
let utilities_showcase =
  div
    ~tw:Tw.[ p 6; bg_white; rounded_lg; shadow_md ]
    [
      h3 ~tw:Tw.[ text_2xl; font_bold; mb 4 ] [ txt "Other Utilities" ];
      (* Overflow *)
      div
        ~tw:Tw.[ grid; grid_cols 2; on_sm [ grid_cols 3 ]; gap 4; mb 4 ]
        [
          div
            ~tw:Tw.[ h 20; p 2; bg gray 100; overflow_hidden ]
            [
              p
                [
                  txt
                    "overflow-hidden: This is a long text that will be clipped \
                     when it overflows the container boundaries.";
                ];
            ];
          div
            ~tw:Tw.[ h 20; p 2; bg gray 100; overflow_auto ]
            [
              p
                [
                  txt
                    "overflow-auto: This is a long text that will show \
                     scrollbars when it overflows the container boundaries.";
                ];
            ];
          div
            ~tw:Tw.[ h 20; p 2; bg gray 100; overflow_scroll ]
            [ p [ txt "overflow-scroll: Always shows scrollbars." ] ];
        ];
      (* Cursor styles *)
      div
        ~tw:Tw.[ grid; grid_cols 2; on_sm [ grid_cols 4 ]; gap 2; mb 4 ]
        [
          div
            ~tw:Tw.[ p 2; bg blue 100; cursor_pointer; text_center ]
            [ txt "pointer" ];
          div
            ~tw:Tw.[ p 2; bg blue 100; cursor_move; text_center ]
            [ txt "move" ];
          div
            ~tw:Tw.[ p 2; bg blue 100; cursor_not_allowed; text_center ]
            [ txt "not-allowed" ];
          div
            ~tw:Tw.[ p 2; bg blue 100; cursor_wait; text_center ]
            [ txt "wait" ];
        ];
      (* User select *)
      div
        ~tw:Tw.[ grid; grid_cols 2; gap 4 ]
        [
          div
            ~tw:Tw.[ p 2; bg green 100; select_none ]
            [ txt "select-none: Can't select this text" ];
          div
            ~tw:Tw.[ p 2; bg green 100; select_all ]
            [ txt "select-all: Selects all on click" ];
        ];
    ]

(* Main page *)
let () =
  let main_page =
    page ~title:"Tailwind CSS v3 Core Features" ~tw_css:"v3.css" []
      [
        div
          ~tw:Tw.[ min_h_screen; bg gray 50 ]
          [
            (* Header *)
            header
              ~tw:Tw.[ bg_white; shadow_sm; sticky; top 0; z 50 ]
              [
                div
                  ~tw:Tw.[ max_w_4xl; mx_auto; px 4; py 4 ]
                  [
                    h1
                      ~tw:Tw.[ text_3xl; font_bold; text gray 900 ]
                      [ txt "Tailwind CSS v3 - Complete Feature Demo" ];
                    p
                      ~tw:Tw.[ text gray 600; mt 2 ]
                      [ txt "All core utilities and features in one place" ];
                  ];
              ];
            (* Main content *)
            main
              ~tw:Tw.[ max_w_4xl; mx_auto; px 4; py 8 ]
              [
                (* Two column grid for larger screens *)
                div
                  ~tw:Tw.[ grid; grid_cols 1; on_lg [ grid_cols 2 ]; gap 6 ]
                  [
                    typography_showcase;
                    color_showcase;
                    spacing_showcase;
                    flexbox_showcase;
                    grid_showcase;
                    borders_shadows_showcase;
                    interactive_showcase;
                    animations_showcase;
                    forms_showcase;
                    responsive_showcase;
                    filters_showcase;
                    gradients_showcase;
                    position_showcase;
                    utilities_showcase;
                  ];
              ];
            (* Footer *)
            footer
              ~tw:Tw.[ bg gray 800; text_white; py 8; mt 12 ]
              [
                div
                  ~tw:Tw.[ max_w_4xl; mx_auto; px 4; text_center ]
                  [
                    p
                      [
                        txt "Tailwind CSS v3 Core Features - Complete Reference";
                      ];
                    p
                      ~tw:Tw.[ text gray 400; mt 2 ]
                      [ txt "Generated with OCaml Tw library" ];
                  ];
              ];
          ];
      ]
  in

  (* Generate files *)
  let html_str = html main_page in
  let css_filename, css_stylesheet = css main_page in
  let css_str = Tw.Css.to_string ~minify:true css_stylesheet in

  (* Write HTML file *)
  let oc_html = open_out "v3.html" in
  output_string oc_html html_str;
  close_out oc_html;

  (* Write CSS file *)
  let oc_css = open_out css_filename in
  output_string oc_css css_str;
  close_out oc_css;

  (* Silent during build - no output *)
  ()
