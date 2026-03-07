(** Feature: Animations demo

    Shows various animation utilities including keyframe animations, transitions,
    transforms, @starting-style, and motion preferences. *)

open Tw_html

(* Page header *)
let page_header =
  header
    ~tw:Tw.[ bg_white; shadow; shadow_sm; dark [ bg gray 900 ] ]
    [
      div
        ~tw:Tw.[ max_w_5xl; mx_auto; px 4; py 4 ]
        [
          h1
            ~tw:Tw.[ text_2xl; font_bold; text gray 900; dark [ text_white ] ]
            [ txt "Animations Demo" ];
        ];
    ]

(* Keyframe animations section *)
let anim_label =
  Tw.[ text_sm; font_medium; text gray 700; dark [ text gray 300 ] ]

let keyframe_demos =
  div
    ~tw:Tw.[ grid; grid_cols 2; gap 6; md [ grid_cols 4 ] ]
    [
      div
        ~tw:Tw.[ flex; flex_col; items_center; gap 3 ]
        [
          div
            ~tw:
              Tw.
                [
                  w 16;
                  h 16;
                  rounded_full;
                  border_lg;
                  border_color blue 600;
                  animate_spin;
                ]
            [];
          span ~tw:anim_label [ txt "Spin" ];
          span
            ~tw:Tw.[ text_xs; text gray 500; text_center ]
            [ txt "Loading spinners" ];
        ];
      div
        ~tw:Tw.[ flex; flex_col; items_center; gap 3 ]
        [
          div
            ~tw:Tw.[ relative; w 16; h 16 ]
            [
              div
                ~tw:
                  Tw.
                    [
                      absolute;
                      inset 0;
                      bg green 500;
                      rounded_full;
                      animate_ping;
                    ]
                [];
              div ~tw:Tw.[ absolute; inset 2; bg green 600; rounded_full ] [];
            ];
          span ~tw:anim_label [ txt "Ping" ];
          span
            ~tw:Tw.[ text_xs; text gray 500; text_center ]
            [ txt "Notifications" ];
        ];
      div
        ~tw:Tw.[ flex; flex_col; items_center; gap 3 ]
        [
          div ~tw:Tw.[ w 16; h 16; bg purple 500; rounded_lg; animate_pulse ] [];
          span ~tw:anim_label [ txt "Pulse" ];
          span
            ~tw:Tw.[ text_xs; text gray 500; text_center ]
            [ txt "Skeleton loading" ];
        ];
      div
        ~tw:Tw.[ flex; flex_col; items_center; gap 3 ]
        [
          div
            ~tw:Tw.[ w 16; h 16; bg amber 500; rounded_full; animate_bounce ]
            [];
          span ~tw:anim_label [ txt "Bounce" ];
          span
            ~tw:Tw.[ text_xs; text gray 500; text_center ]
            [ txt "Scroll indicators" ];
        ];
    ]

let keyframe_section =
  section
    ~at:[ At.v "aria-labelledby" "keyframe-heading" ]
    ~tw:Tw.[ bg_white; p 6; rounded_xl; shadow_sm; dark [ bg gray 800 ] ]
    [
      h2
        ~at:[ At.id "keyframe-heading" ]
        ~tw:Tw.[ text_xl; font_bold; text gray 900; mb 4; dark [ text_white ] ]
        [ txt "Keyframe Animations" ];
      p
        ~tw:Tw.[ text gray 600; dark [ text gray 300 ]; mb 6 ]
        [
          txt
            "Built-in keyframe animations for common use cases. These run \
             continuously.";
        ];
      keyframe_demos;
    ]

(* Transitions section *)
let transition_hover_demos =
  div
    ~tw:Tw.[ grid; grid_cols 1; gap 4; md [ grid_cols 3 ] ]
    [
      div
        ~tw:
          Tw.
            [
              p 6;
              bg blue 500;
              text_white;
              rounded_lg;
              text_center;
              font_medium;
              transition;
              transition_colors;
              duration 300;
              hover [ bg blue 700 ];
            ]
        [ txt "Hover for color transition" ];
      div
        ~tw:
          Tw.
            [
              p 6;
              bg green 500;
              text_white;
              rounded_lg;
              text_center;
              font_medium;
              transition_transform;
              duration 300;
              hover [ scale 105 ];
            ]
        [ txt "Hover for scale" ];
      div
        ~tw:
          Tw.
            [
              p 6;
              bg_white;
              text gray 700;
              rounded_lg;
              text_center;
              font_medium;
              shadow_sm;
              transition_shadow;
              duration 300;
              hover [ shadow_xl ];
              dark [ bg gray 700; text_white ];
            ]
        [ txt "Hover for shadow" ];
    ]

let timing_functions_demos =
  div
    ~tw:Tw.[ grid; grid_cols 2; gap 4; md [ grid_cols 4 ] ]
    (List.map
       (fun (name, ease_style) ->
         div
           ~tw:
             Tw.
               [
                 p 4;
                 bg gray 100;
                 rounded_lg;
                 text_center;
                 transition_all;
                 duration 500;
                 ease_style;
                 hover [ bg gray 300; translate_x 2 ];
                 dark [ bg gray 700; hover [ bg gray 600 ] ];
               ]
           [ span ~tw:anim_label [ txt name ] ])
       [
         ("Linear", Tw.ease_linear);
         ("Ease In", Tw.ease_in);
         ("Ease Out", Tw.ease_out);
         ("Ease In-Out", Tw.ease_in_out);
       ])

let transitions_section =
  section
    ~at:[ At.v "aria-labelledby" "transitions-heading" ]
    ~tw:Tw.[ bg_white; p 6; rounded_xl; shadow_sm; dark [ bg gray 800 ] ]
    [
      h2
        ~at:[ At.id "transitions-heading" ]
        ~tw:Tw.[ text_xl; font_bold; text gray 900; mb 4; dark [ text_white ] ]
        [ txt "Transitions" ];
      p
        ~tw:Tw.[ text gray 600; dark [ text gray 300 ]; mb 6 ]
        [
          txt
            "Smooth property transitions on hover. Different timing functions \
             and durations.";
        ];
      transition_hover_demos;
      h3
        ~tw:
          Tw.
            [
              text_lg;
              font_semibold;
              text gray 800;
              mt 8;
              mb 4;
              dark [ text gray 200 ];
            ]
        [ txt "Timing Functions" ];
      timing_functions_demos;
    ]

(* Transform section *)
let transform_demo ~bg_style ~hover_style label =
  div
    ~tw:Tw.[ flex; flex_col; items_center; gap 3 ]
    [
      div
        ~tw:
          Tw.
            [
              w 16;
              h 16;
              bg_style;
              rounded_lg;
              transition_transform;
              duration 300;
              hover_style;
            ]
        [];
      span ~tw:anim_label [ txt label ];
    ]

let transform_demos =
  div
    ~tw:Tw.[ grid; grid_cols 2; gap 6; md [ grid_cols 4 ] ]
    [
      transform_demo
        ~bg_style:Tw.(bg rose 500)
        ~hover_style:Tw.(hover [ rotate 45 ])
        "Rotate 45";
      transform_demo
        ~bg_style:Tw.(bg cyan 500)
        ~hover_style:Tw.(hover [ scale 125 ])
        "Scale 125%";
      transform_demo
        ~bg_style:Tw.(bg indigo 500)
        ~hover_style:Tw.(hover [ translate_y (-2) ])
        "Translate Y";
      div
        ~tw:Tw.[ flex; flex_col; items_center; gap 3 ]
        [
          div
            ~tw:
              Tw.
                [
                  w 16;
                  h 16;
                  bg_gradient_to Right;
                  from_color ~shade:500 pink;
                  to_color ~shade:500 orange;
                  rounded_lg;
                  transition_transform;
                  duration 300;
                  hover [ scale 110; rotate 12 ];
                ]
            [];
          span ~tw:anim_label [ txt "Combined" ];
        ];
    ]

let transform_section =
  section
    ~at:[ At.v "aria-labelledby" "transform-heading" ]
    ~tw:Tw.[ bg_white; p 6; rounded_xl; shadow_sm; dark [ bg gray 800 ] ]
    [
      h2
        ~at:[ At.id "transform-heading" ]
        ~tw:Tw.[ text_xl; font_bold; text gray 900; mb 4; dark [ text_white ] ]
        [ txt "Transforms" ];
      p
        ~tw:Tw.[ text gray 600; dark [ text gray 300 ]; mb 6 ]
        [ txt "Various transformation utilities: rotate, scale, translate." ];
      transform_demos;
    ]

(* Starting style section *)
let starting_card ~start_styles ~end_styles ~bg_styles ~heading_tw ~text_tw
    ~heading ~description =
  div
    ~tw:Tw.(start_styles @ end_styles @ [ p 6; rounded_lg ] @ bg_styles)
    [ h3 ~tw:heading_tw [ txt heading ]; p ~tw:text_tw [ txt description ] ]

let starting_demos =
  div
    ~tw:Tw.[ grid; grid_cols 1; gap 4; md [ grid_cols 2 ] ]
    [
      starting_card
        ~start_styles:Tw.[ starting [ opacity 0 ] ]
        ~end_styles:Tw.[ opacity 100; transition_opacity; duration 700 ]
        ~bg_styles:Tw.[ bg blue 100; dark [ bg blue 900 ] ]
        ~heading_tw:
          Tw.[ font_semibold; text blue 900; mb 2; dark [ text blue 100 ] ]
        ~text_tw:Tw.[ text_sm; text blue 700; dark [ text blue 200 ] ]
        ~heading:"Fade In"
        ~description:"This element fades in from transparent to opaque.";
      starting_card
        ~start_styles:Tw.[ starting [ opacity 0; scale 90 ] ]
        ~end_styles:Tw.[ opacity 100; scale 100; transition_all; duration 500 ]
        ~bg_styles:Tw.[ bg green 100; dark [ bg green 900 ] ]
        ~heading_tw:
          Tw.[ font_semibold; text green 900; mb 2; dark [ text green 100 ] ]
        ~text_tw:Tw.[ text_sm; text green 700; dark [ text green 200 ] ]
        ~heading:"Scale In"
        ~description:"This element scales up while fading in.";
      starting_card
        ~start_styles:Tw.[ starting [ opacity 0; translate_x (-4) ] ]
        ~end_styles:
          Tw.[ opacity 100; translate_x 0; transition_all; duration 500 ]
        ~bg_styles:Tw.[ bg purple 100; dark [ bg purple 900 ] ]
        ~heading_tw:
          Tw.[ font_semibold; text purple 900; mb 2; dark [ text purple 100 ] ]
        ~text_tw:Tw.[ text_sm; text purple 700; dark [ text purple 200 ] ]
        ~heading:"Slide In Left"
        ~description:"This element slides in from the left.";
      starting_card
        ~start_styles:Tw.[ starting [ opacity 0; scale 95 ] ]
        ~end_styles:Tw.[ opacity 100; scale 100; transition_all; duration 700 ]
        ~bg_styles:
          Tw.
            [
              bg_gradient_to Right;
              from_color ~shade:500 pink;
              to_color ~shade:600 purple;
              text_white;
            ]
        ~heading_tw:Tw.[ font_semibold; mb 2 ]
        ~text_tw:Tw.[ text_sm; opacity 90 ]
        ~heading:"Gradient Entry"
        ~description:"Smooth entry with gradient background.";
    ]

let starting_section =
  section
    ~at:[ At.v "aria-labelledby" "starting-heading" ]
    ~tw:Tw.[ bg_white; p 6; rounded_xl; shadow_sm; dark [ bg gray 800 ] ]
    [
      h2
        ~at:[ At.id "starting-heading" ]
        ~tw:Tw.[ text_xl; font_bold; text gray 900; mb 4; dark [ text_white ] ]
        [ txt "@starting-style Entry Animations" ];
      p
        ~tw:Tw.[ text gray 600; dark [ text gray 300 ]; mb 6 ]
        [
          txt
            "CSS @starting-style allows elements to animate from an initial \
             state when first rendered. Refresh the page to see the effect.";
        ];
      starting_demos;
    ]

(* Motion preferences section *)
let motion_card ~title ~anim_styles ~description =
  div
    ~tw:Tw.[ p 6; bg_white; rounded_lg; shadow_sm; dark [ bg gray 700 ] ]
    [
      h3
        ~tw:Tw.[ font_semibold; text gray 800; mb 3; dark [ text_white ] ]
        [ txt title ];
      div
        ~tw:Tw.[ flex; items_center; gap 4 ]
        [
          div ~tw:Tw.([ w 12; h 12; rounded_full ] @ anim_styles) [];
          p
            ~tw:Tw.[ text_sm; text gray 600; dark [ text gray 300 ] ]
            [ txt description ];
        ];
    ]

let motion_demos =
  div
    ~tw:Tw.[ grid; grid_cols 1; gap 4; md [ grid_cols 2 ] ]
    [
      motion_card ~title:"motion-safe"
        ~anim_styles:Tw.[ bg blue 500; motion_safe [ animate_bounce ] ]
        ~description:"Only animates when motion is allowed";
      motion_card ~title:"motion-reduce"
        ~anim_styles:
          Tw.[ bg green 500; animate_spin; motion_reduce [ animate_none ] ]
        ~description:"Stops animation when reduced motion preferred";
    ]

let motion_section =
  section
    ~at:[ At.v "aria-labelledby" "motion-heading" ]
    ~tw:Tw.[ bg blue 50; p 6; rounded_xl; dark [ bg gray 800 ] ]
    [
      h2
        ~at:[ At.id "motion-heading" ]
        ~tw:Tw.[ text_xl; font_bold; text gray 900; mb 4; dark [ text_white ] ]
        [ txt "Motion Preferences" ];
      p
        ~tw:Tw.[ text gray 600; dark [ text gray 300 ]; mb 6 ]
        [
          txt
            "These animations respect prefers-reduced-motion. Enable 'Reduce \
             motion' in your OS to see the difference.";
        ];
      motion_demos;
    ]

(* Duration examples *)
let duration_demos =
  div
    ~tw:Tw.[ flex; flex_wrap; gap 4 ]
    (List.map
       (fun (label, dur) ->
         div
           ~tw:
             Tw.
               [
                 px 6;
                 py 3;
                 bg gray 200;
                 rounded_lg;
                 transition_colors;
                 dur;
                 hover [ bg gray 400 ];
                 dark [ bg gray 700; hover [ bg gray 500 ] ];
               ]
           [ span ~tw:anim_label [ txt label ] ])
       [
         ("75ms", Tw.duration 75);
         ("150ms", Tw.duration 150);
         ("300ms", Tw.duration 300);
         ("500ms", Tw.duration 500);
         ("700ms", Tw.duration 700);
         ("1000ms", Tw.duration 1000);
       ])

let duration_section =
  section
    ~at:[ At.v "aria-labelledby" "duration-heading" ]
    ~tw:Tw.[ bg_white; p 6; rounded_xl; shadow_sm; dark [ bg gray 800 ] ]
    [
      h2
        ~at:[ At.id "duration-heading" ]
        ~tw:Tw.[ text_xl; font_bold; text gray 900; mb 4; dark [ text_white ] ]
        [ txt "Duration & Delay" ];
      p
        ~tw:Tw.[ text gray 600; dark [ text gray 300 ]; mb 6 ]
        [
          txt
            "Control animation speed and delay with duration and delay \
             utilities.";
        ];
      duration_demos;
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
              text gray 900;
              mb 4;
              dark [ text_white ];
            ]
        [ txt "Animation Utilities" ];
      p
        ~tw:
          Tw.
            [
              text_lg; text gray 600; max_w_2xl; mx_auto; dark [ text gray 400 ];
            ]
        [
          txt
            "Explore Tailwind's animation system: keyframe animations, \
             transitions, transformations, and motion preferences.";
        ];
    ]

let page_view =
  page ~title:"Animations Demo" ~tw_css:"animations.css" []
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
              keyframe_section;
              transitions_section;
              transform_section;
              starting_section;
              motion_section;
              duration_section;
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
