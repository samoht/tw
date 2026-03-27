(* Feature: Animations demo

   Shows various animation utilities including keyframe animations, transitions,
   transforms, @starting-style, and motion preferences. *)

open Tw_html

(* Page header *)
let page_header =
  header
    ~tw:Tw.[ bg white; shadow; shadow_sm; dark [ bg ~shade:900 gray ] ]
    [
      div
        ~tw:Tw.[ max_w_5xl; mx_auto; px 4; py 4 ]
        [
          h1
            ~tw:
              Tw.
                [
                  text_2xl; font_bold; text ~shade:900 gray; dark [ text white ];
                ]
            [ txt "Animations Demo" ];
        ];
    ]

(* Keyframe animations section *)
let anim_label =
  Tw.
    [
      text_sm; font_medium; text ~shade:700 gray; dark [ text ~shade:300 gray ];
    ]

let keyframe_desc = Tw.[ text_xs; text gray; text_center ]

let keyframe_demo ~anim_el ~name ~description =
  div
    ~tw:Tw.[ flex; flex_col; items_center; gap 3 ]
    [
      anim_el;
      span ~tw:anim_label [ txt name ];
      span ~tw:keyframe_desc [ txt description ];
    ]

let ping_el =
  div
    ~tw:Tw.[ relative; w 16; h 16 ]
    [
      div ~tw:Tw.[ absolute; inset 0; bg green; rounded_full; animate_ping ] [];
      div ~tw:Tw.[ absolute; inset 2; bg ~shade:600 green; rounded_full ] [];
    ]

let keyframe_demos =
  div
    ~tw:Tw.[ grid; grid_cols 2; gap 6; md [ grid_cols 4 ] ]
    [
      keyframe_demo
        ~anim_el:
          (div
             ~tw:
               Tw.
                 [
                   w 16;
                   h 16;
                   rounded_full;
                   border_lg;
                   border_color ~shade:600 blue;
                   animate_spin;
                 ]
             [])
        ~name:"Spin" ~description:"Loading spinners";
      keyframe_demo ~anim_el:ping_el ~name:"Ping" ~description:"Notifications";
      keyframe_demo
        ~anim_el:
          (div ~tw:Tw.[ w 16; h 16; bg purple; rounded_lg; animate_pulse ] [])
        ~name:"Pulse" ~description:"Skeleton loading";
      keyframe_demo
        ~anim_el:
          (div ~tw:Tw.[ w 16; h 16; bg amber; rounded_full; animate_bounce ] [])
        ~name:"Bounce" ~description:"Scroll indicators";
    ]

let keyframe_section =
  section
    ~at:[ At.v "aria-labelledby" "keyframe-heading" ]
    ~tw:Tw.[ bg white; p 6; rounded_xl; shadow_sm; dark [ bg ~shade:800 gray ] ]
    [
      h2
        ~at:[ At.id "keyframe-heading" ]
        ~tw:
          Tw.
            [
              text_xl;
              font_bold;
              text ~shade:900 gray;
              mb 4;
              dark [ text white ];
            ]
        [ txt "Keyframe Animations" ];
      p
        ~tw:Tw.[ text ~shade:600 gray; dark [ text ~shade:300 gray ]; mb 6 ]
        [
          txt
            "Built-in keyframe animations for common use cases. These run \
             continuously.";
        ];
      keyframe_demos;
    ]

(* Transitions section *)
let transition_base =
  Tw.[ p 6; rounded_lg; text_center; font_medium; duration 300 ]

let color_transition_demo =
  div
    ~tw:
      Tw.(
        [
          bg blue;
          text white;
          transition;
          transition_colors;
          hover [ bg ~shade:700 blue ];
        ]
        @ transition_base)
    [ txt "Hover for color transition" ]

let scale_transition_demo =
  div
    ~tw:
      Tw.(
        [ bg green; text white; transition_transform; hover [ scale 105 ] ]
        @ transition_base)
    [ txt "Hover for scale" ]

let shadow_transition_demo =
  div
    ~tw:
      Tw.(
        [
          bg white;
          text ~shade:700 gray;
          shadow_sm;
          transition_shadow;
          hover [ shadow_xl ];
          dark [ bg ~shade:700 gray; text white ];
        ]
        @ transition_base)
    [ txt "Hover for shadow" ]

let transition_hover_demos =
  div
    ~tw:Tw.[ grid; grid_cols 1; gap 4; md [ grid_cols 3 ] ]
    [ color_transition_demo; scale_transition_demo; shadow_transition_demo ]

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
                 bg ~shade:100 gray;
                 rounded_lg;
                 text_center;
                 transition_all;
                 duration 500;
                 ease_style;
                 hover [ bg ~shade:300 gray; translate_x 2 ];
                 dark [ bg ~shade:700 gray; hover [ bg ~shade:600 gray ] ];
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
    ~tw:Tw.[ bg white; p 6; rounded_xl; shadow_sm; dark [ bg ~shade:800 gray ] ]
    [
      h2
        ~at:[ At.id "transitions-heading" ]
        ~tw:
          Tw.
            [
              text_xl;
              font_bold;
              text ~shade:900 gray;
              mb 4;
              dark [ text white ];
            ]
        [ txt "Transitions" ];
      p
        ~tw:Tw.[ text ~shade:600 gray; dark [ text ~shade:300 gray ]; mb 6 ]
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
              text ~shade:800 gray;
              mt 8;
              mb 4;
              dark [ text ~shade:200 gray ];
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
        ~bg_style:Tw.(bg rose)
        ~hover_style:Tw.(hover [ rotate 45 ])
        "Rotate 45";
      transform_demo
        ~bg_style:Tw.(bg cyan)
        ~hover_style:Tw.(hover [ scale 125 ])
        "Scale 125%";
      transform_demo
        ~bg_style:Tw.(bg indigo)
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
    ~tw:Tw.[ bg white; p 6; rounded_xl; shadow_sm; dark [ bg ~shade:800 gray ] ]
    [
      h2
        ~at:[ At.id "transform-heading" ]
        ~tw:
          Tw.
            [
              text_xl;
              font_bold;
              text ~shade:900 gray;
              mb 4;
              dark [ text white ];
            ]
        [ txt "Transforms" ];
      p
        ~tw:Tw.[ text ~shade:600 gray; dark [ text ~shade:300 gray ]; mb 6 ]
        [ txt "Various transformation utilities: rotate, scale, translate." ];
      transform_demos;
    ]

(* Starting style section *)
let starting_card ~start_styles ~end_styles ~bg_styles ~heading_tw ~text_tw
    ~heading ~description =
  div
    ~tw:Tw.(start_styles @ end_styles @ [ p 6; rounded_lg ] @ bg_styles)
    [ h3 ~tw:heading_tw [ txt heading ]; p ~tw:text_tw [ txt description ] ]

let fade_in_card =
  starting_card
    ~start_styles:Tw.[ starting [ opacity 0 ] ]
    ~end_styles:Tw.[ opacity 100; transition_opacity; duration 700 ]
    ~bg_styles:Tw.[ bg ~shade:100 blue; dark [ bg ~shade:900 blue ] ]
    ~heading_tw:
      Tw.
        [
          font_semibold;
          text ~shade:900 blue;
          mb 2;
          dark [ text ~shade:100 blue ];
        ]
    ~text_tw:Tw.[ text_sm; text ~shade:700 blue; dark [ text ~shade:200 blue ] ]
    ~heading:"Fade In"
    ~description:"This element fades in from transparent to opaque."

let scale_in_card =
  starting_card
    ~start_styles:Tw.[ starting [ opacity 0; scale 90 ] ]
    ~end_styles:Tw.[ opacity 100; scale 100; transition_all; duration 500 ]
    ~bg_styles:Tw.[ bg ~shade:100 green; dark [ bg ~shade:900 green ] ]
    ~heading_tw:
      Tw.
        [
          font_semibold;
          text ~shade:900 green;
          mb 2;
          dark [ text ~shade:100 green ];
        ]
    ~text_tw:
      Tw.[ text_sm; text ~shade:700 green; dark [ text ~shade:200 green ] ]
    ~heading:"Scale In" ~description:"This element scales up while fading in."

let starting_demos =
  div
    ~tw:Tw.[ grid; grid_cols 1; gap 4; md [ grid_cols 2 ] ]
    [
      fade_in_card;
      scale_in_card;
      starting_card
        ~start_styles:Tw.[ starting [ opacity 0; translate_x (-4) ] ]
        ~end_styles:
          Tw.[ opacity 100; translate_x 0; transition_all; duration 500 ]
        ~bg_styles:Tw.[ bg ~shade:100 purple; dark [ bg ~shade:900 purple ] ]
        ~heading_tw:
          Tw.
            [
              font_semibold;
              text ~shade:900 purple;
              mb 2;
              dark [ text ~shade:100 purple ];
            ]
        ~text_tw:
          Tw.
            [ text_sm; text ~shade:700 purple; dark [ text ~shade:200 purple ] ]
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
              text white;
            ]
        ~heading_tw:Tw.[ font_semibold; mb 2 ]
        ~text_tw:Tw.[ text_sm; opacity 90 ]
        ~heading:"Gradient Entry"
        ~description:"Smooth entry with gradient background.";
    ]

let starting_section =
  section
    ~at:[ At.v "aria-labelledby" "starting-heading" ]
    ~tw:Tw.[ bg white; p 6; rounded_xl; shadow_sm; dark [ bg ~shade:800 gray ] ]
    [
      h2
        ~at:[ At.id "starting-heading" ]
        ~tw:
          Tw.
            [
              text_xl;
              font_bold;
              text ~shade:900 gray;
              mb 4;
              dark [ text white ];
            ]
        [ txt "@starting-style Entry Animations" ];
      p
        ~tw:Tw.[ text ~shade:600 gray; dark [ text ~shade:300 gray ]; mb 6 ]
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
    ~tw:Tw.[ p 6; bg white; rounded_lg; shadow_sm; dark [ bg ~shade:700 gray ] ]
    [
      h3
        ~tw:
          Tw.[ font_semibold; text ~shade:800 gray; mb 3; dark [ text white ] ]
        [ txt title ];
      div
        ~tw:Tw.[ flex; items_center; gap 4 ]
        [
          div ~tw:Tw.([ w 12; h 12; rounded_full ] @ anim_styles) [];
          p
            ~tw:
              Tw.
                [ text_sm; text ~shade:600 gray; dark [ text ~shade:300 gray ] ]
            [ txt description ];
        ];
    ]

let motion_demos =
  div
    ~tw:Tw.[ grid; grid_cols 1; gap 4; md [ grid_cols 2 ] ]
    [
      motion_card ~title:"motion-safe"
        ~anim_styles:Tw.[ bg blue; motion_safe [ animate_bounce ] ]
        ~description:"Only animates when motion is allowed";
      motion_card ~title:"motion-reduce"
        ~anim_styles:
          Tw.[ bg green; animate_spin; motion_reduce [ animate_none ] ]
        ~description:"Stops animation when reduced motion preferred";
    ]

let motion_section =
  section
    ~at:[ At.v "aria-labelledby" "motion-heading" ]
    ~tw:Tw.[ bg ~shade:50 blue; p 6; rounded_xl; dark [ bg ~shade:800 gray ] ]
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
                 bg ~shade:200 gray;
                 rounded_lg;
                 transition_colors;
                 dur;
                 hover [ bg ~shade:400 gray ];
                 dark [ bg ~shade:700 gray; hover [ bg gray ] ];
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
    ~tw:Tw.[ bg white; p 6; rounded_xl; shadow_sm; dark [ bg ~shade:800 gray ] ]
    [
      h2
        ~at:[ At.id "duration-heading" ]
        ~tw:
          Tw.
            [
              text_xl;
              font_bold;
              text ~shade:900 gray;
              mb 4;
              dark [ text white ];
            ]
        [ txt "Duration & Delay" ];
      p
        ~tw:Tw.[ text ~shade:600 gray; dark [ text ~shade:300 gray ]; mb 6 ]
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
              text ~shade:900 gray;
              mb 4;
              dark [ text white ];
            ]
        [ txt "Animation Utilities" ];
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
