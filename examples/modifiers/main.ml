(** Feature: Modifiers demo

    Comprehensive showcase of Tailwind modifiers: state variants, group/peer,
    pseudo-elements, breakpoints, accessibility, and ARIA variants. *)

open Tw_html

(* Page header *)
let page_header =
  header
    ~tw:Tw.[ bg white; shadow_sm; dark [ bg ~shade:900 gray ] ]
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
            [ txt "Modifiers Demo" ];
        ];
    ]

(* Shared demo styles *)
let demo_label =
  Tw.
    [
      text_sm;
      font_semibold;
      text ~shade:700 gray;
      mb 2;
      dark [ text ~shade:300 gray ];
    ]

(* State Variants Section *)
let hover_demo =
  div
    [
      h3 ~tw:demo_label [ txt "hover" ];
      button
        ~tw:
          Tw.
            [
              w_full;
              px 4;
              py 3;
              bg blue;
              text white;
              rounded_lg;
              transition_colors;
              duration 200;
              hover [ bg ~shade:700 blue ];
            ]
        [ txt "Hover me" ];
    ]

let focus_demo =
  div
    [
      h3 ~tw:demo_label [ txt "focus" ];
      input
        ~tw:
          Tw.
            [
              w_full;
              px 4;
              py 3;
              border;
              border_color ~shade:300 gray;
              rounded_lg;
              focus [ ring_md; ring_color blue 500; outline_none ];
              dark
                [ bg ~shade:700 gray; border_color ~shade:600 gray; text white ];
            ]
        ~at:[ At.placeholder "Click or tab to focus" ]
        ();
    ]

let active_demo =
  div
    [
      h3 ~tw:demo_label [ txt "active" ];
      button
        ~tw:
          Tw.
            [
              w_full;
              px 4;
              py 3;
              bg green;
              text white;
              rounded_lg;
              transition_all;
              duration 150;
              active [ bg ~shade:700 green; scale 95 ];
            ]
        [ txt "Press and hold" ];
    ]

let disabled_demo =
  div
    [
      h3 ~tw:demo_label [ txt "disabled" ];
      button
        ~at:[ At.v "disabled" "" ]
        ~tw:
          Tw.
            [
              w_full;
              px 4;
              py 3;
              bg ~shade:300 gray;
              text gray;
              rounded_lg;
              disabled [ opacity 50; cursor_not_allowed ];
            ]
        [ txt "Disabled button" ];
    ]

let state_section =
  section
    ~at:[ At.v "aria-labelledby" "state-heading" ]
    ~tw:Tw.[ bg white; p 6; rounded_xl; shadow_sm; dark [ bg ~shade:800 gray ] ]
    [
      h2
        ~at:[ At.id "state-heading" ]
        ~tw:
          Tw.
            [
              text_xl;
              font_bold;
              text ~shade:900 gray;
              mb 4;
              dark [ text white ];
            ]
        [ txt "State Variants" ];
      p
        ~tw:Tw.[ text ~shade:600 gray; dark [ text ~shade:300 gray ]; mb 6 ]
        [
          txt
            "Apply styles based on user interaction states: hover, focus, \
             active, disabled.";
        ];
      div
        ~tw:Tw.[ grid; grid_cols 1; gap 4; md [ grid_cols 2 ] ]
        [ hover_demo; focus_demo; active_demo; disabled_demo ];
    ]

(* Group and Peer Section *)
let group_hover_demo =
  div
    [
      h3 ~tw:demo_label [ txt "group-hover" ];
      div
        ~tw:
          Tw.
            [
              group;
              p 4;
              bg ~shade:100 gray;
              rounded_lg;
              cursor_pointer;
              transition_colors;
              duration 200;
              hover [ bg ~shade:200 gray ];
              dark [ bg ~shade:700 gray; hover [ bg ~shade:600 gray ] ];
            ]
        [
          h4
            ~tw:
              Tw.
                [
                  font_semibold;
                  text ~shade:800 gray;
                  group_hover [ text ~shade:600 blue ];
                  dark [ text white; group_hover [ text ~shade:400 blue ] ];
                ]
            [ txt "Hover this card" ];
          p
            ~tw:
              Tw.
                [
                  text ~shade:600 gray;
                  text_sm;
                  group_hover [ text ~shade:900 gray ];
                  dark
                    [
                      text ~shade:400 gray; group_hover [ text ~shade:200 gray ];
                    ];
                ]
            [ txt "Child elements respond to parent hover" ];
        ];
    ]

let group_focus_demo =
  div
    [
      h3 ~tw:demo_label [ txt "group-focus" ];
      div
        ~tw:
          Tw.
            [
              group;
              p 4;
              bg ~shade:100 gray;
              rounded_lg;
              dark [ bg ~shade:700 gray ];
            ]
        [
          input
            ~tw:
              Tw.
                [
                  w_full;
                  px 3;
                  py 2;
                  border;
                  border_color ~shade:300 gray;
                  rounded_md;
                  mb 2;
                  focus [ outline_none; ring_md; ring_color blue 500 ];
                  dark [ bg ~shade:600 gray; border_color gray; text white ];
                ]
            ~at:[ At.placeholder "Focus this input" ]
            ();
          p
            ~tw:
              Tw.
                [
                  text_sm;
                  text gray;
                  group_focus [ text ~shade:600 blue; font_semibold ];
                  dark
                    [
                      text ~shade:400 gray; group_focus [ text ~shade:400 blue ];
                    ];
                ]
            [ txt "This text responds to input focus" ];
        ];
    ]

let peer_checked_demo =
  div
    [
      h3 ~tw:demo_label [ txt "peer-checked" ];
      div
        ~tw:
          Tw.
            [ p 4; bg ~shade:100 gray; rounded_lg; dark [ bg ~shade:700 gray ] ]
        [
          label
            ~tw:Tw.[ flex; items_center; gap 3; cursor_pointer ]
            [
              input ~tw:Tw.[ peer; w 5; h 5 ] ~at:[ At.type' "checkbox" ] ();
              span
                ~tw:
                  Tw.
                    [
                      text ~shade:700 gray;
                      peer_checked [ text ~shade:600 green; font_semibold ];
                      dark
                        [
                          text ~shade:300 gray;
                          peer_checked [ text ~shade:400 green ];
                        ];
                    ]
                [ txt "Check to enable" ];
            ];
        ];
    ]

let peer_has_demo =
  div
    [
      h3 ~tw:demo_label [ txt "peer-has (validation)" ];
      div
        ~tw:
          Tw.
            [ p 4; bg ~shade:100 gray; rounded_lg; dark [ bg ~shade:700 gray ] ]
        [
          input
            ~tw:
              Tw.
                [
                  peer;
                  w_full;
                  px 3;
                  py 2;
                  border;
                  border_color ~shade:300 gray;
                  rounded_md;
                  mb 2;
                  focus [ outline_none; ring_md ];
                  dark [ bg ~shade:600 gray; border_color gray; text white ];
                ]
            ~at:[ At.type' "email"; At.placeholder "Enter email" ]
            ();
          p
            ~tw:
              Tw.
                [
                  text_sm;
                  text red;
                  opacity 0;
                  transition_opacity;
                  duration 200;
                  peer_has ":invalid:not(:placeholder-shown)" [ opacity 100 ];
                ]
            [ txt "Please enter a valid email address" ];
        ];
    ]

let group_peer_section =
  section
    ~at:[ At.v "aria-labelledby" "group-peer-heading" ]
    ~tw:Tw.[ bg white; p 6; rounded_xl; shadow_sm; dark [ bg ~shade:800 gray ] ]
    [
      h2
        ~at:[ At.id "group-peer-heading" ]
        ~tw:
          Tw.
            [
              text_xl;
              font_bold;
              text ~shade:900 gray;
              mb 4;
              dark [ text white ];
            ]
        [ txt "Group & Peer Modifiers" ];
      p
        ~tw:Tw.[ text ~shade:600 gray; dark [ text ~shade:300 gray ]; mb 6 ]
        [
          txt
            "Style child elements based on parent (group) or sibling (peer) \
             state.";
        ];
      div
        ~tw:Tw.[ grid; grid_cols 1; gap 6; lg [ grid_cols 2 ] ]
        [ group_hover_demo; group_focus_demo; peer_checked_demo; peer_has_demo ];
    ]

(* Has Selector Section *)
let has_checked_demo =
  div
    [
      h3 ~tw:demo_label [ txt "has(:checked)" ];
      div
        ~tw:
          Tw.
            [
              p 4;
              bg ~shade:100 gray;
              rounded_lg;
              border_md;
              border_transparent;
              transition_colors;
              duration 200;
              has ":checked" [ bg ~shade:100 green; border_color green ];
              dark
                [
                  bg ~shade:700 gray;
                  has ":checked"
                    [ bg ~shade:900 green; border_color ~shade:400 green ];
                ];
            ]
        [
          label
            ~tw:Tw.[ flex; items_center; gap 3; cursor_pointer ]
            [
              input ~tw:Tw.[ w 5; h 5 ] ~at:[ At.type' "checkbox" ] ();
              span
                ~tw:Tw.[ text ~shade:700 gray; dark [ text ~shade:300 gray ] ]
                [ txt "Check to change wrapper style" ];
            ];
        ];
    ]

let group_has_demo =
  div
    [
      h3 ~tw:demo_label [ txt "group-has" ];
      div
        ~tw:
          Tw.
            [
              group;
              p 4;
              bg ~shade:100 gray;
              rounded_lg;
              dark [ bg ~shade:700 gray ];
            ]
        [
          div
            ~tw:Tw.[ flex; flex_col; gap 2; mb 4 ]
            [
              label
                ~tw:Tw.[ flex; items_center; gap 2; cursor_pointer ]
                [ input ~at:[ At.type' "checkbox" ] (); txt "Option 1" ];
              label
                ~tw:Tw.[ flex; items_center; gap 2; cursor_pointer ]
                [ input ~at:[ At.type' "checkbox" ] (); txt "Option 2" ];
            ];
          p
            ~tw:
              Tw.
                [
                  text_sm;
                  text gray;
                  group_has ":checked" [ text ~shade:600 green; font_semibold ];
                  dark
                    [
                      text ~shade:400 gray;
                      group_has ":checked" [ text ~shade:400 green ];
                    ];
                ]
            [ txt "Selection status" ];
        ];
    ]

let has_section =
  section
    ~at:[ At.v "aria-labelledby" "has-heading" ]
    ~tw:Tw.[ bg white; p 6; rounded_xl; shadow_sm; dark [ bg ~shade:800 gray ] ]
    [
      h2
        ~at:[ At.id "has-heading" ]
        ~tw:
          Tw.
            [
              text_xl;
              font_bold;
              text ~shade:900 gray;
              mb 4;
              dark [ text white ];
            ]
        [ txt ":has() Selector" ];
      p
        ~tw:Tw.[ text ~shade:600 gray; dark [ text ~shade:300 gray ]; mb 6 ]
        [
          txt
            "Style elements based on their descendants using the :has() \
             selector.";
        ];
      div
        ~tw:Tw.[ grid; grid_cols 1; gap 6; md [ grid_cols 2 ] ]
        [ has_checked_demo; group_has_demo ];
    ]

(* Focus Variants Section *)
let focus_within_demo =
  div
    [
      h3 ~tw:demo_label [ txt "focus-within (wrapper)" ];
      div
        ~tw:
          Tw.
            [
              p 4;
              bg ~shade:100 gray;
              rounded_lg;
              border_md;
              border_transparent;
              transition_all;
              duration 200;
              focus_within [ border_color blue; shadow_md ];
              dark [ bg ~shade:700 gray ];
            ]
        [
          label
            ~tw:
              Tw.
                [
                  block;
                  text_sm;
                  font_medium;
                  text ~shade:700 gray;
                  mb 1;
                  dark [ text ~shade:300 gray ];
                ]
            [ txt "Username" ];
          input
            ~tw:
              Tw.
                [
                  w_full;
                  px 3;
                  py 2;
                  border;
                  border_color ~shade:300 gray;
                  rounded_md;
                  focus [ outline_none ];
                  dark [ bg ~shade:600 gray; border_color gray; text white ];
                ]
            ~at:[ At.placeholder "Enter username" ]
            ();
        ];
    ]

let focus_visible_demo =
  div
    [
      h3 ~tw:demo_label [ txt "focus-visible (keyboard only)" ];
      div
        ~tw:
          Tw.
            [ p 4; bg ~shade:100 gray; rounded_lg; dark [ bg ~shade:700 gray ] ]
        [
          p
            ~tw:
              Tw.
                [
                  text_sm;
                  text ~shade:600 gray;
                  mb 3;
                  dark [ text ~shade:400 gray ];
                ]
            [ txt "Click vs Tab to see the difference:" ];
          button
            ~tw:
              Tw.
                [
                  px 4;
                  py 2;
                  bg blue;
                  text white;
                  rounded_lg;
                  focus [ outline_none ];
                  focus_visible [ ring_md; ring_color blue 400 ];
                ]
            [ txt "Tab to focus" ];
        ];
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
        [ txt "Focus Variants" ];
      p
        ~tw:Tw.[ text ~shade:600 gray; dark [ text ~shade:300 gray ]; mb 6 ]
        [ txt "Different focus states for various accessibility and UX needs." ];
      div
        ~tw:Tw.[ grid; grid_cols 1; gap 6; md [ grid_cols 2 ] ]
        [ focus_within_demo; focus_visible_demo ];
    ]

(* Pseudo-elements Section *)
let before_demo =
  div
    [
      h3 ~tw:demo_label [ txt "before" ];
      div
        ~tw:
          Tw.
            [
              relative;
              p 4;
              pl 8;
              bg ~shade:100 gray;
              rounded_lg;
              before
                [ absolute; left 3; top 4; w 2; h 2; bg blue; rounded_full ];
              dark [ bg ~shade:700 gray ];
            ]
        [
          span
            ~tw:Tw.[ text ~shade:700 gray; dark [ text ~shade:300 gray ] ]
            [ txt "List item with custom bullet" ];
        ];
    ]

let after_demo =
  div
    [
      h3 ~tw:demo_label [ txt "after" ];
      div
        ~tw:
          Tw.
            [
              relative;
              inline_block;
              px 4;
              py 2;
              bg blue;
              text white;
              rounded_lg;
              after
                [
                  absolute; right (-1); top (-1); w 3; h 3; bg red; rounded_full;
                ];
            ]
        [ txt "Badge with notification dot" ];
    ]

let pseudo_section =
  section
    ~at:[ At.v "aria-labelledby" "pseudo-heading" ]
    ~tw:Tw.[ bg white; p 6; rounded_xl; shadow_sm; dark [ bg ~shade:800 gray ] ]
    [
      h2
        ~at:[ At.id "pseudo-heading" ]
        ~tw:
          Tw.
            [
              text_xl;
              font_bold;
              text ~shade:900 gray;
              mb 4;
              dark [ text white ];
            ]
        [ txt "Pseudo-elements" ];
      p
        ~tw:Tw.[ text ~shade:600 gray; dark [ text ~shade:300 gray ]; mb 6 ]
        [ txt "Style ::before and ::after pseudo-elements." ];
      div
        ~tw:Tw.[ grid; grid_cols 1; gap 6; md [ grid_cols 2 ] ]
        [ before_demo; after_demo ];
    ]

(* ARIA and Data Attributes Section *)
let aria_expanded_demo =
  div
    [
      h3 ~tw:demo_label [ txt "aria-expanded" ];
      button
        ~at:
          [
            At.v "aria-expanded" "false";
            At.v "onclick"
              "this.setAttribute('aria-expanded', \
               this.getAttribute('aria-expanded') === 'true' ? 'false' : \
               'true')";
          ]
        ~tw:
          Tw.
            [
              w_full;
              px 4;
              py 3;
              bg white;
              rounded_lg;
              shadow_sm;
              text_left;
              flex;
              items_center;
              justify_between;
              aria_expanded [ bg ~shade:100 blue ];
              dark [ bg ~shade:700 gray; aria_expanded [ bg ~shade:900 blue ] ];
            ]
        [
          span
            ~tw:Tw.[ font_medium; text ~shade:800 gray; dark [ text white ] ]
            [ txt "Click to toggle" ];
          span
            ~tw:
              Tw.
                [
                  transition_transform;
                  duration 200;
                  aria_expanded [ rotate 180 ];
                ]
            [ txt "v" ];
        ];
    ]

let aria_selected_demo =
  div
    [
      h3 ~tw:demo_label [ txt "aria-selected (tabs)" ];
      div
        ~tw:Tw.[ flex; gap 2 ]
        (List.mapi
           (fun i label ->
             button
               ~at:[ At.v "aria-selected" (if i = 0 then "true" else "false") ]
               ~tw:
                 Tw.
                   [
                     px 4;
                     py 2;
                     rounded_lg;
                     text ~shade:600 gray;
                     aria_selected [ bg blue; text white ];
                     dark
                       [
                         text ~shade:400 gray;
                         aria_selected [ bg ~shade:600 blue; text white ];
                       ];
                   ]
               [ txt label ])
           [ "Tab 1"; "Tab 2"; "Tab 3" ]);
    ]

let aria_section =
  section
    ~at:[ At.v "aria-labelledby" "aria-heading" ]
    ~tw:Tw.[ bg ~shade:50 blue; p 6; rounded_xl; dark [ bg ~shade:800 gray ] ]
    [
      h2
        ~at:[ At.id "aria-heading" ]
        ~tw:
          Tw.
            [
              text_xl;
              font_bold;
              text ~shade:900 gray;
              mb 4;
              dark [ text white ];
            ]
        [ txt "ARIA Variants" ];
      p
        ~tw:Tw.[ text ~shade:600 gray; dark [ text ~shade:300 gray ]; mb 6 ]
        [
          txt
            "Style elements based on ARIA attributes for accessible interfaces.";
        ];
      div
        ~tw:Tw.[ grid; grid_cols 1; gap 6; md [ grid_cols 2 ] ]
        [ aria_expanded_demo; aria_selected_demo ];
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
        [ txt "Modifier Utilities" ];
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
            "Conditional styling with state variants, group/peer modifiers, \
             pseudo-elements, and ARIA support.";
        ];
    ]

let page_view =
  page ~title:"Modifiers Demo" ~tw_css:"modifiers.css" []
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
              state_section;
              group_peer_section;
              has_section;
              focus_section;
              pseudo_section;
              aria_section;
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
