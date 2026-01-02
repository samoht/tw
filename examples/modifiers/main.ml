(** Feature: Modifiers demo

    Comprehensive showcase of Tailwind modifiers: state variants, group/peer,
    pseudo-elements, breakpoints, accessibility, and ARIA variants. *)

open Tw_html

(* Page header *)
let page_header =
  header
    ~tw:Tw.[ bg_white; shadow_sm; dark [ bg gray 900 ] ]
    [
      div
        ~tw:Tw.[ max_w_5xl; mx_auto; px 4; py 4 ]
        [
          h1
            ~tw:Tw.[ text_2xl; font_bold; text gray 900; dark [ text_white ] ]
            [ txt "Modifiers Demo" ];
        ];
    ]

(* State Variants Section *)
let state_section =
  section
    ~at:[ At.v "aria-labelledby" "state-heading" ]
    ~tw:Tw.[ bg_white; p 6; rounded_xl; shadow_sm; dark [ bg gray 800 ] ]
    [
      h2
        ~at:[ At.id "state-heading" ]
        ~tw:Tw.[ text_xl; font_bold; text gray 900; mb 4; dark [ text_white ] ]
        [ txt "State Variants" ];
      p
        ~tw:Tw.[ text gray 600; dark [ text gray 300 ]; mb 6 ]
        [
          txt
            "Apply styles based on user interaction states: hover, focus, \
             active, disabled.";
        ];
      div
        ~tw:Tw.[ grid; grid_cols 1; gap 4; md [ grid_cols 2 ] ]
        [
          (* Hover *)
          div
            [
              h3
                ~tw:
                  Tw.
                    [
                      text_sm;
                      font_semibold;
                      text gray 700;
                      mb 2;
                      dark [ text gray 300 ];
                    ]
                [ txt "hover" ];
              button
                ~tw:
                  Tw.
                    [
                      w_full;
                      px 4;
                      py 3;
                      bg blue 500;
                      text_white;
                      rounded_lg;
                      transition_colors;
                      duration 200;
                      hover [ bg blue 700 ];
                    ]
                [ txt "Hover me" ];
            ];
          (* Focus *)
          div
            [
              h3
                ~tw:
                  Tw.
                    [
                      text_sm;
                      font_semibold;
                      text gray 700;
                      mb 2;
                      dark [ text gray 300 ];
                    ]
                [ txt "focus" ];
              input
                ~tw:
                  Tw.
                    [
                      w_full;
                      px 4;
                      py 3;
                      border;
                      border_color gray 300;
                      rounded_lg;
                      focus [ ring_md; ring_color blue 500; outline_none ];
                      dark [ bg gray 700; border_color gray 600; text_white ];
                    ]
                ~at:[ At.placeholder "Click or tab to focus" ]
                ();
            ];
          (* Active *)
          div
            [
              h3
                ~tw:
                  Tw.
                    [
                      text_sm;
                      font_semibold;
                      text gray 700;
                      mb 2;
                      dark [ text gray 300 ];
                    ]
                [ txt "active" ];
              button
                ~tw:
                  Tw.
                    [
                      w_full;
                      px 4;
                      py 3;
                      bg green 500;
                      text_white;
                      rounded_lg;
                      transition_all;
                      duration 150;
                      active [ bg green 700; scale 95 ];
                    ]
                [ txt "Press and hold" ];
            ];
          (* Disabled *)
          div
            [
              h3
                ~tw:
                  Tw.
                    [
                      text_sm;
                      font_semibold;
                      text gray 700;
                      mb 2;
                      dark [ text gray 300 ];
                    ]
                [ txt "disabled" ];
              button
                ~at:[ At.v "disabled" "" ]
                ~tw:
                  Tw.
                    [
                      w_full;
                      px 4;
                      py 3;
                      bg gray 300;
                      text gray 500;
                      rounded_lg;
                      disabled [ opacity 50; cursor_not_allowed ];
                    ]
                [ txt "Disabled button" ];
            ];
        ];
    ]

(* Group and Peer Section *)
let group_peer_section =
  section
    ~at:[ At.v "aria-labelledby" "group-peer-heading" ]
    ~tw:Tw.[ bg_white; p 6; rounded_xl; shadow_sm; dark [ bg gray 800 ] ]
    [
      h2
        ~at:[ At.id "group-peer-heading" ]
        ~tw:Tw.[ text_xl; font_bold; text gray 900; mb 4; dark [ text_white ] ]
        [ txt "Group & Peer Modifiers" ];
      p
        ~tw:Tw.[ text gray 600; dark [ text gray 300 ]; mb 6 ]
        [
          txt
            "Style child elements based on parent (group) or sibling (peer) \
             state.";
        ];
      div
        ~tw:Tw.[ grid; grid_cols 1; gap 6; lg [ grid_cols 2 ] ]
        [
          (* Group hover *)
          div
            [
              h3
                ~tw:
                  Tw.
                    [
                      text_sm;
                      font_semibold;
                      text gray 700;
                      mb 2;
                      dark [ text gray 300 ];
                    ]
                [ txt "group-hover" ];
              div
                ~tw:
                  Tw.
                    [
                      group;
                      p 4;
                      bg gray 100;
                      rounded_lg;
                      cursor_pointer;
                      transition_colors;
                      duration 200;
                      hover [ bg gray 200 ];
                      dark [ bg gray 700; hover [ bg gray 600 ] ];
                    ]
                [
                  h4
                    ~tw:
                      Tw.
                        [
                          font_semibold;
                          text gray 800;
                          group_hover [ text blue 600 ];
                          dark [ text_white; group_hover [ text blue 400 ] ];
                        ]
                    [ txt "Hover this card" ];
                  p
                    ~tw:
                      Tw.
                        [
                          text gray 600;
                          text_sm;
                          group_hover [ text gray 900 ];
                          dark [ text gray 400; group_hover [ text gray 200 ] ];
                        ]
                    [ txt "Child elements respond to parent hover" ];
                ];
            ];
          (* Group focus *)
          div
            [
              h3
                ~tw:
                  Tw.
                    [
                      text_sm;
                      font_semibold;
                      text gray 700;
                      mb 2;
                      dark [ text gray 300 ];
                    ]
                [ txt "group-focus" ];
              div
                ~tw:
                  Tw.
                    [
                      group; p 4; bg gray 100; rounded_lg; dark [ bg gray 700 ];
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
                          border_color gray 300;
                          rounded_md;
                          mb 2;
                          focus [ outline_none; ring_md; ring_color blue 500 ];
                          dark
                            [ bg gray 600; border_color gray 500; text_white ];
                        ]
                    ~at:[ At.placeholder "Focus this input" ]
                    ();
                  p
                    ~tw:
                      Tw.
                        [
                          text_sm;
                          text gray 500;
                          group_focus [ text blue 600; font_semibold ];
                          dark [ text gray 400; group_focus [ text blue 400 ] ];
                        ]
                    [ txt "This text responds to input focus" ];
                ];
            ];
          (* Peer checked *)
          div
            [
              h3
                ~tw:
                  Tw.
                    [
                      text_sm;
                      font_semibold;
                      text gray 700;
                      mb 2;
                      dark [ text gray 300 ];
                    ]
                [ txt "peer-checked" ];
              div
                ~tw:Tw.[ p 4; bg gray 100; rounded_lg; dark [ bg gray 700 ] ]
                [
                  label
                    ~tw:Tw.[ flex; items_center; gap 3; cursor_pointer ]
                    [
                      input
                        ~tw:Tw.[ peer; w 5; h 5 ]
                        ~at:[ At.type' "checkbox" ]
                        ();
                      span
                        ~tw:
                          Tw.
                            [
                              text gray 700;
                              peer_checked [ text green 600; font_semibold ];
                              dark
                                [
                                  text gray 300; peer_checked [ text green 400 ];
                                ];
                            ]
                        [ txt "Check to enable" ];
                    ];
                ];
            ];
          (* Peer has (validation) *)
          div
            [
              h3
                ~tw:
                  Tw.
                    [
                      text_sm;
                      font_semibold;
                      text gray 700;
                      mb 2;
                      dark [ text gray 300 ];
                    ]
                [ txt "peer-has (validation)" ];
              div
                ~tw:Tw.[ p 4; bg gray 100; rounded_lg; dark [ bg gray 700 ] ]
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
                          border_color gray 300;
                          rounded_md;
                          mb 2;
                          focus [ outline_none; ring_md ];
                          dark
                            [ bg gray 600; border_color gray 500; text_white ];
                        ]
                    ~at:[ At.type' "email"; At.placeholder "Enter email" ]
                    ();
                  p
                    ~tw:
                      Tw.
                        [
                          text_sm;
                          text red 500;
                          opacity 0;
                          transition_opacity;
                          duration 200;
                          peer_has ":invalid:not(:placeholder-shown)"
                            [ opacity 100 ];
                        ]
                    [ txt "Please enter a valid email address" ];
                ];
            ];
        ];
    ]

(* Has Selector Section *)
let has_section =
  section
    ~at:[ At.v "aria-labelledby" "has-heading" ]
    ~tw:Tw.[ bg_white; p 6; rounded_xl; shadow_sm; dark [ bg gray 800 ] ]
    [
      h2
        ~at:[ At.id "has-heading" ]
        ~tw:Tw.[ text_xl; font_bold; text gray 900; mb 4; dark [ text_white ] ]
        [ txt ":has() Selector" ];
      p
        ~tw:Tw.[ text gray 600; dark [ text gray 300 ]; mb 6 ]
        [
          txt
            "Style elements based on their descendants using the :has() \
             selector.";
        ];
      div
        ~tw:Tw.[ grid; grid_cols 1; gap 6; md [ grid_cols 2 ] ]
        [
          (* Has checked checkbox *)
          div
            [
              h3
                ~tw:
                  Tw.
                    [
                      text_sm;
                      font_semibold;
                      text gray 700;
                      mb 2;
                      dark [ text gray 300 ];
                    ]
                [ txt "has(:checked)" ];
              div
                ~tw:
                  Tw.
                    [
                      p 4;
                      bg gray 100;
                      rounded_lg;
                      border_md;
                      border_transparent;
                      transition_colors;
                      duration 200;
                      has ":checked" [ bg green 100; border_color green 500 ];
                      dark
                        [
                          bg gray 700;
                          has ":checked"
                            [ bg green 900; border_color green 400 ];
                        ];
                    ]
                [
                  label
                    ~tw:Tw.[ flex; items_center; gap 3; cursor_pointer ]
                    [
                      input ~tw:Tw.[ w 5; h 5 ] ~at:[ At.type' "checkbox" ] ();
                      span
                        ~tw:Tw.[ text gray 700; dark [ text gray 300 ] ]
                        [ txt "Check to change wrapper style" ];
                    ];
                ];
            ];
          (* Group has *)
          div
            [
              h3
                ~tw:
                  Tw.
                    [
                      text_sm;
                      font_semibold;
                      text gray 700;
                      mb 2;
                      dark [ text gray 300 ];
                    ]
                [ txt "group-has" ];
              div
                ~tw:
                  Tw.
                    [
                      group; p 4; bg gray 100; rounded_lg; dark [ bg gray 700 ];
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
                          text gray 500;
                          group_has ":checked" [ text green 600; font_semibold ];
                          dark
                            [
                              text gray 400;
                              group_has ":checked" [ text green 400 ];
                            ];
                        ]
                    [ txt "Selection status" ];
                ];
            ];
        ];
    ]

(* Focus Variants Section *)
let focus_section =
  section
    ~at:[ At.v "aria-labelledby" "focus-heading" ]
    ~tw:Tw.[ bg_white; p 6; rounded_xl; shadow_sm; dark [ bg gray 800 ] ]
    [
      h2
        ~at:[ At.id "focus-heading" ]
        ~tw:Tw.[ text_xl; font_bold; text gray 900; mb 4; dark [ text_white ] ]
        [ txt "Focus Variants" ];
      p
        ~tw:Tw.[ text gray 600; dark [ text gray 300 ]; mb 6 ]
        [ txt "Different focus states for various accessibility and UX needs." ];
      div
        ~tw:Tw.[ grid; grid_cols 1; gap 6; md [ grid_cols 2 ] ]
        [
          (* focus-within *)
          div
            [
              h3
                ~tw:
                  Tw.
                    [
                      text_sm;
                      font_semibold;
                      text gray 700;
                      mb 2;
                      dark [ text gray 300 ];
                    ]
                [ txt "focus-within (wrapper)" ];
              div
                ~tw:
                  Tw.
                    [
                      p 4;
                      bg gray 100;
                      rounded_lg;
                      border_md;
                      border_transparent;
                      transition_all;
                      duration 200;
                      focus_within [ border_color blue 500; shadow_md ];
                      dark [ bg gray 700 ];
                    ]
                [
                  label
                    ~tw:
                      Tw.
                        [
                          block;
                          text_sm;
                          font_medium;
                          text gray 700;
                          mb 1;
                          dark [ text gray 300 ];
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
                          border_color gray 300;
                          rounded_md;
                          focus [ outline_none ];
                          dark
                            [ bg gray 600; border_color gray 500; text_white ];
                        ]
                    ~at:[ At.placeholder "Enter username" ]
                    ();
                ];
            ];
          (* focus-visible *)
          div
            [
              h3
                ~tw:
                  Tw.
                    [
                      text_sm;
                      font_semibold;
                      text gray 700;
                      mb 2;
                      dark [ text gray 300 ];
                    ]
                [ txt "focus-visible (keyboard only)" ];
              div
                ~tw:Tw.[ p 4; bg gray 100; rounded_lg; dark [ bg gray 700 ] ]
                [
                  p
                    ~tw:
                      Tw.
                        [ text_sm; text gray 600; mb 3; dark [ text gray 400 ] ]
                    [ txt "Click vs Tab to see the difference:" ];
                  button
                    ~tw:
                      Tw.
                        [
                          px 4;
                          py 2;
                          bg blue 500;
                          text_white;
                          rounded_lg;
                          focus [ outline_none ];
                          focus_visible [ ring_md; ring_color blue 400 ];
                        ]
                    [ txt "Tab to focus" ];
                ];
            ];
        ];
    ]

(* Pseudo-elements Section *)
let pseudo_section =
  section
    ~at:[ At.v "aria-labelledby" "pseudo-heading" ]
    ~tw:Tw.[ bg_white; p 6; rounded_xl; shadow_sm; dark [ bg gray 800 ] ]
    [
      h2
        ~at:[ At.id "pseudo-heading" ]
        ~tw:Tw.[ text_xl; font_bold; text gray 900; mb 4; dark [ text_white ] ]
        [ txt "Pseudo-elements" ];
      p
        ~tw:Tw.[ text gray 600; dark [ text gray 300 ]; mb 6 ]
        [ txt "Style ::before and ::after pseudo-elements." ];
      div
        ~tw:Tw.[ grid; grid_cols 1; gap 6; md [ grid_cols 2 ] ]
        [
          (* Before *)
          div
            [
              h3
                ~tw:
                  Tw.
                    [
                      text_sm;
                      font_semibold;
                      text gray 700;
                      mb 2;
                      dark [ text gray 300 ];
                    ]
                [ txt "before" ];
              div
                ~tw:
                  Tw.
                    [
                      relative;
                      p 4;
                      pl 8;
                      bg gray 100;
                      rounded_lg;
                      before
                        [
                          absolute;
                          left 3;
                          top 4;
                          w 2;
                          h 2;
                          bg blue 500;
                          rounded_full;
                        ];
                      dark [ bg gray 700 ];
                    ]
                [
                  span
                    ~tw:Tw.[ text gray 700; dark [ text gray 300 ] ]
                    [ txt "List item with custom bullet" ];
                ];
            ];
          (* After *)
          div
            [
              h3
                ~tw:
                  Tw.
                    [
                      text_sm;
                      font_semibold;
                      text gray 700;
                      mb 2;
                      dark [ text gray 300 ];
                    ]
                [ txt "after" ];
              div
                ~tw:
                  Tw.
                    [
                      relative;
                      inline_block;
                      px 4;
                      py 2;
                      bg blue 500;
                      text_white;
                      rounded_lg;
                      after
                        [
                          absolute;
                          right (-1);
                          top (-1);
                          w 3;
                          h 3;
                          bg red 500;
                          rounded_full;
                        ];
                    ]
                [ txt "Badge with notification dot" ];
            ];
        ];
    ]

(* ARIA and Data Attributes Section *)
let aria_section =
  section
    ~at:[ At.v "aria-labelledby" "aria-heading" ]
    ~tw:Tw.[ bg blue 50; p 6; rounded_xl; dark [ bg gray 800 ] ]
    [
      h2
        ~at:[ At.id "aria-heading" ]
        ~tw:Tw.[ text_xl; font_bold; text gray 900; mb 4; dark [ text_white ] ]
        [ txt "ARIA Variants" ];
      p
        ~tw:Tw.[ text gray 600; dark [ text gray 300 ]; mb 6 ]
        [
          txt
            "Style elements based on ARIA attributes for accessible interfaces.";
        ];
      div
        ~tw:Tw.[ grid; grid_cols 1; gap 6; md [ grid_cols 2 ] ]
        [
          (* aria-expanded *)
          div
            [
              h3
                ~tw:
                  Tw.
                    [
                      text_sm;
                      font_semibold;
                      text gray 700;
                      mb 2;
                      dark [ text gray 300 ];
                    ]
                [ txt "aria-expanded" ];
              button
                ~at:
                  [
                    At.v "aria-expanded" "false";
                    At.v "onclick"
                      "this.setAttribute('aria-expanded', \
                       this.getAttribute('aria-expanded') === 'true' ? 'false' \
                       : 'true')";
                  ]
                ~tw:
                  Tw.
                    [
                      w_full;
                      px 4;
                      py 3;
                      bg_white;
                      rounded_lg;
                      shadow_sm;
                      text_left;
                      flex;
                      items_center;
                      justify_between;
                      aria_expanded [ bg blue 100 ];
                      dark [ bg gray 700; aria_expanded [ bg blue 900 ] ];
                    ]
                [
                  span
                    ~tw:Tw.[ font_medium; text gray 800; dark [ text_white ] ]
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
            ];
          (* aria-selected *)
          div
            [
              h3
                ~tw:
                  Tw.
                    [
                      text_sm;
                      font_semibold;
                      text gray 700;
                      mb 2;
                      dark [ text gray 300 ];
                    ]
                [ txt "aria-selected (tabs)" ];
              div
                ~tw:Tw.[ flex; gap 2 ]
                (List.mapi
                   (fun i label ->
                     button
                       ~at:
                         [
                           At.v "aria-selected"
                             (if i = 0 then "true" else "false");
                         ]
                       ~tw:
                         Tw.
                           [
                             px 4;
                             py 2;
                             rounded_lg;
                             text gray 600;
                             aria_selected [ bg blue 500; text_white ];
                             dark
                               [
                                 text gray 400;
                                 aria_selected [ bg blue 600; text_white ];
                               ];
                           ]
                       [ txt label ])
                   [ "Tab 1"; "Tab 2"; "Tab 3" ]);
            ];
        ];
    ]

(* Main page *)
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
                    [ txt "Modifier Utilities" ];
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
                        "Conditional styling with state variants, group/peer \
                         modifiers, pseudo-elements, and ARIA support.";
                    ];
                ];
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
