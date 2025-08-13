(** Tailwind CSS v4 Features Demo

    This example showcases the new features available in the v4 migration:
    - JIT compilation for optimized CSS output
    - New v4 modifiers (has, not, group-has, focus-within, etc.)
    - OKLCH color space support
    - CSS layers structure *)

open Tw_html

let () =
  (* Example 1: Card component with v4 modifiers *)
  let card =
    div
      ~tw:
        Tw.
          [
            (* Base styles *)
            p 4;
            rounded_lg;
            bg_white;
            (* Shadow that changes on hover *)
            shadow_md;
            on_hover [ shadow_xl ];
            (* New v4: focus-within for better form UX *)
            on_focus_within [ ring_md; ring_color blue 500 ];
            (* New v4: has modifier - style changes when containing an image *)
            has "img" [ p 0 ];
          ]
      [
        (* Card header with group interaction *)
        div
          ~tw:Tw.[ group; cursor_pointer ]
          [
            h2
              ~tw:
                Tw.
                  [
                    text_xl;
                    font_bold;
                    mb 2;
                    (* New v4: group hover - style when parent group is
                       hovered *)
                    on_group_hover [ text blue 600 ];
                  ]
              [ txt "Interactive Card" ];
            p
              ~tw:
                Tw.
                  [
                    text gray 600;
                    (* New v4: motion-safe for respectful animations *)
                    motion_safe [ transition_all; duration 300 ];
                    motion_reduce [ transition_none ];
                  ]
              [ txt "This card demonstrates v4 features" ];
          ];
        (* Form input with focus-visible *)
        input
          ~tw:
            Tw.
              [
                w_full;
                p 2;
                border;
                rounded_md;
                (* New v4: focus-visible for keyboard navigation *)
                on_focus_visible [ ring_md; ring_color blue 400 ];
              ]
          ();
      ]
  in

  (* Example 2: Responsive grid with container queries *)
  let responsive_grid =
    div
      ~tw:
        Tw.
          [
            grid;
            grid_cols 1;
            (* Traditional responsive design *)
            on_sm [ grid_cols 2 ];
            on_lg [ grid_cols 3 ];
            gap 4;
          ]
      [
        (* Grid items *)
        div ~tw:Tw.[ p 4; bg gray 100; rounded_md ] [ txt "Item 1" ];
        div ~tw:Tw.[ p 4; bg gray 200; rounded_md ] [ txt "Item 2" ];
        div ~tw:Tw.[ p 4; bg gray 300; rounded_md ] [ txt "Item 3" ];
      ]
  in

  (* Example 3: Dark mode with system preferences *)
  let dark_mode_component =
    div
      ~tw:
        Tw.
          [
            p 6;
            rounded_xl;
            (* Light mode (default) *)
            bg_white;
            text gray 900;
            (* Dark mode styles *)
            on_dark [ bg gray 900; text_white ];
            (* High contrast mode support *)
            contrast_more [ border_lg; border_color black 0 ];
          ]
      [
        h3
          ~tw:Tw.[ text_2xl; font_semibold; mb 4 ]
          [ txt "Accessibility-First Design" ];
        p
          ~tw:Tw.[ text gray 600; on_dark [ text gray 400 ] ]
          [ txt "Automatically adapts to user preferences" ];
      ]
  in

  (* Example 4: Animation with starting styles *)
  let animated_element =
    div
      ~tw:
        Tw.
          [
            (* New v4: starting styles for entry animations *)
            starting [ opacity 0; scale 95 ];
            (* Transition to full opacity and scale *)
            opacity 100;
            scale 100;
            transition_all;
            duration 500;
            p 4;
            bg_gradient_to_r;
            from_color ~shade:500 blue;
            to_color ~shade:600 purple;
            text_white;
            rounded_lg;
          ]
      [ txt "Smooth entry animation with @starting-style" ]
  in

  (* Example 5: Peer modifiers *)
  let peer_example =
    div
      ~tw:Tw.[ flex; flex_col; gap 2 ]
      [
        input
          ~tw:Tw.[ peer; border; p 2; rounded_md ]
          ~at:[ At.type' "email"; At.placeholder "Enter email" ]
          ();
        p
          ~tw:
            Tw.
              [
                text red 500;
                text_sm;
                opacity 0;
                (* New v4: peer-has - style when peer input has certain state *)
                peer_has ":invalid" [ opacity 100 ];
              ]
          [ txt "Please provide a valid email address" ];
      ]
  in

  (* Example 6: Group-has modifier *)
  let group_has_example =
    div
      ~tw:Tw.[ group; p 4; border; rounded_lg ]
      [
        h4 ~tw:Tw.[ font_semibold; mb 2 ] [ txt "Task List" ];
        div
          ~tw:Tw.[ flex; flex_col; gap 2 ]
          [
            label
              ~tw:Tw.[ flex; items_center; gap 2 ]
              [ input ~at:[ At.type' "checkbox" ] (); txt "Task 1" ];
            label
              ~tw:Tw.[ flex; items_center; gap 2 ]
              [ input ~at:[ At.type' "checkbox" ] (); txt "Task 2" ];
          ];
        p
          ~tw:
            Tw.
              [
                text gray 500;
                text_sm;
                mt 4;
                (* New v4: group-has - style when group contains checked
                   inputs *)
                group_has "input:checked" [ text green 600; font_semibold ];
              ]
          [ txt "Tasks in progress" ];
      ]
  in

  (* Combine all examples into a page *)
  let demo_page =
    page ~title:"Tailwind CSS v4 Features Demo" ~tw_css:"v4.css" []
      [
        div
          ~tw:Tw.[ max_w_4xl; mx_auto; p 8; flex; flex_col; gap 8 ]
          [
            h1
              ~tw:Tw.[ text_4xl; font_bold; mb 8; text_center ]
              [ txt "Tailwind CSS v4 with OCaml" ];
            (* Feature cards *)
            div
              ~tw:Tw.[ grid; grid_cols 1; on_lg [ grid_cols 2 ]; gap 6 ]
              [
                div
                  [
                    h2
                      ~tw:Tw.[ text_2xl; font_semibold; mb 4 ]
                      [ txt "Interactive Card" ];
                    card;
                  ];
                div
                  [
                    h2
                      ~tw:Tw.[ text_2xl; font_semibold; mb 4 ]
                      [ txt "Responsive Grid" ];
                    responsive_grid;
                  ];
                div
                  [
                    h2
                      ~tw:Tw.[ text_2xl; font_semibold; mb 4 ]
                      [ txt "Dark Mode" ];
                    dark_mode_component;
                  ];
                div
                  [
                    h2
                      ~tw:Tw.[ text_2xl; font_semibold; mb 4 ]
                      [ txt "Entry Animation" ];
                    animated_element;
                  ];
                div
                  [
                    h2
                      ~tw:Tw.[ text_2xl; font_semibold; mb 4 ]
                      [ txt "Peer Modifiers" ];
                    peer_example;
                  ];
                div
                  [
                    h2
                      ~tw:Tw.[ text_2xl; font_semibold; mb 4 ]
                      [ txt "Group Has" ];
                    group_has_example;
                  ];
              ];
            (* JIT compilation demo *)
            div
              ~tw:Tw.[ mt 8; p 4; bg gray 50; rounded_md ]
              [
                code
                  ~tw:Tw.[ text_sm; font_mono ]
                  [ txt "Tip: JIT compilation significantly reduces CSS size" ];
              ];
          ];
      ]
  in

  (* Generate CSS *)
  let css_file, stylesheet = css demo_page in
  let html_string = html demo_page in
  let css_string = Tw.Css.to_string ~minify:true stylesheet in

  (* Write HTML file *)
  let oc_html = open_out "v4.html" in
  output_string oc_html html_string;
  close_out oc_html;

  (* Write CSS file *)
  let oc_css = open_out css_file in
  output_string oc_css css_string;
  close_out oc_css;

  (* Silent during build - no output *)
  ()
