(** Feature: Modifiers demo

    Shows group, group-has, peer, peer-has, has, focus-within, focus-visible. *)

open Tw_html

let group_example =
  div
    ~tw:Tw.[ group; p 4; border; rounded_lg ]
    [
      h3 ~tw:Tw.[ font_semibold; mb 2 ] [ txt "Group & Group Hover" ];
      p
        ~tw:Tw.[ text gray 600; group_hover [ text blue 600; font_semibold ] ]
        [ txt "Hover the container to style this text" ];
    ]

let group_has_example =
  div
    ~tw:Tw.[ group; p 4; border; rounded_lg ]
    [
      h3 ~tw:Tw.[ font_semibold; mb 2 ] [ txt "Group Has" ];
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
              group_has "input:checked" [ text green 600; font_semibold ];
            ]
        [ txt "Tasks in progress" ];
    ]

let peer_example =
  div
    ~tw:Tw.[ flex; flex_col; gap 2 ]
    [
      h3 ~tw:Tw.[ font_semibold ] [ txt "Peer & Peer Has" ];
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
              peer_has ":invalid" [ opacity 100 ];
            ]
        [ txt "Please provide a valid email address" ];
    ]

let has_example =
  div
    ~tw:Tw.[ p 4; border; rounded_lg; has "img" [ p 0 ] ]
    [
      h3 ~tw:Tw.[ font_semibold; mb 2 ] [ txt ":has()" ];
      img ~at:[ At.src "https://via.placeholder.com/300x80"; At.alt "img" ] ();
    ]

let focus_examples =
  div
    ~tw:Tw.[ flex; flex_col; gap 4 ]
    [
      div
        [
          h3 ~tw:Tw.[ font_semibold; mb 2 ] [ txt "focus-within" ];
          div
            ~tw:
              Tw.
                [
                  p 4;
                  border;
                  rounded_lg;
                  focus_within [ ring_md; ring_color blue 400 ];
                ]
            [ input ~tw:Tw.[ w_full; p 2; border; rounded_md ] () ];
        ];
      div
        [
          h3 ~tw:Tw.[ font_semibold; mb 2 ] [ txt "focus-visible" ];
          input
            ~tw:
              Tw.
                [
                  w_full;
                  p 2;
                  border;
                  rounded_md;
                  focus_visible [ ring_md; ring_color blue 400 ];
                ]
            ();
        ];
    ]

let demo_page =
  page ~title:"Modifiers Demo" ~tw_css:"modifiers.css" []
    [
      div
        ~tw:Tw.[ max_w_4xl; mx_auto; p 8; flex; flex_col; gap 8 ]
        [
          h1
            ~tw:Tw.[ text_4xl; font_bold; mb 4; text_center ]
            [ txt "Modifiers" ];
          group_example;
          group_has_example;
          peer_example;
          has_example;
          focus_examples;
        ];
    ]

let () =
  let html_str = html demo_page in
  let css_file, css_stylesheet = css demo_page in
  let css_str = Tw.Css.to_string ~minify:true ~optimize:true css_stylesheet in
  let oc_html = open_out "index.html" in
  output_string oc_html html_str;
  close_out oc_html;
  let oc_css = open_out css_file in
  output_string oc_css css_str;
  close_out oc_css;
  ()
