(* Feature: Form utilities demo

   Shows styled form controls using @tailwindcss/forms plugin utilities. *)

open Tw_html
open At

let label_style = Tw.[ block; text ~shade:700 gray; font_medium; mb 2 ]

let text_fields =
  [
    div
      ~tw:Tw.[ mb 4 ]
      [
        label ~tw:label_style [ txt "Name" ];
        input
          ~at:[ type' "text"; placeholder "Enter your name" ]
          ~tw:Tw.[ w_full ]
          ();
      ];
    div
      ~tw:Tw.[ mb 4 ]
      [
        label ~tw:label_style [ txt "Email" ];
        input
          ~at:[ type' "email"; placeholder "you@example.com" ]
          ~tw:Tw.[ w_full ]
          ();
      ];
    div
      ~tw:Tw.[ mb 4 ]
      [
        label ~tw:label_style [ txt "Topic" ];
        select
          ~tw:Tw.[ w_full ]
          [
            option [ txt "General Inquiry" ];
            option [ txt "Technical Support" ];
            option [ txt "Feedback" ];
          ];
      ];
    div
      ~tw:Tw.[ mb 4 ]
      [
        label ~tw:label_style [ txt "Message" ];
        textarea
          ~at:[ rows 4; placeholder "Tell us what you're thinking..." ]
          ~tw:Tw.[ w_full ]
          [];
      ];
  ]

let choice_fields =
  [
    div
      ~tw:Tw.[ mb 4 ]
      [
        label
          ~tw:Tw.[ flex; items_center; gap 2 ]
          [
            input ~at:[ type' "checkbox" ] ~tw:Tw.[ accent ~shade:600 blue ] ();
            span
              ~tw:Tw.[ text ~shade:700 gray ]
              [ txt "I agree to the terms and conditions" ];
          ];
      ];
    div
      ~tw:Tw.[ mb 6 ]
      [
        p
          ~tw:Tw.[ text ~shade:700 gray; font_medium; mb 2 ]
          [ txt "Preferred contact method:" ];
        div
          ~tw:Tw.[ flex; flex_col; gap 2 ]
          (List.map
             (fun method_name ->
               label
                 ~tw:Tw.[ flex; items_center; gap 2 ]
                 [
                   input
                     ~at:[ type' "radio"; name "contact" ]
                     ~tw:Tw.[ accent ~shade:600 blue ]
                     ();
                   span ~tw:Tw.[ text ~shade:700 gray ] [ txt method_name ];
                 ])
             [ "Email"; "Phone" ]);
      ];
  ]

let submit_button =
  button
    ~at:[ type' "submit" ]
    ~tw:
      Tw.
        [
          w_full;
          bg ~shade:600 blue;
          text white;
          font_medium;
          py 2;
          px 4;
          rounded_md;
          hover [ bg ~shade:700 blue ];
        ]
    [ txt "Send Message" ]

let contact_form =
  div
    ~tw:Tw.[ max_w_md; mx_auto; p 6; bg white; rounded_lg; shadow_md ]
    [
      h2
        ~tw:Tw.[ text_2xl; font_bold; mb 6; text ~shade:900 gray ]
        [ txt "Contact Form" ];
      form (text_fields @ choice_fields @ [ submit_button ]);
    ]

let page_view =
  page ~title:"Forms Demo" ~tw_css:"forms.css" []
    [
      div
        ~tw:Tw.[ min_h_screen; bg ~shade:100 gray; py 12 ]
        [
          h1
            ~tw:
              Tw.
                [ text_4xl; font_bold; mb 8; text_center; text ~shade:900 gray ]
            [ txt "Form Utilities" ];
          contact_form;
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
