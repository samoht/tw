(** Feature: Form utilities demo

    Shows styled form controls using @tailwindcss/forms plugin utilities. *)

open Tw_html
open At

let contact_form =
  div
    ~tw:Tw.[ max_w_md; mx_auto; p 6; bg_white; rounded_lg; shadow_md ]
    [
      h2
        ~tw:Tw.[ text_2xl; font_bold; mb 6; text gray 900 ]
        [ txt "Contact Form" ];
      form
        [
          (* Text input *)
          div
            ~tw:Tw.[ mb 4 ]
            [
              label
                ~tw:Tw.[ block; text gray 700; font_medium; mb 2 ]
                [ txt "Name" ];
              input
                ~at:[ type' "text"; placeholder "Enter your name" ]
                ~tw:Tw.[ form_input; w_full ]
                ();
            ];
          (* Email input *)
          div
            ~tw:Tw.[ mb 4 ]
            [
              label
                ~tw:Tw.[ block; text gray 700; font_medium; mb 2 ]
                [ txt "Email" ];
              input
                ~at:[ type' "email"; placeholder "you@example.com" ]
                ~tw:Tw.[ form_input; w_full ]
                ();
            ];
          (* Select dropdown *)
          div
            ~tw:Tw.[ mb 4 ]
            [
              label
                ~tw:Tw.[ block; text gray 700; font_medium; mb 2 ]
                [ txt "Topic" ];
              select
                ~tw:Tw.[ form_select; w_full ]
                [
                  option [ txt "General Inquiry" ];
                  option [ txt "Technical Support" ];
                  option [ txt "Feedback" ];
                ];
            ];
          (* Textarea *)
          div
            ~tw:Tw.[ mb 4 ]
            [
              label
                ~tw:Tw.[ block; text gray 700; font_medium; mb 2 ]
                [ txt "Message" ];
              textarea
                ~at:[ rows 4; placeholder "Tell us what you're thinking..." ]
                ~tw:Tw.[ form_textarea; w_full ]
                [];
            ];
          (* Checkbox group *)
          div
            ~tw:Tw.[ mb 4 ]
            [
              label
                ~tw:Tw.[ flex; items_center; gap 2 ]
                [
                  input
                    ~at:[ type' "checkbox" ]
                    ~tw:Tw.[ form_checkbox; text blue 600 ]
                    ();
                  span
                    ~tw:Tw.[ text gray 700 ]
                    [ txt "I agree to the terms and conditions" ];
                ];
            ];
          (* Radio group *)
          div
            ~tw:Tw.[ mb 6 ]
            [
              p
                ~tw:Tw.[ text gray 700; font_medium; mb 2 ]
                [ txt "Preferred contact method:" ];
              div
                ~tw:Tw.[ flex; flex_col; gap 2 ]
                [
                  label
                    ~tw:Tw.[ flex; items_center; gap 2 ]
                    [
                      input
                        ~at:[ type' "radio"; name "contact" ]
                        ~tw:Tw.[ form_radio; text blue 600 ]
                        ();
                      span ~tw:Tw.[ text gray 700 ] [ txt "Email" ];
                    ];
                  label
                    ~tw:Tw.[ flex; items_center; gap 2 ]
                    [
                      input
                        ~at:[ type' "radio"; name "contact" ]
                        ~tw:Tw.[ form_radio; text blue 600 ]
                        ();
                      span ~tw:Tw.[ text gray 700 ] [ txt "Phone" ];
                    ];
                ];
            ];
          (* Submit button *)
          button
            ~at:[ type' "submit" ]
            ~tw:
              Tw.
                [
                  w_full;
                  bg blue 600;
                  text_white;
                  font_medium;
                  py 2;
                  px 4;
                  rounded_md;
                  hover [ bg blue 700 ];
                ]
            [ txt "Send Message" ];
        ];
    ]

let page_view =
  page ~title:"Forms Demo" ~tw_css:"forms.css" []
    [
      div
        ~tw:Tw.[ min_h_screen; bg gray 100; py 12 ]
        [
          h1
            ~tw:Tw.[ text_4xl; font_bold; mb 8; text_center; text gray 900 ]
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
