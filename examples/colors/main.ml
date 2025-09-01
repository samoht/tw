(** Feature: Color palette demo

    Shows background and text colors across shades, plus gray scale. *)

open Tw_html

let swatch ~label ~tw = div ~tw [ span ~tw:Tw.[ text_sm ] [ txt label ] ]

let hue_row ~title ~make_color =
  let shades = [ 50; 100; 200; 300; 400; 500; 600; 700; 800; 900 ] in
  let cells =
    List.map
      (fun shade ->
        let label = string_of_int shade in
        swatch ~label ~tw:Tw.[ make_color shade; text gray 900; rounded_md ])
      shades
  in
  div
    [
      h3 ~tw:Tw.[ font_semibold; mb 2 ] [ txt title ];
      div ~tw:Tw.[ grid; grid_cols 5; md [ grid_cols 10 ]; gap 2 ] cells;
    ]

let text_on_bg_examples =
  div
    [
      h3 ~tw:Tw.[ font_semibold; mb 2 ] [ txt "Text on backgrounds" ];
      div
        ~tw:Tw.[ grid; grid_cols 1; md [ grid_cols 2 ]; gap 2 ]
        [
          (* Dark text on light bg *)
          div
            ~tw:Tw.[ p 4; bg gray 50; rounded_md ]
            [ p ~tw:Tw.[ text gray 800 ] [ txt "Text gray-800 on gray-50" ] ];
          (* Light text on dark bg *)
          div
            ~tw:Tw.[ p 4; bg gray 900; rounded_md ]
            [ p ~tw:Tw.[ text_white ] [ txt "Text white on gray-900" ] ];
        ];
    ]

let page_view =
  page ~title:"Colors Demo" ~tw_css:"colors.css" []
    [
      div
        ~tw:Tw.[ max_w_6xl; mx_auto; p 8; flex; flex_col; gap 6 ]
        [
          h1 ~tw:Tw.[ text_4xl; font_bold; mb 2; text_center ] [ txt "Colors" ];
          p
            ~tw:Tw.[ text gray 600; text_center ]
            [ txt "Palette swatches for common hues and gray scale" ];
          hue_row ~title:"Red" ~make_color:(fun s -> Tw.bg Tw.red s);
          hue_row ~title:"Orange" ~make_color:(fun s -> Tw.bg Tw.orange s);
          hue_row ~title:"Yellow" ~make_color:(fun s -> Tw.bg Tw.yellow s);
          hue_row ~title:"Green" ~make_color:(fun s -> Tw.bg Tw.green s);
          hue_row ~title:"Blue" ~make_color:(fun s -> Tw.bg Tw.blue s);
          hue_row ~title:"Indigo" ~make_color:(fun s -> Tw.bg Tw.indigo s);
          hue_row ~title:"Purple" ~make_color:(fun s -> Tw.bg Tw.purple s);
          hue_row ~title:"Pink" ~make_color:(fun s -> Tw.bg Tw.pink s);
          hue_row ~title:"Gray" ~make_color:(fun s -> Tw.bg Tw.gray s);
          text_on_bg_examples;
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
