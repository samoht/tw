(** Feature: Color palette demo

    Shows background and text colors across shades, plus gray scale. *)

open Tw_html

let swatch ~label ~tw = div ~tw [ span ~tw:Tw.[ text_sm ] [ txt label ] ]

let hue_row ~title ~make_color =
  let shades =
    [
      (`S50, "50");
      (`S100, "100");
      (`S200, "200");
      (`S300, "300");
      (`S400, "400");
      (`S500, "500");
      (`S600, "600");
      (`S700, "700");
      (`S800, "800");
      (`S900, "900");
    ]
  in
  let cells =
    List.map
      (fun (shade, label) ->
        swatch ~label
          ~tw:Tw.[ make_color shade; text ~shade:`S900 gray; rounded_md ])
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
            ~tw:Tw.[ p 4; bg ~shade:`S50 gray; rounded_md ]
            [
              p
                ~tw:Tw.[ text ~shade:`S800 gray ]
                [ txt "Text gray-800 on gray-50" ];
            ];
          (* Light text on dark bg *)
          div
            ~tw:Tw.[ p 4; bg ~shade:`S900 gray; rounded_md ]
            [ p ~tw:Tw.[ text white ] [ txt "Text white on gray-900" ] ];
        ];
    ]

let page_view =
  page ~title:"Colors Demo" ~tw_css:(Link "colors.css") []
    [
      div
        ~tw:Tw.[ max_w_6xl; mx_auto; p 8; flex; flex_col; gap 6 ]
        [
          h1 ~tw:Tw.[ text_4xl; font_bold; mb 2; text_center ] [ txt "Colors" ];
          p
            ~tw:Tw.[ text ~shade:`S600 gray; text_center ]
            [ txt "Palette swatches for common hues and gray scale" ];
          hue_row ~title:"Red" ~make_color:(fun s -> Tw.bg ~shade:s Tw.red);
          hue_row ~title:"Orange" ~make_color:(fun s ->
              Tw.bg ~shade:s Tw.orange);
          hue_row ~title:"Yellow" ~make_color:(fun s ->
              Tw.bg ~shade:s Tw.yellow);
          hue_row ~title:"Green" ~make_color:(fun s -> Tw.bg ~shade:s Tw.green);
          hue_row ~title:"Blue" ~make_color:(fun s -> Tw.bg ~shade:s Tw.blue);
          hue_row ~title:"Indigo" ~make_color:(fun s ->
              Tw.bg ~shade:s Tw.indigo);
          hue_row ~title:"Purple" ~make_color:(fun s ->
              Tw.bg ~shade:s Tw.purple);
          hue_row ~title:"Pink" ~make_color:(fun s -> Tw.bg ~shade:s Tw.pink);
          hue_row ~title:"Gray" ~make_color:(fun s -> Tw.bg ~shade:s Tw.gray);
          text_on_bg_examples;
        ];
    ]

let () =
  let html_str = html page_view in
  let css_file, css_stylesheet = css page_view in
  let css_str = Tw.Css.to_string ~minify:true css_stylesheet in
  let oc_html = open_out "index.html" in
  output_string oc_html html_str;
  close_out oc_html;
  Option.iter
    (fun file ->
      let oc_css = open_out file in
      output_string oc_css css_str;
      close_out oc_css)
    css_file;
  ()
