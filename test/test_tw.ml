(** Tests for the Tw module using exact byte-to-byte comparison with Tailwind v4

    All tests use the `check` function which compares our CSS output directly
    with real Tailwind CSS output for exact correspondence. This ensures 1:1
    compatibility. *)

open Alcotest
open Tw

let generate_tw_css ?(minify = false) ?(preserve_order = false) styles =
  let stylesheet = to_css ~reset:true styles in
  Css.to_string ~minify ~preserve_order stylesheet

let generate_tailwind_css = Test_lib.Tailwind_gen.generate

let check_exact_match tw_styles =
  try
    let tw_styles = match tw_styles with [] -> [] | styles -> styles in
    let classnames = List.map pp tw_styles in
    let tw_css =
      generate_tw_css ~minify:true tw_styles
      |> Test_lib.Css_compare.strip_header |> String.trim
    in
    let tailwind_css =
      generate_tailwind_css ~minify:true classnames
      |> Test_lib.Css_compare.strip_header |> String.trim
    in

    let test_name =
      match classnames with [] -> "empty" | names -> String.concat " " names
    in

    if tw_css <> tailwind_css then (
      Fmt.epr "\n=== CSS MISMATCH for %s ===\n" test_name;

      (* Use the CSS comparison library for better diffs *)
      let diff_output = Test_lib.Css_compare.format_diff tw_css tailwind_css in
      Fmt.epr "%s\n" diff_output;

      (* Also use detailed debugging *)
      (match Test_lib.Css_debug.find_first_diff tw_css tailwind_css with
      | Some (_pos, desc, context) ->
          Fmt.epr "\nFirst difference: %s\n%s\n" desc context
      | None -> ());

      (* Save for inspection if needed *)
      (* let save_path = Test_lib.Css_debug.save_for_inspection 
        ~our_css:tw_css ~tailwind_css ~test_name in
      Fmt.epr "\n%s\n" save_path; *)
      Fmt.epr "===============================\n");

    Alcotest.check string
      (Fmt.str "%s CSS exact match" test_name)
      tailwind_css tw_css
  with
  | Failure msg -> fail ("Test setup failed: " ^ msg)
  | exn -> fail ("Unexpected error: " ^ Printexc.to_string exn)

let check tw_style = check_exact_match [ tw_style ]
let check_list tw_styles = check_exact_match tw_styles

(* ===== CORE TESTS (renamed to shorter names) ===== *)

let empty_test () =
  (* Test with no styles to see base output *)
  check_list []

let multiple_classes () =
  (* Test with multiple classes together *)
  check_list [ p 4; m 2; bg blue 500; text white 0 ];
  check_list [ flex; items_center; justify_center; gap 4 ];
  check_list [ rounded_lg; shadow_md; p 6 ]

let responsive_classes () =
  (* Test responsive modifiers with multiple classes *)
  check_list [ p 4; on_md [ p 8 ]; on_lg [ p 12 ] ];
  check_list
    [ text_sm; on_sm [ text_base ]; on_md [ text_lg ]; on_lg [ text_xl ] ];
  check_list [ hidden; on_sm [ block ]; on_md [ flex ] ]

let basic_spacing () =
  check (p 4);
  check (m 2);
  check (px 6);
  check (py 3);
  check (pt 1);
  check (pr 8);
  check (pb 12);
  check (pl 16);
  check (mx 0);
  check (my 10);
  check (mt 20);
  check (mr 24);
  check (mb 56);
  check (ml 6)

let color_classes () =
  check bg_white;
  check (text gray 900);
  check (border_color gray 200);
  check (bg gray 500);
  check (bg sky 600);
  check (text yellow 400);
  check (border_color teal 600)

let display_classes () =
  check block;
  check inline;
  check inline_block;
  check flex;
  check inline_flex;
  check grid;
  check inline_grid;
  check hidden

let sizing () =
  check (w 4);
  check (h 8);
  check w_fit;
  check h_fit;
  check h_full;
  check (min_w 0)

let typography () =
  check text_xs;
  check text_sm;
  check text_base;
  check text_lg;
  check text_xl;
  check text_2xl;
  check text_3xl;
  check text_4xl;
  check font_thin;
  check font_light;
  check font_normal;
  check font_medium;
  check font_semibold;
  check font_bold;
  check font_extrabold;
  check font_black;
  check font_sans;
  check font_serif;
  check font_mono

let responsive () =
  check (on_md [ block ]);
  check (on_lg [ flex ]);
  check (on_xl [ hidden ]);
  check (on_sm [ p 4 ]);
  check (on_md [ m 6 ])

let states () =
  check (on_hover [ bg_white ]);
  check (on_focus [ bg sky 500 ]);
  check (on_active [ text gray 900 ])

let borders () =
  check border_none;
  check border;
  check border_sm;
  check border_md;
  check border_solid;
  check border_dashed;
  check border_dotted;
  check rounded_none;
  check rounded_sm;
  check rounded;
  check rounded_md;
  check rounded_lg;
  check rounded_xl;
  check rounded_full

let shadows () =
  check shadow_sm;
  check shadow;
  check shadow_md;
  check shadow_lg;
  check shadow_xl;
  check shadow_none

let negative_spacing () =
  check (mx (-4));
  check (my (-2));
  check (mt (-8));
  check (ml (-6))

let container_queries () =
  check (on_container_sm [ p 4 ]);
  check (on_container_md [ m 8 ])

let hex_colors () =
  check (bg (hex "#1da1f2") 0);
  check (text (hex "ff5733") 0);
  check (border_color (hex "#00ff00") 0);
  check (bg (hex "rgb(29,161,242)") 0)

let gradients () =
  check (from_color blue ~shade:500);
  check (via_color purple ~shade:500);
  check (to_color pink ~shade:500)

let responsive_breakpoints () =
  check (on_sm [ block ]);
  check (on_md [ flex ]);
  check (on_lg [ grid ]);
  check (on_xl [ hidden ]);
  check (on_sm [ text_lg ]);
  check (on_md [ p 8 ])

let layout () =
  check static;
  check relative;
  check absolute;
  check fixed;
  check sticky;
  check (top 0);
  check (right 0);
  check (bottom 0);
  check (left 0)

let opacity_effects () =
  check (opacity 0);
  check (opacity 50);
  check (opacity 100)

let extended_colors () =
  check (bg slate 50);
  check (bg zinc 500);
  check (bg orange 900);
  check (text amber 100);
  check (text lime 600);
  check (border_color emerald 300);
  check (border_color cyan 700);
  check (bg violet 400);
  check (text fuchsia 800);
  check (bg rose 200)

let peer_selectors () =
  check (on_peer_hover [ text blue 500 ]);
  check (on_peer_focus [ bg yellow 200 ])

let aria_selectors () =
  check (on_aria_checked [ text green 500 ]);
  check (on_aria_disabled [ opacity 50 ])

let data_selectors () =
  check (on_data_active [ bg blue 100 ]);
  check (on_data_inactive [ opacity 50 ])

let transforms_3d () =
  check (rotate 45);
  check (rotate 90);
  check (rotate 180);
  check (scale 50);
  check (scale 75);
  check (scale 125)

let flexbox () =
  check flex_row;
  check flex_col;
  check flex_wrap;
  check items_center;
  check justify_center;
  check justify_between;
  check flex_1

let scroll_snap () =
  check snap_x;
  check snap_y;
  check snap_both;
  check snap_mandatory;
  check snap_proximity;
  check snap_start;
  check snap_center;
  check snap_end

let prose_basic () =
  check prose;
  check prose_sm;
  check prose_lg;
  check prose_xl

let prose_with_modifiers () =
  check (on_hover [ prose ]);
  check (on_md [ prose_lg ])

let prose_variants () =
  check prose;
  check prose_sm;
  check prose_lg;
  check prose_xl;
  check prose_2xl

(* ===== UTILITY/PROPERTY TESTS (keep essential ones) ===== *)

let inline_styles () =
  let inline = to_inline_style [ p 4; bg blue 500; text white 0 ] in
  let expected_patterns = [ "padding:"; "background-color:"; "color:" ] in
  List.iter
    (fun pattern ->
      if
        not
          (String.contains inline ':'
          && Astring.String.is_infix ~affix:pattern inline)
      then
        Alcotest.failf "Missing pattern %s in inline styles: %s" pattern inline)
    expected_patterns

let style_combination () =
  let combined = [ p 4; bg blue 500; text white 0; rounded_lg ] in
  let css = to_css combined |> Css.to_string in
  let expected_classes =
    [ ".p-4"; ".bg-blue-500"; ".text-white"; ".rounded-lg" ]
  in
  List.iter
    (fun class_name ->
      if
        not
          (String.contains css '.'
          && Astring.String.is_infix ~affix:class_name css)
      then Alcotest.failf "Missing class %s in combined CSS" class_name)
    expected_classes

(* ===== TEST SUITE ===== *)

let core_tests =
  [
    test_case "empty test" `Quick empty_test;
    test_case "multiple classes" `Quick multiple_classes;
    test_case "responsive classes" `Quick responsive_classes;
    test_case "basic spacing" `Quick basic_spacing;
    test_case "color classes" `Quick color_classes;
    test_case "display classes" `Quick display_classes;
    test_case "sizing" `Quick sizing;
    test_case "typography" `Quick typography;
    test_case "responsive" `Quick responsive;
    test_case "states" `Quick states;
    test_case "borders" `Quick borders;
    test_case "shadows" `Quick shadows;
    test_case "negative spacing" `Quick negative_spacing;
    test_case "container queries" `Quick container_queries;
    test_case "hex colors" `Quick hex_colors;
    test_case "gradients" `Quick gradients;
    test_case "responsive breakpoints" `Quick responsive_breakpoints;
    test_case "layout" `Quick layout;
    test_case "opacity" `Quick opacity_effects;
    test_case "extended colors" `Quick extended_colors;
    test_case "peer selectors" `Quick peer_selectors;
    test_case "aria selectors" `Quick aria_selectors;
    test_case "data selectors" `Quick data_selectors;
    test_case "3d transforms" `Quick transforms_3d;
    test_case "flexbox" `Quick flexbox;
    test_case "scroll snap" `Quick scroll_snap;
    test_case "prose basic" `Quick prose_basic;
    test_case "prose with modifiers" `Quick prose_with_modifiers;
    test_case "prose variants" `Quick prose_variants;
    test_case "inline styles" `Quick inline_styles;
    test_case "style combination" `Quick style_combination;
  ]

let suite = ("core_tests", core_tests)
let () = run "tw" [ suite ]
