(** Tests for the Tw module using exact byte-to-byte comparison with Tailwind v4

    All tests use the `check` function which compares our CSS output directly
    with real Tailwind CSS output for exact correspondence. This ensures 1:1
    compatibility. *)

open Alcotest
open Tw

let generate_tw_css ?(minify = false) styles =
  let stylesheet = to_css ~reset:true styles in
  Css.to_string ~minify stylesheet

let generate_tailwind_css = Tw_tools.Tailwind_gen.generate

let check_exact_match tw_styles =
  try
    let tw_styles = match tw_styles with [] -> [] | styles -> styles in
    let classnames = List.map pp tw_styles in
    let tw_css =
      generate_tw_css ~minify:true tw_styles
      |> Tw_tools.Css_compare.strip_header |> String.trim
    in
    let tailwind_css =
      generate_tailwind_css ~minify:true classnames
      |> Tw_tools.Css_compare.strip_header |> String.trim
    in

    let test_name =
      match classnames with
      | [] -> "empty"
      | names ->
          let full_name = String.concat " " names in
          if String.length full_name > 100 then
            String.sub full_name 0 97 ^ "..."
          else full_name
    in

    if tw_css <> tailwind_css then (
      Fmt.epr "\n=== CSS MISMATCH for %s ===\n" test_name;

      (* Use detailed debugging for main diff *)
      match Tw_tools.Css_debug.find_first_diff tw_css tailwind_css with
      | Some (_pos, desc, context) -> Fmt.epr "%s\n%s\n" desc context
      | None ->
          (* Fallback to structural comparison if no char diff found *)
          let diff_output =
            Tw_tools.Css_compare.format_diff tw_css tailwind_css
          in
          Fmt.epr "%s" diff_output);

    (* Use testable with custom pp to display CSS concisely *)
    let css_testable =
      Alcotest.testable
        (fun fmt css ->
          (* For display only: try to show CSS without base layer *)
          let display_css =
            try
              (* Find and remove the entire @layer base{...} block for
                 display *)
              let rec find_layer_end s start depth =
                if start >= String.length s then String.length s
                else
                  match s.[start] with
                  | '{' -> find_layer_end s (start + 1) (depth + 1)
                  | '}' ->
                      if depth = 1 then start + 1
                      else find_layer_end s (start + 1) (depth - 1)
                  | _ -> find_layer_end s (start + 1) depth
              in

              let pattern = "@layer base" in
              let rec find_pattern s pat i =
                if i + String.length pat > String.length s then raise Not_found
                else if String.sub s i (String.length pat) = pat then i
                else find_pattern s pat (i + 1)
              in

              let base_start = find_pattern css pattern 0 in
              let base_end =
                find_layer_end css (base_start + String.length pattern) 0
              in
              let before = String.sub css 0 base_start in
              let after =
                String.sub css base_end (String.length css - base_end)
              in
              before ^ "@layer base{...}" ^ after
            with Not_found -> css
          in

          (* Display logic - just for pretty printing *)
          if String.length display_css < 300 then Fmt.pf fmt "\n%s" display_css
          else if String.length display_css < 800 then
            (* Truncate middle for medium outputs *)
            let start = String.sub display_css 0 200 in
            let ending =
              String.sub display_css (String.length display_css - 200) 200
            in
            Fmt.pf fmt "<css: %d chars>\n%s\n...\n%s" (String.length css) start
              ending
          else Fmt.pf fmt "<css: %d chars>" (String.length css))
        String.equal (* Comparison still uses original strings *)
    in
    let test_label =
      if String.length test_name > 50 then
        String.sub test_name 0 47 ^ "... CSS exact match"
      else Fmt.str "%s CSS exact match" test_name
    in
    Alcotest.check css_testable test_label tailwind_css tw_css
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

let all_colors_comprehensive () =
  (* Test all color scales with common shades to identify mismatches *)
  check_list
    [
      (* Grays *)
      bg slate 50;
      bg slate 100;
      bg slate 200;
      bg slate 300;
      bg slate 400;
      bg slate 500;
      bg slate 600;
      bg slate 700;
      bg slate 800;
      bg slate 900;
      bg slate 950;
      bg gray 50;
      bg gray 100;
      bg gray 200;
      bg gray 300;
      bg gray 400;
      bg gray 500;
      bg gray 600;
      bg gray 700;
      bg gray 800;
      bg gray 900;
      bg gray 950;
      bg zinc 50;
      bg zinc 100;
      bg zinc 200;
      bg zinc 300;
      bg zinc 400;
      bg zinc 500;
      bg zinc 600;
      bg zinc 700;
      bg zinc 800;
      bg zinc 900;
      bg zinc 950;
      bg neutral 50;
      bg neutral 100;
      bg neutral 200;
      bg neutral 300;
      bg neutral 400;
      bg neutral 500;
      bg neutral 600;
      bg neutral 700;
      bg neutral 800;
      bg neutral 900;
      bg neutral 950;
      bg stone 50;
      bg stone 100;
      bg stone 200;
      bg stone 300;
      bg stone 400;
      bg stone 500;
      bg stone 600;
      bg stone 700;
      bg stone 800;
      bg stone 900;
      bg stone 950;
      (* Reds *)
      bg red 50;
      bg red 100;
      bg red 200;
      bg red 300;
      bg red 400;
      bg red 500;
      bg red 600;
      bg red 700;
      bg red 800;
      bg red 900;
      bg red 950;
      (* Oranges *)
      bg orange 50;
      bg orange 100;
      bg orange 200;
      bg orange 300;
      bg orange 400;
      bg orange 500;
      bg orange 600;
      bg orange 700;
      bg orange 800;
      bg orange 900;
      bg orange 950;
      (* Ambers *)
      bg amber 50;
      bg amber 100;
      bg amber 200;
      bg amber 300;
      bg amber 400;
      bg amber 500;
      bg amber 600;
      bg amber 700;
      bg amber 800;
      bg amber 900;
      bg amber 950;
      (* Yellows *)
      bg yellow 50;
      bg yellow 100;
      bg yellow 200;
      bg yellow 300;
      bg yellow 400;
      bg yellow 500;
      bg yellow 600;
      bg yellow 700;
      bg yellow 800;
      bg yellow 900;
      bg yellow 950;
      (* Limes *)
      bg lime 50;
      bg lime 100;
      bg lime 200;
      bg lime 300;
      bg lime 400;
      bg lime 500;
      bg lime 600;
      bg lime 700;
      bg lime 800;
      bg lime 900;
      bg lime 950;
      (* Greens *)
      bg green 50;
      bg green 100;
      bg green 200;
      bg green 300;
      bg green 400;
      bg green 500;
      bg green 600;
      bg green 700;
      bg green 800;
      bg green 900;
      bg green 950;
      (* Emeralds *)
      bg emerald 50;
      bg emerald 100;
      bg emerald 200;
      bg emerald 300;
      bg emerald 400;
      bg emerald 500;
      bg emerald 600;
      bg emerald 700;
      bg emerald 800;
      bg emerald 900;
      bg emerald 950;
      (* Teals *)
      bg teal 50;
      bg teal 100;
      bg teal 200;
      bg teal 300;
      bg teal 400;
      bg teal 500;
      bg teal 600;
      bg teal 700;
      bg teal 800;
      bg teal 900;
      bg teal 950;
      (* Cyans *)
      bg cyan 50;
      bg cyan 100;
      bg cyan 200;
      bg cyan 300;
      bg cyan 400;
      bg cyan 500;
      bg cyan 600;
      bg cyan 700;
      bg cyan 800;
      bg cyan 900;
      bg cyan 950;
      (* Skys *)
      bg sky 50;
      bg sky 100;
      bg sky 200;
      bg sky 300;
      bg sky 400;
      bg sky 500;
      bg sky 600;
      bg sky 700;
      bg sky 800;
      bg sky 900;
      bg sky 950;
      (* Blues *)
      bg blue 50;
      bg blue 100;
      bg blue 200;
      bg blue 300;
      bg blue 400;
      bg blue 500;
      bg blue 600;
      bg blue 700;
      bg blue 800;
      bg blue 900;
      bg blue 950;
      (* Indigos *)
      bg indigo 50;
      bg indigo 100;
      bg indigo 200;
      bg indigo 300;
      bg indigo 400;
      bg indigo 500;
      bg indigo 600;
      bg indigo 700;
      bg indigo 800;
      bg indigo 900;
      bg indigo 950;
      (* Violets *)
      bg violet 50;
      bg violet 100;
      bg violet 200;
      bg violet 300;
      bg violet 400;
      bg violet 500;
      bg violet 600;
      bg violet 700;
      bg violet 800;
      bg violet 900;
      bg violet 950;
      (* Purples *)
      bg purple 50;
      bg purple 100;
      bg purple 200;
      bg purple 300;
      bg purple 400;
      bg purple 500;
      bg purple 600;
      bg purple 700;
      bg purple 800;
      bg purple 900;
      bg purple 950;
      (* Fuchsias *)
      bg fuchsia 50;
      bg fuchsia 100;
      bg fuchsia 200;
      bg fuchsia 300;
      bg fuchsia 400;
      bg fuchsia 500;
      bg fuchsia 600;
      bg fuchsia 700;
      bg fuchsia 800;
      bg fuchsia 900;
      bg fuchsia 950;
      (* Pinks *)
      bg pink 50;
      bg pink 100;
      bg pink 200;
      bg pink 300;
      bg pink 400;
      bg pink 500;
      bg pink 600;
      bg pink 700;
      bg pink 800;
      bg pink 900;
      bg pink 950;
      (* Roses *)
      bg rose 50;
      bg rose 100;
      bg rose 200;
      bg rose 300;
      bg rose 400;
      bg rose 500;
      bg rose 600;
      bg rose 700;
      bg rose 800;
      bg rose 900;
      bg rose 950;
    ]

let all_colors_same_shade () =
  (* Test all colors with shade 500 to verify proper color ordering *)
  check_list
    [
      (* All color families at shade 500 in Tailwind's canonical order *)
      bg slate 500;
      bg gray 500;
      bg zinc 500;
      bg neutral 500;
      bg stone 500;
      bg red 500;
      bg orange 500;
      bg amber 500;
      bg yellow 500;
      bg lime 500;
      bg green 500;
      bg emerald 500;
      bg teal 500;
      bg cyan 500;
      bg sky 500;
      bg blue 500;
      bg indigo 500;
      bg violet 500;
      bg purple 500;
      bg fuchsia 500;
      bg pink 500;
      bg rose 500;
    ]

(* ===== TEST SUITE ===== *)

let core_tests =
  [
    test_case "empty test" `Quick empty_test;
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
    test_case "responsive classes" `Quick responsive_classes;
    test_case "multiple classes" `Quick multiple_classes;
    test_case "all colors same shade" `Quick all_colors_same_shade;
    test_case "all colors comprehensive" `Quick all_colors_comprehensive;
  ]

let suite = ("tw", core_tests)
let () = run "tw" [ suite ]
