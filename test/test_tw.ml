(** Tests for the Tw module using exact byte-to-byte comparison with Tailwind v4

    All tests use the `check` function which compares our CSS output directly
    with real Tailwind CSS output for exact correspondence. This ensures 1:1
    compatibility. *)

open Alcotest
open Tw

(* CSS generation *)
let generate_tw_css ?(minify = false) ?(optimize = true) styles =
  let stylesheet = to_css ~base:true ~optimize styles in
  Css.to_string ~minify ~optimize stylesheet

let generate_tailwind_css = Tw_tools.Tailwind_gen.generate

(* File utilities *)
let write_file path content =
  let oc = open_out path in
  output_string oc content;
  close_out oc

let slugify s =
  let b = Buffer.create (String.length s) in
  String.iter
    (fun c ->
      if
        (c >= 'a' && c <= 'z')
        || (c >= 'A' && c <= 'Z')
        || (c >= '0' && c <= '9')
        || c = '_'
      then Buffer.add_char b c)
    s;
  Buffer.contents b

(* Test name generation *)
let make_test_name = function
  | [] -> "empty"
  | names ->
      let full_name = String.concat " " names in
      if String.length full_name > 100 then String.sub full_name 0 97 ^ "..."
      else full_name

let make_debug_files test_name tw_css tailwind_css =
  let out_dir = "/tmp" in
  let test_name_slug = slugify test_name in
  let tw_file =
    Filename.concat out_dir ("test_css_tw_" ^ test_name_slug ^ ".css")
  in
  let tailwind_file =
    Filename.concat out_dir ("test_css_tailwind_" ^ test_name_slug ^ ".css")
  in
  write_file tw_file tw_css;
  write_file tailwind_file tailwind_css;
  (tw_file, tailwind_file)

(* Test configuration helpers *)
let test_config tw_styles classnames ~minify ~optimize =
  let tw =
    generate_tw_css ~minify ~optimize tw_styles
    |> Tw_tools.Css_compare.strip_header |> String.trim
  in
  let tailwind =
    generate_tailwind_css ~minify ~optimize classnames
    |> Tw_tools.Css_compare.strip_header |> String.trim
  in
  (tw = tailwind, tw, tailwind)

(* Failure reporting *)
let report_failure test_name tw_file tailwind_file =
  Fmt.epr "@[<v>@,";
  Fmt.epr "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━@,";
  Fmt.epr "FAILED: %s@," test_name;
  Fmt.epr "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━@,";
  Fmt.epr "@,Debug files written to:@,";
  Fmt.epr "  diff -u %s %s@," tw_file tailwind_file;
  Fmt.epr "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━@,"

let pp_parse_error (Css_parser.Parse_error (msg, reader)) =
  let before, after = Css_parser.Reader.context_string reader in
  let pos = Css_parser.Reader.position reader in
  let len = Css_parser.Reader.length reader in
  Fmt.str "%s@,Position %d/%d:@,%s[HERE]%s" msg pos len before after

let are_similar_errors e1 e2 =
  let (Css_parser.Parse_error (msg1, r1)) = e1 in
  let (Css_parser.Parse_error (msg2, r2)) = e2 in
  if msg1 <> msg2 then false
  else
    let b1, a1 = Css_parser.Reader.context_string ~window:20 r1 in
    let b2, a2 = Css_parser.Reader.context_string ~window:20 r2 in
    b1 = b2 && a1 = a2

let show_diff_with_label label tw_css tailwind_css tw_file tailwind_file =
  Fmt.epr "@,▶ %s@,@," label;
  let tw_css = Tw_tools.Css_compare.strip_header tw_css in
  let tailwind_css = Tw_tools.Css_compare.strip_header tailwind_css in
  (match (Css_parser.of_string tw_css, Css_parser.of_string tailwind_css) with
  | Ok ast1, Ok ast2 ->
      let diff_result = Tw_tools.Css_compare.diff ast1 ast2 in
      Fmt.epr "%a@," Tw_tools.Css_compare.pp diff_result
  | Error e1, Error e2 ->
      if are_similar_errors e1 e2 then
        Fmt.epr
          "Parse error in both CSS: %s@,\
           CSS sizes: TW=%d chars, Tailwind=%d chars@,"
          (pp_parse_error e1) (String.length tw_css)
          (String.length tailwind_css)
      else
        Fmt.epr
          "Parse errors in both CSS:@,\
          \  TW: %s@,\
          \  Tailwind: %s@,\
           CSS sizes: TW=%d chars, Tailwind=%d chars@,"
          (pp_parse_error e1) (pp_parse_error e2) (String.length tw_css)
          (String.length tailwind_css)
  | Error e1, _ ->
      Fmt.epr
        "Failed to parse TW CSS: %s@,\
         CSS sizes: TW=%d chars, Tailwind=%d chars@,"
        (pp_parse_error e1) (String.length tw_css)
        (String.length tailwind_css)
  | _, Error e2 ->
      Fmt.epr
        "Failed to parse Tailwind CSS: %s@,\
         CSS sizes: TW=%d chars, Tailwind=%d chars@,"
        (pp_parse_error e2) (String.length tw_css)
        (String.length tailwind_css));
  write_file
    (tw_file ^ "."
    ^ String.map (function ' ' -> '_' | c -> c) (String.lowercase_ascii label))
    tw_css;
  write_file
    (tailwind_file ^ "."
    ^ String.map (function ' ' -> '_' | c -> c) (String.lowercase_ascii label))
    tailwind_css

(* Shared CSS pretty-printer for failures *)
let css_testable =
  Alcotest.testable
    (fun fmt css ->
      let display_css =
        try
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
          let after = String.sub css base_end (String.length css - base_end) in
          before ^ "@layer base{...}" ^ after
        with Not_found -> css
      in
      if String.length display_css < 300 then Fmt.pf fmt "\n%s" display_css
      else if String.length display_css < 800 then
        let start = String.sub display_css 0 200 in
        let ending =
          String.sub display_css (String.length display_css - 200) 200
        in
        Fmt.pf fmt "<css: %d chars>\n%s\n...\n%s" (String.length css) start
          ending
      else Fmt.pf fmt "<css: %d chars>" (String.length css))
    String.equal

let check_exact_match tw_styles =
  try
    let tw_styles = match tw_styles with [] -> [] | styles -> styles in
    let classnames = List.map pp tw_styles in

    let tw_css =
      generate_tw_css ~minify:true ~optimize:true tw_styles
      |> Tw_tools.Css_compare.strip_header |> String.trim
    in
    let tailwind_css =
      generate_tailwind_css ~minify:true ~optimize:true classnames
      |> Tw_tools.Css_compare.strip_header |> String.trim
    in

    let test_name = make_test_name classnames in
    let tw_file, tailwind_file =
      make_debug_files test_name tw_css tailwind_css
    in

    if tw_css <> tailwind_css then (
      report_failure test_name tw_file tailwind_file;

      (* Show structural diff for minified+optimized output *)
      let tw_css_stripped = Tw_tools.Css_compare.strip_header tw_css in
      let tailwind_css_stripped =
        Tw_tools.Css_compare.strip_header tailwind_css
      in
      (match
         ( Css_parser.of_string tw_css_stripped,
           Css_parser.of_string tailwind_css_stripped )
       with
      | Ok ast1, Ok ast2 ->
          let diff_result = Tw_tools.Css_compare.diff ast1 ast2 in
          Fmt.epr "@,Production output (minified+optimized):@,@[<v 2>%a@]@,"
            Tw_tools.Css_compare.pp diff_result
      | Error e1, Error e2 ->
          if are_similar_errors e1 e2 then
            Fmt.epr
              "@,\
               Production output (minified+optimized):@,\
               Parse error in both CSS: %s@,\
               CSS sizes: TW=%d chars, Tailwind=%d chars@,"
              (pp_parse_error e1)
              (String.length tw_css_stripped)
              (String.length tailwind_css_stripped)
          else
            Fmt.epr
              "@,\
               Production output (minified+optimized):@,\
               Parse errors in both CSS:@,\
              \  TW: %s@,\
              \  Tailwind: %s@,\
               CSS sizes: TW=%d chars, Tailwind=%d chars@,"
              (pp_parse_error e1) (pp_parse_error e2)
              (String.length tw_css_stripped)
              (String.length tailwind_css_stripped)
      | Error e1, _ ->
          Fmt.epr
            "@,\
             Production output (minified+optimized):@,\
             Failed to parse TW CSS: %s@,\
             CSS sizes: TW=%d chars, Tailwind=%d chars@,"
            (pp_parse_error e1)
            (String.length tw_css_stripped)
            (String.length tailwind_css_stripped)
      | _, Error e2 ->
          Fmt.epr
            "@,\
             Production output (minified+optimized):@,\
             Failed to parse Tailwind CSS: %s@,\
             CSS sizes: TW=%d chars, Tailwind=%d chars@,"
            (pp_parse_error e2)
            (String.length tw_css_stripped)
            (String.length tailwind_css_stripped));

      (* Then show debugging info to help identify the issue *)
      let base_match, base_tw, base_tailwind =
        test_config tw_styles classnames ~minify:false ~optimize:false
      in

      (if not base_match then (
         Fmt.epr "@,Debug: Checking base generation (unoptimized)...@,";
         show_diff_with_label "Base generation differs" base_tw base_tailwind
           tw_file tailwind_file)
       else
         let opt_match, opt_tw, opt_tailwind =
           test_config tw_styles classnames ~minify:false ~optimize:true
         in

         if not opt_match then (
           Fmt.epr "@,Debug: Base matches, checking optimization...@,";
           show_diff_with_label "Optimization differs" opt_tw opt_tailwind
             tw_file tailwind_file));
      Fmt.epr "@,@]");

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
  check_list [ p 4; md [ p 8 ]; lg [ p 12 ] ];
  check_list [ text_sm; sm [ text_base ]; md [ text_lg ]; lg [ text_xl ] ];
  check_list [ hidden; sm [ block ]; md [ flex ] ]

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

let typography_new_utilities () =
  (* Text decoration style *)
  check decoration_solid;
  check decoration_double;
  check decoration_dotted;
  check decoration_dashed;
  check decoration_wavy;
  (* Text decoration color & thickness *)
  check (decoration_color ~shade:500 blue);
  check (decoration_thickness 2);
  check decoration_from_font;
  (* Text underline offset *)
  check underline_offset_2;
  (* Text align already covered; add vertical-align *)
  check align_top;
  check align_middle;
  check align_bottom;
  check align_text_top;
  check align_text_bottom;
  check align_sub;
  check align_super;
  (* Letter spacing covered; add line-clamp *)
  check (line_clamp 3);
  (* Text wrap/overflow *)
  check text_ellipsis;
  check text_clip;
  check text_wrap;
  check text_nowrap;
  check text_balance;
  check text_pretty;
  (* Word/overflow wrap and hyphens *)
  check break_words;
  check break_all;
  check break_keep;
  check hyphens_auto;
  (* List styles *)
  check list_none;
  check list_disc;
  check list_decimal;
  check list_inside;
  check list_outside;
  check list_image_none;
  (* Text indent *)
  check (indent 4);
  (* Font stretch (discrete) *)
  check font_stretch_normal;
  check font_stretch_condensed;
  check font_stretch_expanded;
  (* Numeric variants *)
  check normal_nums;
  check ordinal;
  check slashed_zero;
  check lining_nums;
  check oldstyle_nums;
  check proportional_nums;
  check tabular_nums;
  check diagonal_fractions;
  check stacked_fractions

let responsive () =
  check (md [ block ]);
  check (lg [ flex ]);
  check (xl [ hidden ]);
  check (sm [ p 4 ]);
  check (md [ m 6 ])

let states () =
  check (hover [ bg_white ]);
  check (focus [ bg sky 500 ]);
  check (active [ text gray 900 ])

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
  check (container_sm [ p 4 ]);
  check (container_md [ m 8 ])

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
  check (sm [ block ]);
  check (md [ flex ]);
  check (lg [ grid ]);
  check (xl [ hidden ]);
  check (sm [ text_lg ]);
  check (md [ p 8 ])

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
  check (peer_hover [ text blue 500 ]);
  check (peer_focus [ bg yellow 200 ])

let aria_selectors () =
  check (aria_checked [ text green 500 ]);
  check (aria_disabled [ opacity 50 ])

let data_selectors () =
  check (data_active [ bg blue 100 ]);
  check (data_inactive [ opacity 50 ])

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
  check items_start;
  check items_end;
  check items_baseline;
  check items_stretch;
  check justify_center;
  check justify_between;
  check justify_start;
  check justify_end;
  check justify_around;
  check justify_evenly;
  check flex_1

let grid () =
  check grid;
  check inline_grid;
  check (grid_cols 3);
  check (grid_rows 2);
  check justify_items_center;
  check justify_self_start;
  check justify_self_end;
  check justify_self_center;
  check justify_self_stretch;
  check justify_self_auto

let scroll_snap () =
  check snap_x;
  check snap_y;
  check snap_both;
  check snap_mandatory;
  check snap_proximity;
  check snap_start;
  check snap_center;
  check snap_end

let content_variants () =
  (* before/after + content-none *)
  check_list [ before [ content_none ] ];
  check_list [ after [ content_none ] ];
  (* before + content with auto quotes and arbitrary value class name *)
  check_list [ before [ content "→" ] ];
  (* another simple ASCII content *)
  check_list [ after [ content "*" ] ]

let prose_basic () =
  check prose;
  check prose_sm;
  check prose_lg;
  check prose_xl

let prose_with_modifiers () =
  check (hover [ prose ]);
  check (md [ prose_lg ])

let prose_variants () =
  check prose;
  check prose_sm;
  check prose_lg;
  check prose_xl;
  check prose_2xl

(* ===== PRECEDENCE TESTS ===== *)

let precedence_base_overrides () =
  (* Later base utility should win over earlier base of same kind *)
  check_list [ p 4; p 2 ];
  check_list [ text blue 600; text blue 500 ]

let precedence_breakpoints () =
  (* Responsive variants should scope correctly and not affect base *)
  check_list [ p 2; md [ p 8 ] ];
  check_list [ text green 400; sm [ text green 500 ]; lg [ text green 700 ] ]

let precedence_states () =
  (* State modifiers combine without interfering with base *)
  check_list [ bg blue 500; hover [ bg blue 600 ] ];
  check_list [ text gray 500; focus [ text gray 700 ] ]

(* ===== UTILITY/PROPERTY TESTS (keep essential ones) ===== *)

(* Helpers for comprehensive color coverage *)
let shades = [ 50; 100; 200; 300; 400; 500; 600; 700; 800; 900; 950 ]
let bg_shades color shade_list = List.map (fun s -> bg color s) shade_list

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
  let css = to_css combined |> Css.to_string ~minify:true ~optimize:true in
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

let all_colors_grays () =
  check_list
    (bg_shades slate shades @ bg_shades gray shades @ bg_shades zinc shades
   @ bg_shades neutral shades @ bg_shades stone shades)

let all_colors_warm () =
  check_list
    (bg_shades red shades @ bg_shades orange shades @ bg_shades amber shades
   @ bg_shades yellow shades)

let all_colors_greens () =
  check_list
    (bg_shades lime shades @ bg_shades green shades @ bg_shades emerald shades
   @ bg_shades teal shades)

let all_colors_blues () =
  check_list
    (bg_shades cyan shades @ bg_shades sky shades @ bg_shades blue shades)

let all_colors_purples () =
  check_list
    (bg_shades indigo shades @ bg_shades violet shades @ bg_shades purple shades)

let all_colors_pinks () =
  check_list
    (bg_shades fuchsia shades @ bg_shades pink shades @ bg_shades rose shades)

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
    test_case "basic spacing" `Slow basic_spacing;
    test_case "color classes" `Slow color_classes;
    test_case "display classes" `Slow display_classes;
    test_case "sizing" `Slow sizing;
    test_case "typography" `Slow typography;
    test_case "typography new utilities" `Slow typography_new_utilities;
    test_case "responsive" `Slow responsive;
    test_case "states" `Slow states;
    test_case "borders" `Slow borders;
    test_case "shadows" `Slow shadows;
    test_case "negative spacing" `Slow negative_spacing;
    test_case "container queries" `Slow container_queries;
    test_case "hex colors" `Slow hex_colors;
    test_case "gradients" `Slow gradients;
    test_case "responsive breakpoints" `Slow responsive_breakpoints;
    test_case "layout" `Slow layout;
    test_case "opacity" `Slow opacity_effects;
    test_case "extended colors" `Slow extended_colors;
    test_case "peer selectors" `Slow peer_selectors;
    test_case "aria selectors" `Slow aria_selectors;
    test_case "data selectors" `Slow data_selectors;
    test_case "3d transforms" `Slow transforms_3d;
    test_case "flexbox" `Slow flexbox;
    test_case "grid" `Slow grid;
    test_case "scroll snap" `Slow scroll_snap;
    test_case "content variants" `Slow content_variants;
    test_case "prose basic" `Slow prose_basic;
    test_case "prose with modifiers" `Slow prose_with_modifiers;
    test_case "prose variants" `Slow prose_variants;
    test_case "precedence base overrides" `Slow precedence_base_overrides;
    test_case "precedence breakpoints" `Slow precedence_breakpoints;
    test_case "precedence states" `Slow precedence_states;
    test_case "inline styles" `Slow inline_styles;
    test_case "style combination" `Slow style_combination;
    test_case "responsive classes" `Slow responsive_classes;
    test_case "multiple classes" `Slow multiple_classes;
    test_case "all colors same shade" `Slow all_colors_same_shade;
    test_case "all colors grays" `Slow all_colors_grays;
    test_case "all colors warm" `Slow all_colors_warm;
    test_case "all colors greens" `Slow all_colors_greens;
    test_case "all colors blues" `Slow all_colors_blues;
    test_case "all colors purples" `Slow all_colors_purples;
    test_case "all colors pinks" `Slow all_colors_pinks;
  ]

let suite = ("tw", core_tests)
