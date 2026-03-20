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
let test_name_of = function
  | [] -> "empty"
  | names ->
      let full_name = String.concat " " names in
      if String.length full_name > 100 then String.sub full_name 0 97 ^ "..."
      else full_name

let debug_files test_name tw_css tailwind_css =
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

(* Failure reporting *)
let report_failure test_name tw_file tailwind_file =
  Fmt.epr "@[<v>@,";
  Fmt.epr "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━@,";
  Fmt.epr "FAILED: %s@," test_name;
  Fmt.epr "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━@,";
  Fmt.epr "@,Debug files written to:@,";
  Fmt.epr "  diff -u %s %s@," tw_file tailwind_file;
  Fmt.epr "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━@,@,"

(* Simple CSS testable that just shows diff on failure *)
let css_testable =
  Alcotest.testable
    (fun fmt css -> Fmt.pf fmt "<css: %d chars>" (String.length css))
    String.equal

let check_exact_match tw_styles =
  (* Prepare classnames and raw CSS strings outside the try to use in
     handlers *)
  let tw_styles = match tw_styles with [] -> [] | styles -> styles in
  let classnames = List.map pp tw_styles in
  let tw_css_raw = ref "" in
  let tailwind_css_raw = ref "" in
  try
    (* Generate CSS without stripping for original comparison *)
    tw_css_raw := generate_tw_css ~minify:true ~optimize:true tw_styles;
    tailwind_css_raw :=
      generate_tailwind_css ~minify:true ~optimize:true classnames;

    (* Strip headers for both outputs before any comparison *)
    let tw_css =
      !tw_css_raw |> Css_tools.Css_compare.strip_header |> String.trim
    in
    let tailwind_css =
      !tailwind_css_raw |> Css_tools.Css_compare.strip_header |> String.trim
    in

    let test_name = test_name_of classnames in
    (* Write stripped CSS to test files for better error context *)
    let tw_file, tailwind_file = debug_files test_name tw_css tailwind_css in

    if tw_css <> tailwind_css then (
      report_failure test_name tw_file tailwind_file;

      let diff_result =
        Css_tools.Css_compare.diff ~expected:tailwind_css ~actual:tw_css
      in

      (* Show diff statistics *)
      let stats =
        Css_tools.Css_compare.stats ~expected_str:tailwind_css
          ~actual_str:tw_css diff_result
      in
      Fmt.epr "%a@,@," Css_tools.Css_compare.pp_stats stats;

      (* Show the actual diff *)
      Fmt.epr "%a@,"
        (Css_tools.Css_compare.pp ~expected:"Tailwind (expected)"
           ~actual:"Our TW (actual)")
        diff_result);

    let test_label =
      if String.length test_name > 50 then String.sub test_name 0 47 ^ "..."
      else test_name
    in
    Alcotest.check css_testable test_label tailwind_css tw_css
  with
  | Failure msg -> fail ("Test setup failed: " ^ msg)
  | Css.Reader.Parse_error err ->
      let details = Css.pp_parse_error err in
      (* Print a more helpful parse error with context and callstack. *)
      Fmt.epr "CSS parse error:\n%s@." details;
      (* Also try to show a quick 80-char window around the position in both
         strings for extra context. *)
      let show_window name s pos =
        let start = max 0 (pos - 40) in
        let len = min (String.length s - start) 80 in
        if len > 0 then
          let snippet = String.sub s start len in
          Fmt.epr "[%s context @ %d]: %S@." name pos snippet
      in
      show_window "Our TW" !tw_css_raw err.position;
      show_window "Tailwind" !tailwind_css_raw err.position;
      fail ("CSS parse error: " ^ details)
  | exn ->
      (* Fallback: print full exception type/name if it's not a CSS parse
         error. *)
      let exn_str = Printexc.to_string exn in
      Fmt.epr "Unexpected exception: %s@." exn_str;
      fail ("Unexpected error: " ^ exn_str)

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

let states () =
  check (hover [ bg_white ]);
  check (focus [ bg sky 500 ]);
  check (active [ text gray 900 ]);

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
  check (text (hex "#ff5733") 0);
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
  check (scale 125);

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
  (* NOTE: Using ASCII character instead of Unicode (→) because Tailwind v4
     doesn't support Unicode in class names *)
  check_list [ before [ content ">" ] ];
  (* another simple ASCII content *)
  check_list [ after [ content "*" ] ]

let prose_basic () =
  check prose;
  (* TODO: Implement prose size variants *)
  check prose_sm;
  check prose_lg;
  check prose_xl;
  check prose_2xl

let prose_with_modifiers () =
  check (hover [ prose ]);
  check (md [ prose_lg ])

let prose_ordering () =
  check_list [ mb 0; prose ];
  check_list [ mb 4; mb 8; mb 12; prose ]

(* ===== CROSS-HANDLER ORDERING TESTS ===== *)
(* Test CSS output ordering when mixing utilities from different handlers.
   These catch bugs where sort priority/suborder comparisons produce wrong
   interleaving across handler boundaries. *)

let ordering_padding_margin () =
  check_list [ p 1; px 3; py 3 ];
  check_list [ m 4; p 4 ];
  check_list [ mx_auto; my 4; p 2 ]

let ordering_layout_sizing () =
  check_list [ block; w_full; h 5 ];
  check_list [ flex; flex_col; items_center; gap 4 ]

let ordering_text_bg_border () =
  check_list [ text gray 700; bg white 0; border; border_color gray 300 ];
  check_list [ text_sm; font_bold; text blue 500 ]

let ordering_shadow_ring () =
  check_list [ shadow_sm; ring; ring_color blue 500 ]

let ordering_transitions () = check_list [ transition_colors; duration 200 ]

let ordering_with_variants () =
  check_list [ bg blue 500; hover [ bg blue 600 ]; focus [ ring ] ];
  check_list [ p 4; md [ p 8 ]; lg [ p 12 ] ];
  check_list [ text white 0; dark [ text gray 300 ] ]

let ordering_complex_card () =
  check_list
    [
      flex;
      flex_col;
      gap 4;
      p 6;
      bg white 0;
      rounded_lg;
      shadow_sm;
      border;
      border_color gray 200;
      hover [ shadow_md ];
    ]

(* ===== MISSING FEATURE TESTS ===== *)
(* These tests exercise features that may have incomplete typed API or known
   diffs against real Tailwind. They serve as regression targets: once the
   underlying implementation is fixed, the test should pass. *)

(* Helper: parse a Tailwind class string via of_string; fail the test if the
   class is not recognised by our parser (i.e. the feature is completely
   missing). *)
let require_parse class_name =
  match of_string class_name with
  | Ok v -> v
  | Error (`Msg m) ->
      Alcotest.fail
        (Printf.sprintf "of_string %S failed — API gap: %s" class_name m)

(* -- 1. Fractional spacing ------------------------------------------------ *)
(* Tailwind v4 supports half-step spacing values like space-x-2.5, p-0.5, etc.
   Our typed API only accepts int, so these must go through of_string.
   NOTE: space-x-2.5 is currently rejected by of_string (API gap: fractional
   spacing values are not yet implemented). These tests document the gap. *)

let fractional_spacing () =
  (* space-x / space-y with fractional values *)
  check (require_parse "space-x-2.5");
  check (require_parse "space-y-3.5");
  (* padding with fractional values *)
  check (require_parse "p-0.5");
  check (require_parse "p-1.5");
  (* margin with fractional values *)
  check (require_parse "m-0.5");
  check (require_parse "m-2.5");
  (* gap with fractional values *)
  check (require_parse "gap-0.5");
  check (require_parse "gap-1.5")

let fractional_translate () =
  (* translate-x/y with fractional values (e.g. 1/2, 1/3, 1/4) *)
  (* NOTE: translate-x-1/2 is currently rejected by of_string (API gap:
     fractional translate values are not yet implemented). *)
  check (require_parse "translate-x-1/2");
  check (require_parse "translate-y-1/2");
  check (require_parse "translate-x-1/3");
  check (require_parse "translate-x-2/3");
  check (require_parse "translate-x-1/4");
  check (require_parse "translate-x-3/4");
  check (require_parse "translate-x-full");
  check (require_parse "translate-y-full")

let fractional_width_height () =
  (* Width/height with fractional values *)
  check (require_parse "w-1/2");
  check (require_parse "w-1/3");
  check (require_parse "w-2/3");
  check (require_parse "w-1/4");
  check (require_parse "w-3/4");
  check (require_parse "h-1/2");
  check (require_parse "h-1/3")

(* -- 2. Prose element variant tests --------------------------------------- *)
(* Tailwind typography plugin provides modifier variants like
   prose-headings:text-white, prose-pre:bg-gray-900 etc. that target specific
   element types within prose. Our typed API currently only has prose_lead,
   prose_gray, prose_slate — no element-targeting variants.
   NOTE: These are currently rejected by of_string (API gap). *)

let prose_element_variants () =
  (* prose-headings targets h1-h4 within prose *)
  check (require_parse "prose-headings:text-white");
  (* prose-p targets paragraphs *)
  check (require_parse "prose-p:text-gray-700");
  (* prose-a targets links *)
  check (require_parse "prose-a:text-blue-500");
  (* prose-strong targets <strong> *)
  check (require_parse "prose-strong:font-black");
  (* prose-code targets inline <code> *)
  check (require_parse "prose-code:bg-gray-100");
  (* prose-pre targets <pre> blocks *)
  check (require_parse "prose-pre:bg-gray-900");
  (* prose-ol targets ordered lists *)
  check (require_parse "prose-ol:list-decimal");
  (* prose-ul targets unordered lists *)
  check (require_parse "prose-ul:list-disc");
  (* prose-li targets list items *)
  check (require_parse "prose-li:my-1");
  (* prose-blockquote targets blockquotes *)
  check (require_parse "prose-blockquote:italic");
  (* prose-img targets images *)
  check (require_parse "prose-img:rounded-lg");
  (* prose-hr targets horizontal rules *)
  check (require_parse "prose-hr:border-gray-300")

let prose_element_variants_combined () =
  (* Multiple prose element variants together with base prose *)
  check_list
    [
      prose;
      require_parse "prose-headings:text-blue-600";
      require_parse "prose-a:text-blue-500";
      require_parse "prose-code:bg-gray-100";
    ]

(* -- 3. Divide utilities -------------------------------------------------- *)
(* divide-x and divide-y with width values are parseable by CLI but have no
   typed API (only divide_x_reverse and divide_y_reverse are exposed).
   divide-x/divide-y without explicit widths also exist. *)

let divide_width () =
  (* Basic divide-x and divide-y (1px default) *)
  check (require_parse "divide-x");
  check (require_parse "divide-y");
  (* Divide with explicit widths *)
  check (require_parse "divide-x-2");
  check (require_parse "divide-x-4");
  check (require_parse "divide-x-8");
  check (require_parse "divide-y-2");
  check (require_parse "divide-y-4")

let divide_with_reverse () =
  (* divide-x combined with divide-x-reverse for RTL layouts *)
  check_list [ require_parse "divide-x"; divide_x_reverse ];
  check_list [ require_parse "divide-y"; divide_y_reverse ];
  (* divide with width + reverse *)
  check_list [ require_parse "divide-x-2"; divide_x_reverse ]

let divide_combined () =
  (* divide-x and divide-y used together *)
  check_list [ require_parse "divide-x"; require_parse "divide-y" ];
  (* divide with widths together *)
  check_list [ require_parse "divide-x-2"; require_parse "divide-y-4" ]

let divide_color () =
  (* divide-color utilities *)
  check (require_parse "divide-blue-500");
  check (require_parse "divide-gray-300");
  check (require_parse "divide-red-600")

(* -- 4. Decoration skip ink ----------------------------------------------- *)
(* text-decoration-skip-ink is a CSS property controlling ink skipping around
   descenders. Tailwind v4 has decoration-skip-ink-auto and
   decoration-skip-ink-none.
   NOTE: Currently rejected by of_string (API gap). *)

(* decoration-skip-ink was dropped in Tailwind v4. Verify we reject it. *)
let decoration_skip_ink () =
  (match of_string "decoration-skip-ink-auto" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "decoration-skip-ink-auto should not be a v4 utility");
  match of_string "decoration-skip-ink-none" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "decoration-skip-ink-none should not be a v4 utility"

(* -- 5. Word break utilities ---------------------------------------------- *)
(* These have typed API and should already work. Test them for completeness
   and parity. *)

let word_break_utilities () =
  check break_normal;
  check break_words;
  check break_all;
  check break_keep

(* -- 6. Scroll margin and scroll padding ---------------------------------- *)
(* scroll-margin and scroll-padding utilities exist in the parser (CLI works)
   but have no typed API in tw.mli (marked as TODO).
   Test via of_string. *)

let scroll_margin () =
  check (require_parse "scroll-m-4");
  check (require_parse "scroll-mt-4");
  check (require_parse "scroll-mr-2");
  check (require_parse "scroll-mb-8");
  check (require_parse "scroll-ml-4");
  check (require_parse "scroll-mx-4");
  check (require_parse "scroll-my-2")

let scroll_padding () =
  check (require_parse "scroll-p-4");
  check (require_parse "scroll-pt-4");
  check (require_parse "scroll-pr-2");
  check (require_parse "scroll-pb-8");
  check (require_parse "scroll-pl-4");
  check (require_parse "scroll-px-4");
  check (require_parse "scroll-py-2")

let scroll_margin_padding_combined () =
  check_list [ require_parse "scroll-mt-4"; require_parse "scroll-px-2" ]

(* -- 7. Border spacing axis variants -------------------------------------- *)
(* border-spacing-x and border-spacing-y have no typed API but the parser
   supports them. The combined border-spacing has a typed API. *)

let border_spacing_axis () =
  check (require_parse "border-spacing-x-4");
  check (require_parse "border-spacing-y-2");
  check_list
    [ require_parse "border-spacing-x-4"; require_parse "border-spacing-y-2" ]

let border_spacing_combined () =
  (* Typed API border_spacing combined with axis-specific *)
  check (border_spacing 4.);
  check_list [ border_spacing 4.; require_parse "border-spacing-x-8" ]

(* -- 8. Opacity modifiers (Tailwind v3 compat / v4 approach) -------------- *)
(* In Tailwind v4, bg-opacity-* and text-opacity-* are removed in favor of
   color opacity syntax like bg-blue-500/50. Test that color-with-opacity
   works if the API supports it, and document what's missing.
   NOTE: bg-opacity-* and text-opacity-* are Tailwind v3 utilities and are
   NOT expected to exist in v4. Instead, slash-opacity syntax should be used. *)

let color_opacity_slash () =
  (* Tailwind v4 uses slash syntax: bg-blue-500/50 for 50% opacity *)
  check (require_parse "bg-blue-500/50");
  check (require_parse "bg-blue-500/75");
  check (require_parse "text-white/80");
  check (require_parse "text-gray-900/50");
  check (require_parse "border-red-500/25")

(* -- 9. Nesting variant --------------------------------------------------- *)
(* The * variant (nesting) targets direct children. Test if it's supported. *)

let nesting_variant () =
  check (require_parse "*:p-4");
  check (require_parse "*:text-blue-500");
  check_list [ require_parse "*:p-4"; require_parse "*:m-2" ]

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
          (Astring.String.is_infix ~affix:":" inline
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
          (Astring.String.is_infix ~affix:"." css
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

let color_shade_ordering () =
  (* Test ordering of base colors vs shaded colors to verify formula: base *
     1000 + shade. This ensures proper ordering in theme layer. Tests that
     variables appear in the correct order. *)
  check_list
    [
      (* Base colors: get order base*1000 + 0 *)
      text_black;
      (* --color-black: 1*1000 + 0 = 1000 *)
      bg_white;
      (* --color-white: 24*1000 + 0 = 24000 *)

      (* Regular colors with shades: get order base*1000 + shade *)
      bg blue 400;
      (* --color-blue-400: 12*1000 + 400 = 12400 *)
      bg blue 500;
      (* --color-blue-500: 12*1000 + 500 = 12500 *)
      bg blue 600;
      (* --color-blue-600: 12*1000 + 600 = 12600 *)

      (* Verify order: black < blue-* < white *)
      (* This tests that the variables appear in the theme layer in order:
         --color-black, --color-blue-400, --color-blue-500, --color-blue-600, --color-white *)
    ]

let grid_cols_reordering () =
  (* Test case showing responsive grid columns that trigger reordering
     detection. When both md:grid-cols-2 and md:grid-cols-10 are used, they
     should appear in a specific order within the @media query. This test
     ensures our diff tool can detect and clearly report such reorderings. *)
  check_list [ md [ grid_cols 10 ]; md [ grid_cols 2 ] ]

(* ===== STABLE ORDERING TESTS ===== *)

let gen_minified_css styles =
  to_css ~base:true ~optimize:true styles |> Css.to_string ~minify:true

let assert_stable_ordering ~label styles_a styles_b =
  let a = gen_minified_css styles_a |> Css_tools.Css_compare.strip_header in
  let b = gen_minified_css styles_b |> Css_tools.Css_compare.strip_header in
  if a <> b then (
    Fmt.epr "Stable ordering failed for %s\n" label;
    Fmt.epr "--- A (%s)\n%s\n" label a;
    Fmt.epr "+++ B (%s)\n%s\n" label b;
    Alcotest.fail ("stable ordering differs for " ^ label))
  else Alcotest.(check bool) (label ^ " is stable") true true

let stable_order_basic () =
  (* Order of independent base utilities must not affect CSS output *)
  assert_stable_ordering ~label:"base utils" Tw.[ p 4; m 2 ] Tw.[ m 2; p 4 ]

let stable_order_with_modifiers () =
  (* Regular vs responsive media ordering should be independent of input
     order *)
  assert_stable_ordering ~label:"responsive vs regular"
    Tw.[ bg blue 500; md [ bg red 500 ] ]
    Tw.[ md [ bg red 500 ]; bg blue 500 ]

let stable_order_complex_modifiers () =
  (* Complex selectors like group-has/focus-within must also sort
     deterministically *)
  assert_stable_ordering ~label:"group-has vs responsive"
    Tw.[ group_has "a" [ text black 0 ]; md [ text white 0 ] ]
    Tw.[ md [ text white 0 ]; group_has "a" [ text black 0 ] ];
  assert_stable_ordering ~label:"focus-within vs responsive"
    Tw.[ focus_within [ bg red 500 ]; md [ bg blue 500 ] ]
    Tw.[ md [ bg blue 500 ]; focus_within [ bg red 500 ] ]

(* ===== PROPERTY ORDERING TESTS ===== *)
(* These tests verify that the @layer properties @supports block ordering
   matches Tailwind. The order depends on which utility groups are used:
   - Direct scale (scale-105) puts scale vars before duration
   - Hover scale (hover:scale-105) puts duration before scale *)

let property_order_scale_first () =
  (* When scale is used directly, scale vars should come before duration *)
  check_list Tw.[ transform; duration 300; scale 105 ]

let property_order_duration_first () =
  (* When scale is only via hover, duration should come before scale *)
  check_list Tw.[ transform; duration 300; hover [ scale 105 ] ]

(* ===== TEST SUITE ===== *)

let core_tests =
  [
    test_case "empty test" `Quick empty_test;
    test_case "responsive classes" `Slow responsive_classes;
    test_case "states" `Slow states;
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
    test_case "grid columns reordering" `Slow grid_cols_reordering;
    (* Stable ordering tests *)
    test_case "stable: base utils order" `Quick stable_order_basic;
    test_case "stable: responsive vs regular" `Quick stable_order_with_modifiers;
    test_case "stable: complex modifiers" `Quick stable_order_complex_modifiers;
    (* Property ordering tests *)
    test_case "property order: scale first" `Slow property_order_scale_first;
    test_case "property order: duration first" `Slow
      property_order_duration_first;
    test_case "content variants" `Slow content_variants;
    test_case "prose basic" `Slow prose_basic;
    test_case "prose with modifiers" `Slow prose_with_modifiers;
    test_case "prose ordering" `Quick prose_ordering;
    (* Cross-handler ordering *)
    test_case "order: padding/margin" `Quick ordering_padding_margin;
    test_case "order: layout/sizing" `Quick ordering_layout_sizing;
    test_case "order: text/bg/border" `Quick ordering_text_bg_border;
    test_case "order: shadow/ring" `Quick ordering_shadow_ring;
    test_case "order: transitions" `Quick ordering_transitions;
    test_case "order: with variants" `Quick ordering_with_variants;
    test_case "order: complex card" `Quick ordering_complex_card;
    (* Missing feature tests — expected to fail until features are
       implemented *)
    test_case "fractional spacing" `Slow fractional_spacing;
    test_case "fractional translate" `Slow fractional_translate;
    test_case "fractional width/height" `Slow fractional_width_height;
    test_case "prose element variants" `Slow prose_element_variants;
    test_case "prose element variants combined" `Slow
      prose_element_variants_combined;
    test_case "divide width" `Slow divide_width;
    test_case "divide with reverse" `Slow divide_with_reverse;
    test_case "divide combined" `Slow divide_combined;
    test_case "divide color" `Slow divide_color;
    test_case "decoration skip ink" `Slow decoration_skip_ink;
    test_case "word break utilities" `Slow word_break_utilities;
    test_case "scroll margin" `Slow scroll_margin;
    test_case "scroll padding" `Slow scroll_padding;
    test_case "scroll margin+padding combined" `Slow
      scroll_margin_padding_combined;
    test_case "border spacing axis" `Slow border_spacing_axis;
    test_case "border spacing combined" `Slow border_spacing_combined;
    test_case "color opacity slash" `Slow color_opacity_slash;
    test_case "nesting variant" `Slow nesting_variant;
    test_case "precedence base overrides" `Slow precedence_base_overrides;
    test_case "precedence breakpoints" `Slow precedence_breakpoints;
    test_case "precedence states" `Slow precedence_states;
    test_case "inline styles" `Slow inline_styles;
    test_case "style combination" `Slow style_combination;
    test_case "responsive classes" `Slow responsive_classes;
    test_case "multiple classes" `Slow multiple_classes;
    test_case "all colors same shade" `Slow all_colors_same_shade;
    test_case "color shade ordering" `Slow color_shade_ordering;
    test_case "all colors grays" `Slow all_colors_grays;
    test_case "all colors warm" `Slow all_colors_warm;
    test_case "all colors greens" `Slow all_colors_greens;
    test_case "all colors blues" `Slow all_colors_blues;
    test_case "all colors purples" `Slow all_colors_purples;
    test_case "all colors pinks" `Slow all_colors_pinks;
  ]

let suite = ("tw", core_tests)
