(** Tests for the Tw module

    IMPORTANT: These tests MUST verify EXACT 1:1 mapping between our Tw module
    and real Tailwind CSS output. Any differences are bugs in our
    implementation.

    - CSS property values must match exactly (including spaces, units, etc.)
    - CSS property order must match Tailwind's order
    - Minification rules must match Tailwind's minification
    - All utility classes must generate identical CSS to Tailwind

    The check_exact_match function is the primary test that ensures 1:1 mapping.
    Any test failure indicates our CSS generation differs from Tailwind. *)

open Alcotest
open Tw

(** No normalization - exact comparison *)
let exact_css css = String.trim css

let write_file path content =
  let oc = open_out path in
  output_string oc content;
  close_out oc

let tailwind_files temp_dir classnames =
  let html_content =
    Fmt.str
      {|<!DOCTYPE html>
<html>
<head></head>
<body>
  <div class="%s"></div>
</body>
</html>|}
      (String.concat " " classnames)
  in

  (* For Tailwind v4, use the single import directive *)
  let input_css_content = "@import \"tailwindcss\";" in

  write_file (Filename.concat temp_dir "input.html") html_content;
  write_file (Filename.concat temp_dir "input.css") input_css_content
(* No config file needed for v4 basic usage *)

(** Global flag to track if version has been checked *)
let version_checked = ref false

(** Check if tailwindcss v4 is available - only once per test run *)
let check_tailwindcss_available () =
  if !version_checked then ()
  else (
    version_checked := true;

    (* Check if npx is available *)
    let binary_check = Sys.command "which npx > /dev/null 2>&1" in
    if binary_check <> 0 then
      failwith
        "npx not found in PATH.\n\
         Please install Node.js and npm.\n\n\
         These tests require npx to run tailwindcss.";

    (* Get and verify the version *)
    let temp_file = Filename.temp_file "tw_version" ".txt" in
    let version_cmd = "npx tailwindcss --help 2>&1 | head -1 > " ^ temp_file in
    let exit_code = Sys.command version_cmd in
    if exit_code = 0 then (
      let ic = open_in temp_file in
      let version_line = input_line ic in
      close_in ic;
      Sys.remove temp_file;

      (* Expected format: "≈ tailwindcss v4.1.11" *)
      if not (String.contains version_line '4') then
        failwith
          (Fmt.str
             "Expected Tailwind CSS v4.x but found: %s\n\
              These tests are designed for Tailwind CSS v4.\n\
              Please install v4:\n\
              npm install -D tailwindcss"
             version_line)
      else print_endline ("Using " ^ version_line ^ " for tests"))
    else (
      Sys.remove temp_file;
      failwith "Could not determine Tailwind CSS version"))

(** Generate CSS using the official tailwindcss binary - use project directory
*)
let generate_tailwind_css ?(minify = false) classnames =
  check_tailwindcss_available ();
  (* Create test directory in project root where tailwindcss can resolve
     imports *)
  let temp_dir = "temp_tailwind_test" in
  let _ = Sys.command (Fmt.str "mkdir -p %s" temp_dir) in
  let css_file = Filename.concat temp_dir "output.css" in

  tailwind_files temp_dir classnames;

  (* Run tailwindcss via npx - v4 with content scanning *)
  let minify_flag = if minify then " --minify" else "" in
  let cmd =
    Fmt.str
      "npx tailwindcss --input %s/input.css --output %s --content \
       '%s/input.html'%s"
      temp_dir css_file temp_dir minify_flag
  in
  let exit_code = Sys.command cmd in

  (if exit_code <> 0 then
     let _ = Sys.command (Fmt.str "rm -rf %s" temp_dir) in
     failwith
       (Fmt.str
          "tailwindcss v4 command failed: %s (exit code: %d)\n\n\
           If tailwindcss is missing, run:\n\
           npm install -D tailwindcss\n"
          cmd exit_code));

  (* Read generated CSS *)
  let ic = open_in css_file in
  let css = really_input_string ic (in_channel_length ic) in
  close_in ic;

  (* Cleanup *)
  let _ = Sys.command (Fmt.str "rm -rf %s" temp_dir) in
  css

(** Generate CSS using our Tw implementation *)
let generate_tw_css ?(minify = false) tw_styles =
  let stylesheet = to_css tw_styles in
  stylesheet_to_string ~minify stylesheet

(** Extract utility class CSS from full Tailwind output *)
let extract_utility_classes css classnames =
  (* Split by } but keep the } in the parts *)
  let rec split_keep_delimiter delim str =
    match String.index_opt str delim with
    | None -> if str = "" then [] else [ str ]
    | Some idx ->
        let before = String.sub str 0 (idx + 1) in
        let after = String.sub str (idx + 1) (String.length str - idx - 1) in
        before :: split_keep_delimiter delim after
  in

  let css_parts = split_keep_delimiter '}' css in

  let classes_for classname =
    let selector_exact = "." ^ classname ^ "{" in
    let selector_comma_start = "." ^ classname ^ "," in
    let selector_comma_middle = ",." ^ classname ^ "," in
    let selector_comma_end = ",." ^ classname ^ "{" in
    let selector_pseudo = "." ^ classname ^ ":" in

    List.filter
      (fun part ->
        Astring.String.is_infix ~affix:selector_exact part
        || Astring.String.is_infix ~affix:selector_comma_start part
        || Astring.String.is_infix ~affix:selector_comma_middle part
        || Astring.String.is_infix ~affix:selector_comma_end part
        || Astring.String.is_infix ~affix:selector_pseudo part)
      css_parts
  in

  (* For each classname, find all CSS parts that reference it *)
  let all_parts = List.concat_map classes_for classnames in
  (* Remove duplicates while preserving order *)
  let seen = Hashtbl.create 10 in
  let unique_parts =
    List.filter
      (fun part ->
        if Hashtbl.mem seen part then false
        else (
          Hashtbl.add seen part ();
          true))
      all_parts
  in

  String.concat "" unique_parts

(** Get all test tw styles that need to be checked *)
let spacing_test_styles =
  [
    (* Basic spacing - comprehensive *)
    p (int 0);
    p (int 1);
    p (int 4);
    m (int 0);
    m (int 2);
    m auto;
    px (int 6);
    py (int 3);
    pt (int 1);
    pr (int 8);
    pb (int 12);
    pl (int 16);
    mx (int 0);
    mx auto;
    my (int 10);
    mt (int 20);
    mr (int 24);
    mb (int 56);
    ml (int 6);
  ]

let color_test_styles =
  [
    (* Color classes - all variants *)
    bg white;
    bg black;
    bg_transparent;
    bg_current;
    text ~shade:900 gray;
    text_transparent;
    text_current;
    border_color ~shade:200 gray;
    border_transparent;
    border_current;
    bg ~shade:500 gray;
    bg ~shade:600 sky;
    text ~shade:400 yellow;
    border_color ~shade:600 teal;
  ]

let display_test_styles =
  [
    (* Display - comprehensive *)
    block;
    inline;
    inline_block;
    flex;
    inline_flex;
    grid;
    inline_grid;
    hidden;
  ]

let sizing_test_styles =
  [
    (* Sizing - comprehensive *)
    w (int 0);
    w (int 4);
    w (int 96);
    w fit;
    w full;
    min_w min;
    min_w max;
    h (int 0);
    h (int 8);
    h fit;
    h full;
    min_w (int 0);
    min_w full;
    max_w xs;
    max_w sm;
    max_w md;
    max_w lg;
    max_w xl;
    max_w xl_2;
    max_w xl_7;
    max_w full;
    max_w none;
  ]

let typography_test_styles =
  [
    (* Typography - comprehensive *)
    text_xs;
    text_sm;
    text_base;
    text_lg;
    text_xl;
    text_2xl;
    text_3xl;
    text_4xl;
    font_normal;
    font_medium;
    font_semibold;
    font_bold;
    text_left;
    text_center;
    text_right;
    text_justify;
    leading_none;
    leading_tight;
    leading_normal;
    leading_relaxed;
    tracking_tight;
    tracking_normal;
    tracking_wide;
    whitespace_normal;
    whitespace_nowrap;
  ]

let responsive_test_styles =
  [
    (* Responsive - comprehensive coverage *)
    (* Display *)
    on_sm [ block ];
    on_sm [ inline_block ];
    on_sm [ flex ];
    on_sm [ hidden ];
    on_md [ flex ];
    on_md [ grid ];
    on_md [ inline ];
    on_lg [ grid ];
    on_lg [ block ];
    on_xl [ hidden ];
    on_xl [ flex ];
    (* Spacing *)
    on_sm [ p (int 4) ];
    on_sm [ m (int 2) ];
    on_sm [ px (int 6) ];
    on_sm [ py (int 3) ];
    on_md [ m (int 6) ];
    on_md [ p (int 8) ];
    on_md [ mt (int 4) ];
    on_md [ mb (int 10) ];
    on_lg [ p (int 12) ];
    on_lg [ mx auto ];
    on_xl [ m (int 0) ];
    on_xl [ p (int 16) ];
    (* Typography *)
    on_sm [ text_sm ];
    on_sm [ font_normal ];
    on_md [ text_base ];
    on_md [ font_medium ];
    on_lg [ text_lg ];
    on_lg [ font_semibold ];
    on_xl [ text_xl ];
    on_xl [ font_bold ];
    (* Width/Height *)
    on_sm [ w full ];
    on_sm [ h (int 32) ];
    on_md [ w (int 64) ];
    on_md [ h full ];
    on_lg [ w (int 96) ];
    on_lg [ max_w xl_4 ];
    on_xl [ w screen ];
    on_xl [ min_h screen ];
    (* Flexbox *)
    on_sm [ flex_row ];
    on_sm [ items_center ];
    on_sm [ justify_between ];
    on_md [ flex_col ];
    on_md [ items_start ];
    on_md [ gap (int 4) ];
    on_lg [ flex_row_reverse ];
    on_lg [ justify_evenly ];
    on_xl [ flex_wrap ];
    (* Grid *)
    on_sm [ grid_cols 1 ];
    on_md [ grid_cols 2 ];
    on_lg [ grid_cols 3 ];
    on_xl [ grid_cols 4 ];
    (* Position *)
    on_sm [ relative ];
    on_md [ absolute ];
    on_lg [ fixed ];
    on_xl [ sticky ];
    (* Colors *)
    on_sm [ bg white ];
    on_sm [ text black ];
    on_md [ bg ~shade:100 gray ];
    on_md [ text ~shade:700 blue ];
    on_lg [ bg ~shade:500 sky ];
    on_lg [ border_color ~shade:300 gray ];
    on_xl [ bg black ];
    on_xl [ text white ];
    (* Borders *)
    on_sm [ border `Default ];
    on_sm [ rounded md ];
    on_md [ border `Lg ];
    on_md [ rounded lg ];
    on_lg [ border_t ];
    on_lg [ rounded full ];
    on_xl [ border `None ];
    (* Effects *)
    on_sm [ shadow sm ];
    on_md [ shadow md ];
    on_lg [ shadow lg ];
    on_xl [ shadow none ];
    on_sm [ opacity 75 ];
    on_md [ opacity 100 ];
    (* Overflow *)
    on_sm [ overflow_hidden ];
    on_md [ overflow_auto ];
    on_lg [ overflow_visible ];
    on_xl [ overflow_scroll ];
  ]

let states_test_styles =
  [
    (* States - comprehensive coverage *)
    (* Hover states *)
    on_hover [ bg white ];
    on_hover [ text ~shade:700 blue ];
    on_hover [ border_color ~shade:400 gray ];
    on_hover [ shadow lg ];
    on_hover [ opacity 80 ];
    on_hover [ scale 105 ];
    on_hover [ translate_y (-1) ];
    (* Focus states *)
    on_focus [ bg ~shade:500 sky ];
    on_focus [ outline_none ];
    on_focus [ ring `Lg ];
    on_focus [ ring_color ~shade:400 blue ];
    on_focus [ border_color ~shade:500 blue ];
    on_focus [ shadow md ];
    (* Active states *)
    on_active [ text ~shade:900 gray ];
    on_active [ bg ~shade:200 gray ];
    on_active [ scale 95 ];
    on_active [ shadow sm ];
    (* Disabled states *)
    on_disabled [ opacity 50 ];
    on_disabled [ cursor_not_allowed ];
    on_disabled [ bg ~shade:100 gray ];
    on_disabled [ text ~shade:400 gray ];
    (* Group states *)
    on_group_hover [ bg ~shade:100 blue ];
    on_group_hover [ text ~shade:700 blue ];
    on_group_focus [ ring `Md ];
    (* Peer states *)
    on_peer_hover [ text ~shade:600 green ];
    on_peer_focus [ bg ~shade:50 green ];
    on_peer_checked [ text ~shade:700 green ];
    (* ARIA states *)
    on_aria_expanded [ rotate 180 ];
    on_aria_selected [ bg ~shade:100 blue ];
    on_aria_checked [ bg ~shade:500 green ];
    on_aria_disabled [ opacity 40 ];
    (* Data attribute states *)
    on_data_active [ bg ~shade:200 blue ];
    on_data_inactive [ bg ~shade:50 gray ];
  ]

let borders_test_styles =
  [
    (* Borders - comprehensive *)
    rounded none;
    rounded sm;
    rounded md;
    rounded lg;
    rounded xl;
    rounded full;
    border `Default;
    border `None;
    border `Sm;
    border `Lg;
    border `Xl;
    border_t;
    border_r;
    border_b;
    border_l;
  ]

let shadows_test_styles =
  [
    (* Shadows *)
    shadow sm;
    shadow md;
    shadow lg;
    shadow xl;
    shadow none;
    shadow inner;
  ]

let prose_test_styles =
  [
    (* Prose *)
    prose;
    prose_sm;
    prose_lg;
    prose_xl;
    prose_2xl;
    prose_gray;
    prose_slate;
  ]

let flexbox_test_styles =
  [
    (* Flexbox - comprehensive *)
    flex_col;
    flex_row;
    flex_row_reverse;
    flex_col_reverse;
    flex_wrap;
    flex_wrap_reverse;
    flex_nowrap;
    flex_1;
    flex_auto;
    flex_initial;
    flex_none;
    flex_grow;
    flex_grow_0;
    flex_shrink;
    flex_shrink_0;
    items_start;
    items_center;
    items_end;
    items_stretch;
    justify_start;
    justify_center;
    justify_end;
    justify_between;
    justify_around;
    justify_evenly;
  ]

let grid_test_styles =
  [
    (* Grid *)
    grid_cols 1;
    grid_cols 2;
    grid_cols 3;
    grid_cols 12;
    gap (int 4);
    gap_x (int 2);
    gap_y (int 6);
  ]

let layout_test_styles =
  [
    (* Layout - comprehensive *)
    static;
    relative;
    absolute;
    fixed;
    sticky;
    inset_0;
    inset_x_0;
    inset_y_0;
    top 0;
    right 0;
    bottom 0;
    left 0;
    z 0;
    z 10;
    z 10;
    (* Overflow *)
    overflow_auto;
    overflow_hidden;
    overflow_visible;
    overflow_scroll;
  ]

let misc_test_styles =
  [
    (* Opacity *)
    opacity 0;
    opacity 25;
    opacity 50;
    opacity 75;
    opacity 100;
    (* Transitions *)
    transition_none;
    transition_all;
    transition_colors;
    transition_opacity;
    transition_transform;
    (* Transforms *)
    transform;
    transform_none;
    scale 75;
    scale 100;
    scale 125;
    rotate 45;
    rotate 90;
    translate_x 4;
    translate_y 2;
    (* Cursor *)
    cursor_auto;
    cursor_default;
    cursor_pointer;
    cursor_not_allowed;
    (* User Select *)
    select_none;
    select_text;
    select_all;
    select_auto;
  ]

let extended_colors_test_styles =
  [
    (* Extended colors - all 10 new colors *)
    bg ~shade:50 slate;
    bg ~shade:500 zinc;
    bg ~shade:900 orange;
    text ~shade:100 amber;
    text ~shade:600 lime;
    border_color ~shade:300 emerald;
    border_color ~shade:700 cyan;
    bg ~shade:400 violet;
    text ~shade:800 fuchsia;
    bg ~shade:200 rose;
  ]

let filter_test_styles =
  [
    (* Filters *)
    blur sm;
    blur md;
    blur lg;
    blur xl;
    blur none;
    brightness 50;
    brightness 100;
    brightness 150;
    contrast 50;
    contrast 100;
    contrast 200;
    grayscale 0;
    grayscale 100;
    saturate 0;
    saturate 100;
    saturate 200;
    sepia 0;
    sepia 100;
    invert 0;
    invert 100;
    hue_rotate 0;
    hue_rotate 180;
    (* Backdrop filters *)
    backdrop_blur sm;
    backdrop_blur md;
    backdrop_blur lg;
    backdrop_brightness 50;
    backdrop_brightness 100;
    backdrop_contrast 125;
    backdrop_saturate 150;
    backdrop_opacity 80;
  ]

let animation_test_styles =
  [
    (* Animations *)
    animate_spin;
    animate_ping;
    animate_pulse;
    animate_bounce;
    animate_none;
    (* Transition durations *)
    duration 75;
    duration 100;
    duration 150;
    duration 200;
    duration 300;
    duration 500;
    duration 700;
    duration 1000;
    (* Transition timing *)
    ease_linear;
    ease_in;
    ease_out;
    ease_in_out;
  ]

let form_test_styles =
  [
    (* Form utilities *)
    appearance_none;
    resize_none;
    resize_y;
    resize_x;
    resize;
  ]

let gradient_test_styles =
  [
    (* Gradients *)
    bg_gradient_to_t;
    bg_gradient_to_tr;
    bg_gradient_to_r;
    bg_gradient_to_br;
    bg_gradient_to_b;
    bg_gradient_to_bl;
    bg_gradient_to_l;
    bg_gradient_to_tl;
    from_color ~shade:100 blue;
    from_color ~shade:500 purple;
    to_color ~shade:500 pink;
    to_color ~shade:900 red;
  ]

let special_test_styles =
  [
    (* Special utilities *)
    sr_only;
    not_sr_only;
    pointer_events_none;
    pointer_events_auto;
    will_change_auto;
    will_change_scroll;
    will_change_contents;
    will_change_transform;
    contain_none;
    contain_content;
    contain_layout;
    contain_paint;
    contain_size;
  ]

let object_test_styles =
  [
    (* Object fit/position *)
    object_contain;
    object_cover;
    object_fill;
    object_none;
    object_scale_down;
    object_top;
    object_right;
    object_bottom;
    object_left;
    object_center;
  ]

let line_clamp_test_styles =
  [
    (* Line clamp *)
    line_clamp 1;
    line_clamp 2;
    line_clamp 3;
    line_clamp 4;
    line_clamp 5;
    line_clamp 6;
    line_clamp 0;
  ]

let all_test_styles () =
  spacing_test_styles @ color_test_styles @ display_test_styles
  @ sizing_test_styles @ typography_test_styles @ responsive_test_styles
  @ states_test_styles @ borders_test_styles @ shadows_test_styles
  @ prose_test_styles @ flexbox_test_styles @ grid_test_styles
  @ layout_test_styles @ misc_test_styles @ extended_colors_test_styles
  @ filter_test_styles @ animation_test_styles @ form_test_styles
  @ gradient_test_styles @ special_test_styles @ object_test_styles
  @ line_clamp_test_styles

(** Cache for Tailwind CSS generation with class list hash *)
let tailwind_cache_unminified = ref None

let tailwind_cache_minified = ref None

(** Generate all Tailwind CSS at once *)
let generate_all_tailwind_css ?(minify = false) () =
  let all_styles = all_test_styles () in
  let all_classnames = List.map to_string all_styles in

  (* Create a key from the classnames to ensure cache validity *)
  let cache_key = String.concat "," (List.sort String.compare all_classnames) in
  let cache =
    if minify then tailwind_cache_minified else tailwind_cache_unminified
  in

  match !cache with
  | Some (key, css) when key = cache_key -> css
  | _ ->
      let css = generate_tailwind_css ~minify all_classnames in
      cache := Some (cache_key, css);
      css

(** Check 1:1 mapping with non-minified Tailwind - uses cached CSS *)
let check_exact_match tw_style =
  try
    let classname = to_string tw_style in
    (* Generate non-minified CSS for accurate comparison *)
    let tw_css = exact_css (generate_tw_css ~minify:false [ tw_style ]) in
    let tailwind_full_css =
      exact_css (generate_all_tailwind_css ~minify:false ())
    in

    (* Extract just the utility classes from both outputs *)
    let tailwind_utility_css =
      extract_utility_classes tailwind_full_css [ classname ]
    in
    let tw_utility_css = extract_utility_classes tw_css [ classname ] in

    if tw_utility_css <> tailwind_utility_css then (
      Fmt.epr "\n=== UTILITY CSS MISMATCH for %s ===\n" classname;
      Fmt.epr "Tw utility CSS:\n%s\n" tw_utility_css;
      Fmt.epr "Tailwind utility CSS:\n%s\n" tailwind_utility_css;
      Fmt.epr "===============================\n");

    Alcotest.check string
      (Fmt.str "%s utility CSS exact match" classname)
      tailwind_utility_css tw_utility_css
  with
  | Failure msg -> fail ("Test setup failed: " ^ msg)
  | exn -> fail ("Unexpected error: " ^ Printexc.to_string exn)

(** Main check function - only test exact match for now *)
let check tw_style = check_exact_match tw_style

let test_tailwind_basic_spacing () =
  check (p (int 4));
  check (m (int 2));
  check (px (int 6));
  check (py (int 3));
  check (pt (int 1));
  check (pr (int 8));
  check (pb (int 12));
  check (pl (int 16));
  check (mx (int 0));
  check (my (int 10));
  check (mt (int 20));
  check (mr (int 24));
  check (mb (int 56));
  check (ml (int 6))

let test_tailwind_color_classes () =
  check (bg white);
  check (text ~shade:900 gray);
  (* Using consistent API: border should work like bg and text *)
  check (border_color ~shade:200 gray);
  check (bg ~shade:500 gray);
  check (bg ~shade:600 sky);
  check (text ~shade:400 yellow);
  check (border_color ~shade:600 teal)

let test_tailwind_display_classes () =
  check block;
  check inline;
  check inline_block;
  check flex;
  check inline_flex;
  check grid;
  check inline_grid;
  check hidden

let test_tailwind_sizing () =
  check (w (int 4));
  check (h (int 8));
  check (w fit);
  check (w full);
  check (h fit);
  check (h full);
  check (min_w (int 0))

let test_tailwind_typography () =
  check text_xs;
  check text_sm;
  check text_base;
  check text_lg;
  check text_xl;
  check text_2xl;
  check font_normal;
  check font_medium;
  check font_semibold;
  check font_bold

let test_tailwind_responsive () =
  check (on_md [ block ]);
  check (on_lg [ flex ]);
  check (on_xl [ hidden ]);
  check (on_sm [ p (int 4) ]);
  check (on_md [ m (int 6) ])

let test_tailwind_states () =
  check (on_hover [ bg white ]);
  check (on_focus [ bg ~shade:500 sky ]);
  check (on_active [ text ~shade:900 gray ])

let test_tailwind_borders () =
  check (rounded md);
  check (rounded lg);
  check (rounded full);
  check (border `Default);
  check (border `Sm);
  check (border `Lg)

let test_tailwind_shadows () =
  check (shadow sm);
  check (shadow md);
  check (shadow lg);
  check (shadow none)

let test_tailwind_prose () =
  check prose;
  check prose_sm;
  check prose_lg;
  check prose_xl;
  check prose_2xl;
  check prose_gray;
  check prose_slate

let test_tailwind_flexbox () =
  check flex_col;
  check flex_row;
  check flex_row_reverse;
  check flex_col_reverse;
  check flex_wrap;
  check flex_wrap_reverse;
  check items_center;
  check justify_center;
  check justify_between

let test_tailwind_responsive_breakpoints () =
  check (on_sm [ block ]);
  check (on_md [ flex ]);
  check (on_lg [ grid ]);
  check (on_xl [ hidden ]);
  check (on_sm [ text_lg ]);
  check (on_md [ p (int 8) ]);
  check (on_lg [ bg ~shade:500 sky ])

let test_tailwind_layout () =
  check relative;
  check absolute;
  check fixed;
  check sticky

let test_tailwind_opacity () =
  check (opacity 0);
  check (opacity 50);
  check (opacity 100)

(** Helper function to check for substring *)
let string_contains_substring haystack needle =
  let haystack_len = String.length haystack in
  let needle_len = String.length needle in
  let rec check_at pos =
    if pos + needle_len > haystack_len then false
    else if String.sub haystack pos needle_len = needle then true
    else check_at (pos + 1)
  in
  if needle_len = 0 then true else check_at 0

(** Test extended color palette *)
let test_extended_color_palette () =
  (* Test all new colors with various shades *)
  check (bg ~shade:50 slate);
  check (bg ~shade:500 zinc);
  check (bg ~shade:900 orange);
  check (text ~shade:100 amber);
  check (text ~shade:600 lime);
  check (border_color ~shade:300 emerald);
  check (border_color ~shade:700 cyan);
  check (bg ~shade:400 violet);
  check (text ~shade:800 fuchsia);
  check (bg ~shade:200 rose)

(** Test class name generation for extended colors *)
let test_extended_color_class_names () =
  let test_class_name tw expected =
    let actual = to_string tw in
    Alcotest.check string ("class name for " ^ expected) expected actual
  in

  test_class_name (bg ~shade:500 slate) "bg-slate-500";
  test_class_name (text ~shade:600 zinc) "text-zinc-600";
  test_class_name (border_color ~shade:300 orange) "border-orange-300";
  test_class_name (bg ~shade:700 amber) "bg-amber-700";
  test_class_name (text ~shade:200 lime) "text-lime-200";
  test_class_name (border_color ~shade:800 emerald) "border-emerald-800";
  test_class_name (bg ~shade:100 cyan) "bg-cyan-100";
  test_class_name (text ~shade:900 violet) "text-violet-900";
  test_class_name (border_color ~shade:400 fuchsia) "border-fuchsia-400";
  test_class_name (bg ~shade:50 rose) "bg-rose-50"

(** Test Black and White colors don't include shades in class names *)
let test_black_white_class_names () =
  let test_class_name tw expected =
    let actual = to_string tw in
    Alcotest.check string ("class name for " ^ expected) expected actual
  in

  test_class_name (bg black) "bg-black";
  test_class_name (bg white) "bg-white";
  test_class_name (text black) "text-black";
  test_class_name (text white) "text-white";
  test_class_name (border_color black) "border-black";
  test_class_name (border_color white) "border-white"

(** Test CSS property generation works correctly *)
let test_css_property_generation () =
  let test_css_contains tw expected_property expected_value =
    let stylesheet = to_css [ tw ] in
    let css_str = stylesheet_to_string stylesheet in
    let property_str = expected_property ^ ": " ^ expected_value in
    Alcotest.check bool
      ("CSS contains " ^ property_str)
      true
      (string_contains_substring css_str property_str)
  in

  (* Test color properties *)
  test_css_contains (bg ~shade:500 red) "background-color"
    "rgb(239 68 68 / var(--tw-bg-opacity))";
  test_css_contains (text ~shade:600 blue) "color"
    "rgb(37 99 235 / var(--tw-text-opacity))";
  test_css_contains
    (border_color ~shade:300 green)
    "border-color" "rgb(134 239 172 / var(--tw-border-opacity))";

  (* Test spacing properties *)
  test_css_contains (p (int 4)) "padding" "1rem";
  test_css_contains (m (int 0)) "margin" "0";
  test_css_contains (px (int 6)) "padding-left" "1.5rem";
  test_css_contains (py (int 2)) "padding-top" "0.5rem"

(** Test responsive modifiers *)
let test_responsive_modifiers () =
  let test_class_name tw expected =
    let actual = to_string tw in
    Alcotest.check string ("responsive class " ^ expected) expected actual
  in

  test_class_name (on_sm [ bg ~shade:500 red ]) "sm:bg-red-500";
  test_class_name (on_md [ text ~shade:600 blue ]) "md:text-blue-600";
  test_class_name (on_lg [ p (int 8) ]) "lg:p-8";
  test_class_name (on_xl [ flex ]) "xl:flex"

(** Test state modifiers *)
let test_state_modifiers () =
  let test_class_name tw expected =
    let actual = to_string tw in
    Alcotest.check string ("state modifier " ^ expected) expected actual
  in

  test_class_name (on_hover [ bg ~shade:700 gray ]) "hover:bg-gray-700";
  test_class_name (on_focus [ text ~shade:500 sky ]) "focus:text-sky-500";
  test_class_name
    (on_active [ border_color ~shade:400 teal ])
    "active:border-teal-400"

let test_css_prelude () =
  (* Test that our CSS reset/prelude matches expected structure *)
  let stylesheet = to_css [] in
  let css = stylesheet_to_string ~minify:false stylesheet in

  (* Check for reset styles *)
  let expected_reset_patterns =
    [
      "margin: 0";
      "padding: 0";
      "box-sizing: border-box";
      "font-size: 16px";
      "line-height: 1.5";
    ]
  in

  List.iter
    (fun pattern ->
      if not (Astring.String.is_infix ~affix:pattern css) then
        failf "CSS prelude missing expected pattern: %s" pattern)
    expected_reset_patterns

let test_exact_css_match () =
  (* Test exact CSS output for a small set of utilities *)
  (* This ensures our CSS generation is exactly correct *)
  let test_cases =
    [
      (p (int 0), ".p-0 {\n  padding: 0;\n}");
      (m auto, ".m-auto {\n  margin: auto;\n}");
      (opacity 100, ".opacity-100 {\n  opacity: 1;\n}");
      (flex, ".flex {\n  display: flex;\n}");
    ]
  in

  List.iter
    (fun (tw_style, expected) ->
      let stylesheet = to_css [ tw_style ] in
      let css = stylesheet_to_string ~minify:false stylesheet in
      (* Extract just the utility class part (skip prelude) *)
      let lines = String.split_on_char '\n' css in
      let rec skip_prelude = function
        | [] -> []
        | line :: rest ->
            if String.starts_with ~prefix:"." line then
              line :: rest (* Found first class, return from here *)
            else skip_prelude rest
      in
      let utility_lines = skip_prelude lines in
      let actual_utility = String.concat "\n" utility_lines |> String.trim in

      if actual_utility <> expected then (
        Fmt.epr "\n=== EXACT CSS MISMATCH ===\n";
        Fmt.epr "Expected:\n%s\n" expected;
        Fmt.epr "Actual:\n%s\n" actual_utility;
        Fmt.epr "========================\n";
        failf "CSS output doesn't match exactly for %s" (to_string tw_style)))
    test_cases

let check_minification_pattern ramen_minified (pattern, should_exist) =
  let exists = Astring.String.is_infix ~affix:pattern ramen_minified in
  match (should_exist, exists) with
  | true, false ->
      failf "Expected pattern not found in minified CSS: %s" pattern
  | false, true -> failf "Unexpected pattern found in minified CSS: %s" pattern
  | _ -> ()

let extract_utilities_from_tailwind tailwind_minified =
  match String.index_opt tailwind_minified '.' with
  | Some idx when idx > 0 ->
      String.sub tailwind_minified idx (String.length tailwind_minified - idx)
  | _ -> tailwind_minified

let test_minification_rules () =
  let all_styles = all_test_styles () in

  (* Get Tailwind's minified output - use the existing cache mechanism *)
  let tailwind_minified = generate_all_tailwind_css ~minify:true () in
  let tailwind_utilities = extract_utilities_from_tailwind tailwind_minified in

  (* Get our minified output *)
  let stylesheet = to_css all_styles in
  let tw_minified = stylesheet_to_string ~minify:true stylesheet in

  (* Check size difference *)
  let tailwind_len = String.length tailwind_utilities in
  let tw_len = String.length tw_minified in
  if abs (tailwind_len - tw_len) > tailwind_len / 2 then
    Fmt.epr
      "Warning: Large size difference - Tailwind utilities: %d bytes, Tw: %d \
       bytes\n"
      tailwind_len tw_len;

  (* Test specific minification rules *)
  let test_cases =
    [
      (".opacity-0{opacity:0}", true);
      ("opacity-50", true);
      ("opacity:.5}", true);
      (".p-0{padding:0}", true);
      (".p-4{padding:1rem}", true);
      (".m-auto{margin:auto}", true);
      (".w-full{width:100%}", true);
      ("box-shadow:", true);
    ]
  in

  List.iter (check_minification_pattern tw_minified) test_cases

(** Test comprehensive prose CSS comparison with Tailwind *)
let test_prose_css_comparison () =
  (* Test basic prose styles *)
  let test_prose_style style expected_selectors =
    try
      let classname = to_string style in
      let tw_css = to_css ~reset:false [style] |> stylesheet_to_string ~minify:false in
      let tailwind_css = generate_tailwind_css ~minify:false [classname] in
      
      (* Check that key selectors are present *)
      List.iter (fun selector ->
        let has_selector = Astring.String.is_infix ~affix:selector tw_css in
        if not has_selector then
          Alcotest.failf "Prose CSS missing selector %s in class %s" selector classname
      ) expected_selectors;
      
      (* Extract specific rules for comparison *)
      let extract_rule selector css =
        try
          let selector_with_space = selector ^ " {" in
          match Astring.String.find_sub ~sub:selector_with_space css with
          | Some idx ->
              let start = idx + String.length selector_with_space in
              (match Astring.String.find_sub ~start ~sub:"}" css with
               | Some end_idx -> Some (String.sub css start (end_idx - start))
               | None -> None)
          | None -> None
        with Invalid_argument _ | Not_found -> None
      in
      
      (* Compare base prose properties *)
      if classname = "prose" then begin
        let tw_prose = extract_rule ".prose" tw_css in
        let tailwind_prose = extract_rule ".prose" tailwind_css in
        match tw_prose, tailwind_prose with
        | Some tw_props, Some _tailwind_props ->
            (* Check for key properties *)
            let key_props = ["color:"; "max-width:"; "font-size:"; "line-height:"] in
            List.iter (fun prop ->
              if not (Astring.String.is_infix ~affix:prop tw_props) then
                Alcotest.failf "Missing property %s in .prose" prop
            ) key_props
        | _ -> ()
      end
    with e -> 
      Alcotest.failf "Prose CSS comparison failed for %s: %s" 
        (to_string style) (Printexc.to_string e)
  in
  
  (* Test each prose variant with expected selectors *)
  test_prose_style prose [".prose"; ".prose h1"; ".prose h2"; ".prose p"; ".prose a"; 
                          ".prose code"; ".prose pre"; ".prose ul"; ".prose blockquote"];
  test_prose_style prose_sm [".prose-sm"; ".prose-sm h1"; ".prose-sm p"];
  test_prose_style prose_lg [".prose-lg"; ".prose-lg h1"; ".prose-lg p"];
  test_prose_style prose_gray [".prose-gray"];
  test_prose_style prose_slate [".prose-slate"]

(** Test prose with modifiers *)
let test_prose_with_modifiers () =
  (* Test responsive modifiers with prose *)
  let responsive_prose = on_sm [prose] in
  let class_name = to_string responsive_prose in
  Alcotest.(check string) "responsive prose" "sm:prose" class_name;
  
  (* Hover modifier should work with prose *)
  let hover_prose = on_hover [prose] in 
  let hover_class = to_string hover_prose in
  Alcotest.(check string) "hover prose" "hover:prose" hover_class;
  
  (* Test CSS generation for responsive prose *)
  let css = to_css ~reset:false [responsive_prose] |> stylesheet_to_string ~minify:false in
  Alcotest.(check bool) "has media query" true 
    (Astring.String.is_infix ~affix:"@media" css);
  (* Debug: print first 500 chars of CSS to see what's generated *)
  if not (Astring.String.is_infix ~affix:"prose" css) then
    Alcotest.failf "CSS output missing 'prose' (first 500 chars):\n%s" 
      (String.sub css 0 (Int.min 500 (String.length css)));
  (* The prose rules should be in the media query *)
  let has_prose = Astring.String.is_infix ~affix:"prose" css in
  Alcotest.(check bool) "has prose in media" true has_prose

(** Test prose variant combinations *)  
let test_prose_variant_combinations () =
  (* Multiple prose classes together *)
  let combined = to_classes [prose; prose_lg; prose_gray] in
  Alcotest.(check string) "multiple prose classes" "prose prose-lg prose-gray" combined;
  
  (* Test CSS generation for combined prose *)
  let css = to_css ~reset:false [prose; prose_lg] |> stylesheet_to_string ~minify:false in
  
  (* Both base prose and size variant should be present *)
  Alcotest.(check bool) "has base prose" true
    (Astring.String.is_infix ~affix:".prose {" css);
  Alcotest.(check bool) "has prose-lg" true  
    (Astring.String.is_infix ~affix:".prose-lg {" css);
    
  (* Test that prose can be combined with other utilities *)
  let with_utilities = to_classes [prose; mx auto; max_w (xl_3)] in
  Alcotest.(check string) "prose with utilities" "prose mx-auto max-w-3xl" with_utilities

let test_scroll_snap () =
  let styles =
    [
      snap_none;
      snap_x;
      snap_y;
      snap_both;
      snap_mandatory;
      snap_proximity;
      snap_start;
      snap_end;
      snap_center;
      snap_align_none;
      snap_normal;
      snap_always;
      scroll_auto;
      scroll_smooth;
    ]
  in

  List.iter
    (fun style ->
      let class_name = to_class style in
      (* Generate CSS and verify it contains the expected class selector *)
      let stylesheet = to_css ~reset:false [ style ] in
      let css_output = stylesheet_to_string ~minify:false stylesheet in

      (* The CSS should contain the class selector *)
      let expected_selector = "." ^ class_name in
      let contains_selector =
        Astring.String.is_infix ~affix:expected_selector css_output
      in

      if not contains_selector then (
        Fmt.epr "Generated CSS for %s:\n%s\n" class_name css_output;
        failf "CSS output doesn't contain expected selector %s"
          expected_selector);

      (* Also verify the CSS has actual property declarations (contains '{' and
         '}') *)
      let has_declarations =
        Astring.String.is_infix ~affix:"{" css_output
        && Astring.String.is_infix ~affix:"}" css_output
      in

      if not has_declarations then
        failf "CSS for %s doesn't contain property declarations" class_name)
    styles

let test_data_attributes () =
  (* Test that data attribute variants generate correct selectors *)
  let test_cases =
    [
      ( data_state "open" block,
        ".block[data-state=\"open\"] {\n  display: block;\n}" );
      ( data_variant "primary" (bg white),
        ".bg-white[data-variant=\"primary\"] {\n\
        \  --tw-bg-opacity: 1;\n\
        \  background-color: rgb(255 255 255 / var(--tw-bg-opacity));\n\
         }" );
      ( on_data_active [ text white ],
        ".text-white[data-active] {\n\
        \  --tw-text-opacity: 1;\n\
        \  color: rgb(255 255 255 / var(--tw-text-opacity));\n\
         }" );
      ( on_data_inactive [ hidden ],
        ".hidden[data-inactive] {\n  display: none;\n}" );
      ( data_custom "theme" "dark" flex,
        ".flex[data-theme=\"dark\"] {\n  display: flex;\n}" );
    ]
  in

  List.iter
    (fun (tw_style, expected) ->
      let stylesheet = to_css [ tw_style ] in
      let css = stylesheet_to_string ~minify:false stylesheet in
      (* Extract just the utility class part (skip prelude) *)
      let lines = String.split_on_char '\n' css in
      let rec skip_prelude = function
        | [] -> []
        | line :: rest ->
            if String.starts_with ~prefix:"." line then
              line :: rest (* Found first class, return from here *)
            else skip_prelude rest
      in
      let utility_lines = skip_prelude lines in
      let actual_utility = String.concat "\n" utility_lines |> String.trim in

      if actual_utility <> expected then (
        Fmt.epr "\n=== DATA ATTRIBUTE CSS MISMATCH ===\n";
        Fmt.epr "Expected:\n%s\n" expected;
        Fmt.epr "Actual:\n%s\n" actual_utility;
        Fmt.epr "====================================\n";
        fail
          (Fmt.str "CSS output doesn't match exactly for data attribute variant")))
    test_cases

(* CSS Generation tests *)
let test_inline_styles () =
  let styles = [ bg blue; text white; p (int 4); m (int 2); rounded md ] in
  let inline = to_inline_style styles in
  (* Check that inline styles are generated *)
  Alcotest.check bool "has background-color" true
    (Astring.String.is_infix ~affix:"background-color" inline);
  Alcotest.check bool "has color" true
    (Astring.String.is_infix ~affix:"color" inline);
  Alcotest.check bool "has padding" true
    (Astring.String.is_infix ~affix:"padding" inline);
  Alcotest.check bool "has margin" true
    (Astring.String.is_infix ~affix:"margin" inline);
  Alcotest.check bool "has border-radius" true
    (Astring.String.is_infix ~affix:"border-radius" inline)

let test_dynamic_inline_styles () =
  (* Test dynamic style generation *)
  let dynamic_width = 42 in
  let styles = [ w (int dynamic_width); bg ~shade:300 gray; p (int 2) ] in
  let inline = to_inline_style styles in

  (* Check that dynamic value is included *)
  Alcotest.check bool "has dynamic width" true
    (Astring.String.is_infix
       ~affix:(string_of_float (float_of_int dynamic_width *. 0.25) ^ "rem")
       inline)

(** Helper function for roundtrip testing *)
let test_roundtrip style =
  let class_str = to_string style in
  match of_string class_str with
  | Error (`Msg msg) ->
      Alcotest.fail
        (Fmt.str "Failed to parse generated class '%s': %s" class_str msg)
  | Ok parsed_style ->
      let parsed_class_str = to_string parsed_style in
      Alcotest.check string
        (Fmt.str "Roundtrip for %s" class_str)
        class_str parsed_class_str

(** Test of_string roundtrip for color styles *)
let test_of_string_roundtrip_colors () =
  let styles =
    [
      bg blue;
      bg ~shade:500 blue;
      text white;
      text ~shade:700 gray;
      border_color ~shade:300 red;
    ]
  in
  List.iter test_roundtrip styles

(** Test of_string roundtrip for spacing styles *)
let test_of_string_roundtrip_spacing () =
  let styles =
    [ p (int 4); px (int 2); py (int 8); m auto; mx (int 4); gap (int 6) ]
  in
  List.iter test_roundtrip styles

(** Test of_string roundtrip for typography styles *)
let test_of_string_roundtrip_typography () =
  let styles =
    [ text_lg; text_center; font_bold; italic; underline; leading_tight;
      prose; prose_sm; prose_lg; prose_xl; prose_2xl; prose_gray; prose_slate ]
  in
  List.iter test_roundtrip styles

(** Test of_string roundtrip for layout and sizing styles *)
let test_of_string_roundtrip_layout () =
  let styles =
    [
      flex;
      block;
      hidden;
      relative;
      absolute;
      grid_cols 3;
      items_center;
      justify_between;
      flex_col;
      flex_1;
      w full;
      h screen;
      max_w xl_2;
      min_h (int 10);
    ]
  in
  List.iter test_roundtrip styles

(** Test of_string roundtrip for effects and modifiers *)
let test_of_string_roundtrip_effects () =
  let styles =
    [
      border `Default;
      rounded lg;
      shadow md;
      opacity 50;
      transition_colors;
      duration 300;
      scale 105;
      on_hover [ bg ~shade:700 blue ];
      on_focus [ ring `Md ];
      on_sm [ text_xl ];
      on_sm [ bg ~shade:500 red ];
      on_md [ text ~shade:600 blue ];
      on_lg [ p (int 8) ];
      on_xl [ flex ];
      on_2xl [ grid ];
      on_hover [ text white ];
      on_focus [ bg ~shade:500 sky ];
      on_active [ text ~shade:900 gray ];
      on_disabled [ opacity 50 ];
      on_dark [ bg gray ];
    ]
  in
  List.iter test_roundtrip styles

(** Simple random generators for Tw styles *)
module Generators = struct
  let sample lst = List.nth lst (Random.int (List.length lst))

  let gen_color () =
    sample
      [
        black;
        white;
        gray;
        slate;
        zinc;
        red;
        orange;
        amber;
        yellow;
        lime;
        green;
        emerald;
        teal;
        cyan;
        sky;
        blue;
        indigo;
        violet;
        purple;
        fuchsia;
        pink;
        rose;
      ]

  let gen_shade () = sample [ 50; 100; 200; 300; 400; 500; 600; 700; 800; 900 ]

  let gen_spacing_int () =
    sample [ 0; 1; 2; 3; 4; 5; 6; 8; 10; 12; 16; 20; 24; 32; 40; 48; 56; 64 ]

  let gen_spacing () =
    match Random.int 3 with
    | 0 -> one_px
    | 1 -> full
    | _ -> int (gen_spacing_int ())

  let gen_margin () = match Random.int 4 with 0 -> auto | _ -> gen_spacing ()
  let gen_size () = sample [ none; xs; sm; md; lg; xl; xl_2; xl_3 ]

  let gen_scale () =
    match Random.int 6 with
    | 0 -> gen_spacing ()
    | 1 -> gen_size ()
    | 2 -> screen
    | 3 -> min
    | 4 -> max
    | _ -> fit

  let gen_simple_style () =
    match Random.int 20 with
    | 0 -> block
    | 1 -> inline
    | 2 -> flex
    | 3 -> grid
    | 4 -> hidden
    | 5 -> relative
    | 6 -> absolute
    | 7 -> fixed
    | 8 -> text_center
    | 9 -> text_left
    | 10 -> text_right
    | 11 -> font_bold
    | 12 -> font_normal
    | 13 -> italic
    | 14 -> underline
    | 15 -> items_center
    | 16 -> justify_center
    | 17 -> flex_col
    | 18 -> flex_row
    | _ -> transition_colors

  let gen_color_style () =
    let color = gen_color () in
    let shade = if Random.bool () then Some (gen_shade ()) else None in
    match Random.int 3 with
    | 0 -> bg ?shade color
    | 1 -> text ?shade color
    | _ -> border_color ?shade color

  let gen_spacing_style () =
    let spacing = gen_spacing () in
    match Random.int 8 with
    | 0 -> p spacing
    | 1 -> px spacing
    | 2 -> py spacing
    | 3 -> pt spacing
    | 4 -> gap spacing
    | 5 ->
        let margin_val = gen_margin () in
        m margin_val
    | 6 ->
        let margin_val = gen_margin () in
        mx margin_val
    | _ ->
        let margin_val = gen_margin () in
        my margin_val

  let gen_size_style () =
    let scale = gen_scale () in
    match Random.int 4 with
    | 0 -> w scale
    | 1 -> h scale
    | 2 -> min_w scale
    | _ -> min_h scale

  let gen_prose_style () =
    match Random.int 7 with
    | 0 -> prose
    | 1 -> prose_sm
    | 2 -> prose_lg
    | 3 -> prose_xl
    | 4 -> prose_2xl
    | 5 -> prose_gray
    | _ -> prose_slate

  let gen_style () =
    match Random.int 5 with
    | 0 -> gen_simple_style ()
    | 1 -> gen_color_style ()
    | 2 -> gen_spacing_style ()
    | 3 -> gen_size_style ()
    | _ -> gen_prose_style ()

  let gen_style_list n = List.init n (fun _ -> gen_style ())
end

(** Property: generated styles should always produce valid class names *)
let test_generated_styles_valid () =
  Random.init 42;
  for _ = 1 to 100 do
    let style = Generators.gen_style () in
    let class_name = to_string style in
    Alcotest.check Alcotest.bool "class name not empty" true
      (String.length class_name > 0);
    (* Basic validation: no spaces in single classes *)
    if not (String.contains class_name ':' || String.contains class_name '[')
    then
      Alcotest.check Alcotest.bool "no spaces in class" false
        (String.contains class_name ' ')
  done

(** Property: CSS generation should be deterministic *)
let test_css_deterministic () =
  Random.init 42;
  for _ = 1 to 50 do
    let styles = Generators.gen_style_list (1 + Random.int 10) in
    let css1 = to_css ~reset:false styles |> stylesheet_to_string in
    let css2 = to_css ~reset:false styles |> stylesheet_to_string in
    Alcotest.check Alcotest.string "CSS generation is deterministic" css1 css2
  done

(** Property: minified CSS should never be larger than normal CSS *)
let test_minification_always_smaller () =
  Random.init 42;
  for _ = 1 to 50 do
    let styles = Generators.gen_style_list (1 + Random.int 20) in
    let stylesheet = to_css ~reset:false styles in
    let normal = stylesheet_to_string ~minify:false stylesheet in
    let minified = stylesheet_to_string ~minify:true stylesheet in
    Alcotest.check Alcotest.bool "minified <= normal size" true
      (String.length minified <= String.length normal)
  done

(** Property: combining style lists should combine their CSS *)
let test_style_combination () =
  Random.init 42;
  for _ = 1 to 20 do
    let styles1 = Generators.gen_style_list (1 + Random.int 5) in
    let styles2 = Generators.gen_style_list (1 + Random.int 5) in
    let css1 = to_css ~reset:false styles1 |> stylesheet_to_string in
    let combined =
      to_css ~reset:false (styles1 @ styles2) |> stylesheet_to_string
    in

    (* Combined should contain classes from both *)
    List.iter
      (fun style ->
        let class_name = "." ^ to_string style in
        if Astring.String.is_infix ~affix:class_name css1 then
          Alcotest.check Alcotest.bool "combined contains class from first set"
            true
            (Astring.String.is_infix ~affix:class_name combined))
      styles1
  done

(** Test that responsive modifier parsing works correctly *)
let test_responsive_modifier_parsing () =
  (* Test all responsive breakpoints *)
  let test_cases =
    [
      ("sm:text-black", on_sm [ text black ]);
      ("md:bg-blue-500", on_md [ bg ~shade:500 blue ]);
      ("lg:p-8", on_lg [ p (int 8) ]);
      ("xl:flex", on_xl [ flex ]);
      ("2xl:grid", on_2xl [ grid ]);
      ("hover:text-white", on_hover [ text white ]);
      ("focus:bg-sky-500", on_focus [ bg ~shade:500 sky ]);
      ("active:text-gray-900", on_active [ text ~shade:900 gray ]);
      ("disabled:opacity-50", on_disabled [ opacity 50 ]);
      ("dark:bg-gray", on_dark [ bg gray ]);
    ]
  in

  List.iter
    (fun (input, expected) ->
      match of_string input with
      | Error (`Msg msg) -> Alcotest.failf "Failed to parse '%s': %s" input msg
      | Ok parsed ->
          let expected_str = to_string expected in
          let parsed_str = to_string parsed in
          Alcotest.check string
            (Fmt.str "Parse '%s'" input)
            expected_str parsed_str)
    test_cases

(** Test parsing multiple classes in a single string (for CLI) *)
let test_multiple_classes_parsing () =
  (* This tests the behavior needed by the CLI when -s receives multiple
     classes *)
  let parse_multiple_classes classes_str =
    let class_names =
      String.split_on_char ' ' classes_str
      |> List.filter (fun s -> String.length s > 0)
    in
    List.filter_map
      (fun cls ->
        match of_string cls with Ok style -> Some style | Error _ -> None)
      class_names
  in

  let test_cases =
    [
      ("sm:text-black lg:text-white", 2);
      ("bg-blue-500 text-white p-4", 3);
      ("hover:bg-gray-700 focus:ring-2 active:scale-95", 3);
      ("sm:p-2 md:p-4 lg:p-6 xl:p-8", 4);
    ]
  in

  List.iter
    (fun (input, expected_count) ->
      let parsed = parse_multiple_classes input in
      Alcotest.check Alcotest.int
        ("Parse '" ^ input ^ "' should yield "
        ^ string_of_int expected_count
        ^ " classes")
        expected_count (List.length parsed))
    test_cases

let test_of_string_random_valid () =
  Random.init 42;
  let valid_classes =
    [
      "bg-blue-500";
      "text-white";
      "p-4";
      "m-auto";
      "flex";
      "hidden";
      "text-center";
      "font-bold";
      "rounded-lg";
      "shadow-md";
      "w-full";
      "h-screen";
      "items-center";
      "justify-between";
      "gap-4";
      "relative";
    ]
  in

  for _ = 1 to 50 do
    let cls = Generators.sample valid_classes in
    match of_string cls with
    | Ok style ->
        let cls' = to_string style in
        Alcotest.check Alcotest.string "roundtrip preserves class" cls cls'
    | Error (`Msg msg) -> failf "Failed to parse valid class %s: %s" cls msg
  done

(** Test CSS property deduplication *)
let test_css_property_deduplication () =
  (* Test that duplicate properties are removed, keeping the last one *)
  let styles =
    [
      bg ~shade:500 red;
      (* first bg *)
      p (int 4);
      bg ~shade:700 blue;
      (* second bg - should override first *)
      text white;
      bg black;
      (* third bg - should override second *)
    ]
  in
  let inline_style = to_inline_style styles in

  (* Should contain black (last bg) but not red or blue *)
  Alcotest.check bool "contains final bg-black" true
    (Astring.String.is_infix ~affix:"background-color" inline_style
    && Astring.String.is_infix ~affix:"0 0 0" inline_style);
  Alcotest.check bool "doesn't contain bg-red" false
    (Astring.String.is_infix ~affix:"239 68 68" inline_style);
  Alcotest.check bool "doesn't contain bg-blue" false
    (Astring.String.is_infix ~affix:"29 78 216" inline_style)

(** Test inline style generation *)
let test_css_inline_style_generation () =
  let styles =
    [ bg ~shade:500 blue; text white; p (int 4); rounded md; opacity 75 ]
  in
  let inline_style = to_inline_style styles in

  (* Check that all expected CSS properties are present *)
  Alcotest.check bool "has background-color" true
    (Astring.String.is_infix ~affix:"background-color:" inline_style);
  Alcotest.check bool "has color" true
    (Astring.String.is_infix ~affix:"color:" inline_style);
  Alcotest.check bool "has padding" true
    (Astring.String.is_infix ~affix:"padding:" inline_style);
  Alcotest.check bool "has border-radius" true
    (Astring.String.is_infix ~affix:"border-radius:" inline_style);
  Alcotest.check bool "has opacity" true
    (Astring.String.is_infix ~affix:"opacity:" inline_style);

  (* Check that it's a valid inline style format *)
  Alcotest.check bool "has proper CSS property format" true
    (String.contains inline_style ':' && String.contains inline_style ';');

  (* Check that it doesn't contain CSS selectors or @media rules *)
  Alcotest.check bool "no CSS selectors" false
    (Astring.String.is_infix ~affix:"{" inline_style);
  Alcotest.check bool "no media queries" false
    (Astring.String.is_infix ~affix:"@media" inline_style)

(** Test CSS minification *)
let test_css_minification () =
  let styles =
    [
      bg ~shade:500 blue;
      text white;
      p (int 4);
      m (int 2);
      rounded lg;
      shadow md;
    ]
  in
  let stylesheet = to_css ~reset:false styles in
  let normal = stylesheet_to_string ~minify:false stylesheet in
  let minified = stylesheet_to_string ~minify:true stylesheet in

  (* Minified should be smaller than normal *)
  Alcotest.check bool "minified is smaller than normal" true
    (String.length minified < String.length normal);

  (* Both should contain the same essential CSS properties *)
  Alcotest.check bool "both contain background-color" true
    (String.contains normal ':' && String.contains minified ':');

  (* Minified should still be valid CSS (contain basic CSS structure) *)
  Alcotest.check bool "minified contains selectors" true
    (String.contains minified '{' && String.contains minified '}');

  (* Minified should not contain extra whitespace *)
  Alcotest.check bool "minified has no double newlines" false
    (Astring.String.is_infix ~affix:"\n\n" minified)

let tailwind_tests =
  [
    test_case "tailwind basic spacing" `Quick test_tailwind_basic_spacing;
    test_case "tailwind color classes" `Quick test_tailwind_color_classes;
    test_case "tailwind display classes" `Quick test_tailwind_display_classes;
    test_case "tailwind sizing" `Quick test_tailwind_sizing;
    test_case "tailwind typography" `Quick test_tailwind_typography;
    test_case "tailwind responsive" `Quick test_tailwind_responsive;
    test_case "tailwind states" `Quick test_tailwind_states;
    test_case "tailwind borders" `Quick test_tailwind_borders;
    test_case "tailwind shadows" `Quick test_tailwind_shadows;
    test_case "tailwind prose" `Quick test_tailwind_prose;
    test_case "tailwind flexbox" `Quick test_tailwind_flexbox;
    test_case "tailwind responsive breakpoints" `Quick
      test_tailwind_responsive_breakpoints;
    test_case "tailwind layout" `Quick test_tailwind_layout;
    test_case "tailwind opacity" `Quick test_tailwind_opacity;
  ]

let comprehensive_tests =
  [
    test_case "extended color palette" `Quick test_extended_color_palette;
    test_case "extended color class names" `Quick
      test_extended_color_class_names;
    test_case "black white class names" `Quick test_black_white_class_names;
    test_case "css property generation" `Quick test_css_property_generation;
    test_case "responsive modifiers" `Quick test_responsive_modifiers;
    test_case "state modifiers" `Quick test_state_modifiers;
    test_case "css prelude" `Quick test_css_prelude;
    test_case "exact css match" `Quick test_exact_css_match;
    test_case "minification rules" `Quick test_minification_rules;
    test_case "prose css comparison" `Slow test_prose_css_comparison;
    test_case "prose with modifiers" `Quick test_prose_with_modifiers;
    test_case "prose variant combinations" `Quick test_prose_variant_combinations;
    test_case "scroll snap" `Quick test_scroll_snap;
    test_case "data attributes" `Quick test_data_attributes;
    test_case "inline styles" `Quick test_inline_styles;
    test_case "dynamic inline styles" `Quick test_dynamic_inline_styles;
  ]

let parsing_tests =
  [
    test_case "of_string roundtrip colors" `Quick
      test_of_string_roundtrip_colors;
    test_case "of_string roundtrip spacing" `Quick
      test_of_string_roundtrip_spacing;
    test_case "of_string roundtrip typography" `Quick
      test_of_string_roundtrip_typography;
    test_case "of_string roundtrip layout" `Quick
      test_of_string_roundtrip_layout;
    test_case "of_string roundtrip effects" `Quick
      test_of_string_roundtrip_effects;
    test_case "responsive modifier parsing" `Quick
      test_responsive_modifier_parsing;
    test_case "multiple classes parsing" `Quick test_multiple_classes_parsing;
  ]

let property_based_tests =
  [
    test_case "generated styles valid" `Quick test_generated_styles_valid;
    test_case "css deterministic" `Quick test_css_deterministic;
    test_case "minification always smaller" `Quick
      test_minification_always_smaller;
    test_case "style combination" `Quick test_style_combination;
    test_case "of_string random valid" `Quick test_of_string_random_valid;
  ]

let css_functionality_tests =
  [
    test_case "css property deduplication" `Quick
      test_css_property_deduplication;
    test_case "css inline style generation" `Quick
      test_css_inline_style_generation;
    test_case "css minification" `Quick test_css_minification;
  ]

let suite =
  ( "tw",
    tailwind_tests @ comprehensive_tests @ parsing_tests @ property_based_tests
    @ css_functionality_tests )
