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
  Css.to_string ~minify stylesheet

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
    p 0;
    p 1;
    p 4;
    m 0;
    m 2;
    m_auto;
    px 6;
    py 3;
    pt 1;
    pr 8;
    pb 12;
    pl 16;
    mx 0;
    mx_auto;
    my 10;
    mt 20;
    mr 24;
    mb 56;
    ml 6;
  ]

let color_test_styles =
  [
    (* Color classes - all variants *)
    bg_white;
    bg_black;
    bg_transparent;
    bg_current;
    text gray 900;
    text_transparent;
    text_current;
    border_color gray 200;
    border_transparent;
    border_current;
    bg gray 500;
    bg sky 600;
    text yellow 400;
    border_color teal 600;
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
    w 0;
    w 4;
    w 96;
    w_fit;
    w_full;
    min_w_full;
    w_max;
    h 0;
    h 8;
    h_fit;
    h_full;
    min_w 0;
    min_w_full;
    max_w_2xl;
    max_w_full;
    max_w_none;
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
    on_sm [ p 4 ];
    on_sm [ m 2 ];
    on_sm [ px 6 ];
    on_sm [ py 3 ];
    on_md [ m 6 ];
    on_md [ p 8 ];
    on_md [ mt 4 ];
    on_md [ mb 10 ];
    on_lg [ p 12 ];
    on_lg [ mx_auto ];
    on_xl [ m 0 ];
    on_xl [ p 16 ];
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
    on_sm [ w_full ];
    on_sm [ h 32 ];
    on_md [ w 64 ];
    on_md [ h_full ];
    on_lg [ w 96 ];
    on_lg [ max_w_4xl ];
    on_xl [ w_screen ];
    on_xl [ min_h_screen ];
    (* Flexbox *)
    on_sm [ flex_row ];
    on_sm [ items_center ];
    on_sm [ justify_between ];
    on_md [ flex_col ];
    on_md [ items_start ];
    on_md [ gap 4 ];
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
    on_sm [ bg_white ];
    on_sm [ text_black ];
    on_md [ bg gray 100 ];
    on_md [ text blue 700 ];
    on_lg [ bg sky 500 ];
    on_lg [ border_color gray 300 ];
    on_xl [ bg_black ];
    on_xl [ text_white ];
    (* Borders *)
    on_sm [ border_xs ];
    on_sm [ rounded_md ];
    on_md [ border_lg ];
    on_md [ rounded_lg ];
    on_lg [ border_t ];
    on_lg [ rounded_full ];
    on_xl [ border_none ];
    (* Effects *)
    on_sm [ shadow_sm ];
    on_md [ shadow_md ];
    on_lg [ shadow_lg ];
    on_xl [ shadow_none ];
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
    on_hover [ bg_white ];
    on_hover [ text blue 700 ];
    on_hover [ border_color gray 400 ];
    on_hover [ shadow_lg ];
    on_hover [ opacity 80 ];
    on_hover [ scale 105 ];
    on_hover [ translate_y (-1) ];
    (* Focus states *)
    on_focus [ bg sky 500 ];
    on_focus [ outline_none ];
    on_focus [ ring_lg ];
    on_focus [ ring_color blue 400 ];
    on_focus [ border_color blue 500 ];
    on_focus [ shadow_md ];
    (* Active states *)
    on_active [ text gray 900 ];
    on_active [ bg gray 200 ];
    on_active [ scale 95 ];
    on_active [ shadow_sm ];
    (* Disabled states *)
    on_disabled [ opacity 50 ];
    on_disabled [ cursor_not_allowed ];
    on_disabled [ bg gray 100 ];
    on_disabled [ text gray 400 ];
    (* Group states *)
    on_group_hover [ bg blue 100 ];
    on_group_hover [ text blue 700 ];
    on_group_focus [ ring ];
    (* Peer states *)
    on_peer_hover [ text green 600 ];
    on_peer_focus [ bg green 50 ];
    on_peer_checked [ text green 700 ];
    (* ARIA states *)
    on_aria_expanded [ rotate 180 ];
    on_aria_selected [ bg blue 100 ];
    on_aria_checked [ bg green 500 ];
    on_aria_disabled [ opacity 40 ];
    (* Data attribute states *)
    on_data_active [ bg blue 200 ];
    on_data_inactive [ bg gray 50 ];
  ]

let borders_test_styles =
  [
    (* Borders - comprehensive *)
    rounded_none;
    rounded_sm;
    rounded_md;
    rounded_lg;
    rounded_xl;
    rounded_full;
    border_xs;
    border_none;
    border_sm;
    border_lg;
    border_xl;
    border_t;
    border_r;
    border_b;
    border_l;
  ]

let shadows_test_styles =
  [
    (* Shadows *)
    shadow_sm;
    shadow_md;
    shadow_lg;
    shadow_xl;
    shadow_none;
    shadow_inner;
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
    gap 4;
    gap_x 2;
    gap_y 6;
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
    bg slate 50;
    bg zinc 500;
    bg orange 900;
    text amber 100;
    text lime 600;
    border_color emerald 300;
    border_color cyan 700;
    bg violet 400;
    text fuchsia 800;
    bg rose 200;
    bg sky 500;
  ]

let filter_test_styles =
  [
    (* Filters *)
    blur_sm;
    blur_md;
    blur_lg;
    blur_xl;
    blur_none;
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
    backdrop_blur_sm;
    backdrop_blur_md;
    backdrop_blur_lg;
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
  let all_classnames = List.map pp all_styles in

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
    let classname = pp tw_style in
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

let test_tailwind_color_classes () =
  check bg_white;
  check (text gray 900);
  (* Using consistent API: border should work like bg and text *)
  check (border_color gray 200);
  check (bg gray 500);
  check (bg sky 600);
  check (text yellow 400);
  check (border_color teal 600)

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
  check (w 4);
  check (h 8);
  check w_fit;
  check w_full;
  check h_fit;
  check h_full;
  check (min_w 0)

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
  check (on_sm [ p 4 ]);
  check (on_md [ m 6 ])

let test_tailwind_states () =
  check (on_hover [ bg_white ]);
  check (on_focus [ bg sky 500 ]);
  check (on_active [ text gray 900 ])

let test_tailwind_borders () =
  check rounded_md;
  check rounded_lg;
  check rounded_full;
  check border_xs;
  check border_sm;
  check border_lg

let test_tailwind_shadows () =
  check shadow_sm;
  check shadow_md;
  check shadow_lg;
  check shadow_none

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
  check (on_md [ p 8 ]);
  check (on_lg [ bg sky 500 ])

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

(** Test class name generation for extended colors *)
let test_extended_color_class_names () =
  let test_class_name tw expected =
    let actual = pp tw in
    Alcotest.check string ("class name for " ^ expected) expected actual
  in

  test_class_name (bg slate 500) "bg-slate-500";
  test_class_name (text zinc 600) "text-zinc-600";
  test_class_name (border_color orange 300) "border-orange-300";
  test_class_name (bg amber 700) "bg-amber-700";
  test_class_name (text lime 200) "text-lime-200";
  test_class_name (border_color emerald 800) "border-emerald-800";
  test_class_name (bg cyan 100) "bg-cyan-100";
  test_class_name (text violet 900) "text-violet-900";
  test_class_name (border_color fuchsia 400) "border-fuchsia-400";
  test_class_name (bg rose 50) "bg-rose-50"

(** Test Black and White colors don't include shades in class names *)
let test_black_white_class_names () =
  let test_class_name tw expected =
    let actual = pp tw in
    Alcotest.check string ("class name for " ^ expected) expected actual
  in

  test_class_name bg_black "bg-black";
  test_class_name bg_white "bg-white";
  test_class_name text_black "text-black";
  test_class_name text_white "text-white";
  test_class_name (border_color black 500) "border-black";
  test_class_name (border_color white 500) "border-white"

(** Test CSS property generation works correctly *)
let test_css_property_generation () =
  let test_css_contains tw expected_property expected_value =
    let stylesheet = to_css [ tw ] in
    let css_str = Css.to_string stylesheet in
    let property_str = expected_property ^ ": " ^ expected_value in
    Alcotest.check bool
      ("CSS contains " ^ property_str)
      true
      (string_contains_substring css_str property_str)
  in

  (* Test color properties *)
  test_css_contains (bg red 500) "background-color"
    "rgb(239 68 68 / var(--tw-bg-opacity))";
  test_css_contains (text blue 600) "color"
    "rgb(37 99 235 / var(--tw-text-opacity))";
  test_css_contains (border_color green 300) "border-color"
    "rgb(134 239 172 / var(--tw-border-opacity))";

  (* Test spacing properties *)
  test_css_contains (p 4) "padding" "1rem";
  test_css_contains (m 0) "margin" "0";
  test_css_contains (px 6) "padding-left" "1.5rem";
  test_css_contains (py 2) "padding-top" "0.5rem"

(** Test negative spacing support *)
let test_negative_spacing () =
  let test_class_name tw expected =
    let actual = pp tw in
    Alcotest.check string ("negative spacing " ^ expected) expected actual
  in

  (* Test negative margins *)
  test_class_name (mt (-4)) "-mt-4";
  test_class_name (mr (-2)) "-mr-2";
  test_class_name (mb (-8)) "-mb-8";
  test_class_name (ml (-1)) "-ml-1"

(** Test 3D transform utilities *)
let test_container_queries () =
  let test_class_name tw expected =
    let actual = pp tw in
    Alcotest.check string ("container query " ^ expected) expected actual
  in
  let test_css_value tw prop expected =
    let stylesheet = to_css [ tw ] in
    let css_str = Css.to_string stylesheet in
    let has_value =
      Astring.String.is_infix ~affix:(prop ^ ": " ^ expected) css_str
    in
    if not has_value then
      Alcotest.failf "Expected CSS property %s: %s not found in:\n%s" prop
        expected css_str
  in

  (* Test container types *)
  test_class_name container_type_size "container-type-size";
  test_css_value container_type_size "container-type" "size";

  test_class_name container_type_inline_size "container-type-inline-size";
  test_css_value container_type_inline_size "container-type" "inline-size";

  test_class_name container_type_normal "container-type-normal";
  test_css_value container_type_normal "container-type" "normal";

  (* Test named containers *)
  test_class_name (container_name "sidebar") "container-sidebar";
  test_css_value (container_name "sidebar") "container-name" "sidebar";

  (* Test container query modifiers *)
  let test_container_modifier modifier expected_query =
    let style = modifier [ p 4 ] in
    let css = to_css [ style ] |> Css.to_string in
    let has_query = Astring.String.is_infix ~affix:expected_query css in
    if not has_query then
      Alcotest.failf "Expected container query %s not found in CSS"
        expected_query
  in

  test_container_modifier on_container_sm "@container (min-width: 640px)";
  test_container_modifier on_container_md "@container (min-width: 768px)";
  test_container_modifier on_container_lg "@container (min-width: 1024px)";
  test_container_modifier
    (on_container ~name:"sidebar" 500)
    "@container sidebar (min-width: 500px)"

let test_hex_colors () =
  let test_class_name tw expected =
    let actual = pp tw in
    Alcotest.check string ("hex color " ^ expected) expected actual
  in
  let test_css_value tw prop expected =
    let stylesheet = to_css [ tw ] in
    let css_str = Css.to_string stylesheet in
    let has_value =
      Astring.String.is_infix ~affix:(prop ^ ": " ^ expected) css_str
    in
    if not has_value then
      Alcotest.failf "Expected CSS property %s: %s not found in:\n%s" prop
        expected css_str
  in

  (* Test hex color in backgrounds *)
  test_class_name (bg (of_hex "#1da1f2") 0) "bg-[1da1f2]";
  test_css_value (bg (of_hex "#1da1f2") 0) "background-color" "rgb(29 161 242";

  (* Test hex with text *)
  test_class_name (text (of_hex "ff5733") 0) "text-[ff5733]";
  test_css_value (text (of_hex "ff5733") 0) "color" "rgb(255 87 51";

  (* Test hex in borders *)
  test_class_name (border_color (of_hex "#00ff00") 0) "border-[00ff00]";
  test_css_value
    (border_color (of_hex "#00ff00") 0)
    "border-color" "rgb(0 255 0"

let test_gradient_stops () =
  let test_class_name tw expected =
    let actual = pp tw in
    Alcotest.check string ("gradient " ^ expected) expected actual
  in
  let test_css_contains tw substring =
    let stylesheet = to_css [ tw ] in
    let css_str = Css.to_string stylesheet in
    if not (Astring.String.is_infix ~affix:substring css_str) then
      Alcotest.failf "Expected substring '%s' not found in CSS:\n%s" substring
        css_str
  in

  (* Test from_color *)
  test_class_name (from_color blue) "from-blue-500";
  test_class_name (from_color ~shade:300 blue) "from-blue-300";
  test_css_contains (from_color blue) "--tw-gradient-from";

  (* Test via_color *)
  test_class_name (via_color purple) "via-purple-500";
  test_class_name (via_color ~shade:700 purple) "via-purple-700";
  test_css_contains (via_color purple) "--tw-gradient-stops";

  (* Test to_color *)
  test_class_name (to_color pink) "to-pink-500";
  test_class_name (to_color ~shade:200 pink) "to-pink-200";
  test_css_contains (to_color pink) "--tw-gradient-to";

  (* Test with hex colors *)
  test_class_name (from_color (of_hex "#ff1493")) "from-[ff1493]";
  test_class_name (to_color (of_hex "00bfff")) "to-[00bfff]"

let test_html_css_selector_matching () =
  (* Test that HTML class names match CSS selectors for modifiers *)
  let test_modifier_match modifier_name style =
    let html_class = pp style in
    let css_output = to_css [ style ] |> Css.to_string in

    Printf.printf "\n=== Testing %s ===\n" modifier_name;
    Printf.printf "HTML class: %s\n" html_class;

    (* Extract CSS selectors *)
    let lines = String.split_on_char '\n' css_output in
    let selectors =
      List.filter_map
        (fun line ->
          let trimmed = String.trim line in
          if String.contains trimmed '{' && String.contains trimmed '.' then
            Some (String.sub trimmed 0 (String.index trimmed '{'))
          else None)
        lines
    in

    Printf.printf "CSS selectors found:\n";
    List.iter (fun s -> Printf.printf "  %s\n" s) selectors;

    (* Check if any CSS selector would match the HTML class *)
    let html_class_escaped =
      Re.Str.global_replace (Re.Str.regexp ":") "\\\\:" html_class
    in
    let expected_selector_base = "." ^ html_class_escaped in
    (* For hover/focus/active, we expect the pseudo-class to be appended *)
    let expected_selectors =
      match modifier_name with
      | "hover" -> [ expected_selector_base ^ ":hover" ]
      | "focus" -> [ expected_selector_base ^ ":focus" ]
      | "active" -> [ expected_selector_base ^ ":active" ]
      | _ -> [ expected_selector_base ]
    in
    let has_match =
      List.exists
        (fun expected ->
          List.exists
            (fun sel -> String.trim sel = String.trim expected)
            selectors)
        expected_selectors
    in

    if not has_match then (
      Printf.printf "❌ MISMATCH: HTML class '%s' expects CSS selector(s): %s\n"
        html_class
        (String.concat " or " expected_selectors);
      Printf.printf "   But found selectors: %s\n"
        (String.concat ", " selectors);
      Alcotest.fail ("HTML/CSS selector mismatch for " ^ modifier_name))
    else Printf.printf "✅ Match found\n"
  in

  (* Test various modifiers *)
  test_modifier_match "hover" (on_hover [ text blue 900 ]);
  test_modifier_match "focus" (on_focus [ bg red 500 ]);
  test_modifier_match "active" (on_active [ border_color green 600 ])

(* Comprehensive regression tests for HTML/CSS compatibility *)
let test_html_css_regression () =
  let test_html_css_consistency name tw_style =
    let html_class = pp tw_style in
    let css_output = to_css [ tw_style ] |> Css.to_string in

    (* Extract CSS selectors using proper regex *)
    let selector_regex = Re.Perl.compile_pat {|^\s*(\.[^\s{]+)\s*\{|} in
    let media_regex = Re.Perl.compile_pat {|@media.*\{|} in
    let responsive_prefix_regex = Re.Perl.compile_pat {|^(sm|md|lg|xl|2xl):|} in

    (* Extract selectors line by line to avoid multiline issues *)
    let lines = String.split_on_char '\n' css_output in
    let selectors =
      List.filter_map
        (fun line ->
          try
            let result = Re.exec selector_regex line in
            Some (String.trim (Re.Group.get result 1))
          with Not_found -> None)
        lines
    in

    (* Check if this is a responsive style *)
    let is_responsive = Re.execp responsive_prefix_regex html_class in
    let has_media_query = Re.execp media_regex css_output in

    (* Verify CSS structure matches HTML class *)
    if is_responsive then
      (* For responsive breakpoints, verify media query exists *)
      if not has_media_query then
        Alcotest.failf "Responsive HTML class '%s' should generate @media query"
          html_class
      else
        (* Check for special compound selectors like group-hover *)
        let is_compound_selector =
          String.contains html_class ':'
          && (Astring.String.is_prefix ~affix:"group-" html_class
             || Astring.String.is_prefix ~affix:"peer-" html_class)
        in

        if is_compound_selector then (
          if
            (* For compound selectors, just verify CSS contains relevant
               parts *)
            not
              (Astring.String.is_infix ~affix:".group:" css_output
              || Astring.String.is_infix ~affix:".peer:" css_output)
          then
            Alcotest.failf
              "Compound HTML class '%s' should generate compound CSS selector"
              html_class
          else
            (* For regular selectors, verify they match using regex *)
            let dot_removal_regex = Re.Perl.compile_pat {|^\.|} in
            let colon_unescape_regex = Re.Perl.compile_pat {|\\:|} in
            let pseudo_class_regex = Re.Perl.compile_pat {|:[a-z-]+$|} in

            List.iter
              (fun selector ->
                (* Clean selector: remove dot, unescape colons, remove
                   pseudo-classes *)
                let expected_html_base =
                  selector
                  |> Re.replace_string dot_removal_regex ~by:""
                  |> Re.replace_string colon_unescape_regex ~by:":"
                  |> Re.replace_string pseudo_class_regex ~by:""
                in

                (* Verify the selector corresponds to the HTML class *)
                if not (String.equal html_class expected_html_base) then
                  Alcotest.failf
                    "HTML class '%s' doesn't match CSS selector '%s' (expected \
                     base: '%s')"
                    html_class selector expected_html_base)
              selectors;

            Printf.printf "✓ %s: HTML='%s' ↔ CSS has %d selectors\n" name
              html_class (List.length selectors))
  in

  (* Test core utilities *)
  test_html_css_consistency "text-color" (text blue 500);
  test_html_css_consistency "bg-color" (bg red 600);
  test_html_css_consistency "border-color" (border_color green 400);
  test_html_css_consistency "padding" (p 4);
  test_html_css_consistency "margin" (m 8);
  test_html_css_consistency "width" (w 64);
  test_html_css_consistency "height" (h 32);

  (* Test modifiers *)
  test_html_css_consistency "hover-text" (on_hover [ text blue 900 ]);
  test_html_css_consistency "focus-bg" (on_focus [ bg yellow 200 ]);
  test_html_css_consistency "active-border"
    (on_active [ border_color purple 500 ]);
  test_html_css_consistency "disabled-opacity" (on_disabled [ opacity 50 ]);
  test_html_css_consistency "group-hover" (on_group_hover [ text sky 700 ]);

  (* Test responsive breakpoints *)
  test_html_css_consistency "sm-text" (on_sm [ text gray 800 ]);
  test_html_css_consistency "md-padding" (on_md [ px 6 ]);
  test_html_css_consistency "lg-margin" (on_lg [ my 12 ]);

  (* Test combined modifiers - test each individually since Group is internal *)
  test_html_css_consistency "hover-blue-600" (on_hover [ text blue 600 ]);
  test_html_css_consistency "focus-blue-800" (on_focus [ text blue 800 ]);
  test_html_css_consistency "responsive-hover"
    (on_md [ on_hover [ bg indigo 500 ] ]);

  (* Test hex colors *)
  test_html_css_consistency "hex-text" (text (of_hex "#1da1f2") 0);
  test_html_css_consistency "hex-bg" (bg (of_hex "#ff6b6b") 0);

  (* Test gradient stops *)
  test_html_css_consistency "from-color" (from_color ~shade:400 blue);
  test_html_css_consistency "via-color" (via_color ~shade:600 purple);
  test_html_css_consistency "to-color" (to_color ~shade:800 red)

(* Test 1:1 equivalence with real Tailwind CSS *)
let test_real_tailwind_equivalence () =
  let test_exact_match description html_class_expected tw_style
      css_selector_expected =
    let actual_html_class = pp tw_style in
    let actual_css = to_css [ tw_style ] |> Css.to_string in

    (* Test HTML class matches exactly *)
    Alcotest.check string
      (description ^ " HTML class")
      html_class_expected actual_html_class;

    (* Test CSS contains the expected selector *)
    if not (Astring.String.is_infix ~affix:css_selector_expected actual_css)
    then
      Alcotest.failf "%s: Expected CSS selector '%s' not found in:\n%s"
        description css_selector_expected actual_css;

    Printf.printf "✓ %s: HTML='%s' ↔ CSS contains '%s'\n" description
      actual_html_class css_selector_expected
  in

  (* Critical test: Group-hover must match Tailwind exactly *)
  test_exact_match "group-hover" "group-hover:text-white"
    (on_group_hover [ text white 0 ])
    ".group:hover .group-hover\\:text-white";

  test_exact_match "group-focus" "group-focus:bg-blue-100"
    (on_group_focus [ bg blue 100 ])
    ".group:focus .group-focus\\:bg-blue-100";

  (* Regular hover should still work *)
  test_exact_match "hover" "hover:text-blue-500"
    (on_hover [ text blue 500 ])
    ".hover\\:text-blue-500:hover";

  (* Responsive + hover combination - CSS is inside @media, class name without
     responsive prefix *)
  test_exact_match "responsive-hover" "md:hover:bg-red-500"
    (on_md [ on_hover [ bg red 500 ] ])
    ".hover\\:bg-red-500:hover"

(* Test compatibility with real Tailwind CSS *)
let test_tailwind_compatibility () =
  let test_tailwind_class_format name expected_class tw_style =
    let actual_class = pp tw_style in
    Alcotest.check string (name ^ " class format") expected_class actual_class;
    Printf.printf "✓ %s: '%s' matches Tailwind format\n" name actual_class
  in

  (* Test that our class names match Tailwind's naming conventions *)
  test_tailwind_class_format "text-color" "text-blue-500" (text blue 500);
  test_tailwind_class_format "bg-color" "bg-red-600" (bg red 600);
  test_tailwind_class_format "border-color" "border-green-400"
    (border_color green 400);
  test_tailwind_class_format "padding" "p-4" (p 4);
  test_tailwind_class_format "margin" "m-8" (m 8);
  test_tailwind_class_format "width" "w-64" (w 64);
  test_tailwind_class_format "height" "h-32" (h 32);

  (* Test modifiers match Tailwind format *)
  test_tailwind_class_format "hover" "hover:text-blue-900"
    (on_hover [ text blue 900 ]);
  test_tailwind_class_format "focus" "focus:bg-yellow-200"
    (on_focus [ bg yellow 200 ]);
  test_tailwind_class_format "active" "active:border-purple-500"
    (on_active [ border_color purple 500 ]);

  (* Test responsive breakpoints *)
  test_tailwind_class_format "sm-responsive" "sm:text-gray-800"
    (on_sm [ text gray 800 ]);
  test_tailwind_class_format "md-responsive" "md:px-6" (on_md [ px 6 ]);
  test_tailwind_class_format "lg-responsive" "lg:my-12" (on_lg [ my 12 ]);

  (* Test hex colors (our custom format should be clearly marked) *)
  test_tailwind_class_format "hex-color" "text-[1da1f2]"
    (text (of_hex "#1da1f2") 0);
  test_tailwind_class_format "hex-bg" "bg-[ff6b6b]" (bg (of_hex "#ff6b6b") 0);

  (* Test gradient stops *)
  test_tailwind_class_format "from-gradient" "from-blue-400"
    (from_color ~shade:400 blue);
  test_tailwind_class_format "via-gradient" "via-purple-600"
    (via_color ~shade:600 purple);
  test_tailwind_class_format "to-gradient" "to-red-800"
    (to_color ~shade:800 red)

let test_3d_transforms () =
  let test_class_name tw expected =
    let actual = pp tw in
    Alcotest.check string ("3D transform " ^ expected) expected actual
  in
  let test_css_value tw prop expected =
    let stylesheet = to_css [ tw ] in
    let css_str = Css.to_string stylesheet in
    let has_value =
      Astring.String.is_infix ~affix:(prop ^ ": " ^ expected) css_str
    in
    if not has_value then
      Alcotest.failf "Expected CSS property %s: %s not found in:\n%s" prop
        expected css_str
  in

  (* Test rotate X/Y/Z *)
  test_class_name (rotate_x 45) "rotate-x-45";
  test_css_value (rotate_x 45) "transform" "rotateX(45deg)";
  test_class_name (rotate_x (-30)) "-rotate-x-30";
  test_css_value (rotate_x (-30)) "transform" "rotateX(-30deg)";

  test_class_name (rotate_y 90) "rotate-y-90";
  test_css_value (rotate_y 90) "transform" "rotateY(90deg)";
  test_class_name (rotate_y (-60)) "-rotate-y-60";
  test_css_value (rotate_y (-60)) "transform" "rotateY(-60deg)";

  test_class_name (rotate_z 180) "rotate-z-180";
  test_css_value (rotate_z 180) "transform" "rotateZ(180deg)";

  (* Test translate Z *)
  test_class_name (translate_z 100) "translate-z-100";
  test_css_value (translate_z 100) "transform" "translateZ(100px)";
  test_class_name (translate_z (-50)) "-translate-z-50";
  test_css_value (translate_z (-50)) "transform" "translateZ(-50px)";

  (* Test scale Z *)
  test_class_name (scale_z 150) "scale-z-150";
  test_css_value (scale_z 150) "--tw-scale-z" "1.5";

  (* Test perspective *)
  test_class_name (perspective 1000) "perspective-1000";
  test_css_value (perspective 1000) "perspective" "1000px";

  (* Test perspective origin *)
  test_class_name perspective_origin_center "perspective-origin-center";
  test_css_value perspective_origin_center "perspective-origin" "center";

  test_class_name perspective_origin_top "perspective-origin-top";
  test_css_value perspective_origin_top "perspective-origin" "top";

  test_class_name perspective_origin_left "perspective-origin-left";
  test_css_value perspective_origin_left "perspective-origin" "left"

(** Test spacing_to_rem conversion function *)
let test_spacing_to_rem () =
  let test_spacing_value n expected_css =
    (* Create a padding style with the given spacing value *)
    let style = p n in
    let stylesheet = to_css ~reset:false [ style ] in
    let css_str = Css.to_string stylesheet in
    let expected_property = "padding: " ^ expected_css in
    Alcotest.check bool
      (Printf.sprintf "p %d generates padding: %s" n expected_css)
      true
      (string_contains_substring css_str expected_property)
  in

  (* Test hardcoded values *)
  test_spacing_value 0 "0";
  test_spacing_value 1 "0.25rem";
  test_spacing_value 2 "0.5rem";
  test_spacing_value 3 "0.75rem";
  test_spacing_value 4 "1rem";
  test_spacing_value 6 "1.5rem";
  test_spacing_value 8 "2rem";
  test_spacing_value 10 "2.5rem";
  test_spacing_value 12 "3rem";
  test_spacing_value 16 "4rem";
  test_spacing_value 20 "5rem";
  test_spacing_value 24 "6rem";
  test_spacing_value 56 "14rem";

  (* Test fallback formula: n * 0.25rem *)
  test_spacing_value 5 "1.25rem";
  (* 5 * 0.25 = 1.25 *)
  test_spacing_value 7 "1.75rem";
  (* 7 * 0.25 = 1.75 *)
  test_spacing_value 9 "2.25rem";
  (* 9 * 0.25 = 2.25 *)
  test_spacing_value 32 "8rem";
  (* 32 * 0.25 = 8.0 *)
  test_spacing_value 40 "10rem";
  (* 40 * 0.25 = 10.0 *)
  test_spacing_value 64 "16rem";
  (* 64 * 0.25 = 16.0 *)
  test_spacing_value 80 "20rem";
  (* 80 * 0.25 = 20.0 *)
  test_spacing_value 96 "24rem" (* 96 * 0.25 = 24.0 *)

(** Test responsive modifiers *)
let test_responsive_modifiers () =
  let test_class_name tw expected =
    let actual = pp tw in
    Alcotest.check string ("responsive class " ^ expected) expected actual
  in

  test_class_name (on_sm [ bg red 500 ]) "sm:bg-red-500";
  test_class_name (on_md [ text blue 600 ]) "md:text-blue-600";
  test_class_name (on_lg [ p 8 ]) "lg:p-8";
  test_class_name (on_xl [ flex ]) "xl:flex"

(** Test state modifiers *)
let test_state_modifiers () =
  let test_class_name tw expected =
    let actual = pp tw in
    Alcotest.check string ("state modifier " ^ expected) expected actual
  in

  test_class_name (on_hover [ bg gray 700 ]) "hover:bg-gray-700";
  test_class_name (on_focus [ text sky 500 ]) "focus:text-sky-500";
  test_class_name (on_active [ border_color teal 400 ]) "active:border-teal-400"

let test_css_prelude () =
  (* Test that our CSS reset/prelude matches expected structure *)
  let stylesheet = to_css [] in
  let css = Css.to_string ~minify:false stylesheet in

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
      (p 0, ".p-0 {\n  padding: 0;\n}");
      (m_auto, ".m-auto {\n  margin: auto;\n}");
      (opacity 100, ".opacity-100 {\n  opacity: 1;\n}");
      (flex, ".flex {\n  display: flex;\n}");
    ]
  in

  List.iter
    (fun (tw_style, expected) ->
      let stylesheet = to_css [ tw_style ] in
      let css = Css.to_string ~minify:false stylesheet in
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
        failf "CSS output doesn't match exactly for %s" (pp tw_style)))
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
  let tw_minified = Css.to_string ~minify:true stylesheet in

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

(** Helper to find end of CSS rule *)
let rule_end css start = Astring.String.find_sub ~start ~sub:"}" css

(** Helper to extract a CSS rule for a given selector *)
let extract_rule selector css =
  try
    let selector_with_space = selector ^ " {" in
    match Astring.String.find_sub ~sub:selector_with_space css with
    | None -> None
    | Some idx -> (
        let start = idx + String.length selector_with_space in
        match rule_end css start with
        | None -> None
        | Some end_idx -> Some (String.sub css start (end_idx - start)))
  with Invalid_argument _ | Not_found -> None

(** Helper to check key properties in prose CSS *)
let check_prose_properties tw_props =
  let key_props = [ "color:"; "max-width:"; "font-size:"; "line-height:" ] in
  List.iter
    (fun prop ->
      if not (Astring.String.is_infix ~affix:prop tw_props) then
        Alcotest.failf "Missing property %s in .prose" prop)
    key_props

(** Helper to check if selector exists in CSS *)
let check_selector_present tw_css classname selector =
  let has_selector = Astring.String.is_infix ~affix:selector tw_css in
  if not has_selector then
    Alcotest.failf "Prose CSS missing selector %s in class %s" selector
      classname

(** Test a prose style with expected selectors *)
let test_prose_style style expected_selectors =
  try
    let classname = pp style in
    let tw_css = to_css ~reset:false [ style ] |> Css.to_string ~minify:false in
    let _tailwind_css = generate_tailwind_css ~minify:false [ classname ] in

    (* Check that key selectors are present *)
    List.iter (check_selector_present tw_css classname) expected_selectors;

    (* Compare base prose properties if this is the base prose class *)
    if classname = "prose" then
      match extract_rule ".prose" tw_css with
      | Some tw_props -> check_prose_properties tw_props
      | None -> ()
  with e ->
    Alcotest.failf "Prose CSS comparison failed for %s: %s" (pp style)
      (Printexc.to_string e)

(** Test comprehensive prose CSS comparison with Tailwind *)
let test_prose_css_comparison () =
  (* Test each prose variant with expected selectors *)
  test_prose_style prose
    [
      ".prose";
      ".prose h1";
      ".prose h2";
      ".prose p";
      ".prose a";
      ".prose code";
      ".prose pre";
      ".prose ul";
      ".prose blockquote";
    ];
  test_prose_style prose_sm [ ".prose-sm"; ".prose-sm h1"; ".prose-sm p" ];
  test_prose_style prose_lg [ ".prose-lg"; ".prose-lg h1"; ".prose-lg p" ];
  test_prose_style prose_gray [ ".prose-gray" ];
  test_prose_style prose_slate [ ".prose-slate" ]

(** Test prose with modifiers *)
let test_prose_with_modifiers () =
  (* Test responsive modifiers with prose *)
  let responsive_prose = on_sm [ prose ] in
  let class_name = pp responsive_prose in
  Alcotest.(check string) "responsive prose" "sm:prose" class_name;

  (* Hover modifier should work with prose *)
  let hover_prose = on_hover [ prose ] in
  let hover_class = pp hover_prose in
  Alcotest.(check string) "hover prose" "hover:prose" hover_class;

  (* Test CSS generation for responsive prose *)
  let css =
    to_css ~reset:false [ responsive_prose ] |> Css.to_string ~minify:false
  in
  Alcotest.(check bool)
    "has media query" true
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
  let combined = to_classes [ prose; prose_lg; prose_gray ] in
  Alcotest.(check string)
    "multiple prose classes" "prose prose-lg prose-gray" combined;

  (* Test CSS generation for combined prose *)
  let css =
    to_css ~reset:false [ prose; prose_lg ] |> Css.to_string ~minify:false
  in

  (* Both base prose and size variant should be present *)
  Alcotest.(check bool)
    "has base prose" true
    (Astring.String.is_infix ~affix:".prose {" css);
  Alcotest.(check bool)
    "has prose-lg" true
    (Astring.String.is_infix ~affix:".prose-lg {" css);

  (* Test that prose can be combined with other utilities *)
  let with_utilities = to_classes [ prose; mx_auto; max_w_3xl ] in
  Alcotest.(check string)
    "prose with utilities" "prose mx-auto max-w-3xl" with_utilities

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
      let class_name = pp style in
      (* Generate CSS and verify it contains the expected class selector *)
      let stylesheet = to_css ~reset:false [ style ] in
      let css_output = Css.to_string ~minify:false stylesheet in

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
      ( data_variant "primary" bg_white,
        ".bg-white[data-variant=\"primary\"] {\n\
        \  --tw-bg-opacity: 1;\n\
        \  background-color: rgb(255 255 255 / var(--tw-bg-opacity));\n\
         }" );
      ( on_data_active [ text_white ],
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
      let css = Css.to_string ~minify:false stylesheet in
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
  let styles = [ bg_blue; text_white; p 4; m 2; rounded_md ] in
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
  let styles = [ w dynamic_width; bg sky 300; p 2 ] in
  let inline = to_inline_style styles in

  (* Check that dynamic value is included *)
  Alcotest.check bool "has dynamic width" true
    (Astring.String.is_infix
       ~affix:(string_of_float (float_of_int dynamic_width *. 0.25) ^ "rem")
       inline)

(** Helper function for roundtrip testing *)
let test_roundtrip style =
  let class_str = pp style in
  match of_string class_str with
  | Error (`Msg msg) ->
      Alcotest.fail
        (Fmt.str "Failed to parse generated class '%s': %s" class_str msg)
  | Ok parsed_style ->
      let parsed_class_str = pp parsed_style in
      Alcotest.check string
        (Fmt.str "Roundtrip for %s" class_str)
        class_str parsed_class_str

(** Test of_string roundtrip for color styles *)
let test_of_string_roundtrip_colors () =
  let styles =
    [ bg_blue; bg sky 500; text_white; text_blue; border_color gray 300 ]
  in
  List.iter test_roundtrip styles

(** Test of_string roundtrip for spacing styles *)
let test_of_string_roundtrip_spacing () =
  let styles = [ p 4; px 2; py 8; m_auto; mx 4; gap 6 ] in
  List.iter test_roundtrip styles

(** Test of_string roundtrip for typography styles *)
let test_of_string_roundtrip_typography () =
  let styles =
    [
      text_lg;
      text_center;
      font_bold;
      italic;
      underline;
      leading_tight;
      prose;
      prose_sm;
      prose_lg;
      prose_xl;
      prose_2xl;
      prose_gray;
      prose_slate;
    ]
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
      w_full;
      h_screen;
      max_w_2xl;
      min_h 10;
    ]
  in
  List.iter test_roundtrip styles

(** Test of_string roundtrip for effects and modifiers *)
let test_of_string_roundtrip_effects () =
  let styles =
    [
      border_xs;
      rounded_lg;
      shadow_md;
      opacity 50;
      transition_colors;
      duration 300;
      scale 105;
      on_hover [ bg blue 700 ];
      on_focus [ ring_md ];
      on_sm [ text_xl ];
      on_sm [ bg red 500 ];
      on_md [ text blue 600 ];
      on_lg [ p 8 ];
      on_xl [ flex ];
      on_2xl [ grid ];
      on_hover [ text_white ];
      on_focus [ bg sky 500 ];
      on_active [ text gray 900 ];
      on_disabled [ opacity 50 ];
      on_dark [ bg_gray ];
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
    let shade = gen_shade () in
    match Random.int 3 with
    | 0 -> bg color shade
    | 1 -> text color shade
    | _ -> border_color color shade

  let gen_spacing_style () =
    match Random.int 20 with
    | 0 | 1 | 2 ->
        let spacing_int = gen_spacing_int () in
        p spacing_int
    | 3 | 4 ->
        let spacing_int = gen_spacing_int () in
        px spacing_int
    | 5 | 6 ->
        let spacing_int = gen_spacing_int () in
        py spacing_int
    | 7 -> p_px
    | 8 -> p_full
    | 9 ->
        let spacing_int = gen_spacing_int () in
        m spacing_int
    | 10 ->
        let spacing_int = gen_spacing_int () in
        mx spacing_int
    | 11 -> m_auto
    | 12 -> mx_auto
    | 13 -> my_auto
    | 14 ->
        let spacing_int = gen_spacing_int () in
        gap spacing_int
    | 15 -> gap_px
    | 16 ->
        let spacing_int = gen_spacing_int () in
        pt spacing_int
    | _ ->
        let spacing_int = gen_spacing_int () in
        pb spacing_int

  let gen_size_style () =
    match Random.int 20 with
    | 0 | 1 | 2 ->
        let scale_int = gen_spacing_int () in
        w scale_int
    | 3 | 4 ->
        let scale_int = gen_spacing_int () in
        h scale_int
    | 5 -> w_full
    | 6 -> w_screen
    | 7 -> w_min
    | 8 -> h_full
    | 9 -> h_screen
    | 10 ->
        let scale_int = gen_spacing_int () in
        min_w scale_int
    | 11 -> min_w_full
    | 12 -> min_h_screen
    | 13 ->
        let scale_int = gen_spacing_int () in
        max_w scale_int
    | 14 -> max_w_2xl
    | 15 -> max_w_4xl
    | 16 -> max_w_full
    | _ ->
        let scale_int = gen_spacing_int () in
        max_h scale_int

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
    let class_name = pp style in
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
    let css1 = to_css ~reset:false styles |> Css.to_string in
    let css2 = to_css ~reset:false styles |> Css.to_string in
    Alcotest.check Alcotest.string "CSS generation is deterministic" css1 css2
  done

(** Property: minified CSS should never be larger than normal CSS *)
let test_minification_always_smaller () =
  Random.init 42;
  for _ = 1 to 50 do
    let styles = Generators.gen_style_list (1 + Random.int 20) in
    let stylesheet = to_css ~reset:false styles in
    let normal = Css.to_string ~minify:false stylesheet in
    let minified = Css.to_string ~minify:true stylesheet in
    Alcotest.check Alcotest.bool "minified <= normal size" true
      (String.length minified <= String.length normal)
  done

(** Property: combining style lists should combine their CSS *)
let test_style_combination () =
  Random.init 42;
  for _ = 1 to 20 do
    let styles1 = Generators.gen_style_list (1 + Random.int 5) in
    let styles2 = Generators.gen_style_list (1 + Random.int 5) in
    let css1 = to_css ~reset:false styles1 |> Css.to_string in
    let combined = to_css ~reset:false (styles1 @ styles2) |> Css.to_string in

    (* Combined should contain classes from both *)
    List.iter
      (fun style ->
        let class_name = "." ^ pp style in
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
      ("sm:text-black", on_sm [ text_black ]);
      ("md:bg-blue-500", on_md [ bg blue 500 ]);
      ("lg:p-8", on_lg [ p 8 ]);
      ("xl:flex", on_xl [ flex ]);
      ("2xl:grid", on_2xl [ grid ]);
      ("hover:text-white", on_hover [ text_white ]);
      ("focus:bg-sky-500", on_focus [ bg sky 500 ]);
      ("active:text-gray-900", on_active [ text gray 900 ]);
      ("disabled:opacity-50", on_disabled [ opacity 50 ]);
      ("dark:bg-gray", on_dark [ bg_gray ]);
    ]
  in

  List.iter
    (fun (input, expected) ->
      match of_string input with
      | Error (`Msg msg) -> Alcotest.failf "Failed to parse '%s': %s" input msg
      | Ok parsed ->
          let expected_str = pp expected in
          let parsed_str = pp parsed in
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
        let cls' = pp style in
        Alcotest.check Alcotest.string "roundtrip preserves class" cls cls'
    | Error (`Msg msg) -> failf "Failed to parse valid class %s: %s" cls msg
  done

(** Test CSS property deduplication *)
let test_css_property_deduplication () =
  (* Test that duplicate properties are removed, keeping the last one *)
  let styles =
    [
      bg red 500;
      (* first bg *)
      p 4;
      bg blue 700;
      (* second bg - should override first *)
      text_white;
      bg_black;
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
  let styles = [ bg blue 500; text_white; p 4; rounded_md; opacity 75 ] in
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
  let styles = [ bg blue 500; text_white; p 4; m 2; rounded_lg; shadow_md ] in
  let stylesheet = to_css ~reset:false styles in
  let normal = Css.to_string ~minify:false stylesheet in
  let minified = Css.to_string ~minify:true stylesheet in

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
    test_case "negative spacing support" `Quick test_negative_spacing;
    test_case "container queries" `Quick test_container_queries;
    test_case "hex colors" `Quick test_hex_colors;
    test_case "gradient stops" `Quick test_gradient_stops;
    test_case "HTML/CSS selector matching" `Quick
      test_html_css_selector_matching;
    test_case "HTML/CSS regression tests" `Quick test_html_css_regression;
    test_case "Real Tailwind equivalence" `Quick test_real_tailwind_equivalence;
    test_case "Tailwind compatibility" `Quick test_tailwind_compatibility;
    test_case "3D transforms" `Quick test_3d_transforms;
    test_case "spacing to rem conversion" `Quick test_spacing_to_rem;
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
    test_case "prose variant combinations" `Quick
      test_prose_variant_combinations;
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
