open Alcotest
open Tw.Color
open Tw.Padding

(* ===== Tests ===== *)

(* Short, reusable helper *)
let sheet_of ?(base = false) ?(mode = Css.Variables) ?(optimize = false) styles
    =
  Tw.Build.to_css
    ~config:{ Tw.Build.base; mode; optimize; forms = None; layers = true }
    styles

let extract_var_names_with_prefix (prefix : string) (props : string list) :
    string list =
  List.filter_map
    (fun prop ->
      if
        String.length prop > String.length prefix
        && String.sub prop 0 (String.length prefix) = prefix
      then
        let rest =
          String.sub prop (String.length prefix)
            (String.length prop - String.length prefix)
        in
        match String.index_opt rest '-' with
        | Some idx -> Some (String.sub rest 0 idx)
        | None -> None
      else None)
    props

let extract_theme_color_vars sheet =
  Css.layer_block "theme" sheet
  |> Option.map Css.rules_from_statements
  |> Option.map Css.custom_props_from_rules
  |> Option.map (extract_var_names_with_prefix "--color-")
  |> Option.value ~default:[]

let extract_bg_color_name sel_str =
  if String.length sel_str > 4 && String.sub sel_str 0 4 = ".bg-" then
    let rest = String.sub sel_str 4 (String.length sel_str - 4) in
    match String.index_opt rest '-' with
    | Some idx -> Some (String.sub rest 0 idx)
    | None -> None
  else None

let extract_utility_selectors sheet =
  Css.layer_block "utilities" sheet
  |> Option.map (fun stmts ->
      Css.rules_from_statements stmts
      |> List.filter_map (fun (sel, _) ->
          extract_bg_color_name (Css.Selector.to_string sel)))
  |> Option.value ~default:[]

(* New tests for exposed functions *)

let test_color_order () =
  check int "amber utilities order" 2
    (let _, order = Tw.Color.utilities_order "amber" in
     order);
  check int "blue utilities order" 3
    (let _, order = Tw.Color.utilities_order "blue" in
     order);
  check int "cyan utilities order" 4
    (let _, order = Tw.Color.utilities_order "cyan" in
     order);
  check int "sky utilities order" 17
    (let _, order = Tw.Color.utilities_order "sky" in
     order);
  check int "unknown color gets 100" 100
    (let _, order = Tw.Color.utilities_order "unknown" in
     order)

let test_theme_layer_color_order () =
  let sheet = sheet_of [ bg cyan 500; bg sky 500; bg blue 500 ] in
  let theme_colors = extract_theme_color_vars sheet in
  check (list string) "theme layer: cyan, sky, blue" [ "cyan"; "sky"; "blue" ]
    theme_colors

let test_utilities_layer_color_order () =
  let sheet = sheet_of [ bg cyan 500; bg sky 500; bg blue 500 ] in
  let util_colors = extract_utility_selectors sheet in
  check (list string) "utilities layer: blue, cyan, sky"
    [ "blue"; "cyan"; "sky" ] util_colors

let test_deterministic_ordering () =
  let inputs =
    [
      [ bg cyan 500; bg sky 500; bg blue 500 ];
      [ bg blue 500; bg sky 500; bg cyan 500 ];
      [ bg sky 500; bg blue 500; bg cyan 500 ];
    ]
  in
  let results =
    List.map
      (fun utilities ->
        let sheet = sheet_of utilities in
        Css.to_string ~minify:true sheet)
      inputs
  in
  match results with
  | [] -> failwith "No results"
  | first :: rest ->
      List.iter (fun css -> check string "deterministic output" first css) rest

let test_cascade_order_violation () =
  (* CSS cascade rule: when selectors have equal specificity, the last one in
     source order wins. Sorting breaks this! *)

  (* Example 1: User intentionally puts p-2 after p-4 to override *)
  let user_intent = [ p 4; p 2 ] in
  (* User wants p-2 to win *)

  (* Extract the rules and convert to pairs *)
  let rules = user_intent |> List.concat_map Tw.Rule.outputs in

  (* Get selector strings from the rules *)
  let selectors =
    List.map
      (fun rule ->
        match rule with
        | Tw.Output.Regular { selector; _ } -> Css.Selector.to_string selector
        | _ -> "")
      rules
  in

  Fmt.pr "@.=== CSS Cascade Order Violation Test ===@.";
  Fmt.pr "User wrote: [ p 4; p 2 ] (expecting p-2 to override p-4)@.";
  Fmt.pr "Extracted selectors: %a@."
    (Fmt.list ~sep:(Fmt.any ", ") Fmt.string)
    selectors;

  (* Find the p-4 and p-2 selectors *)
  let is_p4 sel = sel = ".p-4" in
  let is_p2 sel = sel = ".p-2" in

  let p4_idx = List.find_index is_p4 selectors in
  let p2_idx = List.find_index is_p2 selectors in

  match (p4_idx, p2_idx) with
  | Some i, Some j when i > j ->
      Alcotest.fail
        "CASCADE VIOLATION: p-4 comes after p-2, but user specified p-2 AFTER \
         p-4!"
  | Some i, Some j ->
      Fmt.pr "Correct order preserved: p-4 at index %d, p-2 at index %d@." i j
  | _ -> Alcotest.fail "Could not find both p-4 and p-2 selectors in output"

let test_cascade_prose_separation () =
  (* Test showing how sorting breaks intentional separation of .prose rules *)

  (* Extract prose rules to see their structure *)
  let rules = Tw.Rule.outputs Tw.Prose.prose in
  let pairs = Tw.Build.selector_props_pairs rules in

  Fmt.pr "@.=== Prose Rule Separation Test ===@.";
  Fmt.pr "Prose generates %d rules total@." (List.length pairs);

  (* Count how many .prose rules there are *)
  let prose_rules =
    List.filter (fun (sel, _, _) -> Css.Selector.to_string sel = ".prose") pairs
  in

  Fmt.pr "Found %d rules with selector .prose@." (List.length prose_rules);

  (* Apply of_grouped to see what happens *)
  let sorted_output = Tw.Build.of_grouped pairs in

  (* Find .prose rules in sorted output *)
  let sorted_prose_indices =
    List.mapi
      (fun i stmt ->
        match Css.statement_selector stmt with
        | Some sel when Css.Selector.to_string sel = ".prose" -> Some i
        | _ -> None)
      sorted_output
    |> List.filter_map (fun x -> x)
  in

  Fmt.pr "After of_grouped: .prose rules at indices %a@."
    (Fmt.list ~sep:(Fmt.any ", ") Fmt.int)
    sorted_prose_indices;

  (* Check if prose rules became adjacent *)
  match sorted_prose_indices with
  | i :: j :: _ when j = i + 1 ->
      Fmt.pr "WARNING: .prose rules are now ADJACENT (indices %d and %d)!@." i j;
      Fmt.pr
        "This means the optimizer will merge them, breaking the intended \
         separation.@."
  | _ -> Fmt.pr "Prose rules remain separated in output@."

let test_cascade_color_override () =
  (* Real-world example: user wants to override a color *)
  let styles =
    [
      bg blue 500;
      (* Initial color *)
      text white 0;
      (* Some other styles *)
      bg red 500;
      (* Override the background to red *)
    ]
  in

  (* Extract rules *)
  let rules = styles |> List.concat_map Tw.Rule.outputs in
  let pairs = Tw.Build.selector_props_pairs rules in

  Fmt.pr "@.=== Color Override Cascade Test ===@.";
  Fmt.pr "User intent: bg-blue-500, text-white, bg-red-500 (red should win)@.";
  Fmt.pr "Original pairs: %d rules@." (List.length pairs);

  (* Check original order *)
  let original_selectors = List.map (fun (sel, _, _) -> sel) pairs in
  let orig_blue_idx =
    List.find_index
      (fun sel -> Css.Selector.to_string sel = ".bg-blue-500")
      original_selectors
  in
  let orig_red_idx =
    List.find_index
      (fun sel -> Css.Selector.to_string sel = ".bg-red-500")
      original_selectors
  in

  (match (orig_blue_idx, orig_red_idx) with
  | Some bi, Some ri ->
      Fmt.pr "Original order: blue at %d, red at %d (correct)@." bi ri
  | _ -> Fmt.pr "Could not find both colors in original@.");

  (* Apply of_grouped to see if order changes *)
  let sorted_output = Tw.Build.of_grouped pairs in
  let sorted_selectors =
    List.map
      (fun stmt ->
        match Css.statement_selector stmt with
        | Some sel -> Css.Selector.to_string sel
        | None -> "")
      sorted_output
  in

  let sorted_blue_idx =
    List.find_index (fun sel -> sel = ".bg-blue-500") sorted_selectors
  in
  let sorted_red_idx =
    List.find_index (fun sel -> sel = ".bg-red-500") sorted_selectors
  in

  match (sorted_blue_idx, sorted_red_idx) with
  | Some bi, Some ri when bi > ri ->
      Fmt.pr
        "CASCADE VIOLATION: After sorting, bg-blue-500 (%d) comes AFTER \
         bg-red-500 (%d)!@."
        bi ri;
      Fmt.pr "This breaks the user's intention - red should override blue!@.";
      Alcotest.fail "Sorting reversed cascade order"
  | Some bi, Some ri ->
      Fmt.pr "After sorting: blue at %d, red at %d (order preserved)@." bi ri
  | _ -> Fmt.pr "Could not find both colors in sorted output@."

(* Test typography utilities come before color utilities *)
let test_typography_before_color () =
  let open Tw in
  (* Text alignment, size, and weight should come before text color *)
  let utilities =
    [
      text_center;
      text_4xl;
      font_bold;
      text gray 600;
      text_sm;
      text gray 800;
      text_white;
    ]
  in
  Test_helpers.check_ordering_matches
    ~test_name:"typography before color utilities" utilities

(* Test gap utilities come before self-alignment utilities *)
let test_gap_before_self_alignment () =
  let open Tw in
  (* gap-y-4 should come before self-start because gap-y suborders (65000+) are
     lower than self-* suborders (76000+). Both share priority 17. *)
  let utilities =
    [ gap_y 4; self_start; gap_x 2; self_end; gap 8; self_center ]
  in
  Test_helpers.check_ordering_matches
    ~test_name:"gap utilities before self-alignment" utilities

(* Test 1: Verify priority order - one utility per group *)
let test_priority_order_per_group () =
  let open Tw in
  (* Define pools of utilities for each priority group *)
  let position_utils = [ static; fixed; absolute; relative; sticky ] in
  let transform_utils = [ scale 105; rotate 45; scale 110 ] in
  let margin_utils = List.init 10 (fun i -> m i) in
  let prose_utils = [ prose; prose_sm; prose_lg; prose_xl ] in
  let layout_utils = [ block; inline; inline_block; hidden ] in
  let flex_utils = [ flex; inline_flex ] in
  let grid_utils = List.init 12 (fun i -> grid_cols (i + 1)) in
  let sizing_utils = List.init 10 (fun i -> w i) in
  let cursor_utils = [ cursor_pointer; cursor_default; cursor_wait ] in
  let grid_template_utils = List.init 6 (fun i -> grid_rows (i + 1)) in
  let alignment_utils = [ items_center; items_start; items_end ] in
  let gap_utils = List.init 10 (fun i -> gap i) in
  let border_utils = [ rounded; rounded_lg; rounded_full ] in
  let bg_utils = [ bg blue 500; bg red 500; bg green 500 ] in
  let padding_utils = List.init 10 (fun i -> p i) in
  let typography_utils = [ text_xl; text_sm; text_2xl ] in
  let effect_utils = [ shadow; shadow_md; shadow_lg ] in
  let animation_utils = [ animate_spin; animate_pulse; animate_bounce ] in
  let filter_utils = [ blur_sm; blur; blur_lg ] in

  let pick_random lst =
    List.nth lst (Random.State.int Test_helpers.test_rng (List.length lst))
  in

  let utilities =
    [
      pick_random position_utils;
      (* position: priority 0 *)
      pick_random margin_utils;
      (* margin: priority 2 *)
      pick_random prose_utils;
      (* prose: priority 3 *)
      pick_random layout_utils;
      (* layout: priority 4 *)
      pick_random flex_utils;
      (* flex: priority 4 *)
      pick_random grid_utils;
      (* grid: priority 4 *)
      pick_random sizing_utils;
      (* sizing: priority 6 *)
      pick_random cursor_utils;
      (* cursor: priority 7 *)
      pick_random grid_template_utils;
      (* grid_template: priority 8 *)
      pick_random alignment_utils;
      (* alignment: priority 9 *)
      pick_random gap_utils;
      (* gap: priority 10 *)
      pick_random border_utils;
      (* borders: priority 11 *)
      pick_random transform_utils;
      (* transforms: priority 6 *)
      pick_random animation_utils;
      (* animations: priority 7 *)
      pick_random bg_utils;
      (* backgrounds: priority 12 *)
      pick_random padding_utils;
      (* padding: priority 13 *)
      pick_random typography_utils;
      (* typography: priority 14 *)
      pick_random effect_utils;
      (* effects: priority 15 *)
      pick_random filter_utils;
      (* filters: priority 60 *)
    ]
  in
  Test_helpers.check_ordering_matches
    ~test_name:"priority order matches Tailwind" utilities

(* Verify Handler.priority values are in correct relative order *)
let test_handler_priority_ordering () =
  (* Get priority values directly from Handler modules. Note: Some modules
     (Prose, Display, Tables) don't expose Handler in their .mli, so we skip
     them in this test. Their ordering is still verified by
     test_priority_order_per_group which compares against Tailwind output. *)
  let position_prio = Tw.Position.Handler.priority in
  let margin_prio = Tw.Margin.Handler.priority in
  let layout_prio = Tw.Layout.Handler.priority in
  let flex_prio = Tw.Flex.Handler.priority in
  let grid_prio = Tw.Grid.Handler.priority in
  let sizing_prio = Tw.Sizing.Handler.priority in
  let cursor_prio = Tw.Cursor.Handler.priority in
  let grid_template_prio = Tw.Grid_template.Handler.priority in
  let alignment_prio = Tw.Alignment.Handler.priority in
  let gap_prio = Tw.Gap.Handler.priority in
  let border_prio = Tw.Borders.Handler.priority in
  let bg_prio = Tw.Backgrounds.Handler.priority in
  let padding_prio = Tw.Padding.Handler.priority in
  let typography_early_prio = Tw.Typography.Typography_early.priority in
  let typography_late_prio = Tw.Typography.Typography_late.priority in
  let color_prio = Tw.Color.Handler.priority in
  let effect_prio = Tw.Effects.Handler.priority in
  let transform_prio = Tw.Transforms.Handler.priority in
  let animation_prio = Tw.Animations.Handler.priority in
  let filter_prio = Tw.Filters.Handler.priority in

  (* Verify priority ordering - mostly total order with one exception: display
     utilities (layout, flex, grid, tables) all share priority 4 *)
  check bool "position < margin" true (position_prio < margin_prio);
  (* prose priority 3 skipped - Handler not exposed *)
  check bool "margin < layout" true (margin_prio < layout_prio);
  (* Display utilities all share priority 4: layout, flex, grid, tables. They
     are ordered by suborder instead of priority. *)
  check bool "layout = flex" true (layout_prio = flex_prio);
  check bool "flex = grid" true (flex_prio = grid_prio);
  check bool "grid < sizing" true (grid_prio < sizing_prio);
  check bool "sizing < flex_props" true
    (sizing_prio < Tw.Flex_props.Handler.priority);
  check bool "flex_props < transform" true
    (Tw.Flex_props.Handler.priority < transform_prio);
  check bool "transform < animation" true (transform_prio < animation_prio);
  check bool "animation < cursor" true (animation_prio < cursor_prio);
  check bool "cursor < grid_template" true (cursor_prio < grid_template_prio);
  check bool "grid_template < flex_layout" true
    (grid_template_prio < Tw.Flex_layout.Handler.priority);
  check bool "flex_layout < alignment" true
    (Tw.Flex_layout.Handler.priority < alignment_prio);
  (* Alignment and gap share priority 15, differentiated by suborder *)
  check bool "alignment = gap" true (alignment_prio = gap_prio);
  check bool "gap < border" true (gap_prio < border_prio);
  check bool "border < bg" true (border_prio < bg_prio);
  check bool "bg < padding" true (bg_prio < padding_prio);
  check bool "padding < typography_early" true
    (padding_prio < typography_early_prio);
  check bool "typography_early < color" true (typography_early_prio < color_prio);
  check bool "color < typography_late" true (color_prio < typography_late_prio);
  check bool "typography_late < effect" true (typography_late_prio < effect_prio);
  check bool "effect < filter" true (effect_prio < filter_prio)
(* display and tables priority checked in test_priority_order_per_group *)

(* Test 2: Verify suborder within same group *)
let test_suborder_within_group () =
  let open Tw in
  let test_groups =
    [
      ( "margin",
        List.concat_map
          (fun n -> [ m n; mx n; my n; mt n; mb n; ml n; mr n ])
          Test_helpers.spacing_values );
      ( "padding",
        List.concat_map
          (fun n -> [ p n; px n; py n; pt n; pb n; pl n; pr n ])
          Test_helpers.spacing_values );
      ( "sizing",
        List.concat_map
          (fun n -> [ w n; h n ])
          [ 0; 1; 2; 4; 8; 12; 16; 24; 32 ]
        @ [
            min_w 0;
            min_h 0;
            max_w_none;
            max_w_full;
            max_w_2xl;
            max_w_3xl;
            max_w_4xl;
            max_w_5xl;
            max_w_6xl;
          ] );
      ( "gap",
        List.concat_map
          (fun n -> [ gap n; gap_x n; gap_y n ])
          Test_helpers.spacing_values );
      ( "backgrounds",
        let colors = [ red; blue; green; yellow; purple; pink ] in
        let shades = [ 50; 100; 200; 300; 400; 500; 600; 700; 800; 900 ] in
        List.concat_map
          (fun color -> List.map (fun shade -> bg color shade) shades)
          colors );
      ("flex", [ flex; inline_flex ]);
      ( "grid",
        List.init 12 (fun i -> grid_cols (i + 1))
        @ List.init 6 (fun i -> grid_rows (i + 1)) );
      ( "typography",
        [
          text_xs;
          text_sm;
          text_base;
          text_lg;
          text_xl;
          text_2xl;
          text_3xl;
          font_thin;
          font_light;
          font_normal;
          font_medium;
          font_semibold;
          font_bold;
          font_extrabold;
          font_black;
          text_left;
          text_center;
          text_right;
          text_justify;
        ] );
      ( "borders",
        [
          border_xs;
          border_sm;
          border_md;
          border_lg;
          border_solid;
          border_dashed;
          border_dotted;
          border_double;
          border_none;
          rounded_none;
          rounded_sm;
          rounded;
          rounded_md;
          rounded_lg;
          rounded_xl;
          rounded_2xl;
          rounded_3xl;
          rounded_full;
        ] );
      ( "cursor",
        [
          cursor_auto;
          cursor_default;
          cursor_pointer;
          cursor_wait;
          cursor_move;
          cursor_not_allowed;
        ] );
      ( "alignment",
        [
          justify_start;
          justify_end;
          justify_center;
          justify_between;
          items_start;
          items_end;
          items_center;
          items_baseline;
          content_start;
          content_end;
          content_center;
          self_auto;
          self_start;
          self_end;
          justify_items_start;
          justify_self_auto;
          place_content_start;
          place_items_start;
          place_self_auto;
        ] );
      ( "layout",
        [
          block;
          inline;
          inline_block;
          hidden;
          object_contain;
          object_cover;
          object_fill;
          sr_only;
          not_sr_only;
          table_auto;
          table_fixed;
        ] );
      ("grid_placement", [ grid; inline_grid ]);
      ( "effects",
        [
          shadow_sm;
          shadow;
          shadow_md;
          shadow_lg;
          shadow_none;
          opacity 0;
          opacity 50;
          opacity 100;
        ] );
      ( "position",
        [ static; fixed; absolute; relative; sticky; inset 0; top 4; left 2 ] );
      ( "forms",
        [ form_input; form_checkbox; form_radio; form_select; form_textarea ] );
      ("transforms", [ translate_x 4; translate_y 2; rotate 90; scale 50 ]);
      ( "interactivity",
        [ select_none; select_text; select_all; scroll_auto; scroll_smooth ] );
      ( "filters",
        [ blur_sm; blur; blur_2xl; backdrop_blur; backdrop_opacity 50. ] );
      ( "containers",
        [
          container;
          at_container;
          at_container_normal;
          at_container_named "sidebar";
          at_container_named "header";
          at_container_named "main";
        ] );
      ( "animations",
        [
          animate_spin;
          animate_ping;
          animate_pulse;
          animate_bounce;
          transition_all;
          transition_none;
          duration 150;
          delay 200;
        ] );
    ]
  in

  List.iter
    (fun (group_name, utilities) ->
      let test_name =
        Fmt.str "suborder for %s group matches Tailwind" group_name
      in
      let shuffled = Test_helpers.shuffle utilities in
      let forms = group_name = "forms" in
      Test_helpers.check_ordering_matches ~forms ~test_name shuffled)
    test_groups

(* Test 3: Random utilities with minimization *)
let test_random_utilities_with_minimization () =
  let open Tw in
  (* Large pool of utilities from all groups *)
  let all_utilities =
    [
      (* Margin: priority 2 *)
      m 0;
      m 1;
      m 2;
      m 3;
      m 4;
      m 5;
      m 6;
      m 8;
      m 10;
      m 12;
      m 16;
      m 20;
      mx 0;
      mx 1;
      mx 2;
      mx 4;
      mx 8;
      my 0;
      my 1;
      my 2;
      my 4;
      my 8;
      mt 0;
      mt 1;
      mt 2;
      mt 4;
      mt 6;
      mt 8;
      mb 0;
      mb 1;
      mb 2;
      mb 3;
      mb 4;
      mb 6;
      ml 0;
      ml 1;
      ml 2;
      ml 4;
      mr 0;
      mr 1;
      mr 2;
      mr 4;
      mr 5;
      mr 8;
      (* Display: priority 10 *)
      block;
      inline;
      inline_block;
      flex;
      inline_flex;
      grid;
      inline_grid;
      hidden;
      (* Position: priority 11 *)
      static;
      fixed;
      absolute;
      relative;
      sticky;
      (* Sizing: priority 12 *)
      w 0;
      w 1;
      w 2;
      w 4;
      w 6;
      w 8;
      w 10;
      w 12;
      w 16;
      w 20;
      w 24;
      h 0;
      h 1;
      h 2;
      h 4;
      h 6;
      h 8;
      h 10;
      h 12;
      h 16;
      h 20;
      min_w 0;
      min_h 0;
      max_w_2xl;
      max_w_3xl;
      max_w_4xl;
      max_w_5xl;
      (* Grid layout: priority 13 *)
      grid_cols 1;
      grid_cols 2;
      grid_cols 3;
      grid_cols 4;
      grid_cols 6;
      grid_cols 12;
      grid_rows 1;
      grid_rows 2;
      grid_rows 3;
      grid_rows 4;
      col_span 1;
      col_span 2;
      col_span 3;
      col_span 4;
      row_span 1;
      row_span 2;
      row_span 3;
      (* Flex layout: priority 14 *)
      flex_row;
      flex_col;
      (* Container alignment: priority 15, suborder 0-999 *)
      items_start;
      items_center;
      items_end;
      items_baseline;
      items_stretch;
      justify_start;
      justify_center;
      justify_end;
      justify_between;
      justify_around;
      content_start;
      content_center;
      content_end;
      (* Gap: priority 15, suborder 25000+ *)
      gap 0;
      gap 1;
      gap 2;
      gap 3;
      gap 4;
      gap 6;
      gap 8;
      gap 10;
      gap 12;
      gap_x 0;
      gap_x 2;
      gap_x 4;
      gap_y 0;
      gap_y 2;
      gap_y 4;
      (* Self alignment: priority 15, suborder 50000+ *)
      self_auto;
      self_start;
      self_center;
      (* Border: priority 17 *)
      rounded;
      rounded_md;
      rounded_lg;
      rounded_full;
      border;
      (* Background: priority 18 *)
      bg white 0;
      bg black 0;
      bg gray 50;
      bg gray 100;
      bg gray 200;
      bg gray 300;
      bg gray 500;
      bg gray 900;
      bg red 50;
      bg red 100;
      bg red 500;
      bg red 600;
      bg red 900;
      bg blue 50;
      bg blue 100;
      bg blue 500;
      bg blue 600;
      bg blue 900;
      bg green 50;
      bg green 500;
      bg green 600;
      bg yellow 50;
      bg yellow 500;
      bg purple 500;
      bg pink 500;
      (* Padding: priority 19 *)
      p 0;
      p 1;
      p 2;
      p 3;
      p 4;
      p 5;
      p 6;
      p 8;
      p 10;
      p 12;
      p 16;
      px 0;
      px 1;
      px 2;
      px 4;
      px 6;
      px 8;
      py 0;
      py 1;
      py 2;
      py 4;
      py 6;
      py 8;
      pt 0;
      pt 1;
      pt 2;
      pt 3;
      pt 4;
      pt 6;
      pt 8;
      pb 0;
      pb 1;
      pb 2;
      pb 4;
      pb 6;
      pb 8;
      pl 0;
      pl 1;
      pl 2;
      pl 4;
      pl 6;
      pl 8;
      pr 0;
      pr 1;
      pr 2;
      pr 4;
      pr 6;
      pr 8;
      (* Text align: priority 20 *)
      text_left;
      text_center;
      text_right;
      text_justify;
      (* Typography: priority 100 *)
      text_xs;
      text_sm;
      text_base;
      text_lg;
      text_xl;
      text_2xl;
      text_3xl;
      font_thin;
      font_normal;
      font_medium;
      font_semibold;
      font_bold;
      leading_tight;
      leading_snug;
      leading_normal;
      leading_relaxed;
      (* Effects: priority 700 *)
      shadow;
      shadow_sm;
      shadow_md;
      shadow_lg;
      shadow_xl;
      shadow_none;
      opacity 0;
      opacity 50;
      opacity 75;
      opacity 100;
      (* Interactivity: priority 800 *)
      cursor_pointer;
      cursor_default;
      cursor_wait;
      cursor_not_allowed;
      select_none;
      select_text;
      select_all;
      (* Container/Prose: priority 1000 *)
      prose;
      prose_sm;
      prose_lg;
    ]
  in

  let pick_random n lst =
    let arr = Array.of_list lst in
    let len = Array.length arr in
    let picked = ref [] in
    for _ = 1 to min n len do
      let idx = Random.State.int Test_helpers.test_rng len in
      picked := arr.(idx) :: !picked
    done;
    !picked
  in

  let initial = pick_random 30 all_utilities in
  Fmt.epr "Testing with %d utilities@." (List.length initial);

  match
    Test_helpers.minimize_failing_case Test_helpers.check_ordering_fails initial
  with
  | None -> () (* Test passes *)
  | Some final ->
      let final_classes = List.map Tw.pp final in
      Fmt.epr "@.Minimal failing case (%d utilities): %a@." (List.length final)
        Fmt.(list ~sep:(const string " ") string)
        final_classes;

      (* Now run the actual test with minimal case *)
      Test_helpers.check_ordering_matches
        ~test_name:"random utilities ordering matches Tailwind" final

let test_border_width_color_ordering () =
  (* Test that border width utilities (borders.ml, priority 16) come before
   * border color utilities (borders.ml, priority 16) and background utilities
   * (backgrounds.ml, priority 18 for gradients, color.ml priority 21 for colors).
   *
   * This test verifies the fix for the ordering issue where border-b was
   * being separated from border-gray-200 by bg-* utilities.
   *
   * We use of_string to test the actual parsing behavior. *)
  let classes = [ "border-b"; "bg-blue-600"; "bg-white"; "border-gray-200" ] in
  let utilities = List.map (fun c -> Result.get_ok (Tw.of_string c)) classes in
  Test_helpers.check_ordering_matches
    ~test_name:"border width and color ordering" utilities

let test_regular_before_media () =
  (* Test that regular rules ALWAYS come before media queries, regardless of their priorities.
   * Example: max-w-4xl (regular, priority 8) and md:grid-cols-2 (media, priority 12).
   * Even though md:grid-cols-2 has higher priority, max-w-4xl (regular) should come first
   * because Tailwind emits all regular utilities before any media query utilities. *)
  let utilities = Tw.[ md [ grid_cols 2 ]; max_w_4xl ] in
  Test_helpers.check_ordering_matches ~test_name:"regular always before media"
    utilities

let prose_p_selector prose_class =
  Css.Selector.combine prose_class Css.Selector.Descendant
    (Css.Selector.where
       [
         Css.Selector.compound
           [
             Css.Selector.element "p";
             Css.Selector.not
               [
                 Css.Selector.where
                   [
                     Css.Selector.list
                       [
                         Css.Selector.class_ "not-prose";
                         Css.Selector.combine
                           (Css.Selector.class_ "not-prose")
                           Css.Selector.Descendant Css.Selector.universal;
                       ];
                   ];
               ];
           ];
       ])

let grouped_prose_pairs prose_body_var prose_class prose_p_sel =
  let _, prose_body_v =
    Tw.Var.binding prose_body_var (Tw.Css.oklch 37.3 0.034 259.733)
  in
  (* Order doesn't matter for this test - all have same order *)
  let order = (1000, 0) in
  [
    ( prose_class,
      [
        Tw.Css.color (Tw.Css.Var prose_body_v);
        Tw.Css.max_width (Tw.Css.Ch 65.0);
      ],
      order );
    ( prose_p_sel,
      [
        Tw.Css.margin_top (Tw.Css.Em 1.0); Tw.Css.margin_bottom (Tw.Css.Em 1.0);
      ],
      order );
    ( prose_class,
      [ Tw.Css.font_size (Tw.Css.Rem 1.0); Tw.Css.line_height (Tw.Css.Num 1.5) ],
      order );
  ]

let count_prose_rules rules =
  List.filter
    (fun stmt ->
      match Tw.Css.statement_selector stmt with
      | Some sel -> Tw.Css.Selector.to_string sel = ".prose"
      | None -> false)
    rules

let rules_of_grouped_prose_bug () =
  let prose_body_var = Tw.Var.channel Css.Color "prose-body" in
  let prose_class = Css.Selector.class_ "prose" in
  let prose_p_sel = prose_p_selector prose_class in
  let grouped_pairs =
    grouped_prose_pairs prose_body_var prose_class prose_p_sel
  in
  let output_rules = Tw.Build.of_grouped grouped_pairs in
  let prose_rules = count_prose_rules output_rules in

  Fmt.pr "@.=== test_rules_of_grouped_prose_bug ===@.";
  Fmt.pr "Input: 3 grouped pairs (2 .prose + 1 descendant)@.";
  Fmt.pr "Expected: 2 .prose rules in output@.";
  Fmt.pr "Actual: %d .prose rules in output@." (List.length prose_rules);
  check int "number of .prose rules" 2 (List.length prose_rules);
  check int "total output rules" 3 (List.length output_rules)

let tests =
  [
    (* New tests for exposed functions *)
    test_case "color_order" `Quick test_color_order;
    (* Layer ordering tests *)
    test_case "theme layer color order" `Quick test_theme_layer_color_order;
    test_case "utilities layer color order" `Quick
      test_utilities_layer_color_order;
    test_case "deterministic ordering" `Quick test_deterministic_ordering;
    (* CSS Cascade order tests - ensures source order is preserved *)
    test_case "source order preservation" `Quick test_cascade_order_violation;
    test_case "prose rule separation" `Quick test_cascade_prose_separation;
    test_case "color override cascading" `Quick test_cascade_color_override;
    (* Utility group ordering *)
    test_case "typography before color" `Quick test_typography_before_color;
    test_case "gap before self-alignment" `Quick test_gap_before_self_alignment;
    test_case "priority order per group" `Quick test_priority_order_per_group;
    test_case "handler priority ordering" `Quick test_handler_priority_ordering;
    test_case "border width and color ordering" `Quick
      test_border_width_color_ordering;
    test_case "regular before media same priority" `Quick
      test_regular_before_media;
    test_case "rules_of_grouped prose merging bug" `Quick
      rules_of_grouped_prose_bug;
    test_case "suborder within group" `Slow test_suborder_within_group;
    test_case "random utilities with minimization" `Slow
      test_random_utilities_with_minimization;
  ]

let suite = ("sort", tests)
