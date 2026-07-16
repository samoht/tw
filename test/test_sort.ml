module Css = Cascade.Css
open Alcotest
open Tw.Color
open Tw.Padding

(* OCaml 4.14 compat *)
let index f lst =
  let rec go i = function
    | [] -> None
    | x :: _ when f x -> Some i
    | _ :: rest -> go (i + 1) rest
  in
  go 0 lst

(* ===== Tests ===== *)

(* Short, reusable helper *)
let sheet_of ?(base = false) ?(mode = Css.Variables) styles =
  let sheet =
    Tw.Build.to_css
      ~config:{ Tw.Build.base; forms = None; layers = true }
      styles
  in
  let sheet =
    match mode with
    | Css.Inline -> Css.inline_vars sheet
    | Css.Variables -> sheet
  in
  sheet

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
  |> Option.map Css.rules_of_statements
  |> Option.map Css.custom_props_of_rules
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
      Css.rules_of_statements stmts
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
  let sheet = sheet_of [ bg cyan; bg sky; bg blue ] in
  let theme_colors = extract_theme_color_vars sheet in
  check (list string) "theme layer: cyan, sky, blue" [ "cyan"; "sky"; "blue" ]
    theme_colors

let test_utilities_layer_color_order () =
  let sheet = sheet_of [ bg cyan; bg sky; bg blue ] in
  let util_colors = extract_utility_selectors sheet in
  check (list string) "utilities layer: blue, cyan, sky"
    [ "blue"; "cyan"; "sky" ] util_colors

let test_deterministic_ordering () =
  let inputs =
    [
      [ bg cyan; bg sky; bg blue ];
      [ bg blue; bg sky; bg cyan ];
      [ bg sky; bg blue; bg cyan ];
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

  let index f lst =
    let rec go i = function
      | [] -> None
      | x :: _ when f x -> Some i
      | _ :: rest -> go (i + 1) rest
    in
    go 0 lst
  in
  let p4_idx = index is_p4 selectors in
  let p2_idx = index is_p2 selectors in

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
      bg blue;
      (* Initial color *)
      text white;
      (* Some other styles *)
      bg red;
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
    let rec go i = function
      | [] -> None
      | sel :: _ when Css.Selector.to_string sel = ".bg-blue-500" -> Some i
      | _ :: rest -> go (i + 1) rest
    in
    go 0 original_selectors
  in
  let orig_red_idx =
    let rec go i = function
      | [] -> None
      | sel :: _ when Css.Selector.to_string sel = ".bg-red-500" -> Some i
      | _ :: rest -> go (i + 1) rest
    in
    go 0 original_selectors
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
    index (fun sel -> sel = ".bg-blue-500") sorted_selectors
  in
  let sorted_red_idx =
    index (fun sel -> sel = ".bg-red-500") sorted_selectors
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
      text ~shade:600 gray;
      text_sm;
      text ~shade:800 gray;
      text white;
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
  let bg_utils = [ bg blue; bg red; bg green ] in
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

(* Verify utility families emit in Tailwind's canonical order. Priority is now
   per-variant (visibility, z-index and order live in modules whose other
   variants sort elsewhere), so this asserts the emitted rule order directly
   rather than poking module-level priority constants. *)
let test_handler_priority_ordering () =
  let classes =
    [
      "collapse";
      "sr-only";
      "absolute";
      "inset-0";
      "z-10";
      "order-1";
      "col-span-2";
      "container";
      "m-4";
      "box-border";
      "flex";
      "h-4";
    ]
  in
  let utilities = List.map (fun c -> Result.get_ok (Tw.of_string c)) classes in
  let css = Css.to_string ~minify:true (Tw.to_css ~base:false utilities) in
  let position needle =
    let n = String.length needle and h = String.length css in
    let rec go i =
      if i + n > h then -1
      else if String.sub css i n = needle then i
      else go (i + 1)
    in
    go 0
  in
  let rec check_chain = function
    | a :: (b :: _ as rest) ->
        let pa = position ("." ^ a) and pb = position ("." ^ b) in
        check bool
          (Fmt.str "%s before %s" a b)
          true
          (pa >= 0 && pb >= 0 && pa < pb);
        check_chain rest
    | _ -> ()
  in
  check_chain classes

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
          (fun color -> List.map (fun shade -> bg ~shade color) shades)
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
      bg white;
      bg black;
      bg ~shade:50 gray;
      bg ~shade:100 gray;
      bg ~shade:200 gray;
      bg ~shade:300 gray;
      bg gray;
      bg ~shade:900 gray;
      bg ~shade:50 red;
      bg ~shade:100 red;
      bg red;
      bg ~shade:600 red;
      bg ~shade:900 red;
      bg ~shade:50 blue;
      bg ~shade:100 blue;
      bg blue;
      bg ~shade:600 blue;
      bg ~shade:900 blue;
      bg ~shade:50 green;
      bg green;
      bg ~shade:600 green;
      bg ~shade:50 yellow;
      bg yellow;
      bg purple;
      bg pink;
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

let test_container_order () =
  (* .container sorts by its width property (after the position group, before
     margin), not first in the utilities layer. *)
  let classes = [ "container"; "sr-only"; "z-0"; "top-0"; "m-4"; "w-4" ] in
  let utilities = List.map (fun c -> Result.get_ok (Tw.of_string c)) classes in
  Test_helpers.check_ordering_matches
    ~test_name:"container width-property order" utilities

let test_arbitrary_vs_named_order () =
  (* Within a variant block, arbitrary values sort by their raw class name ('['
     = 0x5b, before lowercase letters), so dark:bg-[#...] precedes
     dark:bg-<name>. *)
  let classes =
    [
      "dark:bg-[#0D2C2E]";
      "dark:bg-gray-400";
      "dark:bg-white";
      "dark:bg-transparent";
    ]
  in
  let utilities = List.map (fun c -> Result.get_ok (Tw.of_string c)) classes in
  Test_helpers.check_ordering_matches
    ~test_name:"arbitrary before named within variant" utilities

let test_arbitrary_named_by_suffix () =
  (* Within a utility family, an arbitrary value sorts by its raw suffix like a
     named one, not always first: '[' is above digits and below letters, so
     rotate-180 precedes rotate-[-10deg], and max-w-3xl precedes max-w-[50%]
     which precedes max-w-sm. Arbitrary values order among themselves by value
     (rotate-[5deg] before rotate-[200deg]). *)
  let classes =
    [
      "rotate-90";
      "rotate-180";
      "rotate-[-10deg]";
      "rotate-[5deg]";
      "rotate-[200deg]";
      "max-w-3xl";
      "max-w-[6%]";
      "max-w-[50%]";
      "max-w-sm";
      "max-w-xs";
    ]
  in
  let utilities = List.map (fun c -> Result.get_ok (Tw.of_string c)) classes in
  Test_helpers.check_ordering_matches
    ~test_name:"arbitrary value sorts by suffix within family" utilities

let test_rounded_position_order () =
  (* Border-radius position groups sort by the CSS corners they write, matching
     Tailwind: the physical ones grouped by first corner clockwise -- top, then
     left, then right, then bottom (t, l, tl, r, tr, b, br, bl). rounded-l and
     rounded-b both write border-bottom-left-radius, so the order is
     render-affecting. Asserted directly on the emitted order because the
     ordering helper compares unoptimized output where the conflict is
     hidden. *)
  let classes =
    [
      "rounded-t-full";
      "rounded-l-full";
      "rounded-tl-full";
      "rounded-r-full";
      "rounded-b-full";
      "rounded-bl-full";
    ]
  in
  let utilities = List.map (fun c -> Result.get_ok (Tw.of_string c)) classes in
  let css = Css.to_string ~minify:true (Tw.to_css ~base:false utilities) in
  let position needle =
    let n = String.length needle and h = String.length css in
    let rec go i =
      if i + n > h then -1
      else if String.sub css i n = needle then i
      else go (i + 1)
    in
    go 0
  in
  let check_before a b =
    let pa = position a and pb = position b in
    Alcotest.check Alcotest.bool
      (Fmt.str "%s before %s" a b)
      true
      (pa >= 0 && pb >= 0 && pa < pb)
  in
  check_before ".rounded-t-full" ".rounded-l-full";
  check_before ".rounded-l-full" ".rounded-tl-full";
  check_before ".rounded-tl-full" ".rounded-r-full";
  check_before ".rounded-r-full" ".rounded-b-full";
  check_before ".rounded-b-full" ".rounded-bl-full"

let test_margin_value_order () =
  (* Margin values sort by raw suffix: numeric, then arbitrary ('['), then
     keywords auto < full < px. -ml-4 and -ml-px conflict on margin-left, so the
     order matters; a lexical/legacy order put px before the numbers. *)
  let classes =
    [
      "ml-0"; "ml-1"; "ml-4"; "ml-[3px]"; "ml-auto"; "ml-px"; "-ml-4"; "-ml-px";
    ]
  in
  let utilities = List.map (fun c -> Result.get_ok (Tw.of_string c)) classes in
  Test_helpers.check_ordering_matches ~test_name:"margin value order" utilities

let test_prose_margin_order () =
  (* prose is a priority-2 utility that sorts among the margin utilities exactly
     where Tailwind puts it: after the inline-end margins me-, before the top
     margins mt-. .prose :where(p) is specificity (0,1,0) -- :where() zeroes
     specificity -- so it ties a margin utility like m-auto and both write
     margin-top; the relative order is render-affecting for a p.m-auto inside
     .prose. Asserted directly on the emitted order. *)
  let classes = [ "me-4"; "prose"; "mt-4" ] in
  let utilities = List.map (fun c -> Result.get_ok (Tw.of_string c)) classes in
  let css = Css.to_string ~minify:true (Tw.to_css ~base:false utilities) in
  let position needle =
    let n = String.length needle and h = String.length css in
    let rec go i =
      if i + n > h then -1
      else if String.sub css i n = needle then i
      else go (i + 1)
    in
    go 0
  in
  let check_before a b =
    let pa = position a and pb = position b in
    Alcotest.check Alcotest.bool
      (Fmt.str "%s before %s" a b)
      true
      (pa >= 0 && pb >= 0 && pa < pb)
  in
  check_before ".me-4" ".prose";
  check_before ".prose" ".mt-4"

let test_variant_same_suborder_tiebreak () =
  (* Two arbitrary values of the same utility in a variant block have equal
     (priority, suborder); they must tie-break by selector, matching Tailwind's
     alphabetical order (dark:bg-[#003357] before dark:bg-[#0D2C2E]). *)
  let classes = [ "dark:bg-[#0D2C2E]"; "dark:bg-[#003357]" ] in
  let utilities = List.map (fun c -> Result.get_ok (Tw.of_string c)) classes in
  Test_helpers.check_ordering_matches
    ~test_name:"variant same-suborder tiebreak" utilities

let test_variant_arbitrary_numeric_order () =
  (* Arbitrary values in a variant block (hover:from-[rgba(5,...)] etc.) must
     order numerically like the regular layer, not lexically: rgba(5,...) sorts
     before rgba(14,...) and rgba(255,...). A lexical sort would place
     rgba(14,...) and rgba(255,...) first because '1' and '2' precede '5'. *)
  let classes =
    [
      "hover:from-[rgba(14,220,174,0.60)]";
      "hover:from-[rgba(5,74,218,0.60)]";
      "hover:from-[rgba(255,0,64,0.60)]";
    ]
  in
  let utilities = List.map (fun c -> Result.get_ok (Tw.of_string c)) classes in
  Test_helpers.check_ordering_matches
    ~test_name:"variant arbitrary values sort numerically" utilities

let test_compound_variant_highest_component () =
  (* A stacked variant sorts into the group of its highest-order component,
     after that group's base rules, matching Tailwind. dark:md:block (dark > md)
     and contrast-more:dark:text-white (dark > contrast-more) both sort with the
     dark rules, after the plain dark rules, even though one has dark as its
     outer token and the other as its inner token. *)
  let classes =
    [
      "dark:flex";
      "dark:font-semibold";
      "dark:md:block";
      "contrast-more:underline";
      "contrast-more:dark:text-white";
    ]
  in
  let utilities = List.map (fun c -> Result.get_ok (Tw.of_string c)) classes in
  Test_helpers.check_ordering_matches
    ~test_name:"compound variant highest component" utilities

let test_compound_variant_same_multiset () =
  (* group-[&_p]:hover and hover:group-[&_p] apply the same variant multiset in
     different nesting orders, so Tailwind gives them the same sort position and
     they collapse into one block. *)
  let classes =
    [
      "group-[&_p]:flex";
      "group-[&_p]:hover:flex";
      "hover:group-[&_p]:flex";
      "hover:group-[&_p]:hover:flex";
    ]
  in
  let utilities = List.map (fun c -> Result.get_ok (Tw.of_string c)) classes in
  Test_helpers.check_ordering_matches
    ~test_name:"compound variant same multiset" utilities

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
    test_case "container width-property order" `Slow test_container_order;
    test_case "arbitrary before named within family" `Slow
      test_arbitrary_vs_named_order;
    test_case "arbitrary value sorts by suffix within family" `Slow
      test_arbitrary_named_by_suffix;
    test_case "rounded position order" `Slow test_rounded_position_order;
    test_case "margin value order" `Slow test_margin_value_order;
    test_case "prose margin order" `Slow test_prose_margin_order;
    test_case "variant same-suborder tiebreak" `Slow
      test_variant_same_suborder_tiebreak;
    test_case "variant arbitrary values sort numerically" `Slow
      test_variant_arbitrary_numeric_order;
    test_case "compound variant highest component" `Slow
      test_compound_variant_highest_component;
    test_case "compound variant same multiset" `Slow
      test_compound_variant_same_multiset;
    test_case "regular before media same priority" `Quick
      test_regular_before_media;
    test_case "rules_of_grouped prose merging bug" `Quick
      rules_of_grouped_prose_bug;
    test_case "suborder within group" `Slow test_suborder_within_group;
    test_case "random utilities with minimization" `Slow
      test_random_utilities_with_minimization;
  ]

let suite = ("sort", tests)
