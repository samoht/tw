module Css = Cascade.Css
open Alcotest
open Tw.Color
open Tw.Backgrounds
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
  Css.layer_block [ "theme" ] sheet
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
  Css.layer_block [ "utilities" ] sheet
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

(* Tailwind's property table sorts letter-spacing, text-wrap, overflow-wrap,
   hyphens and white-space in that order (289, 290, 291, 294, 295): tracking
   opens the late-typography band, text-wrap and the word-wrapping families come
   next, and white-space is last of the five. [wrap-break-word] lives in
   [Overflow_wrap]'s own handler, a separate priority band from
   [Typography_late]'s default 26 - both have to land between tracking and
   whitespace for this to hold. None of these utilities write on a shared
   property, so [check_ordering_matches]'s canonical diff would call any reorder
   among them cascade-neutral and miss it; [check_class_order] reads sheet
   positions directly instead. *)
let test_late_typography_before_whitespace () =
  Test_helpers.check_class_order
    ~test_name:"text-wrap, wrap-break-word, hyphens before white-space"
    [
      "whitespace-nowrap";
      "hyphens-auto";
      "text-wrap";
      "wrap-break-word";
      "tracking-wide";
      "text-red-500";
    ]

(* The word-wrapping families overlap on overflow-wrap/word-break: break-normal
   writes both and so leads its shared prefix, break-words/wrap-anywhere/
   wrap-break-word/wrap-normal tie on overflow-wrap alone, and break-all/
   break-keep tie on word-break alone. Two of these seven utilities come from
   [Typography]'s handler and three from [Overflow_wrap]'s - a separate priority
   band - so this also pins their cross-handler tie-break. *)
let test_word_wrap_family_order () =
  Test_helpers.check_class_order
    ~test_name:"break-normal leads, then overflow-wrap ties, then word-break"
    [
      "break-keep";
      "wrap-normal";
      "break-all";
      "wrap-break-word";
      "wrap-anywhere";
      "break-words";
      "break-normal";
    ]

(* Tailwind ranks text-indent (282) right after text-align (281), well before
   letter-spacing (289) opens the rest of the late-typography band. Indent and
   tracking write disjoint properties, so this too needs [check_class_order]
   rather than the cascade-neutral-blind [check_ordering_matches]. *)
let test_indent_before_tracking () =
  Test_helpers.check_class_order
    ~test_name:"text-indent before tracking and text-wrap"
    [ "tracking-wide"; "indent-4"; "text-wrap" ]

(* text-indent (282) sits between text-align (281) and vertical-align (283), so
   indent belongs inside the early-typography band and not after it: font-family
   (284) and font-weight (285) come later. Sorting it before tracking is not
   enough - a band boundary, not a suborder, is what separates it from
   [font-bold]. *)
let test_indent_within_early_typography () =
  Test_helpers.check_class_order
    ~test_name:"text-indent between text-align and vertical-align"
    [ "font-bold"; "align-middle"; "indent-4"; "text-center"; "font-mono" ]

(* Every spelling of the family shares one slot and is separated by the
   candidate name, read the way Tailwind reads it: negatives first, then bare
   steps in numeric order, then the bracket value, then [px]. *)
let test_indent_family_order () =
  Test_helpers.check_class_order ~test_name:"text-indent spellings"
    [
      "indent-px";
      "indent-[3px]";
      "-indent-px";
      "-indent-[3px]";
      "indent-96";
      "indent-8";
      "indent-10";
      "indent-0.5";
      "-indent-4";
    ]

(* Tailwind's property order puts transform-origin before translate. The
   controls that do not participate in that transform chain occupy later bands:
   backface visibility follows selection, perspective follows contain, and
   transform-style follows text-shadow. *)
let test_transform_control_bands () =
  Test_helpers.check_class_order
    ~test_name:"transform controls keep their bands"
    [
      "transform-3d";
      "text-shadow-sm";
      "perspective-normal";
      "backface-hidden";
      "select-none";
      "cursor-pointer";
      "animate-spin";
      "zoom-75";
      "translate-x-4";
      "origin-center";
      "table-fixed";
    ]

(* Tailwind orders transform candidates by sign, axis, then the natural class
   spelling inside each band. Numeric suborders invert negative magnitudes and
   eventually spill a large value into the following axis. *)
let test_transform_candidate_bands () =
  Test_helpers.check_class_order
    ~test_name:"transform candidates keep their sign and axis bands"
    [
      "translate-none";
      "translate-z-12";
      "-translate-z-8";
      "translate-y-20";
      "translate-y-1/2";
      "-translate-y-12";
      "-translate-y-1";
      "translate-x-12";
      "translate-x-1/2";
      "-translate-x-12";
      "-translate-x-1";
      "translate-45";
      "-translate-6";
      "scale-none";
      "scale-y-150";
      "-scale-y-125";
      "scale-x-125";
      "-scale-x-75";
      "scale-150";
      "-scale-125";
      "-scale-100";
      "rotate-z-45";
      "-rotate-z-45";
      "rotate-y-180";
      "-rotate-y-90";
      "rotate-x-90";
      "-rotate-x-90";
      "rotate-none";
      "rotate-225";
      "-rotate-210";
      "-rotate-12";
      "skew-y-12";
      "-skew-y-3";
      "skew-x-12";
      "-skew-x-10";
      "skew-12";
      "-skew-12";
      "-skew-3";
    ]

(* Cursor opens the interaction block, followed by touch, resize, snap,
   scrolling, scrollbar, list, appearance and columns property bands. *)
let test_scrolling_property_bands () =
  Test_helpers.check_class_order
    ~test_name:"scrolling controls keep their property bands"
    [
      "columns-2";
      "appearance-none";
      "list-disc";
      "scrollbar-gutter-stable";
      "scrollbar-thumb-red-500";
      "scrollbar-auto";
      "scroll-p-4";
      "scroll-m-4";
      "snap-always";
      "snap-start";
      "snap-mandatory";
      "snap-x";
      "resize";
      "touch-auto";
      "touch-pan-x";
      "cursor-pointer";
    ]

(* Columns and break controls open the flow block; grid/flex/alignment/gap and
   divide follow before self-alignment, overflow and scroll behavior. *)
let test_flow_property_bands () =
  Test_helpers.check_class_order ~test_name:"flow controls keep their bands"
    [
      "rounded-lg";
      "scroll-smooth";
      "overscroll-contain";
      "overflow-hidden";
      "place-self-center";
      "divide-red-500";
      "divide-dashed";
      "divide-y-reverse";
      "divide-x-2";
      "gap-4";
      "justify-center";
      "items-center";
      "place-content-center";
      "flex-wrap";
      "flex-row";
      "grid-cols-3";
      "grid-flow-col";
      "auto-cols-fr";
      "break-after-page";
      "break-inside-avoid";
      "break-before-page";
      "columns-2";
    ]

(* Tab size follows white-space and precedes text color and transforms. *)
let test_tab_property_band () =
  Test_helpers.check_class_order ~test_name:"tab size keeps its property band"
    [
      "uppercase";
      "text-red-500";
      "tab-4";
      "whitespace-nowrap";
      "hyphens-auto";
      "text-ellipsis";
      "break-all";
      "break-words";
      "text-wrap";
    ]

let test_field_sizing_property_band () =
  Test_helpers.check_class_order
    ~test_name:"field sizing keeps its property band"
    [
      "cursor-pointer";
      "w-4";
      "field-sizing-content";
      "grid";
      "flex";
      "block";
      "line-clamp-2";
      "box-border";
      "m-4";
    ]

let test_late_control_property_bands () =
  Test_helpers.check_class_order
    ~test_name:"late controls keep their property bands"
    [
      "transform-3d";
      "text-shadow-sm";
      "perspective-normal";
      "backface-hidden";
      "select-none";
      "outline-solid";
      "forced-color-adjust-auto";
      "content-none";
      "contain-layout";
      "will-change-auto";
      "transition";
      "blur-sm";
      "outline-hidden";
      "scheme-dark";
    ]

(* The outline-style, user-select and alpha-bearing text-shadow candidates form
   three consecutive bands after backdrop-filter. The ordinary text-shadow
   shapes still live in their later text-shadow band. *)
let test_outline_select_shadow_property_bands () =
  Test_helpers.check_class_order
    ~test_name:"outline, select and alpha text-shadow property bands"
    [
      "text-shadow-sm";
      "transform-3d";
      "perspective-normal";
      "divide-x-reverse";
      "[hanging-punctuation:first_last]";
      "[animation-name:move-x]";
      "backface-hidden";
      "text-shadow-lg/20";
      "select-none";
      "select-all";
      "outline-solid";
      "outline-dashed";
      "backdrop-filter-none";
      "filter-none";
      "outline-inherit";
    ]

(* Backface visibility and divide-x-reverse precede the logical sizing block;
   perspective follows it. *)
let test_backface_logical_sizing_boundary () =
  Test_helpers.check_class_order ~test_name:"backface/logical sizing boundary"
    [
      "perspective-normal";
      "max-block-full";
      "inline-full";
      "divide-x-reverse";
      "backface-visible";
      "[animation-name:move-x]";
      "text-shadow-lg/20";
    ]

let test_logical_side_property_bands () =
  Test_helpers.check_class_order
    ~test_name:"logical sides keep their property bands"
    [
      "pl-3";
      "pb-6";
      "pr-5";
      "pt-1";
      "pbe-2";
      "pbs-2";
      "pe-2";
      "ps-2";
      "py-4";
      "px-4";
      "p-4";
      "border-spacing-2";
      "border-collapse";
      "caption-bottom";
      "table-fixed";
      "left-1/2";
      "bottom-0";
      "right-0";
      "top-0";
      "end-0";
      "start-0";
      "inset-y-0";
      "inset-x-0";
      "inset-0";
    ]

let test_shadow_and_transform_boundaries () =
  Test_helpers.check_class_order
    ~test_name:"shadow and transform boundaries keep their property bands"
    [
      "transform-3d";
      "text-shadow-sm";
      "perspective-normal";
      "divide-x-reverse";
      "backface-hidden";
      "select-none";
      "outline-hidden";
      "ring-inset";
      "ring-offset-red-500";
      "ring-offset-2";
      "inset-ring-red-500";
      "inset-shadow-red-500";
      "ring-red-500";
      "shadow-red-500";
      "inset-ring-2";
      "inset-shadow-sm";
      "ring-2";
      "shadow-sm";
    ]

let test_late_typography_property_bands () =
  Test_helpers.check_class_order
    ~test_name:"late typography keeps its property bands"
    [
      "antialiased";
      "underline-offset-4";
      "decoration-2";
      "decoration-dashed";
      "decoration-red-500";
      "underline";
      "line-through";
      "tabular-nums";
      "ordinal";
      "font-stretch-expanded";
      "italic";
    ]

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
        [
          form_input;
          form_checkbox;
          form_radio;
          form_select;
          form_textarea;
          form_multiselect;
        ] );
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

(* Test 3: Random utilities with minimization.

   The pool is keyed by the lib/ module that exports its entries, and
   [test_pool_covers_every_family] reads tw.ml's include list back and fails on
   a module with nothing under it. A hand-written list nothing checks drifts:
   the one this replaces reached 103 constructors of tw.mli's 1030, and no seed
   could compile a [hover:] or an [md:] rule at all, modifiers exporting 210
   constructors the pool never touched. *)

(* An entry is written as the class a user types, which is what the reader takes
   back, so a typo fails inside a test rather than at module initialisation. The
   families with no reader are built from their constructor instead. *)
let cls name =
  match Tw.of_string name with
  | Ok u -> u
  | Error (`Msg m) -> Alcotest.failf "pool entry %S does not parse: %s" name m

let pool_families () =
  let c names = List.map cls names in
  [
    ( "accessibility",
      c [ "forced-color-adjust-none"; "forced-color-adjust-auto" ] );
    ( "alignment",
      c
        [
          "items-center";
          "items-start";
          "justify-between";
          "justify-center";
          "self-end";
          "content-around";
          "place-items-center";
          "place-content-between";
          "justify-items-start";
          "justify-self-end";
          "place-self-center";
        ] );
    ("animations", c [ "animate-spin"; "animate-pulse"; "animate-none" ]);
    (* [[property:value]] takes any property, so it sorts into whichever band
       the property it declares belongs to. *)
    ("arbitrary", c [ "[color:red]"; "[mask-type:luminance]"; "[order:3]" ]);
    ( "backgrounds",
      c
        [
          "bg-red-500";
          "bg-white";
          "bg-cover";
          "bg-center";
          "bg-no-repeat";
          "bg-fixed";
          "bg-clip-text";
          "bg-origin-content";
          "bg-linear-to-r";
          "from-blue-500";
          "via-purple-500";
          "to-pink-500";
        ] );
    ( "borders",
      c
        [
          "border";
          "border-2";
          "border-t-4";
          "border-x-2";
          "border-dashed";
          "rounded-lg";
          "rounded-t-md";
          "rounded-full";
          "outline-2";
          "outline-dashed";
          "outline-offset-2";
        ] );
    ("box_sizing", c [ "box-border"; "box-content" ]);
    ( "color",
      c
        [
          "text-red-500";
          "text-white";
          "accent-pink-500";
          "caret-blue-500";
          "placeholder-gray-400";
        ] );
    ("columns", c [ "columns-3"; "columns-md"; "columns-auto" ]);
    ("contain", c [ "contain-layout"; "contain-strict"; "contain-content" ]);
    ("containers", c [ "container"; "@container" ]);
    ("cursor", c [ "cursor-pointer"; "cursor-not-allowed"; "cursor-grab" ]);
    ( "divide",
      c
        [
          "divide-x";
          "divide-y-2";
          "divide-gray-300";
          "divide-dashed";
          "divide-x-reverse";
        ] );
    ( "effects",
      c
        [
          "shadow-md";
          "shadow-none";
          "opacity-50";
          "mix-blend-multiply";
          "bg-blend-multiply";
          "inset-shadow-sm";
          "ring-2";
          "ring-offset-4";
          "inset-ring-2";
        ] );
    ("field_sizing", c [ "field-sizing-content"; "field-sizing-fixed" ]);
    ( "filters",
      c
        [
          "filter-none";
          "blur-sm";
          "brightness-125";
          "grayscale";
          "hue-rotate-90";
          "invert";
          "backdrop-blur-md";
          "backdrop-saturate-150";
          "drop-shadow-lg";
        ] );
    ("flex", c [ "flex"; "inline-flex" ]);
    ( "flex_layout",
      c
        [
          "flex-row";
          "flex-col";
          "flex-row-reverse";
          "flex-col-reverse";
          "flex-wrap";
          "flex-nowrap";
        ] );
    ( "flex_props",
      c
        [
          "flex-1";
          "flex-auto";
          "grow";
          "shrink-0";
          "basis-1/2";
          "order-2";
          "order-first";
        ] );
    ("gap", c [ "gap-4"; "gap-x-2"; "gap-y-8" ]);
    ("grid", c [ "grid"; "inline-grid" ]);
    ( "grid_item",
      c [ "col-span-2"; "row-span-3"; "col-start-2"; "row-end-4"; "col-auto" ]
    );
    ( "grid_template",
      c
        [
          "grid-cols-3";
          "grid-cols-none";
          "grid-rows-2";
          "grid-rows-subgrid";
          "grid-flow-col";
          "auto-cols-fr";
          "auto-rows-min";
        ] );
    ( "interactivity",
      c
        [
          "select-none";
          "resize-y";
          "pointer-events-none";
          "appearance-none";
          "will-change-transform";
          "snap-center";
          "snap-x";
          "scroll-smooth";
          "scheme-dark";
          "group";
          "peer";
        ] );
    ( "layout",
      c
        [
          "block";
          "inline-block";
          "hidden";
          "table";
          "contents";
          "float-left";
          "clear-both";
          "isolate";
          "object-cover";
          "object-center";
          "z-10";
          "break-after-page";
          "box-decoration-clone";
          "sr-only";
          "not-sr-only";
        ] );
    ( "margin",
      c [ "m-4"; "mx-2"; "my-8"; "mt-1"; "mb-6"; "ml-auto"; "-mt-4"; "ms-2" ] );
    ( "mask_gradient",
      c
        [
          "mask-t-from-50%";
          "mask-r-to-90%";
          "mask-radial-from-20%";
          "mask-conic-from-30%";
          "mask-linear-from-40%";
          "mask-x-from-10%";
        ] );
    ( "masks",
      c
        [
          "mask-none";
          "mask-alpha";
          "mask-cover";
          "mask-center";
          "mask-repeat-x";
          "mask-clip-content";
          "mask-origin-padding";
          "mask-add";
          "mask-type-luminance";
        ] );
    ("overflow", c [ "overflow-hidden"; "overflow-x-auto"; "overflow-y-scroll" ]);
    ("overflow_wrap", c [ "wrap-break-word"; "wrap-anywhere"; "wrap-normal" ]);
    ( "overscroll",
      c [ "overscroll-contain"; "overscroll-x-none"; "overscroll-y-auto" ] );
    ( "padding",
      c [ "p-4"; "px-2"; "py-8"; "pt-1"; "pb-6"; "pl-3"; "pr-5"; "ps-2" ] );
    ( "position",
      c
        [
          "relative";
          "absolute";
          "sticky";
          "static";
          "top-0";
          "inset-x-4";
          "left-1/2";
          "start-0";
          "inset-0";
        ] );
    ("scroll", c [ "scroll-m-4"; "scroll-px-2"; "scroll-mt-8"; "scroll-pb-6" ]);
    ( "scrollbar",
      c
        [
          "scrollbar-thin";
          "scrollbar-gutter-stable";
          "scrollbar-thumb-gray-400";
          "scrollbar-track-gray-100";
        ] );
    ( "sizing",
      c
        [
          "w-4";
          "w-full";
          "h-8";
          "h-screen";
          "min-w-0";
          "max-w-4xl";
          "max-h-96";
          "size-10";
          "aspect-video";
          "aspect-square";
          "aspect-auto";
          "aspect-[4/3]";
        ] );
    ("svg", c [ "fill-none"; "fill-red-500"; "stroke-current"; "stroke-2" ]);
    ("tab", c [ "tab-4"; "tab-2" ]);
    ( "tables",
      c
        [
          "border-collapse"; "border-spacing-2"; "table-fixed"; "caption-bottom";
        ] );
    ("text_shadow", c [ "text-shadow-sm"; "text-shadow-lg"; "text-shadow-none" ]);
    ("touch", c [ "touch-pan-x"; "touch-manipulation"; "touch-none" ]);
    ( "transforms",
      c
        [
          "rotate-45";
          "scale-95";
          "translate-x-4";
          "skew-y-3";
          "origin-top-left";
          "transform-gpu";
          "transform-none";
          "transform-3d";
          "perspective-dramatic";
          "backface-hidden";
          "rotate-x-30";
          "translate-z-4";
        ] );
    ( "transitions",
      c
        [
          "transition";
          "transition-colors";
          "duration-300";
          "ease-in-out";
          "delay-150";
        ] );
    ( "typography",
      c
        [
          "text-xs";
          "text-2xl";
          "font-bold";
          "font-sans";
          "leading-relaxed";
          "tracking-wide";
          "italic";
          "uppercase";
          "underline";
          "line-through";
          "text-ellipsis";
          "indent-4";
          "align-middle";
          "whitespace-nowrap";
          "list-disc";
          "line-clamp-3";
          "antialiased";
          "tabular-nums";
          "hyphens-auto";
          "decoration-2";
          "decoration-sky-400";
          "underline-offset-4";
        ] );
    ("zoom", c [ "zoom-50"; "zoom-100" ]);
  ]

(* What a variant may sit next to in a stack.

   [Responsive] nests a media query, and two of those on one class is a spelling
   Tailwind refuses, so a stack takes at most one and it goes outermost. [Hover]
   nests [\@media (hover: hover)], and a stack takes at most one of those too:
   Tailwind nests the query once per hover-gated variant and tw emits it once,
   so [group-hover:hover:x] differs by a redundant wrapper.

   [Innermost] may not wrap another variant. [starting] is here because
   [starting:hover:x] loses the [@media (hover:hover)] the inner variant asked
   for, a bug of its own rather than an ordering question. [in-[...]] is out of
   the list altogether: composed with anything - an inner variant, or a utility
   carrying its own pseudo-element - it wraps the ancestor selector in [:is()]
   where Tailwind appends to that selector's last compound, which is the same
   CSS spelled differently and reads as a difference every time.

   [Element] is [Innermost] and ends the compound with a pseudo-element, which
   nothing may follow: Tailwind writes [marker:last:x] as [::marker:last-child],
   which no reader accepts, and its own minifier then empties what it cannot
   parse. It also needs a utility whose rules stop at the element; see
   {!reaches_past_the_element}. *)
type variant_kind = Plain | Responsive | Hover | Innermost | Element

(* modifiers exports 210 constructors and no utility of its own, so a seed
   reaches the family only by dressing one: this list is what puts a [hover:]
   rule or an [@media] block in front of the comparator at all. Written as the
   prefix rather than as the constructor, since tw.mli re-exports only about a
   fifth of them and the reader takes every spelling. *)
let pool_variants =
  [
    ("hover", Hover);
    ("focus", Plain);
    ("active", Plain);
    ("disabled", Plain);
    ("focus-within", Plain);
    ("focus-visible", Plain);
    ("first", Plain);
    ("last", Plain);
    ("only", Plain);
    ("odd", Plain);
    ("even", Plain);
    ("first-of-type", Plain);
    ("last-of-type", Plain);
    ("empty", Plain);
    ("checked", Plain);
    ("indeterminate", Plain);
    ("default", Plain);
    ("required", Plain);
    ("valid", Plain);
    ("invalid", Plain);
    ("in-range", Plain);
    ("placeholder-shown", Plain);
    ("autofill", Plain);
    ("read-only", Plain);
    ("optional", Plain);
    ("open", Plain);
    ("enabled", Plain);
    ("target", Plain);
    ("visited", Plain);
    ("inert", Plain);
    ("user-valid", Plain);
    ("before", Element);
    ("after", Element);
    ("marker", Element);
    ("selection", Element);
    ("placeholder", Element);
    ("backdrop", Element);
    ("file", Element);
    ("first-letter", Element);
    ("first-line", Element);
    ("details-content", Element);
    ("*", Plain);
    ("**", Plain);
    ("group-hover", Hover);
    ("group-focus", Plain);
    ("group-first", Plain);
    ("group-last", Plain);
    ("group-odd", Plain);
    ("group-open", Plain);
    ("group-checked", Plain);
    ("group-disabled", Plain);
    ("group-focus-within", Plain);
    ("peer-hover", Hover);
    ("peer-focus", Plain);
    ("peer-checked", Plain);
    ("peer-disabled", Plain);
    ("peer-first", Plain);
    ("peer-last", Plain);
    ("peer-invalid", Plain);
    ("aria-checked", Plain);
    ("aria-expanded", Plain);
    ("aria-selected", Plain);
    ("aria-disabled", Plain);
    ("data-[state=open]", Plain);
    ("data-active", Plain);
    ("data-inactive", Plain);
    ("nth-3", Plain);
    ("nth-last-2", Plain);
    (* A class rather than a type selector: Tailwind wraps a [has-[...]] bracket
       in [:is()] and tw does not, which is a difference of spelling alone and
       one the canonical differ folds away for every argument but a bare type
       selector. Kept out of the pool so the fuzzer reports order rather than
       that. *)
    ("has-[.x]", Plain);
    ("group-has-[.x]", Plain);
    ("peer-has-[.x]", Plain);
    ("not-[:hover]", Plain);
    ("ltr", Plain);
    ("rtl", Plain);
    ("starting", Innermost);
    ("sm", Responsive);
    ("md", Responsive);
    ("lg", Responsive);
    ("xl", Responsive);
    ("2xl", Responsive);
    ("max-sm", Responsive);
    ("max-md", Responsive);
    ("max-lg", Responsive);
    ("min-[600px]", Responsive);
    ("max-[900px]", Responsive);
    ("dark", Responsive);
    ("motion-safe", Responsive);
    ("motion-reduce", Responsive);
    ("contrast-more", Responsive);
    ("contrast-less", Responsive);
    ("print", Responsive);
    ("portrait", Responsive);
    ("landscape", Responsive);
    ("forced-colors", Responsive);
    ("supports-[display:grid]", Responsive);
  ]

let pick_variant rng kinds =
  let candidates = List.filter (fun (_, k) -> List.mem k kinds) pool_variants in
  List.nth candidates (Random.State.int rng (List.length candidates))

(* A variant is applied by spelling it onto the class, which is the one form
   every one of them has; [clip-*] has no reader and stays bare, and any other
   refusal is a finding rather than a case to skip. *)
let prefix name u =
  let bare = Tw.pp u in
  let dressed = name ^ ":" ^ bare in
  match Tw.of_string dressed with
  | Ok v -> v
  | Error (`Msg m) ->
      if Result.is_error (Tw.of_string bare) then u
      else Alcotest.failf "%S does not read back: %s" dressed m

(* How much of a variant a utility can wear. Every restriction here is a bug or
   a gap of its own rather than an ordering question, which is what this fuzzer
   is for, and each is named with why. *)
type dressing =
  | Any
  | No_pseudo_element
      (** Its rules reach past the element they are named for - a descendant, a
          child, or a pseudo-element of its own - so a pseudo-element variant
          cannot dress it. [before:divide-x] would have to mean
          [::before > :not(:last-child)] and tw writes [::before] alone,
          dropping the child part; [details-content:placeholder-gray-400] puts
          two pseudo-elements in one compound, which no reader accepts. *)
  | Bare
      (** It emits a companion block - an [@supports] beside the rule, or a
          nested [@media] - and a variant puts each in a [@media] of its own.
          Tailwind merges consecutive blocks and cascade does not (CLAUDE.md
          section 8), so the split reads as a missing declaration rather than as
          the merge gap it is. *)

let dressing_of cls =
  let has prefixes =
    List.exists (fun p -> Astring.String.is_prefix ~affix:p cls) prefixes
  in
  if has [ "bg-linear-to-"; "container"; "@container" ] then Bare
  else if has [ "divide-"; "space-"; "placeholder-"; "group"; "peer" ] then
    No_pseudo_element
  else Any

(* How much of a draw wears a variant. All of it would compare one media block
   against another and never a plain rule against one, and none of it is where
   the pool was. *)
let dress rng ~dressing u =
  let alone =
    match dressing with
    | Any -> [ Plain; Responsive; Hover; Innermost; Element ]
    | No_pseudo_element -> [ Plain; Responsive; Hover; Innermost ]
    | Bare -> []
  in
  let innermost =
    match dressing with
    | Any -> [ Plain; Element ]
    | No_pseudo_element -> [ Plain ]
    | Bare -> []
  in
  if alone = [] then ("", u)
  else
    match Random.State.int rng 6 with
    | 0 | 1 | 2 -> ("", u)
    | 3 | 4 ->
        let name, _ = pick_variant rng alone in
        (name, prefix name u)
    | _ ->
        (* Stacked: the pseudo-element innermost, since it ends the selector,
           and the media query outermost, which is how [md:hover:] is written.
           At most one hover-gated variant, wherever it lands. *)
        let outer, outer_kind = pick_variant rng [ Plain; Responsive; Hover ] in
        let inner_kinds =
          if outer_kind = Hover then innermost else Hover :: innermost
        in
        let inner, _ = pick_variant rng inner_kinds in
        (outer ^ ":" ^ inner, prefix outer (prefix inner u))

(* Without replacement. Drawing 30 from 252 with replacement left about 28
   distinct, and each duplicate cost a comparison and covered nothing. *)
let draw rng n entries =
  let arr = Array.of_list entries in
  let len = Array.length arr in
  for i = len - 1 downto 1 do
    let j = Random.State.int rng (i + 1) in
    let t = arr.(i) in
    arr.(i) <- arr.(j);
    arr.(j) <- t
  done;
  Array.to_list (Array.sub arr 0 (min n len))

let sample_size = 30
let samples = 6

(* An entry the fuzzer draws. [handler] is the registered handler that claims
   the undressed class, which is the key {!known_inversions} is written in;
   dressing the utility in a variant does not change it. *)
type entry = {
  utility : Tw.t;
  handler : string;
  forms : bool;
  dressing : dressing;
  variant : string;  (** the prefixes it was dressed in, [""] undressed *)
}

(* Which handler a class resolves to. Finer than the lib/ module a family is
   keyed by: typography answers early or late, borders answers outline_style for
   its outline utilities. *)
let handler_of cls =
  match Tw.Utility.base_of_class Tw.Scheme.default cls with
  | Ok base -> Some (Tw.Utility.name_of_base base)
  | Error (`Msg _) -> None

let pool_entries () =
  List.concat_map
    (fun (family, us) ->
      List.map
        (fun u ->
          {
            utility = u;
            handler =
              (match handler_of (Tw.pp u) with Some h -> h | None -> family);
            forms = String.equal family "forms";
            dressing = dressing_of (Tw.pp u);
            variant = "";
          })
        us)
    (pool_families ())

(* Handler pairs tw orders the other way round from Tailwind. [(a, b)] reads:
   Tailwind emits some a-utility before some b-utility, and tw emits it after.
   Both directions can hold between one pair of handlers, since a family may
   straddle two of Tailwind's bands, so each is its own entry.

   This is the ordering debt the whole-sheet gate counts - 299 of 3885
   statements in test/parity/sheet_order.ml - named rather than counted. Without
   it the fuzzer fails on nearly every seed for misorderings that predate it;
   with it a pair outside the list fails a draw.
   [test_known_inversions_are_exact] measures the set over the whole pool and
   fails when it grows and when one of these is fixed, so the list can only
   shrink, and closing a bug is what forces an entry out of it. Both oracles
   consult it: the byte positions {!Test_helpers.inverted_pairs} reads, and the
   canonical differ, which reports a reorder of its own whenever the two rules
   are cascade-significant.

   A pair rather than a family: 18 families explain all 188 entries, and naming
   the families instead would tolerate every disagreement either of them has,
   including one against a family that is itself correctly placed. A pair is
   also a property of the two classes and nothing else - both sheets order their
   utilities by a key of their own - so it is measurable once and means the same
   in every draw.

   What a listed pair does not catch is a further inversion between those same
   two handlers. The handler is as fine as the key gets, and a family spanning
   two of Tailwind's bands answers one name for both. That is still far finer
   than a count of statements, which tolerates any 299 of them. *)
let known_inversions = [ ("masks", "arbitrary") ]

(* Which handler each class in a case belongs to, and which variant it wears.
   The variant matters because two classes wearing different ones are not
   comparable here: a variant changes the sort key by design, and where Tailwind
   puts [hover:a] relative to [md:b] is its variant order rather than the
   utility order this measures. The differ still reads that pair. *)
let case_index cases =
  let tbl = Hashtbl.create (List.length cases) in
  List.iter
    (fun e ->
      let cls = Tw.pp e.utility in
      if not (Hashtbl.mem tbl cls) then
        Hashtbl.add tbl cls (e.handler, e.variant))
    cases;
  tbl

(* Is every change in the differ's tree a reorder of whole rules? The differ
   answers what kind of difference there is; which pairs disagree is
   {!Test_helpers.inverted_pairs}'s question, and it is the one that can be
   recorded. Splitting them that way is deliberate: the differ picks which of
   several rotated rules to call moved from the context it is given, so a pair
   it names in a three-class draw is not the pair it names over the pool, while
   two byte positions read the same wherever they are read.

   A [Reordered] node with no [swapped_with] is a reorder inside one rule rather
   than of the rules, and is not this. *)
let rule_is_reorder (r : Cascade_diff.Tree_diff.rule_diff) =
  match r with
  | Cascade_diff.Tree_diff.Reordered { swapped_with = Some _; _ } -> true
  | _ -> false

let rec container_is_reorder (c : Cascade_diff.Tree_diff.container_diff) =
  match c with
  | Cascade_diff.Tree_diff.Modified { rule_changes; container_changes; _ } ->
      List.for_all rule_is_reorder rule_changes
      && List.for_all container_is_reorder container_changes
  | _ -> false

let only_reorders (d : Cascade_diff.Tree_diff.t) =
  d.layer_order = None
  && List.for_all rule_is_reorder d.rules
  && List.for_all container_is_reorder d.containers

(* One reading of a case, for both minimisation and the assertion: two
   predicates that disagree let a fuzzer print a failing case and pass anyway.
   The canonical differ answers what the two sheets declare differently, and the
   pairwise order answers what they emit in a different order, which the differ
   folds away whenever the two utilities write disjoint properties. Both consult
   [known_inversions]. *)
let case_faults cases =
  let utilities = List.map (fun e -> e.utility) cases in
  let forms = List.exists (fun e -> e.forms) cases in
  let tailwind, tw = Test_helpers.sheets ~forms utilities in
  let index = case_index cases in
  let entry cls = Hashtbl.find_opt index cls in
  let diff = Test_helpers.sheet_diff ~tailwind ~tw in
  let dropped = Test_helpers.dropped_declarations diff in
  let unread =
    if dropped = [] then [] else [ Test_helpers.describe_dropped dropped ]
  in
  let unrecorded =
    Test_helpers.inverted_pairs ~tailwind ~tw (List.map Tw.pp utilities)
    |> List.filter_map (fun (a, b) ->
        match (entry a, entry b) with
        | Some (ha, va), Some (hb, vb)
          when String.equal va vb && not (List.mem (ha, hb) known_inversions) ->
            Fmt.kstr
              (fun m -> Some m)
              "%s comes before %s in Tailwind and after it in tw. Fix the \
               order, or record (%S, %S) in [known_inversions] if it is debt \
               this branch does not close."
              a b ha hb
        | _ -> None)
  in
  let structural =
    match diff.Cascade_diff.Css_compare.result with
    | Cascade_diff.Css_compare.No_diff -> []
    (* Only order differs, and every pair that disagrees is recorded. *)
    | Cascade_diff.Css_compare.Tree_diff d
      when only_reorders d && unrecorded = [] ->
        []
    | _ -> [ Test_helpers.describe_diff diff ]
  in
  unread @ structural @ unrecorded

let test_random_utilities_with_minimization () =
  let entries = pool_entries () in
  let rng = Test_helpers.test_rng in
  let sample i =
    let initial =
      List.map
        (fun e ->
          let variant, utility = dress rng ~dressing:e.dressing e.utility in
          { e with utility; variant })
        (draw rng sample_size entries)
    in
    Fmt.epr "Sample %d/%d: %d utilities@." i samples (List.length initial);
    match
      Test_helpers.minimize_failing_case
        (fun cases -> case_faults cases <> [])
        initial
    with
    | None -> ()
    | Some final -> (
        Fmt.epr "@.Minimal failing case (%d utilities): %a@."
          (List.length final)
          Fmt.(list ~sep:(const string " ") string)
          (List.map (fun e -> Tw.pp e.utility) final);
        match case_faults final with
        | [] -> ()
        | faults ->
            Alcotest.failf "random utilities ordering matches Tailwind\n%s"
              (String.concat "\n" faults))
  in
  for i = 1 to samples do
    sample i
  done

(* The set {!known_inversions} records, measured over the whole pool at once.
   Measured rather than collected from failing seeds: every pair the fuzzer can
   draw is in this one comparison, so the list describes the debt instead of
   whichever part of it a seed happened to hit. *)
let measured_inversions () =
  let entries = pool_entries () in
  let utilities = List.map (fun e -> e.utility) entries in
  let tailwind, tw = Test_helpers.sheets ~forms:true utilities in
  let index = case_index entries in
  let handler cls =
    match Hashtbl.find_opt index cls with Some (h, _) -> h | None -> cls
  in
  let from_positions =
    Test_helpers.inverted_pairs ~tailwind ~tw (List.map Tw.pp utilities)
    |> List.map (fun (a, b) -> (handler a, handler b))
  in
  List.sort_uniq compare from_positions

let test_mask_type_arbitrary_order () =
  Test_helpers.check_class_order ~test_name:"arbitrary mask-type order"
    [
      "mask-auto";
      "mask-type-luminance";
      "[mask-type:luminance]";
      "mask-type-alpha";
    ]

let test_known_inversions_are_exact () =
  let measured = measured_inversions () in
  let recorded = List.sort_uniq compare known_inversions in
  let show pairs =
    String.concat "\n"
      (List.map (fun (a, b) -> Fmt.str "    (%S, %S);" a b) pairs)
  in
  let unrecorded = List.filter (fun p -> not (List.mem p recorded)) measured in
  if unrecorded <> [] then
    Alcotest.failf
      "%d handler pair(s) are out of Tailwind's order and not recorded:\n\
       %s\n\
       Fix the order, or add them to [known_inversions] in test/test_sort.ml."
      (List.length unrecorded) (show unrecorded);
  let closed = List.filter (fun p -> not (List.mem p measured)) recorded in
  if closed <> [] then
    Alcotest.failf
      "%d recorded pair(s) are in Tailwind's order now:\n\
       %s\n\
       Delete them from [known_inversions] in test/test_sort.ml. The list is a \
       ratchet: it only shrinks."
      (List.length closed) (show closed)

(* Every module tw.ml includes exports utilities, and one with no pool entry is
   a family no seed can reach. Reading the include list back is what makes a new
   family a failure rather than a hole nobody thinks to look for. *)
let rec dir_containing name dir =
  if Sys.file_exists (Filename.concat dir name) then Some dir
  else
    let parent = Filename.dirname dir in
    if String.equal parent dir then None else dir_containing name parent

let included_modules () =
  let path = "lib/tw.ml" in
  let root =
    match dir_containing path (Sys.getcwd ()) with
    | Some r -> r
    | None -> Alcotest.failf "cannot find %s from %s" path (Sys.getcwd ())
  in
  let ic = open_in_bin (Filename.concat root path) in
  let text =
    Fun.protect
      ~finally:(fun () -> close_in_noerr ic)
      (fun () -> really_input_string ic (in_channel_length ic))
  in
  String.split_on_char '\n' text
  |> List.filter_map (fun line ->
      match Astring.String.cut ~sep:"include " (String.trim line) with
      | Some ("", m) when m <> "" && m.[0] >= 'A' && m.[0] <= 'Z' ->
          Some (String.uncapitalize_ascii m)
      | _ -> None)
  |> List.sort_uniq String.compare

(* A family whose utilities cannot go through the comparator, and why. Never a
   silent omission: the coverage check subtracts this list and nothing else. *)
let unfuzzable =
  [
    ( "modifiers",
      "exports variants rather than utilities; [pool_variants] carries it, and \
       [test_variants_all_compile] checks that list" );
    ( "forms",
      "drawn beside [ring-offset-*] it emits a property rule for \
       [--tw-ring-offset-shadow] and a properties layer Tailwind does not, \
       neither utility doing so on its own: the plugin's own writes register \
       as setting the variable where Tailwind's do not. A bug about which \
       variables need a property rule, not about order" );
    ( "prose",
      "emits a block of about ninety descendant rules, and where another \
       utility falls inside that block is not a question about a pair of \
       classes: the two sheets order prose's own rules differently, so pairing \
       them by position says nothing and only the differ sees it. Covered by \
       test_prose.ml" );
    ( "clipping",
      "[clip_polygon] names its class [clip-[polygon(...)]], which Tailwind \
       does not read and emits nothing for, and which tw's own reader rejects \
       too: there is no comparison to make" );
  ]

let test_pool_covers_every_family () =
  let covered = List.map fst (pool_families ()) in
  let expected = included_modules () in
  (* A read that returned nothing would report every family as covered. *)
  if List.length expected < 40 then
    Alcotest.failf "read only %d includes out of lib/tw.ml, expected the lot"
      (List.length expected);
  let missing =
    List.filter
      (fun m -> (not (List.mem m covered)) && not (List.mem_assoc m unfuzzable))
      expected
  in
  if missing <> [] then
    Alcotest.failf
      "the fuzzer pool has no entry for %d of tw.ml's families: %s.\n\
       Add utilities under that key in [pool_families], or list the family in \
       [unfuzzable] with why it cannot go through the comparator."
      (List.length missing)
      (String.concat ", " missing);
  let stale = List.filter (fun m -> not (List.mem m expected)) covered in
  if stale <> [] then
    Alcotest.failf "the pool is keyed by %s, which tw.ml no longer includes"
      (String.concat ", " stale)

(* The module list above is what tw.ml says exists; this is what the registry
   says. A family can be keyed correctly and still hold nothing of its own -
   [flex] carried six [flex-row]-style classes, every one of them
   [flex_layout]'s - and only the handler each class actually resolves to sees
   that. Each registered handler offers a few of its own utilities as
   {!Tw.Utility.examples_classes}, so the expected set comes out of the registry
   rather than out of a second hand-written list. *)
let test_pool_covers_every_handler () =
  let names classes = List.filter_map handler_of classes in
  let covered =
    names
      (List.concat_map (fun (_, us) -> List.map Tw.pp us) (pool_families ()))
  in
  let expected =
    List.sort_uniq String.compare (names (Tw.Utility.examples_classes ()))
  in
  if List.length expected < 40 then
    Alcotest.failf "the registry offered only %d handlers to compare against"
      (List.length expected);
  let missing = List.filter (fun h -> not (List.mem h covered)) expected in
  if missing <> [] then
    Alcotest.failf
      "no pool entry resolves to %d registered handler(s): %s.\n\
       A class keyed under one family but claimed by another leaves the family \
       it is keyed under with nothing of its own."
      (List.length missing)
      (String.concat ", " missing)

(* Every variant has to read back onto a class and to change it, or a draw that
   reaches one fails on the spelling rather than on the order. *)
let test_variants_all_compile () =
  List.iter
    (fun (name, _) ->
      let u = prefix name (cls "p-4") in
      if Tw.pp u = "p-4" then
        Alcotest.failf "variant %s left the class it wraps unchanged" name;
      ignore (Tw.to_css ~base:false [ u ]))
    pool_variants

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

(* Tailwind sorts a container variant on the text of its value, not on the width
   that text resolves to: the key is the value's unit, or the name before the
   parenthesis when the value is a call. A [theme(--breakpoint-sm)] bracket
   therefore sorts under "theme", after every px and rem width, while the 40rem
   it resolves to would place it among them. Two overlapping conditions setting
   the same property pick different winners under the two orders. *)
let test_container_query_call_order () =
  Test_helpers.check_class_order ~test_name:"container call sorts by its name"
    [
      "@min-[100px]:inline";
      "@min-[2rem]:table";
      "@lg:grid";
      "@min-[64rem]:block";
      "@min-[theme(--breakpoint-sm)]:flex";
    ]

(* The suborder a rule sorts on carries more than the utility's own number: a
   [before:]/[after:] class has 5000 added to it, and a rule built by one of the
   negation, ancestor, has-, aria- or data- handlers has that handler's offset
   added. Both offsets are common to every rule the comparator can reach them
   with - it only reads the suborder for two rules whose whole variant prefix is
   equal - so they cancel, and this pins the order they were meant to produce
   across the families that carry them. *)
let test_variant_family_order () =
  Test_helpers.check_class_order ~test_name:"variant families sort as Tailwind"
    [
      "underline";
      "before:underline";
      "after:underline";
      "has-checked:underline";
      "has-[>img]:underline";
      "aria-checked:underline";
      "aria-[sort=none]:underline";
      "data-open:underline";
      "in-data-open:underline";
      "in-[.foo]:underline";
      "not-hover:underline";
      "not-first:underline";
      "not-in-data-open:underline";
      "group-not-[:checked]:underline";
      "peer-not-checked:underline";
    ]

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

let test_sizing_values_natural_order () =
  let classes =
    [
      "w-(--w)";
      "w-2";
      "w-2xl";
      "w-10";
      "w-[50%]";
      "w-auto";
      "max-w-(--breakpoint-md)";
      "max-w-2xl";
      "max-w-10";
      "max-w-[50%]";
      "max-w-sm";
      "basis-(--basis)";
      "basis-2";
      "basis-2xl";
      "basis-10";
      "basis-[50%]";
      "basis-auto";
    ]
  in
  let utilities = List.map (fun c -> Result.get_ok (Tw.of_string c)) classes in
  Test_helpers.check_ordering_matches
    ~test_name:"sizing values use natural candidate order" utilities

let test_bracket_value_holding_a_colon () =
  (* An arbitrary value can hold a colon, so the variant prefix has to be taken
     with the modifier parser: splitting on the last ':' reads the prefix of
     hover:bg-[color:var(--x)] as "hover:bg-[color", which sorts it away from
     the other hover: rules. *)
  let classes =
    [
      "hover:bg-[color:var(--x)]";
      "hover:bg-red-500";
      "bg-blue-500";
      "focus:bg-[color:var(--y)]";
      "hover:m-2";
    ]
  in
  let utilities = List.map (fun c -> Result.get_ok (Tw.of_string c)) classes in
  Test_helpers.check_ordering_matches ~test_name:"bracket value holding a colon"
    utilities

let test_stacked_variant_outline_order () =
  (* An outline utility sorts after the other focus modifiers on its own
     priority, whatever variants are stacked in front of it: focus:outline-none
     follows focus:bg-red-500, and dark:focus:outline-none closes the dark
     group. The order below is Tailwind's for this class list. *)
  let classes =
    [
      "dark:focus:outline-none";
      "dark:focus:bg-blue-500";
      "dark:focus:ring-2";
      "first:focus:outline-2";
      "first:focus:border-2";
      "focus:outline-none";
      "focus:bg-red-500";
    ]
  in
  let utilities = List.map (fun c -> Result.get_ok (Tw.of_string c)) classes in
  let css = Css.to_string ~minify:true (Tw.to_css ~base:false utilities) in
  let position needle =
    match Astring.String.find_sub ~sub:needle css with
    | Some i -> i
    | None -> Alcotest.failf "%s not found in %s" needle css
  in
  let positions =
    List.map position
      [
        ".focus\\:bg-red-500";
        ".focus\\:outline-none";
        ".first\\:focus\\:border-2";
        ".first\\:focus\\:outline-2";
        ".dark\\:focus\\:bg-blue-500";
        ".dark\\:focus\\:ring-2";
        ".dark\\:focus\\:outline-none";
      ]
  in
  Alcotest.(check (list int))
    "emission order"
    (List.sort Int.compare positions)
    positions;
  Test_helpers.check_ordering_matches ~test_name:"stacked variant outline order"
    utilities

let test_not_supports_variant_order () =
  let classes =
    [
      "px-4";
      "not-supports-hanging-punctuation:px-4";
      "flex";
      "not-supports-[display:grid]:flex";
    ]
  in
  let utilities = List.map (fun c -> Result.get_ok (Tw.of_string c)) classes in
  let css = Css.to_string ~minify:true (Tw.to_css ~base:false utilities) in
  let position needle =
    match Astring.String.find_sub ~sub:needle css with
    | Some i -> i
    | None -> Alcotest.failf "%s not found in %s" needle css
  in
  let positions =
    List.map position
      [
        ".flex{";
        ".px-4{";
        "not-supports-hanging-punctuation";
        "not-supports-\\[display";
      ]
  in
  Alcotest.(check (list int))
    "emission order"
    (List.sort Int.compare positions)
    positions;
  Test_helpers.check_ordering_matches
    ~test_name:"not-supports variants follow base utilities" utilities

let test_breakpoint_groups_stacked_variants () =
  (* A stacked variant sorts under its breakpoint, so first:sm:m-2 stays with
     the other sm rules instead of falling past md:block. Tailwind's order for
     this class list. *)
  let classes = [ "sm:bg-top"; "first:sm:m-2"; "md:block" ] in
  let utilities = List.map (fun c -> Result.get_ok (Tw.of_string c)) classes in
  let css = Css.to_string ~minify:true (Tw.to_css ~base:false utilities) in
  let position needle =
    match Astring.String.find_sub ~sub:needle css with
    | Some i -> i
    | None -> Alcotest.failf "%s not found in %s" needle css
  in
  let positions =
    List.map position [ ".sm\\:bg-top"; ".first\\:sm\\:m-2"; ".md\\:block" ]
  in
  Alcotest.(check (list int))
    "emission order"
    (List.sort Int.compare positions)
    positions

(* The cascade order the variant table gives, end to end. Every token here has a
   position in it; a token with none returns 0, which the comparator reads as
   "this rule carries no variant" and sorts into another group, which is what
   put nth-*, in-* and the child variants ahead of the plain utilities. This
   list is what tailwindcss emits for the same classes. *)
let test_variant_table_emission_order () =
  let classes =
    [
      "block";
      "*:m-1";
      "**:m-2";
      "not-hover:m-3";
      "group-hover:m-4";
      "hover:m-5";
      "inert:m-6";
      "in-focus:m-7";
      "has-checked:m-8";
      "data-active:m-9";
      "nth-3:mt-1";
      "supports-grid:mt-2";
      "md:mt-3";
      "dark:mt-4";
    ]
  in
  let utilities = List.map (fun c -> Result.get_ok (Tw.of_string c)) classes in
  let css = Css.to_string ~minify:true (Tw.to_css ~base:false utilities) in
  let position needle =
    match Astring.String.find_sub ~sub:needle css with
    | Some i -> i
    | None -> Alcotest.failf "%s not found in %s" needle css
  in
  let positions =
    List.map position
      [
        ".block";
        "m-1";
        "m-2";
        "m-3";
        "m-4";
        "m-5";
        "m-6";
        "m-7";
        "m-8";
        "m-9";
        "mt-1";
        "mt-2";
        "mt-3";
        "mt-4";
      ]
  in
  Alcotest.(check (list int))
    "emission order"
    (List.sort Int.compare positions)
    positions

let test_stacked_media_outer_query_order () =
  (* hover:sm: and sm:hover: carry the same variants, so the stack alone cannot
     separate them. The query each writes on the outside does: hover before sm,
     sm before md. Tailwind's order for this class list. *)
  let classes = [ "hover:sm:block"; "sm:hover:block"; "md:block" ] in
  let utilities = List.map (fun c -> Result.get_ok (Tw.of_string c)) classes in
  let css = Css.to_string ~minify:true (Tw.to_css ~base:false utilities) in
  let position needle =
    match Astring.String.find_sub ~sub:needle css with
    | Some i -> i
    | None -> Alcotest.failf "%s not found in %s" needle css
  in
  let positions =
    List.map position
      [ ".hover\\:sm\\:block"; ".sm\\:hover\\:block"; ".md\\:block" ]
  in
  Alcotest.(check (list int))
    "emission order"
    (List.sort Int.compare positions)
    positions

let test_stacked_responsive_variant_order () =
  let classes =
    [ "container"; "sm:bg-top"; "**:[svg]:first:sm:size-4"; "md:block" ]
  in
  let utilities = List.map (fun c -> Result.get_ok (Tw.of_string c)) classes in
  let css = Css.to_string ~minify:true (Tw.to_css ~base:false utilities) in
  let position needle =
    match Astring.String.find_sub ~sub:needle css with
    | Some i -> i
    | None -> Alcotest.failf "%s not found in %s" needle css
  in
  let positions =
    List.map position [ "sm\\:bg-top"; "md\\:block"; "\\*\\*\\:\\[svg\\]" ]
  in
  Alcotest.(check (list int))
    "emission order"
    (List.sort Int.compare positions)
    positions;
  Test_helpers.check_ordering_matches
    ~test_name:"stacked variants retain their highest-order component" utilities

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

let substring_occurrences ~needle haystack =
  let n = String.length needle and h = String.length haystack in
  let rec go i acc =
    if i + n > h then List.rev acc
    else if String.sub haystack i n = needle then go (i + 1) (i :: acc)
    else go (i + 1) acc
  in
  go 0 []

let emitted_classes css classes =
  classes
  |> List.concat_map (fun cls ->
      let selector = Css.Selector.to_string (Css.Selector.class_ cls) in
      substring_occurrences ~needle:selector css
      |> List.map (fun position -> (position, cls)))
  |> List.sort (fun (a, _) (b, _) -> Int.compare a b)
  |> List.map snd

let test_color_mix_supports_companion_order () =
  (* A colour with an opacity modifier emits a hex fallback plus an @supports
     block carrying the color-mix version, and the fallback has to come first or
     it wins in the browsers that support color-mix. Under a variant the
     @supports rules were ordered by the supports-* variant key, which a colour
     utility does not have, so they all tied and drifted away from their base
     rule. *)
  let classes =
    [
      "data-checked:ring-gray-950/10";
      "data-checked:inset-ring-white/10";
      "data-checked:bg-blue-500/20";
      "data-checked:text-red-500/30";
    ]
  in
  let utilities = List.map (fun c -> Result.get_ok (Tw.of_string c)) classes in
  let css = Css.to_string ~minify:true (Tw.to_css ~base:false utilities) in
  (* Every occurrence of every selector, in emitted order. Each utility writes
     exactly two -- the fallback and its @supports companion -- so the sequence
     has to read as adjacent identical pairs. *)
  let emitted =
    emitted_classes css classes
    |> List.filter (fun cls -> String.starts_with ~prefix:"data-checked:" cls)
  in
  let rec paired = function
    | [] -> true
    | a :: b :: rest -> a = b && paired rest
    | [ _ ] -> false
  in
  Alcotest.check Alcotest.bool
    "each fallback is immediately followed by its own @supports companion" true
    (List.length emitted = 2 * List.length classes && paired emitted)

(* A custom variant may put the same utility into a selector branch and a media
   branch. Tailwind keeps all branches of one candidate together, including a
   colour's progressive-enhancement [@supports] rules, before starting the next
   candidate. Sorting the nested media branch on its wrapper instead split one
   candidate around the following colour. *)
let test_custom_variant_supports_companion_order () =
  let defs =
    [
      ( "dark",
        "&:where(.dark,.dark \
         *){@slot;}@media(prefers-color-scheme:dark){&:where(.system,.system \
         *){@slot;}}" );
    ]
  in
  let classes = [ "dark:border-black/10"; "dark:border-fuchsia-500" ] in
  let _, extra, _ =
    Tw_tools.Entrypoint.custom_routed_utilities ~theme:Tw.Scheme.default ~defs
      ~udefs:[] classes
  in
  let css =
    Tw.Build.to_css ~extra [] |> Css.to_string ~minify:true ~lossless:true
  in
  Alcotest.(check (list string))
    "every branch stays with its candidate"
    [
      "dark:border-black/10";
      "dark:border-black/10";
      "dark:border-black/10";
      "dark:border-black/10";
      "dark:border-fuchsia-500";
      "dark:border-fuchsia-500";
    ]
    (emitted_classes css classes)

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

let test_repeated_child_variant_utility_order () =
  (* Repeating the direct-child variant changes which descendants match, but not
     the variant slot. Tailwind still interleaves [*:*:] candidates with [*:],
     according to the utility order inside the child-variant group. *)
  let classes = [ "*:mb-4"; "*:*:max-w-full"; "*:rotate-180" ] in
  Test_helpers.check_class_order
    ~test_name:"repeated child variant follows utility order" classes

let test_named_anchor_inner_order () =
  (* Naming a group or peer changes the marker class, not the state the variant
     wraps. The name must not make focus or checked fall into the zero/unknown
     inner slot ahead of their unnamed forms. *)
  Test_helpers.check_class_order ~test_name:"named anchor keeps inner state"
    [
      "group-open:hidden";
      "group-focus/option:text-white";
      "peer-checked:visible";
      "peer-checked/draft:block";
    ]

let test_stacked_data_variant_slot () =
  (* Two data predicates still occupy one data-variant slot. Tailwind keys the
     stack by its inner data predicate, then keeps utility order inside it,
     instead of placing every two-predicate candidate after all single data
     candidates. Named and arbitrary data variants have separate slots. *)
  Test_helpers.check_class_order ~test_name:"stacked data variant slot"
    [
      "data-hover:bg-indigo-500";
      "data-active:data-hover:bg-indigo-700";
      "data-leave:duration-75";
      "data-[enter]:duration-200";
      "data-[closed]:data-[enter]:-translate-x-8";
      "data-[focus]:border-sky-500";
    ]

let test_compound_data_variant_group () =
  (* A compound whose highest slot is data stays beside the data predicate it
     names. The lower descendant, has, aria, or child slots order candidates
     inside that predicate instead of pushing every compound after all data
     variants. *)
  Test_helpers.check_class_order ~test_name:"compound named data variant group"
    [
      "**:data-avatar:size-12";
      "data-focus:ring-2";
      "data-focus:has-checked:ring-2";
      "aria-[current]:**:data-highlight:fill-gray-300";
      "data-hover:bg-indigo-500";
      "data-leave:duration-75";
      "**:data-outline:stroke-gray-400";
      "aria-[current]:**:data-outline:stroke-gray-950";
    ];
  Test_helpers.check_class_order
    ~test_name:"compound arbitrary data variant group"
    [
      "data-[size=large]:p-8";
      "data-[slot=description]:*:mt-4";
      "data-[state=open]:overflow-hidden";
    ]

let test_arbitrary_variant_selector_order () =
  (* Tailwind orders arbitrary variants by the selector they denote: underscores
     decode to spaces and a bare selector is anchored as [&:is(...)]. Compound
     lower slots then order inside that selector's group. *)
  Test_helpers.check_class_order ~test_name:"arbitrary variant selector order"
    [
      "[&_p]:mt-4";
      "[&.is-dragging]:active:cursor-grabbing";
      "**:[.line]:not-last:min-h-lh";
      "[&:is([open],:popover-open)]:opacity-100";
      "**:[svg]:first:size-5";
      "[&:nth-child(3)]:py-0";
      "[&>*]:rounded-lg";
      "[&>*]:bg-white";
      "[&>*]:p-4";
      "[&>*]:shadow";
      "[&>[data-active]+span]:text-blue-600";
      "[:where(&_.line)]:pl-4";
      "[html:has(&)]:bg-blue-500";
    ]

let test_recursive_compound_variant_order () =
  (* Compound variants compare their wrapped variant recursively. The outer
     group/not slot alone cannot distinguish group-has-indeterminate from
     group-has-focus, or not-group-open from not-group-has-data-lg. *)
  Test_helpers.check_class_order
    ~test_name:"recursive group compound variant order"
    [
      "group-focus:opacity-100";
      "group-has-checked:opacity-100";
      "group-has-indeterminate:opacity-100";
      "group-has-focus:opacity-100";
      "group-has-disabled:stroke-gray-950/25";
      "group-has-[&:focus]:opacity-100";
      "group-has-[a]:block";
      "group-aria-selected:block";
    ];
  Test_helpers.check_class_order
    ~test_name:"recursive negation compound variant order"
    [
      "not-group-open:hidden";
      "not-group-has-data-lg:opacity-40";
      "not-peer-has-checked:opacity-0";
      "not-last:border-b";
    ]

let test_compound_variant_inner_value_order () =
  (* Recursive slot paths still tie for two arbitrary variants. Tailwind then
     compares the decoded selector at the inner variant, so [&:is(.foo)] sorts
     before a selector beginning with [:nth-*]. *)
  Test_helpers.check_class_order
    ~test_name:"group arbitrary inner selector order"
    [
      "group-data-[tooltip-hover=true]:opacity-100";
      "group-[.is-published]:block";
      "group-[:nth-of-type(3)_&]:block";
    ];
  Test_helpers.check_class_order
    ~test_name:"peer arbitrary inner selector order"
    [
      "peer-has-checked:block";
      "peer-[.is-dirty]:peer-required:block";
      "peer-[:nth-of-type(3)_&]:block";
    ]

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

(* A project's [@utility] takes the slot of the property it writes, so it lands
   among the built-ins of that family and the two orders decide which wins.
   Tailwind breaks that tie by how many declarations each rule carries, widest
   first: [.select-none] writes the prefixed spelling as well as [user-select],
   so it comes before a declared utility writing [user-select] alone whatever
   that utility is called. *)
let declared_user_select name =
  let order =
    match Tw.Utility.order_of_property (Key User_select) with
    | Some order -> order
    | None -> Alcotest.fail "user-select has no utility slot"
  in
  ( name,
    order,
    [
      Css.rule ~selector:(Css.Selector.class_ name)
        [ Css.user_select (Text : Css.user_select) ];
    ] )

let builtin cls =
  match Tw.Utility.base_of_class Tw.Scheme.default cls with
  | Ok base -> Tw.Utility.base base
  | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m

let utility_selectors sheet =
  Test_helpers.extract_utilities_layer_rules sheet
  |> Test_helpers.extract_rule_selectors

let check_before what sheet first second =
  let selectors = utility_selectors sheet in
  let position sel =
    match index (String.equal sel) selectors with
    | Some i -> i
    | None -> Alcotest.failf "%s missing from the utilities layer" sel
  in
  check bool what true (position first < position second)

(* A custom-variant expansion arrives through [extra], but it is not a declared
   utility joining the property family. Treating it as one flattens every
   built-in mask-image suborder onto the first slot. *)
let test_mask_gradient_property_walk () =
  let variant = "dark:mask-[linear-gradient(black,transparent)]" in
  let extra =
    ( variant,
      (21, 0),
      [
        Css.rule
          ~selector:(Css.Selector.class_ variant)
          [ Css.mask_image Css.None ];
      ] )
  in
  let sheet =
    Tw.Build.to_css ~extra:[ extra ]
      [ builtin "mask-t-from-50%"; builtin "mask-y-from-70%" ]
  in
  check_before "variant extras preserve mask-image property order" sheet
    ".mask-y-from-70\\%" ".mask-t-from-50\\%"

let test_declared_utility_after_wider_builtin () =
  let sheet =
    Tw.Build.to_css
      ~extra:[ declared_user_select "aaa-sel" ]
      [ builtin "select-none" ]
  in
  check_before "select-none before the declared utility" sheet ".select-none"
    ".aaa-sel"

(* The rule is not an unconditional append: two rules writing the same property
   with as many declarations each still sort by candidate name, so a declared
   utility can precede the built-in. *)
let test_declared_utility_ties_by_name () =
  let sheet =
    Tw.Build.to_css
      ~extra:
        [
          ( "aaa-pad",
            (match Tw.Utility.order_of_property (Key Padding) with
            | Some order -> order
            | None -> Alcotest.fail "padding has no utility slot"),
            [
              Css.rule
                ~selector:(Css.Selector.class_ "aaa-pad")
                [ Css.padding [ Css.Px 5.0 ] ];
            ] );
        ]
      [ builtin "p-4" ]
  in
  check_before "the declared utility keeps its alphabetical place" sheet
    ".aaa-pad" ".p-4"

(* Position starts with two multi-property screen-reader utilities. A declared
   utility writing [position] occupies the preceding property slot without
   collapsing that built-in prelude onto the ordinary position candidates. *)
let test_declared_position_before_screen_reader_prelude () =
  let order =
    match Tw.Utility.order_of_property (Key Position) with
    | Some order -> order
    | None -> Alcotest.fail "position has no utility slot"
  in
  let sheet =
    Tw.Build.to_css
      ~extra:
        [
          ( "line-y",
            order,
            [
              Css.rule
                ~selector:(Css.Selector.class_ "line-y")
                [ Css.position Relative ];
            ] );
        ]
      [ builtin "sr-only"; builtin "not-sr-only"; builtin "absolute" ]
  in
  check (list string) "declared position and built-in prelude"
    [ ".line-y"; ".sr-only"; ".not-sr-only"; ".absolute" ]
    (utility_selectors sheet)

let test_regular_before_media () =
  (* Test that regular rules ALWAYS come before media queries, regardless of their priorities.
   * Example: max-w-4xl (regular, priority 8) and md:grid-cols-2 (media, priority 12).
   * Even though md:grid-cols-2 has higher priority, max-w-4xl (regular) should come first
   * because Tailwind emits all regular utilities before any media query utilities. *)
  let utilities = Tw.[ md [ grid_cols 2 ]; max_w_4xl ] in
  Test_helpers.check_ordering_matches ~test_name:"regular always before media"
    utilities

(* Comparator laws.

   [List.sort] is defined only for a total order. When the comparator is not
   antisymmetric or not transitive, the same set of rules can sort into
   different sequences depending on the order it happens to arrive in, which is
   how every ordering bug recorded in this file has presented: a diff that
   appears and disappears as utilities are added around it.

   Checked over the rules real utilities produce rather than hand-built records,
   so a field or a branch is covered as soon as some class reaches it. The
   corpus deliberately spans the branches the comparator dispatches on: plain
   rules, media and container variants, stacked variants, bracket values holding
   a colon, not-/has-/group-/peer- variants, @supports and @starting-style, and
   the outline and prose special cases.

   [compare_by_order_regular_first] returns -1 on a tie on purpose; the
   Media/Regular arm negates it, so it is the composed comparator - what
   [List.sort] actually calls - that has to obey the laws, and that is what is
   tested here. *)
let comparator_corpus =
  [
    "p-4";
    "px-2";
    "m-4";
    "-m-2";
    "mt-8";
    "gap-4";
    "gap-x-2";
    "block";
    "flex";
    "grid";
    "hidden";
    "w-4";
    "h-8";
    "max-w-4xl";
    "z-10";
    "top-0";
    "absolute";
    "relative";
    "border";
    "border-2";
    "border-b";
    "border-gray-200";
    "border-solid";
    "divide-x-2";
    "divide-gray-200";
    "rounded-sm";
    "rounded-t-lg";
    "bg-white";
    "bg-blue-600";
    "text-lg";
    "text-gray-900";
    "font-bold";
    "leading-relaxed";
    "shadow-md";
    "opacity-50";
    "outline-none";
    "outline-2";
    "grid-cols-2";
    "flex-col";
    "items-center";
    "justify-between";
    "transition-all";
    "duration-150";
    "animate-spin";
    "blur-sm";
    "translate-x-4";
    "rotate-90";
    "scale-50";
    "cursor-pointer";
    "select-none";
    "sr-only";
    "container";
    "prose";
    "prose-sm";
    (* Bracket values: the ones holding a colon are what a bracket-blind variant
       split mis-reads. *)
    "bg-[color:var(--brand)]";
    "hover:bg-[color:var(--brand)]";
    "w-[calc(100%-1rem)]";
    "text-[14px]";
    "rotate-[10deg]";
    (* Single variants. *)
    "hover:bg-blue-500";
    "focus:outline-none";
    "active:bg-blue-700";
    "disabled:opacity-50";
    "first:pt-0";
    "last:pb-0";
    "odd:bg-gray-50";
    "before:block";
    "after:block";
    "marker:text-gray-500";
    "placeholder:text-gray-400";
    "dark:bg-gray-900";
    "sm:p-2";
    "md:grid-cols-2";
    "lg:flex";
    "xl:hidden";
    "max-md:block";
    "min-lg:flex";
    "motion-safe:animate-pulse";
    "motion-reduce:transition-none";
    "contrast-more:border-4";
    "group-hover:text-white";
    "peer-checked:bg-blue-500";
    "peer-focus:ring-2";
    "aria-checked:bg-blue-500";
    "data-[state=open]:block";
    "not-hover:opacity-50";
    "has-[:focus]:border-2";
    "supports-grid:flex";
    "starting:opacity-0";
    "@container";
    "@sm:flex";
    (* Stacked variants: the compound-order path. *)
    "dark:hover:bg-gray-800";
    "dark:focus:outline-none";
    "md:hover:bg-blue-500";
    "sm:dark:p-4";
    "lg:group-hover:text-white";
  ]

let utility_of_corpus_class cls =
  let modifiers, base_class = Tw.Modifiers.of_string cls in
  match Tw.Utility.base_of_class Tw.Scheme.default base_class with
  | Error (`Msg m) -> Alcotest.failf "corpus class %S does not parse: %s" cls m
  | Ok b -> (
      match Tw.Modifiers.apply modifiers (Tw.Utility.base b) with
      | Some u -> u
      | None -> Alcotest.failf "corpus class %S: unknown modifier" cls)

(* Shuffled so a run does not always present the rules in corpus order: the
   index tiebreaker is part of the comparator, and a law that only holds for one
   arrival order is exactly the bug being looked for. *)
let corpus_rules () =
  comparator_corpus |> Test_helpers.shuffle
  |> List.map utility_of_corpus_class
  |> Tw.Build.indexed_rules

let describe r = Tw.Build.rule_selector r

let test_comparator_antisymmetry () =
  let rules = Array.of_list (corpus_rules ()) in
  let n = Array.length rules in
  Alcotest.check Alcotest.bool "corpus produced rules" true (n > 50);
  for i = 0 to n - 1 do
    let c = Tw.Build.compare_rules rules.(i) rules.(i) in
    if c <> 0 then
      Alcotest.failf "compare is not reflexive on %s: %d" (describe rules.(i)) c;
    for j = i + 1 to n - 1 do
      let ab = Tw.Build.compare_rules rules.(i) rules.(j) in
      let ba = Tw.Build.compare_rules rules.(j) rules.(i) in
      if Int.compare ab 0 <> -Int.compare ba 0 then
        Alcotest.failf
          "compare is not antisymmetric:\n\
          \  a = %s\n\
          \  b = %s\n\
          \  a vs b = %d, b vs a = %d"
          (describe rules.(i))
          (describe rules.(j))
          ab ba
    done
  done

let test_comparator_transitivity () =
  let rules = Array.of_list (corpus_rules ()) in
  let n = Array.length rules in
  let sign x = Int.compare x 0 in
  let pick () = rules.(Random.State.int Test_helpers.test_rng n) in
  (* Sampled: the corpus has too many triples to enumerate on every run, and a
     violation is dense enough in the pairs that reach it for sampling to find
     it. The seed is printed, so a failure replays. *)
  for _ = 1 to 200_000 do
    let a = pick () and b = pick () and c = pick () in
    let ab = sign (Tw.Build.compare_rules a b) in
    let bc = sign (Tw.Build.compare_rules b c) in
    let ac = sign (Tw.Build.compare_rules a c) in
    let violates =
      (ab < 0 && bc < 0 && ac >= 0)
      || (ab > 0 && bc > 0 && ac <= 0)
      || (ab = 0 && bc = 0 && ac <> 0)
    in
    if violates then
      Alcotest.failf
        "compare is not transitive:\n\
        \  a = %s\n\
        \  b = %s\n\
        \  c = %s\n\
        \  a vs b = %d, b vs c = %d, a vs c = %d"
        (describe a) (describe b) (describe c) ab bc ac
  done

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
    test_case "late typography before white-space" `Quick
      test_late_typography_before_whitespace;
    test_case "word-wrap family order" `Quick test_word_wrap_family_order;
    test_case "text-indent before tracking" `Quick test_indent_before_tracking;
    test_case "text-indent inside early typography" `Quick
      test_indent_within_early_typography;
    test_case "text-indent family order" `Quick test_indent_family_order;
    test_case "transform control bands" `Slow test_transform_control_bands;
    test_case "transform candidate bands" `Slow test_transform_candidate_bands;
    test_case "mask-gradient property walk" `Slow
      test_mask_gradient_property_walk;
    test_case "scrolling property bands" `Slow test_scrolling_property_bands;
    test_case "flow property bands" `Slow test_flow_property_bands;
    test_case "tab property band" `Slow test_tab_property_band;
    test_case "field sizing property band" `Slow test_field_sizing_property_band;
    test_case "late control property bands" `Slow
      test_late_control_property_bands;
    test_case "outline, select and alpha shadow bands" `Slow
      test_outline_select_shadow_property_bands;
    test_case "backface/logical sizing boundary" `Slow
      test_backface_logical_sizing_boundary;
    test_case "logical side property bands" `Slow
      test_logical_side_property_bands;
    test_case "shadow and transform boundaries" `Slow
      test_shadow_and_transform_boundaries;
    test_case "late typography property bands" `Slow
      test_late_typography_property_bands;
    test_case "priority order per group" `Quick test_priority_order_per_group;
    test_case "handler priority ordering" `Quick test_handler_priority_ordering;
    test_case "border width and color ordering" `Quick
      test_border_width_color_ordering;
    test_case "container width-property order" `Slow test_container_order;
    test_case "container call sorts by its name" `Slow
      test_container_query_call_order;
    test_case "arbitrary before named within family" `Slow
      test_arbitrary_vs_named_order;
    test_case "arbitrary value sorts by suffix within family" `Slow
      test_arbitrary_named_by_suffix;
    test_case "sizing values use natural candidate order" `Slow
      test_sizing_values_natural_order;
    test_case "bracket value holding a colon" `Slow
      test_bracket_value_holding_a_colon;
    test_case "stacked variant outline order" `Slow
      test_stacked_variant_outline_order;
    test_case "not-supports variant order" `Slow test_not_supports_variant_order;
    test_case "variant table emission order" `Slow
      test_variant_table_emission_order;
    test_case "stacked responsive variant order" `Slow
      test_stacked_responsive_variant_order;
    test_case "breakpoint groups stacked variants" `Slow
      test_breakpoint_groups_stacked_variants;
    test_case "stacked media outer query order" `Slow
      test_stacked_media_outer_query_order;
    test_case "rounded position order" `Slow test_rounded_position_order;
    test_case "color-mix @supports companion order" `Slow
      test_color_mix_supports_companion_order;
    test_case "custom variant branches stay with their candidate" `Quick
      test_custom_variant_supports_companion_order;
    test_case "margin value order" `Slow test_margin_value_order;
    test_case "prose margin order" `Slow test_prose_margin_order;
    test_case "variant same-suborder tiebreak" `Slow
      test_variant_same_suborder_tiebreak;
    test_case "variant arbitrary values sort numerically" `Slow
      test_variant_arbitrary_numeric_order;
    test_case "repeated child variant follows utility order" `Slow
      test_repeated_child_variant_utility_order;
    test_case "named anchor keeps inner state" `Slow
      test_named_anchor_inner_order;
    test_case "stacked data variant slot" `Slow test_stacked_data_variant_slot;
    test_case "compound data variant group" `Slow
      test_compound_data_variant_group;
    test_case "arbitrary variant selector order" `Slow
      test_arbitrary_variant_selector_order;
    test_case "recursive compound variant order" `Slow
      test_recursive_compound_variant_order;
    test_case "compound variant inner value order" `Slow
      test_compound_variant_inner_value_order;
    test_case "compound variant highest component" `Slow
      test_compound_variant_highest_component;
    test_case "compound variant same multiset" `Slow
      test_compound_variant_same_multiset;
    test_case "regular before media same priority" `Quick
      test_regular_before_media;
    test_case "declared utility after a wider built-in" `Quick
      test_declared_utility_after_wider_builtin;
    test_case "declared utility ties by candidate name" `Quick
      test_declared_utility_ties_by_name;
    test_case "declared position precedes the screen-reader prelude" `Quick
      test_declared_position_before_screen_reader_prelude;
    test_case "rules_of_grouped prose merging bug" `Quick
      rules_of_grouped_prose_bug;
    test_case "suborder within group" `Slow test_suborder_within_group;
    test_case "pool covers every family" `Quick test_pool_covers_every_family;
    test_case "known inversions are exact" `Slow test_known_inversions_are_exact;
    test_case "mask type arbitrary order" `Slow test_mask_type_arbitrary_order;
    test_case "pool covers every handler" `Quick test_pool_covers_every_handler;
    test_case "pool variants all compile" `Quick test_variants_all_compile;
    test_case "random utilities with minimization" `Slow
      test_random_utilities_with_minimization;
    test_case "variant families sort as Tailwind" `Quick
      test_variant_family_order;
    test_case "comparator is antisymmetric" `Quick test_comparator_antisymmetry;
    test_case "comparator is transitive" `Quick test_comparator_transitivity;
  ]

let suite = ("sort", tests)
