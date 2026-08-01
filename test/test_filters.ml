open Alcotest

let check class_name =
  match Tw.Filters.Handler.of_class Tw.Scheme.default class_name with
  | Ok u ->
      check string "filters class" class_name (Tw.Filters.Handler.to_class u)
  | Error (`Msg msg) -> fail msg

let test_blur () =
  check "blur-sm";
  check "blur-2xl"

(* drop-shadow-xs is a v4.3.1 default size (0 1px 1px, alpha .05); it
   round-trips and references the --drop-shadow-xs theme token. *)
let test_drop_shadow_xs () =
  check "drop-shadow-xs";
  let css =
    Tw.to_css [ Result.get_ok (Tw.of_string "drop-shadow-xs") ]
    |> Tw.Css.to_string ~minify:true
  in
  Alcotest.(check bool)
    "references var(--drop-shadow-xs)" true
    (Astring.String.is_infix ~affix:"var(--drop-shadow-xs)" css);
  Alcotest.(check bool)
    "emits --drop-shadow-xs default" true
    (Astring.String.is_infix ~affix:"--drop-shadow-xs:" css)

(* drop-shadow-<color> resolves the palette colour itself for the fallback, so
   the default theme (which declares no hex colours) still gets one. The
   fallback is what a browser without color-mix reads, so it has to be a plain
   hex rather than a mix of its own. *)
let test_drop_shadow_color () =
  let css cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  Alcotest.(check bool)
    "drop-shadow-red-500 sets the drop-shadow color" true
    (Astring.String.is_infix ~affix:"--tw-drop-shadow-color"
       (css "drop-shadow-red-500"));
  Alcotest.(check bool)
    "drop-shadow-red-500/50 uses color-mix" true
    (Astring.String.is_infix ~affix:"color-mix" (css "drop-shadow-red-500/50"));
  Alcotest.(check bool)
    "drop-shadow-blue-500/50 falls back to a plain hex" true
    (Astring.String.is_infix ~affix:"--tw-drop-shadow-color: #3080ff80"
       (css "drop-shadow-blue-500/50"))

(* drop-shadow/<n> recolours the default shadow, which is a two-layer stack, so
   both layers carry the modifier's alpha as their fallback. It used to emit one
   layer and reference the theme token, losing the first shadow. *)
let test_drop_shadow_opacity_keeps_both_layers () =
  let css =
    match Tw.of_string "drop-shadow/50" with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.pp ~minify:true
    | Error (`Msg m) -> Alcotest.failf "drop-shadow/50: %s" m
  in
  let has affix = Astring.String.is_infix ~affix css in
  Alcotest.(check bool) "the first layer is emitted" true (has "0 1px 2px");
  Alcotest.(check bool) "the second layer is emitted" true (has "0 1px 1px");
  Alcotest.(check bool)
    "--tw-drop-shadow is the default stack, not the theme reference" false
    (has "drop-shadow(var(--drop-shadow))")

(* A fractional opacity modifier keeps its fraction: drop-shadow/12.5 -> alpha
   12.5%, not the truncated 12%. *)
let test_drop_shadow_fractional_alpha () =
  let css =
    match Tw.of_string "drop-shadow/12.5" with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.pp ~minify:true
    | Error (`Msg m) -> Alcotest.failf "drop-shadow/12.5: %s" m
  in
  Alcotest.(check bool)
    "alpha is 12.5%, not 12%" true
    (Astring.String.is_infix ~affix:"--tw-drop-shadow-alpha:12.5%" css)

let test_backdrop () =
  check "backdrop-opacity-50";
  check "backdrop-invert"

(* The drop-shadow sizes come before the colours, and each group is ordered by
   class name. Sizes and colours both write --tw-drop-shadow, so the order
   decides the value: with the colours ahead of the sizes, drop-shadow-current
   beat drop-shadow-sm. *)
let drop_shadow_slot_order () =
  let classes =
    [
      "drop-shadow-2xl";
      "drop-shadow-lg";
      "drop-shadow-sm";
      "drop-shadow-xl";
      "drop-shadow-xs";
      "drop-shadow-current";
      "drop-shadow-indigo-500";
      "drop-shadow-inherit";
    ]
  in
  let utilities = List.map (fun c -> Result.get_ok (Tw.of_string c)) classes in
  let css =
    Cascade.Css.to_string ~minify:true (Tw.to_css ~base:false utilities)
  in
  let positions =
    List.map
      (fun c ->
        let needle = "." ^ c ^ "{" in
        let n = String.length needle and h = String.length css in
        let rec go i =
          if i + n > h then -1
          else if String.sub css i n = needle then i
          else go (i + 1)
        in
        go 0)
      classes
  in
  Alcotest.check bool "every utility is emitted" true
    (List.for_all (fun p -> p >= 0) positions);
  Alcotest.check
    (Alcotest.list Alcotest.int)
    "sizes then colours, each group by class name"
    (List.sort Int.compare positions)
    positions

let suborder_matches_tailwind () =
  let open Tw in
  let shuffled =
    Test_helpers.shuffle
      [ blur_sm; blur; blur_2xl; backdrop_blur; backdrop_opacity 50. ]
  in

  Test_helpers.check_ordering_matches
    ~test_name:"filters suborder matches Tailwind" shuffled

(* backdrop-blur-N must reference the unified v4 --blur-N token (not the dropped
   --backdrop-blur-N) and emit the shipped --blur-N decl. *)
let test_backdrop_blur_token () =
  let css = Tw.to_css [ Tw.backdrop_blur_sm ] |> Tw.Css.pp ~minify:true in
  Alcotest.(check bool)
    "references var(--blur-sm)" true
    (Astring.String.is_infix ~affix:"var(--blur-sm)" css);
  Alcotest.(check bool)
    "emits --blur-sm:8px" true
    (Astring.String.is_infix ~affix:"--blur-sm:8px" css)

(* A drop-shadow colour the theme has no token for, and a named size with an
   alpha: both were unknown classes. The size form replaces the shadow's own
   colour with black at that alpha and leaves the theme token out. *)
let test_drop_shadow_keyword_and_alpha () =
  let css cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string ~minify:true
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  let has cls affix =
    Alcotest.(check bool) cls true (Astring.String.is_infix ~affix (css cls))
  in
  has "drop-shadow-current" "--tw-drop-shadow-color:currentColor";
  has "drop-shadow-transparent" "--tw-drop-shadow-color:transparent";
  has "drop-shadow-xl/25" "--tw-drop-shadow-alpha:25%";
  has "drop-shadow-xl/25" "drop-shadow(0 9px 7px var(--tw-drop-shadow-color,";
  (* the theme token is not declared for the alpha form *)
  Alcotest.(check bool)
    "no --drop-shadow-xl token" false
    (Astring.String.is_infix ~affix:"--drop-shadow-xl:"
       (css "drop-shadow-xl/25"))

(* An arbitrary filter amount that is not a number, a percentage or a var used
   to be coerced to zero, so brightness-[abc] emitted brightness(0). *)
let test_invalid_arbitrary_amount () =
  let rejected cls =
    match Tw.of_string cls with
    | Ok _ -> Alcotest.failf "expected %s to be rejected" cls
    | Error _ -> ()
  in
  rejected "brightness-[abc]";
  rejected "invert-[xyz]";
  rejected "backdrop-sepia-[nope]";
  rejected "drop-shadow-[<value>]";
  check "brightness-[1.5]";
  check "saturate-[150%]";
  check "brightness-[var(--x)]"

(* An arbitrary filter spells its spaces with [_], so a multi-function chain
   like filter-[blur(4px)_saturate(150%)] has to be decoded before it is parsed.
   Without that the whole class parsed as nothing and emitted an empty rule. *)
let test_arbitrary_filter_chain () =
  let css cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  let has cls affix =
    Alcotest.(check bool) cls true (Astring.String.is_infix ~affix (css cls))
  in
  has "filter-[blur(4px)_saturate(150%)]" "filter: blur(4px) saturate(150%)";
  has "backdrop-filter-[blur(4px)_saturate(150%)]"
    "-webkit-backdrop-filter: blur(4px) saturate(150%)";
  has "backdrop-filter-[blur(4px)_saturate(150%)]"
    "backdrop-filter: blur(4px) saturate(150%)";
  (* single-function and var() forms are unchanged *)
  has "filter-[blur(4px)]" "filter: blur(4px)";
  has "filter-[var(--my-filter)]" "filter: var(--my-filter)";
  has "backdrop-filter-[blur(4px)]" "backdrop-filter: blur(4px)";
  has "backdrop-filter-[var(--x)]" "backdrop-filter: var(--x)"

(* A bracket value the filter grammar cannot take is not a utility. It used to
   parse, then emit an empty rule: no CSS and no diagnostic. Same for a
   drop-shadow name the theme has no --drop-shadow-<name> token for. *)
let test_unparseable_arbitrary_filter_rejected () =
  let rejected cls =
    match Tw.of_string cls with
    | Ok _ -> Alcotest.failf "expected %s to be rejected" cls
    | Error _ -> ()
  in
  rejected "filter-[nope(1)]";
  rejected "backdrop-filter-[nope(1)]";
  rejected "drop-shadow-nope";
  check "filter-[blur(4px)]";
  check "backdrop-filter-[blur(4px)]";
  check "drop-shadow-xs";
  check "drop-shadow-red-500"

(* Every filter class the parser accepts renders at least one declaration; an
   accepted class that emits nothing is the silent-acceptance bug. *)
let test_no_empty_rules () =
  let non_empty cls =
    match Tw.of_string cls with
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
    | Ok u ->
        let css = Tw.to_css ~base:false [ u ] |> Tw.Css.to_string in
        Alcotest.(check bool)
          (cls ^ " emits a declaration")
          true
          (Astring.String.is_infix ~affix:":" css)
  in
  non_empty "filter-[blur(4px)]";
  non_empty "backdrop-filter-[blur(4px)]";
  non_empty "drop-shadow-[0_0_2px_red]";
  non_empty "drop-shadow-xl/25"

(* Filters are the case the text-level comparison cannot judge: a drop-shadow
   colour and a drop-shadow size meet in --tw-drop-shadow-size, so what an
   element ends up filtering by is only visible once rendered. *)
let rendering_matches_tailwind () =
  let classes =
    [
      "blur-sm";
      "blur";
      "brightness-125";
      "contrast-75";
      "grayscale";
      "invert";
      "saturate-150";
      "sepia";
      "drop-shadow-sm";
      "drop-shadow-xl";
      "drop-shadow-current";
      "drop-shadow-indigo-500";
      "backdrop-blur";
      "backdrop-opacity-50";
    ]
  in
  Test_helpers.check_rendering_matches ~test_name:"filters render like Tailwind"
    (List.map (fun c -> Result.get_ok (Tw.of_string c)) classes)

let tests =
  [
    test_case "drop-shadow keyword color and alpha" `Quick
      test_drop_shadow_keyword_and_alpha;
    test_case "filters render like Tailwind" `Slow rendering_matches_tailwind;
    test_case "blur" `Quick test_blur;
    test_case "invalid arbitrary amount" `Quick test_invalid_arbitrary_amount;
    test_case "arbitrary filter chain" `Quick test_arbitrary_filter_chain;
    test_case "unparseable arbitrary filter rejected" `Quick
      test_unparseable_arbitrary_filter_rejected;
    test_case "accepted filters emit declarations" `Quick test_no_empty_rules;
    test_case "drop-shadow-xs (v4.3.1 size)" `Quick test_drop_shadow_xs;
    test_case "drop-shadow color (default theme)" `Quick test_drop_shadow_color;
    test_case "drop-shadow opacity keeps both layers" `Quick
      test_drop_shadow_opacity_keeps_both_layers;
    test_case "drop-shadow fractional alpha" `Quick
      test_drop_shadow_fractional_alpha;
    test_case "backdrop" `Quick test_backdrop;
    test_case "backdrop-blur token" `Quick test_backdrop_blur_token;
    test_case "filters suborder matches Tailwind" `Quick
      suborder_matches_tailwind;
    test_case "drop-shadow slot order" `Quick drop_shadow_slot_order;
  ]

let suite = ("filters", tests)
