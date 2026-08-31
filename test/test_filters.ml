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
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string ~minify:true
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
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string ~minify:true
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

(* A project-defined drop-shadow size and the built-in multi-shadow size share
   the same candidate slot. Tailwind orders both by class name, so calc comes
   before multi even though the latter has a dedicated constructor. *)
let project_drop_shadow_size_order () =
  let theme =
    Tw.Scheme.with_overrides Tw.Scheme.default
      [ ("drop-shadow-calc", "0 0 calc(1 * var(--spacing)) black") ]
  in
  let classes = [ "drop-shadow-calc"; "drop-shadow-multi" ] in
  let utilities =
    List.rev classes
    |> List.map (fun cls ->
        match Tw.of_string ~theme cls with
        | Ok utility -> utility
        | Error (`Msg message) -> Alcotest.failf "%s: %s" cls message)
  in
  let css =
    Tw.to_css ~theme ~base:false utilities |> Tw.Css.to_string ~minify:true
  in
  let position cls =
    let needle = "." ^ cls ^ "{" in
    match Astring.String.find_sub ~sub:needle css with
    | Some position -> position
    | None -> Alcotest.failf "%s missing from the tw sheet" cls
  in
  Alcotest.(check bool)
    "project size precedes multi" true
    (position "drop-shadow-calc" < position "drop-shadow-multi")

let drop_shadow_candidate_order () =
  Test_helpers.check_class_order ~test_name:"drop-shadow candidate order"
    [
      "drop-shadow-lg";
      "drop-shadow-[0_3px_1px_rgba(0,0,0,.15)]";
      "drop-shadow-2xl";
      "drop-shadow";
    ]

let suborder_matches_tailwind () =
  let open Tw in
  let shuffled =
    Test_helpers.shuffle
      [ blur_sm; blur; blur_2xl; backdrop_blur; backdrop_opacity 50. ]
  in

  Test_helpers.check_ordering_matches
    ~test_name:"filters suborder matches Tailwind" shuffled

(* A filter kind is one property slot; Tailwind orders its candidate spellings
   naturally. Per-value arithmetic reverses negative angles and the
   grayscale/invert/sepia tails, while hand-numbered blur sizes disagree with
   candidate order. *)
let candidate_order_matches_tailwind () =
  Test_helpers.check_class_order ~test_name:"filter candidate order"
    [
      "sepia-0";
      "sepia";
      "invert-65";
      "invert-0";
      "invert";
      "hue-rotate-270";
      "hue-rotate-0";
      "-hue-rotate-180";
      "-hue-rotate-90";
      "-hue-rotate-15";
      "grayscale-200";
      "grayscale-50";
      "grayscale-0";
      "grayscale";
      "blur-xs";
      "blur-xl";
      "blur-sm";
      "blur-none";
      "blur-md";
      "blur-lg";
      "blur-3xl";
      "blur-2xl";
      "blur";
      "backdrop-filter-none";
      "backdrop-filter";
      "backdrop-sepia-50";
      "backdrop-sepia-0";
      "backdrop-sepia";
      "backdrop-invert-65";
      "backdrop-invert-0";
      "backdrop-invert";
      "backdrop-hue-rotate-270";
      "backdrop-hue-rotate-0";
      "-backdrop-hue-rotate-180";
      "-backdrop-hue-rotate-90";
      "-backdrop-hue-rotate-15";
      "backdrop-grayscale-200";
      "backdrop-grayscale-50";
      "backdrop-grayscale-0";
      "backdrop-grayscale";
      "backdrop-blur-xs";
      "backdrop-blur-xl";
      "backdrop-blur-sm";
      "backdrop-blur-none";
      "backdrop-blur-md";
      "backdrop-blur-lg";
      "backdrop-blur-3xl";
      "backdrop-blur-2xl";
      "backdrop-blur";
    ]

(* backdrop-blur-N must reference the unified v4 --blur-N token (not the dropped
   --backdrop-blur-N) and emit the shipped --blur-N decl. *)
let test_backdrop_blur_token () =
  let css =
    Tw.to_css [ Tw.backdrop_blur_sm ] |> Tw.Css.to_string ~minify:true
  in
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

(* An arbitrary angle is spelled in the class name the way the author wrote it.
   Rendering it back from the parsed angle left the trailing dot of an integral
   float on every unit but [deg], so the selector could not match the markup. *)
let test_arbitrary_angle_class_name () =
  List.iter check
    [
      "hue-rotate-[2rad]";
      "hue-rotate-[1turn]";
      "hue-rotate-[100grad]";
      "hue-rotate-[45deg]";
      "hue-rotate-[0.5rad]";
      "-hue-rotate-[2rad]";
      "backdrop-hue-rotate-[2rad]";
      "-backdrop-hue-rotate-[1turn]";
    ]

(* A bracket that is not an angle is refused. *)
let test_invalid_arbitrary_angle () =
  let rejected cls =
    match Tw.of_string cls with
    | Ok u -> Alcotest.failf "expected %s to be rejected, got %s" cls (Tw.pp u)
    | Error _ -> ()
  in
  rejected "hue-rotate-[2]";
  rejected "hue-rotate-[2zz]";
  rejected "hue-rotate-[deg]";
  rejected "backdrop-hue-rotate-[2px]"

(* [blur-[...]] takes a length. A bracket the length grammar cannot read was
   accepted and then raised out of [to_css], which is a pure conversion. Reading
   the bracket with cascade's grammar also earns [calc()] and the units the
   hand-rolled reader never took. *)
let test_invalid_arbitrary_blur () =
  let rejected cls =
    match Tw.of_string cls with
    | Ok u -> Alcotest.failf "expected %s to be rejected, got %s" cls (Tw.pp u)
    | Error _ -> ()
  in
  let renders cls =
    match Tw.of_string cls with
    | Ok u -> ignore (Tw.to_css ~base:false [ u ] |> Tw.Css.to_string)
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  rejected "blur-[foo]";
  rejected "blur-[red]";
  rejected "blur-[1]";
  rejected "backdrop-blur-[foo]";
  rejected "backdrop-blur-[a,b]";
  renders "blur-[4px]";
  renders "blur-[.5rem]";
  renders "blur-[calc(1px_+_2px)]";
  renders "blur-[1vw]";
  renders "blur-[var(--x)]";
  (* A sizing keyword is not a [blur()] argument, but Tailwind writes it through
     and so do we; rejecting it would drop CSS Tailwind emits. *)
  renders "blur-[none]";
  renders "backdrop-blur-[4px]";
  renders "backdrop-blur-[calc(1px_+_2px)]"

(* A [--blur-*] token the project declared in its [@theme] names a radius the
   built-in scale has no slot for. Tailwind generates the utility from it,
   filter chain included; tw rejected the class outright. *)
let test_project_blur_token () =
  let theme =
    Tw.Scheme.with_overrides Tw.Scheme.default [ ("blur-soft", "7px") ]
  in
  let css cls =
    match Tw.of_string ~theme cls with
    | Ok u -> Tw.to_css ~theme ~base:false [ u ] |> Tw.Css.to_string
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  let out = css "blur-soft" in
  Alcotest.(check bool)
    "sets the blur channel" true
    (Astring.String.is_infix ~affix:"--tw-blur: blur(var(--blur-soft))" out);
  Alcotest.(check bool)
    "an undeclared blur name is rejected" true
    (Result.is_error (Tw.of_string ~theme "blur-nope"))

let tests =
  [
    test_case "arbitrary angle class name" `Quick
      test_arbitrary_angle_class_name;
    test_case "invalid arbitrary angle" `Quick test_invalid_arbitrary_angle;
    test_case "invalid arbitrary blur" `Quick test_invalid_arbitrary_blur;
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
    test_case "filter candidate order" `Slow candidate_order_matches_tailwind;
    test_case "drop-shadow slot order" `Quick drop_shadow_slot_order;
    test_case "project drop-shadow size order" `Quick
      project_drop_shadow_size_order;
    test_case "drop-shadow candidate order" `Slow drop_shadow_candidate_order;
    test_case "project blur token" `Quick test_project_blur_token;
  ]

let suite = ("filters", tests)
