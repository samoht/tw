open Alcotest

let check class_name =
  match Tw.Filters.Handler.of_class Tw.Scheme.default class_name with
  | Ok u ->
      check string "filters class" class_name (Tw.Filters.Handler.to_class u)
  | Error (`Msg msg) -> fail msg

(* Every filter utility re-declares the whole [filter] chain, so a declaration
   list that names one channel ends with this. *)
let chain =
  "filter: var(--tw-blur,) var(--tw-brightness,) var(--tw-contrast,) \
   var(--tw-grayscale,) var(--tw-hue-rotate,) var(--tw-invert,) \
   var(--tw-saturate,) var(--tw-sepia,) var(--tw-drop-shadow,)"

let chain_min =
  "filter:var(--tw-blur,)var(--tw-brightness,)var(--tw-contrast,)var(--tw-grayscale,)var(--tw-hue-rotate,)var(--tw-invert,)var(--tw-saturate,)var(--tw-sepia,)var(--tw-drop-shadow,)"

(* The backdrop chain, which every backdrop-* utility re-declares twice: once
   aliased for WebKit, once not. *)
let backdrop_chain_min =
  "var(--tw-backdrop-blur,)var(--tw-backdrop-brightness,)var(--tw-backdrop-contrast,)var(--tw-backdrop-grayscale,)var(--tw-backdrop-hue-rotate,)var(--tw-backdrop-invert,)var(--tw-backdrop-opacity,)var(--tw-backdrop-saturate,)var(--tw-backdrop-sepia,)"

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
  (* The whole list, which also says the size channel carries the shadow the
     token expands to rather than only referencing it. *)
  Test_helpers.check_declarations ~minify:false "drop-shadow-xs"
    [
      "--tw-drop-shadow-size: drop-shadow(0 1px 1px \
       var(--tw-drop-shadow-color, #0000000d))";
      "--tw-drop-shadow: drop-shadow(var(--drop-shadow-xs))";
      chain;
    ];
  (* The token's own binding is a :root declaration, which the list above leaves
     out by design, so this one stays a substring. *)
  Alcotest.(check bool)
    "emits --drop-shadow-xs default" true
    (Astring.String.is_infix ~affix:"--drop-shadow-xs:" css)

(* drop-shadow-<color> resolves the palette colour itself for the fallback, so
   the default theme (which declares no hex colours) still gets one. The
   fallback is what a browser without color-mix reads, so it has to be a plain
   hex rather than a mix of its own. *)
let test_drop_shadow_color () =
  (* [--tw-drop-shadow-color] alone said a declaration with that name existed,
     and [color-mix] said some declaration somewhere used one. Both are the
     enhancement arm; the whole list names the fallback beside it, which is what
     a browser without color-mix reads. *)
  Test_helpers.check_declarations ~minify:false "drop-shadow-red-500"
    [
      "--tw-drop-shadow-color: oklch(63.7% .237 25.331)";
      "--tw-drop-shadow-color: color-mix(in oklab, var(--color-red-500) \
       var(--tw-drop-shadow-alpha), transparent)";
      "--tw-drop-shadow: var(--tw-drop-shadow-size)";
    ];
  Test_helpers.check_declarations ~minify:false "drop-shadow-red-500/50"
    [
      "--tw-drop-shadow-color: #fb2c3680";
      "--tw-drop-shadow-color: color-mix(in oklab, color-mix(in oklab, \
       var(--color-red-500) 50%, transparent) var(--tw-drop-shadow-alpha), \
       transparent)";
      "--tw-drop-shadow: var(--tw-drop-shadow-size)";
    ];
  (* The whole list here, which is the one whose fallback this file pins: the
     unguarded arm is a plain hex, because a browser without color-mix reads it
     and cannot read a mix of its own. The enhancement arm and the size
     reference travel with it. *)
  Test_helpers.check_declarations ~minify:false "drop-shadow-blue-500/50"
    [
      "--tw-drop-shadow-color: #3080ff80";
      "--tw-drop-shadow-color: color-mix(in oklab, color-mix(in oklab, \
       var(--color-blue-500) 50%, transparent) var(--tw-drop-shadow-alpha), \
       transparent)";
      "--tw-drop-shadow: var(--tw-drop-shadow-size)";
    ]

(* drop-shadow/<n> recolours the default shadow, which is a two-layer stack, so
   both layers carry the modifier's alpha as their fallback. It used to emit one
   layer and reference the theme token, losing the first shadow. *)
let test_drop_shadow_opacity_keeps_both_layers () =
  (* The whole list, which is where "not the theme reference" is said: it used
     to be a search for a spelling that must not appear, and the two layer
     checks were for [0 1px 2px] anywhere in the sheet. *)
  Test_helpers.check_declarations "drop-shadow/50"
    [
      "--tw-drop-shadow-alpha:50%";
      "--tw-drop-shadow-size:drop-shadow(0 1px 2px \
       var(--tw-drop-shadow-color,oklab(0%0 0/.5)))drop-shadow(0 1px 1px \
       var(--tw-drop-shadow-color,oklab(0%0 0/.5)))";
      "--tw-drop-shadow:drop-shadow(0 1px 2px #0000001a)drop-shadow(0 1px 1px \
       #0000000f)";
      chain_min;
    ]

(* A fractional opacity modifier keeps its fraction: drop-shadow/12.5 -> alpha
   12.5%, not the truncated 12%. *)
let test_drop_shadow_fractional_alpha () =
  (* The fraction has to survive into the size channel too, which the affix on
     the alpha channel alone never said. *)
  Test_helpers.check_declarations "drop-shadow/12.5"
    [
      "--tw-drop-shadow-alpha:12.5%";
      "--tw-drop-shadow-size:drop-shadow(0 1px 2px \
       var(--tw-drop-shadow-color,oklab(0%0 0/.125)))drop-shadow(0 1px 1px \
       var(--tw-drop-shadow-color,oklab(0%0 0/.125)))";
      "--tw-drop-shadow:drop-shadow(0 1px 2px #0000001a)drop-shadow(0 1px 1px \
       #0000000f)";
      chain_min;
    ]

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
  (* The whole list, which also says the utility writes the backdrop chain and
     not the plain [filter] one. *)
  Test_helpers.check_declarations "backdrop-blur-sm"
    [
      "--tw-backdrop-blur:blur(var(--blur-sm))";
      "-webkit-backdrop-filter:" ^ backdrop_chain_min;
      "backdrop-filter:" ^ backdrop_chain_min;
    ];
  (* The token's own binding is a :root declaration, left out by design. *)
  Alcotest.(check bool)
    "emits --blur-sm:8px" true
    (Astring.String.is_infix ~affix:"--blur-sm:8px" css)

(* A drop-shadow colour the theme has no token for, and a named size with an
   alpha: both were unknown classes. The size form replaces the shadow's own
   colour with black at that alpha and leaves the theme token out. *)
let test_drop_shadow_keyword_and_alpha () =
  let keyword cls colour mixed =
    Test_helpers.check_declarations cls
      [
        "--tw-drop-shadow-color:" ^ colour;
        "--tw-drop-shadow-color:color-mix(in oklab," ^ mixed
        ^ " var(--tw-drop-shadow-alpha),transparent)";
        "--tw-drop-shadow:var(--tw-drop-shadow-size)";
      ]
  in
  keyword "drop-shadow-current" "currentColor" "currentcolor";
  keyword "drop-shadow-transparent" "transparent" "transparent";
  (* The size form replaces the shadow's own colour and leaves the theme token
     out, which the whole list says: the [--drop-shadow-xl:] search it replaces
     was for a :root binding the list does not reach anyway, and the size affix
     stopped before the value that carries the alpha. *)
  Test_helpers.check_declarations "drop-shadow-xl/25"
    [
      "--tw-drop-shadow-alpha:25%";
      "--tw-drop-shadow-size:drop-shadow(0 9px 7px \
       var(--tw-drop-shadow-color,oklab(0%0 0/.25)))";
      "--tw-drop-shadow:var(--tw-drop-shadow-size)";
      chain_min;
    ]

(* Arbitrary filter arguments are safe token streams; the browser, rather than
   the generator, applies the function's value grammar. *)
let test_arbitrary_amount_token_streams () =
  check "brightness-[abc]";
  check "invert-[xyz]";
  check "backdrop-sepia-[nope]";
  check "drop-shadow-[<value>]";
  check "brightness-[1.5]";
  check "saturate-[150%]";
  check "brightness-[var(--x)]"

(* An unterminated comment is complete at the end of an arbitrary value. When
   the value is embedded in a filter function, the generated closing parenthesis
   must remain outside that comment. *)
let test_arbitrary_amount_unterminated_comment () =
  Test_helpers.check_declarations "backdrop-brightness-[1px/*x]"
    [
      "--tw-backdrop-brightness:brightness(1px)";
      "-webkit-backdrop-filter:" ^ backdrop_chain_min;
      "backdrop-filter:" ^ backdrop_chain_min;
    ]

(* An arbitrary filter spells its spaces with [_], so a multi-function chain
   like filter-[blur(4px)_saturate(150%)] has to be decoded before it is parsed.
   Without that the whole class parsed as nothing and emitted an empty rule. *)
let test_arbitrary_filter_chain () =
  (* An arbitrary filter replaces the chain rather than joining it, so the whole
     list is one declaration - two for backdrop, which aliases its property for
     WebKit. That is what the affixes could not say, and "emitted an empty rule"
     is the failure they were written against. *)
  let filters cls value =
    Test_helpers.check_declarations ~minify:false cls [ "filter: " ^ value ]
  in
  let backdrop_filters cls value =
    Test_helpers.check_declarations ~minify:false cls
      [ "-webkit-backdrop-filter: " ^ value; "backdrop-filter: " ^ value ]
  in
  filters "filter-[blur(4px)_saturate(150%)]" "blur(4px) saturate(150%)";
  backdrop_filters "backdrop-filter-[blur(4px)_saturate(150%)]"
    "blur(4px) saturate(150%)";
  (* single-function and var() forms are unchanged *)
  filters "filter-[blur(4px)]" "blur(4px)";
  filters "filter-[var(--my-filter)]" "var(--my-filter)";
  backdrop_filters "backdrop-filter-[blur(4px)]" "blur(4px)";
  backdrop_filters "backdrop-filter-[var(--x)]" "var(--x)"

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
  (* A search for ":" anywhere in the sheet is satisfied by the @layer line, so
     this said nothing at all. The declaration list is the assertion. *)
  let non_empty cls =
    match Test_helpers.declarations_of_class cls with
    | [] -> Alcotest.failf "%s emits no declaration" cls
    | _ :: _ -> ()
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

(* The arbitrary hue-rotate argument is forwarded even when it is not a typed
   angle. *)
let test_arbitrary_angle_token_streams () =
  List.iter check
    [
      "hue-rotate-[2]";
      "hue-rotate-[2zz]";
      "hue-rotate-[deg]";
      "backdrop-hue-rotate-[2px]";
    ]

(* [blur-[...]] takes a length. A bracket the length grammar cannot read was
   accepted and then raised out of [to_css], which is a pure conversion. Reading
   the bracket with cascade's grammar also earns [calc()] and the units the
   hand-rolled reader never took. *)
let test_arbitrary_blur_token_streams () =
  let renders cls =
    match Tw.of_string cls with
    | Ok u -> ignore (Tw.to_css ~base:false [ u ] |> Tw.Css.to_string)
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  renders "blur-[foo]";
  renders "blur-[red]";
  renders "blur-[1]";
  renders "backdrop-blur-[foo]";
  renders "backdrop-blur-[a,b]";
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
  (* Channel then chain: "filter chain included" is what this test is for, and
     an affix on the channel alone never said the chain was there. *)
  Test_helpers.check_declarations ~theme ~minify:false "blur-soft"
    [ "--tw-blur: blur(var(--blur-soft))"; chain ];
  Alcotest.(check bool)
    "an undeclared blur name is rejected" true
    (Result.is_error (Tw.of_string ~theme "blur-nope"))

let tests =
  [
    test_case "arbitrary angle class name" `Quick
      test_arbitrary_angle_class_name;
    test_case "arbitrary angle token streams" `Quick
      test_arbitrary_angle_token_streams;
    test_case "arbitrary blur token streams" `Quick
      test_arbitrary_blur_token_streams;
    test_case "drop-shadow keyword color and alpha" `Quick
      test_drop_shadow_keyword_and_alpha;
    test_case "filters render like Tailwind" `Slow rendering_matches_tailwind;
    test_case "blur" `Quick test_blur;
    test_case "arbitrary amount token streams" `Quick
      test_arbitrary_amount_token_streams;
    test_case "arbitrary amount unterminated comment" `Quick
      test_arbitrary_amount_unterminated_comment;
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
