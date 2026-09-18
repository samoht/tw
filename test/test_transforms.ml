open Alcotest

let check = Test_helpers.check_handler_roundtrip (module Tw.Transforms.Handler)

let test_translate_rotate () =
  check "translate-x-4";
  check "rotate-90";
  (* the v4.3.3 none keyword on each transform property *)
  check "translate-none";
  check "rotate-none";
  check "scale-none"

(* translate-px (all axes) and the negative px / arbitrary-value variants used
   to be unknown classes: only the per-axis px and positive arbitrary forms
   parsed, and negatives only accepted [var(...)] brackets, not lengths like
   [110%]. *)
let test_translate_px_and_neg_arbitrary () =
  check "translate-px";
  check "-translate-px";
  check "-translate-x-px";
  check "-translate-y-px";
  check "-translate-y-[110%]";
  check "-translate-x-[3px]";
  (* A negative value inside the bracket (not a leading -) parses, and the raw
     token is kept verbatim in the class name (-0.5px, not the folded -.5px). *)
  check "translate-x-[-0.5px]";
  check "translate-y-[-110%]";
  check "translate-x-[-1.15rem]";
  (* The whole list, which is what says [translate-px] sets *both* axes and a
     single-axis class sets one: the substring could not tell those apart. The
     composed [translate] travels with either. *)
  let composed = "translate: var(--tw-translate-x) var(--tw-translate-y)" in
  Test_helpers.check_declarations ~minify:false "translate-px"
    [ "--tw-translate-x: 1px"; "--tw-translate-y: 1px"; composed ];
  Test_helpers.check_declarations ~minify:false "-translate-y-[110%]"
    [ "--tw-translate-y: calc(110% * -1)"; composed ];
  Test_helpers.check_declarations ~minify:false "translate-x-[-0.5px]"
    [ "--tw-translate-x: -.5px"; composed ]

(* The near/midrange/distant perspective keywords reference their theme token,
   like the dramatic/normal ones already did. *)
let test_perspective_keywords () =
  check "perspective-near";
  check "perspective-midrange";
  check "perspective-distant";
  let css cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string ~minify:true
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  Test_helpers.check_declarations "perspective-near"
    [ "perspective:var(--perspective-near)" ];
  (* The token's own binding is a :root declaration, which the list above leaves
     out by design, so this one stays a substring. *)
  Alcotest.(check bool)
    "perspective-distant defines the 1200px token" true
    (Astring.String.is_infix ~affix:"--perspective-distant:1200px"
       (css "perspective-distant"))

let test_perspective_candidate_order () =
  Test_helpers.check_class_order ~test_name:"perspective candidate order"
    [
      "perspective-none";
      "perspective-near";
      "perspective-dramatic";
      "perspective-normal";
      "perspective-distant";
      "perspective-midrange";
    ]

(* A negative half step is a negative translate and sorts in the negative band
   of its axis, by magnitude with the integers, as Tailwind sorts it: the sign
   is the candidate's, whatever the step's shape. *)
let test_negative_step_translate_order () =
  Test_helpers.check_class_order
    ~test_name:"negative translate steps sort by magnitude"
    [
      "-translate-y-10";
      "-translate-y-1.5";
      "-translate-y-2";
      "-translate-y-0.5";
      "-translate-y-1";
      "translate-y-0.5";
      "translate-y-1";
    ];
  Test_helpers.check_class_order
    ~test_name:"negative translate-x steps sort by magnitude"
    [ "-translate-x-1"; "-translate-x-0.5"; "translate-x-0.5" ];
  Test_helpers.check_class_order
    ~test_name:"negative translate-z steps sort by magnitude"
    [ "-translate-z-1"; "-translate-z-0.5"; "translate-z-0.5" ];
  Test_helpers.check_class_order ~test_name:"the hover pair the site carries"
    [ "hover:-translate-y-1"; "hover:-translate-y-0.5" ]

(* A translate fraction is any numerator over any denominator, and the translate
   family writes the division out rather than folding it, so even the zero
   denominator Tailwind emits reads here. *)
let test_any_translate_fraction () =
  check "translate-x-0/2";
  check "translate-x-1/7";
  check "translate-y-13/17";
  check "translate-1/0";
  check "-translate-x-0/2"

let test_of_string_invalid () =
  (* Invalid transform utilities *)
  let test_invalid input =
    let class_name = String.concat "-" input in
    match Tw.Transforms.Handler.of_class Tw.Scheme.default class_name with
    | Ok _ -> fail ("Expected error for: " ^ String.concat "-" input)
    | Error _ -> ()
  in

  (* Invalid translate - missing value *)
  test_invalid [ "translate"; "x" ];
  test_invalid [ "translate"; "y" ];
  test_invalid [ "translate"; "z" ];

  (* Invalid rotate - missing value *)
  test_invalid [ "rotate" ];
  test_invalid [ "rotate"; "x" ];
  test_invalid [ "rotate"; "y" ];
  test_invalid [ "rotate"; "z" ];

  (* Invalid scale - missing value *)
  test_invalid [ "scale" ];
  test_invalid [ "scale"; "x" ];
  test_invalid [ "scale"; "y" ];
  test_invalid [ "scale"; "z" ];

  (* Invalid skew - missing value *)
  test_invalid [ "skew"; "x" ];
  test_invalid [ "skew"; "y" ];

  (* Invalid perspective *)
  test_invalid [ "perspective" ];
  test_invalid [ "perspective"; "123" ];
  test_invalid [ "perspective"; "potato" ];

  (* Invalid perspective origin *)
  test_invalid [ "perspective"; "origin" ];
  test_invalid [ "perspective"; "origin"; "invalid" ];

  (* Invalid transform style *)
  test_invalid [ "transform"; "style" ];
  test_invalid [ "transform"; "style"; "invalid" ];

  (* Invalid prefixes *)
  test_invalid [ "translate" ];
  (* Missing axis *)
  test_invalid [ "scale"; "invalid"; "100" ];
  (* Invalid axis *)
  test_invalid []
(* Empty *)

let suborder_matches_tailwind () =
  let open Tw in
  let shuffled =
    Test_helpers.shuffle [ translate_x 4; translate_y 2; rotate 90; scale 50 ]
  in

  Test_helpers.check_ordering_matches
    ~test_name:"transforms suborder matches Tailwind" shuffled

(* Every transform utility writes into the same --tw-* slots and the shared
   transform property, so the composed matrix is what has to agree. *)
let rendering_matches_tailwind () =
  let classes =
    [
      "translate-x-4";
      "translate-y-2";
      "-translate-x-2";
      "translate-4";
      "rotate-45";
      "-rotate-90";
      "scale-50";
      "scale-x-75";
      "scale-y-125";
      "skew-x-3";
      "skew-y-6";
      "origin-center";
      "origin-top-right";
      "transform-gpu";
      "transform-none";
    ]
  in
  Test_helpers.check_rendering_matches
    ~test_name:"transforms render like Tailwind"
    (List.map (fun c -> Result.get_ok (Tw.of_string c)) classes)

(* skew_x/skew_y (int) and the transform-origin constructors are newly exposed
   in tw.mli; check they agree with the parser on class names. *)
let test_typed () =
  Test_helpers.check_typed_class "skew-x-3" (Tw.skew_x 3);
  Test_helpers.check_typed_class "skew-y-6" (Tw.skew_y 6);
  Test_helpers.check_typed_class "origin-center" Tw.origin_center;
  Test_helpers.check_typed_class "origin-top-right" Tw.origin_top_right;
  Test_helpers.check_typed_class "origin-bottom-left" Tw.origin_bottom_left

(* [--tw-translate-*] is a custom property, an opaque token stream where [0] and
   [0px] are different tokens, so the zero has to keep its unit. Leaving it to
   the length-level zero fold emits a bare [0] and diverges from Tailwind. *)
let test_translate_zero_keeps_unit () =
  Test_helpers.check_declarations ~minify:false "translate-x-0"
    [
      "--tw-translate-x: 0px";
      "translate: var(--tw-translate-x) var(--tw-translate-y)";
    ]

(* Bare-integer translate-N / -translate-N set both axes to calc(var(--spacing)
   * n); they used to be unknown classes (only the per-axis translate-x-N /
   translate-y-N parsed). *)
let test_translate_spacing () =
  check "translate-2";
  check "translate-8";
  check "translate-60";
  check "-translate-4";
  check "-translate-6";
  Test_helpers.check_declarations ~minify:false "translate-2"
    [
      "--tw-translate-x: calc(var(--spacing) * 2)";
      "--tw-translate-y: calc(var(--spacing) * 2)";
      "translate: var(--tw-translate-x) var(--tw-translate-y)";
    ];
  Test_helpers.check_declarations ~minify:false "-translate-4"
    [
      "--tw-translate-x: calc(var(--spacing) * -4)";
      "--tw-translate-y: calc(var(--spacing) * -4)";
      "translate: var(--tw-translate-x) var(--tw-translate-y)";
    ]

(* A fractional spacing step on translate, in both signs: translate-x-0.5 and
   -translate-y-0.5 used to be unknown classes since the axis took an int. The
   unit step folds to the bare variable, as Tailwind writes it. *)
let test_translate_spacing_steps () =
  (* Channel then the [translate] shorthand that reads it. A z step widens the
     shorthand to three channels, which the affixes could not say. *)
  let xy cls channel value =
    Test_helpers.check_declarations cls
      [
        channel ^ ":" ^ value;
        "translate:var(--tw-translate-x)var(--tw-translate-y)";
      ]
  in
  xy "translate-x-0.5" "--tw-translate-x" "calc(var(--spacing)*.5)";
  xy "-translate-y-0.5" "--tw-translate-y" "calc(var(--spacing)*-.5)";
  xy "translate-x-1" "--tw-translate-x" "var(--spacing)";
  Test_helpers.check_declarations "translate-z-0.5"
    [
      "--tw-translate-z:calc(var(--spacing)*.5)";
      "translate:var(--tw-translate-x)var(--tw-translate-y)var(--tw-translate-z)";
    ];
  Alcotest.(check string)
    "-translate-y-0.5 round-trips" "-translate-y-0.5"
    (Tw.pp (Result.get_ok (Tw.of_string "-translate-y-0.5")));
  Alcotest.(check string)
    "translate-z-0.5 round-trips" "translate-z-0.5"
    (Tw.pp (Result.get_ok (Tw.of_string "translate-z-0.5")))

(* [translate_x']/[translate_y']/[translate_z'] take a half-step float; the int
   base keeps emitting what it always did. A whole-number float still keeps the
   int constructor's own shortcuts (e.g. translate-x-1 is the bare variable, not
   calc(var(--spacing) * 1)). *)
let test_translate_prime () =
  let check_class expected value =
    Alcotest.(check string) expected expected (Tw.pp value)
  in
  check_class "translate-x-0.5" (Tw.translate_x' 0.5);
  check_class "-translate-y-0.5" (Tw.translate_y' (-0.5));
  check_class "translate-z-0.5" (Tw.translate_z' 0.5);
  check_class "translate-x-4" (Tw.translate_x' 4.0);
  check_class "translate-x-4" (Tw.translate_x 4);
  Test_helpers.check_declarations "translate-x-4"
    [
      "--tw-translate-x:calc(var(--spacing)*4)";
      "translate:var(--tw-translate-x)var(--tw-translate-y)";
    ]

(* [perspective-none] resolves to whatever a project declared [--perspective-
   none] to be. Reading that value back with a px-only test lost every other
   spelling to a zero before the theme layer restored the project's own text. *)
let perspective_none_bound_value value =
  let theme =
    Tw.Scheme.with_overrides Tw.Scheme.default [ ("perspective-none", value) ]
  in
  let u =
    Result.get_ok (Tw.Transforms.Handler.of_class theme "perspective-none")
  in
  match Tw.Transforms.Handler.to_style theme u with
  | Tw.Style.Style { props; _ } ->
      List.filter_map
        (fun d ->
          match Tw.Css.custom_declaration_name d with
          | Some "--perspective-none" ->
              Some (String.trim (Tw.Css.declaration_value d))
          | _ -> None)
        props
  | Tw.Style.Modified _ | Tw.Style.Group _ -> []

let test_perspective_none_theme_override () =
  let binds value =
    Alcotest.(check (list string))
      ("--perspective-none: " ^ value)
      [ value ]
      (perspective_none_bound_value value)
  in
  binds "0rem";
  binds "2rem";
  binds "500px";
  binds "none";
  (* A value the length grammar cannot read falls back to the utility's own
     meaning rather than to a zero; the theme layer still emits the project's
     text over it. *)
  Alcotest.(check (list string))
    "unreadable override" [ "none" ]
    (perspective_none_bound_value "banana")

(* With no override the utility keeps its own meaning rather than referencing a
   token nothing declares. *)
let test_perspective_none_without_override () =
  Test_helpers.check_declarations "perspective-none" [ "perspective:none" ]

(* [transform-[...]], [origin-[...]] and [perspective-origin-[...]] each take a
   grammar cascade already reads, and a bracket that grammar refuses goes to the
   longhand the class names as the token stream it is. What is ruled out is the
   third answer: reading the grammar in [to_style] left such a bracket accepted
   and then raised out of [to_css], which is a pure conversion. *)
let test_invalid_arbitrary_transform () =
  let renders cls =
    match Tw.of_string cls with
    | Ok u -> ignore (Tw.to_css ~base:false [ u ] |> Tw.Css.to_string)
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  Test_helpers.check_declarations "transform-[foo]" [ "transform:foo" ];
  Test_helpers.check_declarations "transform-[1px]" [ "transform:1px" ];
  Test_helpers.check_declarations "transform-[a,b]" [ "transform:a,b" ];
  Test_helpers.check_declarations "origin-[foo]" [ "transform-origin:foo" ];
  Test_helpers.check_declarations "origin-[red]" [ "transform-origin:red" ];
  Test_helpers.check_declarations "perspective-origin-[foo]"
    [ "perspective-origin:foo" ];
  Test_helpers.check_declarations "perspective-origin-[red]"
    [ "perspective-origin:red" ];
  renders "transform-[rotate(45deg)]";
  renders "transform-[translateX(1px)_rotate(45deg)]";
  renders "origin-[50px_100px]";
  renders "origin-[center]";
  renders "perspective-origin-[50px_100px]";
  renders "perspective-origin-[bottom_right]"

(* An arbitrary transform names its class after the bracket, so the bracket has
   to come back out spelled as the author wrote it. Re-printing the parsed
   number or angle drops a redundant zero and leaves a selector the markup does
   not carry. *)
let test_arbitrary_transform_spelling () =
  List.iter
    (fun cls ->
      match Tw.of_string cls with
      | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
      | Ok u -> Alcotest.(check string) (cls ^ " round-trips") cls (Tw.pp u))
    [
      "scale-[1.5]";
      "scale-[1.50]";
      "scale-[2]";
      "scale-[1.0_2_3]";
      "scale-x-[1.50]";
      "scale-y-[1.50]";
      "rotate-[1.50deg]";
      "rotate-[1.50_2_3_45deg]";
      "-rotate-[1.50deg]";
      "rotate-x-[1.50deg]";
      "rotate-y-[1.50turn]";
      "rotate-z-[1.50grad]";
      "-rotate-x-[1.50deg]";
      "-rotate-y-[1.50deg]";
      "-rotate-z-[1.50deg]";
      "skew-[1.50deg]";
      "skew-x-[1.50deg]";
      "skew-y-[1.50deg]";
      (* [perspective] takes a length rather than a number or an angle, and its
         printer canonicalises one the same way. *)
      "perspective-[1.50px]";
      "perspective-[0.5rem]";
      "perspective-[100px]";
    ]

(* Tailwind forwards declaration-safe arbitrary transform token streams even
   when they are invalid for the target property. The browser then discards the
   invalid declaration. *)
let test_arbitrary_transform_token_streams () =
  let emits cls decl = Test_helpers.check_declarations cls [ decl ] in
  emits "scale-[abc]" "scale:abc";
  emits "rotate-[abc]" "rotate:abc";
  emits "rotate-[1.5px]" "rotate:1.5px";
  (* A bare skew sets both axes and the transform chain that reads them, which
     the affix on one channel never said. *)
  Test_helpers.check_declarations "skew-[1.5]"
    [
      "--tw-skew-x:skewX(1.5)";
      "--tw-skew-y:skewY(1.5)";
      "transform:var(--tw-rotate-x,) var(--tw-rotate-y,) var(--tw-rotate-z,) \
       var(--tw-skew-x,) var(--tw-skew-y,)";
    ]

(* A [--perspective-*] token the project declared in its [@theme] names a depth
   the built-in scale has no slot for. Tailwind generates the utility from it;
   tw rejected the class outright. *)
let test_project_perspective_token () =
  let theme =
    Tw.Scheme.with_overrides Tw.Scheme.default
      [ ("perspective-deep", "1200px") ]
  in
  Test_helpers.check_declarations ~theme ~minify:false "perspective-deep"
    [ "perspective: var(--perspective-deep)" ];
  Alcotest.(check bool)
    "an undeclared perspective name is rejected" true
    (Result.is_error (Tw.of_string ~theme "perspective-nope"))

(* The [@property] block for the five rotate/skew channels belongs to
   [transform]. [transform-cpu] and [transform-gpu] only read them, and asking
   for the rules there put a whole [@layer properties] and five [@property]
   rules in a sheet whose one transform utility was either of those two. *)
let test_property_rules_belong_to_transform () =
  let property_rules cls =
    match Tw.of_string cls with
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
    | Ok u ->
        let css = Tw.to_css ~base:false [ u ] |> Tw.Css.to_string in
        List.length
          (List.filter
             (fun l -> Astring.String.is_prefix ~affix:"@property" l)
             (String.split_on_char '\n' css))
  in
  Alcotest.(check int) "transform declares them" 5 (property_rules "transform");
  Alcotest.(check int)
    "transform-cpu does not" 0
    (property_rules "transform-cpu");
  Alcotest.(check int)
    "transform-gpu does not" 0
    (property_rules "transform-gpu");
  (* A utility that sets one of the channels still brings its rule along. *)
  Alcotest.(check bool)
    "rotate-x-30 declares its own" true
    (property_rules "rotate-x-30" > 0)

(* The 3D form of [rotate] is four space-separated components, and the [\_] that
   spells a literal underscore belongs inside one of them rather than splitting
   it. A component carrying one is not a CSS number, so the value stays as
   written instead of being read as an axis and an angle. *)
let test_rotate_underscore_escape () =
  let has cls decl =
    Test_helpers.check_declarations ~minify:false cls [ decl ]
  in
  has {|rotate-[1_1_1\_2_45deg]|} "rotate: 1 1 1_2 45deg";
  has {|rotate-[var(--a\_b)]|} "rotate: var(--a_b)"

(* A single-axis scale bracket is a token stream Tailwind hands to the custom
   property unvalidated. It goes through the arbitrary-value pipeline, not
   OCaml's [Float.of_string_opt], so [calc()] reaches the property and a hex
   spelling is emitted as written rather than folded to [4]. *)
let test_arbitrary_scale_axis_token_stream () =
  Test_helpers.check_declarations "scale-x-[calc(1+2)]"
    [ "--tw-scale-x:calc(1 + 2)"; "scale:var(--tw-scale-x)var(--tw-scale-y)" ];
  Test_helpers.check_declarations "scale-x-[0x4]"
    [ "--tw-scale-x:0x4"; "scale:var(--tw-scale-x)var(--tw-scale-y)" ];
  Test_helpers.check_declarations "scale-y-[0x4]"
    [ "--tw-scale-y:0x4"; "scale:var(--tw-scale-x)var(--tw-scale-y)" ]

(* [origin-], [perspective-origin-] and [transform-] read their bracket through
   the arbitrary-value pipeline. Applying underscore decoding alone leaves
   [calc(1px+1px)] without the spaces CSS math wants and [--spacing(4)]
   unexpanded, so cascade's grammar refuses the value and the class with it. *)
let test_arbitrary_transform_reads_the_whole_bracket () =
  Test_helpers.check_declarations "origin-[calc(1px+1px)_calc(2px+2px)]"
    [ "transform-origin:calc(1px + 1px) calc(2px + 2px)" ];
  Test_helpers.check_declarations "origin-[--spacing(4)_--spacing(2)]"
    [ "transform-origin:calc(var(--spacing)*4) calc(var(--spacing)*2)" ];
  Test_helpers.check_declarations
    "perspective-origin-[calc(1px+1px)_calc(2px+2px)]"
    [ "perspective-origin:calc(1px + 1px) calc(2px + 2px)" ];
  Test_helpers.check_declarations "transform-[translateX(calc(1px+1px))]"
    [ "transform:translateX(calc(1px + 1px))" ]

(* A data-type hint chooses which longhand a bracket lands in and says nothing
   about the value. The three families reading a bracket through one typed
   cursor each write one longhand, so every hint lands there and the reader is
   handed what follows it; the hint stays in the class name, which is what the
   markup carries. All three were given the hint as well, read nothing, and the
   classes were refused where Tailwind writes the value through. *)
let test_transform_brackets_peel_a_hint () =
  List.iter
    (fun (cls, decl) ->
      match Tw.of_string cls with
      | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
      | Ok u ->
          Alcotest.(check string) "class round-trips" cls (Tw.pp u);
          (* The whole list, which is where "lands in one longhand" is said: an
             affix passes just as well beside a second declaration the hint was
             not supposed to produce. *)
          Test_helpers.check_declarations ~minify:false cls [ decl ])
    [
      ("origin-[position:top]", "transform-origin: top");
      ("origin-[foo:top]", "transform-origin: top");
      ("perspective-origin-[foo:top]", "perspective-origin: top");
      ("transform-[foo:scaleX(2)]", "transform: scaleX(2)");
    ]

let tests =
  [
    test_case "rotate underscore escape" `Quick test_rotate_underscore_escape;
    test_case "property rules belong to transform" `Quick
      test_property_rules_belong_to_transform;
    test_case "invalid arbitrary transform" `Quick
      test_invalid_arbitrary_transform;
    test_case "perspective-none theme override" `Quick
      test_perspective_none_theme_override;
    test_case "perspective-none without override" `Quick
      test_perspective_none_without_override;
    test_case "translate spacing steps" `Quick test_translate_spacing_steps;
    test_case "translate half-step typed constructors" `Quick
      test_translate_prime;
    test_case "translate zero keeps its unit" `Quick
      test_translate_zero_keeps_unit;
    test_case "translate spacing (both axes)" `Quick test_translate_spacing;
    test_case "translate+rotate" `Quick test_translate_rotate;
    test_case "perspective keywords" `Quick test_perspective_keywords;
    test_case "perspective candidate order" `Quick
      test_perspective_candidate_order;
    test_case "negative translate steps sort by magnitude" `Quick
      test_negative_step_translate_order;
    test_case "translate-px and negative arbitrary" `Quick
      test_translate_px_and_neg_arbitrary;
    test_case "any translate fraction" `Quick test_any_translate_fraction;
    test_case "of_string invalid cases" `Quick test_of_string_invalid;
    test_case "typed constructors" `Quick test_typed;
    test_case "transforms suborder matches Tailwind" `Quick
      suborder_matches_tailwind;
    test_case "arbitrary transform spelling" `Quick
      test_arbitrary_transform_spelling;
    test_case "arbitrary transform reads the whole bracket" `Quick
      test_arbitrary_transform_reads_the_whole_bracket;
    test_case "arbitrary transform token streams" `Quick
      test_arbitrary_transform_token_streams;
    test_case "arbitrary scale axis token stream" `Quick
      test_arbitrary_scale_axis_token_stream;
    test_case "project perspective token" `Quick test_project_perspective_token;
    test_case "transform brackets peel a data-type hint" `Quick
      test_transform_brackets_peel_a_hint;
    test_case "transforms render like Tailwind" `Slow rendering_matches_tailwind;
  ]

let suite = ("transforms", tests)
