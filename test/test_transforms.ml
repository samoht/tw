open Alcotest

let check class_name =
  match Tw.Transforms.Handler.of_class Tw.Scheme.default class_name with
  | Ok t ->
      check string "transforms class" class_name
        (Tw.Transforms.Handler.to_class t)
  | Error (`Msg msg) -> fail msg

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
  let css cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  Alcotest.(check bool)
    "translate-px sets both axes to 1px" true
    (Astring.String.is_infix ~affix:"--tw-translate-x: 1px" (css "translate-px"));
  Alcotest.(check bool)
    "-translate-y-[110%] negates the value" true
    (Astring.String.is_infix ~affix:"calc(110% * -1)"
       (css "-translate-y-[110%]"));
  Alcotest.(check bool)
    "translate-x-[-0.5px] keeps the negative value" true
    (Astring.String.is_infix ~affix:"--tw-translate-x: -.5px"
       (css "translate-x-[-0.5px]"))

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
  Alcotest.(check bool)
    "perspective-near references its token" true
    (Astring.String.is_infix ~affix:"perspective:var(--perspective-near)"
       (css "perspective-near"));
  Alcotest.(check bool)
    "perspective-distant defines the 1200px token" true
    (Astring.String.is_infix ~affix:"--perspective-distant:1200px"
       (css "perspective-distant"))

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
  let css = Tw.to_css ~base:false [ Tw.translate_x 0 ] |> Tw.Css.to_string in
  Alcotest.(check bool)
    "translate-x-0 writes 0px" true
    (Astring.String.is_infix ~affix:"--tw-translate-x: 0px" css)

(* Bare-integer translate-N / -translate-N set both axes to calc(var(--spacing)
   * n); they used to be unknown classes (only the per-axis translate-x-N /
   translate-y-N parsed). *)
let test_translate_spacing () =
  check "translate-2";
  check "translate-8";
  check "translate-60";
  check "-translate-4";
  check "-translate-6";
  let css cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  Alcotest.(check bool)
    "translate-2 sets both axes" true
    (Astring.String.is_infix ~affix:"--tw-translate-x: calc(var(--spacing) * 2)"
       (css "translate-2")
    && Astring.String.is_infix
         ~affix:"--tw-translate-y: calc(var(--spacing) * 2)" (css "translate-2")
    );
  Alcotest.(check bool)
    "-translate-4 negates the multiplier" true
    (Astring.String.is_infix
       ~affix:"--tw-translate-x: calc(var(--spacing) * -4)" (css "-translate-4"))

(* A fractional spacing step on translate, in both signs: translate-x-0.5 and
   -translate-y-0.5 used to be unknown classes since the axis took an int. The
   unit step folds to the bare variable, as Tailwind writes it. *)
let test_translate_spacing_steps () =
  let css cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string ~minify:true
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  let has cls affix =
    Alcotest.(check bool) cls true (Astring.String.is_infix ~affix (css cls))
  in
  has "translate-x-0.5" "--tw-translate-x:calc(var(--spacing)*.5)";
  has "-translate-y-0.5" "--tw-translate-y:calc(var(--spacing)*-.5)";
  has "translate-x-1" "--tw-translate-x:var(--spacing)";
  has "translate-z-0.5" "--tw-translate-z:calc(var(--spacing)*.5)";
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
  let css u = Tw.to_css ~base:false [ u ] |> Tw.Css.to_string ~minify:true in
  Alcotest.(check bool)
    "translate_x' 4.0 keeps the bare-var shortcut for 1, calc for others" true
    (Astring.String.is_infix ~affix:"--tw-translate-x:calc(var(--spacing)*4)"
       (css (Tw.translate_x' 4.0)))

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
  let css =
    Tw.to_css ~base:false [ Result.get_ok (Tw.of_string "perspective-none") ]
    |> Tw.Css.to_string ~minify:true
  in
  Alcotest.(check bool)
    "perspective:none" true
    (Astring.String.is_infix ~affix:"perspective:none" css)

(* [transform-[...]], [origin-[...]] and [perspective-origin-[...]] each take a
   grammar cascade already reads. Reading it in [to_style] left a bracket the
   grammar refuses accepted and then raising out of [to_css], which is a pure
   conversion. *)
let test_invalid_arbitrary_transform () =
  let rejected cls =
    match Tw.of_string cls with
    | Ok u ->
        Alcotest.failf "expected %s to be rejected, got %s" cls
          (Tw.to_css ~base:false [ u ] |> Tw.Css.to_string ~minify:true)
    | Error _ -> ()
  in
  let renders cls =
    match Tw.of_string cls with
    | Ok u -> ignore (Tw.to_css ~base:false [ u ] |> Tw.Css.to_string)
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  rejected "transform-[foo]";
  rejected "transform-[1px]";
  rejected "transform-[a,b]";
  rejected "origin-[foo]";
  rejected "origin-[red]";
  rejected "perspective-origin-[foo]";
  rejected "perspective-origin-[red]";
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

(* The bracket holds a number or an angle, so a word is not a transform. *)
let test_arbitrary_transform_rejects_non_number () =
  List.iter
    (fun cls ->
      match Tw.of_string cls with
      | Ok u -> Alcotest.failf "%s parsed as %s" cls (Tw.pp u)
      | Error (`Msg _) -> ())
    [ "scale-[abc]"; "rotate-[abc]"; "skew-[1.5]"; "rotate-[1.5px]" ]

(* A [--perspective-*] token the project declared in its [@theme] names a depth
   the built-in scale has no slot for. Tailwind generates the utility from it;
   tw rejected the class outright. *)
let test_project_perspective_token () =
  let theme =
    Tw.Scheme.with_overrides Tw.Scheme.default
      [ ("perspective-deep", "1200px") ]
  in
  let css cls =
    match Tw.of_string ~theme cls with
    | Ok u -> Tw.to_css ~theme ~base:false [ u ] |> Tw.Css.to_string
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  Alcotest.(check bool)
    "perspective-deep references its token" true
    (Astring.String.is_infix ~affix:"perspective: var(--perspective-deep)"
       (css "perspective-deep"));
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

let tests =
  [
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
    test_case "translate-px and negative arbitrary" `Quick
      test_translate_px_and_neg_arbitrary;
    test_case "of_string invalid cases" `Quick test_of_string_invalid;
    test_case "typed constructors" `Quick test_typed;
    test_case "transforms suborder matches Tailwind" `Quick
      suborder_matches_tailwind;
    test_case "arbitrary transform spelling" `Quick
      test_arbitrary_transform_spelling;
    test_case "arbitrary transform rejects non-number" `Quick
      test_arbitrary_transform_rejects_non_number;
    test_case "project perspective token" `Quick test_project_perspective_token;
    test_case "transforms render like Tailwind" `Slow rendering_matches_tailwind;
  ]

let suite = ("transforms", tests)
