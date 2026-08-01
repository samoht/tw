open Alcotest

let check class_name =
  match Tw.Private.Transforms.Handler.of_class Tw.Theme.default class_name with
  | Ok t ->
      check string "transforms class" class_name
        (Tw.Private.Transforms.Handler.to_class t)
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
    | Ok u ->
        Tw.to_css ~config:(Tw.Config.v ~base:false ()) [ u ] |> Tw.Css.to_string
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
    | Ok u ->
        Tw.to_css ~config:(Tw.Config.v ~base:false ()) [ u ]
        |> Tw.Css.to_string ~minify:true
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
    match
      Tw.Private.Transforms.Handler.of_class Tw.Theme.default class_name
    with
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
  let css =
    Tw.to_css ~config:(Tw.Config.v ~base:false ()) [ Tw.translate_x 0 ]
    |> Tw.Css.to_string
  in
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
    | Ok u ->
        Tw.to_css ~config:(Tw.Config.v ~base:false ()) [ u ] |> Tw.Css.to_string
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
    | Ok u ->
        Tw.to_css ~config:(Tw.Config.v ~base:false ()) [ u ]
        |> Tw.Css.to_string ~minify:true
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  let has cls affix =
    Alcotest.(check bool) cls true (Astring.String.is_infix ~affix (css cls))
  in
  has "translate-x-0.5" "--tw-translate-x:calc(var(--spacing)*.5)";
  has "-translate-y-0.5" "--tw-translate-y:calc(var(--spacing)*-.5)";
  has "translate-x-1" "--tw-translate-x:var(--spacing)";
  Alcotest.(check string)
    "-translate-y-0.5 round-trips" "-translate-y-0.5"
    (Tw.to_string (Result.get_ok (Tw.of_string "-translate-y-0.5")))

let tests =
  [
    test_case "translate spacing steps" `Quick test_translate_spacing_steps;
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
    test_case "transforms render like Tailwind" `Slow rendering_matches_tailwind;
  ]

let suite = ("transforms", tests)
