let check = Test_helpers.check_handler_roundtrip (module Tw.Transitions.Handler)

let test_roundtrip () =
  check "transition-none";
  check "transition-all";
  check "transition-colors";
  check "transition-opacity";
  check "transition-shadow";
  check "transition-transform";
  check "transition";
  check "duration-150";
  check "duration-300";
  check "delay-150";
  check "delay-300";
  (* Arbitrary delay accepts both time units and var(). *)
  check "delay-[300ms]";
  check "delay-[var(--d)]";
  check "ease-linear";
  check "ease-in";
  check "ease-out";
  check "ease-in-out";
  (* the initial keyword resets the duration/ease channel *)
  check "duration-initial";
  check "ease-initial"

let test_invalid () =
  Test_helpers.check_invalid_input (module Tw.Transitions.Handler) "duration";
  Test_helpers.check_invalid_input (module Tw.Transitions.Handler) "delay";
  Test_helpers.check_invalid_input (module Tw.Transitions.Handler) "ease"

(* duration-initial / ease-initial reset their channel var to the CSS initial
   keyword. *)
let test_initial_resets () =
  let css cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string ~minify:true
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  Alcotest.(check bool)
    "duration-initial sets --tw-duration:initial" true
    (Astring.String.is_infix ~affix:"--tw-duration:initial"
       (css "duration-initial"));
  Alcotest.(check bool)
    "ease-initial sets --tw-ease:initial" true
    (Astring.String.is_infix ~affix:"--tw-ease:initial" (css "ease-initial"))

(* [transition-[...]] takes property names, so the docs' [<value>] placeholder
   is not one; it used to reach the sheet as transition-property: <value>. *)
let test_invalid_arbitrary_property () =
  let rejected cls =
    match Tw.of_string cls with
    | Ok _ -> Alcotest.failf "expected %s to be rejected" cls
    | Error _ -> ()
  in
  let accepted cls =
    match Tw.of_string cls with
    | Ok _ -> ()
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  rejected "transition-[<value>]";
  accepted "transition-[opacity]";
  accepted "transition-[opacity,transform]";
  accepted "transition-[var(--x)]"

(* [ease-[...]] takes a timing function. A bracket the timing-function grammar
   cannot read used to be accepted and then raise out of [to_css], which is a
   pure conversion. *)
let test_invalid_arbitrary_ease () =
  let rejected cls =
    match Tw.of_string cls with
    | Ok _ -> Alcotest.failf "expected %s to be rejected" cls
    | Error _ -> ()
  in
  let renders cls =
    match Tw.of_string cls with
    | Ok u -> ignore (Tw.to_css ~base:false [ u ] |> Tw.Css.to_string)
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  rejected "ease-[foo]";
  rejected "ease-[1]";
  rejected "ease-[50%]";
  rejected "ease-[]";
  renders "ease-[linear]";
  renders "ease-[cubic-bezier(0.4,0,0.2,1)]";
  renders "ease-[steps(4,end)]";
  renders "ease-[var(--my-ease)]"

(* CSS Easing 1 sec. 2: an omitted <step-position> defaults to [end]. Tailwind's
   own minifier makes that default explicit, so a bracket steps() without a
   position must render the same explicit keyword tw's sheet would otherwise
   silently disagree over. *)
let test_arbitrary_ease_steps_default_position () =
  let css cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  Alcotest.(check bool)
    "an omitted step position becomes an explicit end" true
    (Astring.String.is_infix ~affix:"transition-timing-function: steps(4, end)"
       (css "ease-[steps(4)]"))

(* An [--ease-*] token the project declared in its [@theme] names a timing
   function the built-in scale has no slot for. Tailwind generates the utility
   from it, channel variable included; tw rejected the class outright. *)
let test_project_ease_token () =
  let theme =
    Tw.Scheme.with_overrides Tw.Scheme.default
      [ ("ease-snap", "cubic-bezier(0.2, 0, 0, 1)") ]
  in
  let css cls =
    match Tw.of_string ~theme cls with
    | Ok u -> Tw.to_css ~theme ~base:false [ u ] |> Tw.Css.to_string
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  let out = css "ease-snap" in
  Alcotest.(check bool)
    "sets the channel" true
    (Astring.String.is_infix ~affix:"--tw-ease: var(--ease-snap)" out);
  Alcotest.(check bool)
    "sets the timing function" true
    (Astring.String.is_infix
       ~affix:"transition-timing-function: var(--ease-snap)" out);
  Alcotest.(check bool)
    "an undeclared ease name is rejected" true
    (Result.is_error (Tw.of_string ~theme "ease-nope"))

(* Every transition rule reads [--default-transition-duration] and
   [--default-transition-timing-function] through a fallback, so the theme has
   to declare them whenever one is emitted. Which classes need them was read off
   the name of the emitted class, and a variant renames that class, so
   [hover:transition] left both undeclared: the rule then fell back to a
   variable nothing set and the element did not animate at all. *)
let test_default_transition_theme_survives_a_variant () =
  let declares cls =
    match Tw.of_string cls with
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
    | Ok u ->
        let css = Tw.to_css ~base:true [ u ] |> Tw.Css.to_string in
        Astring.String.is_infix ~affix:"--default-transition-duration:" css
        && Astring.String.is_infix
             ~affix:"--default-transition-timing-function:" css
  in
  List.iter
    (fun cls ->
      Alcotest.(check bool) (cls ^ " declares the defaults") true (declares cls))
    [
      "transition";
      "hover:transition";
      "md:transition";
      "before:transition";
      "hover:transition-colors";
    ];
  (* [transition-none] animates nothing, so it needs neither, dressed or not. *)
  List.iter
    (fun cls ->
      Alcotest.(check bool) (cls ^ " needs no defaults") false (declares cls))
    [ "transition-none"; "hover:transition-none"; "p-4" ]

(* Values of one candidate are one registration slot in Tailwind. A numeric
   suborder per delay value used to let duration rules leak between them. *)
let test_delay_candidate_band () =
  Test_helpers.check_class_order ~test_name:"delay candidate band"
    [
      "duration-150";
      "delay-700";
      "ease-in";
      "delay-150";
      "transition";
      "duration-300";
      "delay-300";
    ]

(* Tailwind registers the channel resets near the end of its utility list, after
   logical block sizing and divide-x-reverse, but before logical inline sizing
   and perspective. *)
let test_initial_reset_boundary () =
  Test_helpers.check_class_order ~test_name:"initial reset boundary"
    [
      "perspective-normal";
      "duration-initial";
      "inline-full";
      "block-full";
      "divide-x-reverse";
      "backface-hidden";
      "ease-initial";
    ]

let tests =
  Test_helpers.standard ~roundtrip:test_roundtrip ~invalid:test_invalid
  @ [
      Alcotest.test_case "initial resets" `Quick test_initial_resets;
      Alcotest.test_case "invalid arbitrary property" `Quick
        test_invalid_arbitrary_property;
      Alcotest.test_case "invalid arbitrary ease" `Quick
        test_invalid_arbitrary_ease;
      Alcotest.test_case "arbitrary ease steps default position" `Quick
        test_arbitrary_ease_steps_default_position;
      Alcotest.test_case "project ease token" `Quick test_project_ease_token;
      Alcotest.test_case "default transition theme survives a variant" `Quick
        test_default_transition_theme_survives_a_variant;
      Alcotest.test_case "delay candidate band" `Quick test_delay_candidate_band;
      Alcotest.test_case "initial reset boundary" `Quick
        test_initial_reset_boundary;
    ]

let suite = ("transitions", tests)
