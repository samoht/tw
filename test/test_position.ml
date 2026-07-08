open Alcotest

let check class_name =
  match Tw.Position.Handler.of_class Tw.Scheme.default class_name with
  | Ok util ->
      check string "positioning class" class_name
        (Tw.Position.Handler.to_class util)
  | Error (`Msg msg) -> fail msg

let test_inset_and_z () = check "inset-0"
let test_negative () = check "-top-4"

let test_position_utilities () =
  check "static";
  check "fixed";
  check "absolute";
  check "relative";
  check "sticky"

(* Arbitrary values round-trip verbatim in the class name: the leading zero of
   0.67rem (and the sign of negatives) is preserved, not re-serialised to a
   normalised .67rem that would no longer match the HTML class. *)
let test_arbitrary_roundtrip () =
  check "top-[0.67rem]";
  check "right-[-0.9rem]";
  check "bottom-[5rem]";
  check "left-[0.5rem]";
  check "inset-[0.25rem]";
  check "inset-x-[0.5rem]"

(* A named inset (top-header) parses only when the theme defines --inset-<name>
   or --spacing-<name>; stray source tokens like top-level / bottom-right must
   be rejected rather than emitting a bogus placeholder value. *)
let named_inset_requires_theme_token () =
  let reject c =
    match Tw.Position.Handler.of_class Tw.Scheme.default c with
    | Error _ -> ()
    | Ok _ -> Alcotest.failf "%s should be rejected without a theme token" c
  in
  reject "top-level";
  reject "bottom-right";
  reject "left-junk";
  let themed =
    Tw.Scheme.with_overrides Tw.Scheme.default [ ("inset-header", "2rem") ]
  in
  match Tw.Position.Handler.of_class themed "top-header" with
  | Ok _ -> ()
  | Error (`Msg m) -> Alcotest.failf "top-header with theme rejected: %s" m

(* Arbitrary var() insets (top-[var(--t)], inset-[var(--i)]) reference the var
   directly; they used to be unknown classes because the bracket parser only
   accepted numeric lengths. *)
let test_arbitrary_var () =
  let css cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  Alcotest.(check bool)
    "top-[var(--t)] sets top: var(--t)" true
    (Astring.String.is_infix ~affix:"top: var(--t)" (css "top-[var(--t)]"));
  Alcotest.(check bool)
    "inset-[var(--i)] sets inset: var(--i)" true
    (Astring.String.is_infix ~affix:"inset: var(--i)" (css "inset-[var(--i)]"));
  (* round-trips the class name *)
  check "top-[var(--t)]";
  check "left-[var(--l)]"

let suborder_matches_tailwind () =
  let open Tw in
  let shuffled =
    Test_helpers.shuffle
      [ static; fixed; absolute; relative; sticky; inset 0; top 4; left 2 ]
  in

  Test_helpers.check_ordering_matches
    ~test_name:"position suborder matches Tailwind" shuffled

(* Tailwind orders inset values negative-first (ascending magnitude), then
   positives with fractions interleaved by numerator, then arbitrary, then
   keywords: inset-0, inset-1, inset-2, inset-4, inset-40, inset-3/4, -inset-1,
   -inset-4, -inset-full. tw used a too-tight band where a numeric value (e.g.
   inset-40) overflowed past the arbitrary/keyword offset and into the next
   family, and sorted fractions ahead of all numerics. *)
let inset_value_order_matches_tailwind () =
  let mk s =
    match Tw.of_string s with
    | Ok u -> u
    | Error (`Msg m) -> failwith (s ^ ": " ^ m)
  in
  let utilities =
    List.map mk
      [
        "inset-0";
        "inset-1";
        "inset-2";
        "inset-4";
        "inset-40";
        "inset-3/4";
        "-inset-1";
        "-inset-4";
        "inset-auto";
        "inset-full";
        "-inset-full";
        "bottom-24";
        "bottom-40";
        "bottom-[5rem]";
      ]
  in
  Test_helpers.check_ordering_matches
    ~test_name:"inset value order matches Tailwind"
    (Test_helpers.shuffle utilities)

let tests =
  [
    test_case "inset and z" `Quick test_inset_and_z;
    test_case "negative top" `Quick test_negative;
    test_case "arbitrary value roundtrip" `Quick test_arbitrary_roundtrip;
    test_case "position utilities" `Quick test_position_utilities;
    test_case "named inset requires theme token" `Quick
      named_inset_requires_theme_token;
    test_case "arbitrary var insets" `Quick test_arbitrary_var;
    test_case "position suborder matches Tailwind" `Quick
      suborder_matches_tailwind;
    test_case "inset value order matches Tailwind" `Quick
      inset_value_order_matches_tailwind;
  ]

let suite = ("position", tests)
