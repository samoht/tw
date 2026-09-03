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

(* Fractions resolve to a percentage on the inset/top/right/left families,
   including arbitrary numerators/denominators (not just 1/2 and 3/4). *)
let test_fractions () =
  check "inset-1/2";
  check "inset-x-1/2";
  check "top-1/2";
  check "top-3/4";
  check "right-1/2";
  check "left-1/2";
  check "left-1/5";
  check "left-2/3";
  (* negative and improper fractions *)
  check "-left-1/2";
  check "-top-1/3";
  check "-inset-x-1/2";
  check "left-6/5";
  check "-left-6/5"

(* Negative fractions negate the percentage; an improper fraction resolves past
   100% (6/5 -> 120%). *)
let test_negative_and_improper_fractions () =
  let css cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  Alcotest.(check bool)
    "-left-6/5 is -120%" true
    (Astring.String.is_infix ~affix:"left: -120%" (css "-left-6/5"));
  Alcotest.(check bool)
    "left-6/5 is 120%" true
    (Astring.String.is_infix ~affix:"left: 120%" (css "left-6/5"));
  Alcotest.(check bool)
    "-inset-x-1/2 is -50%" true
    (Astring.String.is_infix ~affix:"inset-inline: -50%" (css "-inset-x-1/2"))

(* Tailwind reads any numerator over any denominator, the same rule the sizing
   families follow: [top-1/7] and [top-3/8] are as good as [top-1/2], and a zero
   numerator is a position of its own. Restricting the denominator to a hand
   picked list refused classes the CLI emits. *)
let test_any_fraction_denominator () =
  let css cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  let has cls affix =
    Alcotest.(check bool)
      (cls ^ " contains " ^ affix)
      true
      (Astring.String.is_infix ~affix (css cls))
  in
  has "top-1/7" "top: 14.2857%";
  has "top-3/8" "top: 37.5%";
  has "top-1/13" "top: 7.69231%";
  has "inset-0/2" "inset: 0%";
  has "left-13/17" "left: 76.4706%";
  Test_helpers.check_invalid_input
    ~why:
      (Test_helpers.Diverges
         "Tailwind passes a zero denominator through as calc(1 / 0 * 100%), \
          which no browser can compute; tw refuses the class instead")
    (module Tw.Position.Handler)
    "top-1/0"

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

(* Fractional spacing steps (top-2.5) resolve to calc(var(--spacing) * n) and
   the px step (left-px) to 1px, on the physical/axis inset sides. *)
let test_spacing_steps () =
  let css cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string ~minify:true
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  Alcotest.(check bool)
    "top-2.5 uses calc(var(--spacing)*2.5)" true
    (Astring.String.is_infix ~affix:"top:calc(var(--spacing)*2.5)"
       (css "top-2.5"));
  Alcotest.(check bool)
    "inset-y-0.5 uses the block axis" true
    (Astring.String.is_infix ~affix:"inset-block:calc(var(--spacing)*.5)"
       (css "inset-y-0.5"));
  Alcotest.(check bool)
    "left-px is 1px" true
    (Astring.String.is_infix ~affix:"left:1px" (css "left-px"));
  Alcotest.(check bool)
    "inset-px is 1px" true
    (Astring.String.is_infix ~affix:"inset:1px" (css "inset-px"));
  (* round-trip the class names, escaped dot included *)
  check "top-2.5";
  check "right-1.5";
  check "top-14.25";
  check "left-px";
  check "inset-px"

(* Arbitrary calc() insets go through the full length grammar (the bracket
   parser used to accept only plain <number><unit>). *)
let test_arbitrary_calc () =
  let css cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  Alcotest.(check bool)
    "left-[calc(5%-2px)] spaces the operator" true
    (Astring.String.is_infix ~affix:"left: calc(5% - 2px)"
       (css "left-[calc(5%-2px)]"));
  Alcotest.(check bool)
    "left-[calc(50%+var(--offset))] keeps the var" true
    (Astring.String.is_infix ~affix:"left: calc(50% + var(--offset))"
       (css "left-[calc(50%+var(--offset))]"));
  check "left-[calc(5%-2px)]"

(* An arbitrary inset whose body is not a whole calc expression is not a
   utility. In [-left-[0)/*1]] the stray ')' closes nothing and the '/*' opens a
   comment that never ends, so Tailwind emits no rule for it. *)
let test_unbalanced_arbitrary_rejected () =
  Alcotest.(check bool)
    "-left-[0)/*1] is not a utility" true
    (Result.is_error (Tw.of_string "-left-[0)/*1]"))

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

let position_candidate_bands_match_tailwind () =
  Test_helpers.check_class_order ~test_name:"position candidate bands"
    [
      "left-full";
      "right-px";
      "inset-auto";
      "top-(--top,0)";
      "-inset-1";
      "right-(--right,54%)";
      "inset-px";
      "left-0";
      "top-full";
      "right-1.5";
      "inset-3/4";
      "-left-(--gutter-width)";
      "right-0";
      "inset-full";
      "top-0";
      "left-px";
      "-top-1";
      "inset-0";
    ]

(* top'/right'/bottom'/left'/inset'/inset_x'/inset_y' take a half-step float
   (negative allowed, same as the int base); the int base keeps emitting what it
   always did. *)
let typed_prime () =
  let open Tw in
  let check_class expected value =
    Alcotest.(check string) expected expected (Tw.pp value)
  in
  check_class "top-0.5" (top' 0.5);
  check_class "-top-0.5" (top' (-0.5));
  check_class "right-0.5" (right' 0.5);
  check_class "bottom-0.5" (bottom' 0.5);
  check_class "left-0.5" (left' 0.5);
  check_class "inset-0.5" (inset' 0.5);
  check_class "inset-x-0.5" (inset_x' 0.5);
  check_class "inset-y-0.5" (inset_y' 0.5);
  check_class "top-4" (top 4);
  check_class "-top-4" (top (-4))

(* Tailwind's start-* / end-* handler resolves the spacing step itself and
   writes the product out in full, so the zero and unit multipliers keep
   calc(var(--spacing) * n) where the physical sides fold them to 0px and
   var(--spacing). The folded zero stops referencing --spacing, which also drops
   the declaration the utility reads from the theme layer. *)
let logical_inline_keeps_the_spacing_product () =
  let css cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string ~minify:true
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  let check_css cls affix =
    Alcotest.(check bool)
      (cls ^ " emits " ^ affix)
      true
      (Astring.String.is_infix ~affix (css cls))
  in
  check_css "start-0" "inset-inline-start:calc(var(--spacing)*0)";
  check_css "start-1" "inset-inline-start:calc(var(--spacing)*1)";
  check_css "start-2" "inset-inline-start:calc(var(--spacing)*2)";
  check_css "-start-4" "inset-inline-start:calc(var(--spacing)*-4)";
  check_css "end-0" "inset-inline-end:calc(var(--spacing)*0)";
  check_css "end-1" "inset-inline-end:calc(var(--spacing)*1)";
  check_css "start-0" "--spacing:.25rem";
  check_css "end-0" "--spacing:.25rem"

(* The logical inline sides carry the same scale as the physical ones: the px
   step, the fractional steps and the fractions, in both signs. *)
let logical_inline_scale_steps () =
  let css cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string ~minify:true
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  let check_css cls affix =
    Alcotest.(check bool)
      (cls ^ " emits " ^ affix)
      true
      (Astring.String.is_infix ~affix (css cls))
  in
  check_css "start-px" "inset-inline-start:1px";
  check_css "-start-px" "inset-inline-start:-1px";
  check_css "end-px" "inset-inline-end:1px";
  check_css "start-0.5" "inset-inline-start:calc(var(--spacing)*.5)";
  check_css "-start-0.5" "inset-inline-start:calc(var(--spacing)*-.5)";
  check_css "end-0.5" "inset-inline-end:calc(var(--spacing)*.5)";
  check_css "start-1/2" "inset-inline-start:50%";
  check_css "-start-1/2" "inset-inline-start:-50%";
  check_css "-start-full" "inset-inline-start:-100%";
  check_css "-end-full" "inset-inline-end:-100%";
  check "start-px";
  check "end-px";
  check "start-0.5";
  check "start-1/2";
  check "start-3/4";
  check "start-2";
  check "-start-4";
  check "-start-full"

(* Neither logical inline side is a utility without a scale value: a stray
   source token, the negative of a keyword Tailwind only defines positive, and a
   zero denominator are all rejected. *)
let logical_inline_rejects_non_utilities () =
  let reject c =
    match Tw.of_string c with
    | Error _ -> ()
    | Ok _ -> Alcotest.failf "%s should not be a utility" c
  in
  reject "start-junk";
  reject "end-junk";
  reject "-start-auto";
  reject "start-1/0";
  reject "start-"

let tests =
  [
    test_case "inset and z" `Quick test_inset_and_z;
    test_case "typed constructors: half-step" `Quick typed_prime;
    test_case "negative top" `Quick test_negative;
    test_case "arbitrary value roundtrip" `Quick test_arbitrary_roundtrip;
    test_case "position utilities" `Quick test_position_utilities;
    test_case "position fractions" `Quick test_fractions;
    test_case "negative and improper fractions" `Quick
      test_negative_and_improper_fractions;
    test_case "any fraction denominator" `Quick test_any_fraction_denominator;
    test_case "named inset requires theme token" `Quick
      named_inset_requires_theme_token;
    test_case "arbitrary var insets" `Quick test_arbitrary_var;
    test_case "spacing steps (fractional + px)" `Quick test_spacing_steps;
    test_case "arbitrary calc insets" `Quick test_arbitrary_calc;
    test_case "unbalanced arbitrary inset rejected" `Quick
      test_unbalanced_arbitrary_rejected;
    test_case "position suborder matches Tailwind" `Quick
      suborder_matches_tailwind;
    test_case "inset value order matches Tailwind" `Quick
      inset_value_order_matches_tailwind;
    test_case "position candidate bands match Tailwind" `Quick
      position_candidate_bands_match_tailwind;
    test_case "logical inline sides keep the spacing product" `Quick
      logical_inline_keeps_the_spacing_product;
    test_case "logical inline scale steps" `Quick logical_inline_scale_steps;
    test_case "logical inline rejects non-utilities" `Quick
      logical_inline_rejects_non_utilities;
  ]

let suite = ("position", tests)
