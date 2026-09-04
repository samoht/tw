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

(* An arbitrary value is read by the whole decoder, not by its last stage alone:
   [_] is a space, [--spacing(n)] expands to the spacing product, and only then
   are the math operators re-spaced. The bare suffix keeps its own reading,
   where a step OCaml's number reader accepts but Tailwind does not ([top-0x4],
   [top-1_0]) is still no utility. *)
let test_arbitrary_value_decoder_stages () =
  Test_helpers.check_declarations "top-[calc(1px_+_1px)]"
    [ "top:calc(1px + 1px)" ];
  Test_helpers.check_declarations "inset-[calc(1px_+_1px)]"
    [ "inset:calc(1px + 1px)" ];
  Test_helpers.check_declarations "left-[--spacing(4)]"
    [ "left:calc(var(--spacing)*4)" ];
  let rejected cls =
    match Tw.of_string cls with
    | Ok u -> Alcotest.failf "expected %s to be rejected, got %s" cls (Tw.pp u)
    | Error _ -> ()
  in
  rejected "top-0x4";
  rejected "top-1_0"

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

(* Every inset side reads an arbitrary length, and reads it under either sign.
   The pinned CLI emits [inset-inline-start: 4px] for [start-[4px]] and [top:
   calc(4px * -1)] for [-top-[4px]]; tw folds the negation into the literal,
   which [arbitrary_insets_match_tailwind] holds to the CLI. *)
let arbitrary_length_on_every_inset_side () =
  let open Test_helpers in
  check_declarations "start-[4px]" [ "inset-inline-start:4px" ];
  check_declarations "end-[4px]" [ "inset-inline-end:4px" ];
  check_declarations "start-[var(--x)]" [ "inset-inline-start:var(--x)" ];
  check_declarations "end-[calc(5%-2px)]" [ "inset-inline-end:calc(5% - 2px)" ];
  check_declarations "-start-[4px]" [ "inset-inline-start:-4px" ];
  check_declarations "-end-[4px]" [ "inset-inline-end:-4px" ];
  check_declarations "-top-[4px]" [ "top:-4px" ];
  check_declarations "-right-[4px]" [ "right:-4px" ];
  check_declarations "-bottom-[4px]" [ "bottom:-4px" ];
  check_declarations "-left-[4px]" [ "left:-4px" ];
  check_declarations "-inset-[4px]" [ "inset:-4px" ];
  check_declarations "-inset-x-[4px]" [ "inset-inline:-4px" ];
  check_declarations "-inset-y-[4px]" [ "inset-block:-4px" ];
  check_declarations "-inset-s-[4px]" [ "inset-inline-start:-4px" ];
  check_declarations "-inset-e-[4px]" [ "inset-inline-end:-4px" ];
  check_declarations "-inset-bs-[4px]" [ "inset-block-start:-4px" ];
  check_declarations "-inset-be-[4px]" [ "inset-block-end:-4px" ];
  check_declarations "-top-[var(--t)]" [ "top:calc(var(--t)*-1)" ];
  (* the bracket text is the class name, so it has to survive the round trip *)
  check "start-[4px]";
  check "end-[4px]";
  check "-top-[4px]";
  check "-inset-bs-[4px]";
  check "-start-[var(--x)]"

(* The logical inline sides resolve a name the theme binds, the way every other
   inset side does: the CLI reads [--inset-header] for [start-header]. *)
let named_inset_on_logical_inline_sides () =
  let themed =
    Tw.Scheme.with_overrides Tw.Scheme.default [ ("inset-header", "2rem") ]
  in
  let accepts cls =
    match Tw.Position.Handler.of_class themed cls with
    | Ok util ->
        Alcotest.(check string)
          "round-trips" cls
          (Tw.Position.Handler.to_class util)
    | Error (`Msg m) -> Alcotest.failf "%s with theme rejected: %s" cls m
  in
  accepts "start-header";
  accepts "end-header";
  (* and stays a non-utility when the theme binds no such name *)
  let reject c =
    match Tw.Position.Handler.of_class Tw.Scheme.default c with
    | Error _ -> ()
    | Ok _ -> Alcotest.failf "%s should be rejected without a theme token" c
  in
  reject "start-header";
  reject "end-header"

(* The values above against the pinned CLI, sheet for sheet. *)
let arbitrary_insets_match_tailwind () =
  let mk s =
    match Tw.of_string s with
    | Ok u -> u
    | Error (`Msg m) -> Alcotest.failf "%s: %s" s m
  in
  Test_helpers.check_ordering_matches ~test_name:"arbitrary insets"
    (List.map mk
       [
         "start-[4px]";
         "end-[4px]";
         "-start-[4px]";
         "-end-[4px]";
         "-top-[4px]";
         "-right-[4px]";
         "-bottom-[4px]";
         "-left-[4px]";
         "-inset-[4px]";
         "-inset-x-[4px]";
         "-inset-y-[4px]";
         "-inset-s-[4px]";
         "-inset-e-[4px]";
         "-inset-bs-[4px]";
         "-inset-be-[4px]";
       ])

(* A spacing step is a non-negative multiple of 0.25
   ([isValidSpacingMultiplier]), so the CLI emits for [top-1.25] and for nothing
   between the quarters. The bare suffix is read as a plain decimal too, which
   is what keeps the OCaml literal spellings out. *)
let spacing_step_is_a_quarter_multiple () =
  let open Test_helpers in
  check_declarations "top-1.5" [ "top:calc(var(--spacing)*1.5)" ];
  check_declarations "top-1.25" [ "top:calc(var(--spacing)*1.25)" ];
  check_declarations "top-0.75" [ "top:calc(var(--spacing)*.75)" ];
  check_declarations "start-1.5"
    [ "inset-inline-start:calc(var(--spacing)*1.5)" ];
  let reject c = check_invalid_input (module Tw.Position.Handler) c in
  reject "top-1.7";
  reject "top-0.3";
  reject "top-1.1";
  reject "top-0.125";
  reject "-top-1.7";
  reject "inset-1.7";
  reject "inset-x-1.7";
  reject "left-1.7";
  reject "start-1.7";
  reject "end-1.7";
  reject "-start-1.7";
  (* the plain-decimal reading of a bare suffix, which brackets do not share *)
  reject "top-0x4";
  reject "top-1_0";
  reject "start-0x4";
  reject "start-1_0"

(* {!Test_helpers.check_declarations} compiles against the default theme, which
   binds none of the tokens the named-inset cases need, so those read their own
   sheet. *)
let themed_declarations theme cls =
  match Tw.of_string ~theme cls with
  | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  | Ok u ->
      Tw.to_css ~theme ~base:false [ u ]
      |> Tw.Css.fold
           (fun acc stmt ->
             match Tw.Css.as_rule stmt with
             | Some (sel, decls, _)
               when String.contains (Tw.Css.Selector.to_string sel) '.' ->
                 acc
                 @ List.map (Tw.Css.Declaration.to_string ~minify:true) decls
             | _ -> acc)
           []

let check_themed theme cls expected =
  Alcotest.(check (list string)) cls expected (themed_declarations theme cls)

(* A named inset names a theme token, and the theme owns its value: Tailwind
   reads [--inset-<name>] and falls back to [--spacing-<name>]. tw resolved the
   [--inset-*] spelling whichever namespace matched and bound it to a length of
   its own, so a theme carrying only [--spacing-lg] got [top: var(--inset-lg)]
   over an [--inset-lg: 1940px] no theme ever declared. *)
let named_inset_reads_the_theme_token () =
  let spacing =
    Tw.Scheme.with_overrides Tw.Scheme.default [ ("spacing-lg", "3rem") ]
  in
  let inset =
    Tw.Scheme.with_overrides Tw.Scheme.default [ ("inset-shadowned", "7px") ]
  in
  let both =
    Tw.Scheme.with_overrides Tw.Scheme.default
      [ ("inset-lg", "5rem"); ("spacing-lg", "3rem") ]
  in
  check_themed spacing "top-lg" [ "top:var(--spacing-lg)" ];
  check_themed inset "top-shadowned" [ "top:var(--inset-shadowned)" ];
  (* [--inset-*] first, [--spacing-*] only as the fallback *)
  check_themed both "top-lg" [ "top:var(--inset-lg)" ];
  let theme_vars theme cls =
    match Tw.of_string ~theme cls with
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
    | Ok u ->
        Test_helpers.vars_in_layer "theme" (Tw.to_css ~theme ~base:false [ u ])
  in
  Alcotest.(check (list string))
    "top-lg binds the token the theme declared, and no other" [ "--spacing-lg" ]
    (theme_vars spacing "top-lg")

(* Every inset side reads a named token under a minus, the way it reads a
   spacing step and an arbitrary length: the CLI writes
   [calc(var(--inset-shadowned) * -1)]. tw's negative tail read the bracket
   alone, so the whole negative named family was an unknown class. *)
let negative_named_inset_on_every_side () =
  let theme =
    Tw.Scheme.with_overrides Tw.Scheme.default
      [ ("inset-shadowned", "7px"); ("spacing-lg", "3rem") ]
  in
  let neg = "calc(var(--inset-shadowned)*-1)" in
  check_themed theme "-top-shadowned" [ "top:" ^ neg ];
  check_themed theme "-right-shadowned" [ "right:" ^ neg ];
  check_themed theme "-bottom-shadowned" [ "bottom:" ^ neg ];
  check_themed theme "-left-shadowned" [ "left:" ^ neg ];
  check_themed theme "-inset-shadowned" [ "inset:" ^ neg ];
  check_themed theme "-inset-x-shadowned" [ "inset-inline:" ^ neg ];
  check_themed theme "-inset-y-shadowned" [ "inset-block:" ^ neg ];
  check_themed theme "-start-shadowned" [ "inset-inline-start:" ^ neg ];
  check_themed theme "-end-shadowned" [ "inset-inline-end:" ^ neg ];
  check_themed theme "-inset-s-shadowned" [ "inset-inline-start:" ^ neg ];
  check_themed theme "-inset-e-shadowned" [ "inset-inline-end:" ^ neg ];
  check_themed theme "-inset-bs-shadowned" [ "inset-block-start:" ^ neg ];
  check_themed theme "-inset-be-shadowned" [ "inset-block-end:" ^ neg ];
  (* the [--spacing-*] fallback carries the minus too *)
  check_themed theme "-inset-bs-lg"
    [ "inset-block-start:calc(var(--spacing-lg)*-1)" ];
  (* a bare parenthesised group stays a length under the minus alone: Tailwind
     writes the positive as [top: (var(--a) + var(--b))], which no browser
     accepts *)
  check_themed theme "-top-[(var(--a)+var(--b))]"
    [ "top:calc((var(--a) + var(--b))*-1)" ];
  Test_helpers.check_invalid_input
    ~why:
      (Test_helpers.Diverges
         "Tailwind writes the group out unwrapped, as top: (var(--a) + \
          var(--b)); tw refuses the class rather than emit a declaration no \
          browser reads")
    (module Tw.Position.Handler)
    "top-[(var(--a)+var(--b))]";
  (* and a name the theme binds in neither namespace is still no utility *)
  Test_helpers.check_invalid_input (module Tw.Position.Handler) "-top-level";
  Test_helpers.check_invalid_input (module Tw.Position.Handler) "-bottom-right"

(* The four logical [inset-*] sides carry the whole scale their [start]/[end]
   spellings do: the px step, the fractional steps and the fractions, in both
   signs. Fractions were spelled per side, which left [inset-y-1/2],
   [bottom-1/2] and every fraction but [3/4] on the logical sides unknown. *)
let logical_inset_sides_take_the_whole_scale () =
  let open Test_helpers in
  check_declarations "inset-s-0.5"
    [ "inset-inline-start:calc(var(--spacing)*.5)" ];
  check_declarations "inset-e-0.5"
    [ "inset-inline-end:calc(var(--spacing)*.5)" ];
  check_declarations "inset-bs-0.5"
    [ "inset-block-start:calc(var(--spacing)*.5)" ];
  check_declarations "inset-be-0.5"
    [ "inset-block-end:calc(var(--spacing)*.5)" ];
  check_declarations "-inset-s-0.5"
    [ "inset-inline-start:calc(var(--spacing)*-.5)" ];
  check_declarations "inset-bs-1.25"
    [ "inset-block-start:calc(var(--spacing)*1.25)" ];
  check_declarations "inset-s-px" [ "inset-inline-start:1px" ];
  check_declarations "-inset-bs-px" [ "inset-block-start:-1px" ];
  (* the zero and unit steps still fold, the way the physical sides fold them
     and unlike [start-0] *)
  check_declarations "inset-s-0" [ "inset-inline-start:0px" ];
  check_declarations "inset-bs-1" [ "inset-block-start:var(--spacing)" ];
  (* fractions, on the logical sides and on the two the per-side spelling had
     left with [3/4] alone *)
  check_declarations "inset-s-1/2" [ "inset-inline-start:50%" ];
  check_declarations "inset-e-3/4" [ "inset-inline-end:75%" ];
  check_declarations "inset-bs-1/2" [ "inset-block-start:50%" ];
  check_declarations "inset-be-1/2" [ "inset-block-end:50%" ];
  check_declarations "-inset-bs-1/2" [ "inset-block-start:-50%" ];
  check_declarations "inset-y-1/2" [ "inset-block:50%" ];
  check_declarations "-inset-y-1/2" [ "inset-block:-50%" ];
  check_declarations "bottom-1/2" [ "bottom:50%" ];
  check_declarations "-bottom-3/4" [ "bottom:-75%" ];
  (* the bare suffix is the class name, so it round-trips *)
  check "inset-s-0.5";
  check "inset-bs-1/2";
  check "-inset-y-1/2";
  check "-bottom-3/4";
  (* a zero denominator has no percentage on any side either: Tailwind passes it
     through as calc(1 / 0 * 100%), which no browser can compute *)
  let uncomputable c =
    check_invalid_input
      ~why:
        (Diverges
           "Tailwind passes a zero denominator through as calc(1 / 0 * 100%), \
            which no browser can compute; tw refuses the class instead")
      (module Tw.Position.Handler)
      c
  in
  uncomputable "inset-s-1/0";
  uncomputable "inset-bs-1/0";
  uncomputable "inset-y-1/0";
  uncomputable "bottom-1/0";
  let reject c = check_invalid_input (module Tw.Position.Handler) c in
  reject "inset-s-1.7";
  reject "inset-be-1.7"

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
    test_case "arbitrary value decoder stages" `Quick
      test_arbitrary_value_decoder_stages;
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
    test_case "arbitrary length on every inset side" `Quick
      arbitrary_length_on_every_inset_side;
    test_case "named inset on logical inline sides" `Quick
      named_inset_on_logical_inline_sides;
    test_case "arbitrary insets match Tailwind" `Quick
      arbitrary_insets_match_tailwind;
    test_case "spacing step is a quarter multiple" `Quick
      spacing_step_is_a_quarter_multiple;
    test_case "named inset reads the theme token" `Quick
      named_inset_reads_the_theme_token;
    test_case "negative named inset on every side" `Quick
      negative_named_inset_on_every_side;
    test_case "logical inset sides take the whole scale" `Quick
      logical_inset_sides_take_the_whole_scale;
  ]

let suite = ("position", tests)
