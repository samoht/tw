open Alcotest

let check = Test_helpers.check_handler_roundtrip (module Tw.Flex_props.Handler)

let of_string_valid () =
  (* Note: Direction and Wrap utilities are now in Flex_layout module *)

  (* Flex shortcuts *)
  check "flex-1";
  check "flex-auto";
  check "flex-initial";
  check "flex-none";

  (* Grow/Shrink - Tailwind v4 uses shorter names *)
  check "grow";
  check "grow-0";
  check "shrink";
  check "shrink-0";

  (* Deprecated v3 spellings keep their own class name (not folded into the
     shorter grow/shrink), so the emitted selector matches the source class *)
  check "flex-grow";
  check "flex-grow-0";
  check "flex-shrink";
  check "flex-shrink-0";

  (* Basis *)
  check "basis-0";
  check "basis-1";
  (* v4 accepts any bare integer (spacing scale), not just 0/1 *)
  check "basis-3";
  check "basis-7";
  check "basis-auto";
  check "basis-full";
  (* container scale, including the digit-led names is_named_spacing rejects *)
  check "basis-xs";
  check "basis-xl";
  check "basis-2xs";
  check "basis-3xs";
  check "basis-2xl";
  check "basis-7xl";

  (* Order *)
  check "order-0";
  check "order-1";
  check "order-2";
  check "order-3";
  check "order-4";
  check "order-5";
  check "order-6";
  check "order-first";
  check "order-last";
  check "order-none"

let of_string_invalid () =
  let fail_maybe input =
    let class_name = String.concat "-" input in
    match Tw.Flex_props.Handler.of_class Tw.Scheme.default class_name with
    | Ok _ -> fail ("Expected error for: " ^ String.concat "-" input)
    | Error _ -> ()
  in

  fail_maybe [ "flex"; "invalid" ];
  fail_maybe [ "basis" ];
  (* not a container-scale name *)
  fail_maybe [ "basis"; "4xs" ];
  (* Missing value *)
  fail_maybe [ "order" ];
  fail_maybe []

(* [basis-*] and [flex-*] read the same fraction the sizing families do: any
   numerator, zero included, over any positive denominator. Requiring a positive
   numerator refused [basis-0/2], which the CLI emits.

   [flex-0/2] is the one whose sheets do not compare equal: tw writes the [0%]
   that Tailwind's [calc(0/2 * 100%)] computes to, and cascade reads the two
   spellings of that declaration differently. *)
let test_any_fraction_numerator () =
  check "basis-0/2";
  check "basis-1/7";
  check "basis-13/17";
  check "flex-0/2";
  check "flex-1/7";
  Test_helpers.check_invalid_input
    ~why:
      (Test_helpers.Diverges
         "Tailwind passes a zero denominator through as calc(1 / 0 * 100%), \
          which no browser can compute; tw refuses the class instead")
    (module Tw.Flex_props.Handler)
    "basis-1/0"

let suborder_matches_tailwind () =
  let open Tw in
  let utilities =
    [
      flex_1;
      flex_auto;
      flex_initial;
      flex_none;
      flex_grow;
      flex_grow_0;
      flex_shrink;
      flex_shrink_0;
      basis_0;
      basis_1;
      basis_auto;
      basis_full;
      order 1;
      order 3;
      order 6;
      order_first;
      order_last;
      order_none;
    ]
  in
  let shuffled = Test_helpers.shuffle utilities in

  Test_helpers.check_ordering_matches
    ~test_name:"flex_props suborder matches Tailwind" shuffled

(* [basis] lists --flex-basis, then --spacing, then --container, so a theme that
   sets both --spacing-sm and --container-sm reads the spacing one. *)
let test_basis_named_prefers_spacing () =
  let theme =
    Tw.Scheme.with_overrides Tw.Scheme.default
      [ ("spacing-sm", "8px"); ("container-sm", "256px") ]
  in
  let css =
    match Tw.of_string ~theme "basis-sm" with
    | Ok u -> Tw.to_css ~theme ~base:false [ u ] |> Tw.Css.to_string
    | Error (`Msg m) -> Alcotest.failf "basis-sm: %s" m
  in
  Alcotest.(check bool)
    "basis-sm reads --spacing-sm" true
    (Astring.String.is_infix ~affix:"var(--spacing-sm)" css);
  Alcotest.(check bool)
    "basis-sm declares --spacing-sm" true
    (Astring.String.is_infix ~affix:"--spacing-sm: 8px" css)

(* [basis-[...]] names its rule with the bracket text the author wrote, not with
   the parsed flex-basis printed back: the CSS printer canonicalises a number
   ([0.5ch] to [.5ch]) and drops a comment, and a rule spelled that way cannot
   be matched by the class that generated it. [order-[...]] below carries its
   text the same way. *)
let test_basis_arbitrary_keeps_the_authored_spelling () =
  List.iter
    (fun (cls, escaped) ->
      match Tw.of_string cls with
      | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
      | Ok u ->
          Alcotest.(check string) "class round-trips" cls (Tw.pp u);
          let css = Tw.to_css ~base:false [ u ] |> Tw.Css.to_string in
          Alcotest.(check bool)
            (cls ^ " selects itself") true
            (Astring.String.is_infix ~affix:escaped css))
    [
      ("basis-[0.5ch]", {|.basis-\[0\.5ch\]|});
      ("basis-[0.0]", {|.basis-\[0\.0\]|});
      ("basis-[1px/*x]", {|.basis-\[1px\/\*x\]|});
      ("basis-[10px]", {|.basis-\[10px\]|});
    ]

(* [order-[...]] takes an order value. A bracket the order grammar cannot read
   is accepted and then raises out of [to_css], a pure conversion, so the
   rejection belongs at parse time. *)
let test_invalid_arbitrary_order () =
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
  rejected "order-[foo]";
  rejected "order-[1abc]";
  rejected "order-[1.5]";
  rejected "-order-[foo]";
  renders "order-[13]";
  renders "order-[var(--x)]";
  renders "-order-[13]";
  renders "-order-[var(--x)]"

(* [flex-], [grow-] and [shrink-] read their bracket through the arbitrary-value
   pipeline, which puts the spaces CSS math wants around a binary [+] and
   expands [--spacing()]. Reading the text with OCaml's number reader instead
   refused [calc(1+2)] outright and folded [0x4] to 4, so the class named itself
   after a value nobody wrote. What that reader refuses is not invalid: Tailwind
   writes a bracket out as the token stream it is. *)
let test_arbitrary_flex_reads_the_whole_bracket () =
  Test_helpers.check_declarations "flex-[calc(1+2)]" [ "flex:calc(1 + 2)" ];
  Test_helpers.check_declarations "grow-[calc(1+2)]" [ "flex-grow:calc(1 + 2)" ];
  Test_helpers.check_declarations "shrink-[calc(1+2)]"
    [ "flex-shrink:calc(1 + 2)" ];
  Test_helpers.check_declarations "flex-[--spacing(4)]"
    [ "flex:calc(var(--spacing)*4)" ];
  (* a value no property grammar reads still reaches the sheet as written *)
  Test_helpers.check_declarations "flex-[0x4]" [ "flex:0x4" ];
  Test_helpers.check_declarations "grow-[0x4]" [ "flex-grow:0x4" ];
  Test_helpers.check_declarations "shrink-[0x4]" [ "flex-shrink:0x4" ];
  Test_helpers.check_declarations "grow-[var(--x)]" [ "flex-grow:var(--x)" ];
  (* and the class keeps the spelling it was written with *)
  check "flex-[0x4]";
  check "grow-[0x4]";
  check "shrink-[0x4]";
  check "flex-[calc(1+2)]"

(* [order-[...]] reads the same pipeline: [calc(1+2)] is a value the order
   grammar takes once the operator spacing is normalised. *)
let test_arbitrary_order_reads_the_whole_bracket () =
  (* cascade folds a constant calc, which the canonical differ accepts; Tailwind
     writes [order:calc(1 + 2)]. *)
  Test_helpers.check_declarations "order-[calc(1+2)]" [ "order:3" ];
  Test_helpers.check_declarations "order-[--spacing(4)]"
    [ "order:calc(var(--spacing)*4)" ];
  check "order-[calc(1+2)]"

let tests =
  [
    test_case "basis-* prefers --spacing-*" `Quick
      test_basis_named_prefers_spacing;
    test_case "flex_props of_string - valid values" `Quick of_string_valid;
    test_case "flex_props of_string - invalid values" `Quick of_string_invalid;
    test_case "any fraction numerator" `Quick test_any_fraction_numerator;
    test_case "flex_props suborder matches Tailwind" `Quick
      suborder_matches_tailwind;
    test_case "invalid arbitrary order" `Quick test_invalid_arbitrary_order;
    test_case "arbitrary flex reads the whole bracket" `Quick
      test_arbitrary_flex_reads_the_whole_bracket;
    test_case "arbitrary order reads the whole bracket" `Quick
      test_arbitrary_order_reads_the_whole_bracket;
    test_case "basis-[...] keeps the authored spelling" `Quick
      test_basis_arbitrary_keeps_the_authored_spelling;
  ]

let suite = ("flex_props", tests)
