module Css = Cascade.Css

let test_escape_in_selector () =
  let test_css = ".inset-3\\/4 { inset: 75%; }" in
  match Css.of_string test_css with
  | Ok _stylesheet -> ()
  | Error e ->
      Alcotest.failf "Failed to parse escaped selector: %s"
        (Cascade.Error.to_string e)

let unknown cls =
  match Tw.of_string cls with
  | Ok u -> Alcotest.failf "expected %s to be unknown, got %s" cls (Tw.pp u)
  | Error (`Msg _) -> ()

let round_trips cls =
  match Tw.of_string cls with
  | Ok u -> Alcotest.(check string) "round-trips" cls (Tw.pp u)
  | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m

(* A class suffix is written in plain decimal. OCaml's literal grammar also
   admits [0x]/[0o]/[0b] bases, [_] digit separators and a leading [+]; reading
   a suffix as an OCaml literal would accept [p-0x4] and emit [.p-4], a rule
   nobody wrote. One class per integer reader: spacing_value, int_pos, int_any,
   int_bounded. *)
let test_int_rejects_non_decimal_spellings () =
  List.iter unknown
    [
      "p-0x4";
      "p-0X4";
      "p-0o17";
      "p-0b101";
      "p-1_0";
      "gap-1_0";
      "duration-0x4";
      "delay-1_0";
      "rotate-0x4";
      "z-0x10";
      "order-1_0";
      "columns-0x2";
      "grid-cols-0x2";
      "grid-rows-0x2";
      "basis-1_0";
      "flex-0x2";
      "w-0x4";
      "h-0x4";
      "border-0x2";
      "line-clamp-0x2";
      "opacity-0x10";
    ]

(* The sign is stripped before the suffix is read, so a negative utility takes
   the same grammar. *)
let test_negative_int_rejects_non_decimal_spellings () =
  List.iter unknown [ "-m-1_0"; "-rotate-0x4"; "-translate-x-0x4"; "-z-0x10" ]

(* [Float.of_string] reads the same non-decimal spellings plus hex-float
   exponents, which would make [p-0x1p4] mean [.p-16]. A fractional suffix needs
   digits on both sides of the point. *)
let test_decimal_rejects_non_decimal_spellings () =
  List.iter unknown
    [
      "p-0x1p4"; "p-1_5"; "p-1.2_5"; "p-1e2"; "p-+1.5"; "p-.5"; "p-1."; "m-1e1";
    ]

let test_decimal_class_suffixes_round_trip () =
  List.iter round_trips
    [
      "p-4";
      "p-0";
      "p-96";
      "p-0.5";
      "p-1.5";
      "p-2.5";
      "m-3.5";
      "gap-10";
      "opacity-25";
      "border-2";
      "z-10";
      "order-10";
      "line-clamp-2";
      "duration-150";
      "delay-100";
      "rotate-45";
      "-m-10";
      "-rotate-45";
    ]

(* A valid suffix must also be the canonical decimal spelling. Otherwise the
   parser would accept one class and emit a selector for a different class. *)
let test_redundant_zero_spellings_are_rejected () =
  List.iter unknown [ "p-04"; "p-4.0"; "p-1.50" ]

(* The families that read their own suffix rather than going through the readers
   above. Checked against the pinned CLI: Tailwind emits for none of these. The
   integer ones rename the rule they emit, so the author writes [stroke-0x4] and
   gets a [.stroke-4] that matches nothing. *)
let test_family_int_suffixes_reject_non_decimal_spellings () =
  List.iter unknown
    [
      "stroke-0x4";
      "stroke-04";
      "stroke-1_0";
      "tab-0x4";
      "tab-04";
      "grow-0x4";
      "grow-04";
      "ring-0x4";
      "ring-04";
      "zoom-0x4";
      "zoom-04";
      "from-0x50%";
      "from-050%";
      "via-0x50%";
      "to-0x50%";
      "bg-linear-0x45";
      "bg-linear-045";
      "bg-conic-0x45";
      "bg-conic-045";
      "-bg-linear-0x45";
      "-bg-conic-045";
      "mask-linear-0x45";
      "mask-linear-045";
      "mask-conic-0x45";
      "-mask-linear-045";
      "mask-x-from-0x50%";
      "mask-x-from-050%";
    ]

(* The same for the families whose suffix is a decimal. *)
let test_family_decimal_suffixes_reject_non_decimal_spellings () =
  List.iter unknown
    [
      "underline-offset-0x4";
      "underline-offset-04";
      "underline-offset-4.50";
      "-underline-offset-0x4";
      "backdrop-opacity-0x50";
      "backdrop-opacity-050";
      "auto-cols-0x4";
      "auto-cols-04";
      "auto-rows-0x4";
      "mask-x-from-02";
      "mask-x-from-2.50";
      "top-1_5";
      "inset-1.50";
      "translate-x-1.50";
      "aspect-0x4/3";
      "aspect-04/3";
      "aspect-4.0/3";
      "aspect-1.50/3";
    ]

(* The opacity modifier rides on every colour utility, so one reader that admits
   an OCaml literal admits it across the whole palette. The shade is the same
   suffix a step earlier: [bg-red-0500] emitted [.bg-red-500]. *)
let test_colour_modifier_rejects_non_decimal_spellings () =
  List.iter unknown
    [
      "bg-red-500/0x50";
      "bg-red-500/1_0";
      "bg-red-500/04";
      "bg-red-500/1.50";
      "text-red-500/0x50";
      "border-red-500/1_0";
      "stroke-red-500/04";
      "bg-red-0500";
      "border-red-1_00";
      "text-red-0x500";
    ]

(* A fraction is two plain decimals: [w-04/2] emitted [width: 200%] for a class
   Tailwind does not emit at all. *)
let test_fraction_suffixes_reject_non_decimal_spellings () =
  List.iter unknown
    [
      "w-04/2";
      "w-1_0/2";
      "w-0x2/4";
      "w-1/02";
      "h-04/2";
      "top-04/2";
      "inset-04/2";
      "basis-01/2";
      "flex-01/2";
      "translate-x-01/2";
      "-translate-x-01/2";
    ]

(* What the families above have to keep emitting. *)
let test_family_suffixes_round_trip () =
  List.iter round_trips
    [
      "stroke-4";
      "tab-4";
      "grow-4";
      "ring-4";
      "zoom-4";
      "from-50%";
      "bg-linear-45";
      "bg-conic-45";
      "mask-linear-45";
      "mask-x-from-50%";
      "mask-x-from-2.5";
      "underline-offset-4";
      "backdrop-opacity-50";
      "backdrop-opacity-5.5";
      "auto-cols-4";
      "aspect-4/3";
      "aspect-8.5/11";
      "top-2.5";
      "inset-0.5";
      "translate-x-0.5";
      "bg-red-500/50";
      "bg-red-500/2.5";
      "w-1/2";
      "basis-1/2";
      "flex-1/2";
      "translate-x-1/2";
    ]

(* [Parse.split_class] remembers the split of the last class name it was given,
   because every handler in turn splits the same one. A parse must therefore not
   depend on what was parsed before it, nor on whether two equal class names are
   the same string. *)
let test_parse_is_independent_of_history () =
  let classes =
    [ "p-4"; "px-4"; "m-[calc(1rem-2px)]"; "grid-cols-2"; "bg-blue-500" ]
  in
  List.iter round_trips classes;
  List.iter round_trips (List.rev classes);
  List.iter round_trips classes;
  let built = String.concat "-" [ "grid"; "cols"; "2" ] in
  round_trips "grid-cols-3";
  round_trips built;
  unknown "grid-cols-"

(* A [var()] is one CSS component, not a prefix and suffix to slice away. A
   loose closer after the function is outside the reference and must keep the
   input untouched; a balanced nested fallback belongs to the reference body. *)
let test_extract_var_name_reads_one_reference () =
  let check expected input =
    Alcotest.(check string) input expected (Tw.Parse.extract_var_name input)
  in
  check "x" "var(--x)";
  check "x, calc(var(--y) + 1px)" "var(--x, calc(var(--y) + 1px))";
  check "var(--x))" "var(--x))";
  check "var(--x)garbage" "var(--x)garbage";
  check "not-a-reference" "not-a-reference"

(* The utilities that swallowed a second bracket refuse the class instead of
   raising out of [to_css]. Tailwind emits nothing for any of them. *)
let test_double_bracket_class_rejected () =
  let rejected cls =
    match Tw.of_string cls with
    | Ok u ->
        Alcotest.failf "expected %s to be rejected, got %s" cls
          (Tw.to_css ~base:false [ u ] |> Tw.Css.to_string ~minify:true)
    | Error _ -> ()
  in
  let accepted cls =
    match Tw.of_string cls with
    | Ok _ -> ()
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  rejected "mask-b-from-[foo]/[bar]";
  rejected "mask-linear-from-[foo]-[bar]";
  rejected "bg-linear-[foo]/[bar]";
  rejected "bg-radial-[foo]/[bar]";
  rejected "scale-z-[foo]/[bar]";
  rejected "mask-radial-[foo]/[bar]";
  rejected "-mask-linear-[foo]/[bar]";
  rejected "-mask-conic-[foo]/[bar]";
  rejected "mask-radial-at-[foo]/[bar]";
  accepted "mask-b-from-[10%]";
  accepted "bg-linear-[45deg]";
  accepted "scale-z-[2]";
  accepted "mask-radial-[50%_50%]";
  accepted "-mask-linear-[45deg]";
  accepted "mask-radial-at-[50%_50%]";
  (* A bracket modifier on a utility that takes one still reads. *)
  accepted "text-[10px]/[1.5]";
  accepted "bg-red-500/[0.5]"

(* A bracket value reaching a custom property is author text. One that would not
   stay inside the declaration it is written into - a top-level [;] or [}], an
   unmatched [)], an unterminated function or block - ends that declaration
   early or swallows the rest of the rule, so the class carrying it is refused
   rather than handed to [Css.custom_property], which raises on it. Tailwind
   emits nothing for any of these either. *)
let test_bracket_value_leaving_the_declaration_is_rejected () =
  List.iter unknown
    [
      "scale-z-[a;b]";
      "scale-z-[0)]";
      "scale-z-[1px}]";
      "bg-linear-[a;b]";
      "bg-linear-[0)/*1]";
      "bg-radial-[}.x{color:red]";
      "mask-t-from-[a;b]";
      "mask-x-to-[var(--x);]";
      "mask-y-from-[1px}]";
      "mask-radial-[a{b]";
    ];
  (* and every value that does stay inside it is still kept *)
  List.iter round_trips
    [
      "scale-z-[1.5]";
      "bg-linear-[45deg]";
      "mask-t-from-[10px]";
      "mask-x-to-[calc(100%-1px)]";
    ]

(* [--spacing(N)] is Tailwind's spacing-scale shorthand, expanded outside any
   quoting. The same bytes inside a quoted string are a CSS string literal, not
   the function, so [expand_spacing_fn] must leave them alone. *)
let test_spacing_shorthand_ignored_in_quotes () =
  let css cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  Alcotest.(check bool)
    "single-quoted content keeps the literal text" true
    (Astring.String.is_infix ~affix:{|content: '--spacing(1)'|}
       (css {|[content:'--spacing(1)']|}));
  Alcotest.(check bool)
    "double-quoted content keeps the literal text" true
    (Astring.String.is_infix ~affix:{|content: "--spacing(1)"|}
       (css {|[content:"--spacing(1)"]|}))

(* An arbitrary value writes a space as [_] and a literal underscore as [\_].
   Every family reads its bracket through this, so the two spellings have to
   stay apart wherever a value reaches the sheet. *)
let test_underscore_escape () =
  let reads name input expected =
    Alcotest.(check string) name expected (Tw.Parse.decode_underscores input)
  in
  reads "a bare underscore is a space" "a_b" "a b";
  reads "an escaped underscore is literal" {|a\_b|} "a_b";
  reads "another escape is left alone" {|a\.b|} {|a\.b|};
  reads "a trailing backslash is kept" {|a\|} {|a\|}

(* A quoted []] belongs to the value, not to the bracket wrapping it, so
   [is_bracket_value] tokenises CSS strings and the backslash escape the way CSS
   Syntax 3 sec. 4.3 does. Every family reads its bracket through that one
   answer, which is why the families below span the whole library. A string left
   open runs to the end of the input, so no []] after it closes the value and
   the class is refused, which is what Tailwind does with an unclosed [url(]
   argument. *)
let test_quoted_bracket_stays_inside_the_value () =
  let holds name input expected =
    Alcotest.(check bool) name expected (Tw.Parse.is_bracket_value input)
  in
  holds "a quoted bracket does not close the value" {|[url('a]b')]|} true;
  holds "a double-quoted bracket does not close it either" {|['My]Font']|} true;
  holds "an escaped bracket does not close it" {|[url(a\]b)]|} true;
  holds "an escaped quote keeps the string open" {|['a\']b']|} true;
  holds "a plain value still reads" "[10px]" true;
  holds "an unclosed string swallows the closing bracket" {|[url('a]b)]|} false;
  holds "an unclosed string with no bracket in it also swallows it"
    {|[url('a)]|} false;
  holds "a trailing backslash escapes the closing bracket" {|[a\]|} false;
  holds "two brackets are not one value" "[10px][20px]" false;
  holds "a quoted bracket does not hide a second bracket" {|[url('a]b')][20px]|}
    false;
  (* cascade writes a CSS string with double quotes and a url() unquoted, both
     of which the canonical differ reads as the value Tailwind writes. *)
  Test_helpers.check_declarations {|bg-[url('a]b')]|}
    [ {|background-image:url(a]b)|} ];
  Test_helpers.check_declarations {|font-['My]Font']|}
    [ {|font-family:"My]Font"|} ];
  Test_helpers.check_declarations {|font-["My]Font"]|}
    [ {|font-family:"My]Font"|} ];
  Test_helpers.check_declarations {|mask-[url('a]b')]|}
    [ {|-webkit-mask-image:url(a]b)|}; {|mask-image:url(a]b)|} ];
  Test_helpers.check_declarations {|list-image-[url('a]b')]|}
    [ {|list-style-image:url(a]b)|} ];
  Test_helpers.check_declarations {|after:content-['a]b']|}
    [ {|--tw-content:"a]b"|}; "content:var(--tw-content)" ];
  round_trips {|shadow-[0_0_0_'a]b']|};
  (* an unclosed string, and a second bracket the string does not hide *)
  List.iter unknown
    [
      {|bg-[url('a]b)]|};
      {|bg-[url('a)]|};
      {|font-['My]Font]|};
      "w-[10px][20px]";
      "text-[12px][14px]";
      {|bg-[url(a\]b)][20px]|};
    ]

let tests =
  Alcotest.
    [
      test_case "underscore escape" `Quick test_underscore_escape;
      test_case "a quoted bracket stays inside the value" `Quick
        test_quoted_bracket_stays_inside_the_value;
      test_case "parse backslash escape in selector" `Quick
        test_escape_in_selector;
      test_case "double bracket class rejected" `Quick
        test_double_bracket_class_rejected;
      test_case "int suffixes reject non-decimal spellings" `Quick
        test_int_rejects_non_decimal_spellings;
      test_case "negative int suffixes reject non-decimal spellings" `Quick
        test_negative_int_rejects_non_decimal_spellings;
      test_case "decimal suffixes reject non-decimal spellings" `Quick
        test_decimal_rejects_non_decimal_spellings;
      test_case "decimal class suffixes round-trip" `Quick
        test_decimal_class_suffixes_round_trip;
      test_case "redundant zero suffixes are rejected" `Quick
        test_redundant_zero_spellings_are_rejected;
      test_case "family int suffixes reject non-decimal spellings" `Quick
        test_family_int_suffixes_reject_non_decimal_spellings;
      test_case "family decimal suffixes reject non-decimal spellings" `Quick
        test_family_decimal_suffixes_reject_non_decimal_spellings;
      test_case "colour modifier rejects non-decimal spellings" `Quick
        test_colour_modifier_rejects_non_decimal_spellings;
      test_case "fraction suffixes reject non-decimal spellings" `Quick
        test_fraction_suffixes_reject_non_decimal_spellings;
      test_case "family suffixes round-trip" `Quick
        test_family_suffixes_round_trip;
      test_case "parsing is independent of parse history" `Quick
        test_parse_is_independent_of_history;
      test_case "var name reads one complete reference" `Quick
        test_extract_var_name_reads_one_reference;
      test_case "bracket value leaving the declaration is rejected" `Quick
        test_bracket_value_leaving_the_declaration_is_rejected;
      test_case "--spacing() shorthand ignored inside quotes" `Quick
        test_spacing_shorthand_ignored_in_quotes;
    ]

let suite = ("parse", tests)
