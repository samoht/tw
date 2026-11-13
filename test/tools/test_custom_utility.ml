open Alcotest

let pair_list = list (pair string string)

let test_take () =
  let css, defs =
    Tw_tools.Custom_utility.take
      "@utility line-y { border-block: 1px solid }\n\
       @utility tab-* { tab-size: --value(integer) }\n"
  in
  check string "declarations removed" "\n\n" css;
  (* The functional form keeps its [-*]: that is what tells a candidate's root
     from the whole name a static declaration spells. *)
  check pair_list "both forms are read"
    [
      ("tab-*", " tab-size: --value(integer) ");
      ("line-y", " border-block: 1px solid ");
    ]
    defs

(* {2 Functional utilities} *)

(* The CSS each candidate gets from the declarations, as [class -> minified
   rule], so a candidate the declarations resolve nothing for shows up as an
   absent entry rather than an empty one. *)
let routed_css ~theme ~udefs candidates =
  let _, entries, _ =
    Tw_tools.Routing.custom_routed_utilities ~theme ~defs:[] ~udefs candidates
  in
  List.map
    (fun (cls, _, statements) ->
      (cls, Cascade.Css.to_string ~minify:true (Cascade.Css.v statements)))
    entries

let check_routed ~udefs ?(theme = Tw.Scheme.default) msg expected candidates =
  check pair_list msg expected
    (List.sort compare (routed_css ~theme ~udefs candidates))

(* [@utility example-* { ... }] declares a utility whose candidate carries a
   value, read back in the body with [--value(...)]. A candidate the reads do
   not all resolve is no utility of that declaration and produces nothing. *)
let test_functional_value () =
  let udefs = [ ("example-*", " --resolved-value: --value(integer) ") ] in
  check_routed ~udefs "the integer the candidate spells"
    [
      ("example-1", ".example-1{--resolved-value:1}");
      ("example-76", ".example-76{--resolved-value:76}");
    ]
    [ "example-1"; "example-76"; "example-foo"; "example"; "example-2.5" ]

(* Curly blocks are declaration boundaries rather than opaque values: a nested
   rule may itself contain a functional declaration that needs resolving. *)
let test_functional_nested_rule () =
  let udefs =
    [ ("example-*", " &::before { --resolved-value: --value(integer); } ") ]
  in
  check_routed ~udefs "the declaration under the nested rule is resolved"
    [ ("example-7", ".example-7:before{--resolved-value:7}") ]
    [ "example-7" ]

(* A [--value(--namespace)] reads the theme entry the candidate names. How it is
   spelled follows the block the token was declared in: a plain one is a
   reference the theme layer declares, an [@theme reference] one carries its
   value as the fallback, an [@theme inline] one stands for the value. *)
let test_functional_theme_value () =
  let udefs = [ ("example-*", " --resolved-value: --value(--example) ") ] in
  let theme ?inline ?reference () =
    Tw.Scheme.with_overrides ?inline ?reference Tw.Scheme.default
      [ ("example-a", "8") ]
  in
  check_routed ~udefs ~theme:(theme ()) "a declared token is referenced"
    [ ("example-a", ".example-a{--resolved-value:var(--example-a)}") ]
    [ "example-a"; "example-b" ];
  check_routed ~udefs
    ~theme:(theme ~reference:[ "example-a" ] ())
    "a reference token carries its value"
    [ ("example-a", ".example-a{--resolved-value:var(--example-a,8)}") ]
    [ "example-a" ];
  check_routed ~udefs
    ~theme:(theme ~inline:[ "example-a" ] ())
    "an inline token stands for its value"
    [ ("example-a", ".example-a{--resolved-value:8}") ]
    [ "example-a" ]

(* An arbitrary value resolves against [--value([type])], which reads the hint
   the candidate spelled when there is one and infers the type otherwise. *)
let test_functional_arbitrary_value () =
  let udefs = [ ("example-*", " --resolved-value: --value([integer]) ") ] in
  check_routed ~udefs "only the values that read as integers"
    [
      ("example-[1]", ".example-\\[1\\]{--resolved-value:1}");
      ( "example-[integer:var(--my-value)]",
        ".example-\\[integer\\:var\\(--my-value\\)\\]{--resolved-value:var(--my-value)}"
      );
    ]
    [
      "example-[1]";
      "example-[1px]";
      "example-[integer:var(--my-value)]";
      "example-[color:var(--my-value)]";
      "example-(--my-value)";
    ]

(* [--modifier(...)] reads the [/half] of the candidate, and [--default(...)]
   answers for the half - or the value - the candidate left out. A modifier the
   body never resolves makes the whole candidate invalid. *)
let test_functional_modifier () =
  let udefs =
    [
      ( "example-*",
        " --resolved-value: --value(integer, --default(12)); \
         --resolved-modifier: --modifier(integer) " );
    ]
  in
  check_routed ~udefs "the modifier, and the default for the value"
    [
      ("example", ".example{--resolved-value:12}");
      ("example-1/1", ".example-1\\/1{--resolved-value:1;--resolved-modifier:1}");
      ("example/25", ".example\\/25{--resolved-value:12;--resolved-modifier:25}");
    ]
    [ "example"; "example/25"; "example-1/1"; "example/foo" ]

let tests =
  [
    test_case "custom utilities taken" `Quick test_take;
    test_case "functional value" `Quick test_functional_value;
    test_case "functional nested rule" `Quick test_functional_nested_rule;
    test_case "functional theme value" `Quick test_functional_theme_value;
    test_case "functional arbitrary value" `Quick
      test_functional_arbitrary_value;
    test_case "functional modifier" `Quick test_functional_modifier;
  ]

let suite = ("custom_utility", tests)
