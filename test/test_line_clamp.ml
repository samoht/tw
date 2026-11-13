open Alcotest
open Test_helpers

let check = check_handler_roundtrip (module Tw.Line_clamp.Handler)

let test_line_clamp () =
  check "line-clamp-0";
  check "line-clamp-3"

(* A project [@theme] override of [--line-clamp-none] is a namespace key, not a
   value Tailwind type-checks at build time: any override at all, decimal or
   not, switches line-clamp-none to the variable-driven form. Reading the value
   with [int_of_string_opt] read [0x3] as the integer 3 (an OCaml-only spelling)
   yet rejected a non-numeric override like [banana], which Tailwind still
   honours the same way. *)
let test_line_clamp_none_theme_override () =
  let css theme cls =
    match Tw.of_string ~theme cls with
    | Ok u -> Tw.to_css ~theme ~base:false [ u ] |> Tw.Css.to_string
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  let hex =
    Tw.Scheme.with_overrides Tw.Scheme.default [ ("line-clamp-none", "0x3") ]
  in
  let variable_form theme =
    (* The whole list: the box properties travel with the clamp, so an affix on
       the clamp alone passes on a utility that lost them. *)
    Test_helpers.check_declarations ~theme ~minify:false "line-clamp-none"
      [
        "-webkit-line-clamp: var(--line-clamp-none)";
        "-webkit-box-orient: vertical";
        "display: -webkit-box";
        "overflow: hidden";
      ]
  in
  variable_form hex;
  (* The binding itself is a :root declaration, which declarations_of_class
     leaves out by design, so this one stays a substring. *)
  Alcotest.(check bool)
    "the theme layer keeps the override text as authored" true
    (Astring.String.is_infix ~affix:"--line-clamp-none: 0x3"
       (css hex "line-clamp-none"));
  let non_numeric =
    Tw.Scheme.with_overrides Tw.Scheme.default [ ("line-clamp-none", "banana") ]
  in
  variable_form non_numeric

(* line-clamp sits between box-sizing and the display family in Tailwind's
   order, not among the typography utilities its class name suggests. *)
let line_clamp_sorts_with_box_sizing () =
  let order cls =
    Tw.Utility.base_of_class Tw.Scheme.default cls
    |> Result.get_ok |> Tw.Utility.order
  in
  let named_priority, _ = order "line-clamp-2" in
  let arbitrary_priority, _ = order "line-clamp-[<value>]" in
  Alcotest.(check int)
    "arbitrary line-clamp stays in the line-clamp property band" named_priority
    arbitrary_priority;
  let classes =
    [ "indent-4"; "line-clamp-2"; "block"; "box-border"; "ml-auto" ]
  in
  let utilities = List.map (fun c -> Result.get_ok (Tw.of_string c)) classes in
  Test_helpers.check_ordering_matches ~test_name:"line-clamp order" utilities

let tests =
  [
    test_case "line clamp" `Quick test_line_clamp;
    test_case "line-clamp-none theme override" `Quick
      test_line_clamp_none_theme_override;
    test_case "line-clamp sorts with box-sizing" `Quick
      line_clamp_sorts_with_box_sizing;
  ]

let suite = ("line_clamp", tests)
