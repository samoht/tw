open Alcotest
open Test_helpers

let check = check_handler_roundtrip (module Tw.Letter_spacing.Handler)

let test_letter_spacing () =
  List.iter
    (fun v -> check ("tracking-" ^ v))
    [ "tighter"; "tight"; "normal"; "wide"; "wider"; "widest" ]

(* [tracking-[...]] forwards any safe declaration value. *)
let test_arbitrary_tracking_token_stream () =
  let renders cls =
    match Tw.of_string cls with
    | Ok u -> ignore (Tw.to_css ~base:false [ u ] |> Tw.Css.to_string)
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  renders "tracking-[foo]";
  renders "tracking-[red]";
  renders "tracking-[45deg]";
  renders "tracking-[1e]";
  renders "tracking-[a,b]";
  renders "-tracking-[foo]";
  renders "tracking-[1px]";
  renders "tracking-[.5em]";
  renders "tracking-[-1px]";
  renders "tracking-[2%]";
  renders "tracking-[calc(1px_+_2px)]";
  renders "tracking-[var(--x)]";
  renders "-tracking-[.5em]"

(* tracking-normal's token must keep the em unit (0em), not collapse to 0. *)
let test_tracking_normal_unit () =
  let css = Tw.to_css [ Tw.tracking_normal ] |> Tw.Css.to_string ~minify:true in
  (* The subject is the :root binding, which declarations_of_class leaves out by
     design, so this stays a substring. *)
  Alcotest.check bool "tracking-normal token keeps em unit" true
    (Astring.String.is_infix ~affix:"--tracking-normal:0em" css)

(* A [--tracking-*] token the project declared in its [@theme] names a letter
   spacing the built-in scale has no slot for. Tailwind generates the utility
   from it, channel variable included; tw rejected the class outright. *)
let test_project_tracking_token () =
  let theme =
    Tw.Scheme.with_overrides Tw.Scheme.default [ ("tracking-airy", "0.2em") ]
  in
  Test_helpers.check_declarations ~theme ~minify:false "tracking-airy"
    [
      "--tw-tracking: var(--tracking-airy)";
      "letter-spacing: var(--tracking-airy)";
    ];
  Alcotest.(check bool)
    "an undeclared tracking name is rejected" true
    (Result.is_error (Tw.of_string ~theme "tracking-nope"))

(* [tracking-tightest] names no step of the default scale. *)
let test_invalid () =
  check_invalid_input (module Tw.Letter_spacing.Handler) "tracking-tightest"

let tests =
  [
    test_case "letter spacing" `Quick test_letter_spacing;
    test_case "arbitrary tracking token stream" `Quick
      test_arbitrary_tracking_token_stream;
    test_case "tracking-normal unit" `Quick test_tracking_normal_unit;
    test_case "project tracking token" `Quick test_project_tracking_token;
    test_case "invalid" `Quick test_invalid;
  ]

let suite = ("letter_spacing", tests)
