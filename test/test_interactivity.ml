open Alcotest

let check class_name =
  match Tw.Interactivity.Handler.of_class Tw.Scheme.default class_name with
  | Ok u ->
      check string "interactivity class" class_name
        (Tw.Interactivity.Handler.to_class u)
  | Error (`Msg msg) -> fail msg

let test_select () = check "select-none"

let test_scroll_snap () =
  check "scroll-smooth";
  check "snap-center"

let test_of_string_invalid () =
  (* Invalid interactivity utilities *)
  let test_invalid input =
    let class_name = String.concat "-" input in
    match Tw.Interactivity.Handler.of_class Tw.Scheme.default class_name with
    | Ok _ -> fail ("Expected error for: " ^ String.concat "-" input)
    | Error _ -> ()
  in

  (* Invalid select values *)
  test_invalid [ "select" ];
  (* Missing value *)
  test_invalid [ "select"; "invalid" ];

  (* Invalid value *)

  (* Invalid scroll values *)
  test_invalid [ "scroll" ];
  (* Missing value *)
  test_invalid [ "scroll"; "invalid" ];

  (* Invalid value *)

  (* Invalid snap values *)
  test_invalid [ "snap" ];
  (* Missing value *)
  test_invalid [ "snap"; "invalid" ];
  (* Invalid value *)
  test_invalid [ "snap"; "align" ];

  (* Incomplete snap-align-none *)

  (* Invalid resize values *)
  test_invalid [ "resize"; "invalid" ];

  (* Invalid value *)

  (* Invalid pointer events *)
  test_invalid [ "pointer"; "events" ];
  (* Missing value *)
  test_invalid [ "pointer"; "events"; "invalid" ];

  (* Invalid value *)

  (* Invalid will-change *)
  test_invalid [ "will"; "change" ];
  (* Missing value *)
  test_invalid [ "will"; "change"; "invalid" ];

  (* Invalid value *)

  (* Invalid prefixes *)
  test_invalid [ "appearance" ];
  (* Incomplete *)
  test_invalid [ "unknown" ];
  (* Unknown *)
  test_invalid []
(* Empty *)

let suborder_matches_tailwind () =
  let open Tw in
  let shuffled =
    Test_helpers.shuffle
      [ select_none; select_text; select_all; scroll_auto; scroll_smooth ]
  in

  Test_helpers.check_ordering_matches
    ~test_name:"interactivity suborder matches Tailwind" shuffled

(* Tailwind's utility order opens with the container utilities and then
   pointer-events, before the layout group. *)
let pointer_events_sorts_first () =
  let open Tw in
  let shuffled =
    Test_helpers.shuffle
      [ absolute; pointer_events_none; flex; at_container; pointer_events_auto ]
  in
  Test_helpers.check_ordering_matches
    ~test_name:"pointer-events sorts before the layout group" shuffled

(* [will-change] takes property names, so the docs' [<value>] placeholder is not
   one; it used to reach the sheet as will-change: <value>. *)
let test_invalid_arbitrary_will_change () =
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
  rejected "will-change-[<value>]";
  accepted "will-change-[opacity]";
  accepted "will-change-[opacity,transform]";
  accepted "will-change-[var(--x)]"

(* A property name is an identifier, and an identifier may carry an underscore.
   The arbitrary value spells that one [\_], so the class both parses and keeps
   the character. *)
let test_will_change_underscore_escape () =
  let css cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  Alcotest.(check bool)
    "an escaped underscore stays in the property name" true
    (Astring.String.is_infix ~affix:"will-change: a_b"
       (css {|will-change-[a\_b]|}))

let tests =
  [
    test_case "will-change underscore escape" `Quick
      test_will_change_underscore_escape;
    test_case "select" `Quick test_select;
    test_case "scroll+snap" `Quick test_scroll_snap;
    test_case "of_string invalid cases" `Quick test_of_string_invalid;
    test_case "interactivity suborder matches Tailwind" `Quick
      suborder_matches_tailwind;
    test_case "pointer-events sorts first" `Quick pointer_events_sorts_first;
    test_case "invalid arbitrary will-change" `Quick
      test_invalid_arbitrary_will_change;
  ]

let suite = ("interactivity", tests)
