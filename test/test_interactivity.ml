open Alcotest

let check =
  Test_helpers.check_handler_roundtrip (module Tw.Interactivity.Handler)

let test_select () = check "select-none"

let test_scroll_snap () =
  check "scroll-smooth";
  check "snap-center"

let test_of_string_invalid () =
  (* Invalid interactivity utilities *)
  let test_invalid =
    Test_helpers.check_invalid_parts ~why:Test_helpers.Not_a_utility
      (module Tw.Interactivity.Handler)
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

(* [will-change] takes property names, and a bracket holding anything else is
   still a utility: Tailwind writes it into will-change as the author spelled
   it, and the browser drops it. *)
let test_arbitrary_will_change_falls_through () =
  let has cls value =
    Test_helpers.check_declarations ~minify:false cls
      [ "will-change: " ^ value ]
  in
  has "will-change-[opacity]" "opacity";
  has "will-change-[10px]" "10px";
  has "will-change-[calc(1+2)]" "calc(1 + 2)";
  has "will-change-[url(x.png)_center]" "url(x.png) center";
  has "will-change-[<value>]" "<value>";
  has "will-change-[length:10px]" "10px"

let test_invalid_arbitrary_will_change () =
  Test_helpers.check_invalid_input
    (module Tw.Interactivity.Handler)
    "will-change-[a;b]"

(* A property name is an identifier, and an identifier may carry an underscore.
   The arbitrary value spells that one [\_], so the class both parses and keeps
   the character. *)
let test_will_change_underscore_escape () =
  Test_helpers.check_declarations ~minify:false {|will-change-[a\_b]|}
    [ "will-change: a_b" ]

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
    test_case "arbitrary will-change falls through" `Quick
      test_arbitrary_will_change_falls_through;
    test_case "invalid arbitrary will-change" `Quick
      test_invalid_arbitrary_will_change;
  ]

let suite = ("interactivity", tests)
