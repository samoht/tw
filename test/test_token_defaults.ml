module Token_defaults = Tw.Private.Token_defaults

(* The table is process-wide and every utility family fills it at module-init,
   so tests use names no family owns rather than assuming an empty table. *)
let name = "test-token-defaults-probe"

let test_find_is_none_for_an_unowned_token () =
  Alcotest.(check (option string))
    "no family owns it" None
    (Token_defaults.find "test-token-defaults-nobody-registers-this")

let test_register_then_find () =
  Token_defaults.register name "1rem";
  Alcotest.(check (option string))
    "reads back" (Some "1rem") (Token_defaults.find name)

let test_register_replaces () =
  Token_defaults.register name "1rem";
  Token_defaults.register name "2rem";
  Alcotest.(check (option string))
    "second wins" (Some "2rem") (Token_defaults.find name);
  let occurrences =
    List.length (List.filter (fun (k, _) -> k = name) (Token_defaults.all ()))
  in
  Alcotest.(check int) "one entry, not two" 1 occurrences

let test_all_contains_a_registered_token () =
  Token_defaults.register name "3rem";
  Alcotest.(check bool)
    "listed" true
    (List.mem (name, "3rem") (Token_defaults.all ()))

(* [--spacing] is the one default the library itself publishes, so it is there
   whatever else a test registered. *)
let test_spacing_default_is_published () =
  Alcotest.(check (option string))
    "spacing step" (Some ".25rem")
    (Token_defaults.find "spacing")

let tests =
  Alcotest.
    [
      test_case "find is none for an unowned token" `Quick
        test_find_is_none_for_an_unowned_token;
      test_case "register then find" `Quick test_register_then_find;
      test_case "register replaces" `Quick test_register_replaces;
      test_case "all contains a registered token" `Quick
        test_all_contains_a_registered_token;
      test_case "spacing default is published" `Quick
        test_spacing_default_is_published;
    ]

let suite = ("token_defaults", tests)
