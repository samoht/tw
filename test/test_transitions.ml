let check = Test_helpers.check_handler_roundtrip (module Tw.Transitions.Handler)

let test_roundtrip () =
  check "transition-none";
  check "transition-all";
  check "transition-colors";
  check "transition-opacity";
  check "transition-shadow";
  check "transition-transform";
  check "transition";
  check "duration-150";
  check "duration-300";
  check "delay-150";
  check "delay-300";
  (* Arbitrary delay accepts both time units and var(). *)
  check "delay-[300ms]";
  check "delay-[var(--d)]";
  check "ease-linear";
  check "ease-in";
  check "ease-out";
  check "ease-in-out";
  (* the initial keyword resets the duration/ease channel *)
  check "duration-initial";
  check "ease-initial"

let test_invalid () =
  Test_helpers.check_invalid_input (module Tw.Transitions.Handler) "duration";
  Test_helpers.check_invalid_input (module Tw.Transitions.Handler) "delay";
  Test_helpers.check_invalid_input (module Tw.Transitions.Handler) "ease"

(* duration-initial / ease-initial reset their channel var to the CSS initial
   keyword. *)
let test_initial_resets () =
  let css cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string ~minify:true
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  Alcotest.(check bool)
    "duration-initial sets --tw-duration:initial" true
    (Astring.String.is_infix ~affix:"--tw-duration:initial"
       (css "duration-initial"));
  Alcotest.(check bool)
    "ease-initial sets --tw-ease:initial" true
    (Astring.String.is_infix ~affix:"--tw-ease:initial" (css "ease-initial"))

let tests =
  Test_helpers.standard ~roundtrip:test_roundtrip ~invalid:test_invalid
  @ [ Alcotest.test_case "initial resets" `Quick test_initial_resets ]

let suite = ("transitions", tests)
