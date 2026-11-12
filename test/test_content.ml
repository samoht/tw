open Alcotest
open Test_helpers

let check = check_handler_roundtrip (module Tw.Content.Handler)

let test_content () =
  check "content-none";
  (* Arbitrary content round-trips with its original quote style: the
     double-quoted form already worked, the single-quoted form used to mangle to
     content-[["x"]]. *)
  check "content-[\"x\"]";
  check "content-['x']";
  (* unquoted function values *)
  check "content-[attr(before)]";
  check "content-[counter(x)]";
  (* the bracket binds the channel and [content] reads it back *)
  check_declarations "content-[attr(before)]"
    [ "--tw-content:attr(before)"; "content:var(--tw-content)" ]

(* content-<token> parses only when the @theme defines --content-<token>; a bare
   word like content-wrapper with no token is rejected (it used to parse as a
   named content value, a false positive Tailwind does not emit). *)
let test_content_named_requires_theme () =
  (match Tw.Content.Handler.of_class Tw.Scheme.default "content-wrapper" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "content-wrapper should be rejected without a token");
  let themed =
    Tw.Scheme.with_overrides Tw.Scheme.default [ ("content-slash", "\"/\"") ]
  in
  match Tw.Content.Handler.of_class themed "content-slash" with
  | Ok _ -> ()
  | Error (`Msg m) -> Alcotest.failf "content-slash with theme rejected: %s" m

let tests =
  [
    test_case "content" `Quick test_content;
    test_case "content-named requires theme token" `Quick
      test_content_named_requires_theme;
  ]

let suite = ("content", tests)
