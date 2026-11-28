open Alcotest
open Test_helpers

let check = check_handler_roundtrip (module Tw.Content.Handler)

let test_content () =
  check "content-none";
  (* Arbitrary content round-trips with its original quote style, single quotes
     included. *)
  check "content-[\"x\"]";
  check "content-['x']";
  (* unquoted function values *)
  check "content-[attr(before)]";
  check "content-[counter(x)]";
  (* the bracket binds the channel and [content] reads it back *)
  check_declarations "content-[attr(before)]"
    [ "--tw-content:attr(before)"; "content:var(--tw-content)" ]

(* The bracket is a CSS string, so an escape in it is the author's: Tailwind
   keeps [\201C] and the browser shows the code point, where reading it as text
   would escape the backslash again. *)
let test_content_escape () =
  check {|content-["\201C"]|};
  check_declarations ~minify:false {|content-["\201C"]|}
    [ {|--tw-content: "\201C"|}; "content: var(--tw-content)" ];
  check_declarations {|content-['\201C']|}
    [ "--tw-content:\"\u{201C}\""; "content:var(--tw-content)" ]

(* content-<token> parses only when the @theme defines --content-<token>; a bare
   word like content-wrapper with no token is rejected, as Tailwind emits
   nothing for it. *)
let test_content_named_requires_theme () =
  Test_helpers.check_invalid_input ~why:Test_helpers.Not_a_utility
    (module Tw.Content.Handler)
    "content-wrapper";
  let themed =
    Tw.Scheme.with_overrides Tw.Scheme.default [ ("content-slash", "\"/\"") ]
  in
  match Tw.Content.Handler.of_class themed "content-slash" with
  | Ok _ -> ()
  | Error (`Msg m) -> Alcotest.failf "content-slash with theme rejected: %s" m

let tests =
  [
    test_case "content" `Quick test_content;
    test_case "content keeps the bracket's escapes" `Quick test_content_escape;
    test_case "content-named requires theme token" `Quick
      test_content_named_requires_theme;
  ]

let suite = ("content", tests)
