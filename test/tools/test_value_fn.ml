open Alcotest

(* [--spacing(N)] is a reference to the spacing scale, except under an [@theme
   inline] [--spacing]: there the token has no declaration to reference, so the
   step is multiplied out. *)
let test_spacing_shorthand () =
  let scheme inline =
    Tw.Scheme.with_overrides ~inline Tw.Scheme.default [ ("spacing", "4px") ]
  in
  check string "a declared token is referenced"
    ".a { margin: calc(var(--spacing) * 12) }"
    (Tw_tools.Value_fn.expand_spacing ~theme:(scheme [])
       ".a { margin: --spacing(12) }");
  check string "an inline token is worked out" ".a { margin: 48px }"
    (Tw_tools.Value_fn.expand_spacing ~theme:(scheme [ "spacing" ])
       ".a { margin: --spacing(12) }")

let tests = [ test_case "spacing shorthand" `Quick test_spacing_shorthand ]
let suite = ("value_fn", tests)
