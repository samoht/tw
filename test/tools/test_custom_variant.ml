open Alcotest
open Tw_tools.Custom_variant

let string_list = list string
let pair_list = list (pair string string)

let test_segments () =
  check string_list "plain class" [ "flex" ] (segments "flex");
  check string_list "stacked variants" [ "lg"; "dark"; "flex" ]
    (segments "lg:dark:flex");
  check string_list "colon inside brackets" [ "[&>*]"; "flex" ]
    (segments "[&>*]:flex");
  check string_list "colon inside parens"
    [ "supports-(--x)"; "flex" ]
    (segments "supports-(--x):flex")

let test_split_declared () =
  let defs = [ ("dark", "&:where(.dark, .dark *) { @slot; }") ] in
  check (pair string_list string) "declared variant taken from the middle"
    ([ "dark" ], "lg:flex")
    (split_declared defs "lg:dark:flex");
  check (pair string_list string) "nothing declared" ([], "lg:hover:flex")
    (split_declared defs "lg:hover:flex")

let test_fill_slots () =
  check string "slot replaced by the body"
    "&:where(.dark, .dark *) { color: red }"
    (fill_slots "&:where(.dark, .dark *) { @slot; }" "color: red")

let test_take () =
  let css, defs =
    take
      "@custom-variant dark { &:where(.dark, .dark *) { @slot; } }\n\
       .a { color: red }\n"
  in
  check string "declaration removed" "\n.a { color: red }\n" css;
  check pair_list "name and template"
    [ ("dark", " &:where(.dark, .dark *) { @slot; } ") ]
    defs

let tests =
  [
    test_case "variant segments" `Quick test_segments;
    test_case "declared variants split out" `Quick test_split_declared;
    test_case "slots filled" `Quick test_fill_slots;
    test_case "custom variants taken" `Quick test_take;
  ]

let suite = ("custom_variant", tests)
