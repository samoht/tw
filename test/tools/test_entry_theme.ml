open Alcotest
open Tw_tools.Entry_theme

let string_list = list string
let pair_list = list (pair string string)

let theme_src =
  "@import \"tailwindcss\" theme(static);\n\
   @theme {\n\
  \  --color-*: initial;\n\
  \  --color-brand: red;\n\
  \  --animate-flash: flash 2s;\n\
  \  @keyframes flash { to { opacity: 0 } }\n\
  \  --spacing: 0.3rem;\n\
   }\n\
   @theme inline {\n\
  \  --font-x: var(--font-ext), system-ui;\n\
   }\n"

let test_theme_overrides () =
  let tokens, inline = overrides_of_css theme_src in
  check pair_list "every token each block declares, in source order"
    [
      ("color-*", "initial");
      ("color-brand", "red");
      ("animate-flash", "flash 2s");
      ("spacing", "0.3rem");
      ("font-x", "var(--font-ext), system-ui");
    ]
    tokens;
  check string_list "only the inline block's names" [ "font-x" ] inline

(* [--<ns>-*: initial] takes a whole namespace out of the theme. The name is not
   a <dashed-ident>, so no CSS parser can build a declaration from it and the
   reset has to be read off the token stream; without it a project's reset never
   reaches the renderer, which then keeps the built-in scale. *)
let test_theme_namespace_reset () =
  let tokens, _ =
    overrides_of_css
      "@theme {\n  --breakpoint-*: initial;\n  --breakpoint-tablet: 800px;\n}"
  in
  check pair_list "the reset stands among the tokens beside it"
    [ ("breakpoint-*", "initial"); ("breakpoint-tablet", "800px") ]
    tokens;
  let tokens, _ =
    overrides_of_css "@theme { --example-*: [x); y]; --example-one: ok; }"
  in
  check pair_list "a mismatched closer does not end the open square block"
    [ ("example-*", "[x); y]"); ("example-one", "ok") ]
    tokens

let test_hoist_keyframes () =
  check string "keyframes lifted out of the theme block"
    "@theme {\n\
    \  --animate-flash: flash 2s;\n\
    \  \n\
     }\n\
     @keyframes flash { to { opacity: 0 } }"
    (hoist_keyframes
       "@theme {\n\
       \  --animate-flash: flash 2s;\n\
       \  @keyframes flash { to { opacity: 0 } }\n\
        }\n")

let tests =
  [
    test_case "theme overrides" `Quick test_theme_overrides;
    test_case "theme namespace reset" `Quick test_theme_namespace_reset;
    test_case "theme keyframes hoisted" `Quick test_hoist_keyframes;
  ]

let suite = ("entry_theme", tests)
