open Alcotest

let string_list = list string

(* Tailwind writes the negated arm with [&] read as a universal, and its
   minifier drops that universal where the compound already has a simple
   selector, so the CLI's sheet spells [:not(:where(...))]. Kept, it is bytes
   the CLI's sheet does not carry. A universal standing on its own stays. *)
let test_negated_variant_spelling () =
  let defs, refused =
    Tw_tools.Negated_variant.add
      [
        ("dark", "&:where(.dark, .dark *) { @slot; }");
        ("inside", ".box & { @slot; }");
      ]
  in
  check (option string) "the implied universal is dropped"
    (Some "&:not(:where(.dark, .dark *)) { @slot; }")
    (List.assoc_opt "not-dark" defs);
  check (option string) "a lone universal stays"
    (Some "&:not(.box *) { @slot; }")
    (List.assoc_opt "not-inside" defs);
  check string_list "nothing refused" [] refused

let tests =
  [ test_case "negated variant spelling" `Quick test_negated_variant_spelling ]

let suite = ("negated_variant", tests)
