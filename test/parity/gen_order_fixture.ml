(* Regenerates [tailwind_order.txt]: the identity sequence of Tailwind's [@layer
   utilities], read out of a reference sheet the CLI produced.

   Freezing it is what lets the order check in [test_sort.ml] run on a machine
   with no pinned CLI, where every other oracle that sees order skips. Rerun
   against a fresh [tmp/parity/ref_local.css] whenever the pinned tailwindcss
   moves:

   sh test/parity/measure.sh dune exec test/parity/gen_order_fixture.exe -- \
   tmp/parity/ref_local.css > test/parity/tailwind_order.txt

   The sheet handed in has to be one the CLI produced for the same input the
   checking side generates, or the two sequences are not comparable. *)

let () =
  match Sys.argv with
  | [| _; path |] ->
      let ic = open_in_bin path in
      let s = really_input_string ic (in_channel_length ic) in
      close_in ic;
      List.iter print_endline
        (Test_helpers.layer_statement_identities s ~layer:"utilities")
  | _ ->
      prerr_endline "usage: gen_order_fixture.exe <reference-sheet.css>";
      exit 2
