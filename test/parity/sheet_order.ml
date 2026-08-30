(* The whole-sheet order gate: [measure.sh] reduced to one number that can only
   go down.

   Every other Tailwind oracle in the repo runs the differ in canonical mode,
   which normalises cascade-neutral rule order on purpose, so a family emitted
   in the wrong band is invisible to all of them - [check_ordering_matches]
   included, despite its name. [check_class_order] does read positions back, but
   its call sites pass one family at a time, which pins order inside a family
   and says nothing about where the family sits.

   This measures the whole sheet. Both sides are generated over the committed
   site class list, the top-level statement sequence of a layer is taken out of
   each, and the two are compared over the keys that occur exactly once on both
   sides, where no pairing choice exists. What is reported is the fewest
   statements that have to move for the orders to agree.

   The numbers below are pinned, not aspirational: the gate fails when one goes
   up and prints the new figure when it goes down, so the ceiling can be
   tightened. *)

module Tailwind_gen = Tw_tools.Tailwind_gen

(* Measured 2026-08-30 over [classlist.txt], 4825 classes of tailwindcss.com,
   against cascade main at e829b2d6. Both keys and grouping come out of
   cascade's printer, so the number is only comparable against the cascade a run
   was built with, and every run prints which one that was. CI resolves cascade
   through opam from the range [dune-project] pins; a local [cascade/] symlink
   sitting on someone's branch is the first thing to check when the count moves
   for no reason in the sort code.

   [pairs] is pinned as a floor as well: a sheet that lost most of its rules
   would otherwise have nothing left to be out of order, and the gate would read
   that as a pass. *)
let pinned =
  [
    ("utilities", `Moves 331, `Pairs 3800); ("components", `Moves 0, `Pairs 45);
  ]

(* Skipping is right on a machine with no Tailwind CLI and wrong in CI, where it
   reports a sheet as correctly ordered because nothing looked. Set
   TW_TAILWIND_TESTS=1 where the CLI is meant to be present and a missing or
   off-version one fails instead. *)
let required () = Sys.getenv_opt "TW_TAILWIND_TESTS" = Some "1"

let read_file path =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () -> really_input_string ic (in_channel_length ic))

(* tw's half comes from the built binary rather than from the library, so what
   is measured is the sheet the documented command produces, entrypoint
   compilation and all. *)
let tw_sheet tw_bin =
  let out = Filename.temp_file ~temp_dir:"." "tw_all" ".css" in
  let remove () = try Sys.remove out with Sys_error _ -> () in
  Fun.protect ~finally:remove @@ fun () ->
  let cmd =
    Fmt.str "%s --input-css globals.css --minify classlist.txt > %s"
      (Filename.quote tw_bin) (Filename.quote out)
  in
  if Sys.command cmd <> 0 then failwith ("failed to run " ^ tw_bin);
  read_file out

let sample = 25

let report_moved moved =
  let shown = List.filteri (fun i _ -> i < sample) moved in
  List.iter
    (fun (key, tailwind_rank, tw_rank) ->
      Fmt.pr "    %-58s Tailwind #%d, tw #%d@." key tailwind_rank tw_rank)
    shown;
  let hidden = List.length moved - List.length shown in
  if hidden > 0 then Fmt.pr "    ... and %d more@." hidden

let check ~tailwind ~tw (layer, `Moves ceiling, `Pairs floor) =
  let gap = Test_helpers.sheet_order_gap ~layer ~tailwind ~tw in
  Fmt.pr "@@layer %s: %d of %d unambiguous pairs must move (ceiling %d)@." layer
    gap.Test_helpers.moves gap.Test_helpers.pairs ceiling;
  if gap.Test_helpers.pairs < floor then begin
    Fmt.pr
      "  FAIL: only %d pairs left to compare, was at least %d. The measurement \
       got weaker, so the count above says less than it did.@."
      gap.Test_helpers.pairs floor;
    false
  end
  else if gap.Test_helpers.moves > ceiling then begin
    Fmt.pr
      "  FAIL: %d more statements are out of Tailwind's order than before.@."
      (gap.Test_helpers.moves - ceiling);
    Fmt.pr "  What moved, %d of %d shown:@."
      (min sample gap.Test_helpers.moves)
      gap.Test_helpers.moves;
    report_moved gap.Test_helpers.moved;
    Fmt.pr
      "  The ranks are positions among the paired statements, so a large gap \
       is a family sorted into the wrong band.@.";
    false
  end
  else begin
    if gap.Test_helpers.moves < ceiling then
      Fmt.pr
        "  IMPROVED: %d fewer than the pinned %d. Tighten the ceiling in \
         test/parity/sheet_order.ml to %d.@."
        (ceiling - gap.Test_helpers.moves)
        ceiling gap.Test_helpers.moves;
    true
  end

let () =
  (* Which cascade this run compiled against, since the count depends on it. *)
  Tw_tools.Cascade_provenance.report ();
  let tw_bin =
    if Array.length Sys.argv > 1 then Sys.argv.(1)
    else failwith "usage: sheet_order.exe <path to the tw binary>"
  in
  (match Tailwind_gen.availability () with
  | Ok () -> ()
  | Error reason ->
      if required () then begin
        Fmt.epr "whole-sheet order gate: TW_TAILWIND_TESTS=1 but %s@." reason;
        exit 1
      end
      else begin
        Fmt.pr
          "whole-sheet order gate SKIPPED: %s. Set TW_TAILWIND_TESTS=1 to make \
           that a failure.@."
          reason;
        exit 0
      end);
  let tailwind = Tailwind_gen.generate_entrypoint "ref-entry.css" in
  let tw = tw_sheet tw_bin in
  let ok = List.for_all Fun.id (List.map (check ~tailwind ~tw) pinned) in
  if not ok then begin
    Fmt.pr
      "@.Re-derive by hand with `sh test/parity/measure.sh`; docs/parity.md \
       reads the output.@.";
    exit 1
  end
