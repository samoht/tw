(* The order gate beside this one, with Tailwind's half frozen.

   [sheet_order.ml] regenerates both sheets, so it needs the pinned tailwindcss
   and skips without it. Every other oracle that sees utility order does too:
   [check_class_order] skips, and [measured_inversions] builds Tailwind's sheet
   to compare against. So on a machine with no CLI nothing watches the order at
   all, and dropping the suborder tier of [compare_order] leaves the suite green
   while putting 418 of 3961 statements out of place.

   This reads Tailwind's sequence out of [tailwind_order.txt], committed beside
   the inputs, and generates only tw's half. It needs tw's own binary, which
   dune builds, and nothing else. [gen_order_fixture.ml] regenerates the file
   when the pinned tailwindcss moves. *)

let lines path =
  In_channel.with_open_bin path In_channel.input_all
  |> String.split_on_char '\n'
  |> List.filter (fun l -> l <> "")

let tw_sheet tw_bin =
  let out = Filename.temp_file ~temp_dir:"." "tw_frozen" ".css" in
  let remove () = try Sys.remove out with Sys_error _ -> () in
  Fun.protect ~finally:remove @@ fun () ->
  let cmd =
    Filename.quote_command tw_bin ~stdout:out
      [ "--input-css"; "globals.css"; "--minify"; "classlist.txt" ]
  in
  if Sys.command cmd <> 0 then failwith ("failed to run " ^ tw_bin);
  In_channel.with_open_bin out In_channel.input_all

let () =
  let tw_bin = Sys.argv.(1) in
  let tailwind = lines "tailwind_order.txt" in
  let tw =
    Test_helpers.layer_statement_identities (tw_sheet tw_bin) ~layer:"utilities"
  in
  let gap = Test_helpers.order_gap_of_identities ~tailwind ~tw in
  Fmt.pr
    "@@layer utilities, against the frozen order: %d of %d pairs must move@."
    gap.Test_helpers.moves gap.Test_helpers.pairs;
  if gap.Test_helpers.moves > 0 then begin
    Fmt.pr
      "  FAIL: tw's order left the sequence recorded in \
       test/parity/tailwind_order.txt.@.";
    Fmt.pr
      "  Either the sort regressed, or the pinned tailwindcss moved and the \
       fixture wants regenerating - see gen_order_fixture.ml.@.";
    exit 1
  end
