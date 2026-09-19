(* The page the site corpus renders under: every class of classlist.txt on an
   element of its own.

   A variant that reads markup around the element needs that markup, or both
   sheets leave the element alone and the render says nothing about the variant.
   Each element therefore sits in a wrapper carrying every [group] name the list
   uses, after a sibling carrying every [peer] name, and a class whose variants
   read descendants ([has-*], [*:], [**:]) gets the children those name. A state
   the browser comparison applies to every element reaches the wrapper and the
   sibling too, so [group-hover:] and [peer-focus:] render in their state. What
   the page cannot supply is an attribute or a class the variant tests on the
   ancestor ([group-data-[checked]:], [in-[.dark]:]): both sheets leave those
   elements alone, so the render covers them unmatched only.

   The page is [Test_helpers.classes_page], shared with the parity corpus.
   Unlike [Test_helpers.check_rendering_matches], no element carries a pair of
   classes. Pairing costs a canonicalisation per pair, twelve million over the
   site's list, and the order a pair would expose is what the whole-sheet order
   gate measures already.

   Usage: site_page CLASSLIST [FIRST COUNT] > page.html renders the whole list,
   or the COUNT classes from index FIRST, so a render can be split into
   shards. *)

let read_lines path =
  In_channel.with_open_text path In_channel.input_all
  |> String.split_on_char '\n' |> List.map String.trim
  |> List.filter (fun line -> line <> "")

let slice first count l =
  List.filteri (fun i _ -> i >= first && i < first + count) l

let () =
  let classes, shard =
    match Sys.argv with
    | [| _; path |] ->
        let classes = read_lines path in
        (classes, classes)
    | [| _; path; first; count |] ->
        let classes = read_lines path in
        (classes, slice (int_of_string first) (int_of_string count) classes)
    | _ ->
        prerr_endline "usage: site_page CLASSLIST [FIRST COUNT]";
        exit 2
  in
  (* Scope names come from the whole list, so every shard's wrappers carry the
     same classes. *)
  print_string (Test_helpers.classes_page ~scope:classes shard)
