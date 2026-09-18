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

(* The [/name] a [group-*] or [peer-*] variant scopes to, from any segment of a
   class that starts with [prefix]. *)
let scope_names prefix classes =
  let names = Hashtbl.create 16 in
  List.iter
    (fun cls ->
      List.iter
        (fun segment ->
          if String.starts_with ~prefix segment then
            match String.rindex_opt segment '/' with
            | Some i when i + 1 < String.length segment ->
                let name =
                  String.sub segment (i + 1) (String.length segment - i - 1)
                in
                if not (String.contains name ']') then
                  Hashtbl.replace names name ()
            | _ -> ())
        (Tw_tools.Entrypoint.variant_segments cls))
    classes;
  Hashtbl.fold (fun name () acc -> name :: acc) names [] |> List.sort compare

let scoped base names =
  String.concat " " (base :: List.map (fun n -> base ^ "/" ^ n) names)

let reads_descendants cls =
  List.exists
    (fun segment ->
      String.starts_with ~prefix:"has-" segment
      || String.starts_with ~prefix:"group-has-" segment
      || String.starts_with ~prefix:"peer-has-" segment
      || String.equal segment "*" || String.equal segment "**")
    (Tw_tools.Entrypoint.variant_segments cls)

let children =
  "<p>x <code>x</code> <strong>x</strong> <a href=\"#\">x</a></p><svg \
   width=\"1\" height=\"1\"></svg><ul><li>x</li></ul><pre>x</pre><img \
   alt=\"\">"

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
  let group = scoped "group" (scope_names "group-" classes) in
  let peer = scoped "peer" (scope_names "peer-" classes) in
  let buf = Buffer.create (1 lsl 20) in
  Buffer.add_string buf
    "<!doctype html><html><head><meta charset=\"utf-8\"></head><body>";
  List.iter
    (fun cls ->
      Buffer.add_string buf
        (String.concat ""
           [
             "<div class=\"";
             Test_helpers.escape_attribute group;
             "\"><input type=\"checkbox\" class=\"";
             Test_helpers.escape_attribute peer;
             "\"><div class=\"";
             Test_helpers.escape_attribute cls;
             "\">";
             (if reads_descendants cls then children else "x");
             "</div></div>";
           ]))
    shard;
  Buffer.add_string buf "</body></html>\n";
  print_string (Buffer.contents buf)
