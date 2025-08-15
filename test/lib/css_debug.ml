(** CSS debugging utilities for testing *)

(** Write CSS to a temporary file for inspection *)
let write_temp_css prefix css =
  let file = Filename.temp_file prefix ".css" in
  let oc = open_out file in
  output_string oc css;
  close_out oc;
  file

(** Format minified CSS for readability *)
let format_css css =
  let buf = Buffer.create (String.length css * 2) in
  let len = String.length css in
  let rec loop i indent =
    if i >= len then ()
    else
      match css.[i] with
      | '{' ->
          Buffer.add_char buf '{';
          Buffer.add_char buf '\n';
          let new_indent = indent + 2 in
          Buffer.add_string buf (String.make new_indent ' ');
          loop (i + 1) new_indent
      | '}' ->
          let new_indent = max 0 (indent - 2) in
          Buffer.add_char buf '\n';
          Buffer.add_string buf (String.make new_indent ' ');
          Buffer.add_char buf '}';
          if i + 1 < len && css.[i + 1] <> '}' then Buffer.add_char buf '\n';
          if i + 1 < len && css.[i + 1] <> '}' then
            Buffer.add_string buf (String.make new_indent ' ');
          loop (i + 1) new_indent
      | ';' ->
          Buffer.add_char buf ';';
          Buffer.add_char buf '\n';
          if i + 1 < len && css.[i + 1] <> '}' then
            Buffer.add_string buf (String.make indent ' ');
          loop (i + 1) indent
      | ',' when i + 1 < len && (css.[i + 1] = ':' || css.[i + 1] = '*') ->
          (* Selector separator *)
          Buffer.add_char buf ',';
          Buffer.add_char buf '\n';
          Buffer.add_string buf (String.make indent ' ');
          loop (i + 1) indent
      | c ->
          Buffer.add_char buf c;
          loop (i + 1) indent
  in
  loop 0 0;
  Buffer.contents buf

(** Extract a specific CSS rule by selector *)
let extract_rule css selector =
  (* Escape special regex characters in selector *)
  let escaped_selector = Re.str selector in
  let pattern =
    Re.seq
      [
        escaped_selector;
        Re.rep Re.space;
        Re.char '{';
        Re.group (Re.rep (Re.compl [ Re.char '}' ]));
        Re.char '}';
      ]
  in
  let regex = Re.compile pattern in
  match Re.exec_opt regex css with
  | Some groups -> (
      try Some (String.trim (Re.Group.get groups 1)) with Not_found -> None)
  | None -> None

(** Compare two CSS strings and show detailed differences *)
let detailed_diff css1 css2 =
  let file1 = write_temp_css "tw_our" css1 in
  let file2 = write_temp_css "tw_tailwind" css2 in

  (* First try structural comparison *)
  let structural_diff = Css_compare.format_diff css1 css2 in

  (* Also prepare raw diff command for user *)
  let diff_cmd = Printf.sprintf "diff -u %s %s" file1 file2 in

  (* Cleanup function to remove temporary files *)
  let cleanup () =
    try
      Sys.remove file1;
      Sys.remove file2
    with _ -> ()
  in

  (* Register cleanup with at_exit to ensure files are removed *)
  at_exit cleanup;

  (structural_diff, diff_cmd, cleanup)

(** Find the first difference between two CSS strings *)
let find_first_diff css1 css2 =
  let len1 = String.length css1 in
  let len2 = String.length css2 in
  let min_len = min len1 len2 in

  let rec loop i =
    if i >= min_len then
      if len1 = len2 then None
      else
        Some
          ( i,
            "Length mismatch",
            Printf.sprintf "CSS1 length: %d, CSS2 length: %d" len1 len2 )
    else if css1.[i] <> css2.[i] then
      let context_size = 50 in
      let start = max 0 (i - context_size) in
      let end1 = min len1 (i + context_size) in
      let end2 = min len2 (i + context_size) in
      let context1 = String.sub css1 start (end1 - start) in
      let context2 = String.sub css2 start (end2 - start) in
      Some
        ( i,
          Printf.sprintf "Character mismatch at position %d" i,
          Printf.sprintf "CSS1: ...%s...\nCSS2: ...%s..." context1 context2 )
    else loop (i + 1)
  in
  loop 0

(** Debug helper to save CSS for manual inspection *)
let save_for_inspection ~our_css ~tailwind_css ~test_name =
  let timestamp = Unix.time () |> int_of_float |> string_of_int in
  let dir = Printf.sprintf "/tmp/tw_test_%s_%s" test_name timestamp in
  let _ = Sys.command (Printf.sprintf "mkdir -p %s" dir) in

  let our_file = Printf.sprintf "%s/our.css" dir in
  let tailwind_file = Printf.sprintf "%s/tailwind.css" dir in
  let our_formatted = Printf.sprintf "%s/our_formatted.css" dir in
  let tailwind_formatted = Printf.sprintf "%s/tailwind_formatted.css" dir in

  (* Write raw CSS *)
  let oc = open_out our_file in
  output_string oc our_css;
  close_out oc;

  let oc = open_out tailwind_file in
  output_string oc tailwind_css;
  close_out oc;

  (* Write formatted CSS *)
  let oc = open_out our_formatted in
  output_string oc (format_css our_css);
  close_out oc;

  let oc = open_out tailwind_formatted in
  output_string oc (format_css tailwind_css);
  close_out oc;

  Printf.sprintf "CSS saved for inspection in %s" dir
