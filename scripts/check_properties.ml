#!/usr/bin/env ocaml

(* Script to validate that all properties in properties_intf.ml are handled in
   properties.ml's read_property function *)
#use "topfind"

#require "str"

let read_file filename =
  let ic = open_in filename in
  let rec loop acc =
    try
      let line = input_line ic in
      loop (line :: acc)
    with End_of_file ->
      close_in ic;
      List.rev acc
  in
  String.concat "\n" (loop [])

(* Extract property constructors from properties_intf.ml *)
let extract_property_constructors content =
  let lines = String.split_on_char '\n' content in
  let constructor_pattern = Str.regexp "^[ ]*| \\([A-Z][A-Za-z0-9_]*\\) :" in
  List.fold_left
    (fun acc line ->
      if Str.string_match constructor_pattern line 0 then
        let name = Str.matched_group 1 line in
        name :: acc
      else acc)
    [] lines

(* Extract handled properties from properties.ml read_property function *)
let extract_handled_properties content =
  (* Find the read_property function with PROPERTY_MATCHING_START marker *)
  let start_pattern = Str.regexp ".*PROPERTY_MATCHING_START.*" in
  let end_pattern = Str.regexp ".*PROPERTY_MATCHING_END.*" in

  (* Split content into lines *)
  let lines = String.split_on_char '\n' content in

  (* Find the region between markers *)
  let rec find_region lines in_region acc =
    match lines with
    | [] -> acc
    | line :: rest ->
        if Str.string_match end_pattern line 0 then find_region rest false acc
        else if in_region then find_region rest true (line :: acc)
        else if Str.string_match start_pattern line 0 then
          find_region rest true acc
        else find_region rest false acc
  in

  let region_lines = List.rev (find_region lines false []) in
  let region = String.concat "\n" region_lines in

  (* Extract property names from pattern matches *)
  let pattern = Str.regexp "| \"[^\"]+\" -> Prop \\([A-Z][A-Za-z0-9_]*\\)" in
  let rec extract_from_string str pos acc =
    try
      let _ = Str.search_forward pattern str pos in
      let prop_name = Str.matched_group 1 str in
      let new_pos = Str.match_end () in
      extract_from_string str new_pos (prop_name :: acc)
    with Not_found -> acc
  in
  extract_from_string region 0 []

let () =
  (* Try to find the project root *)
  let rec find_project_root dir =
    if Sys.file_exists (Filename.concat dir "dune-project") then dir
    else
      let parent = Filename.dirname dir in
      if parent = dir then "." else find_project_root parent
  in

  let project_root = find_project_root (Sys.getcwd ()) in
  let intf_file = Filename.concat project_root "lib/css/properties_intf.ml" in
  let impl_file = Filename.concat project_root "lib/css/properties.ml" in

  if not (Sys.file_exists intf_file) then (
    Printf.eprintf "Cannot find %s\n" intf_file;
    exit 1);

  if not (Sys.file_exists impl_file) then (
    Printf.eprintf "Cannot find %s\n" impl_file;
    exit 1);

  let intf_content = read_file intf_file in
  let impl_content = read_file impl_file in

  let all_properties = extract_property_constructors intf_content in
  let handled_properties = extract_handled_properties impl_content in

  (* Convert to sets for comparison *)
  let module StringSet = Set.Make (String) in
  let all_set = StringSet.of_list all_properties in
  let handled_set = StringSet.of_list handled_properties in

  let missing = StringSet.diff all_set handled_set in
  let extra = StringSet.diff handled_set all_set in

  if not (StringSet.is_empty missing) then (
    Printf.printf "ERROR: Missing property mappings in read_property:\n";
    StringSet.iter (Printf.printf "  - %s\n") missing;
    exit 1);

  if not (StringSet.is_empty extra) then (
    Printf.printf
      "WARNING: Extra property mappings in read_property (not in interface):\n";
    StringSet.iter (Printf.printf "  - %s\n") extra);

  Printf.printf "✓ All %d properties are properly mapped in read_property\n"
    (StringSet.cardinal all_set)
