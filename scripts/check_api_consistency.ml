(* CSS API Consistency Checker

   Ensures API consistency across interface definitions, implementations, and
   tests.

   For each interface module (lib/css/*_intf.ml): - Extracts all top-level type
   definitions - Verifies corresponding read_*/pp_* functions in lib/css/*.mli -
   Verifies corresponding check_* functions in test/css/test_*.ml

   Reports missing items with actionable suggestions. *)

open Stdlib

module FS = struct
  let read_file path =
    let ic = open_in path in
    Fun.protect
      ~finally:(fun () -> close_in_noerr ic)
      (fun () -> really_input_string ic (in_channel_length ic))

  let read_lines path =
    try
      let s = read_file path in
      String.split_on_char '\n' s
    with Sys_error _ -> []

  let list_dir path =
    try Array.to_list (Sys.readdir path) with Sys_error _ -> []
end

let ( // ) a b = if a = "" then b else a ^ Filename.dir_sep ^ b

(* Find the project root by looking for dune-project file *)
let find_project_root () =
  let rec search dir =
    if Sys.file_exists (dir // "dune-project") then dir
    else
      let parent = Filename.dirname dir in
      if parent = dir then
        failwith "Could not find project root (no dune-project file found)"
      else search parent
  in
  search (Sys.getcwd ())

let root = find_project_root ()
let lib_css = root // "lib" // "css"
let test_css = root // "test" // "css"
let type_re = Re.Perl.compile_pat "^[\\s]*type[\\s]+([A-Za-z_][A-Za-z0-9_]*)\\b"

let type_blank_re =
  (* Matches: type _ <name> ... -> captures <name> *)
  Re.Perl.compile_pat "^[\\s]*type[\\s]+_+[\\s]+([A-Za-z_][A-Za-z0-9_]*)\\b"

let extract_types path : string list =
  let lines = FS.read_lines path in
  let rec loop acc = function
    | [] -> List.rev acc
    | l :: tl -> (
        (* Prefer capturing the name in "type _ name" if present *)
        match Re.exec_opt type_blank_re l with
        | Some g ->
            let tname = Re.Group.get g 1 in
            let acc =
              if tname <> "_" && not (List.mem tname acc) then tname :: acc
              else acc
            in
            loop acc tl
        | None -> (
            match Re.exec_opt type_re l with
            | Some g ->
                let tname = Re.Group.get g 1 in
                let acc =
                  if tname <> "_" && not (List.mem tname acc) then tname :: acc
                  else acc
                in
                loop acc tl
            | None -> loop acc tl))
  in
  loop [] lines

let file_has_val mli_path name : bool =
  let rex = Re.Perl.compile_pat ("^[\\s]*val[\\s]+" ^ name ^ "\\b") in
  List.exists (fun l -> Re.execp rex l) (FS.read_lines mli_path)

let file_has_let test_path name : bool =
  let rex = Re.Perl.compile_pat ("^[\\s]*let[\\s]+" ^ name ^ "\\b") in
  List.exists (fun l -> Re.execp rex l) (FS.read_lines test_path)

(* Statistics tracking *)
type stats = {
  mutable total_types : int;
  mutable missing_read : int;
  mutable missing_pp : int;
  mutable missing_check : int;
  mutable missing_modules : string list;
}

let stats =
  {
    total_types = 0;
    missing_read = 0;
    missing_pp = 0;
    missing_check = 0;
    missing_modules = [];
  }

(* ANSI color codes for better output *)
let red = "\027[31m"
let yellow = "\027[33m"
let cyan = "\027[36m"
let bold = "\027[1m"
let reset = "\027[0m"

let colored color text =
  if Unix.isatty Unix.stdout then color ^ text ^ reset else text

let () =
  let intf_files =
    FS.list_dir lib_css
    |> List.filter (fun f -> Filename.check_suffix f "_intf.ml")
    |> List.sort compare
  in
  if intf_files = [] then (
    Fmt.pr "%s No interface files found under %s/*_intf.ml@."
      (colored yellow "Warning:")
      lib_css;
    exit 0);

  let modules_with_issues = ref [] in
  let all_missing = ref [] in

  (* Collect all issues first without printing *)
  List.iter
    (fun intf_file ->
      let mod_base =
        match Filename.chop_suffix_opt ~suffix:"_intf.ml" intf_file with
        | Some s -> s
        | None -> String.sub intf_file 0 (max 0 (String.length intf_file - 8))
      in
      let intf_path = lib_css // intf_file in
      let mli_path = lib_css // (mod_base ^ ".mli") in
      let test_path = test_css // ("test_" ^ mod_base ^ ".ml") in

      let types = extract_types intf_path in
      let mli_exists = Sys.file_exists mli_path in
      let test_exists = Sys.file_exists test_path in

      if not mli_exists then
        stats.missing_modules <- mod_base :: stats.missing_modules;

      if types <> [] then (
        let module_issues = ref [] in
        (* Filter out types we should ignore *)
        let filtered_types =
          List.filter (fun t -> t <> "mode" && t <> "kind" && t <> "meta") types
        in
        List.iter
          (fun tname ->
            stats.total_types <- stats.total_types + 1;
            (* Special case: type t uses 'read' and 'pp' instead of 'read_t' and
               'pp_t' *)
            let read_name = if tname = "t" then "read" else "read_" ^ tname in
            let pp_name = if tname = "t" then "pp" else "pp_" ^ tname in
            let check_name =
              if tname = "t" then "check" else "check_" ^ tname
            in
            let read_ok = mli_exists && file_has_val mli_path read_name in
            let pp_ok = mli_exists && file_has_val mli_path pp_name in
            let check_ok = test_exists && file_has_let test_path check_name in

            if not read_ok then stats.missing_read <- stats.missing_read + 1;
            if not pp_ok then stats.missing_pp <- stats.missing_pp + 1;
            if not check_ok then stats.missing_check <- stats.missing_check + 1;

            if not (read_ok && pp_ok && check_ok) then (
              let missing_items = ref [] in
              if not read_ok then missing_items := read_name :: !missing_items;
              if not pp_ok then missing_items := pp_name :: !missing_items;
              if not check_ok then missing_items := check_name :: !missing_items;

              module_issues := (tname, !missing_items) :: !module_issues;
              all_missing := (mod_base, tname, !missing_items) :: !all_missing))
          filtered_types;

        if !module_issues <> [] then
          modules_with_issues := mod_base :: !modules_with_issues))
    intf_files;

  (* Exit early if everything is good *)
  if !all_missing = [] && stats.missing_modules = [] then exit 0;

  (* Now print the issues *)
  Fmt.pr "%s@." (colored bold "CSS API Consistency Issues");
  Fmt.pr "%s@.@." (String.make 50 '=');

  if stats.missing_modules <> [] then (
    Fmt.pr "%s Missing module interfaces:@." (colored red "Critical:");
    List.iter
      (fun m -> Fmt.pr "  • %s.mli@." m)
      (List.rev stats.missing_modules);
    Fmt.pr "@.");

  (* Group missing items by file for cleaner output *)
  if !all_missing <> [] then (
    let by_file = Hashtbl.create 10 in

    List.iter
      (fun (m, t, items) ->
        List.iter
          (fun item ->
            let file =
              if String.starts_with ~prefix:"check_" item then
                Fmt.str "test/css/test_%s.ml" m
              else Fmt.str "lib/css/%s.mli" m
            in
            let current =
              try Hashtbl.find by_file file with Not_found -> []
            in
            Hashtbl.replace by_file file ((m, t, item) :: current))
          items)
      !all_missing;

    Fmt.pr "%s@.@." (colored bold "Missing API Functions:");

    (* Separate API functions from test functions *)
    let api_files = ref [] in
    let test_files = ref [] in

    Hashtbl.iter
      (fun file items ->
        if String.contains file '.' && String.ends_with ~suffix:".mli" file then
          api_files := (file, items) :: !api_files
        else test_files := (file, items) :: !test_files)
      by_file;

    (* Print API functions first (more critical) *)
    if !api_files <> [] then (
      Fmt.pr "%s@." (colored red "Critical - Missing API Functions:");
      List.iter
        (fun (file, items) ->
          Fmt.pr "@.%s@." (colored cyan file);

          (* Group by type and show example implementations *)
          let by_type = Hashtbl.create 10 in
          List.iter
            (fun (m, t, item) ->
              let current = try Hashtbl.find by_type t with Not_found -> [] in
              Hashtbl.replace by_type t ((m, item) :: current))
            items;

          Hashtbl.iter
            (fun t funcs ->
              Fmt.pr "  type %s:@." (colored yellow t);
              List.iter
                (fun (_m, f) ->
                  if f = "read" then Fmt.pr "    val read : Reader.t -> t@."
                  else if f = "pp" then Fmt.pr "    val pp : t Pp.t@."
                  else if String.starts_with ~prefix:"read" f then
                    Fmt.pr "    val %s : Reader.t -> %s@." f t
                  else if String.starts_with ~prefix:"pp" f then
                    Fmt.pr "    val %s : %s Pp.t@." f t
                  else Fmt.pr "    val %s@." f)
                (List.sort compare funcs))
            by_type)
        (List.sort compare !api_files);
      Fmt.pr "@.");

    (* Print test functions separately (less critical) *)
    if !test_files <> [] then (
      Fmt.pr "%s@." (colored yellow "Warning - Missing Test Functions:");
      List.iter
        (fun (file, items) ->
          Fmt.pr "@.%s@." (colored cyan file);

          (* Group by type *)
          let by_type = Hashtbl.create 10 in
          List.iter
            (fun (m, t, item) ->
              let current = try Hashtbl.find by_type t with Not_found -> [] in
              Hashtbl.replace by_type t ((m, item) :: current))
            items;

          Hashtbl.iter
            (fun t funcs ->
              Fmt.pr "  type %s:@." t;
              List.iter
                (fun (_m, f) ->
                  if f = "check" then
                    Fmt.pr "    let check = check_value \"t\" pp read@."
                  else
                    Fmt.pr "    let %s = check_value \"%s\" pp_%s read_%s@." f t
                      t t)
                (List.sort compare funcs))
            by_type)
        (List.sort compare !test_files);
      Fmt.pr "@."));

  (* Summary with counts *)
  Fmt.pr "%s@." (String.make 50 '-');

  let api_missing = stats.missing_read + stats.missing_pp in
  let test_missing = stats.missing_check in

  if api_missing > 0 then
    Fmt.pr "%s %d missing API functions (read_*/pp_*)@." (colored red "ERROR:")
      api_missing;

  if test_missing > 0 then
    Fmt.pr "%s %d missing test functions (check_*)@."
      (colored yellow "WARNING:")
      test_missing;

  (* Exit status - only fail for missing API functions, not tests *)
  (* Missing tests are warnings, not errors *)
  let exit_code =
    if stats.missing_modules <> [] || api_missing > 0 then 1 else 0
  in

  if exit_code = 1 then
    Fmt.pr "@.%s Missing API functions must be added before proceeding.@."
      (colored red "Action Required:")
  else if test_missing > 0 then
    Fmt.pr "@.%s Consider adding test functions for complete coverage.@."
      (colored yellow "Recommendation:");

  exit exit_code
