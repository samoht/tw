(* CSS API Consistency Checker

   Ensures API consistency across interface definitions, implementations, and
   tests.

   For each interface module (lib/css/*_intf.ml): - Extracts all top-level type
   definitions - Verifies corresponding read_*/pp_* functions in lib/css/*.mli -
   Verifies corresponding check_* functions in test/css/test_*.ml

   Reports missing items with actionable suggestions. *)

open Stdlib

(* Types to ignore in consistency checks *)
let ignored_types =
  [
    "meta";
    (* Abstract type without read/pp functions *)
    "mode";
    (* Internal implementation detail *)
    "kind";
    (* Internal implementation detail *)
  ]

module Fs = struct
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

let contains_sub (s : string) (sub : string) : bool =
  let rex = Re.compile (Re.str sub) in
  Re.execp rex s

(* Color constants for terminal output *)
let red = "\027[31m"
let yellow = "\027[33m"
let cyan = "\027[36m"
let bold = "\027[1m"
let reset = "\027[0m"

let colored color text =
  if Unix.isatty Unix.stdout then color ^ text ^ reset else text

(* Type extraction patterns and functions *)
let type_re = Re.Perl.compile_pat "^[\\s]*type[\\s]+([A-Za-z_][A-Za-z0-9_]*)\\b"

let type_blank_re =
  (* Matches: type _ <name> ... -> captures <name> *)
  Re.Perl.compile_pat "^[\\s]*type[\\s]+_+[\\s]+([A-Za-z_][A-Za-z0-9_]*)\\b"

let extract_types path : string list =
  let lines = Fs.read_lines path in
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

(* Extract test functions from a test file *)
let extract_test_functions test_file =
  if not (Sys.file_exists test_file) then []
  else
    let lines = Fs.read_lines test_file in
    let tests = ref [] in
    let current_name = ref None in
    let current_header = ref "" in
    let current_line = ref 0 in
    let current_ignored = ref false in
    let buf = Buffer.create 4096 in

    let flush_current () =
      match !current_name with
      | None -> ()
      | Some name ->
          let body = Buffer.contents buf in
          tests :=
            (name, body, (!current_header, !current_line, !current_ignored))
            :: !tests;
          Buffer.clear buf;
          current_name := None;
          current_header := "";
          current_line := 0;
          current_ignored := false
    in

    let re_header =
      Re.Perl.compile_pat
        "^[\\s]*let[\\s]+test_([A-Za-z0-9_]+)[\\s]*\\(\\)[\\s]*="
    in
    let re_toplevel_let =
      Re.Perl.compile_pat "^let[\\s]+[A-Za-z_][A-Za-z0-9_]*[\\s]*="
    in

    List.iteri
      (fun idx l ->
        match Re.exec_opt re_header l with
        | Some g -> (
            flush_current ();
            let name = Re.Group.get g 1 in
            current_name := Some name;
            current_header := l;
            current_line := idx + 1;
            let rec find_prev_nonempty i =
              if i < 0 then None
              else
                let pl = List.nth lines i in
                if String.trim pl = "" then find_prev_nonempty (i - 1)
                else Some pl
            in
            match find_prev_nonempty (idx - 1) with
            | Some pl ->
                current_ignored :=
                  contains_sub pl "Not a roundtrip test"
                  || contains_sub pl "ignore-test"
            | None -> current_ignored := false)
        | None ->
            (match !current_name with
            | Some _ -> Buffer.add_string buf (l ^ "\n")
            | None -> ());
            if
              Re.execp re_toplevel_let l
              && not (String.trim l |> String.starts_with ~prefix:"let open")
            then flush_current ())
      lines;
    flush_current ();
    !tests

(* Analyze check and neg patterns in test function body *)
let analyze_test_patterns tname body =
  let check_re = Re.Perl.compile_pat "\\bcheck_([A-Za-z0-9_]+)" in
  let neg_read_re = Re.Perl.compile_pat "neg[\\s]+read_([A-Za-z0-9_]+)" in

  let rec collect_checks pos acc =
    if pos >= String.length body then List.rev acc
    else
      match Re.exec_opt ~pos check_re body with
      | None -> List.rev acc
      | Some g ->
          let name = Re.Group.get g 1 in
          collect_checks (Re.Group.stop g 0) (name :: acc)
  in

  let rec collect_neg_reads pos acc =
    if pos >= String.length body then List.rev acc
    else
      match Re.exec_opt ~pos neg_read_re body with
      | None -> List.rev acc
      | Some g ->
          let name = Re.Group.get g 1 in
          collect_neg_reads (Re.Group.stop g 0) (name :: acc)
  in

  let checks = collect_checks 0 [] in
  let neg_reads = collect_neg_reads 0 [] in
  let neg_re = Re.Perl.compile_pat (Fmt.str "neg[\\s]+read_%s\\b" tname) in
  let has_neg = Re.execp neg_re body in

  (checks, neg_reads, has_neg)

(* Check consistency for a single CSS module *)
let check_module_consistency lib_css test_css mod_name =
  let intf_file = lib_css // (mod_name ^ "_intf.ml") in
  let test_file = test_css // ("test_" ^ mod_name ^ ".ml") in

  if not (Sys.file_exists intf_file) then None
  else
    let valid_types = extract_types intf_file |> List.sort_uniq compare in
    let tests = extract_test_functions test_file in
    let test_names = List.map (fun (n, _, _) -> n) tests in

    let invalid_tests =
      tests
      |> List.filter (fun (n, _body, (_hdr, _ln, ign)) ->
             (not ign) && not (List.mem n valid_types))
      |> List.map (fun (n, _, _) -> n)
      |> List.sort_uniq compare
    in

    let missing_tests =
      List.filter
        (fun t ->
          (not (List.mem t ignored_types)) && not (List.mem t test_names))
        valid_types
    in

    let wrong_checks = ref [] in
    let missing_neg = ref [] in

    List.iter
      (fun (tname, body, (_hdr, _ln, ignored)) ->
        if not ignored then (
          let checks, neg_reads, has_neg = analyze_test_patterns tname body in

          (* Check for wrong check_ calls *)
          List.iter
            (fun c ->
              if c <> tname && c <> "value" && c <> "parse_fails" then
                if (not (List.mem tname valid_types)) && List.mem c valid_types
                then wrong_checks := (tname, c) :: !wrong_checks
                else if List.mem tname valid_types then
                  wrong_checks := (tname, c) :: !wrong_checks)
            checks;

          (* Check for wrong neg read_ calls *)
          List.iter
            (fun n ->
              if n <> tname then
                if (not (List.mem tname valid_types)) && List.mem n valid_types
                then wrong_checks := (tname, "neg read_" ^ n) :: !wrong_checks
                else if List.mem tname valid_types && List.mem n valid_types
                then wrong_checks := (tname, "neg read_" ^ n) :: !wrong_checks)
            neg_reads;

          (* Check for missing neg patterns *)
          if not has_neg then missing_neg := tname :: !missing_neg))
      tests;

    Some (mod_name, invalid_tests, missing_tests, !wrong_checks, !missing_neg)

(* Print consistency results for a module *)
let print_module_results
    (mod_name, invalid_tests, missing_tests, wrong_checks, missing_neg) =
  let has_issues =
    invalid_tests <> [] || missing_tests <> [] || wrong_checks <> []
    || missing_neg <> []
  in
  if has_issues then (
    Fmt.pr "@.%s@."
      (colored bold (String.capitalize_ascii mod_name ^ " Tests Consistency:"));

    if invalid_tests <> [] then (
      Fmt.pr "%s invalid test_ names in test_%s.ml:@."
        (colored yellow "Warning -")
        mod_name;
      List.iter
        (fun n -> Fmt.pr "  test_%s (not in %s_intf)@." n mod_name)
        invalid_tests;
      Fmt.pr "@.");

    if wrong_checks <> [] then (
      Fmt.pr "%s wrong check_x inside test_y in test_%s.ml:@."
        (colored yellow "Warning -")
        mod_name;
      let wrong_unique = List.sort_uniq compare wrong_checks in
      List.iter
        (fun (y, x) ->
          if String.starts_with ~prefix:"neg read_" x then
            Fmt.pr "  test_%s: %s@." y x
          else Fmt.pr "  test_%s: check_%s@." y x)
        wrong_unique;
      Fmt.pr "@.");

    if missing_tests <> [] then (
      Fmt.pr "%s missing test_x functions in test_%s.ml:@."
        (colored yellow "Warning -")
        mod_name;
      List.iter
        (fun n -> Fmt.pr "  test_%s@." n)
        (List.sort compare missing_tests);
      Fmt.pr "@.");

    if missing_neg <> [] then (
      Fmt.pr "%s missing neg read_x inside test_x in test_%s.ml:@."
        (colored yellow "Warning -")
        mod_name;
      List.iter
        (fun n -> Fmt.pr "  test_%s@." n)
        (List.sort compare missing_neg);
      Fmt.pr "@."))

(* Project root detected by looking for dune-project file. *)
let project_root () =
  let rec search dir =
    if Sys.file_exists (dir // "dune-project") then dir
    else
      let parent = Filename.dirname dir in
      if parent = dir then
        failwith "Could not find project root (no dune-project file found)"
      else search parent
  in
  search (Sys.getcwd ())

let root = project_root ()
let lib_css = root // "lib" // "css"
let test_css = root // "test" // "css"

let file_has_val mli_path name : bool =
  let rex = Re.Perl.compile_pat ("^[\\s]*val[\\s]+" ^ name ^ "\\b") in
  List.exists (fun l -> Re.execp rex l) (Fs.read_lines mli_path)

let file_has_let test_path name : bool =
  (* Check for multiple patterns: 1. Direct let binding: let check_foo = ... 2.
     Inline check_value call: check_value "foo" pp_foo read_foo 3. Local helper
     within test function: let check_foo ... inside test_foo function *)
  let lines = Fs.read_lines test_path in

  (* First check for direct let binding at any indentation level *)
  let rex = Re.Perl.compile_pat ("[\\s]*let[\\s]+" ^ name ^ "\\b") in
  if List.exists (fun l -> Re.execp rex l) lines then true
  else if
    (* If checking for check_<typename>, also look for inline check_value
       usage *)
    String.starts_with ~prefix:"check_" name
  then
    let typename = String.sub name 6 (String.length name - 6) in
    (* Look for pattern: check_value "typename" pp_typename read_typename *)
    let inline_pattern =
      Re.Perl.compile_pat
        (Fmt.str "check_value[\\s]+\"%s\"[\\s]+pp_%s[\\s]+read_%s" typename
           typename typename)
    in
    (* Also check if there's a test function that tests this type *)
    let test_func_pattern =
      Re.Perl.compile_pat (Fmt.str "let[\\s]+test_%s[\\s]*\\(\\)" typename)
    in
    List.exists
      (fun l -> Re.execp inline_pattern l || Re.execp test_func_pattern l)
      lines
  else false

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
let () =
  let intf_files =
    Fs.list_dir lib_css
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
          List.filter (fun t -> not (List.mem t ignored_types)) types
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
            if (not check_ok) && not (List.mem tname ignored_types) then
              stats.missing_check <- stats.missing_check + 1;

            if
              not
                (read_ok && pp_ok && (check_ok || List.mem tname ignored_types))
            then (
              let missing_items = ref [] in
              if not read_ok then missing_items := read_name :: !missing_items;
              if not pp_ok then missing_items := pp_name :: !missing_items;
              if (not check_ok) && not (List.mem tname ignored_types) then
                missing_items := check_name :: !missing_items;

              module_issues := (tname, !missing_items) :: !module_issues;
              all_missing := (mod_base, tname, !missing_items) :: !all_missing))
          filtered_types;

        if !module_issues <> [] then
          modules_with_issues := mod_base :: !modules_with_issues))
    intf_files;

  (* Do not exit early here — we also want to run properties test checks
     below *)

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
              if String.starts_with ~prefix:"check" item then
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

  (* CSS modules test conformance checks *)
  let css_modules =
    [
      "properties";
      "values";
      "declaration";
      "selector";
      "stylesheet";
      "variables";
    ]
  in
  List.iter
    (fun mod_name ->
      match check_module_consistency lib_css test_css mod_name with
      | Some results -> print_module_results results
      | None -> ())
    css_modules;

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

  let exit_code = if api_missing > 0 then 1 else 0 in

  if exit_code = 1 then
    Fmt.pr "@.%s Missing API functions must be added before proceeding.@."
      (colored red "Action Required:")
  else if test_missing > 0 then
    Fmt.pr "@.%s Consider adding test functions for complete coverage.@."
      (colored yellow "Recommendation:");

  exit exit_code
