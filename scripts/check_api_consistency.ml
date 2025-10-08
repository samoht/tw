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
    "any_var";
    (* Internal type for existential variables *)
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

(* Extract types that contain Var constructors from interface files *)
let extract_var_types path : string list =
  let lines = Fs.read_lines path in
  let var_type_re =
    Re.Perl.compile_pat "\\|\\s+Var\\s+of\\s+([A-Za-z_][A-Za-z0-9_]*)\\s+var"
  in
  let type_def_re =
    Re.Perl.compile_pat "^[\\s]*type[\\s]+([A-Za-z_][A-Za-z0-9_]*)\\s*="
  in

  let rec loop acc current_type = function
    | [] -> List.rev acc
    | l :: tl -> (
        (* Check if this line starts a new type definition *)
        match Re.exec_opt type_def_re l with
        | Some g ->
            let type_name = Re.Group.get g 1 in
            loop acc (Some type_name) tl
        | None -> (
            (* Check if this line has a Var constructor *)
            match Re.exec_opt var_type_re l with
            | Some _g -> (
                match current_type with
                | Some tname when not (List.mem tname acc) ->
                    loop (tname :: acc) current_type tl
                | _ -> loop acc current_type tl)
            | None -> loop acc current_type tl))
  in
  loop [] None lines

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
      Re.Perl.compile_pat "^let[\\s]+[A-Za-z_][A-Za-z0-9_]*"
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
let analyze_test_patterns tname body module_name =
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
  (* For type t, the test function is test_<module> but the read function can be both "read" and "read_<module>" *)
  (* So if tname equals the module name, we're testing type t and should look for both patterns *)
  let has_neg =
    if tname = module_name then
      (* Testing type t - look for both "neg read" (without suffix) and "neg
         read_<module>" *)
      let has_read = Re.execp (Re.Perl.compile_pat "neg[\\s]+read[\\s]") body in
      let has_read_module =
        Re.execp
          (Re.Perl.compile_pat (Fmt.str "neg[\\s]+read_%s\\b" module_name))
          body
      in
      has_read || has_read_module
    else
      (* Testing other types - look for "neg read_<type>" *)
      let neg_re = Re.Perl.compile_pat (Fmt.str "neg[\\s]+read_%s\\b" tname) in
      Re.execp neg_re body
  in

  (checks, neg_reads, has_neg)

(* Check if a .mli file has a val declaration with the given name *)
let file_has_val mli_path name : bool =
  let rex = Re.Perl.compile_pat ("^[\\s]*val[\\s]+" ^ name ^ "\\b") in
  List.exists (fun l -> Re.execp rex l) (Fs.read_lines mli_path)

(* Check consistency for a single CSS module *)
let check_module_consistency lib_css test_css mod_name =
  let intf_file = lib_css // (mod_name ^ "_intf.ml") in
  let test_file = test_css // ("test_" ^ mod_name ^ ".ml") in

  if not (Sys.file_exists intf_file) then None
  else
    let valid_types = extract_types intf_file |> List.sort_uniq compare in
    let tests = extract_test_functions test_file in
    let test_names = List.map (fun (n, _, _) -> n) tests in

    (* Special case: for type 't', expect test_<module_name> instead of
       test_t *)
    let expected_test_name t = if t = "t" then mod_name else t in

    let invalid_tests =
      tests
      |> List.filter (fun (n, _body, (_hdr, _ln, ign)) ->
             (not ign)
             && not
                  (List.exists (fun t -> expected_test_name t = n) valid_types))
      |> List.map (fun (n, _, _) -> n)
      |> List.sort_uniq compare
    in

    let missing_tests =
      List.filter
        (fun t ->
          let expected_name = expected_test_name t in
          (not (List.mem t ignored_types))
          && not (List.mem expected_name test_names))
        valid_types
      |> List.map expected_test_name
      |> List.sort_uniq compare
    in

    let wrong_checks = ref [] in
    let missing_neg = ref [] in

    List.iter
      (fun (tname, body, (_hdr, _ln, ignored)) ->
        if not ignored then (
          let checks, neg_reads, has_neg =
            analyze_test_patterns tname body mod_name
          in

          (* Special case: for type 't', expect check_<module_name> instead of
             check_t *)
          let expected_check_name t = if t = "t" then mod_name else t in

          (* Check for wrong check_ calls *)
          List.iter
            (fun c ->
              let expected_for_tname = expected_check_name tname in
              let valid_check_names =
                List.map expected_check_name valid_types
              in
              if c <> expected_for_tname && c <> "value" && c <> "parse_fails"
              then
                if
                  (not
                     (List.exists
                        (fun t -> expected_test_name t = tname)
                        valid_types))
                  && List.mem c valid_check_names
                then wrong_checks := (tname, c) :: !wrong_checks
                else if
                  List.exists
                    (fun t -> expected_test_name t = tname)
                    valid_types
                then wrong_checks := (tname, c) :: !wrong_checks)
            checks;

          (* Check for wrong neg read_ calls *)
          List.iter
            (fun n ->
              let expected_for_tname = expected_check_name tname in
              if n <> expected_for_tname then
                if
                  (not
                     (List.exists
                        (fun t -> expected_test_name t = tname)
                        valid_types))
                  && List.mem n valid_types
                then wrong_checks := (tname, "neg read_" ^ n) :: !wrong_checks
                else if
                  List.exists
                    (fun t -> expected_test_name t = tname)
                    valid_types
                  && List.mem n valid_types
                then wrong_checks := (tname, "neg read_" ^ n) :: !wrong_checks)
            neg_reads;

          (* Check for missing neg patterns - only for functions that test
             actual types and have read functions *)
          let type_for_tname =
            List.find_opt (fun t -> expected_test_name t = tname) valid_types
          in
          match type_for_tname with
          | Some typename ->
              (* Check if this type has a read function *)
              let mli_path = lib_css // (mod_name ^ ".mli") in
              let read_name =
                if typename = "t" then "read" else "read_" ^ typename
              in
              if
                (not has_neg) && Sys.file_exists mli_path
                && file_has_val mli_path read_name
              then missing_neg := tname :: !missing_neg
          | None -> ()))
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
let variables_path = lib_css // "variables.ml"

(* Check for duplicate Var.create calls and direct Css.var_ref usage *)
let check_var_usage () =
  let lib_dir = root // "lib" in
  let ml_files =
    let rec collect_ml_files acc dir =
      let items = Fs.list_dir dir in
      List.fold_left
        (fun acc item ->
          let path = dir // item in
          if Sys.is_directory path then collect_ml_files acc path
          else if Filename.check_suffix item ".ml" then path :: acc
          else acc)
        acc items
    in
    collect_ml_files [] lib_dir
  in

  (* Track Var.create calls *)
  let var_creates = Hashtbl.create 50 in
  let var_ref_uses = ref [] in

  (* Patterns to detect *)
  let var_create_re = Re.Perl.compile_pat "\\bVar\\.create\\s+\"([^\"]+)\"" in
  let css_var_ref_re = Re.Perl.compile_pat "\\bCss\\.var_ref\\b" in

  List.iter
    (fun file ->
      let lines = Fs.read_lines file in
      let relative_file =
        if String.starts_with ~prefix:(root ^ "/") file then
          String.sub file
            (String.length root + 1)
            (String.length file - String.length root - 1)
        else file
      in

      List.iteri
        (fun idx line ->
          (* Check for Var.create *)
          (match Re.exec_opt var_create_re line with
          | Some g ->
              let var_name = Re.Group.get g 1 in
              let location = (relative_file, idx + 1) in
              let existing =
                try Hashtbl.find var_creates var_name with Not_found -> []
              in
              Hashtbl.replace var_creates var_name (location :: existing)
          | None -> ());

          (* Check for Css.var_ref *)
          if Re.execp css_var_ref_re line then
            var_ref_uses := (relative_file, idx + 1, line) :: !var_ref_uses)
        lines)
    ml_files;

  (* Find duplicates *)
  let duplicates = ref [] in
  Hashtbl.iter
    (fun name locations ->
      if List.length locations > 1 then
        duplicates := (name, List.rev locations) :: !duplicates)
    var_creates;

  (* Report issues *)
  let has_issues = !duplicates <> [] || !var_ref_uses <> [] in

  if has_issues then (
    Fmt.pr "@.%s@." (colored bold "Variable System Issues:");
    Fmt.pr "%s@.@." (String.make 50 '=');

    if !duplicates <> [] then (
      Fmt.pr "%s Duplicate Var.create calls found:@." (colored red "Critical:");
      List.iter
        (fun (name, locations) ->
          Fmt.pr "  Variable \"%s\" created in multiple locations:@."
            (colored yellow name);
          List.iter
            (fun (file, line) -> Fmt.pr "    %s:%d@." file line)
            locations;
          Fmt.pr "@.")
        (List.sort compare !duplicates));

    if !var_ref_uses <> [] then (
      Fmt.pr "%s Direct Css.var_ref usage found (use typed Var instead):@."
        (colored red "Critical:");
      List.iter
        (fun (file, line, _) -> Fmt.pr "  %s:%d@." file line)
        (List.rev !var_ref_uses);
      Fmt.pr "@.");

    true)
  else false

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
  mutable missing_vars_of : int;
  mutable missing_modules : string list;
}

let stats =
  {
    total_types = 0;
    missing_read = 0;
    missing_pp = 0;
    missing_check = 0;
    missing_vars_of = 0;
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
  let consistency_warnings = ref 0 in
  List.iter
    (fun mod_name ->
      match check_module_consistency lib_css test_css mod_name with
      | Some (_, invalid_tests, missing_tests, wrong_checks, missing_neg) ->
          print_module_results
            (mod_name, invalid_tests, missing_tests, wrong_checks, missing_neg);
          (* Count any consistency issues *)
          let issues_count =
            List.length invalid_tests + List.length missing_tests
            + List.length wrong_checks + List.length missing_neg
          in
          consistency_warnings := !consistency_warnings + issues_count
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

  if !consistency_warnings > 0 then
    Fmt.pr "%s %d test consistency issues found@." (colored red "ERROR:")
      !consistency_warnings;

  (* Check for missing property case handling in vars_of_property *)
  let check_property_vars_handling () =
    let properties_intf_path = lib_css // "properties_intf.ml" in

    (* Extract property cases that use types with Var constructors *)
    let extract_property_cases_with_vars path =
      let lines = Fs.read_lines path in
      let property_re =
        Re.Perl.compile_pat
          "^[\\s]*\\|[\\s]+([A-Za-z_][A-Za-z0-9_]*)[\\s]*:[\\s]*([A-Za-z_][A-Za-z0-9_]*.*?)\\s+property"
      in
      let var_types = extract_var_types path in

      let rec loop acc = function
        | [] -> List.rev acc
        | l :: tl -> (
            match Re.exec_opt property_re l with
            | Some g ->
                let prop_name = Re.Group.get g 1 in
                let prop_type = Re.Group.get g 2 in
                (* Extract the base type name (before any list/option
                   modifiers) *)
                let base_type =
                  prop_type |> String.split_on_char ' ' |> List.hd
                  |> String.trim
                in
                if List.mem base_type var_types then
                  loop ((prop_name, base_type) :: acc) tl
                else loop acc tl
            | None -> loop acc tl)
      in
      loop [] lines
    in

    let property_cases_with_vars =
      extract_property_cases_with_vars properties_intf_path
    in

    (* Check which cases are handled in vars_of_property *)
    let check_property_case_handled prop_name =
      let lines = Fs.read_lines variables_path in
      let pattern = "\\|[\\s]+" ^ prop_name ^ "[\\s]*," in
      let rex = Re.Perl.compile_pat pattern in
      List.exists (fun l -> Re.execp rex l) lines
    in

    let missing_cases = ref [] in
    List.iter
      (fun (prop_name, prop_type) ->
        if not (check_property_case_handled prop_name) then
          missing_cases := (prop_name, prop_type) :: !missing_cases)
      property_cases_with_vars;

    stats.missing_vars_of <- List.length !missing_cases;

    if !missing_cases <> [] then (
      Fmt.pr "@.%s@."
        (colored bold "Missing Property Case Handling in vars_of_property:");
      Fmt.pr "%s Missing property cases in %s vars_of_property function:@."
        (colored red "Critical:")
        (colored cyan "lib/css/variables.ml");
      List.iter
        (fun (prop_name, prop_type) ->
          Fmt.pr "  | %s, value -> vars_of_%s value@." prop_name
            (String.lowercase_ascii prop_type))
        (List.sort compare !missing_cases);
      Fmt.pr "@.";
      true)
    else false
  in

  let vars_of_issues = check_property_vars_handling () in

  (* Check for variable system issues *)
  let var_issues = check_var_usage () in

  (* Check for duplicate priorities across Handler modules *)
  let check_unique_priorities () =
    (* Allow certain priority values to be shared by related handler groups.
       Priority 0: Container and position utilities - ordered by suborder within
       the same priority group. Priority 2: Margin and prose size utilities -
       ordered by suborder within the same priority group. Priority 4: Display
       utilities (layout, flex, grid, tables) - these are ordered by suborder
       within the same priority group. Priority 21: Color and prose-color
       utilities - ordered by suborder within the same priority group. *)
    let allowed_shared_priorities = [ 0; 2; 4; 21 ] in

    let lib_dir = root // "lib" in
    let ml_files =
      Fs.list_dir lib_dir
      |> List.filter (fun f -> Filename.check_suffix f ".ml")
      |> List.map (fun f -> lib_dir // f)
    in

    (* Pattern to match Handler modules and extract priority *)
    let handler_re = Re.Perl.compile_pat "module\\s+Handler\\s*=" in
    let priority_re = Re.Perl.compile_pat "let\\s+priority\\s*=\\s*([0-9]+)" in
    let name_re = Re.Perl.compile_pat "let\\s+name\\s*=\\s*\"([^\"]+)\"" in

    let priorities = Hashtbl.create 50 in

    List.iter
      (fun file ->
        let lines = Fs.read_lines file in
        let relative_file =
          if String.starts_with ~prefix:(root ^ "/") file then
            String.sub file
              (String.length root + 1)
              (String.length file - String.length root - 1)
          else file
        in

        (* Check if file contains a Handler module *)
        let has_handler = List.exists (fun l -> Re.execp handler_re l) lines in

        if has_handler then
          (* Find priority and name within the handler module *)
          let rec find_in_handler in_handler prio name idx = function
            | [] -> (prio, name)
            | l :: rest ->
                if (not in_handler) && Re.execp handler_re l then
                  find_in_handler true prio name (idx + 1) rest
                else if
                  in_handler
                  && Re.execp (Re.Perl.compile_pat "^end\\s*$\\|^end\\s*//") l
                then (prio, name)
                else if in_handler then
                  let new_prio =
                    match Re.exec_opt priority_re l with
                    | Some g -> Some (int_of_string (Re.Group.get g 1), idx + 1)
                    | None -> prio
                  in
                  let new_name =
                    match Re.exec_opt name_re l with
                    | Some g -> Some (Re.Group.get g 1)
                    | None -> name
                  in
                  find_in_handler true new_prio new_name (idx + 1) rest
                else find_in_handler in_handler prio name (idx + 1) rest
          in

          match find_in_handler false None None 0 lines with
          | Some (prio_val, line), Some module_name ->
              let existing =
                try Hashtbl.find priorities prio_val with Not_found -> []
              in
              Hashtbl.replace priorities prio_val
                ((module_name, relative_file, line) :: existing)
          | _ -> ())
      ml_files;

    (* Find duplicates, excluding allowed shared priorities *)
    let duplicates = ref [] in
    Hashtbl.iter
      (fun prio modules ->
        if
          List.length modules > 1
          && not (List.mem prio allowed_shared_priorities)
        then duplicates := (prio, List.rev modules) :: !duplicates)
      priorities;

    if !duplicates <> [] then (
      Fmt.pr "@.%s@." (colored bold "Handler Priority Conflicts:");
      Fmt.pr "%s Multiple Handler modules with the same priority:@."
        (colored red "Critical:");
      List.iter
        (fun (prio, modules) ->
          Fmt.pr "@.  Priority %d shared by:@." prio;
          List.iter
            (fun (name, file, line) ->
              Fmt.pr "    - %s (%s:%d)@." (colored yellow name) file line)
            modules)
        (List.sort compare !duplicates);
      Fmt.pr "@.";
      true)
    else false
  in

  let priority_issues = check_unique_priorities () in

  let exit_code =
    if
      api_missing > 0 || !consistency_warnings > 0 || var_issues
      || vars_of_issues || priority_issues
    then 1
    else 0
  in

  if exit_code = 1 then
    Fmt.pr "@.%s Critical issues must be fixed before proceeding.@."
      (colored red "Action Required:")
  else if test_missing > 0 then
    Fmt.pr "@.%s Consider adding test functions for complete coverage.@."
      (colored yellow "Recommendation:");

  exit exit_code
