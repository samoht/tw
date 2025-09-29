(** CSS comparison utilities for testing using the proper CSS parser *)

(* ===== Constants ===== *)

let header_comment_start = 3 (* Position after "/*" *)

(* ===== Type Definitions ===== *)

(* Use types from Tree_diff module *)
module D = Tree_diff

let is_empty = D.is_empty

(* Statistics about CSS differences *)
type stats = {
  expected : string;
  actual : string;
  expected_chars : int;
  actual_chars : int;
  added_rules : int;
  removed_rules : int;
  modified_rules : int;
  reordered_rules : int;
  container_changes : int;
}

(* ===== Helper Functions ===== *)

let strip_header css =
  (* Strip a leading /*!...*/ header comment with simpler flow to reduce
     nesting *)
  let stripped =
    if not (String.starts_with ~prefix:"/*!" css) then css
    else
      let len = String.length css in
      (* Find the end of the opening header comment "*/" starting at index 3 *)
      let rec find_comment_end i =
        if i + 1 >= len then None
        else if css.[i] = '*' && css.[i + 1] = '/' then Some (i + 2)
        else find_comment_end (i + 1)
      in
      match find_comment_end header_comment_start with
      | None -> css
      | Some j ->
          let start_pos = if j < len && css.[j] = '\n' then j + 1 else j in
          if start_pos >= len then ""
          else String.sub css start_pos (len - start_pos)
  in
  (* Trim trailing whitespace for consistent comparison *)
  String.trim stripped

(* Analyze differences between two parsed CSS ASTs, returning structural
   changes *)

let tree_diff ~(expected : Css.t) ~(actual : Css.t) : Tree_diff.t =
  D.diff ~expected ~actual

(* Compare two CSS strings for structural and formatting equality *)
let compare css1 css2 =
  let css1 = strip_header css1 in
  let css2 = strip_header css2 in
  (* If strings are identical, they're equal *)
  if css1 = css2 then true
  else
    match (Css.of_string css1, Css.of_string css2) with
    | Ok expected, Ok actual ->
        let d = tree_diff ~expected ~actual in
        (* Only consider them equal if both structural diff is empty AND strings
           are identical *)
        is_empty d && css1 = css2
    | _ -> false (* Parse errors with different strings are not equal *)

(* Parse two CSS strings and return their diff or parse errors *)
type t =
  | Tree_diff of Tree_diff.t (* CSS AST differences found *)
  | String_diff of String_diff.t (* No structural diff but strings differ *)
  | No_diff (* Strings are identical *)
  | Both_errors of Css.parse_error * Css.parse_error
  | Expected_error of Css.parse_error
  | Actual_error of Css.parse_error

let diff ~expected ~actual =
  let expected = strip_header expected in
  let actual = strip_header actual in
  match (Css.of_string expected, Css.of_string actual) with
  | Ok expected_ast, Ok actual_ast -> (
      let structural_diff =
        tree_diff ~expected:expected_ast ~actual:actual_ast
      in
      (* First check if strings are identical *)
      if expected = actual then
        (* Strings are identical, so there should be no structural diff *)
        if is_empty structural_diff then No_diff
        else
          (* This shouldn't happen - identical strings should produce identical
             ASTs *)
          failwith "BUG: identical strings produced different ASTs"
      else if
        (* Strings differ - always return a diff *)
        not (is_empty structural_diff)
      then Tree_diff structural_diff
      else
        (* Structural diff is empty but strings differ - return string diff *)
        match String_diff.diff ~expected actual with
        | Some sdiff -> String_diff sdiff
        | None ->
            (* This should not happen - if strings differ, String_diff.diff
               should always find the difference *)
            failwith
              "BUG: different strings but String_diff found no difference")
  | Error e1, Error e2 -> Both_errors (e1, e2)
  | Ok _, Error e -> Actual_error e
  | Error e, Ok _ -> Expected_error e

let as_tree_diff = function
  | Tree_diff d -> Some d
  | String_diff _ | No_diff | Both_errors _ | Expected_error _ | Actual_error _
    ->
      None

(* Compute statistics from diff results *)
let compute_stats ~expected_str ~actual_str diff_result =
  let expected_chars = String.length expected_str in
  let actual_chars = String.length actual_str in

  match diff_result with
  | Tree_diff d ->
      let count_rule_type pred = List.filter pred d.rules |> List.length in
      {
        expected = expected_str;
        actual = actual_str;
        expected_chars;
        actual_chars;
        added_rules =
          count_rule_type (function D.Rule_added _ -> true | _ -> false);
        removed_rules =
          count_rule_type (function D.Rule_removed _ -> true | _ -> false);
        modified_rules =
          count_rule_type (function
            | D.Rule_content_changed _ | D.Rule_selector_changed _ -> true
            | _ -> false);
        reordered_rules =
          count_rule_type (function D.Rule_reordered _ -> true | _ -> false);
        container_changes = List.length d.containers;
      }
  | _ ->
      (* For non-tree diffs, just return character stats *)
      {
        expected = expected_str;
        actual = actual_str;
        expected_chars;
        actual_chars;
        added_rules = 0;
        removed_rules = 0;
        modified_rules = 0;
        reordered_rules = 0;
        container_changes = 0;
      }

(* Alias for compute_stats *)
let stats = compute_stats

(* Format the result of diff with optional labels *)
let pp ?(expected = "Expected") ?(actual = "Actual") fmt = function
  | Tree_diff d ->
      (* Show structural differences *)
      D.pp ~expected ~actual fmt d
  | String_diff sdiff -> String_diff.pp fmt sdiff
  | No_diff ->
      (* No output for identical files *)
      ()
  | Both_errors (e1, e2) ->
      let err1 = Css.pp_parse_error e1 in
      let err2 = Css.pp_parse_error e2 in
      if String.equal err1 err2 then
        Fmt.pf fmt "Both CSS have same parse error: %s" err1
      else
        Fmt.pf fmt "Parse errors:\n  %s: %s\n  %s: %s" expected err1 actual err2
  | Expected_error e ->
      Fmt.pf fmt "%s CSS parse error: %s" expected (Css.pp_parse_error e)
  | Actual_error e ->
      Fmt.pf fmt "%s CSS parse error: %s" actual (Css.pp_parse_error e)

let pp_stats fmt stats =
  let char_diff = abs (stats.actual_chars - stats.expected_chars) in
  let char_diff_pct =
    if stats.expected_chars > 0 then
      float_of_int char_diff *. 100.0 /. float_of_int stats.expected_chars
    else 0.0
  in

  Fmt.pf fmt "@[<v>CSS: %d chars vs %d chars (%.1f%% diff)@," stats.actual_chars
    stats.expected_chars char_diff_pct;

  (* Build a list of non-zero changes *)
  let changes = [] in
  let changes =
    if stats.added_rules > 0 then
      let rules = if stats.added_rules = 1 then "rule" else "rules" in
      Printf.sprintf "%d added %s" stats.added_rules rules :: changes
    else changes
  in
  let changes =
    if stats.removed_rules > 0 then
      let rules = if stats.removed_rules = 1 then "rule" else "rules" in
      Printf.sprintf "%d removed %s" stats.removed_rules rules :: changes
    else changes
  in
  let changes =
    if stats.modified_rules > 0 then
      let rules = if stats.modified_rules = 1 then "rule" else "rules" in
      Printf.sprintf "%d modified %s" stats.modified_rules rules :: changes
    else changes
  in
  let changes =
    if stats.reordered_rules > 0 then
      let rules = if stats.reordered_rules = 1 then "rule" else "rules" in
      Printf.sprintf "%d reordered %s" stats.reordered_rules rules :: changes
    else changes
  in
  let changes = List.rev changes in

  (* Also check for container changes *)
  let container_changes =
    if stats.container_changes > 0 then
      [ Printf.sprintf "%d containers" stats.container_changes ]
    else []
  in
  let all_changes = changes @ container_changes in

  if all_changes <> [] then
    Fmt.pf fmt "Changes: %s@," (String.concat ", " all_changes)
  else Fmt.pf fmt "No structural differences@,";

  Fmt.pf fmt "@]"
