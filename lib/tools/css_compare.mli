(** CSS comparison utilities for testing using the proper CSS parser.

    {1:overview Overview}

    This module provides tools for comparing CSS stylesheets and detecting
    differences at the structural level. It parses CSS using the proper CSS
    parser and reports differences in a structured format suitable for testing
    and debugging.

    The comparison results are represented as structured differences that
    distinguish between different types of changes: additions, removals, content
    modifications, and reorderings. *)

(** {1:diffs Difference types} *)

type declaration = {
  property_name : string;
  expected_value : string;
  actual_value : string;
}
(** A CSS property declaration difference. *)

(** CSS rule-level differences. *)
type rule_diff =
  | Rule_added of { selector : string; declarations : Css.declaration list }
      (** A CSS rule was added. *)
  | Rule_removed of { selector : string; declarations : Css.declaration list }
      (** A CSS rule was removed. *)
  | Rule_content_changed of {
      selector : string;
      old_declarations : Css.declaration list;
      new_declarations : Css.declaration list;
      property_changes : declaration list;
    }  (** A CSS rule's content was modified. *)
  | Rule_selector_changed of {
      old_selector : string;
      new_selector : string;
      declarations : Css.declaration list;
    }  (** A CSS rule's selector was changed but content remained the same. *)
  | Rule_reordered of { selector : string }
      (** A CSS rule was moved to a different position but content is unchanged.
      *)

type container_diff =
  | Container_added of container_info
      (** A container rule (media queries, supports queries, etc.) was added. *)
  | Container_removed of container_info  (** A container rule was removed. *)
  | Container_modified of {
      info : container_info;
      rule_changes : rule_diff list;
    }  (** A container rule was modified (rules inside it changed). *)

and container_info = {
  container_type : [ `Media | `Layer | `Supports | `Container | `Property ];
  condition : string;
}
(** Container rule information and the differences within. *)

type tree_diff = { rules : rule_diff list; containers : container_diff list }
(** Structured CSS differences. Contains rule-level and container-level
    differences. *)

val pp_tree_diff : ?expected:string -> ?actual:string -> tree_diff Fmt.t
(** [pp ?expected ?actual] pretty-prints structured CSS diffs with optional
    labels.
    @param expected Label for expected CSS (default: "Expected").
    @param actual Label for actual CSS (default: "Actual"). *)

val equal_tree_diff : tree_diff -> tree_diff -> bool
(** [equal a b] is [true] if [a] and [b] have no differences. *)

val is_empty : tree_diff -> bool
(** [is_empty d] returns [true] if [d] contains no differences. *)

val tree_diff : expected:Css.t -> actual:Css.t -> tree_diff
(** [diff_ast ~expected ~actual] returns a structured diff between two CSS ASTs.
    @param expected The expected/reference CSS AST.
    @param actual The actual/generated CSS AST. *)

val strip_header : string -> string
(** [strip_header css] removes a leading header comment if present. *)

val compare_css : string -> string -> bool
(** [compare_css a b] returns [true] when [a] and [b] are structurally
    equivalent CSS ASTs. *)

val extract_base_rules : string -> string -> string list
[@@deprecated "Use structured diff APIs instead"]
(** [extract_base_rules css class_name] extracts all selector strings for rules
    containing the given class name. *)

val dominant_css_class : string -> string * int
[@@deprecated "Use structured diff APIs instead"]
(** [dominant_css_class css] finds the most common CSS class and its count. *)

type string_diff = {
  position : int;  (** Character position of first difference *)
  line_expected : int;
  column_expected : int;
  line_actual : int;
  column_actual : int;
  context_before : (string * string) list;
      (** (expected, actual) line pairs before diff *)
  diff_lines : string * string;  (** The lines containing the difference *)
  context_after : (string * string) list;
      (** (expected, actual) line pairs after diff *)
}
(** String diff information for character-level differences *)

type t =
  | Tree_diff of tree_diff  (** CSS AST differences found *)
  | String_diff of string_diff  (** No structural diff but strings differ *)
  | No_diff  (** Strings are identical *)
  | Both_errors of Css.parse_error * Css.parse_error
  | Expected_error of Css.parse_error
  | Actual_error of Css.parse_error
      (** Result of diffing two CSS strings - either a diff or parse errors *)

val diff : expected:string -> actual:string -> t
(** [diff ~expected ~actual] parses both CSS strings and returns their diff or
    parse errors if parsing fails. *)

val show_string_diff_context :
  expected:string -> actual:string -> string_diff option
(** [show_string_diff_context ~expected ~actual] finds the first difference
    between two strings and returns context around it.
    @return
      Some (expected_context, actual_context, (line_num, char_pos), diff_pos)
      where:
      - expected_context: Context from expected string up to the line with the
        diff
      - actual_context: Context from actual string up to the line with the diff
      - line_num: Line number containing the diff (0-based)
      - char_pos: Character position within that line
      - diff_pos: Overall position of the first difference Returns None if
        strings are identical. *)

val pp : ?expected:string -> ?actual:string -> t Fmt.t
(** [pp_diff_result ?expected ?actual ?expected_str ?actual_str] formats a
    diff_result with optional labels and original strings for context display.
    @param expected Label for expected CSS (default: "Expected").
    @param actual Label for actual CSS (default: "Actual").
    @param expected_str
      Original expected string for showing context when no structural diffs
    @param actual_str
      Original actual string for showing context when no structural diffs *)

val pp_stats : expected_str:string -> actual_str:string -> t Fmt.t
(** [pp_stats ~expected_str ~actual_str] formats statistics about the
    differences found. Shows character counts and number of differences in each
    category. *)
