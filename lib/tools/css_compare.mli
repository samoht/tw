(** CSS comparison utilities for testing using the proper CSS parser *)

type property_diff_detail = {
  property : string;
  our_value : string;
  their_value : string;
}

type change = Added | Removed | Modified of property_diff_detail list

type rule_change = {
  selector : string;
  change : change;
  properties : Css.declaration list; (* For added/removed rules *)
}

type media_change = {
  condition : string;
  change : change;
  rules : rule_change list;
}

type layer_change = { name : string; change : change; rules : rule_change list }

type supports_change = {
  condition : string;
  change : change;
  rules : rule_change list;
}

type container_change = {
  name : string option;
  condition : string;
  change : change;
  rules : rule_change list;
}

type property_change = {
  name : string;
  change : change;
  details : property_diff_detail list;
}

type t = {
  rules : rule_change list;
  media : media_change list;
  layers : layer_change list;
  supports : supports_change list;
  containers : container_change list;
  properties : property_change list;
}

val pp : ?expected:string -> ?actual:string -> t Fmt.t
(** [pp ?expected ?actual] pretty-prints structured CSS diffs with optional
    labels.
    @param expected Label for expected CSS (default: "Expected").
    @param actual Label for actual CSS (default: "Actual"). *)

val equal : t -> t -> bool
(** [equal a b] is [true] if [a] and [b] have no differences. *)

val diff_ast : expected:Css.t -> actual:Css.t -> t
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

val count_css_class_patterns : string -> string -> int * int * int
[@@deprecated "Use structured diff APIs instead"]
(** [count_css_class_patterns css class_name] returns (base_count, where_count,
    total_count). *)

val dominant_css_class : string -> string * int
[@@deprecated "Use structured diff APIs instead"]
(** [dominant_css_class css] finds the most common CSS class and its count. *)

type diff_result =
  | Diff of t
  | Both_errors of Css.parse_error * Css.parse_error
  | Expected_error of Css.parse_error
  | Actual_error of Css.parse_error
      (** Result of diffing two CSS strings - either a diff or parse errors *)

val diff : expected:string -> actual:string -> diff_result
(** [diff ~expected ~actual] parses both CSS strings and returns their diff or
    parse errors if parsing fails. *)

val show_string_diff_context :
  expected:string ->
  actual:string ->
  (string * string * (int * int) * int) option
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

val pp_diff_result :
  ?expected:string ->
  ?actual:string ->
  ?expected_str:string ->
  ?actual_str:string ->
  diff_result Fmt.t
(** [pp_diff_result ?expected ?actual ?expected_str ?actual_str] formats a
    diff_result with optional labels and original strings for context display.
    @param expected Label for expected CSS (default: "Expected").
    @param actual Label for actual CSS (default: "Actual").
    @param expected_str
      Original expected string for showing context when no structural diffs
    @param actual_str
      Original actual string for showing context when no structural diffs *)
