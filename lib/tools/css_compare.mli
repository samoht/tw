(** CSS comparison utilities for testing *)

val strip_header : string -> string
(** [strip_header css] removes a leading header comment if present. *)

val compare_css : string -> string -> bool
(** [compare_css a b] returns [true] when [a] and [b] are structurally
    equivalent CSS (ignoring property order within a rule). *)

val format_diff : string -> string -> string
(** [format_diff ours theirs] produces a human-readable, single-difference
    summary between two CSS strings. *)

(** {2 Testing Interface} *)

(** The following types and functions are exposed for unit testing purposes. *)

type token =
  | Selector of string
  | Property of string * string
  | Open_brace
  | Close_brace
  | At_rule of string
  | Semicolon
  | Comma

type css_rule = { selector : string; properties : (string * string) list }

type css_block =
  | Rule of css_rule
  | At_block of string * css_block list
  | Layer of string

val tokenize : string -> token list
(** [tokenize css] converts a CSS string into a list of tokens. *)

val parse_blocks : token list -> css_block list
(** [parse_blocks tokens] builds CSS structure from tokens. *)

val normalize_blocks : css_block list -> css_block list
(** [normalize_blocks blocks] sorts properties within rules for comparison. *)

val extract_base_rules : string -> string -> string list
(** [extract_base_rules css class_name] extracts all rules for the given class.
*)

val count_css_class_patterns : string -> string -> int * int * int
(** [count_css_class_patterns css class_name] returns (base_count, where_count,
    total_count). *)

val find_dominant_css_class : string -> string * int
(** [find_dominant_css_class css] finds the most common CSS class and its count.
*)

val structured_diff :
  tw_label:string ->
  tailwind_label:string ->
  ?css1:string ->
  ?css2:string ->
  css_block list ->
  css_block list ->
  string
(** [structured_diff ~tw_label ~tailwind_label ?css1 ?css2 blocks1 blocks2]
    produces a detailed structural diff between two CSS block lists. *)
