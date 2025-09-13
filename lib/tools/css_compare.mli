(** CSS comparison utilities for testing using the proper CSS parser *)

type property_diff = {
  property : string;
  our_value : string;
  their_value : string;
}

type change = Added | Removed | Modified of property_diff list

type rule_change = {
  selector : string;
  change : change;
  properties : (string * string) list;
}

type media_change = {
  condition : string;
  change : change;
  rules : rule_change list;
}

type layer_change = { name : string; change : change; rules : rule_change list }

type t = {
  rules : rule_change list;
  media : media_change list;
  layers : layer_change list;
}

val pp : t Fmt.t
(** [pp] pretty-prints structured CSS diffs. *)

val equal : t -> t -> bool
(** [equal a b] is [true] if [a] and [b] have no differences. *)

val diff : Css.t -> Css.t -> t
(** [diff ast1 ast2] returns a structured diff between two CSS ASTs. *)

val format_css_diff : string -> string -> string
(** [format_css_diff css1 css2] returns a formatted diff of two CSS strings. *)

val strip_header : string -> string
(** [strip_header css] removes a leading header comment if present. *)

val compare_css : string -> string -> bool
(** [compare_css a b] returns [true] when [a] and [b] are structurally
    equivalent CSS ASTs. *)

val extract_base_rules : string -> string -> string list
(** [extract_base_rules css class_name] extracts all selector strings for rules
    containing the given class name. *)

val count_css_class_patterns : string -> string -> int * int * int
(** [count_css_class_patterns css class_name] returns (base_count, where_count,
    total_count). *)

val find_dominant_css_class : string -> string * int
(** [find_dominant_css_class css] finds the most common CSS class and its count.
*)

val format_labeled_css_diff :
  tw_label:string ->
  tailwind_label:string ->
  ?css1:string ->
  ?css2:string ->
  'a ->
  'b ->
  string
(** [format_labeled_css_diff ~tw_label ~tailwind_label ?css1 ?css2 _ _] produces
    a detailed structural diff between two CSS strings with custom labels. The
    last two parameters are ignored for backward compatibility. *)
