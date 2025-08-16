(** CSS debugging utilities for testing *)

val write_temp_css : string -> string -> string
(** [write_temp_css name css] writes CSS to a temporary file and returns the
    path. *)

val format_css : string -> string
(** [format_css css] formats minified CSS for readability. *)

val extract_rule : string -> string -> string option
(** [extract_rule css selector] extracts a specific CSS rule by selector. *)

val find_first_diff : string -> string -> (int * string * string) option
(** [find_first_diff css1 css2] finds the first character difference between two
    CSS strings. Returns: Some (position, description, context) or None if
    identical. *)
