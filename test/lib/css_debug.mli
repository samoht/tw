(** CSS debugging utilities for testing *)

val write_temp_css : string -> string -> string
(** Write CSS to a temporary file and return the path *)

val format_css : string -> string
(** Format minified CSS for readability *)

val extract_rule : string -> string -> string option
(** Extract a specific CSS rule by selector *)

val detailed_diff : string -> string -> string * string * (unit -> unit)
(** Compare two CSS strings and show detailed differences Returns:
    (structural_diff, diff_command, cleanup_function) *)

val find_first_diff : string -> string -> (int * string * string) option
(** Find the first character difference between two CSS strings Returns: Some
    (position, description, context) or None if identical *)

val save_for_inspection :
  our_css:string -> tailwind_css:string -> test_name:string -> string
(** Save CSS files for manual inspection Returns: Path where files were saved *)
