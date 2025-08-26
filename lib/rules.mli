(** CSS rule generation and management *)

open Core

(** {1 Types} *)

type rule_output =
  | Regular of string * Css.declaration list
  | MediaQuery of string * string * Css.declaration list
  | ContainerQuery of string * string * Css.declaration list
  | StartingStyle of string * Css.declaration list

(** {1 CSS Generation} *)

val to_css : ?reset:bool -> ?mode:Css.mode -> t list -> Css.t
(** [to_css ?reset ?mode classes] generates a complete CSS stylesheet from
    Tailwind classes.
    @param reset Whether to include CSS reset rules (default: true).
    @param mode CSS variable handling mode (default: Variables). *)

val to_inline_style : t list -> string
(** [to_inline_style styles] generates inline CSS string from Tailwind classes.
*)

(** {1 Helper Functions} *)

val extract_selector_props : t -> rule_output list
(** [extract_selector_props tw] extracts CSS rules from a Tailwind class. *)

val generate_reset_rules : unit -> Css.rule list
(** [generate_reset_rules ()] generates CSS reset rules. *)

val escape_class_name : string -> string
(** [escape_class_name name] escapes special characters in CSS class names. *)

val string_of_breakpoint : breakpoint -> string
(** [string_of_breakpoint bp] converts a breakpoint to its string
    representation. *)

val responsive_breakpoint : string -> string
(** [responsive_breakpoint prefix] returns the CSS breakpoint value for a
    prefix. *)
