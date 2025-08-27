(** CSS rule generation and management *)

open Core

(** {1 Types} *)

type rule_output =
  | Regular of { selector : string; props : Css.declaration list }
  | Media_query of {
      condition : string;
      selector : string;
      props : Css.declaration list;
    }
  | Container_query of {
      condition : string;
      selector : string;
      props : Css.declaration list;
    }
  | Starting_style of { selector : string; props : Css.declaration list }

val regular : selector:string -> props:Css.declaration list -> rule_output
(** Smart constructors for rule_output *)

val media_query :
  condition:string ->
  selector:string ->
  props:Css.declaration list ->
  rule_output

val container_query :
  condition:string ->
  selector:string ->
  props:Css.declaration list ->
  rule_output

val starting_style :
  selector:string -> props:Css.declaration list -> rule_output

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

(** {1 Rule Extraction and Processing} *)

val modifier_to_rule :
  Core.modifier -> string -> string -> Css.declaration list -> rule_output
(** [modifier_to_rule modifier base_class selector props] converts a modifier
    into appropriate CSS rule output. *)

val group_by_selector : rule_output list -> (string * Css.declaration list) list
(** [group_by_selector rules] groups regular rules by selector. *)

val is_hover_rule : string -> bool
(** [is_hover_rule selector] checks if a selector is a hover rule. *)

val rule_sets :
  t list -> Css.rule list * Css.media_query list * Css.container_query list
(** [rule_sets tw_classes] processes Tailwind classes into CSS rule sets. *)

(** {1 Variable Resolution} *)

val resolve_dependencies : string list -> string list -> string list
(** [resolve_dependencies vars_to_check resolved] resolves CSS variable
    dependencies. *)

(** {1 Conflict Resolution} *)

val conflict_group : string -> int * int
(** [conflict_group selector] returns the conflict group priority for utility
    ordering. *)

val color_order : string -> int
(** [color_order color] returns the ordering priority for color names. *)

(** {1 Layer Generation} *)

val compute_theme_layer : t list -> Css.layered_rules
(** [compute_theme_layer tw_classes] generates the theme layer with CSS
    variables. *)

val compute_properties_layer :
  Css.rule list -> Css.layered_rules option * Css.at_property list
(** [compute_properties_layer rules] generates the properties layer and @property rules. *)

val build_utilities_layer :
  rules:Css.rule list ->
  media_queries:Css.media_query list ->
  container_queries:Css.container_query list ->
  Css.layered_rules
(** [build_utilities_layer ~rules ~media_queries ~container_queries] builds the
    utilities layer with proper conflict ordering. *)

(** {1 Utility Functions} *)

val escape_class_name : string -> string
(** [escape_class_name name] escapes special characters in CSS class names. *)

val string_of_breakpoint : breakpoint -> string
(** [string_of_breakpoint bp] converts a breakpoint to its string
    representation. *)

val responsive_breakpoint : string -> string
(** [responsive_breakpoint prefix] returns the CSS breakpoint value for a
    prefix. *)
