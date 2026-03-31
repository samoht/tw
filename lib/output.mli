(** CSS rule output — the result of extracting a Tailwind utility class.

    A single [Output.t] value represents one CSS rule (or at-rule) produced by a
    utility class with zero or more modifiers. The five variants correspond to
    the five CSS constructs Tailwind can emit:

    - [Regular] — a plain selector rule, possibly with nested CSS
    - [Media_query] — a rule wrapped in a responsive or preference [@ media]
    - [Container_query] — a rule wrapped in [@container]
    - [Starting_style] — a rule wrapped in [@starting-style]
    - [Supports_query] — a rule wrapped in [@supports]

    Values are produced by {!Rule.outputs} and consumed by {!Build}. *)

open Cascade

type t =
  | Regular of {
      selector : Css.Selector.t;
      props : Css.declaration list;
      base_class : string option;
          (** Base class name without the dot, e.g. ["p-4"]. *)
      has_hover : bool;
          (** [true] when the rule carries a hover pseudo-class modifier. Used
              by {!Build} to wrap it in [@media (hover:hover)]. *)
      nested : Css.statement list;
          (** Nested [@media] / [@supports] statements for compound modifiers.
          *)
      merge_key : string option;
          (** Override key for the CSS optimizer's merge heuristic. *)
      not_order : int;
          (** Sort key for [not-*] variant rules; 0 for normal rules. *)
    }
  | Media_query of {
      condition : Css.Media.t;
      selector : Css.Selector.t;
      props : Css.declaration list;
      base_class : string option;
      nested : Css.statement list;
      not_order : int;
    }
  | Container_query of {
      condition : Css.Container.t;
      selector : Css.Selector.t;
      props : Css.declaration list;
      base_class : string option;
    }
  | Starting_style of {
      selector : Css.Selector.t;
      props : Css.declaration list;
      base_class : string option;
    }
  | Supports_query of {
      condition : Css.Supports.t;
      selector : Css.Selector.t;
      props : Css.declaration list;
      base_class : string option;
      merge_key : string option;
      not_order : int;
    }

(** {1 Smart constructors}

    All arguments except [selector] and [props] are optional and default to
    sensible values ([false], [[]], [None], [0]). *)

val regular :
  selector:Css.Selector.t ->
  props:Css.declaration list ->
  ?base_class:string ->
  ?has_hover:bool ->
  ?nested:Css.statement list ->
  ?merge_key:string ->
  ?not_order:int ->
  unit ->
  t

val media_query :
  condition:Css.Media.t ->
  selector:Css.Selector.t ->
  props:Css.declaration list ->
  ?base_class:string ->
  ?nested:Css.statement list ->
  ?not_order:int ->
  unit ->
  t
(** [media_query ~condition ~selector ~props ()] constructs a [Media_query] rule
    wrapped in [@media condition]. *)

val container_query :
  condition:Css.Container.t ->
  selector:Css.Selector.t ->
  props:Css.declaration list ->
  ?base_class:string ->
  unit ->
  t
(** [container_query ~condition ~selector ~props ()] constructs a
    [Container_query] rule wrapped in [@container condition]. *)

val starting_style :
  selector:Css.Selector.t ->
  props:Css.declaration list ->
  ?base_class:string ->
  unit ->
  t
(** [starting_style ~selector ~props ()] constructs a [Starting_style] rule
    wrapped in [@starting-style]. *)

val supports_query :
  condition:Css.Supports.t ->
  selector:Css.Selector.t ->
  props:Css.declaration list ->
  ?base_class:string ->
  ?merge_key:string ->
  ?not_order:int ->
  unit ->
  t
(** [supports_query ~condition ~selector ~props ()] constructs a
    [Supports_query] rule wrapped in [@supports condition]. *)

val pp : t -> string
(** [pp r] returns a short human-readable description of [r], e.g.
    ["Regular(.p-4)"] or ["Media_query(.sm\\:p-4)"]. Useful for test failure
    messages and debug output. *)

(** {1 Classification} *)

type by_type = {
  regular : t list;
  media : t list;
  container : t list;
  starting : t list;
  supports : t list;
}
(** Rules partitioned by variant, preserving original order within each group.
*)

val classify_by_type : t list -> by_type
(** [classify_by_type rules] partitions [rules] by variant into a [by_type]
    record. Each group preserves the original order of its elements. *)

val is_hover_rule : t -> bool
(** [is_hover_rule r] is [true] iff [r] is a [Regular] rule with
    [has_hover = true]. Used by {!Build} to gate hover utilities under
    [@media (hover:hover)]. *)
