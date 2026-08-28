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
    }
  | Media_query of {
      condition : Css.Media.t;
      selector : Css.Selector.t;
      props : Css.declaration list;
      base_class : string option;
      nested : Css.statement list;
    }
  | Container_query of {
      condition : Css.Container.t;
      selector : Css.Selector.t;
      props : Css.declaration list;
      base_class : string option;
      nested : Css.statement list;
    }
  | Starting_style of {
      selector : Css.Selector.t;
      props : Css.declaration list;
      base_class : string option;
      nested : Css.statement list;
    }
  | Supports_query of {
      condition : Css.Supports.t;
      selector : Css.Selector.t;
      props : Css.declaration list;
      base_class : string option;
      merge_key : string option;
      nested : Css.statement list;
    }

(** {1 Smart constructors}

    All arguments except [selector] and [props] are optional and default to
    sensible values ([false], [[]], [None]). *)

val regular :
  selector:Css.Selector.t ->
  props:Css.declaration list ->
  ?base_class:string ->
  ?has_hover:bool ->
  ?nested:Css.statement list ->
  ?merge_key:string ->
  unit ->
  t
(** [regular ~selector ~props ()] constructs a plain (non-media,
    non-conditional) rule. *)

val media_query :
  condition:Css.Media.t ->
  selector:Css.Selector.t ->
  props:Css.declaration list ->
  ?base_class:string ->
  ?nested:Css.statement list ->
  unit ->
  t
(** [media_query ~condition ~selector ~props ()] constructs a
    {!constructor-Media_query} rule wrapped in [@media condition]. *)

val container_query :
  condition:Css.Container.t ->
  selector:Css.Selector.t ->
  props:Css.declaration list ->
  ?base_class:string ->
  ?nested:Css.statement list ->
  unit ->
  t
(** [container_query ~condition ~selector ~props ()] constructs a
    {!constructor-Container_query} rule wrapped in [@container condition]. *)

val starting_style :
  selector:Css.Selector.t ->
  props:Css.declaration list ->
  ?base_class:string ->
  ?nested:Css.statement list ->
  unit ->
  t
(** [starting_style ~selector ~props ()] constructs a
    {!constructor-Starting_style} rule wrapped in [@starting-style]. *)

val supports_query :
  condition:Css.Supports.t ->
  selector:Css.Selector.t ->
  props:Css.declaration list ->
  ?base_class:string ->
  ?merge_key:string ->
  ?nested:Css.statement list ->
  unit ->
  t
(** [supports_query ~condition ~selector ~props ()] constructs a
    {!constructor-Supports_query} rule wrapped in [@supports condition]. *)

val base_class : t -> string option
(** [base_class rule] is the class name [rule] was built for, if it has one. *)

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
(** [classify_by_type rules] partitions [rules] by variant into a
    {!type-by_type} record. Each group preserves the original order of its
    elements. *)

val is_hover_rule : t -> bool
(** [is_hover_rule r] is [true] iff [r] is a {!constructor-Regular} rule with
    [has_hover = true]. Used by {!Build} to gate hover utilities under
    [@media (hover:hover)]. *)
