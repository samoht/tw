(** CSS rule cascade sort order.

    Provides the [indexed_rule] type and [compare_indexed_rules], the comparison
    function that produces Tailwind v4 cascade order when used with [List.sort].
*)

(** {1 Indexed rule} *)

type selector_kind =
  | Simple
  | Pseudo_element
  | Complex of {
      has_focus : bool;
      has_focus_within : bool;
      has_focus_visible : bool;
      has_group : bool;
      has_peer : bool;
      has_group_has : bool;
      has_peer_has : bool;
      has_standalone_has : bool;
      has_aria : bool;
    }

type indexed_rule = {
  index : int;  (** Source position — used as a stable tiebreaker. *)
  rule_type :
    [ `Regular
    | `Media of Css.Media.t
    | `Container of Css.Container.t
    | `Starting
    | `Supports of Css.Supports.t ];
  selector : Css.Selector.t;
  selector_kind : selector_kind;
  has_modifier_colon : bool;
  props : Css.declaration list;
  order : int * int;  (** [(priority, suborder)] from the utility definition. *)
  nested : Css.statement list;
  base_class : string option;
  merge_key : string option;
  not_order : int;
  variant_order : int;
      (** Non-zero for modifier-prefixed rules; they sort after base rules. *)
}

val classify_selector : Css.Selector.t -> selector_kind
(** [classify_selector sel] classifies a selector for ordering purposes. *)

(** {1 Sorting} *)

val compare_indexed_rules : indexed_rule -> indexed_rule -> int
(** [compare_indexed_rules r1 r2] is the total order over [indexed_rule] values
    that produces Tailwind v4 cascade order when used with [List.sort]. *)

(** {1 Debug} *)

val set_debug_compare : bool -> unit
(** [set_debug_compare true] enables comparison traces on stderr. *)

val debug_compare_enabled : unit -> bool
(** [debug_compare_enabled ()] is [true] when comparison tracing is active.
    Exposed so callers can guard their own debug output on the same flag. *)
