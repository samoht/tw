(** CSS rule cascade sort order.

    Provides the [indexed_rule] type and [compare_indexed_rules], the comparison
    function that produces Tailwind v4 cascade order when used with [List.sort].
*)

open Cascade

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

type rule_type =
  [ `Regular
  | `Media of Css.Media.t
  | `Container of Css.Container.t
  | `Starting
  | `Supports of Css.Supports.t ]

type indexed_rule = {
  index : int;  (** Source position — used as a stable tiebreaker. *)
  rule_type : rule_type;
  selector : Css.Selector.t;
  selector_str : string;
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
  variant_key : string * int;
      (** Precomputed [(variant prefix, effective inner order)], built with
          {!variant_sort_key}, so [compare_indexed_rules] does not recompute it
          per comparison. *)
  media_group_sub : int * float;
      (** Precomputed [(group, subkey)] for the rule's media, so a comparison
          never re-derives it with [Css.Media.kind]. *)
  media_key : Css.Media.key option;
      (** Precomputed sort key of the rule's own media condition (the [`Media]
          case of {!field-rule_type}), [None] otherwise. Comparisons use this
          instead of re-serializing the query on every call. *)
  nested_media_key : Css.Media.key option;
      (** Precomputed sort key of a single nested media condition, used to order
          stacked variants such as [min-sm:max-xl]. *)
}

val classify_selector : Css.Selector.t -> selector_kind
(** [classify_selector sel] classifies a selector for ordering purposes. *)

val variant_sort_key : string option -> Css.statement list -> string * int
(** [variant_sort_key base_class nested] is the
    [(variant prefix, effective inner variant order)] pair stored in
    {!field-variant_key}. *)

val precompute_media_keys :
  rule_type ->
  Css.statement list ->
  (int * float) * Css.Media.key option * Css.Media.key option
(** [precompute_media_keys rule_type nested] is the
    [(media_group_sub, media_key, nested_media_key)] triple stored on an
    {!type-indexed_rule}, computed once so comparisons never re-derive a media
    [kind] or re-serialize a query. *)

(** {1 Sorting} *)

val compare_indexed_rules : indexed_rule -> indexed_rule -> int
(** [compare_indexed_rules r1 r2] is the total order over {!type-indexed_rule}
    values that produces Tailwind v4 cascade order when used with [List.sort].
*)

(** {1 Debug} *)

val set_debug_compare : bool -> unit
(** [set_debug_compare true] enables comparison traces on stderr. *)

val debug_compare_enabled : unit -> bool
(** [debug_compare_enabled ()] is [true] when comparison tracing is active.
    Exposed so callers can guard their own debug output on the same flag. *)
