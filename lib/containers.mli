(** Container query utilities for responsive design based on container size

    https://tailwindcss.com/docs/container
    https://tailwindcss.com/docs/container-queries *)

open Utility

(** {1 Container Type Utilities} *)

val container_type_size : t
(** [container_type_size] enables container queries based on both width and
    height. *)

val container_type_inline_size : t
(** [container_type_inline_size] enables container queries based on inline size
    (width in horizontal writing). *)

val container_type_normal : t
(** [container_type_normal] disables container queries (default). *)

val container_name_util : string -> t
(** [container_name_util name] creates a utility for setting a container name
    for targeted queries. *)

val container_name : string -> t
(** [container_name name] directly creates a style for setting a container name.
*)

(** {1 Container Query Modifiers} *)

val container_sm : t list -> t
(** [container_sm styles] applies [styles] when the container is at least 24rem
    (384px) wide. *)

val container_md : t list -> t
(** [container_md styles] applies [styles] when the container is at least 28rem
    (448px) wide. *)

val container_lg : t list -> t
(** [container_lg styles] applies [styles] when the container is at least 32rem
    (512px) wide. *)

val container_xl : t list -> t
(** [container_xl styles] applies [styles] when the container is at least 36rem
    (576px) wide. *)

val container_2xl : t list -> t
(** [container_2xl styles] applies [styles] when the container is at least 42rem
    (672px) wide. *)

val container : ?name:string -> int -> t list -> t
(** [container ?name min_width styles] applies styles when the named container
    (or nearest container if no name) is at least [min_width] pixels wide. *)

(** {1 Helper Functions} *)

val container_query_to_css_prefix : Style.container_query -> string
(** [container_query_to_css_prefix q] converts [q] to its CSS [@container] rule
    prefix. *)

val container_query_to_class_prefix : Style.container_query -> string
(** [container_query_to_class_prefix q] converts [q] to its class name prefix
    (e.g., "@sm"). *)

(** {1 Internal types} *)

module Handler : sig
  type t

  (** [of_string parts] parses string parts into a flex utility. For internal
      use by the Tw module. *)

  val of_class : string -> (t, [ `Msg of string ]) result
  (** [of_class class_name] parses a class name into a container utility. *)

  val to_class : t -> string
  (** [to_class t] returns the class name for the utility. *)

  val suborder : t -> int
  (** [suborder u] returns the ordering value for flex utility [u]. Used for
      deterministic CSS output ordering. *)

  val to_style : t -> Style.t
end
