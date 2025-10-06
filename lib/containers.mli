(** Container query utilities for responsive design based on container size

    https://tailwindcss.com/docs/container
    https://tailwindcss.com/docs/container-queries *)

open Utility

(** {1 Container Type Utilities} *)

val container : t
(** [container] enables container queries based on inline-size (equivalent to
    [@container] in Tailwind v4). Sets [container-type: inline-size]. *)

val container_normal : t
(** [container_normal] disables container queries (equivalent to
    [@container-normal] in Tailwind v4). Sets [container-type: normal]. *)

val container_named : string -> t
(** [container_named name] creates a named container (equivalent to
    [@container/name] in Tailwind v4). Sets [container-type: inline-size] and
    [container-name: name]. *)

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

val container_query : ?name:string -> int -> t list -> t
(** [container_query ?name min_width styles] applies styles when the named
    container (or nearest container if no name) is at least [min_width] pixels
    wide. *)

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
