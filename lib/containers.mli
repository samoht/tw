(** Container query utilities for responsive design based on container size *)

open Style

(** {1 Utility Types} *)

type utility

val to_style : utility -> t
(** [to_style u] converts a structured container utility to a style. For
    internal use by the Tw module. *)

val suborder : utility -> int
(** [suborder u] returns the ordering value for container utility [u]. Used for
    deterministic CSS output ordering. *)

(** {1 Container Type Utilities} *)

val container_type_size : t
(** [container_type_size] enables container queries based on both width and
    height. *)

val container_type_inline_size : t
(** [container_type_inline_size] enables container queries based on inline size
    (width in horizontal writing). *)

val container_type_normal : t
(** [container_type_normal] disables container queries (default). *)

val container_name : string -> t
(** [container_name name] sets a container name for targeted queries. *)

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

val container_query_to_css_prefix : container_query -> string
(** [container_query_to_css_prefix q] converts [q] to its CSS [@container] rule
    prefix. *)

val container_query_to_class_prefix : container_query -> string
(** [container_query_to_class_prefix q] converts [q] to its class name prefix
    (e.g., "@sm"). *)

(** {1 Parsing Functions} *)

val of_string : string list -> (utility, [ `Msg of string ]) result
(** [of_string parts] parses a container utility from string parts. *)
