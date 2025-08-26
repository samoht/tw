(** Container query utilities for responsive design based on container size *)

open Core

(** {1 Container Type Utilities} *)

val container_type_size : t
(** Enables container queries based on both width and height. *)

val container_type_inline_size : t
(** Enables container queries based on inline size (width in horizontal
    writing). *)

val container_type_normal : t
(** Disables container queries (default). *)

val container_name : string -> t
(** [container_name name] sets a container name for targeted queries. *)

(** {1 Container Query Modifiers} *)

val on_container_sm : t list -> t
(** [on_container_sm styles] applies [styles] when the container is at least
    24rem (384px) wide. *)

val on_container_md : t list -> t
(** [on_container_md styles] applies [styles] when the container is at least
    28rem (448px) wide. *)

val on_container_lg : t list -> t
(** [on_container_lg styles] applies [styles] when the container is at least
    32rem (512px) wide. *)

val on_container_xl : t list -> t
(** [on_container_xl styles] applies [styles] when the container is at least
    36rem (576px) wide. *)

val on_container_2xl : t list -> t
(** [on_container_2xl styles] applies [styles] when the container is at least
    42rem (672px) wide. *)

val on_container : ?name:string -> int -> t list -> t
(** [on_container ?name min_width styles] applies styles when the named
    container (or nearest container if no name) is at least min_width pixels
    wide. *)

(** {1 Helper Functions} *)

val container_query_to_css_prefix : container_query -> string
(** [container_query_to_css_prefix q] converts [q] to its CSS @container rule prefix. *)

val container_query_to_class_prefix : container_query -> string
(** [container_query_to_class_prefix q] converts [q] to its class name prefix
    (e.g., "@sm"). *)

(** {1 Parsing Functions} *)

val of_string : string list -> (t, [ `Msg of string ]) result
(** [of_string parts] parses a container utility from string parts. *)
