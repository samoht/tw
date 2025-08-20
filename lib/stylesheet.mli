(** CSS generation module for Tailwind DSL *)

open Core

val to_css : ?reset:bool -> t list -> Css.t
(** [to_css ~reset tw_classes] generates a complete CSS stylesheet from a list
    of Tailwind classes. If [reset] is true (default), includes CSS reset rules.
*)

val to_inline_style : t list -> string
(** [to_inline_style styles] converts Tailwind styles to an inline style
    attribute value. *)

val canonical_color_order : string -> int
(** [canonical_color_order color_name] returns the canonical ordering for color
    names, used to ensure consistent ordering in generated CSS. *)
