(** Background and gradient utilities *)

open Style

(** {1 Utility Types} *)

type utility

type direction =
  | Bottom
  | Bottom_right
  | Right
  | Top_right
  | Top
  | Top_left
  | Left
  | Bottom_left

val of_string : string list -> (utility, [ `Msg of string ]) result
(** [of_string parts] parses a background utility from string parts.
    Returns an internal structured representation. *)

(** {1 Internal Conversion Functions} *)

val to_style : utility -> Style.t
(** [to_style u] converts a structured background utility to a style.
    For internal use by the Tw module. *)

val suborder : utility -> int
(** [suborder u] returns the ordering value for background utility [u].
    Used for deterministic CSS output ordering. *)

val bg_gradient_to : direction -> t
(** [bg_gradient_to dir] sets gradient direction. Prefer this typed variant over
    the fixed functions when composing logic. *)

val from_color : ?shade:int -> Color.color -> t
(** [from_color ?shade color] sets the gradient "from" color (start stop).
    [shade] selects a color shade (e.g., 50..900) when using Tailwind colors. *)

val via_color : ?shade:int -> Color.color -> t
(** [via_color ?shade color] sets the gradient "via" color (middle stop). *)

val to_color : ?shade:int -> Color.color -> t
(** [to_color ?shade color] sets the gradient "to" color (end stop). *)
