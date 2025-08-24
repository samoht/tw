(** Animation and transition utilities *)

open Core

(** {1 Transition Utilities} *)

val transition_none : t
(** Disables transitions. *)

val transition_all : t
(** Enables transitions for all properties. *)

val transition_colors : t
(** Enables transitions for color properties. *)

val transition_opacity : t
(** Enables transitions for opacity. *)

val transition_shadow : t
(** Enables transitions for box-shadow. *)

val transition_transform : t
(** Enables transitions for transform. *)

(** {1 Animation Utilities} *)

val animate_none : t
(** Disables animations. *)

val animate_spin : t
(** Applies a spinning animation. *)

val animate_ping : t
(** Applies a ping/pulse animation. *)

val animate_pulse : t
(** Applies a subtle pulse animation. *)

val animate_bounce : t
(** Applies a bounce animation. *)

(** {1 Duration Utilities} *)

val duration : int -> t
(** [duration ms] sets animation/transition duration in milliseconds. *)

(** {1 Timing Function Utilities} *)

val ease_linear : t
(** Linear timing function. *)

val ease_in : t
(** Ease-in timing function. *)

val ease_out : t
(** Ease-out timing function. *)

val ease_in_out : t
(** Ease-in-out timing function. *)

(** {1 Delay Utilities} *)

val delay : int -> t
(** [delay ms] sets animation/transition delay in milliseconds. *)

(** {1 Parsing Functions} *)

val of_string : string list -> (t, [ `Msg of string ]) result
(** [of_string parts] parses an animation/transition utility from string parts.
*)
