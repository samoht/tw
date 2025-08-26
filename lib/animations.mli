(** Animation and transition utilities *)

open Core

(** {1 Transition Utilities} *)

val transition_none : t
(** [transition_none] disables transitions. *)

val transition_all : t
(** [transition_all] enables transitions for all properties. *)

val transition_colors : t
(** [transition_colors] enables transitions for color properties. *)

val transition_opacity : t
(** [transition_opacity] enables transitions for opacity. *)

val transition_shadow : t
(** [transition_shadow] enables transitions for box-shadow. *)

val transition_transform : t
(** [transition_transform] enables transitions for transform. *)

(** {1 Animation Utilities} *)

val animate_none : t
(** [animate_none] disables animations. *)

val animate_spin : t
(** [animate_spin] applies a spinning animation. *)

val animate_ping : t
(** [animate_ping] applies a ping/pulse animation. *)

val animate_pulse : t
(** [animate_pulse] applies a subtle pulse animation. *)

val animate_bounce : t
(** [animate_bounce] applies a bounce animation. *)

(** {1 Duration Utilities} *)

val duration : int -> t
(** [duration ms] sets animation/transition duration in milliseconds. *)

(** {1 Timing Function Utilities} *)

val ease_linear : t
(** [ease_linear] uses a linear timing function. *)

val ease_in : t
(** [ease_in] uses an ease-in timing function. *)

val ease_out : t
(** [ease_out] uses an ease-out timing function. *)

val ease_in_out : t
(** [ease_in_out] uses an ease-in-out timing function. *)

(** {1 Delay Utilities} *)

val delay : int -> t
(** [delay ms] sets animation/transition delay in milliseconds. *)

(** {1 Parsing Functions} *)

val of_string : string list -> (t, [ `Msg of string ]) result
(** [of_string parts] parses an animation/transition utility from string parts.
*)
