(** Animation utilities

    https://tailwindcss.com/docs/animation *)

open Utility

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

module Handler : Utility.Handler
