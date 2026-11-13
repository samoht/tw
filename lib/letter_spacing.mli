(** Letter spacing utilities.

    https://tailwindcss.com/docs/letter-spacing *)

open Utility

val tracking_tighter : t
(** [tracking_tighter] decreases letter spacing. *)

val tracking_tight : t
(** [tracking_tight] slightly decreases letter spacing. *)

val tracking_normal : t
(** [tracking_normal] uses normal letter spacing. *)

val tracking_wide : t
(** [tracking_wide] slightly increases letter spacing. *)

val tracking_wider : t
(** [tracking_wider] increases letter spacing. *)

val tracking_widest : t
(** [tracking_widest] maximally increases letter spacing. *)

val tracking_var : Cascade.Css.length Var.channel
(** [tracking_var] is the [--tw-tracking] channel a letter spacing utility sets,
    which a font size utility reads back for the letter spacing its theme token
    declares. *)

module Handler : Utility.Handler
