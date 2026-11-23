(** Text transform utilities.

    Tailwind documents them under
    {{:https://tailwindcss.com/docs/text-transform}text transform}. *)

open Utility

val uppercase : t
(** [uppercase] transforms text to uppercase. *)

val lowercase : t
(** [lowercase] transforms text to lowercase. *)

val capitalize : t
(** [capitalize] capitalizes words. *)

val normal_case : t
(** [normal_case] resets text transform. *)

module Handler : Utility.Handler
