(** Gap and space-between utilities

    https://tailwindcss.com/docs/gap https://tailwindcss.com/docs/space *)

open Utility

(** {1 Gap Utilities} *)

val gap : int -> t
(** [gap n] sets gap to [n] × 0.25rem. *)

val gap_x : int -> t
(** [gap_x n] sets column-gap to [n] × 0.25rem. *)

val gap_y : int -> t
(** [gap_y n] sets row-gap to [n] × 0.25rem. *)

val gap' : Style.spacing -> Style.t
(** [gap' s] sets gap using typed spacing [s]. *)

val gap_x' : Style.spacing -> Style.t
(** [gap_x' s] sets column-gap using typed spacing [s]. *)

val gap_y' : Style.spacing -> Style.t
(** [gap_y' s] sets row-gap using typed spacing [s]. *)

(** {1 Special Gap Values} *)

val gap_px : t
(** [gap_px] sets gap to 1px. *)

val gap_full : t
(** [gap_full] sets gap to 100%. *)

val gap_x_px : t
(** [gap_x_px] sets column-gap to 1px. *)

val gap_x_full : t
(** [gap_x_full] sets column-gap to 100%. *)

val gap_y_px : t
(** [gap_y_px] sets row-gap to 1px. *)

val gap_y_full : t
(** [gap_y_full] sets row-gap to 100%. *)

(** {1 Space Between Utilities} *)

val space_x : int -> t
(** [space_x n] creates horizontal space between child elements. *)

val space_y : int -> t
(** [space_y n] creates vertical space between child elements. *)

module Handler : sig
  type t

  val of_string : string list -> (t, [ `Msg of string ]) result
  val suborder : t -> int
  val to_style : t -> Style.t
  val order : t -> int * int
end
