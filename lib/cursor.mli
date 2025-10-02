(** Cursor utilities

    @see <https://tailwindcss.com/docs/cursor> Tailwind CSS Cursor documentation
*)

open Utility

(** {1 Cursor Utilities} *)

val cursor_auto : t
(** [cursor_auto] sets cursor to auto (browser determines cursor). *)

val cursor_default : t
(** [cursor_default] sets cursor to default arrow. *)

val cursor_pointer : t
(** [cursor_pointer] sets cursor to pointing hand. *)

val cursor_wait : t
(** [cursor_wait] sets cursor to waiting/busy indicator. *)

val cursor_move : t
(** [cursor_move] sets cursor to move indicator. *)

val cursor_not_allowed : t
(** [cursor_not_allowed] sets cursor to not-allowed indicator. *)

val cursor_text : t
(** [cursor_text] sets cursor to text/I-beam cursor. *)

val cursor_crosshair : t
(** [cursor_crosshair] sets cursor to crosshair. *)

val cursor_help : t
(** [cursor_help] sets cursor to help cursor. *)

val cursor_grab : t
(** [cursor_grab] sets cursor to grab/open hand. *)

val cursor_grabbing : t
(** [cursor_grabbing] sets cursor to grabbing/closed hand. *)

module Handler : sig
  type t

  val of_string : string list -> (t, [ `Msg of string ]) result
  val suborder : t -> int
  val to_style : t -> Style.t
  val order : t -> int * int
end
