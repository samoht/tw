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

module Handler : sig
  type t

  val of_string : string list -> (t, [ `Msg of string ]) result
  val suborder : t -> int
  val to_style : t -> Style.t
  val order : t -> int * int
end
