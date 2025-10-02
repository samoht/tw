(** Cursor utilities

    @see <https://tailwindcss.com/docs/cursor> Tailwind CSS Cursor documentation
*)

open Utility
open Style
open Css

(** Local cursor utility type *)
type t =
  | Cursor_auto
  | Cursor_default
  | Cursor_pointer
  | Cursor_wait
  | Cursor_move
  | Cursor_not_allowed

(** Extensible variant for cursor utilities *)
type Utility.base += Cursor of t

(** Error helper *)
let err_not_utility = Error (`Msg "Not a cursor utility")

(** Helper functions returning Style.t *)
let cursor_auto' = style "cursor-auto" [ cursor Auto ]

let cursor_default' = style "cursor-default" [ cursor Default ]
let cursor_pointer' = style "cursor-pointer" [ cursor Pointer ]
let cursor_wait' = style "cursor-wait" [ cursor Wait ]
let cursor_move' = style "cursor-move" [ cursor Move ]
let cursor_not_allowed' = style "cursor-not-allowed" [ cursor Not_allowed ]

(** Convert cursor utility to style *)
let to_style = function
  | Cursor_auto -> cursor_auto'
  | Cursor_default -> cursor_default'
  | Cursor_pointer -> cursor_pointer'
  | Cursor_wait -> cursor_wait'
  | Cursor_move -> cursor_move'
  | Cursor_not_allowed -> cursor_not_allowed'

(** Suborder for cursor utilities *)
let suborder = function
  | Cursor_auto -> 0
  | Cursor_default -> 1
  | Cursor_pointer -> 2
  | Cursor_wait -> 3
  | Cursor_move -> 4
  | Cursor_not_allowed -> 5

(** Parse string parts to cursor utility *)
let of_string = function
  | [ "cursor"; "auto" ] -> Ok Cursor_auto
  | [ "cursor"; "default" ] -> Ok Cursor_default
  | [ "cursor"; "pointer" ] -> Ok Cursor_pointer
  | [ "cursor"; "wait" ] -> Ok Cursor_wait
  | [ "cursor"; "move" ] -> Ok Cursor_move
  | [ "cursor"; "not"; "allowed" ] -> Ok Cursor_not_allowed
  | _ -> err_not_utility

(** Priority for cursor utilities *)
let priority = 90

(** Typed handler for cursor utilities *)
let handler : t Utility.handler = { to_style; priority; suborder; of_string }

(** Wrapper functions for extensible variant *)
let wrap x = Cursor x

let unwrap = function Cursor x -> Some x | _ -> None

(** Public API returning Utility.t *)
let utility x = Utility.base (Cursor x)

let cursor_auto = utility Cursor_auto
let cursor_default = utility Cursor_default
let cursor_pointer = utility Cursor_pointer
let cursor_wait = utility Cursor_wait
let cursor_move = utility Cursor_move
let cursor_not_allowed = utility Cursor_not_allowed

(** Register the cursor utility handlers *)
let () = Utility.register ~wrap ~unwrap handler

module Handler = struct
  type nonrec t = t

  let of_string = of_string
  let suborder = suborder
  let to_style = to_style
  let order x = (priority, suborder x)
end
