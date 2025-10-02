(** Cursor utilities

    @see <https://tailwindcss.com/docs/cursor> Tailwind CSS Cursor documentation
*)

module Handler = struct
  open Style
  open Css

  type t =
    | Cursor_auto
    | Cursor_default
    | Cursor_pointer
    | Cursor_wait
    | Cursor_move
    | Cursor_not_allowed
    | Cursor_text
    | Cursor_crosshair
    | Cursor_help
    | Cursor_grab
    | Cursor_grabbing

  type Utility.base += Self of t

  let priority = 90
  let cursor_auto = style "cursor-auto" [ cursor Auto ]
  let cursor_default = style "cursor-default" [ cursor Default ]
  let cursor_pointer = style "cursor-pointer" [ cursor Pointer ]
  let cursor_wait = style "cursor-wait" [ cursor Wait ]
  let cursor_move = style "cursor-move" [ cursor Move ]
  let cursor_not_allowed = style "cursor-not-allowed" [ cursor Not_allowed ]
  let cursor_text = style "cursor-text" [ cursor Text ]
  let cursor_crosshair = style "cursor-crosshair" [ cursor Crosshair ]
  let cursor_help = style "cursor-help" [ cursor Help ]
  let cursor_grab = style "cursor-grab" [ cursor Grab ]
  let cursor_grabbing = style "cursor-grabbing" [ cursor Grabbing ]

  let to_style = function
    | Cursor_auto -> cursor_auto
    | Cursor_default -> cursor_default
    | Cursor_pointer -> cursor_pointer
    | Cursor_wait -> cursor_wait
    | Cursor_move -> cursor_move
    | Cursor_not_allowed -> cursor_not_allowed
    | Cursor_text -> cursor_text
    | Cursor_crosshair -> cursor_crosshair
    | Cursor_help -> cursor_help
    | Cursor_grab -> cursor_grab
    | Cursor_grabbing -> cursor_grabbing

  let suborder = function
    | Cursor_auto -> 0
    | Cursor_default -> 1
    | Cursor_pointer -> 2
    | Cursor_wait -> 3
    | Cursor_move -> 4
    | Cursor_not_allowed -> 5
    | Cursor_text -> 6
    | Cursor_crosshair -> 7
    | Cursor_help -> 8
    | Cursor_grab -> 9
    | Cursor_grabbing -> 10

  let of_string = function
    | [ "cursor"; "auto" ] -> Ok Cursor_auto
    | [ "cursor"; "default" ] -> Ok Cursor_default
    | [ "cursor"; "pointer" ] -> Ok Cursor_pointer
    | [ "cursor"; "wait" ] -> Ok Cursor_wait
    | [ "cursor"; "move" ] -> Ok Cursor_move
    | [ "cursor"; "not"; "allowed" ] -> Ok Cursor_not_allowed
    | [ "cursor"; "text" ] -> Ok Cursor_text
    | [ "cursor"; "crosshair" ] -> Ok Cursor_crosshair
    | [ "cursor"; "help" ] -> Ok Cursor_help
    | [ "cursor"; "grab" ] -> Ok Cursor_grab
    | [ "cursor"; "grabbing" ] -> Ok Cursor_grabbing
    | _ -> Error (`Msg "Not a cursor utility")
end

open Handler

(** Register the cursor utility handlers *)
let () = Utility.register (module Handler)

(** Public API returning Utility.t *)
let utility x = Utility.base (Self x)

let cursor_auto = utility Cursor_auto
let cursor_default = utility Cursor_default
let cursor_pointer = utility Cursor_pointer
let cursor_wait = utility Cursor_wait
let cursor_move = utility Cursor_move
let cursor_not_allowed = utility Cursor_not_allowed
let cursor_text = utility Cursor_text
let cursor_crosshair = utility Cursor_crosshair
let cursor_help = utility Cursor_help
let cursor_grab = utility Cursor_grab
let cursor_grabbing = utility Cursor_grabbing
