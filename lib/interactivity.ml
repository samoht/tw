(** Interactivity utilities for cursor, selection, and scroll behavior

    What's included:
    - Cursor styles, text selection, scroll behavior, scroll snap, resize.

    What's not:
    - Browser-specific interaction properties not surfaced in `Css`.

    Parsing contract (`of_string`):
    - Accepts tokens like ["cursor"; "pointer"], ["select"; "none"],
      ["scroll"; "smooth"], ["snap"; "x"], ["resize"; "y"], etc. Unknown tokens
      yield `Error (`Msg "Not an interactivity utility")`. *)

open Core
open Css

(** {1 Cursor Utilities} *)

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

(** {1 User Select Utilities} *)

let select_none = style "select-none" [ user_select None ]
let select_text = style "select-text" [ user_select Text ]
let select_all = style "select-all" [ user_select All ]
let select_auto = style "select-auto" [ user_select Auto ]

(** {1 Scroll Behavior Utilities} *)

let scroll_auto = style "scroll-auto" [ scroll_behavior Auto ]
let scroll_smooth = style "scroll-smooth" [ scroll_behavior Smooth ]

(** {1 Scroll Snap Utilities} *)

let snap_start = style "snap-start" [ scroll_snap_align Start ]
let snap_end = style "snap-end" [ scroll_snap_align End ]
let snap_center = style "snap-center" [ scroll_snap_align Center ]
let snap_none = style "snap-none" [ scroll_snap_type None ]
let snap_mandatory = style "snap-mandatory" [ scroll_snap_type Y_mandatory ]
let snap_proximity = style "snap-proximity" [ scroll_snap_type Y_proximity ]

(** {1 Resize Utilities} *)

let resize_none = style "resize-none" [ Css.resize None ]
let resize = style "resize" [ Css.resize Both ]
let resize_x = style "resize-x" [ Css.resize Horizontal ]
let resize_y = style "resize-y" [ Css.resize Vertical ]

(** {1 Parsing Functions} *)

let of_string = function
  | [ "cursor"; "auto" ] -> Ok cursor_auto
  | [ "cursor"; "default" ] -> Ok cursor_default
  | [ "cursor"; "pointer" ] -> Ok cursor_pointer
  | [ "cursor"; "wait" ] -> Ok cursor_wait
  | [ "cursor"; "move" ] -> Ok cursor_move
  | [ "cursor"; "not"; "allowed" ] -> Ok cursor_not_allowed
  | [ "cursor"; "text" ] -> Ok cursor_text
  | [ "cursor"; "crosshair" ] -> Ok cursor_crosshair
  | [ "cursor"; "help" ] -> Ok cursor_help
  | [ "cursor"; "grab" ] -> Ok cursor_grab
  | [ "cursor"; "grabbing" ] -> Ok cursor_grabbing
  | [ "select"; "none" ] -> Ok select_none
  | [ "select"; "text" ] -> Ok select_text
  | [ "select"; "all" ] -> Ok select_all
  | [ "select"; "auto" ] -> Ok select_auto
  | [ "scroll"; "auto" ] -> Ok scroll_auto
  | [ "scroll"; "smooth" ] -> Ok scroll_smooth
  | [ "snap"; "start" ] -> Ok snap_start
  | [ "snap"; "end" ] -> Ok snap_end
  | [ "snap"; "center" ] -> Ok snap_center
  | [ "snap"; "none" ] -> Ok snap_none
  | [ "snap"; "mandatory" ] -> Ok snap_mandatory
  | [ "snap"; "proximity" ] -> Ok snap_proximity
  | [ "resize"; "none" ] -> Ok resize_none
  | [ "resize" ] -> Ok resize
  | [ "resize"; "x" ] -> Ok resize_x
  | [ "resize"; "y" ] -> Ok resize_y
  | _ -> Error (`Msg "Not an interactivity utility")
