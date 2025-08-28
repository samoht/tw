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

(* Axis and strictness helpers using CSS var for strictness *)
let _tw_scroll_snap_strictness_def, _tw_scroll_snap_strictness_var =
  Var.utility Var.Scroll_snap_strictness Proximity

let snap_x = style "snap-x" [ scroll_snap_type (Axis (X, None)) ]
let snap_y = style "snap-y" [ scroll_snap_type (Axis (Y, None)) ]
let snap_both = style "snap-both" [ scroll_snap_type (Axis (Both, None)) ]

let snap_mandatory =
  let def, _ = Var.utility Var.Scroll_snap_strictness Mandatory in
  style "snap-mandatory" [ def ]

let snap_proximity =
  let def, _ = Var.utility Var.Scroll_snap_strictness Proximity in
  style "snap-proximity" [ def ]

let snap_align_none = style "snap-align-none" [ scroll_snap_align None ]
let snap_normal = style "snap-normal" [ scroll_snap_stop Normal ]
let snap_always = style "snap-always" [ scroll_snap_stop Always ]

(** {1 Resize Utilities} *)

let resize_none = style "resize-none" [ Css.resize None ]
let resize = style "resize" [ Css.resize Both ]
let resize_x = style "resize-x" [ Css.resize Horizontal ]
let resize_y = style "resize-y" [ Css.resize Vertical ]

(** {1 Parsing Functions} *)

let rec of_string = function
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
  | [ "snap"; "x" ] -> Ok snap_x
  | [ "snap"; "y" ] -> Ok snap_y
  | [ "snap"; "both" ] -> Ok snap_both
  | [ "snap"; "mandatory" ] -> Ok snap_mandatory
  | [ "snap"; "proximity" ] -> Ok snap_proximity
  | [ "snap"; "align"; "none" ] -> Ok snap_align_none
  | [ "snap"; "normal" ] -> Ok snap_normal
  | [ "snap"; "always" ] -> Ok snap_always
  | [ "resize"; "none" ] -> Ok resize_none
  | [ "resize" ] -> Ok resize
  | [ "resize"; "x" ] -> Ok resize_x
  | [ "resize"; "y" ] -> Ok resize_y
  | [ "pointer"; "events"; "none" ] -> Ok pointer_events_none
  | [ "pointer"; "events"; "auto" ] -> Ok pointer_events_auto
  | [ "appearance"; "none" ] -> Ok appearance_none
  | [ "will"; "change"; "auto" ] -> Ok will_change_auto
  | [ "will"; "change"; "scroll" ] -> Ok will_change_scroll
  | [ "will"; "change"; "contents" ] -> Ok will_change_contents
  | [ "will"; "change"; "transform" ] -> Ok will_change_transform
  | _ -> Error (`Msg "Not an interactivity utility")

(* Additional utilities moved from Tw *)
and pointer_events_none = style "pointer-events-none" [ pointer_events None ]
and pointer_events_auto = style "pointer-events-auto" [ pointer_events Auto ]
and appearance_none = style "appearance-none" [ appearance None ]
and will_change_auto = style "will-change-auto" [ will_change "auto" ]

and will_change_scroll =
  style "will-change-scroll" [ will_change "scroll-position" ]

and will_change_contents =
  style "will-change-contents" [ will_change "contents" ]

and will_change_transform =
  style "will-change-transform" [ will_change "transform" ]
