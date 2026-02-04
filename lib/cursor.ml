(** Cursor utilities

    @see <https://tailwindcss.com/docs/cursor> Tailwind CSS Cursor documentation
*)

module Handler = struct
  open Style
  open Css

  type t =
    | Cursor_alias
    | Cursor_all_scroll
    | Cursor_auto
    | Cursor_cell
    | Cursor_col_resize
    | Cursor_context_menu
    | Cursor_copy
    | Cursor_crosshair
    | Cursor_default
    | Cursor_e_resize
    | Cursor_ew_resize
    | Cursor_grab
    | Cursor_grabbing
    | Cursor_help
    | Cursor_move
    | Cursor_n_resize
    | Cursor_ne_resize
    | Cursor_nesw_resize
    | Cursor_no_drop
    | Cursor_none
    | Cursor_not_allowed
    | Cursor_ns_resize
    | Cursor_nw_resize
    | Cursor_nwse_resize
    | Cursor_pointer
    | Cursor_progress
    | Cursor_row_resize
    | Cursor_s_resize
    | Cursor_se_resize
    | Cursor_sw_resize
    | Cursor_text
    | Cursor_vertical_text
    | Cursor_w_resize
    | Cursor_wait
    | Cursor_zoom_in
    | Cursor_zoom_out

  type Utility.base += Self of t

  let name = "cursor"
  let priority = 11

  let to_class = function
    | Cursor_alias -> "cursor-alias"
    | Cursor_all_scroll -> "cursor-all-scroll"
    | Cursor_auto -> "cursor-auto"
    | Cursor_cell -> "cursor-cell"
    | Cursor_col_resize -> "cursor-col-resize"
    | Cursor_context_menu -> "cursor-context-menu"
    | Cursor_copy -> "cursor-copy"
    | Cursor_crosshair -> "cursor-crosshair"
    | Cursor_default -> "cursor-default"
    | Cursor_e_resize -> "cursor-e-resize"
    | Cursor_ew_resize -> "cursor-ew-resize"
    | Cursor_grab -> "cursor-grab"
    | Cursor_grabbing -> "cursor-grabbing"
    | Cursor_help -> "cursor-help"
    | Cursor_move -> "cursor-move"
    | Cursor_n_resize -> "cursor-n-resize"
    | Cursor_ne_resize -> "cursor-ne-resize"
    | Cursor_nesw_resize -> "cursor-nesw-resize"
    | Cursor_no_drop -> "cursor-no-drop"
    | Cursor_none -> "cursor-none"
    | Cursor_not_allowed -> "cursor-not-allowed"
    | Cursor_ns_resize -> "cursor-ns-resize"
    | Cursor_nw_resize -> "cursor-nw-resize"
    | Cursor_nwse_resize -> "cursor-nwse-resize"
    | Cursor_pointer -> "cursor-pointer"
    | Cursor_progress -> "cursor-progress"
    | Cursor_row_resize -> "cursor-row-resize"
    | Cursor_s_resize -> "cursor-s-resize"
    | Cursor_se_resize -> "cursor-se-resize"
    | Cursor_sw_resize -> "cursor-sw-resize"
    | Cursor_text -> "cursor-text"
    | Cursor_vertical_text -> "cursor-vertical-text"
    | Cursor_w_resize -> "cursor-w-resize"
    | Cursor_wait -> "cursor-wait"
    | Cursor_zoom_in -> "cursor-zoom-in"
    | Cursor_zoom_out -> "cursor-zoom-out"

  let to_style = function
    | Cursor_alias -> style [ cursor Alias ]
    | Cursor_all_scroll -> style [ cursor All_scroll ]
    | Cursor_auto -> style [ cursor Auto ]
    | Cursor_cell -> style [ cursor Cell ]
    | Cursor_col_resize -> style [ cursor Col_resize ]
    | Cursor_context_menu -> style [ cursor Context_menu ]
    | Cursor_copy -> style [ cursor Copy ]
    | Cursor_crosshair -> style [ cursor Crosshair ]
    | Cursor_default -> style [ cursor Default ]
    | Cursor_e_resize -> style [ cursor E_resize ]
    | Cursor_ew_resize -> style [ cursor Ew_resize ]
    | Cursor_grab -> style [ cursor Grab ]
    | Cursor_grabbing -> style [ cursor Grabbing ]
    | Cursor_help -> style [ cursor Help ]
    | Cursor_move -> style [ cursor Move ]
    | Cursor_n_resize -> style [ cursor N_resize ]
    | Cursor_ne_resize -> style [ cursor Ne_resize ]
    | Cursor_nesw_resize -> style [ cursor Nesw_resize ]
    | Cursor_no_drop -> style [ cursor No_drop ]
    | Cursor_none -> style [ cursor None ]
    | Cursor_not_allowed -> style [ cursor Not_allowed ]
    | Cursor_ns_resize -> style [ cursor Ns_resize ]
    | Cursor_nw_resize -> style [ cursor Nw_resize ]
    | Cursor_nwse_resize -> style [ cursor Nwse_resize ]
    | Cursor_pointer -> style [ cursor Pointer ]
    | Cursor_progress -> style [ cursor Progress ]
    | Cursor_row_resize -> style [ cursor Row_resize ]
    | Cursor_s_resize -> style [ cursor S_resize ]
    | Cursor_se_resize -> style [ cursor Se_resize ]
    | Cursor_sw_resize -> style [ cursor Sw_resize ]
    | Cursor_text -> style [ cursor Text ]
    | Cursor_vertical_text -> style [ cursor Vertical_text ]
    | Cursor_w_resize -> style [ cursor W_resize ]
    | Cursor_wait -> style [ cursor Wait ]
    | Cursor_zoom_in -> style [ cursor Zoom_in ]
    | Cursor_zoom_out -> style [ cursor Zoom_out ]

  let suborder = function
    (* Alphabetical order *)
    | Cursor_alias -> 0
    | Cursor_all_scroll -> 1
    | Cursor_auto -> 2
    | Cursor_cell -> 3
    | Cursor_col_resize -> 4
    | Cursor_context_menu -> 5
    | Cursor_copy -> 6
    | Cursor_crosshair -> 7
    | Cursor_default -> 8
    | Cursor_e_resize -> 9
    | Cursor_ew_resize -> 10
    | Cursor_grab -> 11
    | Cursor_grabbing -> 12
    | Cursor_help -> 13
    | Cursor_move -> 14
    | Cursor_n_resize -> 15
    | Cursor_ne_resize -> 16
    | Cursor_nesw_resize -> 17
    | Cursor_no_drop -> 18
    | Cursor_none -> 19
    | Cursor_not_allowed -> 20
    | Cursor_ns_resize -> 21
    | Cursor_nw_resize -> 22
    | Cursor_nwse_resize -> 23
    | Cursor_pointer -> 24
    | Cursor_progress -> 25
    | Cursor_row_resize -> 26
    | Cursor_s_resize -> 27
    | Cursor_se_resize -> 28
    | Cursor_sw_resize -> 29
    | Cursor_text -> 30
    | Cursor_vertical_text -> 31
    | Cursor_w_resize -> 32
    | Cursor_wait -> 33
    | Cursor_zoom_in -> 34
    | Cursor_zoom_out -> 35

  let of_class = function
    | "cursor-alias" -> Ok Cursor_alias
    | "cursor-all-scroll" -> Ok Cursor_all_scroll
    | "cursor-auto" -> Ok Cursor_auto
    | "cursor-cell" -> Ok Cursor_cell
    | "cursor-col-resize" -> Ok Cursor_col_resize
    | "cursor-context-menu" -> Ok Cursor_context_menu
    | "cursor-copy" -> Ok Cursor_copy
    | "cursor-crosshair" -> Ok Cursor_crosshair
    | "cursor-default" -> Ok Cursor_default
    | "cursor-e-resize" -> Ok Cursor_e_resize
    | "cursor-ew-resize" -> Ok Cursor_ew_resize
    | "cursor-grab" -> Ok Cursor_grab
    | "cursor-grabbing" -> Ok Cursor_grabbing
    | "cursor-help" -> Ok Cursor_help
    | "cursor-move" -> Ok Cursor_move
    | "cursor-n-resize" -> Ok Cursor_n_resize
    | "cursor-ne-resize" -> Ok Cursor_ne_resize
    | "cursor-nesw-resize" -> Ok Cursor_nesw_resize
    | "cursor-no-drop" -> Ok Cursor_no_drop
    | "cursor-none" -> Ok Cursor_none
    | "cursor-not-allowed" -> Ok Cursor_not_allowed
    | "cursor-ns-resize" -> Ok Cursor_ns_resize
    | "cursor-nw-resize" -> Ok Cursor_nw_resize
    | "cursor-nwse-resize" -> Ok Cursor_nwse_resize
    | "cursor-pointer" -> Ok Cursor_pointer
    | "cursor-progress" -> Ok Cursor_progress
    | "cursor-row-resize" -> Ok Cursor_row_resize
    | "cursor-s-resize" -> Ok Cursor_s_resize
    | "cursor-se-resize" -> Ok Cursor_se_resize
    | "cursor-sw-resize" -> Ok Cursor_sw_resize
    | "cursor-text" -> Ok Cursor_text
    | "cursor-vertical-text" -> Ok Cursor_vertical_text
    | "cursor-w-resize" -> Ok Cursor_w_resize
    | "cursor-wait" -> Ok Cursor_wait
    | "cursor-zoom-in" -> Ok Cursor_zoom_in
    | "cursor-zoom-out" -> Ok Cursor_zoom_out
    | _ -> Error (`Msg "Not a cursor utility")
end

open Handler

(** Register the cursor utility handlers *)
let () = Utility.register (module Handler)

(** Public API returning Utility.t *)
let utility x = Utility.base (Self x)

let cursor_alias = utility Cursor_alias
let cursor_all_scroll = utility Cursor_all_scroll
let cursor_auto = utility Cursor_auto
let cursor_cell = utility Cursor_cell
let cursor_col_resize = utility Cursor_col_resize
let cursor_context_menu = utility Cursor_context_menu
let cursor_copy = utility Cursor_copy
let cursor_crosshair = utility Cursor_crosshair
let cursor_default = utility Cursor_default
let cursor_e_resize = utility Cursor_e_resize
let cursor_ew_resize = utility Cursor_ew_resize
let cursor_grab = utility Cursor_grab
let cursor_grabbing = utility Cursor_grabbing
let cursor_help = utility Cursor_help
let cursor_move = utility Cursor_move
let cursor_n_resize = utility Cursor_n_resize
let cursor_ne_resize = utility Cursor_ne_resize
let cursor_nesw_resize = utility Cursor_nesw_resize
let cursor_no_drop = utility Cursor_no_drop
let cursor_none = utility Cursor_none
let cursor_not_allowed = utility Cursor_not_allowed
let cursor_ns_resize = utility Cursor_ns_resize
let cursor_nw_resize = utility Cursor_nw_resize
let cursor_nwse_resize = utility Cursor_nwse_resize
let cursor_pointer = utility Cursor_pointer
let cursor_progress = utility Cursor_progress
let cursor_row_resize = utility Cursor_row_resize
let cursor_s_resize = utility Cursor_s_resize
let cursor_se_resize = utility Cursor_se_resize
let cursor_sw_resize = utility Cursor_sw_resize
let cursor_text = utility Cursor_text
let cursor_vertical_text = utility Cursor_vertical_text
let cursor_w_resize = utility Cursor_w_resize
let cursor_wait = utility Cursor_wait
let cursor_zoom_in = utility Cursor_zoom_in
let cursor_zoom_out = utility Cursor_zoom_out
