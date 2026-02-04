(** Cursor utilities

    @see <https://tailwindcss.com/docs/cursor> Tailwind CSS Cursor documentation
*)

open Utility

(** {1 Cursor Utilities} *)

val cursor_alias : t
(** [cursor_alias] sets cursor to alias. *)

val cursor_all_scroll : t
(** [cursor_all_scroll] sets cursor to all-scroll. *)

val cursor_auto : t
(** [cursor_auto] sets cursor to auto (browser determines cursor). *)

val cursor_cell : t
(** [cursor_cell] sets cursor to cell. *)

val cursor_col_resize : t
(** [cursor_col_resize] sets cursor to column resize. *)

val cursor_context_menu : t
(** [cursor_context_menu] sets cursor to context menu. *)

val cursor_copy : t
(** [cursor_copy] sets cursor to copy. *)

val cursor_crosshair : t
(** [cursor_crosshair] sets cursor to crosshair. *)

val cursor_default : t
(** [cursor_default] sets cursor to default arrow. *)

val cursor_e_resize : t
(** [cursor_e_resize] sets cursor to east resize. *)

val cursor_ew_resize : t
(** [cursor_ew_resize] sets cursor to east-west resize. *)

val cursor_grab : t
(** [cursor_grab] sets cursor to grab/open hand. *)

val cursor_grabbing : t
(** [cursor_grabbing] sets cursor to grabbing/closed hand. *)

val cursor_help : t
(** [cursor_help] sets cursor to help cursor. *)

val cursor_move : t
(** [cursor_move] sets cursor to move indicator. *)

val cursor_n_resize : t
(** [cursor_n_resize] sets cursor to north resize. *)

val cursor_ne_resize : t
(** [cursor_ne_resize] sets cursor to northeast resize. *)

val cursor_nesw_resize : t
(** [cursor_nesw_resize] sets cursor to northeast-southwest resize. *)

val cursor_no_drop : t
(** [cursor_no_drop] sets cursor to no-drop. *)

val cursor_none : t
(** [cursor_none] sets cursor to none. *)

val cursor_not_allowed : t
(** [cursor_not_allowed] sets cursor to not-allowed indicator. *)

val cursor_ns_resize : t
(** [cursor_ns_resize] sets cursor to north-south resize. *)

val cursor_nw_resize : t
(** [cursor_nw_resize] sets cursor to northwest resize. *)

val cursor_nwse_resize : t
(** [cursor_nwse_resize] sets cursor to northwest-southeast resize. *)

val cursor_pointer : t
(** [cursor_pointer] sets cursor to pointing hand. *)

val cursor_progress : t
(** [cursor_progress] sets cursor to progress. *)

val cursor_row_resize : t
(** [cursor_row_resize] sets cursor to row resize. *)

val cursor_s_resize : t
(** [cursor_s_resize] sets cursor to south resize. *)

val cursor_se_resize : t
(** [cursor_se_resize] sets cursor to southeast resize. *)

val cursor_sw_resize : t
(** [cursor_sw_resize] sets cursor to southwest resize. *)

val cursor_text : t
(** [cursor_text] sets cursor to text/I-beam cursor. *)

val cursor_vertical_text : t
(** [cursor_vertical_text] sets cursor to vertical text. *)

val cursor_w_resize : t
(** [cursor_w_resize] sets cursor to west resize. *)

val cursor_wait : t
(** [cursor_wait] sets cursor to waiting/busy indicator. *)

val cursor_zoom_in : t
(** [cursor_zoom_in] sets cursor to zoom in. *)

val cursor_zoom_out : t
(** [cursor_zoom_out] sets cursor to zoom out. *)

module Handler : Utility.Handler
