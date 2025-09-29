(** String difference formatting. *)

type config = {
  max_width : int;  (** Maximum display width. *)
  short_threshold : int;  (** Threshold below which strings are shown inline. *)
  show_caret : bool;  (** Whether to display position indicators. *)
  indent : int;  (** Left margin for position indicators. *)
}
(** Configuration for difference formatting. *)

val default_config : config
(** Default configuration with [max_width = 60], [short_threshold = 30],
    [show_caret = true], and [indent = 0]. *)

type t =
  [ `Equal  (** Strings are identical. *)
  | `Diff_short of string * string  (** Both strings shown completely. *)
  | `Diff_medium of string * string * int
    (** Strings with difference position. *)
  | `Diff_long of string * string * int  (** Truncated windows with position. *)
  ]
(** The type for difference analysis results. *)

val diff : ?config:config -> expected:string -> string -> t
(** [diff ?config ~expected actual] analyzes differences between [expected] and
    [actual] strings, selecting an appropriate display mode based on their
    lengths and the configuration. *)

(** {1 Utilities} *)

val first_diff_pos : string -> string -> int option
(** [first_diff_pos s1 s2] is the position of the first differing character, or
    [None] if the strings are equal. *)

val truncate_middle : int -> string -> string
(** [truncate_middle max_len s] truncates [s] to at most [max_len] characters,
    preserving both the beginning and end with ellipsis in the middle if
    truncation occurs. *)

val pp_caret : ?indent:int -> Format.formatter -> int -> unit
(** [pp_caret ?indent fmt pos] formats a position indicator at column [pos] with
    optional left margin. *)
