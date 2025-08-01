(** Fmt - Lightweight formatting DSL for UI modules

    This module provides a lightweight string formatting DSL that avoids
    Printf/Format modules to keep js_of_ocaml bundle sizes small. The Format
    module is known to significantly increase JavaScript bundle sizes, so we use
    simple string concatenation instead. *)

type 'a t = 'a -> string
(** The type of formatters *)

(** {1 Basic Formatters} *)

val str : string list -> string
(** [str segments] concatenates string segments into a single string. *)

val sep : string -> string list -> string
(** [sep separator segments] joins segments with the given separator. *)

val lines : string list -> string
(** [lines segments] joins segments with newlines. *)

(** {1 Structural Formatters} *)

val kv : string -> string -> string
(** [kv key value] formats a key-value pair as "key: value". *)

val field : string -> string -> string
(** [field key value] formats a record field as "key = value;". *)

val braces : string -> string
(** [braces content] wraps content in braces: {content}. *)

val parens : string -> string
(** [parens content] wraps content in parentheses: (content). *)

val quote : string -> string
(** [quote s] wraps string in double quotes: "s". *)

val indent : int -> string -> string
(** [indent n s] indents string with n spaces. *)

(** {1 Type Formatters} *)

val option : ('a -> string) -> 'a option -> string
(** [option f opt] formats an option type. *)

val list : ?sep:string -> ('a -> string) -> 'a list -> string
(** [list ?sep f lst] formats a list with optional separator (default: ", "). *)

val record : (string * string) list -> string
(** [record fields] formats a record from a list of (key, value) pairs. *)

val string : string -> string
(** [string s] identity formatter for strings. *)

val bool : bool -> string
(** [bool b] formats a boolean. *)

val int : int -> string
(** [int n] formats an integer. *)

val float : float -> string
(** [float f] formats a float without trailing dots. *)
