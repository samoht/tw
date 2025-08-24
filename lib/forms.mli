(** Form element utilities *)

open Core

(** {1 Form Input Utilities} *)

val form_input : t
(** Basic form input styles. *)

val form_textarea : t
(** Textarea form element styles. *)

val form_select : t
(** Select dropdown styles with custom arrow. *)

val form_checkbox : t
(** Checkbox input styles. *)

val form_radio : t
(** Radio button input styles. *)

(** {1 Parsing Functions} *)

val of_string : string list -> (t, [ `Msg of string ]) result
(** [of_string parts] parses a form utility from string parts. *)
