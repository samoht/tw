(** Form element utilities *)

open Core

(** {1 Form Input Utilities} *)

val form_input : t
(** [form_input] applies basic form input styles. *)

val form_textarea : t
(** [form_textarea] applies textarea form element styles. *)

val form_select : t
(** [form_select] applies select dropdown styles with a custom arrow. *)

val form_checkbox : t
(** [form_checkbox] applies checkbox input styles. *)

val form_radio : t
(** [form_radio] applies radio button input styles. *)

(** {1 Parsing Functions} *)

val of_string : string list -> (t, [ `Msg of string ]) result
(** [of_string parts] parses a form utility from string parts. *)
