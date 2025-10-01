(** Form element utilities *)

open Style

(** {1 Utility Type} *)

type utility =
  | Form_input
  | Form_textarea
  | Form_select
  | Form_checkbox
  | Form_radio

val to_style : utility -> t
(** [to_style utility] converts a form utility to a style. *)

val suborder : utility -> int
(** [suborder utility] returns the suborder for utility ordering. *)

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

val of_string : string list -> (utility, [ `Msg of string ]) result
(** [of_string parts] parses a form utility from string parts. *)
