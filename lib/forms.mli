(** Form element utilities

    https://tailwindcss.com/docs/accent-color
    https://tailwindcss.com/docs/appearance *)

open Utility

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

module Handler : sig
  type t

  val of_string : string list -> (t, [ `Msg of string ]) result
  val suborder : t -> int
  val to_style : t -> Style.t
  val order : t -> int * int
end
