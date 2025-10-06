(** Form element utilities

    https://tailwindcss.com/docs/accent-color
    https://tailwindcss.com/docs/appearance *)

open Utility

(** {1 Form Variables} *)

val tw_shadow_var : (Css.shadow, [ `Property_default ]) Var.t
(** Variable for shadow effects used in form focus states *)

val tw_ring_inset_var : (string, [ `Channel ]) Var.t
(** Variable for ring inset mode *)

val tw_ring_offset_width_var : (string, [ `Channel ]) Var.t
(** Variable for ring offset width *)

val tw_ring_offset_color_var : (string, [ `Channel ]) Var.t
(** Variable for ring offset color *)

val tw_ring_color_var : (string, [ `Channel ]) Var.t
(** Variable for ring color *)

val tw_ring_offset_shadow_var : (string, [ `Channel ]) Var.t
(** Variable for ring offset shadow *)

val tw_ring_shadow_var : (string, [ `Channel ]) Var.t
(** Variable for ring shadow *)

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

  val of_class : string -> (t, [ `Msg of string ]) result
  val to_class : t -> string
  val suborder : t -> int
  val to_style : t -> Style.t
end
