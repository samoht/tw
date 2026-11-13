(** Box shadow, inset shadow and opacity utilities, and the ring and shadow
    variables every [box-shadow] utility composes.

    See {{:https://tailwindcss.com/docs/box-shadow}box-shadow} and
    {{:https://tailwindcss.com/docs/opacity}opacity}. *)

open Cascade
open Utility

val order : base -> (int * int) option
(** [order u] returns the priority and suborder for the given effect utility. *)

module Handler : Utility.Handler

(** {1 Shadow Utilities} *)

val shadow_none : t
(** [shadow_none] applies no shadow. *)

val shadow_2xs : t
(** [shadow_2xs] applies the smallest shadow. *)

val shadow_xs : t
(** [shadow_xs] applies an extra-small shadow. *)

val shadow_sm : t
(** [shadow_sm] applies a small shadow. *)

val shadow : t
(** [shadow] applies the default shadow. *)

val shadow_md : t
(** [shadow_md] applies a medium shadow. *)

val shadow_lg : t
(** [shadow_lg] applies a large shadow. *)

val shadow_xl : t
(** [shadow_xl] applies an extra-large shadow. *)

val shadow_2xl : t
(** [shadow_2xl] applies a 2× extra-large shadow. *)

val shadow_inner : t
(** [shadow_inner] applies an inner (inset) shadow. *)

(** {1 Inset Shadow Utilities} *)

val inset_shadow_none : t
(** [inset_shadow_none] removes the inset shadow. *)

val inset_shadow_2xs : t
(** [inset_shadow_2xs] applies the smallest inset shadow ([inset 0 1px]). *)

val inset_shadow_xs : t
(** [inset_shadow_xs] applies an extra-small inset shadow ([inset 0 1px 1px]).
*)

val inset_shadow_sm : t
(** [inset_shadow_sm] applies a small inset shadow ([inset 0 2px 4px]). *)

val inset_shadow : t
(** [inset_shadow] applies the inset shadow defined by the [--inset-shadow]
    theme token. It has no default value: it is only a valid class when the
    active theme defines that token. *)

(** {1 Opacity Utilities} *)

val opacity : int -> t
(** [opacity n] sets opacity to n%. *)

(** {1 Ring/Shadow Variables}

    Every [box-shadow] utility sets one of these variables and composes them
    all. They are declared once here and exported for the {!Ring} and Forms
    utilities. *)

val shadow_var : Css.shadow Var.property_default
(** [shadow_var] is the --tw-shadow variable. *)

val inset_shadow_var : Css.shadow Var.property_default
(** [inset_shadow_var] is the --tw-inset-shadow variable. *)

val ring_inset_var : Css.Variables.custom_value Var.channel
(** [ring_inset_var] is the --tw-ring-inset variable. *)

val ring_offset_width_var : Css.length Var.property_default
(** [ring_offset_width_var] is the --tw-ring-offset-width variable. *)

val ring_offset_color_var : Css.color Var.property_default
(** [ring_offset_color_var] is the --tw-ring-offset-color variable. *)

val ring_color_var : Css.color Var.channel
(** [ring_color_var] is the --tw-ring-color variable. *)

val ring_offset_shadow_var : Css.shadow Var.property_default
(** [ring_offset_shadow_var] is the --tw-ring-offset-shadow variable. *)

val ring_shadow_var : Css.shadow Var.property_default
(** [ring_shadow_var] is the --tw-ring-shadow variable. *)

val inset_ring_color_var : Css.color Var.channel
(** [inset_ring_color_var] is the --tw-inset-ring-color variable. *)

val inset_ring_shadow_var : Css.shadow Var.property_default
(** [inset_ring_shadow_var] is the --tw-inset-ring-shadow variable. *)

val shadow_property_rules : Css.t
(** [shadow_property_rules] is the [@property] rule of every ring and shadow
    variable, which each [box-shadow] utility registers. *)

val shadow_property_metadata : Var.metadata list
(** [shadow_property_metadata] is the build metadata of every ring and shadow
    variable. *)
