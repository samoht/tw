(** Form element utilities - matching Tailwind v4's [@tailwindcss/forms] plugin.

    https://tailwindcss.com/docs/accent-color
    https://tailwindcss.com/docs/appearance

    Form utilities are split into two priority groups to match Tailwind's
    output:
    - checkbox, radio, input: priority 3 (before layout at 4)
    - select, textarea: priority 7 (after sizing at 6) *)

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

module Handler : Utility.Handler
(** Handler for checkbox/radio/input (priority 3) *)

module Select : Utility.Handler
(** Handler for select/textarea (priority 7) *)
