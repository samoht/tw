(** List style utilities.

    https://tailwindcss.com/docs/list-style-type
    https://tailwindcss.com/docs/list-style-position
    https://tailwindcss.com/docs/list-style-image *)

open Utility

val list_none : t
(** [list_none] removes bullets/numbers from lists. *)

val list_disc : t
(** [list_disc] uses disc bullets. *)

val list_decimal : t
(** [list_decimal] uses decimal numbering. *)

val list_inside : t
(** [list_inside] places list markers inside content box. *)

val list_outside : t
(** [list_outside] places list markers outside content box. *)

val list_image_none : t
(** [list_image_none] removes list-style-image. *)

val list_image_url : string -> t
(** [list_image_url url] sets list-style-image to the given URL. *)

module Handler : Utility.Handler
