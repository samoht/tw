(** Columns utilities for multi-column layout

    @see <https://tailwindcss.com/docs/columns>
      Tailwind CSS Columns documentation *)

open Utility

(** {1 Columns Utilities} *)

val columns_auto : t
(** [columns_auto] sets columns to auto. *)

val columns : int -> t
(** [columns n] sets the number of columns to [n]. *)

val columns_3xs : t
(** [columns_3xs] sets column width to --container-3xs. *)

val columns_2xs : t
(** [columns_2xs] sets column width to --container-2xs. *)

val columns_xs : t
(** [columns_xs] sets column width to --container-xs. *)

val columns_sm : t
(** [columns_sm] sets column width to --container-sm. *)

val columns_md : t
(** [columns_md] sets column width to --container-md. *)

val columns_lg : t
(** [columns_lg] sets column width to --container-lg. *)

val columns_xl : t
(** [columns_xl] sets column width to --container-xl. *)

val columns_2xl : t
(** [columns_2xl] sets column width to --container-2xl. *)

val columns_3xl : t
(** [columns_3xl] sets column width to --container-3xl. *)

val columns_4xl : t
(** [columns_4xl] sets column width to --container-4xl. *)

val columns_5xl : t
(** [columns_5xl] sets column width to --container-5xl. *)

val columns_6xl : t
(** [columns_6xl] sets column width to --container-6xl. *)

val columns_7xl : t
(** [columns_7xl] sets column width to --container-7xl. *)
