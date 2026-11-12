(** Font variant numeric utilities.

    https://tailwindcss.com/docs/font-variant-numeric *)

open Utility

val normal_nums : t
(** [normal_nums] resets to normal numeric glyphs. *)

val ordinal : t
(** [ordinal] enables ordinal markers (e.g., 1st, 2nd). *)

val slashed_zero : t
(** [slashed_zero] uses a slashed zero glyph. *)

val lining_nums : t
(** [lining_nums] uses lining numeral forms. *)

val oldstyle_nums : t
(** [oldstyle_nums] uses old‑style (text) figures. *)

val proportional_nums : t
(** [proportional_nums] uses proportional-width numbers. *)

val tabular_nums : t
(** [tabular_nums] uses tabular-width numbers. *)

val diagonal_fractions : t
(** [diagonal_fractions] uses diagonal fraction glyphs. *)

val stacked_fractions : t
(** [stacked_fractions] uses stacked fraction glyphs. *)

module Handler : Utility.Handler
