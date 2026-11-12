(** Font stretch utilities.

    https://tailwindcss.com/docs/font-stretch *)

open Utility

val font_stretch_ultra_condensed : t
(** [font_stretch_ultra_condensed] uses ultra-condensed font stretch (50%). *)

val font_stretch_extra_condensed : t
(** [font_stretch_extra_condensed] uses extra-condensed font stretch (62.5%). *)

val font_stretch_condensed : t
(** [font_stretch_condensed] uses condensed font stretch (75%). *)

val font_stretch_semi_condensed : t
(** [font_stretch_semi_condensed] uses semi-condensed font stretch (87.5%). *)

val font_stretch_normal : t
(** [font_stretch_normal] uses normal font stretch (100%). *)

val font_stretch_semi_expanded : t
(** [font_stretch_semi_expanded] uses semi-expanded font stretch (112.5%). *)

val font_stretch_expanded : t
(** [font_stretch_expanded] uses expanded font stretch (125%). *)

val font_stretch_extra_expanded : t
(** [font_stretch_extra_expanded] uses extra-expanded font stretch (150%). *)

val font_stretch_ultra_expanded : t
(** [font_stretch_ultra_expanded] uses ultra-expanded font stretch (200%). *)

val font_stretch_percent : int -> t
(** [font_stretch_percent n] sets font stretch to [n]%. *)

module Handler : Utility.Handler
