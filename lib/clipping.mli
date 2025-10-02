(** Clipping utilities *)

open Utility

val clip_polygon : (float * float) list -> t
(** [clip_polygon points] clips element to a polygon defined by percentage
    points (x%, y%). Follows Tailwind's arbitrary value convention. *)
