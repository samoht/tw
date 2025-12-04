(** Layout utilities for basic display, positioning, and object properties

    https://tailwindcss.com/docs/display https://tailwindcss.com/docs/visibility
    https://tailwindcss.com/docs/z-index https://tailwindcss.com/docs/object-fit
    https://tailwindcss.com/docs/object-position
    https://tailwindcss.com/docs/border-collapse
    https://tailwindcss.com/docs/table-layout *)

open Utility

(** {1 Display Utilities} *)

val block : t
val inline : t
val inline_block : t
val table : t
val hidden : t
val sr_only : t
val not_sr_only : t
val visible : t
val invisible : t
val collapse : t
val isolate : t
val z_0 : t
val z_10 : t
val z_20 : t
val z_30 : t
val z_40 : t
val z_50 : t
val z_auto : t
val object_contain : t
val object_cover : t
val object_fill : t
val object_none : t
val object_scale_down : t
val object_center : t
val object_top : t
val object_bottom : t
val object_left : t
val object_right : t

module Handler : Utility.Handler
