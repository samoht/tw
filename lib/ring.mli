(** Ring utilities: ring width and colour, inset ring and ring offset. Each sets
    its own variable of the [box-shadow] composition that {!Effects} declares.

    See {{:https://tailwindcss.com/docs/box-shadow#adding-a-ring}rings} and
    {{:https://tailwindcss.com/docs/box-shadow#adding-an-inset-ring}inset rings}.
*)

open Utility
open Color
module Handler : Utility.Handler

(** {1 Ring Utilities} *)

val ring : t
(** [ring] applies the default ring outline. *)

val ring_none : t
(** [ring_none] removes the ring outline. *)

val ring_xs : t
(** [ring_xs] applies an extra-small ring. *)

val ring_sm : t
(** [ring_sm] applies a small ring. *)

val ring_md : t
(** [ring_md] applies a medium ring. *)

val ring_lg : t
(** [ring_lg] applies a large ring. *)

val ring_xl : t
(** [ring_xl] applies an extra-large ring. *)

val inset_ring : t
(** [inset_ring] applies a default (1px) inset ring. *)

val ring_color : ?opacity:int -> ?shade:int -> color -> t
(** [ring_color color] sets the ring color class, e.g., [ring_color blue]. Use
    [~shade] to pick a shade (default 500) and [~opacity] to set opacity as a
    percentage. *)

val ring_inset : t
(** [ring_inset] applies an inset ring. *)

val ring_offset_width : int -> t
(** [ring_offset_width n] sets the ring offset width to [n] pixels.

    @raise Invalid_argument if [n] is negative. *)

val ring_offset_color : ?opacity:int -> ?shade:int -> color -> t
(** [ring_offset_color color] sets the ring offset color. [shade] defaults to
    500 and [opacity] sets the alpha modifier (0-100). *)

val ring_offset_transparent : t
(** [ring_offset_transparent] makes the ring offset transparent. *)

val ring_offset_current : t
(** [ring_offset_current] uses [currentColor] for the ring offset. *)

val ring_offset_inherit : t
(** [ring_offset_inherit] inherits the ring offset color. *)
