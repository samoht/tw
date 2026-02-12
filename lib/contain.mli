(** Contain utilities for CSS containment.

    The CSS {i contain} property allows an author to indicate that an element
    and its contents are, as much as possible, independent of the rest of the
    document tree. This allows the browser to recalculate layout, style, paint,
    size, or any combination of them for a limited area of the DOM.

    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/contain> MDN: contain
    @see <https://tailwindcss.com/docs/contain> Tailwind: contain *)

open Utility
module Handler : Handler

val contain_none : t
(** [contain_none] removes all containment. *)

val contain_strict : t
(** [contain_strict] applies all containment rules except style. *)

val contain_content : t
(** [contain_content] applies layout, paint, and style containment. *)

val contain_size : t
(** [contain_size] enables size containment. Composable with other contain
    values. *)

val contain_layout : t
(** [contain_layout] enables layout containment. Composable with other contain
    values. *)

val contain_paint : t
(** [contain_paint] enables paint containment. Composable with other contain
    values. *)

val contain_style : t
(** [contain_style] enables style containment. Composable with other contain
    values. *)

val contain_inline_size : t
(** [contain_inline_size] enables inline-size containment. Composable with other
    contain values. *)

val contain_arbitrary : string -> t
(** [contain_arbitrary value] sets contain to an arbitrary value. *)
