(** Flexbox layout utilities (direction, wrap).

    These utilities control flex container layout direction and wrap behavior.
    They come after grid-template utilities in Tailwind's ordering. *)

module Handler : Utility.Handler

val flex_row : Utility.t
(** [flex_row] arranges flex items horizontally. *)

val flex_row_reverse : Utility.t
(** [flex_row_reverse] arranges flex items horizontally but reversed. *)

val flex_col : Utility.t
(** [flex_col] arranges flex items vertically. *)

val flex_col_reverse : Utility.t
(** [flex_col_reverse] arranges flex items vertically but reversed. *)

val flex_wrap : Utility.t
(** [flex_wrap] allows flex items to wrap to multiple lines. *)

val flex_wrap_reverse : Utility.t
(** [flex_wrap_reverse] wraps items in reverse order. *)

val flex_nowrap : Utility.t
(** [flex_nowrap] prevents flex items from wrapping. *)
