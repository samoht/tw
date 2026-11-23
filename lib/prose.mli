(** Prose typography utilities.

    [prose] styles the HTML inside a container (headings, paragraphs, lists,
    blockquotes, code and tables) with the font sizes, spacing and colours of
    the
    {{:https://tailwindcss.com/docs/typography-plugin}Tailwind Typography
     plugin}. The colours are CSS variables, so a colour variant such as
    [prose-slate] changes them all at once. *)

open Cascade

(** Example usage:
    {[
    let article_class = Tw.to_classes Tw.[ prose; prose_lg ]
    ]} *)

type variant =
  [ `Base | `Sm | `Lg | `Xl | `Xl2 | `Gray | `Slate | `Zinc | `Neutral | `Stone ]
(** The type for prose size and colour variants. *)

(** {1 Internal Conversion Functions} *)

open Utility

val pp : variant -> string
(** [pp variant] pretty-prints a prose variant. *)

val to_class : variant -> string
(** [to_class variant] is the class name of [variant], such as ["prose-sm"]. *)

(** {2 Prose Utilities} *)

val prose : t
(** [prose] applies base prose styling. *)

val prose_sm : t
(** [prose_sm] applies small prose styling. *)

val prose_lg : t
(** [prose_lg] applies large prose styling. *)

val prose_xl : t
(** [prose_xl] applies extra large prose styling. *)

val prose_2xl : t
(** [prose_2xl] applies 2xl prose styling. *)

val prose_gray : t
(** [prose_gray] applies gray color theme. *)

val prose_slate : t
(** [prose_slate] applies slate color theme. *)

val prose_zinc : t
(** [prose_zinc] applies zinc color theme. *)

val prose_neutral : t
(** [prose_neutral] applies neutral color theme. *)

val prose_stone : t
(** [prose_stone] applies stone color theme. *)

val prose_invert : t
(** [prose_invert] remaps the prose palette to its inverted (dark) variant. *)

val prose_orange : t
(** [prose_orange] applies the orange accent (link) color theme. *)

val stylesheet : unit -> Css.statement list
(** [stylesheet ()] is the CSS rules of every prose variant. *)

(** {2 Prose markers}

    Helper utilities that add semantic markers used by prose selectors without
    emitting any CSS on their own. Useful to avoid raw class strings in HTML. *)

val prose_lead : t
(** [prose_lead] applies the "lead" marker class for emphasized first paragraphs
    inside prose. *)

val not_prose : t
(** [not_prose] applies the "not-prose" marker class to exclude a subtree from
    prose styling. *)

module Handler : Utility.Handler
