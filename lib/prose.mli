(** Prose typography utilities

    The Prose module provides beautiful typographic defaults for HTML content.
    It's designed to make text-heavy content like articles, blog posts, or
    documentation look great out of the box.

    When you apply prose styles to a container, all the content inside gets
    carefully crafted typography including:
    - Proper font sizes and line heights for readability
    - Beautiful spacing between paragraphs, headings, and other elements
    - Styled lists, blockquotes, code blocks, and more
    - Consistent color schemes using CSS variables

    Based on Tailwind CSS Typography plugin:
    https://tailwindcss.com/docs/typography-plugin *)

(** Example usage:
    {[
      (* Apply prose styling to article content *)
      div
        ~attrs:[ Tw.to_class_list [ Tw.prose; Tw.prose_lg ] ]
        [
          h1 [ text "Article Title" ];
          p [ text "This paragraph will have beautiful typography..." ];
          ul
            [
              li [ text "List items are properly styled" ];
              li [ text "With appropriate spacing" ];
            ];
        ]
    ]} *)

type variant =
  [ `Base | `Sm | `Lg | `Xl | `Xl2 | `Gray | `Slate | `Zinc | `Neutral | `Stone ]
(** Prose variant types *)

(** {1 Internal Conversion Functions} *)

open Utility

val pp : variant -> string
(** [pp variant] pretty-prints a prose variant. *)

val to_class : variant -> string
(** [to_class variant] converts prose variant to CSS class name. *)

val to_css_rules : variant -> Css.statement list
(** [to_css_rules variant] generates CSS rules for a prose variant.

    This generates all the CSS rules needed for the prose variant, including
    styles for headings, paragraphs, lists, quotes, etc.

    The rules use descendant selectors to style child elements:
    - [.prose h1] - styles all h1 elements inside prose
    - [.prose p] - styles all paragraphs inside prose
    - etc. *)

val css_variables : Css.declaration list
(** [css_variables] returns default prose CSS variables for theming.

    These CSS custom properties control the color scheme:
    - [--tw-prose-body] - body text color
    - [--tw-prose-headings] - heading color
    - [--tw-prose-links] - link color
    - [--tw-prose-bold] - bold text color
    - [--tw-prose-code] - inline code color
    - [--tw-prose-quotes] - blockquote text color
    - [--tw-prose-borders] - border colors
    - etc.

    These variables are included with the base prose styles and can be
    overridden by color theme variants like [Gray] or [Slate]. *)

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

val stylesheet : unit -> Css.statement list
(** [stylesheet ()] generates complete CSS rules for all prose variants. *)

(** {2 Prose markers}

    Helper utilities that add semantic markers used by prose selectors without
    emitting any CSS on their own. Useful to avoid raw class strings in HTML. *)

val prose_lead : t
(** [prose_lead] applies the "lead" marker class for emphasized first paragraphs
    inside prose. *)

val not_prose : t
(** [not_prose] applies the "not-prose" marker class to exclude a subtree from
    prose styling. *)
