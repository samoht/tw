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
    https://tailwindcss.com/docs/typography-plugin

    Example usage:
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

(** Prose variant types *)
type t =
  | Base  (** Default prose styling - optimized for 16px text *)
  | Sm  (** Small variant - optimized for 14px text *)
  | Lg  (** Large variant - optimized for 18px text *)
  | Xl  (** Extra large variant - optimized for 20px text *)
  | Xl2  (** 2x large variant - optimized for 24px text *)
  | Gray  (** Gray color theme - uses gray color palette *)
  | Slate  (** Slate color theme - uses slate color palette *)

val pp : t -> string
(** [pp variant] pretty-prints a prose variant. *)

val to_class : t -> string
(** [to_class variant] converts prose variant to CSS class name. *)

val to_css_rules : t -> Css.rule list
(** [to_css_rules variant] generates CSS rules for a prose variant.

    This generates all the CSS rules needed for the prose variant, including
    styles for headings, paragraphs, lists, quotes, etc.

    The rules use descendant selectors to style child elements:
    - [.prose h1] - styles all h1 elements inside prose
    - [.prose p] - styles all paragraphs inside prose
    - etc. *)

val to_base_properties : t -> Css.declaration list
(** [to_base_properties variant] returns base CSS properties for inline styles.

    Returns the CSS properties that apply to the prose container itself, not the
    descendant selectors. This is useful for inline styles.

    Examples:
    - [Base] returns font size, line height, color, and max-width
    - [Lg] returns font size and line height adjustments
    - [Gray] returns empty list (only affects CSS variables) *)

val css_variables : Css.declaration list
(** Default prose CSS variables for theming

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
