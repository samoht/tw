(** Content utilities.

    https://tailwindcss.com/docs/content *)

open Utility

val content_none : t
(** [content_none] sets content to none. Useful with pseudo-elements when
    combined with variants. *)

val content : string -> t
(** [content s] sets CSS content to the literal text [s]. Quotes are added
    automatically for you and the class name uses the Tailwind v4 arbitrary
    value syntax: [content-["s"]]. For example, [content "→"] generates the
    class [content-["→"]] and sets [content: "→"]. Use {!content_none} for
    [content: none]. *)

module Handler : Utility.Handler
