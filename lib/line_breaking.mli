(** Line breaking utilities: white space, text wrap, word break, overflow wrap
    and hyphens.

    https://tailwindcss.com/docs/white-space
    https://tailwindcss.com/docs/text-wrap
    https://tailwindcss.com/docs/word-break
    https://tailwindcss.com/docs/overflow-wrap
    https://tailwindcss.com/docs/hyphens *)

open Utility

(** {1 Whitespace Utilities} *)

val whitespace_normal : t
(** [whitespace_normal] preserves normal whitespace collapsing and wrapping. *)

val whitespace_nowrap : t
(** [whitespace_nowrap] prevents wrapping. *)

val whitespace_pre : t
(** [whitespace_pre] preserves whitespace and line breaks. *)

val whitespace_pre_line : t
(** [whitespace_pre_line] preserves whitespace but collapses newlines to spaces.
*)

val whitespace_pre_wrap : t
(** [whitespace_pre_wrap] preserves whitespace and wraps when needed. *)

val whitespace_break_spaces : t
(** [whitespace_break_spaces] preserves whitespace with breaking at soft wrap
    opportunities. *)

(** {1 Text Wrap} *)

val text_wrap : t
(** [text_wrap] enables automatic text wrapping. *)

val text_nowrap : t
(** [text_nowrap] disables text wrapping. *)

val text_balance : t
(** [text_balance] balances text across lines. *)

val text_pretty : t
(** [text_pretty] optimizes wrapping for readability. *)

(** {1 Word/Overflow Wrap} *)

val break_normal : t
(** [break_normal] uses normal word-breaking and overflow-wrap. *)

val break_words : t
(** [break_words] breaks words as needed (overflow-wrap: break-word). *)

val break_all : t
(** [break_all] breaks within words to prevent overflow. *)

val break_keep : t
(** [break_keep] keeps words intact (CJK keep-all). *)

val overflow_wrap_normal : t
(** [overflow_wrap_normal] uses the browser default overflow wrap behavior. *)

val overflow_wrap_anywhere : t
(** [overflow_wrap_anywhere] allows breaks anywhere to prevent overflow. *)

val overflow_wrap_break_word : t
(** [overflow_wrap_break_word] breaks long words to prevent overflow. *)

(** {1 Hyphens} *)

val hyphens_none : t
(** [hyphens_none] disables automatic hyphenation. *)

val hyphens_manual : t
(** [hyphens_manual] enables manual hyphenation via soft hyphens. *)

val hyphens_auto : t
(** [hyphens_auto] enables automatic hyphenation. *)

module Handler : Utility.Handler
