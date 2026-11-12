(** Line breaking utilities: white space, text wrap, word break, overflow wrap
    and hyphens. *)

module Css = Cascade.Css

module Handler = struct
  open Style
  open Css

  type t =
    | (* Whitespace *)
      Whitespace_normal
    | Whitespace_nowrap
    | Whitespace_pre
    | Whitespace_pre_line
    | Whitespace_pre_wrap
    | Whitespace_break_spaces
    | (* Text wrap *)
      Text_wrap
    | Text_nowrap
    | Text_balance
    | Text_pretty
    | (* Break utilities *)
      Break_normal
    | Break_words
    | Break_all
    | Break_keep
    | (* Overflow wrap *)
      Overflow_wrap_normal
    | Overflow_wrap_anywhere
    | Overflow_wrap_break_word
    | (* Hyphens *)
      Hyphens_none
    | Hyphens_manual
    | Hyphens_auto

  let name = "line_breaking"

  (* Tailwind ranks text-wrap, overflow-wrap, word-break, hyphens and
     white-space at 290-295, after letter-spacing (289) and inside the
     priority-26 band the late text families share. *)
  let priority _ = 26
  let err_not_utility = Error (`Msg "Not a line-breaking utility")

  let of_class _theme class_name =
    let parts = Parse.split_class class_name in
    match parts with
    | [ "whitespace"; "normal" ] -> Ok Whitespace_normal
    | [ "whitespace"; "nowrap" ] -> Ok Whitespace_nowrap
    | [ "whitespace"; "pre" ] -> Ok Whitespace_pre
    | [ "whitespace"; "pre"; "line" ] -> Ok Whitespace_pre_line
    | [ "whitespace"; "pre"; "wrap" ] -> Ok Whitespace_pre_wrap
    | [ "whitespace"; "break"; "spaces" ] -> Ok Whitespace_break_spaces
    | [ "text"; "wrap" ] -> Ok Text_wrap
    | [ "text"; "nowrap" ] -> Ok Text_nowrap
    | [ "text"; "balance" ] -> Ok Text_balance
    | [ "text"; "pretty" ] -> Ok Text_pretty
    | [ "break"; "normal" ] -> Ok Break_normal
    | [ "break"; "words" ] -> Ok Break_words
    | [ "break"; "all" ] -> Ok Break_all
    | [ "break"; "keep" ] -> Ok Break_keep
    | [ "overflow"; "wrap"; "normal" ] -> Ok Overflow_wrap_normal
    | [ "overflow"; "wrap"; "anywhere" ] -> Ok Overflow_wrap_anywhere
    | [ "overflow"; "wrap"; "break"; "word" ] -> Ok Overflow_wrap_break_word
    | [ "hyphens"; "none" ] -> Ok Hyphens_none
    | [ "hyphens"; "manual" ] -> Ok Hyphens_manual
    | [ "hyphens"; "auto" ] -> Ok Hyphens_auto
    | _ -> err_not_utility

  let to_class = function
    | Whitespace_normal -> "whitespace-normal"
    | Whitespace_nowrap -> "whitespace-nowrap"
    | Whitespace_pre -> "whitespace-pre"
    | Whitespace_pre_line -> "whitespace-pre-line"
    | Whitespace_pre_wrap -> "whitespace-pre-wrap"
    | Whitespace_break_spaces -> "whitespace-break-spaces"
    | Text_wrap -> "text-wrap"
    | Text_nowrap -> "text-nowrap"
    | Text_balance -> "text-balance"
    | Text_pretty -> "text-pretty"
    | Break_normal -> "break-normal"
    | Break_words -> "break-words"
    | Break_all -> "break-all"
    | Break_keep -> "break-keep"
    | Overflow_wrap_normal -> "overflow-wrap-normal"
    | Overflow_wrap_anywhere -> "overflow-wrap-anywhere"
    | Overflow_wrap_break_word -> "overflow-wrap-break-word"
    | Hyphens_none -> "hyphens-none"
    | Hyphens_manual -> "hyphens-manual"
    | Hyphens_auto -> "hyphens-auto"

  let suborder = function
    (* Whitespace - alphabetical order *)
    | Whitespace_break_spaces -> 8330
    | Whitespace_normal -> 8331
    | Whitespace_nowrap -> 8332
    | Whitespace_pre -> 8333
    | Whitespace_pre_line -> 8334
    | Whitespace_pre_wrap -> 8335
    (* Text wrap - Tailwind ranks [text-wrap] at 290, right after letter-spacing
       (289, lib/letter_spacing.ml's suborder tops out at 8306) and before
       overflow-wrap (291, Break_normal below). Every variant here writes only
       [text-wrap], so they tie and the class name breaks it alphabetically:
       balance, nowrap, pretty, wrap.

       Not [Property_order.last 290]: that mechanism suits a family whose
       neighbours already use it (backgrounds, masks), where every rank in the
       shared priority is on the same 1000-wide scale. Here letter spacing,
       text-overflow and Whitespace stay on the priority-26 band's bare-integer
       scale (82xx-83xx) and are not being touched by this fix, so a suborder on
       the real-rank scale (290 * 1000 and up) would sort after all of them
       regardless of true rank. Every family below that needs to interleave with
       an unconverted neighbour stays on this same local scale for the same
       reason. *)
    | Text_balance -> 8307
    | Text_nowrap -> 8307
    | Text_pretty -> 8307
    | Text_wrap -> 8307
    (* Break utilities - Tailwind ranks [overflow-wrap] at 291 and [word-break]
       at 292. [break-normal] writes both, so it leads the 291-292 stretch;
       lib/overflow_wrap.ml's wrap-anywhere/wrap-break-word/ wrap-normal write
       only [overflow-wrap], same as [break-words], so all four tie with it at
       8309 - a bare constant matching the band's bare-integer scale (see the
       [Text_wrap] comment above). [break-all] and [break-keep] write only
       [word-break] and tie at 8310. *)
    | Break_normal -> 8308
    | Break_words -> 8309
    | Break_all -> 8310
    | Break_keep -> 8310
    (* Overflow wrap - not real Tailwind utility names ([overflow-wrap-normal]
       etc. rather than [wrap-normal]); real tailwindcss generates nothing for
       them, so there is no oracle order to match. Left where they were. *)
    | Overflow_wrap_normal -> 9300
    | Overflow_wrap_anywhere -> 9301
    | Overflow_wrap_break_word -> 9302
    (* Hyphens - Tailwind ranks [hyphens] at 294, between text-overflow (293,
       lib/text_overflow.ml) and white-space (295, Whitespace_* above); the
       [-webkit-hyphens] declaration every variant also writes carries no rank
       of its own, so all three tie on [hyphens] alone and the class name breaks
       it alphabetically: auto, manual, none. *)
    | Hyphens_auto -> 8325
    | Hyphens_manual -> 8325
    | Hyphens_none -> 8325

  let whitespace_normal = style [ white_space Normal ]
  let whitespace_nowrap = style [ white_space Nowrap ]
  let whitespace_pre = style [ white_space Pre ]
  let whitespace_pre_line = style [ white_space Pre_line ]
  let whitespace_pre_wrap = style [ white_space Pre_wrap ]
  let whitespace_break_spaces = style [ white_space Break_spaces ]
  let text_wrap = style [ Css.text_wrap Wrap ]
  let text_nowrap = style [ Css.text_wrap No_wrap ]
  let text_balance = style [ Css.text_wrap Balance ]
  let text_pretty = style [ Css.text_wrap Pretty ]
  let break_normal = style [ overflow_wrap Normal; word_break Normal ]
  let break_words = style [ overflow_wrap Break_word ]
  let break_all = style [ word_break Break_all ]
  let break_keep = style [ word_break Keep_all ]
  let overflow_wrap_normal = style [ overflow_wrap Normal ]
  let overflow_wrap_anywhere = style [ overflow_wrap Anywhere ]
  let overflow_wrap_break_word = style [ overflow_wrap Break_word ]
  let hyphens_none = style [ webkit_hyphens None; hyphens None ]
  let hyphens_manual = style [ webkit_hyphens Manual; hyphens Manual ]
  let hyphens_auto = style [ webkit_hyphens Auto; hyphens Auto ]

  let to_style _theme = function
    | Whitespace_normal -> whitespace_normal
    | Whitespace_nowrap -> whitespace_nowrap
    | Whitespace_pre -> whitespace_pre
    | Whitespace_pre_line -> whitespace_pre_line
    | Whitespace_pre_wrap -> whitespace_pre_wrap
    | Whitespace_break_spaces -> whitespace_break_spaces
    | Text_wrap -> text_wrap
    | Text_nowrap -> text_nowrap
    | Text_balance -> text_balance
    | Text_pretty -> text_pretty
    | Break_normal -> break_normal
    | Break_words -> break_words
    | Break_all -> break_all
    | Break_keep -> break_keep
    | Overflow_wrap_normal -> overflow_wrap_normal
    | Overflow_wrap_anywhere -> overflow_wrap_anywhere
    | Overflow_wrap_break_word -> overflow_wrap_break_word
    | Hyphens_none -> hyphens_none
    | Hyphens_manual -> hyphens_manual
    | Hyphens_auto -> hyphens_auto

  let examples =
    [
      Break_normal;
      Hyphens_none;
      Overflow_wrap_normal;
      Text_balance;
      Text_nowrap;
      Whitespace_normal;
    ]
end

open Handler
module Utility_factory = Utility.Make (Handler)

let utility = Utility_factory.v
let whitespace_normal = utility Whitespace_normal
let whitespace_nowrap = utility Whitespace_nowrap
let whitespace_pre = utility Whitespace_pre
let whitespace_pre_line = utility Whitespace_pre_line
let whitespace_pre_wrap = utility Whitespace_pre_wrap
let whitespace_break_spaces = utility Whitespace_break_spaces
let text_wrap = utility Text_wrap
let text_nowrap = utility Text_nowrap
let text_balance = utility Text_balance
let text_pretty = utility Text_pretty
let break_normal = utility Break_normal
let break_words = utility Break_words
let break_all = utility Break_all
let break_keep = utility Break_keep
let overflow_wrap_normal = utility Overflow_wrap_normal
let overflow_wrap_anywhere = utility Overflow_wrap_anywhere
let overflow_wrap_break_word = utility Overflow_wrap_break_word
let hyphens_none = utility Hyphens_none
let hyphens_manual = utility Hyphens_manual
let hyphens_auto = utility Hyphens_auto
