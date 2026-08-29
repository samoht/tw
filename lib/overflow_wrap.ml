(** Overflow wrap utilities for controlling word wrapping behavior.

    What's included:
    - `wrap-normal` - Default word wrapping.
    - `wrap-break-word` - Break words to prevent overflow.
    - `wrap-anywhere` - Break at any character to prevent overflow. *)

module Css = Cascade.Css

module Handler = struct
  open Style
  open Css

  type t = Normal | Break_word | Anywhere
  type Utility.base += Self of t

  let name = "overflow_wrap"

  (* Tailwind's property table ranks [overflow-wrap] at 291, between [text-wrap]
     (290) and [word-break] (292); that puts these utilities in
     [Typography_late]'s default late-typography band (priority 26), not a band
     of their own. All three set only [overflow-wrap], so Tailwind's candidate
     sort ties them - and [break-words] in lib/typography.ml, which writes the
     same lone property - and breaks the tie alphabetically by class name. 8309
     is that shared suborder: it sits right after [Typography_late]'s
     [break-normal] (8308, which writes [overflow-wrap] then carries on to
     [word-break] and so sorts first) and before its [break-all]/[break-keep]
     (8310, tied on [word-break] alone). A bare constant, not [Property_order],
     because migrating this shared slot to Tailwind's real-rank scale would
     require moving every other family in the priority-26 band along with it
     (see lib/typography.ml). *)
  let priority _ = 26
  let suborder _ = 8309

  let to_class = function
    | Normal -> "wrap-normal"
    | Break_word -> "wrap-break-word"
    | Anywhere -> "wrap-anywhere"

  let to_style _theme = function
    | Normal -> style [ overflow_wrap Normal ]
    | Break_word -> style [ overflow_wrap Break_word ]
    | Anywhere -> style [ overflow_wrap Anywhere ]

  let of_class _theme class_name =
    let parts = Parse.split_class class_name in
    match parts with
    | [ "wrap"; "normal" ] -> Ok Normal
    | [ "wrap"; "break"; "word" ] -> Ok Break_word
    | [ "wrap"; "anywhere" ] -> Ok Anywhere
    | _ -> Error (`Msg "Not an overflow-wrap utility")

  let examples = [ Normal ]
end

open Handler

let () = Utility.register (module Handler)
let utility x = Utility.base (Self x)
let wrap_normal = utility Normal
let wrap_break_word = utility Break_word
let wrap_anywhere = utility Anywhere
