(** Flexbox layout utilities (direction, wrap).

    These utilities control flex container layout direction and wrap behavior.
    They come after grid-template utilities in Tailwind's ordering. *)

module Css = Cascade.Css

module Handler = struct
  open Style

  (* The seven values a [flex-*] class names, spelled as the utility's own type
     rather than as a pair of CSS value types. A CSS-wide keyword and a [var()]
     are both a [Css.flex_direction] and neither is a utility, so carrying the
     CSS type here would leave [data] partial and [to_class] raising on a value
     it was handed. *)
  type t =
    | Col
    | Col_reverse
    | Row
    | Row_reverse
    | Nowrap
    | Wrap
    | Wrap_reverse

  type Utility.base += Self of t

  let name = "flex_layout"
  let priority _ = 16

  (* Class suffix, declaration and cascade suborder of one utility, in one
     match, so a constructor added above without an entry here is a compile
     error. *)
  let data : t -> string * Css.declaration * int = function
    | Col -> ("col", Css.flex_direction Css.Column, 0)
    | Col_reverse -> ("col-reverse", Css.flex_direction Css.Column_reverse, 1)
    | Row -> ("row", Css.flex_direction Css.Row, 2)
    | Row_reverse -> ("row-reverse", Css.flex_direction Css.Row_reverse, 3)
    | Nowrap -> ("nowrap", Css.flex_wrap Css.Nowrap, 10)
    | Wrap -> ("wrap", Css.flex_wrap Css.Wrap, 11)
    | Wrap_reverse -> ("wrap-reverse", Css.flex_wrap Css.Wrap_reverse, 12)

  (* Every constructor a class names, for the class-name lookup. *)
  let all = [ Col; Col_reverse; Row; Row_reverse; Nowrap; Wrap; Wrap_reverse ]

  let to_class t =
    let suffix, _, _ = data t in
    "flex-" ^ suffix

  let suborder t =
    let _, _, o = data t in
    o

  let to_style _theme t =
    let _, decl, _ = data t in
    style [ decl ]

  let of_class_map = List.map (fun t -> (to_class t, t)) all

  let of_class _theme cls =
    match List.assoc_opt cls of_class_map with
    | Some t -> Ok t
    | None -> Error (`Msg "Not a flex layout utility")

  let examples = [ Row; Wrap ]
end

open Handler

(** Register handler with Utility system *)
let () = Utility.register (module Handler)

let utility x = Utility.base (Self x)
let flex_row = utility Row
let flex_row_reverse = utility Row_reverse
let flex_col = utility Col
let flex_col_reverse = utility Col_reverse
let flex_wrap = utility Wrap
let flex_wrap_reverse = utility Wrap_reverse
let flex_nowrap = utility Nowrap
