(** Flexbox layout utilities (direction, wrap).

    These utilities control flex container layout direction and wrap behavior.
    They come after grid-template utilities in Tailwind's ordering. *)

module Css = Cascade.Css

module Handler = struct
  open Style

  type t = Direction of Css.flex_direction | Wrap of Css.flex_wrap
  type Utility.base += Self of t

  let name = "flex_layout"
  let priority _ = 16

  (* Class suffix and cascade suborder of one utility, as a match rather than a
     lookup table: a constructor added to either CSS value type without an entry
     here is a compile error, not a [Not_found] raised out of [to_class] partway
     through rendering a sheet. The CSS-wide keywords and the [var()] forms are
     spelled out rather than swept up by a catch-all, for the same reason. *)
  let data : t -> string * int = function
    | Direction Css.Column -> ("col", 0)
    | Direction Css.Column_reverse -> ("col-reverse", 1)
    | Direction Css.Row -> ("row", 2)
    | Direction Css.Row_reverse -> ("row-reverse", 3)
    | Wrap Css.Nowrap -> ("nowrap", 10)
    | Wrap Css.Wrap -> ("wrap", 11)
    | Wrap Css.Wrap_reverse -> ("wrap-reverse", 12)
    | Direction
        ( Css.Inherit | Css.Initial | Css.Unset | Css.Revert | Css.Revert_layer
        | Css.Var _ )
    | Wrap
        ( Css.Inherit | Css.Initial | Css.Unset | Css.Revert | Css.Revert_layer
        | Css.Var _ ) ->
        (* No flex class names a CSS-wide keyword or a var(), so [of_class]
           never builds one. *)
        invalid_arg "flex_layout: value has no class name"

  (* Every constructor a class names, for the class-name lookup. *)
  let all =
    [
      Direction Css.Column;
      Direction Css.Column_reverse;
      Direction Css.Row;
      Direction Css.Row_reverse;
      Wrap Css.Nowrap;
      Wrap Css.Wrap;
      Wrap Css.Wrap_reverse;
    ]

  let to_class t =
    let suffix, _ = data t in
    "flex-" ^ suffix

  let suborder t =
    let _, o = data t in
    o

  let to_style _theme = function
    | Direction d -> style [ Css.flex_direction d ]
    | Wrap w -> style [ Css.flex_wrap w ]

  let of_class_map = List.map (fun t -> (to_class t, t)) all

  let of_class _theme cls =
    match List.assoc_opt cls of_class_map with
    | Some t -> Ok t
    | None -> Error (`Msg "Not a flex layout utility")

  let examples = [ Direction Css.Row; Wrap Css.Wrap ]
end

open Handler

(** Register handler with Utility system *)
let () = Utility.register (module Handler)

let utility x = Utility.base (Self x)
let flex_row = utility (Direction Css.Row)
let flex_row_reverse = utility (Direction Css.Row_reverse)
let flex_col = utility (Direction Css.Column)
let flex_col_reverse = utility (Direction Css.Column_reverse)
let flex_wrap = utility (Wrap Css.Wrap)
let flex_wrap_reverse = utility (Wrap Css.Wrap_reverse)
let flex_nowrap = utility (Wrap Css.Nowrap)
