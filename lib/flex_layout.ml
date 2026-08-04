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

  let flex_data =
    [
      (Direction Css.Column, "col", 0);
      (Direction Css.Column_reverse, "col-reverse", 1);
      (Direction Css.Row, "row", 2);
      (Direction Css.Row_reverse, "row-reverse", 3);
      (Wrap Css.Nowrap, "nowrap", 10);
      (Wrap Css.Wrap, "wrap", 11);
      (Wrap Css.Wrap_reverse, "wrap-reverse", 12);
    ]

  let to_class_map = List.map (fun (t, s, _) -> (t, "flex-" ^ s)) flex_data
  let suborder_map = List.map (fun (t, _, o) -> (t, o)) flex_data
  let of_class_map = List.map (fun (t, s, _) -> ("flex-" ^ s, t)) flex_data

  let to_style _theme = function
    | Direction d -> style [ Css.flex_direction d ]
    | Wrap w -> style [ Css.flex_wrap w ]

  let suborder t = List.assoc t suborder_map
  let to_class t = List.assoc t to_class_map

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
