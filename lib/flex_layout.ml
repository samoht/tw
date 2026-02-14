(** Flexbox layout utilities (direction, wrap).

    These utilities control flex container layout direction and wrap behavior.
    They come after grid-template utilities in Tailwind's ordering. *)

module Handler = struct
  open Style
  open Css

  type t =
    (* Direction *)
    | Flex_row
    | Flex_row_reverse
    | Flex_col
    | Flex_col_reverse
    (* Wrap *)
    | Flex_wrap
    | Flex_wrap_reverse
    | Flex_nowrap

  type Utility.base += Self of t

  let name = "flex_layout"
  let priority = 16

  let flex_data =
    [
      (Flex_col, "col", (fun () -> style [ flex_direction Column ]), 0);
      ( Flex_col_reverse,
        "col-reverse",
        (fun () -> style [ flex_direction Column_reverse ]),
        1 );
      (Flex_row, "row", (fun () -> style [ flex_direction Row ]), 2);
      ( Flex_row_reverse,
        "row-reverse",
        (fun () -> style [ flex_direction Row_reverse ]),
        3 );
      (Flex_nowrap, "nowrap", (fun () -> style [ flex_wrap Nowrap ]), 10);
      (Flex_wrap, "wrap", (fun () -> style [ flex_wrap Wrap ]), 11);
      ( Flex_wrap_reverse,
        "wrap-reverse",
        (fun () -> style [ flex_wrap Wrap_reverse ]),
        12 );
    ]

  let to_class_map = List.map (fun (t, s, _, _) -> (t, "flex-" ^ s)) flex_data
  let to_style_map = List.map (fun (t, _, f, _) -> (t, f)) flex_data
  let suborder_map = List.map (fun (t, _, _, o) -> (t, o)) flex_data
  let of_class_map = List.map (fun (t, s, _, _) -> ("flex-" ^ s, t)) flex_data
  let to_style t = (List.assoc t to_style_map) ()
  let suborder t = List.assoc t suborder_map
  let to_class t = List.assoc t to_class_map

  let of_class cls =
    match List.assoc_opt cls of_class_map with
    | Some t -> Ok t
    | None -> Error (`Msg "Not a flex layout utility")
end

open Handler

(** Register handler with Utility system *)
let () = Utility.register (module Handler)

let utility x = Utility.base (Self x)
let flex_row = utility Flex_row
let flex_row_reverse = utility Flex_row_reverse
let flex_col = utility Flex_col
let flex_col_reverse = utility Flex_col_reverse
let flex_wrap = utility Flex_wrap
let flex_wrap_reverse = utility Flex_wrap_reverse
let flex_nowrap = utility Flex_nowrap
