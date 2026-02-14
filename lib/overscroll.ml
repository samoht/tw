(** Overscroll behavior utilities for controlling scroll chaining.

    What's included:
    - `overscroll-auto` - Default scroll chaining behavior.
    - `overscroll-contain` - Prevent scroll chaining to parent.
    - `overscroll-none` - Prevent scroll chaining and overscroll effects.
    - `overscroll-x-*`, `overscroll-y-*` - Axis-specific overscroll behavior.

    Parsing contract ([of_class]):
    - Accepts ["overscroll"; value] and ["overscroll"; axis; value].
    - Unknown tokens yield [Error (`Msg "Not an overscroll utility")]. *)

module Handler = struct
  open Style
  open Css

  type t =
    | Auto
    | Contain
    | None_
    | X_auto
    | X_contain
    | X_none
    | Y_auto
    | Y_contain
    | Y_none

  type Utility.base += Self of t

  let name = "overscroll"

  (* Same priority as overflow (18) - these are related utilities *)
  let priority = 18

  (* Single source of truth: (handler, class_suffix, style_fn, suborder) *)
  let overscroll_data =
    [
      (Auto, "auto", (fun () -> style [ overscroll_behavior Auto ]), 600);
      ( Contain,
        "contain",
        (fun () -> style [ overscroll_behavior Contain ]),
        601 );
      (None_, "none", (fun () -> style [ overscroll_behavior None ]), 602);
      (X_auto, "x-auto", (fun () -> style [ overscroll_behavior_x Auto ]), 603);
      ( X_contain,
        "x-contain",
        (fun () -> style [ overscroll_behavior_x Contain ]),
        604 );
      (X_none, "x-none", (fun () -> style [ overscroll_behavior_x None ]), 605);
      (Y_auto, "y-auto", (fun () -> style [ overscroll_behavior_y Auto ]), 606);
      ( Y_contain,
        "y-contain",
        (fun () -> style [ overscroll_behavior_y Contain ]),
        607 );
      (Y_none, "y-none", (fun () -> style [ overscroll_behavior_y None ]), 608);
    ]

  (* Derived lookup tables *)
  let to_class_map =
    List.map (fun (t, s, _, _) -> (t, "overscroll-" ^ s)) overscroll_data

  let to_style_map = List.map (fun (t, _, f, _) -> (t, f)) overscroll_data
  let suborder_map = List.map (fun (t, _, _, o) -> (t, o)) overscroll_data

  let of_class_map =
    List.map (fun (t, s, _, _) -> ("overscroll-" ^ s, t)) overscroll_data

  (* Handler functions derived from maps *)
  let suborder t = List.assoc t suborder_map
  let to_class t = List.assoc t to_class_map
  let to_style t = (List.assoc t to_style_map) ()

  let of_class cls =
    match List.assoc_opt cls of_class_map with
    | Some t -> Ok t
    | None -> Error (`Msg "Not an overscroll utility")
end

open Handler

let () = Utility.register (module Handler)
let utility x = Utility.base (Self x)
let overscroll_auto = utility Auto
let overscroll_contain = utility Contain
let overscroll_none = utility None_
let overscroll_x_auto = utility X_auto
let overscroll_x_contain = utility X_contain
let overscroll_x_none = utility X_none
let overscroll_y_auto = utility Y_auto
let overscroll_y_contain = utility Y_contain
let overscroll_y_none = utility Y_none
