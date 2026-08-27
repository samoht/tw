(** Overscroll behavior utilities for controlling scroll chaining.

    What's included:
    - `overscroll-auto` - Default scroll chaining behavior.
    - `overscroll-contain` - Prevent scroll chaining to parent.
    - `overscroll-none` - Prevent scroll chaining and overscroll effects.
    - `overscroll-x-*`, `overscroll-y-*` - Axis-specific overscroll behavior.

    Parsing contract ([of_class]):
    - Accepts ["overscroll"; value] and ["overscroll"; axis; value].
    - Unknown tokens yield [Error (`Msg "Not an overscroll utility")]. *)

module Css = Cascade.Css

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
  let priority _ = 18

  (* Class suffix, style and cascade suborder of one utility. Written as a
     match, not a lookup table: a constructor added to [t] without an entry here
     is a compile error rather than a [Not_found] raised out of [to_class]
     halfway through rendering a sheet. *)
  let data : t -> string * Style.t * int = function
    | Auto -> ("auto", style [ overscroll_behavior [ Auto ] ], 600)
    | Contain -> ("contain", style [ overscroll_behavior [ Contain ] ], 601)
    | None_ -> ("none", style [ overscroll_behavior [ None ] ], 602)
    | X_auto -> ("x-auto", style [ overscroll_behavior_x Auto ], 603)
    | X_contain -> ("x-contain", style [ overscroll_behavior_x Contain ], 604)
    | X_none -> ("x-none", style [ overscroll_behavior_x None ], 605)
    | Y_auto -> ("y-auto", style [ overscroll_behavior_y Auto ], 606)
    | Y_contain -> ("y-contain", style [ overscroll_behavior_y Contain ], 607)
    | Y_none -> ("y-none", style [ overscroll_behavior_y None ], 608)

  (* Every constructor, for the class-name lookup. A missing entry costs a class
     that no longer parses, which the round-trip test reports. *)
  let all =
    [
      Auto; Contain; None_; X_auto; X_contain; X_none; Y_auto; Y_contain; Y_none;
    ]

  let to_class t =
    let suffix, _, _ = data t in
    "overscroll-" ^ suffix

  let to_style _theme t =
    let _, s, _ = data t in
    s

  let suborder t =
    let _, _, o = data t in
    o

  let of_class_map = List.map (fun t -> (to_class t, t)) all

  let of_class _theme cls =
    match List.assoc_opt cls of_class_map with
    | Some t -> Ok t
    | None -> Error (`Msg "Not an overscroll utility")

  let examples = [ Auto; X_auto; Y_auto ]
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
