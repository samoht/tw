(** Overflow utilities for controlling element overflow behavior.

    These utilities are in their own module to get correct ordering - they
    should appear after display, sizing, flex, alignment, and gap utilities but
    before borders. *)

module Css = Cascade.Css

module Handler = struct
  open Style
  open Css

  type t =
    | Auto
    | Hidden
    | Clip
    | Visible
    | Scroll
    | X_auto
    | X_clip
    | X_hidden
    | X_visible
    | X_scroll
    | Y_auto
    | Y_clip
    | Y_hidden
    | Y_visible
    | Y_scroll

  type Utility.base += Self of t

  let name = "overflow"

  (* Overflow comes after alignment (17) in Tailwind's utility ordering. *)
  let priority _ = 18

  (* Class suffix, style and cascade suborder of one utility. Written as a
     match, not a lookup table: a constructor added to [t] without an entry here
     is a compile error rather than a [Not_found] raised out of [to_class]
     halfway through rendering a sheet. *)
  let data : t -> string * Style.t * int = function
    | Auto -> ("auto", style [ overflow Auto ], 550)
    | Clip -> ("clip", style [ overflow Clip ], 551)
    | Hidden -> ("hidden", style [ overflow Hidden ], 552)
    | Scroll -> ("scroll", style [ overflow Scroll ], 553)
    | Visible -> ("visible", style [ overflow Visible ], 554)
    | X_auto -> ("x-auto", style [ overflow_x Auto ], 555)
    | X_clip -> ("x-clip", style [ overflow_x Clip ], 556)
    | X_hidden -> ("x-hidden", style [ overflow_x Hidden ], 557)
    | X_scroll -> ("x-scroll", style [ overflow_x Scroll ], 558)
    | X_visible -> ("x-visible", style [ overflow_x Visible ], 559)
    | Y_auto -> ("y-auto", style [ overflow_y Auto ], 560)
    | Y_clip -> ("y-clip", style [ overflow_y Clip ], 561)
    | Y_hidden -> ("y-hidden", style [ overflow_y Hidden ], 562)
    | Y_scroll -> ("y-scroll", style [ overflow_y Scroll ], 563)
    | Y_visible -> ("y-visible", style [ overflow_y Visible ], 564)

  (* Every constructor, for the class-name lookup. A missing entry costs a class
     that no longer parses, which the round-trip test reports. *)
  let all =
    [
      Auto;
      Clip;
      Hidden;
      Scroll;
      Visible;
      X_auto;
      X_clip;
      X_hidden;
      X_scroll;
      X_visible;
      Y_auto;
      Y_clip;
      Y_hidden;
      Y_scroll;
      Y_visible;
    ]

  let to_class t =
    let suffix, _, _ = data t in
    "overflow-" ^ suffix

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
    | None -> Error (`Msg "Not an overflow utility")

  let examples = [ Auto; X_auto; Y_auto ]
end

open Handler

let () = Utility.register (module Handler)

(** {1 Public API} *)

let utility x = Utility.base (Self x)
let overflow_auto = utility Auto
let overflow_hidden = utility Hidden
let overflow_clip = utility Clip
let overflow_visible = utility Visible
let overflow_scroll = utility Scroll
let overflow_x_auto = utility X_auto
let overflow_x_clip = utility X_clip
let overflow_x_hidden = utility X_hidden
let overflow_x_visible = utility X_visible
let overflow_x_scroll = utility X_scroll
let overflow_y_auto = utility Y_auto
let overflow_y_clip = utility Y_clip
let overflow_y_hidden = utility Y_hidden
let overflow_y_visible = utility Y_visible
let overflow_y_scroll = utility Y_scroll
