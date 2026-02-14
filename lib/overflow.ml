(** Overflow utilities for controlling element overflow behavior.

    These utilities are in their own module to get correct ordering - they
    should appear after display, sizing, flex, alignment, and gap utilities but
    before borders. *)

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
  let priority = 18

  (* Single source of truth: (handler, class_suffix, style_fn, suborder) *)
  let overflow_data =
    [
      (Auto, "auto", (fun () -> style [ overflow Auto ]), 550);
      (Clip, "clip", (fun () -> style [ overflow Clip ]), 551);
      (Hidden, "hidden", (fun () -> style [ overflow Hidden ]), 552);
      (Scroll, "scroll", (fun () -> style [ overflow Scroll ]), 553);
      (Visible, "visible", (fun () -> style [ overflow Visible ]), 554);
      (X_auto, "x-auto", (fun () -> style [ overflow_x Auto ]), 555);
      (X_clip, "x-clip", (fun () -> style [ overflow_x Clip ]), 556);
      (X_hidden, "x-hidden", (fun () -> style [ overflow_x Hidden ]), 557);
      (X_scroll, "x-scroll", (fun () -> style [ overflow_x Scroll ]), 558);
      (X_visible, "x-visible", (fun () -> style [ overflow_x Visible ]), 559);
      (Y_auto, "y-auto", (fun () -> style [ overflow_y Auto ]), 560);
      (Y_clip, "y-clip", (fun () -> style [ overflow_y Clip ]), 561);
      (Y_hidden, "y-hidden", (fun () -> style [ overflow_y Hidden ]), 562);
      (Y_scroll, "y-scroll", (fun () -> style [ overflow_y Scroll ]), 563);
      (Y_visible, "y-visible", (fun () -> style [ overflow_y Visible ]), 564);
    ]

  (* Derived lookup tables *)
  let to_class_map =
    List.map (fun (t, s, _, _) -> (t, "overflow-" ^ s)) overflow_data

  let to_style_map = List.map (fun (t, _, f, _) -> (t, f)) overflow_data
  let suborder_map = List.map (fun (t, _, _, o) -> (t, o)) overflow_data

  let of_class_map =
    List.map (fun (t, s, _, _) -> ("overflow-" ^ s, t)) overflow_data

  (* Handler functions derived from maps *)
  let suborder t = List.assoc t suborder_map
  let to_class t = List.assoc t to_class_map
  let to_style t = (List.assoc t to_style_map) ()

  let of_class cls =
    match List.assoc_opt cls of_class_map with
    | Some t -> Ok t
    | None -> Error (`Msg "Not an overflow utility")
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
