(** Animation utilities

    What's included:
    - `animate-*` - Predefined animations (spin, ping, pulse, bounce).

    What's not:
    - Custom keyframe animations beyond the predefined ones.

    Parsing contract (`of_string`):
    - Accepts ["animate"; ...]. Unknown tokens yield `Error (`Msg "Not an
      animation utility")`. *)

module Handler = struct
  open Style
  open Css

  type t =
    | Animate_none
    | Animate_spin
    | Animate_ping
    | Animate_pulse
    | Animate_bounce

  type Utility.base += Self of t

  let name = "animations"

  (* Match Tailwind ordering: animations after transforms, before cursor *)
  let priority = 10

  let animate_none =
    style
      [
        Css.animation
          (Css.Shorthand
             {
               name = Some "none";
               duration = None;
               timing_function = None;
               delay = None;
               iteration_count = None;
               direction = None;
               fill_mode = None;
               play_state = None;
             });
      ]

  let animate_spin =
    style
      [
        Css.animation
          (Css.Shorthand
             {
               name = Some "spin";
               duration = Some (S 1.0);
               timing_function = Some Linear;
               delay = None;
               iteration_count = Some Infinite;
               direction = None;
               fill_mode = None;
               play_state = None;
             });
      ]

  let animate_ping =
    style
      [
        Css.animation
          (Css.Shorthand
             {
               name = Some "ping";
               duration = Some (S 1.0);
               timing_function = Some (Cubic_bezier (0.0, 0.0, 0.2, 1.0));
               delay = None;
               iteration_count = Some Infinite;
               direction = None;
               fill_mode = None;
               play_state = None;
             });
      ]

  (* Theme variable for animate-pulse - order (7, 10) places it after radius (7,
     0-5) but before default-font-family (9, x) to match Tailwind ordering *)
  let animate_pulse_var = Var.theme Css.Animation "animate-pulse" ~order:(7, 10)

  let animate_pulse =
    (* The animation value stored as theme variable *)
    let pulse_animation =
      Css.Shorthand
        {
          name = Some "pulse";
          duration = Some (S 2.0);
          timing_function = Some (Cubic_bezier (0.4, 0., 0.6, 1.));
          delay = None;
          iteration_count = Some Infinite;
          direction = None;
          fill_mode = None;
          play_state = None;
        }
    in
    let theme_decl, pulse_var = Var.binding animate_pulse_var pulse_animation in
    let pulse_keyframes =
      Css.keyframes "pulse"
        [
          {
            Css.Stylesheet.keyframe_selector = "50%";
            keyframe_declarations = [ Css.Declaration.opacity 0.5 ];
          };
        ]
    in
    style ~rules:(Some [ pulse_keyframes ])
      [ theme_decl; Css.animation (Css.Var pulse_var) ]

  let animate_bounce =
    style
      [
        Css.animation
          (Css.Shorthand
             {
               name = Some "bounce";
               duration = Some (S 1.0);
               timing_function = None;
               delay = None;
               iteration_count = Some Infinite;
               direction = None;
               fill_mode = None;
               play_state = None;
             });
      ]

  let to_style = function
    | Animate_none -> animate_none
    | Animate_spin -> animate_spin
    | Animate_ping -> animate_ping
    | Animate_pulse -> animate_pulse
    | Animate_bounce -> animate_bounce

  let suborder = function
    | Animate_bounce -> 0
    | Animate_none -> 1
    | Animate_ping -> 2
    | Animate_pulse -> 3
    | Animate_spin -> 4

  let of_class class_name =
    let parts = String.split_on_char '-' class_name in
    match parts with
    | [ "animate"; "none" ] -> Ok Animate_none
    | [ "animate"; "spin" ] -> Ok Animate_spin
    | [ "animate"; "ping" ] -> Ok Animate_ping
    | [ "animate"; "pulse" ] -> Ok Animate_pulse
    | [ "animate"; "bounce" ] -> Ok Animate_bounce
    | _ -> Error (`Msg "Not an animation utility")

  let to_class = function
    | Animate_none -> "animate-none"
    | Animate_spin -> "animate-spin"
    | Animate_ping -> "animate-ping"
    | Animate_pulse -> "animate-pulse"
    | Animate_bounce -> "animate-bounce"
end

open Handler

let () = Utility.register (module Handler)
let utility x = Utility.base (Self x)
let animate_none = utility Animate_none
let animate_spin = utility Animate_spin
let animate_ping = utility Animate_ping
let animate_pulse = utility Animate_pulse
let animate_bounce = utility Animate_bounce
