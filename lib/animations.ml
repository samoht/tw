(** Animation and transition utilities

    What's included:
    - `transition-*` - Transition properties (none, all, colors, opacity,
      shadow, transform).
    - `animate-*` - Predefined animations (spin, ping, pulse, bounce).
    - `duration-*` - Animation/transition duration values.
    - `delay-*` - Animation/transition delay values.
    - `ease-*` - Easing functions (linear, in, out, in-out).

    What's not:
    - Custom keyframe animations beyond the predefined ones.
    - Complex timing functions beyond basic easing.

    Parsing contract (`of_string`):
    - Accepts ["transition"; ...], ["animate"; ...], ["duration"; n],
      ["delay"; n], ["ease"; ...]. Unknown tokens yield `Error (`Msg "Not an
      animation/transition utility")`. *)

module Handler = struct
  open Style
  open Css

  type t =
    | (* Transitions *)
      Transition_none
    | Transition_all
    | Transition_colors
    | Transition_opacity
    | Transition_shadow
    | Transition_transform
    | Transition_behavior_normal
    | Transition_behavior_allow_discrete
    | Transition
    | (* Animations *)
      Animate_none
    | Animate_spin
    | Animate_ping
    | Animate_pulse
    | Animate_bounce
    | (* Duration *)
      Duration of int
    | (* Delay *)
      Delay of int
    | (* Easing *)
      Ease_linear
    | Ease_in
    | Ease_out
    | Ease_in_out

  type Utility.base += Self of t

  let name = "animations"
  let priority = 8

  let transition_none =
    style
      [
        Css.transition
          (Css.Shorthand
             {
               property = Css.None;
               duration = Some (Css.S 0.0);
               timing_function = None;
               delay = None;
             });
      ]

  let transition =
    style
      [
        Css.transitions
          [
            Css.Shorthand
              {
                property = Css.Property "color";
                duration = Some (Css.Ms 150.);
                timing_function = Some (Css.Cubic_bezier (0.4, 0.0, 0.2, 1.0));
                delay = None;
              };
            Css.Shorthand
              {
                property = Css.Property "background-color";
                duration = Some (Css.Ms 150.);
                timing_function = Some (Css.Cubic_bezier (0.4, 0.0, 0.2, 1.0));
                delay = None;
              };
            Css.Shorthand
              {
                property = Css.Property "border-color";
                duration = Some (Css.Ms 150.);
                timing_function = Some (Css.Cubic_bezier (0.4, 0.0, 0.2, 1.0));
                delay = None;
              };
            Css.Shorthand
              {
                property = Css.Property "text-decoration-color";
                duration = Some (Css.Ms 150.);
                timing_function = Some (Css.Cubic_bezier (0.4, 0.0, 0.2, 1.0));
                delay = None;
              };
            Css.Shorthand
              {
                property = Css.Property "fill";
                duration = Some (Css.Ms 150.);
                timing_function = Some (Css.Cubic_bezier (0.4, 0.0, 0.2, 1.0));
                delay = None;
              };
            Css.Shorthand
              {
                property = Css.Property "stroke";
                duration = Some (Css.Ms 150.);
                timing_function = Some (Css.Cubic_bezier (0.4, 0.0, 0.2, 1.0));
                delay = None;
              };
            Css.Shorthand
              {
                property = Css.Property "opacity";
                duration = Some (Css.Ms 150.);
                timing_function = Some (Css.Cubic_bezier (0.4, 0.0, 0.2, 1.0));
                delay = None;
              };
            Css.Shorthand
              {
                property = Css.Property "box-shadow";
                duration = Some (Css.Ms 150.);
                timing_function = Some (Css.Cubic_bezier (0.4, 0.0, 0.2, 1.0));
                delay = None;
              };
            Css.Shorthand
              {
                property = Css.Property "transform";
                duration = Some (Css.Ms 150.);
                timing_function = Some (Css.Cubic_bezier (0.4, 0.0, 0.2, 1.0));
                delay = None;
              };
          ];
      ]

  let transition_all =
    style
      [
        Css.transition
          (Css.Shorthand
             {
               property = Css.All;
               duration = Some (Css.Ms 150.);
               timing_function = Some (Css.Cubic_bezier (0.4, 0.0, 0.2, 1.0));
               delay = None;
             });
      ]

  let transition_colors =
    style
      [
        Css.transitions
          [
            Css.Shorthand
              {
                property = Css.Property "background-color";
                duration = Some (Css.Ms 150.);
                timing_function = Some (Css.Cubic_bezier (0.4, 0.0, 0.2, 1.0));
                delay = None;
              };
            Css.Shorthand
              {
                property = Css.Property "border-color";
                duration = Some (Css.Ms 150.);
                timing_function = Some (Css.Cubic_bezier (0.4, 0.0, 0.2, 1.0));
                delay = None;
              };
            Css.Shorthand
              {
                property = Css.Property "color";
                duration = Some (Css.Ms 150.);
                timing_function = Some (Css.Cubic_bezier (0.4, 0.0, 0.2, 1.0));
                delay = None;
              };
            Css.Shorthand
              {
                property = Css.Property "fill";
                duration = Some (Css.Ms 150.);
                timing_function = Some (Css.Cubic_bezier (0.4, 0.0, 0.2, 1.0));
                delay = None;
              };
            Css.Shorthand
              {
                property = Css.Property "stroke";
                duration = Some (Css.Ms 150.);
                timing_function = Some (Css.Cubic_bezier (0.4, 0.0, 0.2, 1.0));
                delay = None;
              };
          ];
      ]

  let transition_opacity =
    style
      [
        Css.transition
          (Css.Shorthand
             {
               property = Css.Property "opacity";
               duration = Some (Css.Ms 150.);
               timing_function = Some (Css.Cubic_bezier (0.4, 0.0, 0.2, 1.0));
               delay = None;
             });
      ]

  let transition_shadow =
    style
      [
        Css.transition
          (Css.Shorthand
             {
               property = Css.Property "box-shadow";
               duration = Some (Css.Ms 150.);
               timing_function = Some (Css.Cubic_bezier (0.4, 0.0, 0.2, 1.0));
               delay = None;
             });
      ]

  let transition_transform =
    style
      [
        Css.transition
          (Css.Shorthand
             {
               property = Css.Property "transform";
               duration = Some (Css.Ms 150.);
               timing_function = Some (Css.Cubic_bezier (0.4, 0.0, 0.2, 1.0));
               delay = None;
             });
      ]

  (* Transition behavior (CSS Transitions Level 2) *)
  let transition_behavior_normal = style [ Css.transition_behavior Normal ]

  let transition_behavior_allow_discrete =
    style [ Css.transition_behavior Allow_discrete ]

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

  let animate_pulse =
    style
      [
        Css.animation
          (Css.Shorthand
             {
               name = Some "pulse";
               duration = Some (S 2.0);
               timing_function = Some (Cubic_bezier (0.4, 0.0, 0.6, 1.0));
               delay = None;
               iteration_count = Some Infinite;
               direction = None;
               fill_mode = None;
               play_state = None;
             });
      ]

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

  let duration n = style [ Css.transition_duration (Css.Ms (float_of_int n)) ]
  let ease_linear = style [ Css.transition_timing_function Linear ]

  let ease_in =
    style [ Css.transition_timing_function (Cubic_bezier (0.4, 0.0, 1.0, 1.0)) ]

  let ease_out =
    style [ Css.transition_timing_function (Cubic_bezier (0.0, 0.0, 0.2, 1.0)) ]

  let ease_in_out =
    style [ Css.transition_timing_function (Cubic_bezier (0.4, 0.0, 0.2, 1.0)) ]

  let delay n = style [ Css.transition_delay (Css.Ms (float_of_int n)) ]

  let to_style = function
    | Transition_none -> transition_none
    | Transition_all -> transition_all
    | Transition_colors -> transition_colors
    | Transition_opacity -> transition_opacity
    | Transition_shadow -> transition_shadow
    | Transition_transform -> transition_transform
    | Transition_behavior_normal -> transition_behavior_normal
    | Transition_behavior_allow_discrete -> transition_behavior_allow_discrete
    | Transition -> transition
    | Animate_none -> animate_none
    | Animate_spin -> animate_spin
    | Animate_ping -> animate_ping
    | Animate_pulse -> animate_pulse
    | Animate_bounce -> animate_bounce
    | Duration n -> duration n
    | Delay n -> delay n
    | Ease_linear -> ease_linear
    | Ease_in -> ease_in
    | Ease_out -> ease_out
    | Ease_in_out -> ease_in_out

  let suborder = function
    | Animate_bounce -> 0
    | Animate_none -> 1
    | Animate_ping -> 2
    | Animate_pulse -> 3
    | Animate_spin -> 4
    | Transition -> 100
    | Transition_all -> 101
    | Transition_colors -> 102
    | Transition_opacity -> 103
    | Transition_shadow -> 104
    | Transition_transform -> 105
    | Transition_none -> 106
    | Transition_behavior_normal -> 107
    | Transition_behavior_allow_discrete -> 108
    | Delay n -> 200 + n
    | Duration n -> 300 + n
    | Ease_linear -> 400
    | Ease_in -> 401
    | Ease_out -> 402
    | Ease_in_out -> 403

  let ( >|= ) = Parse.( >|= )

  let of_class class_name =
    let parts = String.split_on_char '-' class_name in
    match parts with
    | [ "transition"; "none" ] -> Ok Transition_none
    | [ "transition"; "all" ] -> Ok Transition_all
    | [ "transition"; "colors" ] -> Ok Transition_colors
    | [ "transition"; "opacity" ] -> Ok Transition_opacity
    | [ "transition"; "shadow" ] -> Ok Transition_shadow
    | [ "transition"; "transform" ] -> Ok Transition_transform
    | [ "transition"; "behavior"; "normal" ] -> Ok Transition_behavior_normal
    | [ "transition"; "behavior"; "allow"; "discrete" ] ->
        Ok Transition_behavior_allow_discrete
    | [ "transition" ] -> Ok Transition
    | [ "animate"; "none" ] -> Ok Animate_none
    | [ "animate"; "spin" ] -> Ok Animate_spin
    | [ "animate"; "ping" ] -> Ok Animate_ping
    | [ "animate"; "pulse" ] -> Ok Animate_pulse
    | [ "animate"; "bounce" ] -> Ok Animate_bounce
    | [ "duration"; n ] ->
        Parse.int_pos ~name:"duration" n >|= fun n -> Duration n
    | [ "delay"; n ] -> Parse.int_pos ~name:"delay" n >|= fun n -> Delay n
    | [ "ease"; "linear" ] -> Ok Ease_linear
    | [ "ease"; "in" ] -> Ok Ease_in
    | [ "ease"; "out" ] -> Ok Ease_out
    | [ "ease"; "in"; "out" ] -> Ok Ease_in_out
    | _ -> Error (`Msg "Not an animation utility")

  let to_class = function
    (* Transitions *)
    | Transition_none -> "transition-none"
    | Transition_all -> "transition-all"
    | Transition_colors -> "transition-colors"
    | Transition_opacity -> "transition-opacity"
    | Transition_shadow -> "transition-shadow"
    | Transition_transform -> "transition-transform"
    | Transition_behavior_normal -> "transition-behavior-normal"
    | Transition_behavior_allow_discrete -> "transition-behavior-allow-discrete"
    | Transition -> "transition"
    (* Animations *)
    | Animate_none -> "animate-none"
    | Animate_spin -> "animate-spin"
    | Animate_ping -> "animate-ping"
    | Animate_pulse -> "animate-pulse"
    | Animate_bounce -> "animate-bounce"
    (* Duration *)
    | Duration n -> "duration-" ^ string_of_int n
    (* Delay *)
    | Delay n -> "delay-" ^ string_of_int n
    (* Easing *)
    | Ease_linear -> "ease-linear"
    | Ease_in -> "ease-in"
    | Ease_out -> "ease-out"
    | Ease_in_out -> "ease-in-out"
end

open Handler

let () = Utility.register (module Handler)
let utility x = Utility.base (Self x)
let transition_none = utility Transition_none
let transition = utility Transition
let transition_all = utility Transition_all
let transition_colors = utility Transition_colors
let transition_opacity = utility Transition_opacity
let transition_shadow = utility Transition_shadow
let transition_transform = utility Transition_transform
let transition_behavior_normal = utility Transition_behavior_normal

let transition_behavior_allow_discrete =
  utility Transition_behavior_allow_discrete

let animate_none = utility Animate_none
let animate_spin = utility Animate_spin
let animate_ping = utility Animate_ping
let animate_pulse = utility Animate_pulse
let animate_bounce = utility Animate_bounce
let duration n = utility (Duration n)
let delay n = utility (Delay n)
let ease_linear = utility Ease_linear
let ease_in = utility Ease_in
let ease_out = utility Ease_out
let ease_in_out = utility Ease_in_out
