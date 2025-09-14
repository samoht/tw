(** Animation and transition utilities *)

open Core
open Css
module Parse = Parse

(** {1 Transition Utilities} *)

let transition_none =
  style "transition-none"
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
  style "transition"
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
  style "transition-all"
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
  style "transition-colors"
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
  style "transition-opacity"
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
  style "transition-shadow"
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
  style "transition-transform"
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

(** {1 Animation Utilities} *)

let animate_none =
  style "animate-none"
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
  style "animate-spin"
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
  style "animate-ping"
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
  style "animate-pulse"
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
  style "animate-bounce"
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

(** {1 Duration Utilities} *)

let duration n =
  let class_name = "duration-" ^ string_of_int n in
  style class_name [ Css.transition_duration (Css.Ms (float_of_int n)) ]

(** {1 Timing Function Utilities} *)

let ease_linear = style "ease-linear" [ Css.transition_timing_function Linear ]

let ease_in =
  style "ease-in"
    [ Css.transition_timing_function (Cubic_bezier (0.4, 0.0, 1.0, 1.0)) ]

let ease_out =
  style "ease-out"
    [ Css.transition_timing_function (Cubic_bezier (0.0, 0.0, 0.2, 1.0)) ]

let ease_in_out =
  style "ease-in-out"
    [ Css.transition_timing_function (Cubic_bezier (0.4, 0.0, 0.2, 1.0)) ]

(** {1 Delay Utilities} *)

let delay n =
  let class_name = "delay-" ^ string_of_int n in
  style class_name [ Css.transition_delay (Css.Ms (float_of_int n)) ]

(** {1 Parsing Functions} *)

let of_string = function
  | [ "transition"; "none" ] -> Ok transition_none
  | [ "transition"; "all" ] -> Ok transition_all
  | [ "transition"; "colors" ] -> Ok transition_colors
  | [ "transition"; "opacity" ] -> Ok transition_opacity
  | [ "transition"; "shadow" ] -> Ok transition_shadow
  | [ "transition"; "transform" ] -> Ok transition_transform
  | [ "transition" ] -> Ok transition (* Default transition *)
  | [ "animate"; "none" ] -> Ok animate_none
  | [ "animate"; "spin" ] -> Ok animate_spin
  | [ "animate"; "ping" ] -> Ok animate_ping
  | [ "animate"; "pulse" ] -> Ok animate_pulse
  | [ "animate"; "bounce" ] -> Ok animate_bounce
  | [ "duration"; n ] -> Parse.int_pos ~name:"duration" n |> Result.map duration
  | [ "delay"; n ] -> Parse.int_pos ~name:"delay" n |> Result.map delay
  | [ "ease"; "linear" ] -> Ok ease_linear
  | [ "ease"; "in" ] -> Ok ease_in
  | [ "ease"; "out" ] -> Ok ease_out
  | [ "ease"; "in"; "out" ] -> Ok ease_in_out
  | _ -> Error (`Msg "Not an animation/transition utility")
