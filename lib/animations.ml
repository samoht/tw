(** Animation and transition utilities *)

open Style
open Css
module Parse = Parse

(** {1 Animations Utility Type} *)

type utility =
  (* Transitions *)
  | Transition_none
  | Transition_all
  | Transition_colors
  | Transition_opacity
  | Transition_shadow
  | Transition_transform
  | Transition
  (* Animations *)
  | Animate_none
  | Animate_spin
  | Animate_ping
  | Animate_pulse
  | Animate_bounce
  (* Duration *)
  | Duration of int
  (* Delay *)
  | Delay of int
  (* Easing *)
  | Ease_linear
  | Ease_in
  | Ease_out
  | Ease_in_out

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

let ( >|= ) = Parse.( >|= )

(** {1 Utility Conversion Functions} *)

let to_style = function
  | Transition_none -> transition_none
  | Transition_all -> transition_all
  | Transition_colors -> transition_colors
  | Transition_opacity -> transition_opacity
  | Transition_shadow -> transition_shadow
  | Transition_transform -> transition_transform
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

let of_string = function
  | [ "transition"; "none" ] -> Ok Transition_none
  | [ "transition"; "all" ] -> Ok Transition_all
  | [ "transition"; "colors" ] -> Ok Transition_colors
  | [ "transition"; "opacity" ] -> Ok Transition_opacity
  | [ "transition"; "shadow" ] -> Ok Transition_shadow
  | [ "transition"; "transform" ] -> Ok Transition_transform
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
  | _ -> Error (`Msg "Not an animation/transition utility")

(** {1 Utility Ordering} *)

let suborder = function
  | Transition -> 0
  | Transition_none -> 1
  | Transition_all -> 2
  | Transition_colors -> 3
  | Transition_opacity -> 4
  | Transition_shadow -> 5
  | Transition_transform -> 6
  | Duration n -> 100 + n
  | Delay n -> 200 + n
  | Ease_linear -> 300
  | Ease_in -> 301
  | Ease_out -> 302
  | Ease_in_out -> 303
  | Animate_none -> 400
  | Animate_spin -> 401
  | Animate_ping -> 402
  | Animate_pulse -> 403
  | Animate_bounce -> 404
