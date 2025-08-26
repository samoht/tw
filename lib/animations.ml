(** Animation and transition utilities *)

open Core
open Css
module Parse = Parse

(** {1 Transition Utilities} *)

let transition_none =
  style "transition-none" [ Css.transition (Css.Simple (Css.None, Css.S 0.0)) ]

let transition =
  style "transition"
    [
      Css.transition
        (Css.Multiple
           [
             Css.With_timing
               ( Css.Property "color",
                 Css.Ms 150,
                 Css.Cubic_bezier (0.4, 0.0, 0.2, 1.0) );
             Css.With_timing
               ( Css.Property "background-color",
                 Css.Ms 150,
                 Css.Cubic_bezier (0.4, 0.0, 0.2, 1.0) );
             Css.With_timing
               ( Css.Property "border-color",
                 Css.Ms 150,
                 Css.Cubic_bezier (0.4, 0.0, 0.2, 1.0) );
             Css.With_timing
               ( Css.Property "text-decoration-color",
                 Css.Ms 150,
                 Css.Cubic_bezier (0.4, 0.0, 0.2, 1.0) );
             Css.With_timing
               ( Css.Property "fill",
                 Css.Ms 150,
                 Css.Cubic_bezier (0.4, 0.0, 0.2, 1.0) );
             Css.With_timing
               ( Css.Property "stroke",
                 Css.Ms 150,
                 Css.Cubic_bezier (0.4, 0.0, 0.2, 1.0) );
             Css.With_timing
               ( Css.Property "opacity",
                 Css.Ms 150,
                 Css.Cubic_bezier (0.4, 0.0, 0.2, 1.0) );
             Css.With_timing
               ( Css.Property "box-shadow",
                 Css.Ms 150,
                 Css.Cubic_bezier (0.4, 0.0, 0.2, 1.0) );
             Css.With_timing
               ( Css.Property "transform",
                 Css.Ms 150,
                 Css.Cubic_bezier (0.4, 0.0, 0.2, 1.0) );
           ]);
    ]

let transition_all =
  style "transition-all"
    [
      Css.transition
        (Css.With_timing
           (Css.All, Css.Ms 150, Css.Cubic_bezier (0.4, 0.0, 0.2, 1.0)));
    ]

let transition_colors =
  style "transition-colors"
    [
      Css.transition
        (Css.Multiple
           [
             Css.With_timing
               ( Css.Property "background-color",
                 Css.Ms 150,
                 Css.Cubic_bezier (0.4, 0.0, 0.2, 1.0) );
             Css.With_timing
               ( Css.Property "border-color",
                 Css.Ms 150,
                 Css.Cubic_bezier (0.4, 0.0, 0.2, 1.0) );
             Css.With_timing
               ( Css.Property "color",
                 Css.Ms 150,
                 Css.Cubic_bezier (0.4, 0.0, 0.2, 1.0) );
             Css.With_timing
               ( Css.Property "fill",
                 Css.Ms 150,
                 Css.Cubic_bezier (0.4, 0.0, 0.2, 1.0) );
             Css.With_timing
               ( Css.Property "stroke",
                 Css.Ms 150,
                 Css.Cubic_bezier (0.4, 0.0, 0.2, 1.0) );
           ]);
    ]

let transition_opacity =
  style "transition-opacity"
    [
      Css.transition
        (Css.With_timing
           ( Css.Property "opacity",
             Css.Ms 150,
             Css.Cubic_bezier (0.4, 0.0, 0.2, 1.0) ));
    ]

let transition_shadow =
  style "transition-shadow"
    [
      Css.transition
        (Css.With_timing
           ( Css.Property "box-shadow",
             Css.Ms 150,
             Css.Cubic_bezier (0.4, 0.0, 0.2, 1.0) ));
    ]

let transition_transform =
  style "transition-transform"
    [
      Css.transition
        (Css.With_timing
           ( Css.Property "transform",
             Css.Ms 150,
             Css.Cubic_bezier (0.4, 0.0, 0.2, 1.0) ));
    ]

(** {1 Animation Utilities} *)

let animate_none = style "animate-none" [ Css.animation "none" ]

let animate_spin =
  style "animate-spin" [ Css.animation "spin 1s linear infinite" ]

let animate_ping =
  style "animate-ping"
    [ Css.animation "ping 1s cubic-bezier(0, 0, 0.2, 1) infinite" ]

let animate_pulse =
  style "animate-pulse"
    [ Css.animation "pulse 2s cubic-bezier(0.4, 0, 0.6, 1) infinite" ]

let animate_bounce =
  style "animate-bounce" [ Css.animation "bounce 1s infinite" ]

(** {1 Duration Utilities} *)

let duration n =
  let class_name = "duration-" ^ string_of_int n in
  style class_name [ Css.transition_duration (Css.Ms n) ]

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
  style class_name [ Css.transition_delay (Css.Ms n) ]

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
