(** Transition utilities

    What's included:
    - `transition-*` - Transition properties (none, all, colors, opacity,
      shadow, transform).
    - `duration-*` - Animation/transition duration values.
    - `delay-*` - Animation/transition delay values.
    - `ease-*` - Easing functions (linear, in, out, in-out).

    What's not:
    - Custom timing functions beyond basic easing. *)

module Handler = struct
  open Style
  open Css

  type t =
    | Transition_none
    | Transition_all
    | Transition_colors
    | Transition_opacity
    | Transition_shadow
    | Transition_transform
    | Transition_behavior_normal
    | Transition_behavior_allow_discrete
    | Transition
    | Duration of int
    | Delay of int
    | Ease_linear
    | Ease_in
    | Ease_out
    | Ease_in_out

  type Utility.base += Self of t

  let name = "transitions"

  let priority =
    30 (* Transition utilities come after all other styling utilities *)

  (* Theme variables for default transition settings *)
  let default_transition_duration_var =
    Var.theme Css.Duration "default-transition-duration" ~order:(8, 0)

  let default_transition_timing_function_var =
    Var.theme Css.Timing_function "default-transition-timing-function"
      ~order:(8, 1)

  (* Variable for transition duration with @property *)
  let tw_duration_var =
    (* In Tailwind landing, duration appears after ring-offset-shadow (index
       21). *)
    Var.channel ~needs_property:true ~property_order:21 ~family:`Duration
      Css.Duration "tw-duration"

  (* Variable for transition timing function with @property *)
  let tw_ease_var =
    Var.channel ~needs_property:true ~property_order:26 Css.Timing_function
      "tw-ease"

  let transition_none = style [ Css.transition_property Css.None ]

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
    (* Use individual properties with nested var fallbacks Output:
       transition-property: all; transition-timing-function: var(--tw-ease,
       var(--default-transition-timing-function)); transition-duration:
       var(--tw-duration, var(--default-transition-duration)); *)
    let ease_ref =
      Var.reference_with_var_fallback tw_ease_var
        default_transition_timing_function_var
        (Css.Cubic_bezier (0., 0., 0., 0.))
    in
    let duration_ref =
      Var.reference_with_var_fallback tw_duration_var
        default_transition_duration_var (Css.Ms 0.)
    in
    (* Include theme bindings for default transition values *)
    let duration_theme_decl, _ =
      Var.binding default_transition_duration_var (Css.Ms 150.)
    in
    let timing_theme_decl, _ =
      Var.binding default_transition_timing_function_var
        (Css.Cubic_bezier (0.4, 0., 0.2, 1.))
    in
    style
      [
        duration_theme_decl;
        timing_theme_decl;
        Css.transition_property Css.All;
        Css.transition_timing_function (Css.Var ease_ref);
        Css.transition_duration (Css.Var duration_ref);
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

  let duration n =
    let duration_val = Css.Ms (float_of_int n) in
    let tw_duration_decl, _ = Var.binding tw_duration_var duration_val in
    let prop_rule = Var.property_rule tw_duration_var in
    let property_rules =
      match prop_rule with Some r -> r | None -> Css.empty
    in
    style ~property_rules
      [ tw_duration_decl; Css.transition_duration duration_val ]

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
    | Duration n -> duration n
    | Delay n -> delay n
    | Ease_linear -> ease_linear
    | Ease_in -> ease_in
    | Ease_out -> ease_out
    | Ease_in_out -> ease_in_out

  let suborder = function
    | Transition -> 0
    | Transition_all -> 1
    | Transition_colors -> 2
    | Transition_opacity -> 3
    | Transition_shadow -> 4
    | Transition_transform -> 5
    | Transition_none -> 6
    | Transition_behavior_normal -> 7
    | Transition_behavior_allow_discrete -> 8
    | Delay n -> 100 + n
    | Duration n -> 200 + n
    | Ease_linear -> 300
    | Ease_in -> 301
    | Ease_out -> 302
    | Ease_in_out -> 303

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
    | [ "duration"; n ] ->
        Parse.int_pos ~name:"duration" n >|= fun n -> Duration n
    | [ "delay"; n ] -> Parse.int_pos ~name:"delay" n >|= fun n -> Delay n
    | [ "ease"; "linear" ] -> Ok Ease_linear
    | [ "ease"; "in" ] -> Ok Ease_in
    | [ "ease"; "out" ] -> Ok Ease_out
    | [ "ease"; "in"; "out" ] -> Ok Ease_in_out
    | _ -> Error (`Msg "Not a transition utility")

  let to_class = function
    | Transition_none -> "transition-none"
    | Transition_all -> "transition-all"
    | Transition_colors -> "transition-colors"
    | Transition_opacity -> "transition-opacity"
    | Transition_shadow -> "transition-shadow"
    | Transition_transform -> "transition-transform"
    | Transition_behavior_normal -> "transition-behavior-normal"
    | Transition_behavior_allow_discrete -> "transition-behavior-allow-discrete"
    | Transition -> "transition"
    | Duration n -> "duration-" ^ string_of_int n
    | Delay n -> "delay-" ^ string_of_int n
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

let duration n = utility (Duration n)
let delay n = utility (Delay n)
let ease_linear = utility Ease_linear
let ease_in = utility Ease_in
let ease_out = utility Ease_out
let ease_in_out = utility Ease_in_out

(* Theme declarations for default transition values *)
let default_transition_declarations =
  (* --default-transition-duration: 150ms *)
  let duration_decl, _ =
    Var.binding Handler.default_transition_duration_var (Css.Ms 150.)
  in
  (* --default-transition-timing-function: cubic-bezier(0.4, 0, 0.2, 1) *)
  let timing_decl, _ =
    Var.binding Handler.default_transition_timing_function_var
      (Css.Cubic_bezier (0.4, 0., 0.2, 1.))
  in
  [ duration_decl; timing_decl ]
