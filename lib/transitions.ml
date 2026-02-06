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
    (* Duration appears at position 21 in @layer properties *)
    Var.channel ~needs_property:true ~property_order:21 ~family:`Duration
      Css.Duration "tw-duration"

  (* Variable for transition timing function with @property *)
  let tw_ease_var =
    Var.channel ~needs_property:true ~property_order:22 Css.Timing_function
      "tw-ease"

  let transition_none = style [ Css.transition_property [ Css.None ] ]

  let transition =
    (* Use longhand properties with variable references like transition-all *)
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
    (* Use typed variable names for gradient properties *)
    let gradient_from_name =
      Var.css_name Backgrounds.Handler.gradient_from_var
    in
    let gradient_via_name = Var.css_name Backgrounds.Handler.gradient_via_var in
    let gradient_to_name = Var.css_name Backgrounds.Handler.gradient_to_var in
    style
      [
        duration_theme_decl;
        timing_theme_decl;
        Css.transition_property
          [
            Css.Property "color";
            Css.Property "background-color";
            Css.Property "border-color";
            Css.Property "outline-color";
            Css.Property "text-decoration-color";
            Css.Property "fill";
            Css.Property "stroke";
            Css.Property gradient_from_name;
            Css.Property gradient_via_name;
            Css.Property gradient_to_name;
            Css.Property "opacity";
            Css.Property "box-shadow";
            Css.Property "transform";
            Css.Property "translate";
            Css.Property "scale";
            Css.Property "rotate";
            Css.Property "filter";
            Css.Property "-webkit-backdrop-filter";
            Css.Property "backdrop-filter";
            Css.Property "display";
            Css.Property "content-visibility";
            Css.Property "overlay";
            Css.Property "pointer-events";
          ];
        Css.transition_timing_function (Css.Var ease_ref);
        Css.transition_duration (Css.Var duration_ref);
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
        Css.transition_property [ Css.All ];
        Css.transition_timing_function (Css.Var ease_ref);
        Css.transition_duration (Css.Var duration_ref);
      ]

  let transition_colors =
    (* Use individual properties like Tailwind v4 *)
    (* --tw-ease and --tw-duration have fallbacks to theme defaults *)
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
    (* Use typed variable names for gradient properties *)
    let gradient_from_name =
      Var.css_name Backgrounds.Handler.gradient_from_var
    in
    let gradient_via_name = Var.css_name Backgrounds.Handler.gradient_via_var in
    let gradient_to_name = Var.css_name Backgrounds.Handler.gradient_to_var in
    style
      [
        duration_theme_decl;
        timing_theme_decl;
        Css.transition_property
          [
            Css.Property "color";
            Css.Property "background-color";
            Css.Property "border-color";
            Css.Property "outline-color";
            Css.Property "text-decoration-color";
            Css.Property "fill";
            Css.Property "stroke";
            Css.Property gradient_from_name;
            Css.Property gradient_via_name;
            Css.Property gradient_to_name;
          ];
        Css.transition_timing_function (Css.Var ease_ref);
        Css.transition_duration (Css.Var duration_ref);
      ]

  let transition_opacity =
    (* Use longhand properties with variable references *)
    let ease_ref =
      Var.reference_with_var_fallback tw_ease_var
        default_transition_timing_function_var
        (Css.Cubic_bezier (0., 0., 0., 0.))
    in
    let duration_ref =
      Var.reference_with_var_fallback tw_duration_var
        default_transition_duration_var (Css.Ms 0.)
    in
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
        Css.transition_property [ Css.Property "opacity" ];
        Css.transition_timing_function (Css.Var ease_ref);
        Css.transition_duration (Css.Var duration_ref);
      ]

  let transition_shadow =
    let ease_ref =
      Var.reference_with_var_fallback tw_ease_var
        default_transition_timing_function_var
        (Css.Cubic_bezier (0., 0., 0., 0.))
    in
    let duration_ref =
      Var.reference_with_var_fallback tw_duration_var
        default_transition_duration_var (Css.Ms 0.)
    in
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
        Css.transition_property [ Css.Property "box-shadow" ];
        Css.transition_timing_function (Css.Var ease_ref);
        Css.transition_duration (Css.Var duration_ref);
      ]

  let transition_transform =
    let ease_ref =
      Var.reference_with_var_fallback tw_ease_var
        default_transition_timing_function_var
        (Css.Cubic_bezier (0., 0., 0., 0.))
    in
    let duration_ref =
      Var.reference_with_var_fallback tw_duration_var
        default_transition_duration_var (Css.Ms 0.)
    in
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
        Css.transition_property
          [
            Css.Property "transform";
            Css.Property "translate";
            Css.Property "scale";
            Css.Property "rotate";
          ];
        Css.transition_timing_function (Css.Var ease_ref);
        Css.transition_duration (Css.Var duration_ref);
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

  (* Theme variables for easing functions - order (7, 6-8) places them after
     radius (7, 0-5) but before animate (7, 9-12) *)
  let ease_in_var = Var.theme Css.Timing_function "ease-in" ~order:(7, 6)
  let ease_out_var = Var.theme Css.Timing_function "ease-out" ~order:(7, 7)
  let ease_in_out_var = Var.theme Css.Timing_function "ease-in-out" ~order:(7, 8)

  let ease_linear =
    (* Set --tw-ease to linear and use it for transition-timing-function *)
    let tw_ease_decl, _ = Var.binding tw_ease_var Linear in
    let prop_rule = Var.property_rule tw_ease_var in
    let property_rules =
      match prop_rule with Some r -> r | None -> Css.empty
    in
    style ~property_rules
      [ tw_ease_decl; Css.transition_timing_function Linear ]

  let ease_in =
    (* Set --tw-ease to var(--ease-in) and use the theme variable *)
    let ease_value = Cubic_bezier (0.4, 0.0, 1.0, 1.0) in
    let theme_decl, ease_in_ref = Var.binding ease_in_var ease_value in
    let tw_ease_decl, _ = Var.binding tw_ease_var (Css.Var ease_in_ref) in
    let prop_rule = Var.property_rule tw_ease_var in
    let property_rules =
      match prop_rule with Some r -> r | None -> Css.empty
    in
    style ~property_rules
      [
        theme_decl;
        tw_ease_decl;
        Css.transition_timing_function (Css.Var ease_in_ref);
      ]

  let ease_out =
    let ease_value = Cubic_bezier (0.0, 0.0, 0.2, 1.0) in
    let theme_decl, ease_out_ref = Var.binding ease_out_var ease_value in
    let tw_ease_decl, _ = Var.binding tw_ease_var (Css.Var ease_out_ref) in
    let prop_rule = Var.property_rule tw_ease_var in
    let property_rules =
      match prop_rule with Some r -> r | None -> Css.empty
    in
    style ~property_rules
      [
        theme_decl;
        tw_ease_decl;
        Css.transition_timing_function (Css.Var ease_out_ref);
      ]

  let ease_in_out =
    let ease_value = Cubic_bezier (0.4, 0.0, 0.2, 1.0) in
    let theme_decl, ease_in_out_ref = Var.binding ease_in_out_var ease_value in
    let tw_ease_decl, _ = Var.binding tw_ease_var (Css.Var ease_in_out_ref) in
    let prop_rule = Var.property_rule tw_ease_var in
    let property_rules =
      match prop_rule with Some r -> r | None -> Css.empty
    in
    style ~property_rules
      [
        theme_decl;
        tw_ease_decl;
        Css.transition_timing_function (Css.Var ease_in_out_ref);
      ]

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
    | Transition_behavior_allow_discrete -> 7 (* transition-discrete *)
    | Transition_behavior_normal -> 8 (* transition-normal *)
    | Delay n -> 100 + n
    | Duration n -> 200 + n
    (* Ease utilities come after Duration. Tailwind orders: duration then ease.
       Use a high base to ensure even duration-5000 (suborder 5200) < ease.
       Within ease, Tailwind orders alphabetically: in, in-out, linear, out. *)
    | Ease_in -> 100000
    | Ease_in_out -> 100001
    | Ease_linear -> 100002
    | Ease_out -> 100003

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
    | [ "transition"; "normal" ] -> Ok Transition_behavior_normal
    | [ "transition"; "discrete" ] -> Ok Transition_behavior_allow_discrete
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
    | Transition_behavior_normal -> "transition-normal"
    | Transition_behavior_allow_discrete -> "transition-discrete"
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
