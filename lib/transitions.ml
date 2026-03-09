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
    | Transition_arbitrary of string
    | Duration of int
    | Duration_arbitrary of string * Css.duration
    | Delay of int
    | Delay_arbitrary of string * Css.duration
    | Ease_linear
    | Ease_in
    | Ease_out
    | Ease_in_out
    | Ease_arbitrary of string

  type Utility.base += Self of t

  let name = "transitions"

  let priority =
    30 (* Transition utilities come after all other styling utilities *)

  (* Theme variables for default transition settings. Timing-function has lower
     order (8,0) so it appears before duration (8,1) in the theme layer output,
     matching Tailwind's order. *)
  let default_transition_duration_var =
    Var.theme Css.Duration "default-transition-duration" ~order:(8, 1)

  let default_transition_timing_function_var =
    Var.theme Css.Timing_function "default-transition-timing-function"
      ~order:(8, 0)

  (* Variable for transition duration with @property *)
  let tw_duration_var =
    (* Duration appears after skew (4) but before scale (6-8) in properties *)
    Var.channel ~needs_property:true ~property_order:5 ~family:`Duration
      Css.Duration "tw-duration"

  (* Variable for transition timing function with @property *)
  let tw_ease_var =
    Var.channel ~needs_property:true ~property_order:5 Css.Timing_function
      "tw-ease"

  (* Theme variable for transition-property-opacity *)
  let transition_property_opacity_var =
    Var.theme Css.Transition_property_value "transition-property-opacity"
      ~order:(8, 2)

  let transition_none = style [ Css.transition_property [ Css.None ] ]

  (* Shared ease and duration refs using nested var fallback to theme defaults.
     Produces var(--tw-ease, var(--default-transition-timing-function)) and
     var(--tw-duration, var(--default-transition-duration)). Note: standalone
     tests (transition-all alone) expect direct fallbacks like ease/0s, but the
     full-set test expects nested var. We match the full-set behavior since it's
     correct when theme vars are declared. *)
  let ease_ref =
    Var.reference_with_var_fallback tw_ease_var
      default_transition_timing_function_var
      (Css.Cubic_bezier (0., 0., 0., 0.))

  let duration_ref =
    Var.reference_with_var_fallback tw_duration_var
      default_transition_duration_var (Css.Ms 0.)

  (* Theme declarations for the default transition vars. These go into :root,
     :host when transition utilities are used. Only included when theme values
     are set, so @config none tests don't get the :root, :host block. *)
  let default_theme_decls () =
    let has_timing =
      Var.theme_value "default-transition-timing-function" <> None
    in
    let has_duration = Var.theme_value "default-transition-duration" <> None in
    let timing =
      if has_timing then
        let d, _ =
          Var.binding default_transition_timing_function_var Css.Ease
        in
        [ d ]
      else []
    in
    let duration =
      if has_duration then
        let d, _ = Var.binding default_transition_duration_var (Css.Ms 100.) in
        [ d ]
      else []
    in
    timing @ duration

  let transition () =
    (* Use typed variable names for gradient properties *)
    let gradient_from_name =
      Var.css_name Backgrounds.Handler.gradient_from_var
    in
    let gradient_via_name = Var.css_name Backgrounds.Handler.gradient_via_var in
    let gradient_to_name = Var.css_name Backgrounds.Handler.gradient_to_var in
    style
      (default_theme_decls ()
      @ [
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
        ])

  let transition_all () =
    style
      (default_theme_decls ()
      @ [
          Css.transition_property [ Css.All ];
          Css.transition_timing_function (Css.Var ease_ref);
          Css.transition_duration (Css.Var duration_ref);
        ])

  (* Theme variable for transition-property-colors *)
  let transition_property_colors_var =
    Var.theme Css.Transition_property_value "transition-property-colors"
      ~order:(8, 3)

  let transition_colors () =
    let gradient_from_name =
      Var.css_name Backgrounds.Handler.gradient_from_var
    in
    let gradient_via_name = Var.css_name Backgrounds.Handler.gradient_via_var in
    let gradient_to_name = Var.css_name Backgrounds.Handler.gradient_to_var in
    (* When --transition-property-colors is set in theme, use the var reference;
       otherwise inline the full property list *)
    let has_theme_var = Var.theme_value "transition-property-colors" <> None in
    let extra_decls, (transition_props : Css.transition_property_value list) =
      if has_theme_var then
        let colors_decl, colors_ref =
          Var.binding transition_property_colors_var
            (Css.Property
               (String.concat ", "
                  [
                    "color";
                    "background-color";
                    "border-color";
                    "outline-color";
                    "text-decoration-color";
                    "fill";
                    "stroke";
                    gradient_from_name;
                    gradient_via_name;
                    gradient_to_name;
                  ]))
        in
        ([ colors_decl ], [ Css.Var colors_ref ])
      else
        ( [],
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
          ] )
    in
    style
      (default_theme_decls () @ extra_decls
      @ [
          Css.transition_property transition_props;
          Css.transition_timing_function (Css.Var ease_ref);
          Css.transition_duration (Css.Var duration_ref);
        ])

  let transition_opacity () =
    let opacity_decl, opacity_ref =
      Var.binding transition_property_opacity_var (Css.Property "opacity")
    in
    style
      (default_theme_decls ()
      @ [
          opacity_decl;
          Css.transition_property [ Css.Var opacity_ref ];
          Css.transition_timing_function (Css.Var ease_ref);
          Css.transition_duration (Css.Var duration_ref);
        ])

  let transition_shadow () =
    style
      (default_theme_decls ()
      @ [
          Css.transition_property [ Css.Property "box-shadow" ];
          Css.transition_timing_function (Css.Var ease_ref);
          Css.transition_duration (Css.Var duration_ref);
        ])

  let transition_transform () =
    style
      (default_theme_decls ()
      @ [
          Css.transition_property
            [
              Css.Property "transform";
              Css.Property "translate";
              Css.Property "scale";
              Css.Property "rotate";
            ];
          Css.transition_timing_function (Css.Var ease_ref);
          Css.transition_duration (Css.Var duration_ref);
        ])

  let transition_arbitrary var_str =
    let bare_name = Parse.extract_var_name var_str in
    let ref_ : Css.transition_property_value Css.var = Css.var_ref bare_name in
    style
      (default_theme_decls ()
      @ [
          Css.transition_property [ Css.Var ref_ ];
          Css.transition_timing_function (Css.Var ease_ref);
          Css.transition_duration (Css.Var duration_ref);
        ])

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
  let _ease_linear_var =
    Var.theme Css.Timing_function "ease-linear" ~order:(7, 15)

  let ease_in_var = Var.theme Css.Timing_function "ease-in" ~order:(7, 16)
  let ease_out_var = Var.theme Css.Timing_function "ease-out" ~order:(7, 17)

  let ease_in_out_var =
    Var.theme Css.Timing_function "ease-in-out" ~order:(7, 18)

  let ease_linear =
    let tw_ease_decl, _ = Var.binding tw_ease_var Css.Linear in
    let prop_rule = Var.property_rule tw_ease_var in
    let property_rules =
      match prop_rule with Some r -> r | None -> Css.empty
    in
    style ~property_rules
      [ tw_ease_decl; Css.transition_timing_function Css.Linear ]

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

  let ease_arbitrary var_str =
    let bare_name = Parse.extract_var_name var_str in
    let ref_ : Css.timing_function Css.var = Css.var_ref bare_name in
    let tw_ease_decl, _ = Var.binding tw_ease_var (Css.Var ref_) in
    let prop_rule = Var.property_rule tw_ease_var in
    let property_rules =
      match prop_rule with Some r -> r | None -> Css.empty
    in
    style ~property_rules
      [ tw_ease_decl; Css.transition_timing_function (Css.Var ref_) ]

  let delay n = style [ Css.transition_delay (Css.Ms (float_of_int n)) ]
  let delay_arbitrary d = style [ Css.transition_delay d ]

  let duration_arbitrary d =
    let tw_duration_decl, _ = Var.binding tw_duration_var d in
    let prop_rule = Var.property_rule tw_duration_var in
    let property_rules =
      match prop_rule with Some r -> r | None -> Css.empty
    in
    style ~property_rules [ tw_duration_decl; Css.transition_duration d ]

  let to_style = function
    | Transition_none -> transition_none
    | Transition_all -> transition_all ()
    | Transition_colors -> transition_colors ()
    | Transition_opacity -> transition_opacity ()
    | Transition_shadow -> transition_shadow ()
    | Transition_transform -> transition_transform ()
    | Transition_behavior_normal -> transition_behavior_normal
    | Transition_behavior_allow_discrete -> transition_behavior_allow_discrete
    | Transition -> transition ()
    | Transition_arbitrary s -> transition_arbitrary s
    | Duration n -> duration n
    | Duration_arbitrary (_, d) -> duration_arbitrary d
    | Delay n -> delay n
    | Delay_arbitrary (_, d) -> delay_arbitrary d
    | Ease_linear -> ease_linear
    | Ease_in -> ease_in
    | Ease_out -> ease_out
    | Ease_in_out -> ease_in_out
    | Ease_arbitrary s -> ease_arbitrary s

  let suborder = function
    | Transition -> 0
    | Transition_all -> 1
    | Transition_colors -> 2
    | Transition_opacity -> 3
    | Transition_shadow -> 4
    | Transition_transform -> 5
    | Transition_arbitrary _ ->
        0 (* before named transitions, like Transition *)
    | Transition_none -> 6
    | Transition_behavior_allow_discrete -> 7 (* transition-discrete *)
    | Transition_behavior_normal -> 8 (* transition-normal *)
    | Delay n -> 100 + n
    | Delay_arbitrary _ -> 100000
    | Duration n -> 200 + n
    | Duration_arbitrary _ -> 200000
    (* Ease utilities come after Duration. Tailwind orders: duration then ease.
       Use a high base to ensure even duration-5000 (suborder 5200) < ease.
       Within ease, Tailwind orders alphabetically: in, in-out, linear, out. *)
    | Ease_in -> 100000
    | Ease_in_out -> 100001
    | Ease_linear -> 100002
    | Ease_out -> 100003
    | Ease_arbitrary _ -> 99999

  let ( >|= ) = Parse.( >|= )

  let of_class class_name =
    let parts = Parse.split_class class_name in
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
    | [ "transition"; value ] when Parse.is_bracket_value value ->
        Ok (Transition_arbitrary (Parse.bracket_inner value))
    | [ "transition" ] -> Ok Transition
    | [ "duration"; n ] when String.length n > 0 && n.[0] = '[' ->
        (* Arbitrary duration: duration-[300ms] *)
        let len = String.length n in
        if len > 2 && n.[len - 1] = ']' then
          let inner = String.sub n 1 (len - 2) in
          if String.ends_with ~suffix:"ms" inner then
            let num = String.sub inner 0 (String.length inner - 2) in
            match float_of_string_opt num with
            | Some _ ->
                Ok (Duration_arbitrary (inner, Css.Ms (float_of_string num)))
            | None -> Error (`Msg "Invalid duration value")
          else if String.ends_with ~suffix:"s" inner then
            let num = String.sub inner 0 (String.length inner - 1) in
            match float_of_string_opt num with
            | Some _ ->
                Ok (Duration_arbitrary (inner, Css.S (float_of_string num)))
            | None -> Error (`Msg "Invalid duration value")
          else Error (`Msg "Invalid duration unit")
        else Error (`Msg "Invalid arbitrary syntax")
    | [ "duration"; n ] ->
        Parse.int_pos ~name:"duration" n >|= fun n -> Duration n
    | [ "delay"; n ] when String.length n > 0 && n.[0] = '[' ->
        (* Arbitrary delay: delay-[300ms] *)
        let len = String.length n in
        if len > 2 && n.[len - 1] = ']' then
          let inner = String.sub n 1 (len - 2) in
          if String.ends_with ~suffix:"ms" inner then
            let num = String.sub inner 0 (String.length inner - 2) in
            match float_of_string_opt num with
            | Some _ ->
                Ok (Delay_arbitrary (inner, Css.Ms (float_of_string num)))
            | None -> Error (`Msg "Invalid delay value")
          else if String.ends_with ~suffix:"s" inner then
            let num = String.sub inner 0 (String.length inner - 1) in
            match float_of_string_opt num with
            | Some _ ->
                Ok (Delay_arbitrary (inner, Css.S (float_of_string num)))
            | None -> Error (`Msg "Invalid delay value")
          else Error (`Msg "Invalid delay unit")
        else Error (`Msg "Invalid arbitrary syntax")
    | [ "delay"; n ] -> Parse.int_pos ~name:"delay" n >|= fun n -> Delay n
    | [ "ease"; "linear" ] -> Ok Ease_linear
    | [ "ease"; "in" ] -> Ok Ease_in
    | [ "ease"; "out" ] -> Ok Ease_out
    | [ "ease"; "in"; "out" ] -> Ok Ease_in_out
    | [ "ease"; value ] when Parse.is_bracket_value value ->
        Ok (Ease_arbitrary (Parse.bracket_inner value))
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
    | Transition_arbitrary s -> "transition-[" ^ s ^ "]"
    | Duration n -> "duration-" ^ string_of_int n
    | Duration_arbitrary (s, _) -> "duration-[" ^ s ^ "]"
    | Delay n -> "delay-" ^ string_of_int n
    | Delay_arbitrary (s, _) -> "delay-[" ^ s ^ "]"
    | Ease_linear -> "ease-linear"
    | Ease_in -> "ease-in"
    | Ease_out -> "ease-out"
    | Ease_in_out -> "ease-in-out"
    | Ease_arbitrary s -> "ease-[" ^ s ^ "]"
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
