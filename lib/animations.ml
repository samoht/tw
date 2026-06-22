(** Animation utilities

    What's included:
    - `animate-*` - Predefined animations (spin, ping, pulse, bounce).

    What's not:
    - Custom keyframe animations beyond the predefined ones.

    Parsing contract (`of_string`):
    - Accepts ["animate"; ...]. Unknown tokens yield `Error (`Msg "Not an
      animation utility")`. *)

module Css = Cascade.Css

let opt_none : 'a option = None
let opt_some x : 'a option = Some x

module Handler = struct
  open Style
  open Css

  type t =
    | Animate_none
    | Animate_spin
    | Animate_ping
    | Animate_pulse
    | Animate_bounce
    | Animate_bracket of string
    | Animate_named of string

  type Utility.base += Self of t

  let name = "animations"

  (* Match Tailwind ordering: animations after transforms, before cursor *)
  let priority = 10

  let animate_none ?theme () =
    (* If theme defines --animate-none, use the theme variable. Otherwise use
       animation: none directly. *)
    match Scheme.theme_value theme "animate-none" with
    | Some _ ->
        let tv = Var.theme Css.Animation "animate-none" ~order:(7, 12) in
        let none_animation : Css.animation =
          Css.Shorthand
            {
              name = Some Css.None;
              duration = None;
              timing_function = None;
              delay = None;
              iteration_count = None;
              direction = None;
              fill_mode = None;
              play_state = None;
              timeline = None;
            }
        in
        let theme_decl, none_var = Var.binding tv none_animation in
        style [ theme_decl; Css.animation (Css.Var none_var) ]
    | None -> style [ Css.animation None ]

  (* Theme variable for animate-spin - order (7, 13) places it after ease (7,
     9-11) *)
  let animate_spin_var = Var.theme Css.Animation "animate-spin" ~order:(7, 13)

  let animate_spin ?theme () =
    let spin_animation : Css.animation =
      Css.Shorthand
        {
          name = Some (Name "spin");
          duration = Some (S 1.0);
          timing_function = Some Linear;
          delay = None;
          iteration_count = Some Infinite;
          direction = None;
          fill_mode = None;
          play_state = None;
          timeline = None;
        }
    in
    let theme_decl, spin_var = Var.binding animate_spin_var spin_animation in
    (* Only include @keyframes when theme doesn't define the animation *)
    let rules =
      if Scheme.theme_value theme "animate-spin" <> opt_none then opt_none
      else
        opt_some
          [
            Css.keyframes "spin"
              [
                {
                  Css.Stylesheet.selector =
                    Css.Keyframe.Positions [ Css.Keyframe.To ];
                  declarations =
                    [ Css.Declaration.transform (Rotate (Deg 360.)) ];
                };
              ];
          ]
    in
    style ~rules [ theme_decl; Css.animation (Css.Var spin_var) ]

  (* Theme variable for animate-ping - order (7, 14) places it after
     animate-spin (7, 13) *)
  let animate_ping_var = Var.theme Css.Animation "animate-ping" ~order:(7, 14)

  let animate_ping ?theme () =
    let ping_animation : Css.animation =
      Css.Shorthand
        {
          name = Some (Name "ping");
          duration = Some (S 1.0);
          timing_function = Some (Cubic_bezier (0.0, 0.0, 0.2, 1.0));
          delay = None;
          iteration_count = Some Infinite;
          direction = None;
          fill_mode = None;
          play_state = None;
          timeline = None;
        }
    in
    let theme_decl, ping_var = Var.binding animate_ping_var ping_animation in
    let rules =
      if Scheme.theme_value theme "animate-ping" <> opt_none then opt_none
      else
        opt_some
          [
            Css.keyframes "ping"
              [
                {
                  Css.Stylesheet.selector =
                    Css.Keyframe.Positions
                      [ Css.Keyframe.Percent 75.; Css.Keyframe.To ];
                  declarations =
                    [
                      Css.Declaration.opacity (Opacity_number 0.0);
                      Css.Declaration.transform (Scale (Num 2.0, opt_none));
                    ];
                };
              ];
          ]
    in
    style ~rules [ theme_decl; Css.animation (Css.Var ping_var) ]

  (* Theme variable for animate-pulse - order (7, 15) places it after
     animate-ping (7, 14) *)
  let animate_pulse_var = Var.theme Css.Animation "animate-pulse" ~order:(7, 15)

  let animate_pulse ?theme () =
    let pulse_animation : Css.animation =
      Css.Shorthand
        {
          name = Some (Name "pulse");
          duration = Some (S 2.0);
          timing_function = Some (Cubic_bezier (0.4, 0., 0.6, 1.));
          delay = None;
          iteration_count = Some Infinite;
          direction = None;
          fill_mode = None;
          play_state = None;
          timeline = None;
        }
    in
    let theme_decl, pulse_var = Var.binding animate_pulse_var pulse_animation in
    let rules =
      if Scheme.theme_value theme "animate-pulse" <> opt_none then opt_none
      else
        opt_some
          [
            Css.keyframes "pulse"
              [
                {
                  Css.Stylesheet.selector =
                    Css.Keyframe.Positions [ Css.Keyframe.Percent 50. ];
                  declarations =
                    [ Css.Declaration.opacity (Opacity_number 0.5) ];
                };
              ];
          ]
    in
    style ~rules [ theme_decl; Css.animation (Css.Var pulse_var) ]

  (* Theme variable for animate-bounce - order (7, 16) places it after
     animate-pulse (7, 15) *)
  let animate_bounce_var =
    Var.theme Css.Animation "animate-bounce" ~order:(7, 16)

  let animate_bounce ?theme () =
    let bounce_animation : Css.animation =
      Css.Shorthand
        {
          name = Some (Name "bounce");
          duration = Some (S 1.0);
          timing_function = None;
          delay = None;
          iteration_count = Some Infinite;
          direction = None;
          fill_mode = None;
          play_state = None;
          timeline = None;
        }
    in
    let theme_decl, bounce_var =
      Var.binding animate_bounce_var bounce_animation
    in
    let rules =
      if Scheme.theme_value theme "animate-bounce" <> opt_none then opt_none
      else
        opt_some
          [
            Css.keyframes "bounce"
              [
                {
                  Css.Stylesheet.selector =
                    Css.Keyframe.Positions
                      [ Css.Keyframe.Percent 0.; Css.Keyframe.To ];
                  declarations =
                    [
                      Css.Declaration.animation_timing_function
                        (Cubic_bezier (0.8, 0., 1., 1.));
                      Css.Declaration.transform (Translate_y (Pct (-25.)));
                    ];
                };
                {
                  Css.Stylesheet.selector =
                    Css.Keyframe.Positions [ Css.Keyframe.Percent 50. ];
                  declarations =
                    [
                      Css.Declaration.animation_timing_function
                        (Cubic_bezier (0., 0., 0.2, 1.));
                      Css.Declaration.transform None;
                    ];
                };
              ];
          ]
    in
    style ~rules [ theme_decl; Css.animation (Css.Var bounce_var) ]

  (* Known @keyframes for bracket animation references *)
  let spin_keyframes =
    Css.keyframes "spin"
      [
        {
          Css.Stylesheet.selector = Css.Keyframe.Positions [ Css.Keyframe.To ];
          declarations = [ Css.Declaration.transform (Rotate (Deg 360.)) ];
        };
      ]

  let known_keyframes = function
    | "spin" -> Some spin_keyframes
    | _ -> Option.None

  let animate_bracket value =
    let css_value = String.map (fun c -> if c = '_' then ' ' else c) value in
    let parts = String.split_on_char ' ' css_value in
    (* Tailwind moves the animation name to the end of the shorthand *)
    let anim_name, reordered =
      match parts with
      | name :: rest when rest <> [] ->
          (name, String.concat " " (rest @ [ name ]))
      | _ -> (css_value, css_value)
    in
    let rules =
      match known_keyframes anim_name with
      | Some kf -> opt_some [ kf ]
      | Option.None -> opt_none
    in
    let cursor = Cascade.Cursor.of_string reordered in
    match
      Cascade.Cursor.try_parse_full_err Css.Properties.read_animation cursor
    with
    | Ok anim -> style ~rules [ Css.animation anim ]
    | Error _ -> invalid_arg ("animate-[" ^ value ^ "]: not a valid animation")

  let animate_named name =
    let var_name = "animate-" ^ name in
    let tv = Var.theme Css.Animation var_name ~order:(7, 16) in
    let theme_decl, theme_ref =
      Var.binding tv
        (Shorthand
           {
             name = Some (Name name);
             duration = None;
             timing_function = None;
             delay = None;
             iteration_count = None;
             direction = None;
             fill_mode = None;
             play_state = None;
             timeline = None;
           })
    in
    style [ theme_decl; Css.animation (Css.Var theme_ref) ]

  let to_style theme =
    let animate_none () = animate_none ~theme () in
    let animate_spin () = animate_spin ~theme () in
    let animate_ping () = animate_ping ~theme () in
    let animate_pulse () = animate_pulse ~theme () in
    let animate_bounce () = animate_bounce ~theme () in
    function
    | Animate_none -> animate_none ()
    | Animate_spin -> animate_spin ()
    | Animate_ping -> animate_ping ()
    | Animate_pulse -> animate_pulse ()
    | Animate_bounce -> animate_bounce ()
    | Animate_bracket value -> animate_bracket value
    | Animate_named name -> animate_named name

  let suborder = function
    | Animate_bracket _ -> 0
    | Animate_bounce -> 1
    | Animate_named _ -> 2
    | Animate_none -> 3
    | Animate_ping -> 4
    | Animate_pulse -> 5
    | Animate_spin -> 6

  let of_class theme class_name =
    let parts = Parse.split_class class_name in
    match parts with
    | [ "animate"; "none" ] -> Ok Animate_none
    | [ "animate"; "spin" ] -> Ok Animate_spin
    | [ "animate"; "ping" ] -> Ok Animate_ping
    | [ "animate"; "pulse" ] -> Ok Animate_pulse
    | [ "animate"; "bounce" ] -> Ok Animate_bounce
    | "animate" :: rest ->
        let value = String.concat "-" rest in
        if Parse.is_bracket_value value then
          Ok (Animate_bracket (Parse.bracket_inner value))
        else
          (* Check if it's a named animation with a theme value *)
          let var_name = "animate-" ^ value in
          if Scheme.theme_value (Some theme) var_name <> None then
            Ok (Animate_named value)
          else Error (`Msg "Not an animation utility")
    | _ -> Error (`Msg "Not an animation utility")

  let to_class = function
    | Animate_none -> "animate-none"
    | Animate_spin -> "animate-spin"
    | Animate_ping -> "animate-ping"
    | Animate_pulse -> "animate-pulse"
    | Animate_bounce -> "animate-bounce"
    | Animate_bracket v -> "animate-[" ^ v ^ "]"
    | Animate_named name -> "animate-" ^ name
end

open Handler

let () = Utility.register (module Handler)
let utility x = Utility.base (Self x)
let animate_none = utility Animate_none
let animate_spin = utility Animate_spin
let animate_ping = utility Animate_ping
let animate_pulse = utility Animate_pulse
let animate_bounce = utility Animate_bounce
