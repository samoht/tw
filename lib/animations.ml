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
    | No_animation
    | Spin
    | Ping
    | Pulse
    | Bounce
    (* The author's bracket text travels with the animation it denotes, so the
       class name is spelled exactly as it was written. *)
    | Bracket of string * [ `Animation of Css.animation | `Raw of string ]
    | Named of string

  let name = "animations"

  (* Match Tailwind ordering: animations after transforms, before cursor *)
  let priority _ = 10

  (* The animations Tailwind's default theme carries [@keyframes] for, in the
     order the theme declares them: a value naming several gets them back in
     that order. *)
  let builtin_keyframes =
    [
      ( "spin",
        Css.keyframes "spin"
          [
            {
              Css.Stylesheet.selector =
                Css.Keyframe.Positions [ Css.Keyframe.To ];
              declarations = [ Css.Declaration.transform (Rotate (Deg 360.)) ];
            };
          ] );
      ( "ping",
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
          ] );
      ( "pulse",
        Css.keyframes "pulse"
          [
            {
              Css.Stylesheet.selector =
                Css.Keyframe.Positions [ Css.Keyframe.Percent 50. ];
              declarations = [ Css.Declaration.opacity (Opacity_number 0.5) ];
            };
          ] );
      ( "bounce",
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
          ] );
    ]

  (* The animation a shorthand names, when it names one. *)
  let shorthand_name (anim : Css.animation) : string option =
    match anim with
    | Shorthand { name = Some (Name n | Ambiguous n | Quoted n); _ } -> Some n
    | _ -> opt_none

  (* The animations a [--animate-*] value names, read through the animation
     grammar rather than off the text, so a substring of a longer ident or a
     function argument cannot pass for a name. *)
  let animation_names value =
    let cursor = Cascade.Cursor.of_string value in
    match
      Cascade.Cursor.try_parse_full_err Css.Properties.read_animations cursor
    with
    | Ok anims -> List.filter_map shorthand_name anims
    | Error _ -> []

  (* Tailwind carries the built-in [@keyframes] of the animations a value names,
     not of the token holding it: a [@theme] redefining [--animate-ping] keeps
     [@keyframes ping] as long as the value still says [ping], one pointing the
     token at another built-in pulls that one in instead, and one naming an
     animation with no built-in keyframes gets none invented. *)
  let keyframes_rules names =
    match
      List.filter_map
        (fun (name, frames) ->
          if List.mem name names then Some frames else opt_none)
        builtin_keyframes
    with
    | [] -> opt_none
    | frames -> opt_some frames

  (* The keyframes for a [--animate-*] token, read off the theme's value for it
     and falling back to the animation the built-in default names. *)
  let theme_keyframes ?theme ~token default =
    match Scheme.theme_value theme token with
    | Some value -> keyframes_rules (animation_names value)
    | Option.None -> keyframes_rules (Option.to_list (shorthand_name default))

  let animate_none ?theme () =
    (* If theme defines --animate-none, use the theme variable. Otherwise use
       animation: none directly. *)
    match Scheme.theme_value theme "animate-none" with
    | Some _ ->
        let tv = Var.theme Css.Animation "animate-none" ~order:(7, 40) in
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

  (* Theme variable for animate-spin - slot (7, 41) places it after ease (7,
     30-34) *)
  let animate_spin_var = Var.theme Css.Animation "animate-spin" ~order:(7, 41)

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

  let animate_spin ?theme () =
    let theme_decl, spin_var = Var.binding animate_spin_var spin_animation in
    let rules = theme_keyframes ?theme ~token:"animate-spin" spin_animation in
    style ~rules [ theme_decl; Css.animation (Css.Var spin_var) ]

  (* Theme variable for animate-ping - slot (7, 42) places it after animate-spin
     (7, 41) *)
  let animate_ping_var = Var.theme Css.Animation "animate-ping" ~order:(7, 42)

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

  let animate_ping ?theme () =
    let theme_decl, ping_var = Var.binding animate_ping_var ping_animation in
    let rules = theme_keyframes ?theme ~token:"animate-ping" ping_animation in
    style ~rules [ theme_decl; Css.animation (Css.Var ping_var) ]

  (* Theme variable for animate-pulse - slot (7, 43) places it after
     animate-ping (7, 42) *)
  let animate_pulse_var = Var.theme Css.Animation "animate-pulse" ~order:(7, 43)

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

  let animate_pulse ?theme () =
    let theme_decl, pulse_var = Var.binding animate_pulse_var pulse_animation in
    let rules = theme_keyframes ?theme ~token:"animate-pulse" pulse_animation in
    style ~rules [ theme_decl; Css.animation (Css.Var pulse_var) ]

  (* Theme variable for animate-bounce - slot (7, 44) places it after
     animate-pulse (7, 43) *)
  let animate_bounce_var =
    Var.theme Css.Animation "animate-bounce" ~order:(7, 44)

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

  let animate_bounce ?theme () =
    let theme_decl, bounce_var =
      Var.binding animate_bounce_var bounce_animation
    in
    let rules =
      theme_keyframes ?theme ~token:"animate-bounce" bounce_animation
    in
    style ~rules [ theme_decl; Css.animation (Css.Var bounce_var) ]

  (* Publish the scale through the theme-token registry, the way rule.ml
     publishes the breakpoints, so [animate-[theme(--animate-spin)]] resolves
     and [theme(static)] emits it. [--animate-none] is not in Tailwind's default
     theme - the utility writes [animation: none] unless a project declares the
     token - so it stays out. *)
  let () =
    List.iter
      (fun (var, animation) -> Theme.register_default var animation)
      [
        (animate_spin_var, spin_animation);
        (animate_ping_var, ping_animation);
        (animate_pulse_var, pulse_animation);
        (animate_bounce_var, bounce_animation);
      ]

  (* Tailwind moves the animation name to the end of the shorthand. *)
  let animation_shorthand value =
    let css_value = Parse.decode_underscores value in
    match String.split_on_char ' ' css_value with
    | name :: (_ :: _ as rest) -> String.concat " " (rest @ [ name ])
    | _ -> css_value

  (* Parse a valid animation shorthand so keyframe metadata remains available.
     [None] leaves [of_class] to apply Tailwind's declaration-safe token-stream
     contract. *)
  let arbitrary_animation value : Css.animation option =
    let cursor = Cascade.Cursor.of_string (animation_shorthand value) in
    match
      Cascade.Cursor.try_parse_full_err Css.Properties.read_animation cursor
    with
    | Ok anim -> Some anim
    | Error _ -> None

  let animate_bracket = function
    | `Animation anim ->
        let rules = keyframes_rules (Option.to_list (shorthand_name anim)) in
        style ~rules [ Css.animation anim ]
    | `Raw value -> (
        match Parse.opaque_declaration "animation" value with
        | Some decl -> style [ decl ]
        | None -> assert false)

  let animate_named ?theme name =
    let var_name = "animate-" ^ name in
    let tv = Var.theme Css.Animation var_name ~order:(7, 45) in
    let animation : Css.animation =
      Shorthand
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
        }
    in
    let theme_decl, theme_ref = Var.binding tv animation in
    let rules = theme_keyframes ?theme ~token:var_name animation in
    style ~rules [ theme_decl; Css.animation (Css.Var theme_ref) ]

  let to_style theme =
    let animate_none () = animate_none ~theme () in
    let animate_spin () = animate_spin ~theme () in
    let animate_ping () = animate_ping ~theme () in
    let animate_pulse () = animate_pulse ~theme () in
    let animate_bounce () = animate_bounce ~theme () in
    function
    | No_animation -> animate_none ()
    | Spin -> animate_spin ()
    | Ping -> animate_ping ()
    | Pulse -> animate_pulse ()
    | Bounce -> animate_bounce ()
    | Bracket (_, value) -> animate_bracket value
    | Named name -> animate_named ~theme name

  (* Tailwind emits the animate-* rules in class-name order, with no family
     structure above it: a project animation sits among the built-ins wherever
     its own name falls, and an arbitrary one leads because the backslash its
     bracket is escaped with precedes every letter. One shared slot hands the
     whole family to the alphabetical tie-break, which is that order; numbering
     the built-ins apart pins every theme-declared name to one gap instead. *)
  let suborder _ = 0

  let of_class theme class_name =
    let parts = Parse.split_class class_name in
    match parts with
    | [ "animate"; "none" ] -> Ok No_animation
    | [ "animate"; "spin" ] -> Ok Spin
    | [ "animate"; "ping" ] -> Ok Ping
    | [ "animate"; "pulse" ] -> Ok Pulse
    | [ "animate"; "bounce" ] -> Ok Bounce
    | "animate" :: rest ->
        let value = String.concat "-" rest in
        if Parse.is_bracket_value value then
          let inner = Parse.bracket_inner value in
          match arbitrary_animation inner with
          | Some anim -> Ok (Bracket (inner, `Animation anim))
          | Option.None -> (
              match Parse.arbitrary_declaration_value inner with
              | Some value -> Ok (Bracket (inner, `Raw value))
              | None -> Error (`Msg "Invalid animation value"))
        else
          (* Check if it's a named animation with a theme value *)
          let var_name = "animate-" ^ value in
          if Scheme.theme_value (Some theme) var_name <> None then
            Ok (Named value)
          else Error (`Msg "Not an animation utility")
    | _ -> Error (`Msg "Not an animation utility")

  let to_class = function
    | No_animation -> "animate-none"
    | Spin -> "animate-spin"
    | Ping -> "animate-ping"
    | Pulse -> "animate-pulse"
    | Bounce -> "animate-bounce"
    | Bracket (v, _) -> "animate-[" ^ v ^ "]"
    | Named name -> "animate-" ^ name

  let examples = [ No_animation ]
end

open Handler
module Utility_factory = Utility.Make (Handler)

let utility = Utility_factory.v
let animate_none = utility No_animation
let animate_spin = utility Spin
let animate_ping = utility Ping
let animate_pulse = utility Pulse
let animate_bounce = utility Bounce
