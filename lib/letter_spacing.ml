(** Letter spacing utilities ([tracking-*]). *)

module Css = Cascade.Css

(* Theme variables for named tracking values *)
let tracking_tighter_var = Var.theme Css.Length "tracking-tighter" ~order:(6, 39)
let tracking_tight_var = Var.theme Css.Length "tracking-tight" ~order:(6, 40)
let tracking_normal_var = Var.theme Css.Length "tracking-normal" ~order:(6, 41)
let tracking_wide_var = Var.theme Css.Length "tracking-wide" ~order:(6, 42)
let tracking_wider_var = Var.theme Css.Length "tracking-wider" ~order:(6, 43)
let tracking_widest_var = Var.theme Css.Length "tracking-widest" ~order:(6, 44)

(* A [--tracking-*] token the project declared names a letter spacing the
   built-in scale has no slot for; they share the slot after the scale, the way
   the project's font families and text sizes share theirs. *)
let tracking_named_cache = Domain_cache.v 8

let tracking_named_var name =
  Domain_cache.or_add tracking_named_cache name (fun () ->
      Var.theme Css.Length ("tracking-" ^ name) ~order:(6, 45))

(* Publish the tracking scale through the theme-token registry, the way rule.ml
   publishes the breakpoints, so [tracking-[theme(--tracking-wide)]] resolves
   and [theme(static)] emits it. *)
let () =
  List.iter
    (fun (var, spacing) -> Theme.register_default var spacing)
    [
      (tracking_tighter_var, (Em (-0.05) : Css.length));
      (tracking_tight_var, Em (-0.025));
      (* Tailwind's default theme spells the normal tracking [0em], not a
         unitless zero. *)
      (tracking_normal_var, Em 0.0);
      (tracking_wide_var, Em 0.025);
      (tracking_wider_var, Em 0.05);
      (tracking_widest_var, Em 0.1);
    ]

let tracking_var =
  (* Tracking doesn't appear in @layer properties in standard Tailwind - only
     referenced *)
  Var.channel ~needs_property:true ~property_order:101 ~family:`Tracking
    Css.Length "tw-tracking"

module Handler = struct
  open Style
  open Css

  type t =
    | (* The author's bracket text travels with the length it denotes, so the
         class name is spelled exactly as it was written. *)
      Tracking_arbitrary of string * Css.length
    | Neg_tracking_arbitrary of string * Css.length
    | Tracking_raw of string * string
    | Neg_tracking_raw of string * string
    | Tracking_var of string
    | Neg_tracking_var of string
    | Tracking_tighter
    | Tracking_tight
    | Tracking_normal
    | Tracking_wide
    | Tracking_wider
    | Tracking_widest
    | (* Letter spacing named by a project [--tracking-*] token *)
      Tracking_theme of string

  let name = "letter_spacing"

  (* Tailwind ranks [letter-spacing] at 289, which opens the priority-26 band
     the late text families share: text-wrap (290) and the word-wrapping
     families follow in lib/line_breaking.ml. *)
  let priority _ = 26
  let err_not_utility = Error (`Msg "Not a letter-spacing utility")

  (* The length a [tracking-[...]] bracket denotes, read with cascade's own
     grammar so [calc()] and the function forms come for free. [None] is a
     bracket no length reads from, and [of_class] refuses the utility rather
     than leaving [to_style] to raise. *)
  let arbitrary_tracking inner : Css.length option =
    Css.parse_length (Parse.decode_underscores inner)

  (* A letter spacing the project named in its [@theme]. The built-in scale is
     matched before this is consulted, so those names never reach here. *)
  let is_theme_tracking theme n =
    Scheme.theme_value (Some theme) ("tracking-" ^ n) <> None

  let of_class theme class_name =
    let parts = Parse.split_class class_name in
    match parts with
    | [ "tracking"; v ] when Parse.is_bracket_value v -> (
        let inner = Parse.bracket_inner v in
        if Parse.is_var inner then Ok (Tracking_var inner)
        else
          match arbitrary_tracking inner with
          | Some len -> Ok (Tracking_arbitrary (inner, len))
          | None -> (
              match Parse.arbitrary_declaration_value inner with
              | Some value -> Ok (Tracking_raw (inner, value))
              | None -> Error (`Msg ("Invalid tracking value: " ^ inner))))
    | "" :: "tracking" :: rest when rest <> [] -> (
        let value = String.concat "-" rest in
        if not (Parse.is_bracket_value value) then err_not_utility
        else
          let inner = Parse.bracket_inner value in
          if Parse.is_var inner then Ok (Neg_tracking_var inner)
          else
            match arbitrary_tracking inner with
            | Some len -> Ok (Neg_tracking_arbitrary (inner, len))
            | None -> (
                match Parse.arbitrary_declaration_value inner with
                | Some value -> Ok (Neg_tracking_raw (inner, value))
                | None -> Error (`Msg ("Invalid tracking value: " ^ inner))))
    | [ "tracking"; "tighter" ] -> Ok Tracking_tighter
    | [ "tracking"; "tight" ] -> Ok Tracking_tight
    | [ "tracking"; "normal" ] -> Ok Tracking_normal
    | [ "tracking"; "wide" ] -> Ok Tracking_wide
    | [ "tracking"; "wider" ] -> Ok Tracking_wider
    | [ "tracking"; "widest" ] -> Ok Tracking_widest
    | [ "tracking"; name ] when is_theme_tracking theme name ->
        Ok (Tracking_theme name)
    | "tracking" :: (_ :: _ :: _ as rest)
      when is_theme_tracking theme (String.concat "-" rest) ->
        Ok (Tracking_theme (String.concat "-" rest))
    | _ -> err_not_utility

  let to_class = function
    | Tracking_arbitrary (v, _) -> "tracking-[" ^ v ^ "]"
    | Neg_tracking_arbitrary (v, _) -> "-tracking-[" ^ v ^ "]"
    | Tracking_raw (v, _) -> "tracking-[" ^ v ^ "]"
    | Neg_tracking_raw (v, _) -> "-tracking-[" ^ v ^ "]"
    | Tracking_var v -> "tracking-[" ^ v ^ "]"
    | Neg_tracking_var v -> "-tracking-[" ^ v ^ "]"
    | Tracking_tighter -> "tracking-tighter"
    | Tracking_tight -> "tracking-tight"
    | Tracking_normal -> "tracking-normal"
    | Tracking_wide -> "tracking-wide"
    | Tracking_wider -> "tracking-wider"
    | Tracking_widest -> "tracking-widest"
    | Tracking_theme name -> "tracking-" ^ name

  let suborder = function
    (* Tracking — negative first, then arbitrary, then named *)
    | Neg_tracking_arbitrary _ -> 8245
    | Neg_tracking_raw _ -> 8245
    | Neg_tracking_var _ -> 8250
    | Tracking_arbitrary _ -> 8255
    | Tracking_raw _ -> 8255
    | Tracking_var _ -> 8260
    (* Alphabetical by class name: normal, tight, tighter, wide, wider,
       widest *)
    | Tracking_normal -> 8300
    | Tracking_tight -> 8301
    | Tracking_tighter -> 8302
    | Tracking_wide -> 8303
    | Tracking_wider -> 8304
    | Tracking_widest -> 8305
    | Tracking_theme _ -> 8306

  let tracking_tighter =
    let theme_decl, theme_ref = Var.binding tracking_tighter_var (Em (-0.05)) in
    let channel_decl = Var.set tracking_var (Css.Var theme_ref) in
    let property_rules =
      Var.property_rule tracking_var |> Option.to_list |> Css.concat
    in
    style ~property_rules
      [ theme_decl; channel_decl; letter_spacing (Css.Var theme_ref) ]

  let tracking_tight =
    (* Theme var: --tracking-tight: -0.025em *)
    let theme_decl, theme_ref = Var.binding tracking_tight_var (Em (-0.025)) in
    (* Channel var: --tw-tracking: var(--tracking-tight) *)
    let channel_decl = Var.set tracking_var (Css.Var theme_ref) in
    (* Property: letter-spacing: var(--tracking-tight) *)
    let property_rules =
      Var.property_rule tracking_var |> Option.to_list |> Css.concat
    in
    style ~property_rules
      [ theme_decl; channel_decl; letter_spacing (Css.Var theme_ref) ]

  let tracking_normal () =
    (* Tailwind's default theme defines --tracking-normal as 0em, not a unitless
       0, so keep the explicit em unit. *)
    let theme_decl, theme_ref = Var.binding tracking_normal_var (Em 0.0) in
    let channel_decl = Var.set tracking_var (Css.Var theme_ref) in
    let property_rules =
      Var.property_rule tracking_var |> Option.to_list |> Css.concat
    in
    style ~property_rules
      [ theme_decl; channel_decl; letter_spacing (Css.Var theme_ref) ]

  let tracking_wide =
    let theme_decl, theme_ref = Var.binding tracking_wide_var (Em 0.025) in
    let channel_decl = Var.set tracking_var (Css.Var theme_ref) in
    let property_rules =
      Var.property_rule tracking_var |> Option.to_list |> Css.concat
    in
    style ~property_rules
      [ theme_decl; channel_decl; letter_spacing (Css.Var theme_ref) ]

  let tracking_theme theme name =
    let token = "tracking-" ^ name in
    match Scheme.theme_value (Some theme) token with
    | Stdlib.Option.None -> style []
    | Stdlib.Option.Some raw -> (
        match Css.parse_length raw with
        | Stdlib.Option.None -> style []
        | Stdlib.Option.Some len ->
            let theme_decl, theme_ref =
              Var.binding (tracking_named_var name) len
            in
            let channel_decl = Var.set tracking_var (Css.Var theme_ref) in
            let property_rules =
              Var.property_rule tracking_var |> Option.to_list |> Css.concat
            in
            style ~property_rules
              [ theme_decl; channel_decl; letter_spacing (Css.Var theme_ref) ])

  let tracking_wider =
    let theme_decl, theme_ref = Var.binding tracking_wider_var (Em 0.05) in
    let channel_decl = Var.set tracking_var (Css.Var theme_ref) in
    let property_rules =
      Var.property_rule tracking_var |> Option.to_list |> Css.concat
    in
    style ~property_rules
      [ theme_decl; channel_decl; letter_spacing (Css.Var theme_ref) ]

  let tracking_widest =
    let theme_decl, theme_ref = Var.binding tracking_widest_var (Em 0.1) in
    let channel_decl = Var.set tracking_var (Css.Var theme_ref) in
    let property_rules =
      Var.property_rule tracking_var |> Option.to_list |> Css.concat
    in
    style ~property_rules
      [ theme_decl; channel_decl; letter_spacing (Css.Var theme_ref) ]

  let tracking_arbitrary len =
    let channel_decl = Var.set tracking_var len in
    let property_rules =
      Var.property_rule tracking_var |> Option.to_list |> Css.concat
    in
    style ~property_rules [ channel_decl; letter_spacing len ]

  let tracking_raw ?(negative = false) value =
    let value = if negative then "calc(" ^ value ^ " * -1)" else value in
    let channel_decl =
      Css.custom_property ~layer:"utilities" (Var.css_name tracking_var) value
    in
    let property_rules =
      Var.property_rule tracking_var |> Option.to_list |> Css.concat
    in
    let declarations =
      channel_decl
      :: Option.to_list (Parse.opaque_declaration "letter-spacing" value)
    in
    style ~metadata:[ Var.metadata tracking_var ] ~property_rules declarations

  let negate_length : Css.length -> Css.length = function
    | Px n -> Px (-.n)
    | Em n -> Em (-.n)
    | Rem n -> Rem (-.n)
    | Cm n -> Cm (-.n)
    | Mm n -> Mm (-.n)
    | Pt n -> Pt (-.n)
    | Pc n -> Pc (-.n)
    | Ex n -> Ex (-.n)
    | Ch n -> Ch (-.n)
    | Lh n -> Lh (-.n)
    | Vw n -> Vw (-.n)
    | Vh n -> Vh (-.n)
    | Pct n -> Pct (-.n)
    | len -> Calc (Calc.mul (Calc.length len) (Calc.float (-1.)))

  let neg_tracking_arbitrary len =
    (* --tw-tracking uses calc() form, letter-spacing uses direct negation *)
    let calc_neg : Css.length =
      Calc (Calc.mul (Calc.length len) (Calc.float (-1.)))
    in
    let direct_neg = negate_length len in
    let channel_decl = Var.set tracking_var calc_neg in
    let property_rules =
      Var.property_rule tracking_var |> Option.to_list |> Css.concat
    in
    style ~property_rules [ channel_decl; letter_spacing direct_neg ]

  let to_style theme = function
    | Tracking_arbitrary (_, len) -> tracking_arbitrary len
    | Neg_tracking_arbitrary (_, len) -> neg_tracking_arbitrary len
    | Tracking_raw (_, value) -> tracking_raw value
    | Neg_tracking_raw (_, value) -> tracking_raw ~negative:true value
    | Tracking_var v ->
        let bare_name = Parse.extract_var_name v in
        let var_ref : Css.length Css.var = Var.bracket bare_name in
        let channel_decl = Var.set tracking_var (Css.Var var_ref) in
        let property_rules =
          Var.property_rule tracking_var |> Option.to_list |> Css.concat
        in
        style ~property_rules [ channel_decl; letter_spacing (Css.Var var_ref) ]
    | Neg_tracking_var v ->
        let bare_name = Parse.extract_var_name v in
        let neg_len : Css.length =
          Calc (Calc.mul (Calc.var bare_name) (Calc.float (-1.)))
        in
        let channel_decl = Var.set tracking_var neg_len in
        let property_rules =
          Var.property_rule tracking_var |> Option.to_list |> Css.concat
        in
        style ~property_rules [ channel_decl; letter_spacing neg_len ]
    | Tracking_tighter -> tracking_tighter
    | Tracking_tight -> tracking_tight
    | Tracking_normal -> tracking_normal ()
    | Tracking_wide -> tracking_wide
    | Tracking_wider -> tracking_wider
    | Tracking_widest -> tracking_widest
    | Tracking_theme name -> tracking_theme theme name

  let examples = [ Tracking_normal ]
end

open Handler
module Utility_factory = Utility.Make (Handler)

let utility = Utility_factory.v
let tracking_tighter = utility Tracking_tighter
let tracking_tight = utility Tracking_tight
let tracking_normal = utility Tracking_normal
let tracking_wide = utility Tracking_wide
let tracking_wider = utility Tracking_wider
let tracking_widest = utility Tracking_widest
