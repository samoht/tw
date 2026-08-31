(** Margin utilities with negative value support. *)

module Css = Cascade.Css

module Handler = struct
  open Style
  open Css

  type signed =
    | Spacing of Style.spacing
    | Arbitrary of string * Css.length (* mx-[4px], raw kept for round-trip *)
    | Arbitrary_var of string (* mx-[var(--value)] *)
    | Named of string (* mx-big - custom spacing *)

  (* [Auto] sits outside [signed] because there is no [-m-auto] and negating
     [auto] means nothing. Carrying the sign in the value rather than beside it
     is what makes that pair unrepresentable instead of a case [to_style] has to
     turn away. *)
  type margin_value = Auto | Positive of signed | Negative of signed

  type t = {
    axis : [ `All | `X | `Y | `T | `R | `B | `L | `S | `E | `Bs | `Be ];
    value : margin_value;
  }
  (** Local margin utility type *)

  (** Extensible variant for margin utilities *)

  let name = "margin"
  let priority _ = 2

  (** {2 Typed Margin Utilities} *)

  let named_margin_value ?theme name : Css.declaration option * Css.length =
    let prop_name = "spacing-" ^ name in
    match Scheme.theme_value theme prop_name with
    | Some value_str ->
        let decl =
          Css.custom_property ~layer:"theme" ("--" ^ prop_name) value_str
        in
        let ref : Css.length Css.var =
          Var.theme_ref prop_name
            ~default:(Css.Zero : Css.length)
            ~default_css:"0px"
        in
        (Some decl, Css.Var ref)
    | None ->
        let ref = Spacing.named_spacing_ref name in
        (None, ref)

  (** {1 Conversion Functions} *)

  (* Spacing keywords sort by their suffix's first character, matching
     Tailwind's raw order after the numeric rem values: auto ('a') < a named
     spacing like big ('b') < full ('f') < px ('p'). *)
  let keyword_order c = 100000 + Char.code c

  let spacing_value_order = function
    | `Rem f ->
        let units = f /. 0.25 in
        int_of_float (units *. 10.)
    | `Named s -> keyword_order (if String.length s > 0 then s.[0] else '~')
    | `Full -> keyword_order 'f'
    | `Px -> keyword_order 'p'

  (* Get the CSS property function for an axis *)
  let prop_for_axis axis =
    match axis with
    | `All -> fun len -> margin [ len ]
    | `X -> margin_inline
    | `Y -> margin_block
    | `T -> margin_top
    | `R -> margin_right
    | `B -> margin_bottom
    | `L -> margin_left
    | `S -> margin_inline_start
    | `E -> margin_inline_end
    | `Bs -> margin_block_start
    | `Be -> margin_block_end

  (** Convert margin utility to style *)
  let to_style theme { axis; value } =
    let prop = prop_for_axis axis in
    let named_margin_value name = named_margin_value ~theme name in
    let spacing_style ~negative s =
      (* The typed constructors keep the scale they were handed, so [m (-4)]
         stores a negative rem while [-m-4] parses to a positive one. Both spell
         the same class, so the sign comes from [Negative] alone. *)
      let s = match s with `Rem f -> `Rem (Float.abs f) | other -> other in
      let decl, len = Spacing.to_decl_len ~theme ~negative s in
      style (Option.to_list decl @ [ prop len ])
    in
    let of_signed ~negative = function
      | Spacing s -> spacing_style ~negative s
      | Arbitrary (_, len) ->
          if negative then
            (* A plain unit negates directly, [-4px] rather than a calc with a
               factor of -1. *)
            let neg_len : Css.length =
              match len with
              | Px f -> Px (-.f)
              | Rem f -> Rem (-.f)
              | Pct f -> Pct (-.f)
              | _ -> Calc (Calc.mul (Calc.length len) (Calc.float (-1.)))
            in
            style [ prop neg_len ]
          else style [ prop len ]
      | Arbitrary_var var_str ->
          let bare_name = Parse.extract_var_name var_str in
          let len : Css.length =
            if negative then
              Calc (Calc.mul (Calc.var bare_name) (Calc.float (-1.)))
            else Var (Var.bracket bare_name)
          in
          style [ prop len ]
      | Named name ->
          let decl_opt, len = named_margin_value name in
          let decls = Option.to_list decl_opt in
          if negative then
            style
              (decls
              @ [ prop (Calc (Calc.mul (Calc.length len) (Calc.float (-1.)))) ]
              )
          else style (decls @ [ prop len ])
    in
    match value with
    | Auto -> style [ prop Auto ]
    | Positive v -> of_signed ~negative:false v
    | Negative v -> of_signed ~negative:true v

  let suborder { axis; value } =
    (* Tailwind orders margins by side first, then sign (negatives before
       positives within a side), then value. Side spacing (1_000_000) exceeds
       the sign+value range so the tiers never cross. *)
    let side_index =
      match axis with
      | `All -> 0
      | `X -> 1
      | `Y -> 2
      | `S -> 3
      | `E -> 4
      | `Bs -> 5
      | `Be -> 6
      | `T -> 7
      | `R -> 8
      | `B -> 9
      | `L -> 10
    in
    let sign_offset =
      match value with Negative _ -> 0 | Auto | Positive _ -> 200000
    in
    let value_order =
      match value with
      | Auto -> keyword_order 'a'
      | Positive (Spacing s) | Negative (Spacing s) -> spacing_value_order s
      | Positive (Arbitrary _) | Negative (Arbitrary _) ->
          50000 (* after numbered, before auto *)
      | Positive (Arbitrary_var _) | Negative (Arbitrary_var _) -> 55000
      | Positive (Named _) | Negative (Named _) -> 60000 (* after arbitrary *)
    in
    (side_index * 1000000) + sign_offset + value_order

  let to_class { axis; value } =
    let prefix =
      match axis with
      | `All -> "m-"
      | `X -> "mx-"
      | `Y -> "my-"
      | `T -> "mt-"
      | `R -> "mr-"
      | `B -> "mb-"
      | `L -> "ml-"
      | `S -> "ms-"
      | `E -> "me-"
      | `Bs -> "mbs-"
      | `Be -> "mbe-"
    in
    let neg_prefix =
      match value with Negative _ -> "-" | Auto | Positive _ -> ""
    in
    let value_suffix =
      match value with
      | Auto -> Spacing.pp_margin_suffix `Auto
      | Positive (Spacing s) | Negative (Spacing s) ->
          Spacing.pp_margin_suffix (s :> margin)
      | Positive (Arbitrary (raw, _)) | Negative (Arbitrary (raw, _)) ->
          "[" ^ raw ^ "]"
      | Positive (Arbitrary_var s) | Negative (Arbitrary_var s) -> "[" ^ s ^ "]"
      | Positive (Named name) | Negative (Named name) -> name
    in
    neg_prefix ^ prefix ^ value_suffix

  let parse_arbitrary s : signed option =
    (* Parse [4px], [1rem], [50%], [-5cqw], [calc(...)], or [var(--value)]. The
       raw inner is kept verbatim for the class name; the value goes through the
       full length grammar so any unit or calc() is accepted. *)
    let len = String.length s in
    if len > 2 && s.[0] = '[' && s.[len - 1] = ']' then
      let inner = String.sub s 1 (len - 2) in
      if Parse.is_var inner then Some (Arbitrary_var inner)
      else
        match Css.parse_length (Parse.normalize_css_math_operators inner) with
        | Some l -> Some (Arbitrary (inner, l))
        | None -> None
    else None

  let axis_of_prefix_ext = function
    | "m" -> Some `All
    | "mx" -> Some `X
    | "my" -> Some `Y
    | "mt" -> Some `T
    | "mr" -> Some `R
    | "mb" -> Some `B
    | "ml" -> Some `L
    | "ms" -> Some `S
    | "me" -> Some `E
    | "mbs" -> Some `Bs
    | "mbe" -> Some `Be
    | _ -> None

  (** Check if a prefix is an extended margin prefix (ms, me, mbs, mbe) *)
  let is_extended_margin_prefix = function
    | "ms" | "me" | "mbs" | "mbe" -> true
    | _ -> false

  (* A named margin (mx-big) is valid only when the theme actually defines the
     [--spacing-<name>] token; otherwise a stray source token like [my-form]
     would parse as a utility and emit a bogus var(). *)
  let is_named_spacing theme name =
    Scheme.theme_value theme ("spacing-" ^ name) <> None

  let sign ~is_negative v = if is_negative then Negative v else Positive v

  (** Parse value to standard or named margin *)
  let parse_value ?theme ~is_negative value =
    let allow_auto = not is_negative in
    match Spacing.parse_value_string ?theme ~allow_auto value with
    | Some (#spacing as spacing_val) ->
        Some { axis = `All; value = sign ~is_negative (Spacing spacing_val) }
    | Some `Auto when not is_negative -> Some { axis = `All; value = Auto }
    | None
      when (not is_negative)
           && Parse.is_valid_theme_name value
           && is_named_spacing theme value ->
        (* Try as a named spacing: mx-big *)
        Some { axis = `All; value = Positive (Named value) }
    | _ -> None

  (** Parse string parts to margin utility using shared logic *)
  let of_class theme class_name =
    let parts = Parse.split_class class_name in
    match parts with
    (* Handle arbitrary values: mx-[4px], mx-[var(--value)] *)
    | [ prefix; arb ] when String.length arb > 0 && arb.[0] = '[' -> (
        match (axis_of_prefix_ext prefix, parse_arbitrary arb) with
        | Some axis, Some value -> Ok { axis; value = Positive value }
        | _ -> Error (`Msg "Not a margin utility"))
    (* Handle negative arbitrary: -mx-[4px], -mx-[var(--value)] *)
    | [ ""; prefix; arb ] when String.length arb > 0 && arb.[0] = '[' -> (
        match (axis_of_prefix_ext prefix, parse_arbitrary arb) with
        | Some axis, Some value -> Ok { axis; value = Negative value }
        | _ -> Error (`Msg "Not a margin utility"))
    (* Handle extended axes (ms, me, mbs, mbe) with values *)
    | [ prefix; value ] when is_extended_margin_prefix prefix -> (
        match
          ( axis_of_prefix_ext prefix,
            parse_value ~theme ~is_negative:false value )
        with
        | Some axis, Some t -> Ok { t with axis }
        | _ -> Error (`Msg "Not a margin utility"))
    (* Handle negative extended axes: -ms-4 *)
    | [ ""; prefix; value ] when is_extended_margin_prefix prefix -> (
        match
          (axis_of_prefix_ext prefix, parse_value ~theme ~is_negative:true value)
        with
        | Some axis, Some t -> Ok { t with axis }
        | _ -> Error (`Msg "Not a margin utility"))
    (* Use existing Spacing parser for standard values *)
    | _ -> (
        match Spacing.parse_class_parts parts with
        | Some (is_negative, prefix, value) -> (
            if not (Spacing.is_margin_prefix prefix) then
              Error (`Msg "Not a margin utility")
            else
              match Spacing.axis_of_prefix prefix with
              | None -> Error (`Msg "Not a margin utility")
              | Some axis -> (
                  let axis =
                    match axis with
                    | `All -> `All
                    | `X -> `X
                    | `Y -> `Y
                    | `T -> `T
                    | `R -> `R
                    | `B -> `B
                    | `L -> `L
                    | `S -> `S
                    | `E -> `E
                    | `Bs -> `Bs
                    | `Be -> `Be
                  in
                  let allow_auto = not is_negative in
                  match Spacing.parse_value_string ~theme ~allow_auto value with
                  | None ->
                      (* Try as a named spacing: mx-big, -mx-big *)
                      if
                        Parse.is_valid_theme_name value
                        && is_named_spacing (Some theme) value
                      then Ok { axis; value = sign ~is_negative (Named value) }
                      else Error (`Msg "Not a margin utility")
                  | Some (#spacing as spacing_val) ->
                      Ok
                        {
                          axis;
                          value = sign ~is_negative (Spacing spacing_val);
                        }
                  | Some `Auto ->
                      if is_negative then Error (`Msg "Not a margin utility")
                      else Ok { axis; value = Auto }))
        | None -> Error (`Msg "Not a margin utility"))

  let examples =
    List.map
      (fun axis -> { axis; value = Auto })
      [ `All; `X; `Y; `T; `R; `B; `L; `S; `E; `Bs; `Be ]
end

open Handler
module Utility_factory = Utility.Make (Handler)

let utility axis value = Utility_factory.v { axis; value }

let v d n =
  let s = Handler.Spacing (Spacing.int n :> Style.spacing) in
  utility d (if n < 0 then Handler.Negative s else Handler.Positive s)

let v' d n =
  let s = Handler.Spacing (Spacing.float n :> Style.spacing) in
  utility d (if n < 0.0 then Handler.Negative s else Handler.Positive s)

let m n = v `All n
let mx n = v `X n
let my n = v `Y n
let mt n = v `T n
let mr n = v `R n
let mb n = v `B n
let ml n = v `L n
let m' n = v' `All n
let mx' n = v' `X n
let my' n = v' `Y n
let mt' n = v' `T n
let mr' n = v' `R n
let mb' n = v' `B n
let ml' n = v' `L n
let m_auto = utility `All Handler.Auto
let mx_auto = utility `X Handler.Auto
let my_auto = utility `Y Handler.Auto
let mt_auto = utility `T Handler.Auto
let mr_auto = utility `R Handler.Auto
let mb_auto = utility `B Handler.Auto
let ml_auto = utility `L Handler.Auto
