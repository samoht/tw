(** Font variant numeric utilities. *)

module Css = Cascade.Css

let ordinal_var =
  Var.channel ~needs_property:true ~property_order:102
    Css.Font_variant_numeric_token "tw-ordinal"

let slashed_var =
  Var.channel ~needs_property:true ~property_order:103
    Css.Font_variant_numeric_token "tw-slashed-zero"

let figure_var =
  Var.channel ~needs_property:true ~property_order:104
    Css.Font_variant_numeric_token "tw-numeric-figure"

let spacing_var =
  Var.channel ~needs_property:true ~property_order:105
    Css.Font_variant_numeric_token "tw-numeric-spacing"

let fraction_var =
  Var.channel ~needs_property:true ~property_order:106
    Css.Font_variant_numeric_token "tw-numeric-fraction"

(* Default theme for font variant numeric variables *)
type font_variant_theme = {
  ordinal : Css.declaration * Css.font_variant_numeric_token Css.var;
  slashed : Css.declaration * Css.font_variant_numeric_token Css.var;
  figure : Css.declaration * Css.font_variant_numeric_token Css.var;
  spacing : Css.declaration * Css.font_variant_numeric_token Css.var;
  fraction : Css.declaration * Css.font_variant_numeric_token Css.var;
}

let default_font_variant_theme : font_variant_theme =
  let ordinal_decl, ordinal_ref =
    Var.binding ordinal_var ~fallback:Css.Empty Normal
  in
  let slashed_decl, slashed_ref =
    Var.binding slashed_var ~fallback:Css.Empty Normal
  in
  let figure_decl, figure_ref =
    Var.binding figure_var ~fallback:Css.Empty Normal
  in
  let spacing_decl, spacing_ref =
    Var.binding spacing_var ~fallback:Css.Empty Normal
  in
  let fraction_decl, fraction_ref =
    Var.binding fraction_var ~fallback:Css.Empty Normal
  in
  {
    ordinal = (ordinal_decl, ordinal_ref);
    slashed = (slashed_decl, slashed_ref);
    figure = (figure_decl, figure_ref);
    spacing = (spacing_decl, spacing_ref);
    fraction = (fraction_decl, fraction_ref);
  }

module Handler = struct
  open Style
  open Css

  type t =
    | Normal_nums
    | Ordinal
    | Slashed_zero
    | Lining_nums
    | Oldstyle_nums
    | Proportional_nums
    | Tabular_nums
    | Diagonal_fractions
    | Stacked_fractions

  let name = "font_variant_numeric"

  (* The numeric variants sort in the priority-26 band the late text families
     share, after font stretch. *)
  let priority _ = 26
  let err_not_utility = Error (`Msg "Not a font-variant-numeric utility")

  let of_class _theme class_name =
    let parts = Parse.split_class class_name in
    match parts with
    | [ "normal"; "nums" ] -> Ok Normal_nums
    | [ "ordinal" ] -> Ok Ordinal
    | [ "slashed"; "zero" ] -> Ok Slashed_zero
    | [ "lining"; "nums" ] -> Ok Lining_nums
    | [ "oldstyle"; "nums" ] -> Ok Oldstyle_nums
    | [ "proportional"; "nums" ] -> Ok Proportional_nums
    | [ "tabular"; "nums" ] -> Ok Tabular_nums
    | [ "diagonal"; "fractions" ] -> Ok Diagonal_fractions
    | [ "stacked"; "fractions" ] -> Ok Stacked_fractions
    | _ -> err_not_utility

  let to_class = function
    | Normal_nums -> "normal-nums"
    | Ordinal -> "ordinal"
    | Slashed_zero -> "slashed-zero"
    | Lining_nums -> "lining-nums"
    | Oldstyle_nums -> "oldstyle-nums"
    | Proportional_nums -> "proportional-nums"
    | Tabular_nums -> "tabular-nums"
    | Diagonal_fractions -> "diagonal-fractions"
    | Stacked_fractions -> "stacked-fractions"

  let suborder = function
    (* Numeric variants - alphabetical order with normal-nums last *)
    | Diagonal_fractions -> 8390
    | Lining_nums -> 8391
    | Oldstyle_nums -> 8392
    | Ordinal -> 8393
    | Proportional_nums -> 8394
    | Slashed_zero -> 8395
    | Stacked_fractions -> 8396
    | Tabular_nums -> 8397
    | Normal_nums -> 8398

  let normal_nums = style [ font_variant_numeric (Tokens [ Normal ]) ]

  let font_variant_numeric_utility var_to_set value =
    (* Use the shared default theme *)
    let theme = default_font_variant_theme in

    (* Override the specific variable being set - with EMPTY fallback to match
       Tailwind *)
    let updated_theme =
      match var_to_set with
      | `Ordinal ->
          let new_decl, new_ref =
            Var.binding ordinal_var ~fallback:Css.Empty value
          in
          { theme with ordinal = (new_decl, new_ref) }
      | `Slashed ->
          let new_decl, new_ref =
            Var.binding slashed_var ~fallback:Css.Empty value
          in
          { theme with slashed = (new_decl, new_ref) }
      | `Figure ->
          let new_decl, new_ref =
            Var.binding figure_var ~fallback:Css.Empty value
          in
          { theme with figure = (new_decl, new_ref) }
      | `Spacing ->
          let new_decl, new_ref =
            Var.binding spacing_var ~fallback:Css.Empty value
          in
          { theme with spacing = (new_decl, new_ref) }
      | `Fraction ->
          let new_decl, new_ref =
            Var.binding fraction_var ~fallback:Css.Empty value
          in
          { theme with fraction = (new_decl, new_ref) }
    in

    (* Extract only the active declaration (the one being set) *)
    let active_decl =
      match var_to_set with
      | `Ordinal -> fst updated_theme.ordinal
      | `Slashed -> fst updated_theme.slashed
      | `Figure -> fst updated_theme.figure
      | `Spacing -> fst updated_theme.spacing
      | `Fraction -> fst updated_theme.fraction
    in

    (* Compose the font-variant-numeric value using all var references *)
    let composed_value =
      Composed
        {
          ordinal = Some (Css.Var (snd updated_theme.ordinal));
          slashed_zero = Some (Css.Var (snd updated_theme.slashed));
          numeric_figure = Some (Css.Var (snd updated_theme.figure));
          numeric_spacing = Some (Css.Var (snd updated_theme.spacing));
          numeric_fraction = Some (Css.Var (snd updated_theme.fraction));
        }
    in

    (* Get property rules for animation support *)
    let property_rules =
      List.filter_map
        (fun x -> x)
        [
          Var.property_rule ordinal_var;
          Var.property_rule slashed_var;
          Var.property_rule figure_var;
          Var.property_rule spacing_var;
          Var.property_rule fraction_var;
        ]
    in

    style
      ~property_rules:(Css.concat property_rules)
      [ active_decl; font_variant_numeric composed_value ]

  let ordinal = font_variant_numeric_utility `Ordinal Ordinal
  let slashed_zero = font_variant_numeric_utility `Slashed Slashed_zero
  let lining_nums = font_variant_numeric_utility `Figure Lining_nums
  let oldstyle_nums = font_variant_numeric_utility `Figure Oldstyle_nums

  let proportional_nums =
    font_variant_numeric_utility `Spacing Proportional_nums

  let tabular_nums = font_variant_numeric_utility `Spacing Tabular_nums

  let diagonal_fractions =
    font_variant_numeric_utility `Fraction Diagonal_fractions

  let stacked_fractions =
    font_variant_numeric_utility `Fraction Stacked_fractions

  let to_style _theme = function
    | Normal_nums -> normal_nums
    | Ordinal -> ordinal
    | Slashed_zero -> slashed_zero
    | Lining_nums -> lining_nums
    | Oldstyle_nums -> oldstyle_nums
    | Proportional_nums -> proportional_nums
    | Tabular_nums -> tabular_nums
    | Diagonal_fractions -> diagonal_fractions
    | Stacked_fractions -> stacked_fractions

  let examples = [ Normal_nums ]
end

open Handler
module Utility_factory = Utility.Make (Handler)

let utility = Utility_factory.v
let normal_nums = utility Normal_nums
let ordinal = utility Ordinal
let slashed_zero = utility Slashed_zero
let lining_nums = utility Lining_nums
let oldstyle_nums = utility Oldstyle_nums
let proportional_nums = utility Proportional_nums
let tabular_nums = utility Tabular_nums
let diagonal_fractions = utility Diagonal_fractions
let stacked_fractions = utility Stacked_fractions
