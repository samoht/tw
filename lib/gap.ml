(** Gap and space-between utilities. *)

module Handler = struct
  open Style
  open Css

  type gap_value =
    | Standard of spacing
    | Arbitrary of Css.length (* gap-[4px] *)
    | ArbitraryVar of string (* gap-[var(--value)] *)

  type t =
    | Gap of { axis : [ `All | `X | `Y ]; value : gap_value }
    | Space of { negative : bool; axis : [ `X | `Y ]; value : spacing }
    | Space_arb of { axis : [ `X | `Y ]; value : Css.length }
    | Space_x_reverse
    | Space_y_reverse

  type Utility.base += Self of t

  let name = "gap"
  let priority = 17

  (** Space reverse variables for flex-reverse support. Property order -1
      ensures these come FIRST in {i \@layer properties} (Tailwind places them
      before --tw-border-style). *)
  let space_x_reverse_var =
    Var.property_default Css.Number_percentage ~initial:(Css.Num 0.0)
      ~property_order:(-2) "tw-space-x-reverse"

  let space_y_reverse_var =
    Var.property_default Css.Number_percentage ~initial:(Css.Num 0.0)
      ~property_order:(-1) "tw-space-y-reverse"

  (** {2 Typed Gap Utilities} *)

  (** Convert spacing to (declaration, length) using Theme.spacing_calc_float.
  *)
  let spacing_to_decl_len (s : spacing) : Css.declaration option * length =
    match s with
    | `Px ->
        let len : length = Px 1. in
        let decl, _ = Var.binding Spacing.spacing_var (Rem 0.25) in
        (Some decl, len)
    | `Full ->
        let len : length = Pct 100. in
        let decl, _ = Var.binding Spacing.spacing_var (Rem 0.25) in
        (Some decl, len)
    | `Named name ->
        let len = Spacing.named_spacing_ref name in
        (None, len)
    | `Rem f ->
        let n = f /. 0.25 in
        let decl, len = Theme.spacing_calc_float n in
        (Some decl, len)

  let gap_standard (s : spacing) =
    let decl, len = spacing_to_decl_len s in
    let gap_value = { row_gap = Some len; column_gap = Some len } in
    style (Option.to_list decl @ [ gap gap_value ])

  let gap_x_standard (s : spacing) =
    let decl, len = spacing_to_decl_len s in
    style (Option.to_list decl @ [ column_gap len ])

  let gap_y_standard (s : spacing) =
    let decl, len = spacing_to_decl_len s in
    style (Option.to_list decl @ [ row_gap len ])

  let gap_arb len =
    let gap_value = { row_gap = Some len; column_gap = Some len } in
    style [ gap gap_value ]

  let gap_x_arb len = style [ column_gap len ]
  let gap_y_arb len = style [ row_gap len ]

  let gap_var_ref var_str : Css.length =
    let bare_name = Parse.extract_var_name var_str in
    Css.Var (Css.var_ref bare_name)

  let gap_value axis (v : gap_value) =
    match v with
    | Standard s -> (
        match axis with
        | `All -> gap_standard s
        | `X -> gap_x_standard s
        | `Y -> gap_y_standard s)
    | Arbitrary len -> (
        match axis with
        | `All -> gap_arb len
        | `X -> gap_x_arb len
        | `Y -> gap_y_arb len)
    | ArbitraryVar var_str -> (
        let len = gap_var_ref var_str in
        match axis with
        | `All -> gap_arb len
        | `X -> gap_x_arb len
        | `Y -> gap_y_arb len)

  (** {2 Space Between Utilities} *)

  (* Build margin calc expressions for space utilities. For negative values,
     folds * -1 into the expression to avoid nested calc(). *)
  let space_margin_calcs ~negative ~spacing_len ~reverse_var_name =
    let base = Css.Calc.length spacing_len in
    let neg_factor = Css.Calc.float (-1.0) in
    let start_expr =
      if negative then
        Css.Calc.(mul (mul base neg_factor) (var reverse_var_name))
      else Css.Calc.(mul base (var reverse_var_name))
    in
    let end_expr =
      if negative then
        Css.Calc.(
          mul (mul base neg_factor)
            (parens (sub (float 1.0) (var reverse_var_name))))
      else Css.Calc.(mul base (parens (sub (float 1.0) (var reverse_var_name))))
    in
    ((Css.Calc start_expr : Css.length), (Css.Calc end_expr : Css.length))

  let space_x n =
    let negative = n < 0 in
    let abs_n = abs n in
    let class_name =
      (if negative then "-space-x-" else "space-x-") ^ string_of_int abs_n
    in
    let selector =
      Css.Selector.(where [ class_ class_name >> not [ Last_child ] ])
    in
    let spacing_decl, spacing_len = Theme.spacing_calc abs_n in
    let reverse_decl, reverse_ref =
      Var.binding space_x_reverse_var (Css.Num 0.0)
    in
    let reverse_var_name = Css.var_name reverse_ref in
    let margin_start, margin_end =
      space_margin_calcs ~negative ~spacing_len ~reverse_var_name
    in
    let property_rules =
      [ Var.property_rule space_x_reverse_var ] |> List.filter_map Fun.id
    in
    let rule =
      Css.rule ~selector
        [
          spacing_decl;
          reverse_decl;
          margin_inline_start margin_start;
          margin_inline_end margin_end;
        ]
    in
    style ~rules:(Some [ rule ]) ~property_rules:(Css.concat property_rules) []

  let space_y n =
    let negative = n < 0 in
    let abs_n = abs n in
    let class_name =
      (if negative then "-space-y-" else "space-y-") ^ string_of_int abs_n
    in
    let selector =
      Css.Selector.(where [ class_ class_name >> not [ Last_child ] ])
    in
    let spacing_decl, spacing_len = Theme.spacing_calc abs_n in
    let reverse_decl, reverse_ref =
      Var.binding space_y_reverse_var (Css.Num 0.0)
    in
    let reverse_var_name = Css.var_name reverse_ref in
    let margin_start, margin_end =
      space_margin_calcs ~negative ~spacing_len ~reverse_var_name
    in
    let property_rules =
      [ Var.property_rule space_y_reverse_var ] |> List.filter_map Fun.id
    in
    let rule =
      Css.rule ~selector
        [
          spacing_decl;
          reverse_decl;
          margin_block_start margin_start;
          margin_block_end margin_end;
        ]
    in
    style ~rules:(Some [ rule ]) ~property_rules:(Css.concat property_rules) []

  (* Bracket arbitrary space: space-x-[4px], space-y-[10rem] *)
  let space_arb_x (len : Css.length) class_name =
    let selector =
      Css.Selector.(where [ class_ class_name >> not [ Last_child ] ])
    in
    let reverse_decl, reverse_ref =
      Var.binding space_x_reverse_var (Css.Num 0.0)
    in
    let reverse_var_name = Css.var_name reverse_ref in
    let margin_start : Css.length =
      Calc Calc.(mul (length len) (var reverse_var_name))
    in
    let margin_end : Css.length =
      Calc
        Calc.(
          mul (length len) (parens (sub (float 1.0) (var reverse_var_name))))
    in
    let property_rules =
      [ Var.property_rule space_x_reverse_var ] |> List.filter_map Fun.id
    in
    let rule =
      Css.rule ~selector
        [
          reverse_decl;
          margin_inline_start margin_start;
          margin_inline_end margin_end;
        ]
    in
    style ~rules:(Some [ rule ]) ~property_rules:(Css.concat property_rules) []

  let space_arb_y (len : Css.length) class_name =
    let selector =
      Css.Selector.(where [ class_ class_name >> not [ Last_child ] ])
    in
    let reverse_decl, reverse_ref =
      Var.binding space_y_reverse_var (Css.Num 0.0)
    in
    let reverse_var_name = Css.var_name reverse_ref in
    let margin_start : Css.length =
      Calc Calc.(mul (length len) (var reverse_var_name))
    in
    let margin_end : Css.length =
      Calc
        Calc.(
          mul (length len) (parens (sub (float 1.0) (var reverse_var_name))))
    in
    let property_rules =
      [ Var.property_rule space_y_reverse_var ] |> List.filter_map Fun.id
    in
    let rule =
      Css.rule ~selector
        [
          reverse_decl;
          margin_block_start margin_start;
          margin_block_end margin_end;
        ]
    in
    style ~rules:(Some [ rule ]) ~property_rules:(Css.concat property_rules) []

  let spacing_value_order = function
    | `Px -> 1
    | `Full -> 10000
    | `Named _ -> 20000
    | `Rem f ->
        let units = f /. 0.25 in
        int_of_float (units *. 10.)

  (* space-x-reverse utility sets --tw-space-x-reverse: 1 on children *)
  let space_x_reverse_style () =
    let selector =
      Css.Selector.(where [ class_ "space-x-reverse" >> not [ Last_child ] ])
    in
    let decl, _ = Var.binding space_x_reverse_var (Css.Num 1.0) in
    let property_rules =
      [ Var.property_rule space_x_reverse_var ] |> List.filter_map Fun.id
    in
    let rule = Css.rule ~selector [ decl ] in
    style ~rules:(Some [ rule ]) ~property_rules:(Css.concat property_rules) []

  (* space-y-reverse utility sets --tw-space-y-reverse: 1 on children *)
  let space_y_reverse_style () =
    let selector =
      Css.Selector.(where [ class_ "space-y-reverse" >> not [ Last_child ] ])
    in
    let decl, _ = Var.binding space_y_reverse_var (Css.Num 1.0) in
    let property_rules =
      [ Var.property_rule space_y_reverse_var ] |> List.filter_map Fun.id
    in
    let rule = Css.rule ~selector [ decl ] in
    style ~rules:(Some [ rule ]) ~property_rules:(Css.concat property_rules) []

  let pp_float n =
    let s = string_of_float n in
    if String.ends_with ~suffix:"." s then String.sub s 0 (String.length s - 1)
    else s

  let to_style t =
    match t with
    | Gap { axis; value } -> gap_value axis value
    | Space { negative; axis; value } -> (
        let n =
          match value with
          | `Rem f -> int_of_float (f /. 0.25)
          | `Px -> 0
          | `Full -> 0
          | `Named _ -> 0
        in
        let n = if negative then -n else n in
        match axis with `X -> space_x n | `Y -> space_y n)
    | Space_arb { axis; value } -> (
        let len_suffix =
          match value with
          | Px n -> "[" ^ pp_float n ^ "px]"
          | Rem n -> "[" ^ pp_float n ^ "rem]"
          | Pct n -> "[" ^ pp_float n ^ "%]"
          | _ -> "[<length>]"
        in
        let class_name =
          match axis with
          | `X -> "space-x-" ^ len_suffix
          | `Y -> "space-y-" ^ len_suffix
        in
        match axis with
        | `X -> space_arb_x value class_name
        | `Y -> space_arb_y value class_name)
    | Space_x_reverse -> space_x_reverse_style ()
    | Space_y_reverse -> space_y_reverse_style ()

  let gap_value_order = function
    | Standard s -> spacing_value_order s
    | Arbitrary _ -> 50000
    | ArbitraryVar _ -> 55000

  let suborder = function
    | Gap { axis; value } ->
        let axis_offset =
          match axis with `All -> 0 | `X -> 20000 | `Y -> 40000
        in
        25000 + axis_offset + gap_value_order value
    | Space { negative; axis; value } ->
        let neg_offset = if negative then -100000 else 0 in
        let axis_offset = match axis with `X -> 0 | `Y -> 10000 in
        20000 + neg_offset + axis_offset + spacing_value_order value
    | Space_arb { axis; _ } ->
        let axis_offset = match axis with `X -> 0 | `Y -> 10000 in
        70000 + axis_offset
    (* Reverse utilities come after their regular counterparts *)
    | Space_x_reverse -> 130000
    | Space_y_reverse -> 140000

  let pp_gap_value_suffix = function
    | Standard s -> Spacing.pp_spacing_suffix s
    | Arbitrary len -> (
        match len with
        | Px n -> "[" ^ pp_float n ^ "px]"
        | Rem n -> "[" ^ pp_float n ^ "rem]"
        | Pct n -> "[" ^ pp_float n ^ "%]"
        | _ -> "[<length>]")
    | ArbitraryVar s -> "[" ^ s ^ "]"

  let to_class = function
    | Gap { axis; value } -> (
        let suffix = pp_gap_value_suffix value in
        match axis with
        | `All -> "gap-" ^ suffix
        | `X -> "gap-x-" ^ suffix
        | `Y -> "gap-y-" ^ suffix)
    | Space { negative; axis; value } -> (
        let suffix = Spacing.pp_spacing_suffix value in
        let prefix = if negative then "-" else "" in
        match axis with
        | `X -> prefix ^ "space-x-" ^ suffix
        | `Y -> prefix ^ "space-y-" ^ suffix)
    | Space_arb { axis; value } -> (
        let suffix = pp_gap_value_suffix (Arbitrary value) in
        match axis with `X -> "space-x-" ^ suffix | `Y -> "space-y-" ^ suffix)
    | Space_x_reverse -> "space-x-reverse"
    | Space_y_reverse -> "space-y-reverse"

  let parse_gap_arbitrary s : gap_value option =
    let len = String.length s in
    if len > 2 && s.[0] = '[' && s.[len - 1] = ']' then
      let inner = String.sub s 1 (len - 2) in
      if String.length inner > 4 && String.sub inner 0 4 = "var(" then
        Some (ArbitraryVar inner)
      else if String.ends_with ~suffix:"px" inner then
        let n = String.sub inner 0 (String.length inner - 2) in
        match float_of_string_opt n with
        | Some f -> Some (Arbitrary (Css.Px f))
        | None -> None
      else if String.ends_with ~suffix:"rem" inner then
        let n = String.sub inner 0 (String.length inner - 3) in
        match float_of_string_opt n with
        | Some f -> Some (Arbitrary (Css.Rem f))
        | None -> None
      else None
    else None

  let parse_gap_value value =
    if String.length value > 0 && value.[0] = '[' then parse_gap_arbitrary value
    else
      match Parse.spacing_value ~name:"gap" value with
      | Ok f -> Some (Standard (`Rem (f *. 0.25)))
      | Error _ ->
          if value = "px" then Some (Standard `Px)
          else if value = "full" then Some (Standard `Full)
          else None

  let of_class class_name =
    let parts = Parse.split_class class_name in
    let err_not_utility = Error (`Msg "Not a gap utility") in
    let parse_class = function
      | [ "gap"; value ] -> (
          match parse_gap_value value with
          | Some v -> Ok (Gap { axis = `All; value = v })
          | None -> err_not_utility)
      | [ "gap"; "x"; value ] -> (
          match parse_gap_value value with
          | Some v -> Ok (Gap { axis = `X; value = v })
          | None -> err_not_utility)
      | [ "gap"; "y"; value ] -> (
          match parse_gap_value value with
          | Some v -> Ok (Gap { axis = `Y; value = v })
          | None -> err_not_utility)
      | [ "space"; "x"; value ] -> (
          if value = "reverse" then Ok Space_x_reverse
          else
            match parse_gap_arbitrary value with
            | Some (Arbitrary len) -> Ok (Space_arb { axis = `X; value = len })
            | _ -> (
                match Parse.int_pos ~name:"space-x" value with
                | Ok n ->
                    Ok
                      (Space
                         {
                           negative = false;
                           axis = `X;
                           value = `Rem (float_of_int n *. 0.25);
                         })
                | Error _ -> err_not_utility))
      | [ "space"; "y"; value ] -> (
          if value = "reverse" then Ok Space_y_reverse
          else
            match parse_gap_arbitrary value with
            | Some (Arbitrary len) -> Ok (Space_arb { axis = `Y; value = len })
            | _ -> (
                match Parse.int_pos ~name:"space-y" value with
                | Ok n ->
                    Ok
                      (Space
                         {
                           negative = false;
                           axis = `Y;
                           value = `Rem (float_of_int n *. 0.25);
                         })
                | Error _ -> err_not_utility))
      | [ ""; "space"; "x"; value ] -> (
          match Parse.int_pos ~name:"space-x" value with
          | Ok n ->
              Ok
                (Space
                   {
                     negative = true;
                     axis = `X;
                     value = `Rem (float_of_int n *. 0.25);
                   })
          | Error _ -> err_not_utility)
      | [ ""; "space"; "y"; value ] -> (
          match Parse.int_pos ~name:"space-y" value with
          | Ok n ->
              Ok
                (Space
                   {
                     negative = true;
                     axis = `Y;
                     value = `Rem (float_of_int n *. 0.25);
                   })
          | Error _ -> err_not_utility)
      | _ -> err_not_utility
    in
    parse_class parts
end

open Handler

(** Register handler with Utility system *)
let () = Utility.register (module Handler)

(** Public API *)
let utility x = Utility.base (Self x)

(** Helpers to create Utility.t from Gap/Space *)
let gap_util axis value = utility (Gap { axis; value })

let space_util negative axis value = utility (Space { negative; axis; value })

(** {2 Int-based Gap Utilities} *)

let gap n = gap_util `All (Handler.Standard (Spacing.int n))
let gap_x n = gap_util `X (Handler.Standard (Spacing.int n))
let gap_y n = gap_util `Y (Handler.Standard (Spacing.int n))

(** {2 Special Gap Values} *)

let gap_px = gap_util `All (Handler.Standard `Px)
let gap_full = gap_util `All (Handler.Standard `Full)
let gap_x_px = gap_util `X (Handler.Standard `Px)
let gap_x_full = gap_util `X (Handler.Standard `Full)
let gap_y_px = gap_util `Y (Handler.Standard `Px)
let gap_y_full = gap_util `Y (Handler.Standard `Full)

(** {2 Space Between Utilities} *)

let space_x n =
  let s = Spacing.int n in
  let neg = n < 0 in
  space_util neg `X s

let space_y n =
  let s = Spacing.int n in
  let neg = n < 0 in
  space_util neg `Y s
