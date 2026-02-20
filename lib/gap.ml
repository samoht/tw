(** Gap and space-between utilities. *)

module Handler = struct
  open Style
  open Css

  type t =
    | Gap of { axis : [ `All | `X | `Y ]; value : spacing }
    | Space of { negative : bool; axis : [ `X | `Y ]; value : spacing }
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

  let gap (s : spacing) =
    let decl, len = spacing_to_decl_len s in
    let gap_value = { row_gap = Some len; column_gap = Some len } in
    style (Option.to_list decl @ [ gap gap_value ])

  let gap_x (s : spacing) =
    let decl, len = spacing_to_decl_len s in
    style (Option.to_list decl @ [ column_gap len ])

  let gap_y (s : spacing) =
    let decl, len = spacing_to_decl_len s in
    style (Option.to_list decl @ [ row_gap len ])

  (** {2 Space Between Utilities} *)

  let space_x n =
    let class_name = "space-x-" ^ string_of_int (abs n) in
    (* Tailwind v4 uses flat selector: :where(.space-x-N > :not(:last-child)) *)
    let selector =
      Css.Selector.(where [ class_ class_name >> not [ Last_child ] ])
    in
    (* Use Theme.spacing_calc to get scheme-aware spacing value *)
    let spacing_decl, spacing_len = Theme.spacing_calc n in
    (* Create reverse_decl and reverse_ref for --tw-space-x-reverse var *)
    let reverse_decl, reverse_ref =
      Var.binding space_x_reverse_var (Css.Num 0.0)
    in
    let reverse_var_name = Css.var_name reverse_ref in
    (* margin-inline-start = calc(var(--spacing-N) *
       var(--tw-space-x-reverse)) *)
    let margin_start : Css.length =
      Calc Calc.(mul (length spacing_len) (var reverse_var_name))
    in
    (* margin-inline-end = calc(var(--spacing-N) * (1 -
       var(--tw-space-x-reverse))) *)
    let margin_end : Css.length =
      Calc
        Calc.(
          mul (length spacing_len)
            (nested (sub (float 1.0) (var reverse_var_name))))
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
    let class_name = "space-y-" ^ string_of_int (abs n) in
    (* Tailwind v4 uses flat selector: :where(.space-y-N > :not(:last-child)) *)
    let selector =
      Css.Selector.(where [ class_ class_name >> not [ Last_child ] ])
    in
    (* Use Theme.spacing_calc to get scheme-aware spacing value *)
    let spacing_decl, spacing_len = Theme.spacing_calc n in
    (* Create reverse_decl and reverse_ref for --tw-space-y-reverse var *)
    let reverse_decl, reverse_ref =
      Var.binding space_y_reverse_var (Css.Num 0.0)
    in
    let reverse_var_name = Css.var_name reverse_ref in
    (* margin-block-start = calc(var(--spacing-N) *
       var(--tw-space-y-reverse)) *)
    let margin_start : Css.length =
      Calc Calc.(mul (length spacing_len) (var reverse_var_name))
    in
    (* margin-block-end = calc(var(--spacing-N) * (1 -
       var(--tw-space-y-reverse))) *)
    let margin_end : Css.length =
      Calc
        Calc.(
          mul (length spacing_len)
            (nested (sub (float 1.0) (var reverse_var_name))))
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

  let to_style t =
    match t with
    | Gap { axis; value } -> (
        match axis with
        | `All -> gap value
        | `X -> gap_x value
        | `Y -> gap_y value)
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
    | Space_x_reverse -> space_x_reverse_style ()
    | Space_y_reverse -> space_y_reverse_style ()

  let suborder = function
    | Gap { axis; value } ->
        let axis_offset =
          match axis with `All -> 0 | `X -> 20000 | `Y -> 40000
        in
        25000 + axis_offset + spacing_value_order value
    | Space { negative; axis; value } ->
        let neg_offset = if negative then -100000 else 0 in
        let axis_offset = match axis with `X -> 0 | `Y -> 10000 in
        20000 + neg_offset + axis_offset + spacing_value_order value
    (* Reverse utilities come after their regular counterparts *)
    | Space_x_reverse -> 130000
    | Space_y_reverse -> 140000

  let to_class = function
    | Gap { axis; value } -> (
        let suffix = Spacing.pp_spacing_suffix value in
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
    | Space_x_reverse -> "space-x-reverse"
    | Space_y_reverse -> "space-y-reverse"

  let of_class class_name =
    let parts = Parse.split_class class_name in
    let err_not_utility = Error (`Msg "Not a gap utility") in
    let parse_class = function
      | [ "gap"; value ] -> (
          match Parse.spacing_value ~name:"gap" value with
          | Ok f -> Ok (Gap { axis = `All; value = `Rem (f *. 0.25) })
          | Error _ ->
              if value = "px" then Ok (Gap { axis = `All; value = `Px })
              else if value = "full" then
                Ok (Gap { axis = `All; value = `Full })
              else err_not_utility)
      | [ "gap"; "x"; value ] -> (
          match Parse.spacing_value ~name:"gap-x" value with
          | Ok f -> Ok (Gap { axis = `X; value = `Rem (f *. 0.25) })
          | Error _ ->
              if value = "px" then Ok (Gap { axis = `X; value = `Px })
              else if value = "full" then Ok (Gap { axis = `X; value = `Full })
              else err_not_utility)
      | [ "gap"; "y"; value ] -> (
          match Parse.spacing_value ~name:"gap-y" value with
          | Ok f -> Ok (Gap { axis = `Y; value = `Rem (f *. 0.25) })
          | Error _ ->
              if value = "px" then Ok (Gap { axis = `Y; value = `Px })
              else if value = "full" then Ok (Gap { axis = `Y; value = `Full })
              else err_not_utility)
      | [ "space"; "x"; value ] -> (
          if value = "reverse" then Ok Space_x_reverse
          else
            match Parse.int_pos ~name:"space-x" value with
            | Ok n ->
                Ok
                  (Space
                     {
                       negative = false;
                       axis = `X;
                       value = `Rem (float_of_int n *. 0.25);
                     })
            | Error _ -> err_not_utility)
      | [ "space"; "y"; value ] -> (
          if value = "reverse" then Ok Space_y_reverse
          else
            match Parse.int_pos ~name:"space-y" value with
            | Ok n ->
                Ok
                  (Space
                     {
                       negative = false;
                       axis = `Y;
                       value = `Rem (float_of_int n *. 0.25);
                     })
            | Error _ -> err_not_utility)
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

let gap n = gap_util `All (Spacing.int n)
let gap_x n = gap_util `X (Spacing.int n)
let gap_y n = gap_util `Y (Spacing.int n)

(** {2 Special Gap Values} *)

let gap_px = gap_util `All `Px
let gap_full = gap_util `All `Full
let gap_x_px = gap_util `X `Px
let gap_x_full = gap_util `X `Full
let gap_y_px = gap_util `Y `Px
let gap_y_full = gap_util `Y `Full

(** {2 Space Between Utilities} *)

let space_x n =
  let s = Spacing.int n in
  let neg = n < 0 in
  space_util neg `X s

let space_y n =
  let s = Spacing.int n in
  let neg = n < 0 in
  space_util neg `Y s
