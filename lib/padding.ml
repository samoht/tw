(** Padding utilities. *)

module Handler = struct
  open Style
  open Css

  type padding_value =
    | Standard of spacing
    | Arbitrary of Css.length (* p-[4px] *)
    | ArbitraryVar of string (* p-[var(--value)] *)

  type t = {
    axis : [ `All | `X | `Y | `T | `R | `B | `L | `S | `E | `Bs | `Be ];
    value : padding_value;
  }

  type Utility.base += Self of t

  let name = "padding"
  let priority = 21

  let pp_float n =
    let s = string_of_float n in
    if String.ends_with ~suffix:"." s then String.sub s 0 (String.length s - 1)
    else s

  let pp_length_suffix (len : Css.length) =
    match len with
    | Px n -> "[" ^ pp_float n ^ "px]"
    | Rem n -> "[" ^ pp_float n ^ "rem]"
    | Pct n -> "[" ^ pp_float n ^ "%]"
    | _ -> "[<length>]"

  let to_class { axis; value } =
    let prefix =
      match axis with
      | `All -> "p-"
      | `X -> "px-"
      | `Y -> "py-"
      | `T -> "pt-"
      | `R -> "pr-"
      | `B -> "pb-"
      | `L -> "pl-"
      | `S -> "ps-"
      | `E -> "pe-"
      | `Bs -> "pbs-"
      | `Be -> "pbe-"
    in
    let value_suffix =
      match value with
      | Standard s -> Spacing.pp_spacing_suffix s
      | Arbitrary len -> pp_length_suffix len
      | ArbitraryVar s -> "[" ^ s ^ "]"
    in
    prefix ^ value_suffix

  (** Convert spacing to (declaration option, length) using
      Theme.spacing_calc_float. *)
  let spacing_to_decl_len (s : Style.spacing) : Css.declaration option * length
      =
    match s with
    | `Px ->
        let len : length = Px 1. in
        let decl, _ = Var.binding Spacing.spacing_var (Rem 0.25) in
        (Some decl, len)
    | `Full ->
        let len : length = Pct 100. in
        let decl, _ = Var.binding Spacing.spacing_var (Rem 0.25) in
        (Some decl, len)
    | `Named name -> Spacing.named_spacing_binding name
    | `Rem f ->
        let n = f /. 0.25 in
        let decl, len = Theme.spacing_calc_float n in
        (Some decl, len)

  let v_spacing (prop : length -> declaration) (s : Style.spacing) =
    let decl, len = spacing_to_decl_len s in
    style (Option.to_list decl @ [ prop len ])

  let vs_spacing (prop : length list -> declaration) (s : Style.spacing) =
    let decl, len = spacing_to_decl_len s in
    style (Option.to_list decl @ [ prop [ len ] ])

  let spacing_value_order = function
    | `Px -> 1
    | `Full -> 10000
    | `Named _ -> 20000
    | `Rem f ->
        let units = f /. 0.25 in
        int_of_float (units *. 10.)

  let value_order = function
    | Standard s -> spacing_value_order s
    | Arbitrary _ -> 20000
    | ArbitraryVar _ -> 20000

  let apply_prop axis (prop_v : length -> declaration)
      (prop_vs : length list -> declaration) value =
    match value with
    | Standard s ->
        if axis = `All then vs_spacing prop_vs s else v_spacing prop_v s
    | Arbitrary len ->
        if axis = `All then style [ prop_vs [ len ] ] else style [ prop_v len ]
    | ArbitraryVar var_str ->
        let bare_name = Parse.extract_var_name var_str in
        let var_len : Css.length = Var (Css.var_ref bare_name) in
        if axis = `All then style [ prop_vs [ var_len ] ]
        else style [ prop_v var_len ]

  let to_style t =
    match t.axis with
    | `All -> apply_prop `All padding_top padding t.value
    | `X -> apply_prop `X padding_inline padding t.value
    | `Y -> apply_prop `Y padding_block padding t.value
    | `T -> apply_prop `T padding_top padding t.value
    | `R -> apply_prop `R padding_right padding t.value
    | `B -> apply_prop `B padding_bottom padding t.value
    | `L -> apply_prop `L padding_left padding t.value
    | `S -> apply_prop `S padding_inline_start padding t.value
    | `E -> apply_prop `E padding_inline_end padding t.value
    | `Bs -> apply_prop `Bs padding_block_start padding t.value
    | `Be -> apply_prop `Be padding_block_end padding t.value

  let suborder { axis; value } =
    let side_offset =
      match axis with
      | `All -> 0
      | `X -> 10000
      | `Y -> 20000
      | `T -> 30000
      | `R -> 40000
      | `B -> 50000
      | `L -> 60000
      | `S -> 70000
      | `E -> 80000
      | `Bs -> 90000
      | `Be -> 100000
    in
    side_offset + value_order value

  let parse_arbitrary s : padding_value option =
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

  let padding_axis_of_prefix = function
    | "p" -> Some `All
    | "px" -> Some `X
    | "py" -> Some `Y
    | "pt" -> Some `T
    | "pr" -> Some `R
    | "pb" -> Some `B
    | "pl" -> Some `L
    | "ps" -> Some `S
    | "pe" -> Some `E
    | "pbs" -> Some `Bs
    | "pbe" -> Some `Be
    | _ -> None

  let of_class class_name =
    let parts = Parse.split_class class_name in
    match parts with
    (* Handle arbitrary values: p-[4px], px-[var(--value)] *)
    | [ prefix; arb ] when String.length arb > 0 && arb.[0] = '[' -> (
        match (padding_axis_of_prefix prefix, parse_arbitrary arb) with
        | Some axis, Some value -> Ok { axis; value }
        | _ -> Error (`Msg "Not a padding utility"))
    (* Standard parsing via Spacing *)
    | _ -> (
        match Spacing.parse_class_parts parts with
        | Some (true, _, _) ->
            (* Padding doesn't support negative values *)
            Error (`Msg "Not a padding utility")
        | Some (false, prefix, value) -> (
            if Spacing.is_margin_prefix prefix then
              Error (`Msg "Not a padding utility")
            else
              match Spacing.axis_of_prefix prefix with
              | None -> Error (`Msg "Not a padding utility")
              | Some axis -> (
                  match Spacing.parse_value_string ~allow_auto:false value with
                  | None -> Error (`Msg "Not a padding utility")
                  | Some (#spacing as spacing_val) ->
                      Ok { axis; value = Standard spacing_val }
                  | Some `Auto -> Error (`Msg "Not a padding utility")))
        | None -> Error (`Msg "Not a padding utility"))
end

open Handler

let () = Utility.register (module Handler)
let utility axis value = Utility.base (Self { axis; value })
let p n = utility `All (Handler.Standard (Spacing.int n))
let px n = utility `X (Handler.Standard (Spacing.int n))
let py n = utility `Y (Handler.Standard (Spacing.int n))
let pt n = utility `T (Handler.Standard (Spacing.int n))
let pr n = utility `R (Handler.Standard (Spacing.int n))
let pb n = utility `B (Handler.Standard (Spacing.int n))
let pl n = utility `L (Handler.Standard (Spacing.int n))
let p_px = utility `All (Handler.Standard `Px)
let p_full = utility `All (Handler.Standard `Full)
let px_px = utility `X (Handler.Standard `Px)
let px_full = utility `X (Handler.Standard `Full)
let py_px = utility `Y (Handler.Standard `Px)
let py_full = utility `Y (Handler.Standard `Full)
let pt_px = utility `T (Handler.Standard `Px)
let pt_full = utility `T (Handler.Standard `Full)
let pr_px = utility `R (Handler.Standard `Px)
let pr_full = utility `R (Handler.Standard `Full)
let pb_px = utility `B (Handler.Standard `Px)
let pb_full = utility `B (Handler.Standard `Full)
let pl_px = utility `L (Handler.Standard `Px)
let pl_full = utility `L (Handler.Standard `Full)
