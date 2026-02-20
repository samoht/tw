(** Scroll margin and padding utilities.

    Provides utilities for scroll-m-*, scroll-mx-*, scroll-p-*, etc. *)

module Handler = struct
  open Style

  type scroll_kind = Margin | Padding

  type axis =
    | All
    | X (* inline *)
    | Y (* block *)
    | T (* top *)
    | R (* right *)
    | B (* bottom *)
    | L (* left *)
    | S (* inline-start *)
    | E (* inline-end *)
    | Bs (* block-start *)
    | Be (* block-end *)

  type scroll_value =
    | Spacing of int (* scroll-m-4 *)
    | Arbitrary of Css.length (* scroll-m-[4px] *)
    | ArbitraryVar of string (* scroll-m-[var(--value)] *)

  type t = {
    kind : scroll_kind;
    negative : bool;
    axis : axis;
    value : scroll_value;
  }

  type Utility.base += Self of t

  let name = "scroll"
  let priority = 2

  (** Get (declaration, length) for spacing value using Theme.spacing_calc_float
  *)
  let spacing_to_decl_len ~negative n : Css.declaration * Css.length =
    if n = 0 then
      let decl, _ = Var.binding Theme.spacing_var (Css.Rem 0.25) in
      (decl, Css.Px 0.)
    else
      let mult = if negative then float_of_int (-n) else float_of_int n in
      Theme.spacing_calc_float mult

  let parse_arbitrary s : [ `Length of Css.length | `Var of string ] option =
    (* Parse [4px] or [1rem] or [var(--value)] etc. *)
    let len = String.length s in
    if len > 2 && s.[0] = '[' && s.[len - 1] = ']' then
      let inner = String.sub s 1 (len - 2) in
      (* Check if it's a var reference *)
      if String.length inner > 4 && String.sub inner 0 4 = "var(" then
        Some (`Var inner)
      else if
        (* Try to parse as a length *)
        String.ends_with ~suffix:"px" inner
      then
        let n = String.sub inner 0 (String.length inner - 2) in
        match float_of_string_opt n with
        | Some f -> Some (`Length (Css.Px f : Css.length))
        | None -> None
      else if String.ends_with ~suffix:"rem" inner then
        let n = String.sub inner 0 (String.length inner - 3) in
        match float_of_string_opt n with
        | Some f -> Some (`Length (Css.Rem f : Css.length))
        | None -> None
      else if String.ends_with ~suffix:"em" inner then
        let n = String.sub inner 0 (String.length inner - 2) in
        match float_of_string_opt n with
        | Some f -> Some (`Length (Css.Em f : Css.length))
        | None -> None
      else if String.ends_with ~suffix:"%" inner then
        let n = String.sub inner 0 (String.length inner - 1) in
        match float_of_string_opt n with
        | Some f -> Some (`Length (Css.Pct f : Css.length))
        | None -> None
      else
        (* Unknown format, try as var *)
        Some (`Var inner)
    else None

  (* Get the CSS property function for scroll margin by axis *)
  let scroll_margin_prop = function
    | All -> Css.scroll_margin
    | X -> Css.scroll_margin_inline
    | Y -> Css.scroll_margin_block
    | T -> Css.scroll_margin_top
    | R -> Css.scroll_margin_right
    | B -> Css.scroll_margin_bottom
    | L -> Css.scroll_margin_left
    | S -> Css.scroll_margin_inline_start
    | E -> Css.scroll_margin_inline_end
    | Bs -> Css.scroll_margin_block_start
    | Be -> Css.scroll_margin_block_end

  (* Get the CSS property function for scroll padding by axis *)
  let scroll_padding_prop = function
    | All -> Css.scroll_padding
    | X -> Css.scroll_padding_inline
    | Y -> Css.scroll_padding_block
    | T -> Css.scroll_padding_top
    | R -> Css.scroll_padding_right
    | B -> Css.scroll_padding_bottom
    | L -> Css.scroll_padding_left
    | S -> Css.scroll_padding_inline_start
    | E -> Css.scroll_padding_inline_end
    | Bs -> Css.scroll_padding_block_start
    | Be -> Css.scroll_padding_block_end

  let to_style { kind; negative; axis; value } =
    let prop =
      match kind with
      | Margin -> scroll_margin_prop axis
      | Padding -> scroll_padding_prop axis
    in
    match value with
    | Spacing n ->
        let decl, len = spacing_to_decl_len ~negative n in
        style [ decl; prop len ]
    | Arbitrary len ->
        if negative then
          style
            [
              prop
                (Css.Calc
                   (Css.Calc.mul (Css.Calc.length len) (Css.Calc.float (-1.))));
            ]
        else style [ prop len ]
    | ArbitraryVar var_str ->
        let bare_name = Parse.extract_var_name var_str in
        let len : Css.length =
          if negative then
            Css.Calc
              (Css.Calc.mul (Css.Calc.var bare_name) (Css.Calc.float (-1.)))
          else Css.Var (Css.var_ref bare_name)
        in
        style [ prop len ]

  let suborder { kind; negative; axis; value } =
    let kind_offset = match kind with Margin -> 0 | Padding -> 10000000 in
    let neg_offset = if negative then -5000000 else 0 in
    let axis_offset =
      match axis with
      | All -> 0
      | X -> 100000
      | Y -> 200000
      | T -> 300000
      | R -> 400000
      | B -> 500000
      | L -> 600000
      | S -> 700000
      | E -> 800000
      | Bs -> 900000
      | Be -> 1000000
    in
    let value_order =
      match value with
      | Spacing n -> n * 10
      | Arbitrary _ -> 50000
      | ArbitraryVar _ -> 50001
    in
    kind_offset + neg_offset + axis_offset + value_order

  let pp_float n =
    (* Format float without trailing dot: 4. -> 4, 4.5 -> 4.5 *)
    let s = string_of_float n in
    if String.ends_with ~suffix:"." s then String.sub s 0 (String.length s - 1)
    else s

  let pp_length_suffix (len : Css.length) =
    match len with
    | Css.Px n -> "[" ^ pp_float n ^ "px]"
    | Css.Rem n -> "[" ^ pp_float n ^ "rem]"
    | Css.Em n -> "[" ^ pp_float n ^ "em]"
    | Css.Pct n -> "[" ^ pp_float n ^ "%]"
    | _ -> "[<length>]"

  let axis_suffix = function
    | All -> ""
    | X -> "x"
    | Y -> "y"
    | T -> "t"
    | R -> "r"
    | B -> "b"
    | L -> "l"
    | S -> "s"
    | E -> "e"
    | Bs -> "bs"
    | Be -> "be"

  let to_class { kind; negative; axis; value } =
    let kind_prefix =
      match kind with Margin -> "scroll-m" | Padding -> "scroll-p"
    in
    let neg_prefix = if negative then "-" else "" in
    let axis_str = axis_suffix axis in
    let value_suffix =
      match value with
      | Spacing n -> string_of_int (abs n)
      | Arbitrary len -> pp_length_suffix len
      | ArbitraryVar s -> "[" ^ s ^ "]"
    in
    neg_prefix ^ kind_prefix ^ axis_str ^ "-" ^ value_suffix

  let axis_of_suffix = function
    | "" -> Some All
    | "x" -> Some X
    | "y" -> Some Y
    | "t" -> Some T
    | "r" -> Some R
    | "b" -> Some B
    | "l" -> Some L
    | "s" -> Some S
    | "e" -> Some E
    | "bs" -> Some Bs
    | "be" -> Some Be
    | _ -> None

  let of_class class_name =
    let parts = Parse.split_class class_name in
    match parts with
    (* scroll-m-4, scroll-p-4, scroll-mx-4, scroll-py-4, etc. *)
    | [ "scroll"; kind_axis; value ] | [ ""; "scroll"; kind_axis; value ] -> (
        let negative = List.hd parts = "" in
        let kind, axis_suffix =
          if String.length kind_axis >= 1 then
            let first = kind_axis.[0] in
            let rest =
              if String.length kind_axis > 1 then
                String.sub kind_axis 1 (String.length kind_axis - 1)
              else ""
            in
            match first with
            | 'm' -> (Some Margin, rest)
            | 'p' -> (Some Padding, rest)
            | _ -> (None, "")
          else (None, "")
        in
        match (kind, axis_of_suffix axis_suffix) with
        | Some Padding, _ when negative ->
            Error (`Msg "Negative scroll-padding not supported")
        | Some kind, Some axis -> (
            (* Try as integer spacing *)
            match int_of_string_opt value with
            | Some n -> Ok { kind; negative; axis; value = Spacing n }
            | None -> (
                (* Try as arbitrary value *)
                match parse_arbitrary value with
                | Some (`Length len) ->
                    Ok { kind; negative; axis; value = Arbitrary len }
                | Some (`Var var_str) ->
                    Ok { kind; negative; axis; value = ArbitraryVar var_str }
                | None -> Error (`Msg "Not a scroll utility")))
        | _ -> Error (`Msg "Not a scroll utility"))
    (* Handle block-start/end with longer axis names: scroll-mbs-4 *)
    | [ "scroll"; kind_axis; value2; value3 ]
    | [ ""; "scroll"; kind_axis; value2; value3 ] -> (
        let negative = List.hd parts = "" in
        (* Reconstruct as scroll-mbs-4 might have been split as scroll-mbs /
           4 *)
        let combined_axis = kind_axis ^ "-" ^ value2 in
        let kind, axis_suffix =
          if String.length combined_axis >= 1 then
            let first = combined_axis.[0] in
            let rest =
              if String.length combined_axis > 1 then
                String.sub combined_axis 1 (String.length combined_axis - 1)
              else ""
            in
            match first with
            | 'm' -> (Some Margin, rest)
            | 'p' -> (Some Padding, rest)
            | _ -> (None, "")
          else (None, "")
        in
        match (kind, axis_of_suffix axis_suffix) with
        | Some Padding, _ when negative ->
            Error (`Msg "Negative scroll-padding not supported")
        | Some kind, Some axis -> (
            match int_of_string_opt value3 with
            | Some n -> Ok { kind; negative; axis; value = Spacing n }
            | None -> (
                match parse_arbitrary value3 with
                | Some (`Length len) ->
                    Ok { kind; negative; axis; value = Arbitrary len }
                | Some (`Var var_str) ->
                    Ok { kind; negative; axis; value = ArbitraryVar var_str }
                | None -> Error (`Msg "Not a scroll utility")))
        | _ -> Error (`Msg "Not a scroll utility"))
    | _ -> Error (`Msg "Not a scroll utility")
end

open Handler

let () = Utility.register (module Handler)

let utility kind negative axis value =
  Utility.base (Self { kind; negative; axis; value })

(* Scroll margin utilities *)
let scroll_m n = utility Margin (n < 0) All (Spacing (abs n))
let scroll_mx n = utility Margin (n < 0) X (Spacing (abs n))
let scroll_my n = utility Margin (n < 0) Y (Spacing (abs n))
let scroll_mt n = utility Margin (n < 0) T (Spacing (abs n))
let scroll_mr n = utility Margin (n < 0) R (Spacing (abs n))
let scroll_mb n = utility Margin (n < 0) B (Spacing (abs n))
let scroll_ml n = utility Margin (n < 0) L (Spacing (abs n))
let scroll_ms n = utility Margin (n < 0) S (Spacing (abs n))
let scroll_me n = utility Margin (n < 0) E (Spacing (abs n))
let scroll_mbs n = utility Margin (n < 0) Bs (Spacing (abs n))
let scroll_mbe n = utility Margin (n < 0) Be (Spacing (abs n))

(* Scroll padding utilities *)
let scroll_p n = utility Padding (n < 0) All (Spacing (abs n))
let scroll_px n = utility Padding (n < 0) X (Spacing (abs n))
let scroll_py n = utility Padding (n < 0) Y (Spacing (abs n))
let scroll_pt n = utility Padding (n < 0) T (Spacing (abs n))
let scroll_pr n = utility Padding (n < 0) R (Spacing (abs n))
let scroll_pb n = utility Padding (n < 0) B (Spacing (abs n))
let scroll_pl n = utility Padding (n < 0) L (Spacing (abs n))
let scroll_ps n = utility Padding (n < 0) S (Spacing (abs n))
let scroll_pe n = utility Padding (n < 0) E (Spacing (abs n))
let scroll_pbs n = utility Padding (n < 0) Bs (Spacing (abs n))
let scroll_pbe n = utility Padding (n < 0) Be (Spacing (abs n))
