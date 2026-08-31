(** Scroll margin and padding utilities.

    Provides utilities for scroll-m-*, scroll-mx-*, scroll-p-*, etc. *)

module Css = Cascade.Css

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
    | Spacing of float (* scroll-m-4, scroll-m-0.5 *)
    | Arbitrary of string * Css.length
      (* scroll-m-[4px], raw kept for round-trip *)
    | Arbitrary_var of string (* scroll-m-[var(--value)] *)

  type t = {
    kind : scroll_kind;
    negative : bool;
    axis : axis;
    value : scroll_value;
  }

  let name = "scroll"

  (* Scroll margin and padding follow the snap controls in the shared
     interaction band and precede scrollbar/list/appearance. *)
  let priority _ = 11

  (** Get (declaration, length) for spacing value using Theme.spacing_calc_float
  *)
  let spacing_to_decl_len ?theme ~negative n : Css.declaration * Css.length =
    if n = 0.0 then
      let decl, _ = Var.binding Theme.spacing_var Theme.spacing_base in
      (decl, Css.Px 0.)
    else
      let mult = if negative then -.n else n in
      Theme.spacing_calc_float ?theme mult

  let parse_arbitrary s : scroll_value option =
    (* Parse [4px] or [1rem] or [var(--value)] etc. Only a var() reference is a
       variable name: anything else that is not a length is not a utility, and
       reading it as one emitted [scroll-margin: var(--2vh)]. *)
    let len = String.length s in
    if len > 2 && s.[0] = '[' && s.[len - 1] = ']' then
      let inner = String.sub s 1 (len - 2) in
      if Parse.is_var inner then Some (Arbitrary_var inner)
      else
        Option.map
          (fun l -> Arbitrary (inner, l))
          (Parse.arbitrary_length inner)
    else None

  let scroll_prop kind axis len =
    match (kind, axis) with
    | Margin, All -> Css.scroll_margin [ len ]
    | Margin, X -> Css.scroll_margin_inline [ len ]
    | Margin, Y -> Css.scroll_margin_block [ len ]
    | Margin, T -> Css.scroll_margin_top len
    | Margin, R -> Css.scroll_margin_right len
    | Margin, B -> Css.scroll_margin_bottom len
    | Margin, L -> Css.scroll_margin_left len
    | Margin, S -> Css.scroll_margin_inline_start len
    | Margin, E -> Css.scroll_margin_inline_end len
    | Margin, Bs -> Css.scroll_margin_block_start len
    | Margin, Be -> Css.scroll_margin_block_end len
    | Padding, All -> Css.scroll_padding [ len ]
    | Padding, X -> Css.scroll_padding_inline [ len ]
    | Padding, Y -> Css.scroll_padding_block [ len ]
    | Padding, T -> Css.scroll_padding_top len
    | Padding, R -> Css.scroll_padding_right len
    | Padding, B -> Css.scroll_padding_bottom len
    | Padding, L -> Css.scroll_padding_left len
    | Padding, S -> Css.scroll_padding_inline_start len
    | Padding, E -> Css.scroll_padding_inline_end len
    | Padding, Bs -> Css.scroll_padding_block_start len
    | Padding, Be -> Css.scroll_padding_block_end len

  let to_style theme { kind; negative; axis; value } =
    let spacing_to_decl_len ~negative n =
      spacing_to_decl_len ~theme ~negative n
    in
    match value with
    | Spacing n ->
        let decl, len = spacing_to_decl_len ~negative n in
        style [ decl; scroll_prop kind axis len ]
    | Arbitrary (_, len) ->
        let len : Css.length =
          if negative then
            Css.Calc (Css.Calc.mul (Css.Calc.length len) (Css.Calc.float (-1.)))
          else len
        in
        style [ scroll_prop kind axis len ]
    | Arbitrary_var var_str ->
        let bare_name = Parse.extract_var_name var_str in
        let len : Css.length =
          if negative then
            Css.Calc
              (Css.Calc.mul
                 (Css.Calc.var bare_name : Css.length Css.calc)
                 (Css.Calc.float (-1.)))
          else Css.Var (Var.bracket bare_name)
        in
        style [ scroll_prop kind axis len ]

  let suborder { kind; negative; axis; value } =
    let interaction_offset = 1_200_000 in
    let kind_offset = match kind with Margin -> 0 | Padding -> 110_000 in
    (* Side decides first and the sign is a tie-break inside the side, which is
       the order Tailwind emits: the negative of a side sits with that side, not
       ahead of every positive. The logical sides come before the physical
       ones. *)
    let axis_offset =
      match axis with
      | All -> 0
      | X -> 10_000
      | Y -> 20_000
      | S -> 30_000
      | E -> 40_000
      | Bs -> 50_000
      | Be -> 60_000
      | T -> 70_000
      | R -> 80_000
      | B -> 90_000
      | L -> 100_000
    in
    let neg_offset = if negative then 0 else 5_000 in
    (* Half the axis band each, so a value can never carry a rule across the
       sign boundary: the arbitrary spellings sit at the top of their half. *)
    let value_order =
      match value with
      | Spacing n -> min 4_997 (int_of_float (n *. 10.))
      | Arbitrary _ -> 4_998
      | Arbitrary_var _ -> 4_999
    in
    interaction_offset + kind_offset + neg_offset + axis_offset + value_order

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
      | Spacing n -> Spacing.pp_spacing_suffix (`Rem (Float.abs n *. 0.25))
      | Arbitrary (raw, _) -> "[" ^ raw ^ "]"
      | Arbitrary_var s -> "[" ^ s ^ "]"
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

  let of_class theme class_name =
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
            (* Try as spacing value (integer or fractional like 0.5, 2.5) *)
            match Parse.spacing_value ~name:"scroll" value with
            | Ok n when Theme.has_spacing_step ~theme n ->
                Ok { kind; negative; axis; value = Spacing n }
            | Ok _ | Error _ -> (
                (* Try as arbitrary value *)
                match parse_arbitrary value with
                | Some value -> Ok { kind; negative; axis; value }
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
            match Parse.spacing_value ~name:"scroll" value3 with
            | Ok n -> Ok { kind; negative; axis; value = Spacing n }
            | Error _ -> (
                match parse_arbitrary value3 with
                | Some value -> Ok { kind; negative; axis; value }
                | None -> Error (`Msg "Not a scroll utility")))
        | _ -> Error (`Msg "Not a scroll utility"))
    | _ -> Error (`Msg "Not a scroll utility")

  let examples =
    [
      { kind = Margin; negative = false; axis = All; value = Spacing 1. };
      { kind = Margin; negative = false; axis = X; value = Spacing 1. };
      { kind = Margin; negative = false; axis = Y; value = Spacing 1. };
      { kind = Margin; negative = false; axis = T; value = Spacing 1. };
      { kind = Margin; negative = false; axis = R; value = Spacing 1. };
      { kind = Margin; negative = false; axis = B; value = Spacing 1. };
      { kind = Margin; negative = false; axis = L; value = Spacing 1. };
      { kind = Margin; negative = false; axis = S; value = Spacing 1. };
      { kind = Margin; negative = false; axis = E; value = Spacing 1. };
      { kind = Margin; negative = false; axis = Bs; value = Spacing 1. };
      { kind = Margin; negative = false; axis = Be; value = Spacing 1. };
      { kind = Padding; negative = false; axis = All; value = Spacing 1. };
      { kind = Padding; negative = false; axis = X; value = Spacing 1. };
      { kind = Padding; negative = false; axis = Y; value = Spacing 1. };
      { kind = Padding; negative = false; axis = T; value = Spacing 1. };
      { kind = Padding; negative = false; axis = R; value = Spacing 1. };
      { kind = Padding; negative = false; axis = B; value = Spacing 1. };
      { kind = Padding; negative = false; axis = L; value = Spacing 1. };
      { kind = Padding; negative = false; axis = S; value = Spacing 1. };
      { kind = Padding; negative = false; axis = E; value = Spacing 1. };
      { kind = Padding; negative = false; axis = Bs; value = Spacing 1. };
      { kind = Padding; negative = false; axis = Be; value = Spacing 1. };
    ]
end

open Handler
module Utility_factory = Utility.Make (Handler)

let utility kind negative axis value =
  Utility_factory.v { kind; negative; axis; value }

(* Scroll margin utilities *)
let scroll_m' n = utility Margin (n < 0.0) All (Spacing (Float.abs n))
let scroll_mx' n = utility Margin (n < 0.0) X (Spacing (Float.abs n))
let scroll_my' n = utility Margin (n < 0.0) Y (Spacing (Float.abs n))
let scroll_mt' n = utility Margin (n < 0.0) T (Spacing (Float.abs n))
let scroll_mr' n = utility Margin (n < 0.0) R (Spacing (Float.abs n))
let scroll_mb' n = utility Margin (n < 0.0) B (Spacing (Float.abs n))
let scroll_ml' n = utility Margin (n < 0.0) L (Spacing (Float.abs n))
let scroll_ms' n = utility Margin (n < 0.0) S (Spacing (Float.abs n))
let scroll_me' n = utility Margin (n < 0.0) E (Spacing (Float.abs n))
let scroll_mbs' n = utility Margin (n < 0.0) Bs (Spacing (Float.abs n))
let scroll_mbe' n = utility Margin (n < 0.0) Be (Spacing (Float.abs n))
let scroll_m n = scroll_m' (float_of_int n)
let scroll_mx n = scroll_mx' (float_of_int n)
let scroll_my n = scroll_my' (float_of_int n)
let scroll_mt n = scroll_mt' (float_of_int n)
let scroll_mr n = scroll_mr' (float_of_int n)
let scroll_mb n = scroll_mb' (float_of_int n)
let scroll_ml n = scroll_ml' (float_of_int n)
let scroll_ms n = scroll_ms' (float_of_int n)
let scroll_me n = scroll_me' (float_of_int n)
let scroll_mbs n = scroll_mbs' (float_of_int n)
let scroll_mbe n = scroll_mbe' (float_of_int n)

(* Scroll padding utilities *)
let scroll_p' n = utility Padding (n < 0.0) All (Spacing (Float.abs n))
let scroll_px' n = utility Padding (n < 0.0) X (Spacing (Float.abs n))
let scroll_py' n = utility Padding (n < 0.0) Y (Spacing (Float.abs n))
let scroll_pt' n = utility Padding (n < 0.0) T (Spacing (Float.abs n))
let scroll_pr' n = utility Padding (n < 0.0) R (Spacing (Float.abs n))
let scroll_pb' n = utility Padding (n < 0.0) B (Spacing (Float.abs n))
let scroll_pl' n = utility Padding (n < 0.0) L (Spacing (Float.abs n))
let scroll_ps' n = utility Padding (n < 0.0) S (Spacing (Float.abs n))
let scroll_pe' n = utility Padding (n < 0.0) E (Spacing (Float.abs n))
let scroll_pbs' n = utility Padding (n < 0.0) Bs (Spacing (Float.abs n))
let scroll_pbe' n = utility Padding (n < 0.0) Be (Spacing (Float.abs n))
let scroll_p n = scroll_p' (float_of_int n)
let scroll_px n = scroll_px' (float_of_int n)
let scroll_py n = scroll_py' (float_of_int n)
let scroll_pt n = scroll_pt' (float_of_int n)
let scroll_pr n = scroll_pr' (float_of_int n)
let scroll_pb n = scroll_pb' (float_of_int n)
let scroll_pl n = scroll_pl' (float_of_int n)
let scroll_ps n = scroll_ps' (float_of_int n)
let scroll_pe n = scroll_pe' (float_of_int n)
let scroll_pbs n = scroll_pbs' (float_of_int n)
let scroll_pbe n = scroll_pbe' (float_of_int n)
