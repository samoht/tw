(** Spacing utilities for padding, margin. *)

open Core
open Css

(* Define the spacing variable at the module level *)
let spacing_var = Var.create Css.Length "spacing" ~layer:Theme ~order:4

module Parse = Parse

(* Pretty-printing helpers *)
let pp_spacing_suffix : spacing -> string = function
  | `Px -> "px"
  | `Full -> "full"
  | `Rem f ->
      (* Convert rem values back to Tailwind scale *)
      let scale = f /. 0.25 in
      if Float.is_integer scale then string_of_int (abs (int_of_float scale))
      else
        (* Handle decimal values like 0.5, 1.5, etc. *)
        let abs_scale = Float.abs scale in
        let s = string_of_float abs_scale in
        (* Remove trailing zeros and decimal point if integer *)
        if String.contains s '.' then
          let len = String.length s in
          let rec find_end i =
            if i < 0 then 0
            else if s.[i] = '0' then find_end (i - 1)
            else if s.[i] = '.' then i
            else i + 1
          in
          String.sub s 0 (find_end (len - 1))
        else s

let pp_margin_suffix : margin -> string = function
  | `Auto -> "auto"
  | #spacing as s -> pp_spacing_suffix s

(* Convert spacing to CSS length *)
let to_length : spacing -> length = function
  | `Px -> Px 1.
  | `Full -> Pct 100.0
  | `Rem f ->
      let n = int_of_float (f /. 0.25) in
      let _, spacing_ref = Var.binding spacing_var (Rem 0.25) in
      Calc
        (Calc.mul (Calc.length (Var spacing_ref)) (Calc.float (float_of_int n)))

let margin_to_length : margin -> length = function
  | `Auto -> Auto
  | #spacing as s -> to_length s

(** {2 Helper Functions} *)

(* Helper to convert int to spacing *)
let int n = `Rem (float_of_int n *. 0.25)
let decimal f = `Rem (f *. 0.25)

(** {2 Typed Padding Utilities} *)

(* Helper to include spacing variable binding when needed *)
let apply_style class_name decls s =
  match s with
  | `Rem _ ->
      let decl, _ = Var.binding spacing_var (Rem 0.25) in
      style class_name (decl :: decls)
  | _ -> style class_name decls

let padding_util prefix prop (s : spacing) =
  let class_name = prefix ^ pp_spacing_suffix s in
  let len = to_length s in
  apply_style class_name [ prop len ] s

let padding_list_util prefix prop (s : spacing) =
  let class_name = prefix ^ pp_spacing_suffix s in
  let len = to_length s in
  apply_style class_name [ prop [ len ] ] s

let p' = padding_list_util "p-" padding
let px' = padding_util "px-" padding_inline
let py' = padding_util "py-" padding_block
let pt' = padding_util "pt-" padding_top
let pr' = padding_util "pr-" padding_right
let pb' = padding_util "pb-" padding_bottom
let pl' = padding_util "pl-" padding_left

(** {2 Int-based Padding Utilities} *)

let p n = p' (int n)
let px n = px' (int n)
let py n = py' (int n)
let pt n = pt' (int n)
let pr n = pr' (int n)
let pb n = pb' (int n)
let pl n = pl' (int n)

(* Decimal-aware versions for parsing *)
let p_decimal f = p' (decimal f)
let px_decimal f = px' (decimal f)
let py_decimal f = py' (decimal f)
let pt_decimal f = pt' (decimal f)
let pr_decimal f = pr' (decimal f)
let pb_decimal f = pb' (decimal f)
let pl_decimal f = pl' (decimal f)

(** {2 Typed Margin Utilities} *)

(* Helper to include spacing variable binding when margin uses Rem *)
let margin_style class_name decls m =
  match m with
  | `Rem _ ->
      let decl, _ = Var.binding spacing_var (Rem 0.25) in
      style class_name (decl :: decls)
  | _ -> style class_name decls

let margin_util prefix prop (m : margin) =
  let class_name = prefix ^ pp_margin_suffix m in
  let len = margin_to_length m in
  margin_style class_name [ prop len ] m

let margin_list_util prefix prop (m : margin) =
  let class_name = prefix ^ pp_margin_suffix m in
  let len = margin_to_length m in
  margin_style class_name [ prop [ len ] ] m

let m' = margin_list_util "m-" margin
let mx' = margin_util "mx-" margin_inline
let my' = margin_util "my-" margin_block
let mt' = margin_util "mt-" margin_top
let mr' = margin_util "mr-" margin_right
let mb' = margin_util "mb-" margin_bottom
let ml' = margin_util "ml-" margin_left

(** {2 Int-based Margin Utilities} *)

let m n =
  let s = int n in
  let prefix = if n < 0 then "-" else "" in
  let class_name = prefix ^ "m-" ^ pp_margin_suffix s in
  margin_style class_name [ margin [ margin_to_length s ] ] s

let mx n =
  let s = int n in
  let prefix = if n < 0 then "-" else "" in
  let class_name = prefix ^ "mx-" ^ pp_margin_suffix s in
  let len = margin_to_length s in
  margin_style class_name [ margin_inline len ] s

let my n =
  let s = int n in
  let prefix = if n < 0 then "-" else "" in
  let class_name = prefix ^ "my-" ^ pp_margin_suffix s in
  let len = margin_to_length s in
  margin_style class_name [ margin_block len ] s

let mt n =
  let s = int n in
  let prefix = if n < 0 then "-" else "" in
  let class_name = prefix ^ "mt-" ^ pp_margin_suffix s in
  let len = margin_to_length s in
  margin_style class_name [ margin_top len ] s

let mr n =
  let s = int n in
  let prefix = if n < 0 then "-" else "" in
  let class_name = prefix ^ "mr-" ^ pp_margin_suffix s in
  let len = margin_to_length s in
  margin_style class_name [ margin_right len ] s

let mb n =
  let s = int n in
  let prefix = if n < 0 then "-" else "" in
  let class_name = prefix ^ "mb-" ^ pp_margin_suffix s in
  let len = margin_to_length s in
  margin_style class_name [ margin_bottom len ] s

let ml n =
  let s = int n in
  let prefix = if n < 0 then "-" else "" in
  let class_name = prefix ^ "ml-" ^ pp_margin_suffix s in
  let len = margin_to_length s in
  margin_style class_name [ margin_left len ] s

(** {2 Space Between Utilities} *)

let space_x n =
  let s = int n in
  let prefix = if n < 0 then "-" else "" in
  let class_name = prefix ^ "space-x-" ^ pp_spacing_suffix s in
  match s with
  | `Rem _ ->
      let decl, spacing_ref = Var.binding spacing_var (Rem 0.25) in
      let n_units =
        int_of_float ((match s with `Rem f -> f | _ -> 0.) /. 0.25)
      in
      let len : Css.length =
        Calc
          Calc.(mul (length (Var spacing_ref)) (float (float_of_int n_units)))
      in
      style class_name (decl :: [ margin_left len ])
  | `Px -> style class_name [ margin_left (Px 1.) ]
  | `Full -> style class_name [ margin_left (Pct 100.0) ]

let space_y n =
  let s = int n in
  let prefix = if n < 0 then "-" else "" in
  let class_name = prefix ^ "space-y-" ^ pp_spacing_suffix s in
  match s with
  | `Rem _ ->
      let decl, spacing_ref = Var.binding spacing_var (Rem 0.25) in
      let n_units =
        int_of_float ((match s with `Rem f -> f | _ -> 0.) /. 0.25)
      in
      let len : Css.length =
        Calc
          Calc.(mul (length (Var spacing_ref)) (float (float_of_int n_units)))
      in
      style class_name (decl :: [ margin_top len ])
  | `Px -> style class_name [ margin_top (Px 1.) ]
  | `Full -> style class_name [ margin_top (Pct 100.0) ]

(** {2 Parsing Functions} *)

let ( >|= ) = Parse.( >|= )

let parse_tokens prefix px_var full_var int_fn = function
  | [ p; "px" ] when p = prefix -> Ok px_var
  | [ p; "full" ] when p = prefix -> Ok full_var
  | [ p; n ] when p = prefix -> (
      let name =
        if prefix = "p" then "padding"
        else if prefix = "px" then "padding-x"
        else if prefix = "py" then "padding-y"
        else "padding-" ^ String.sub prefix 1 (String.length prefix - 1)
      in
      Parse.spacing_value ~name n >|= fun f ->
      if Float.is_integer f then int_fn (int_of_float f)
      else
        (* Use decimal version for non-integer values *)
        match prefix with
        | "p" -> p_decimal f
        | "px" -> px_decimal f
        | "py" -> py_decimal f
        | "pt" -> pt_decimal f
        | "pr" -> pr_decimal f
        | "pb" -> pb_decimal f
        | "pl" -> pl_decimal f
        | _ -> int_fn (int_of_float f)
      (* fallback *))
  | _ -> Error (`Msg "")

let margin_of_string prefix auto_var int_fn = function
  | [ p; "auto" ] when p = prefix -> Ok auto_var
  | [ p; n ] when p = prefix ->
      let name =
        if prefix = "m" then "margin"
        else if prefix = "mx" then "margin-x"
        else if prefix = "my" then "margin-y"
        else "margin-" ^ String.sub prefix 1 (String.length prefix - 1)
      in
      Parse.int_pos ~name n >|= int_fn
  | _ -> Error (`Msg "")

let negative_margin_of_string prefix int_fn = function
  | [ p; n ] when p = prefix ->
      let name =
        if prefix = "-m" then "margin"
        else if prefix = "-mx" then "margin-x"
        else if prefix = "-my" then "margin-y"
        else "margin-" ^ String.sub prefix 2 (String.length prefix - 2)
      in
      Parse.int_pos ~name n >|= fun x -> int_fn (-x)
  | _ -> Error (`Msg "")

let of_string parts =
  match parts with
  | "p" :: _ -> parse_tokens "p" (p' `Px) (p' `Full) p parts
  | "px" :: _ -> parse_tokens "px" (px' `Px) (px' `Full) px parts
  | "py" :: _ -> parse_tokens "py" (py' `Px) (py' `Full) py parts
  | "pt" :: _ -> parse_tokens "pt" (pt' `Px) (pt' `Full) pt parts
  | "pr" :: _ -> parse_tokens "pr" (pr' `Px) (pr' `Full) pr parts
  | "pb" :: _ -> parse_tokens "pb" (pb' `Px) (pb' `Full) pb parts
  | "pl" :: _ -> parse_tokens "pl" (pl' `Px) (pl' `Full) pl parts
  | "m" :: _ -> margin_of_string "m" (m' `Auto) m parts
  | "mx" :: _ -> margin_of_string "mx" (mx' `Auto) mx parts
  | "my" :: _ -> margin_of_string "my" (my' `Auto) my parts
  | "mt" :: _ -> margin_of_string "mt" (mt' `Auto) mt parts
  | "mr" :: _ -> margin_of_string "mr" (mr' `Auto) mr parts
  | "mb" :: _ -> margin_of_string "mb" (mb' `Auto) mb parts
  | "ml" :: _ -> margin_of_string "ml" (ml' `Auto) ml parts
  | "-m" :: _ -> negative_margin_of_string "-m" m parts
  | "-mx" :: _ -> negative_margin_of_string "-mx" mx parts
  | "-my" :: _ -> negative_margin_of_string "-my" my parts
  | "-mt" :: _ -> negative_margin_of_string "-mt" mt parts
  | "-mr" :: _ -> negative_margin_of_string "-mr" mr parts
  | "-mb" :: _ -> negative_margin_of_string "-mb" mb parts
  | "-ml" :: _ -> negative_margin_of_string "-ml" ml parts
  | [ "space"; "x"; n ] -> Parse.int_pos ~name:"space-x" n >|= space_x
  | [ "space"; "y"; n ] -> Parse.int_pos ~name:"space-y" n >|= space_y
  | [ "-space"; "x"; n ] ->
      Parse.int_pos ~name:"space-x" n >|= fun x -> space_x (-x)
  | [ "-space"; "y"; n ] ->
      Parse.int_pos ~name:"space-y" n >|= fun x -> space_y (-x)
  | "gap" :: _ -> Flow.of_string parts
  | _ -> Error (`Msg "Not a spacing utility")

(** {1 Special Values} *)

let p_px = p' `Px
let p_full = p' `Full
let px_px = px' `Px
let px_full = px' `Full
let py_px = py' `Px
let py_full = py' `Full
let pt_px = pt' `Px
let pt_full = pt' `Full
let pr_px = pr' `Px
let pr_full = pr' `Full
let pb_px = pb' `Px
let pb_full = pb' `Full
let pl_px = pl' `Px
let pl_full = pl' `Full
let m_auto = m' `Auto
let mx_auto = mx' `Auto
let my_auto = my' `Auto
let mt_auto = mt' `Auto
let mr_auto = mr' `Auto
let mb_auto = mb' `Auto
let ml_auto = ml' `Auto
