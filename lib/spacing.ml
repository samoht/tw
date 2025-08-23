(** Spacing utilities for padding, margin, and gap *)

open Core
open Css

(* Pretty-printing helpers *)
let pp_spacing_suffix : spacing -> string = function
  | `Px -> "px"
  | `Full -> "full"
  | `Rem f ->
      (* Convert rem values back to Tailwind scale *)
      let n = int_of_float (f /. 0.25) in
      string_of_int (abs n)

let pp_margin_suffix : margin -> string = function
  | `Auto -> "auto"
  | #spacing as s -> pp_spacing_suffix s

(* Convert spacing to CSS length *)
let spacing_to_length : spacing -> length = function
  | `Px -> Px 1
  | `Full -> Pct 100.0
  | `Rem f ->
      let n = int_of_float (f /. 0.25) in
      Calc (Calc.mul (Calc.var "spacing") (Calc.float (float_of_int n)))

let margin_to_length : margin -> length = function
  | `Auto -> Auto
  | #spacing as s -> spacing_to_length s

(* Helper to extract spacing variables from spacing types *)
let spacing_vars = function
  | `Rem _ ->
      [] (* The --spacing variable is handled via string parsing in all_vars *)
  | _ -> []

(* Helper to extract spacing variables from margin types *)
let margin_vars = function
  | `Rem _ ->
      [] (* The --spacing variable is handled via string parsing in all_vars *)
  | _ -> []

(** {2 Helper Functions} *)

(* Helper to convert int to spacing *)
let int n = `Rem (float_of_int n *. 0.25)

(** {2 Typed Padding Utilities} *)

let p' (s : spacing) =
  let class_name = "p-" ^ pp_spacing_suffix s in
  let len = spacing_to_length s in
  style_with_vars class_name [ padding len ] (spacing_vars s)

let px' (s : spacing) =
  let class_name = "px-" ^ pp_spacing_suffix s in
  let len = spacing_to_length s in
  style_with_vars class_name [ padding_inline len ] (spacing_vars s)

let py' (s : spacing) =
  let class_name = "py-" ^ pp_spacing_suffix s in
  let len = spacing_to_length s in
  style_with_vars class_name [ padding_block len ] (spacing_vars s)

let pt' (s : spacing) =
  let class_name = "pt-" ^ pp_spacing_suffix s in
  let len = spacing_to_length s in
  style_with_vars class_name [ padding_top len ] (spacing_vars s)

let pr' (s : spacing) =
  let class_name = "pr-" ^ pp_spacing_suffix s in
  let len = spacing_to_length s in
  style_with_vars class_name [ padding_right len ] (spacing_vars s)

let pb' (s : spacing) =
  let class_name = "pb-" ^ pp_spacing_suffix s in
  let len = spacing_to_length s in
  style_with_vars class_name [ padding_bottom len ] (spacing_vars s)

let pl' (s : spacing) =
  let class_name = "pl-" ^ pp_spacing_suffix s in
  let len = spacing_to_length s in
  style_with_vars class_name [ padding_left len ] (spacing_vars s)

(** {2 Int-based Padding Utilities} *)

let p n = p' (int n)
let px n = px' (int n)
let py n = py' (int n)
let pt n = pt' (int n)
let pr n = pr' (int n)
let pb n = pb' (int n)
let pl n = pl' (int n)

(** {2 Typed Margin Utilities} *)

let m' (m : margin) =
  let class_name = "m-" ^ pp_margin_suffix m in
  let len = margin_to_length m in
  style_with_vars class_name [ margin len ] (margin_vars m)

let mx' (m : margin) =
  let v = margin_to_length m in
  let class_name = "mx-" ^ pp_margin_suffix m in
  style_with_vars class_name [ margin_inline v ] (margin_vars m)

let my' (m : margin) =
  let v = margin_to_length m in
  let class_name = "my-" ^ pp_margin_suffix m in
  style_with_vars class_name [ margin_block v ] (margin_vars m)

let mt' (m : margin) =
  let class_name = "mt-" ^ pp_margin_suffix m in
  let len = margin_to_length m in
  style_with_vars class_name [ margin_top len ] (margin_vars m)

let mr' (m : margin) =
  let class_name = "mr-" ^ pp_margin_suffix m in
  let len = margin_to_length m in
  style_with_vars class_name [ margin_right len ] (margin_vars m)

let mb' (m : margin) =
  let class_name = "mb-" ^ pp_margin_suffix m in
  let len = margin_to_length m in
  style_with_vars class_name [ margin_bottom len ] (margin_vars m)

let ml' (m : margin) =
  let class_name = "ml-" ^ pp_margin_suffix m in
  let len = margin_to_length m in
  style_with_vars class_name [ margin_left len ] (margin_vars m)

(** {2 Int-based Margin Utilities} *)

let m n =
  let s = int n in
  let prefix = if n < 0 then "-" else "" in
  let class_name = prefix ^ "m-" ^ pp_spacing_suffix s in
  let len = spacing_to_length s in
  style_with_vars class_name [ margin len ] (spacing_vars s)

let mx n =
  let s = int n in
  let prefix = if n < 0 then "-" else "" in
  let class_name = prefix ^ "mx-" ^ pp_spacing_suffix s in
  let len = spacing_to_length s in
  style_with_vars class_name [ margin_inline len ] (spacing_vars s)

let my n =
  let s = int n in
  let prefix = if n < 0 then "-" else "" in
  let class_name = prefix ^ "my-" ^ pp_spacing_suffix s in
  let len = spacing_to_length s in
  style_with_vars class_name [ margin_block len ] (spacing_vars s)

let mt n =
  let s = int n in
  let prefix = if n < 0 then "-" else "" in
  let class_name = prefix ^ "mt-" ^ pp_spacing_suffix s in
  let len = spacing_to_length s in
  style_with_vars class_name [ margin_top len ] (spacing_vars s)

let mr n =
  let s = int n in
  let prefix = if n < 0 then "-" else "" in
  let class_name = prefix ^ "mr-" ^ pp_spacing_suffix s in
  let len = spacing_to_length s in
  style_with_vars class_name [ margin_right len ] (spacing_vars s)

let mb n =
  let s = int n in
  let prefix = if n < 0 then "-" else "" in
  let class_name = prefix ^ "mb-" ^ pp_spacing_suffix s in
  let len = spacing_to_length s in
  style_with_vars class_name [ margin_bottom len ] (spacing_vars s)

let ml n =
  let s = int n in
  let prefix = if n < 0 then "-" else "" in
  let class_name = prefix ^ "ml-" ^ pp_spacing_suffix s in
  let len = spacing_to_length s in
  style_with_vars class_name [ margin_left len ] (spacing_vars s)

(** {2 Typed Gap Utilities} *)

let gap' (s : spacing) =
  let class_name = "gap-" ^ pp_spacing_suffix s in
  let len = spacing_to_length s in
  style_with_vars class_name [ gap len ] (spacing_vars s)

let gap_x' (s : spacing) =
  let class_name = "gap-x-" ^ pp_spacing_suffix s in
  let len = spacing_to_length s in
  style_with_vars class_name [ column_gap len ] (spacing_vars s)

let gap_y' (s : spacing) =
  let class_name = "gap-y-" ^ pp_spacing_suffix s in
  let len = spacing_to_length s in
  style_with_vars class_name [ row_gap len ] (spacing_vars s)

(** {2 Int-based Gap Utilities} *)

let gap n = gap' (int n)
let gap_x n = gap_x' (int n)
let gap_y n = gap_y' (int n)

(** {2 Space Between Utilities} *)

let space_x n =
  let s = int n in
  let class_name = "space-x-" ^ pp_spacing_suffix s in
  let len = spacing_to_length s in
  style_with_vars class_name [ margin_left len ] (spacing_vars s)

let space_y n =
  let s = int n in
  let class_name = "space-y-" ^ pp_spacing_suffix s in
  let len = spacing_to_length s in
  style_with_vars class_name [ margin_top len ] (spacing_vars s)

(** {2 Parsing Functions} *)

let int_of_string_positive name s =
  match int_of_string_opt s with
  | None -> Error (`Msg ("Invalid " ^ name ^ " value: " ^ s))
  | Some n when n >= 0 -> Ok n
  | Some _ -> Error (`Msg (name ^ " must be non-negative: " ^ s))

let ( >|= ) r f = Result.map f r

let spacing_of_string prefix px_var full_var int_fn = function
  | [ p; "px" ] when p = prefix -> Ok px_var
  | [ p; "full" ] when p = prefix -> Ok full_var
  | [ p; n ] when p = prefix ->
      let name =
        if prefix = "p" then "padding"
        else if prefix = "px" then "padding-x"
        else if prefix = "py" then "padding-y"
        else "padding-" ^ String.sub prefix 1 (String.length prefix - 1)
      in
      int_of_string_positive name n >|= int_fn
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
      int_of_string_positive name n >|= int_fn
  | _ -> Error (`Msg "")

let gap_of_string = function
  | [ "gap"; "px" ] -> Ok (gap' `Px)
  | [ "gap"; "full" ] -> Ok (gap' `Full)
  | [ "gap"; n ] -> int_of_string_positive "gap" n >|= gap
  | [ "gap"; "x"; "px" ] -> Ok (gap_x' `Px)
  | [ "gap"; "x"; "full" ] -> Ok (gap_x' `Full)
  | [ "gap"; "x"; n ] -> int_of_string_positive "gap-x" n >|= gap_x
  | [ "gap"; "y"; "px" ] -> Ok (gap_y' `Px)
  | [ "gap"; "y"; "full" ] -> Ok (gap_y' `Full)
  | [ "gap"; "y"; n ] -> int_of_string_positive "gap-y" n >|= gap_y
  | _ -> Error (`Msg "")

let of_string parts =
  match parts with
  | "p" :: _ -> spacing_of_string "p" (p' `Px) (p' `Full) p parts
  | "px" :: _ -> spacing_of_string "px" (px' `Px) (px' `Full) px parts
  | "py" :: _ -> spacing_of_string "py" (py' `Px) (py' `Full) py parts
  | "pt" :: _ -> spacing_of_string "pt" (pt' `Px) (pt' `Full) pt parts
  | "pr" :: _ -> spacing_of_string "pr" (pr' `Px) (pr' `Full) pr parts
  | "pb" :: _ -> spacing_of_string "pb" (pb' `Px) (pb' `Full) pb parts
  | "pl" :: _ -> spacing_of_string "pl" (pl' `Px) (pl' `Full) pl parts
  | "m" :: _ -> margin_of_string "m" (m' `Auto) m parts
  | "mx" :: _ -> margin_of_string "mx" (mx' `Auto) mx parts
  | "my" :: _ -> margin_of_string "my" (my' `Auto) my parts
  | "mt" :: _ -> margin_of_string "mt" (mt' `Auto) mt parts
  | "mr" :: _ -> margin_of_string "mr" (mr' `Auto) mr parts
  | "mb" :: _ -> margin_of_string "mb" (mb' `Auto) mb parts
  | "ml" :: _ -> margin_of_string "ml" (ml' `Auto) ml parts
  | "gap" :: _ -> gap_of_string parts
  | _ -> Error (`Msg "Not a spacing utility")
