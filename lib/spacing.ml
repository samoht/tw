(** Spacing utilities for padding, margin, and gap *)

open Core

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
let spacing_to_length : spacing -> Css.length = function
  | `Px -> Css.Px 1
  | `Full -> Css.Pct 100.0
  | `Rem f ->
      let n = int_of_float (f /. 0.25) in
      Css.Calc
        (Css.Calc.mul (Css.Calc.var "spacing")
           (Css.Calc.float (float_of_int n)))

let margin_to_length : margin -> Css.length = function
  | `Auto -> Css.Auto
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

(** {2 Padding Utilities} *)

let p (s : spacing) =
  let class_name = "p-" ^ pp_spacing_suffix s in
  let len = spacing_to_length s in
  style_with_vars class_name [ Css.padding len ] (spacing_vars s)

let px (s : spacing) =
  let class_name = "px-" ^ pp_spacing_suffix s in
  let len = spacing_to_length s in
  style_with_vars class_name [ Css.padding_inline len ] (spacing_vars s)

let py (s : spacing) =
  let class_name = "py-" ^ pp_spacing_suffix s in
  let len = spacing_to_length s in
  style_with_vars class_name [ Css.padding_block len ] (spacing_vars s)

let pt (s : spacing) =
  let class_name = "pt-" ^ pp_spacing_suffix s in
  let len = spacing_to_length s in
  style_with_vars class_name [ Css.padding_top len ] (spacing_vars s)

let pr (s : spacing) =
  let class_name = "pr-" ^ pp_spacing_suffix s in
  let len = spacing_to_length s in
  style_with_vars class_name [ Css.padding_right len ] (spacing_vars s)

let pb (s : spacing) =
  let class_name = "pb-" ^ pp_spacing_suffix s in
  let len = spacing_to_length s in
  style_with_vars class_name [ Css.padding_bottom len ] (spacing_vars s)

let pl (s : spacing) =
  let class_name = "pl-" ^ pp_spacing_suffix s in
  let len = spacing_to_length s in
  style_with_vars class_name [ Css.padding_left len ] (spacing_vars s)

(** {2 Margin Utilities} *)

let m (m : margin) =
  let class_name = "m-" ^ pp_margin_suffix m in
  let len = margin_to_length m in
  style_with_vars class_name [ Css.margin len ] (margin_vars m)

let mx (m : margin) =
  let v = margin_to_length m in
  let class_name = "mx-" ^ pp_margin_suffix m in
  style_with_vars class_name [ Css.margin_inline v ] (margin_vars m)

let my (m : margin) =
  let v = margin_to_length m in
  let class_name = "my-" ^ pp_margin_suffix m in
  style_with_vars class_name [ Css.margin_block v ] (margin_vars m)

let mt (m : margin) =
  let class_name = "mt-" ^ pp_margin_suffix m in
  let len = margin_to_length m in
  style_with_vars class_name [ Css.margin_top len ] (margin_vars m)

let mr (m : margin) =
  let class_name = "mr-" ^ pp_margin_suffix m in
  let len = margin_to_length m in
  style_with_vars class_name [ Css.margin_right len ] (margin_vars m)

let mb (m : margin) =
  let class_name = "mb-" ^ pp_margin_suffix m in
  let len = margin_to_length m in
  style_with_vars class_name [ Css.margin_bottom len ] (margin_vars m)

let ml (m : margin) =
  let class_name = "ml-" ^ pp_margin_suffix m in
  let len = margin_to_length m in
  style_with_vars class_name [ Css.margin_left len ] (margin_vars m)

(** {2 Gap Utilities} *)

let gap (s : spacing) =
  let class_name = "gap-" ^ pp_spacing_suffix s in
  let len = spacing_to_length s in
  style_with_vars class_name [ Css.gap len ] (spacing_vars s)

let gap_x (s : spacing) =
  let class_name = "gap-x-" ^ pp_spacing_suffix s in
  let len = spacing_to_length s in
  style_with_vars class_name [ Css.column_gap len ] (spacing_vars s)

let gap_y (s : spacing) =
  let class_name = "gap-y-" ^ pp_spacing_suffix s in
  let len = spacing_to_length s in
  style_with_vars class_name [ Css.row_gap len ] (spacing_vars s)

(** {2 Space Between Utilities} *)

let space_x (s : spacing) =
  let class_name = "space-x-" ^ pp_spacing_suffix s in
  let len = spacing_to_length s in
  style class_name [ Css.margin_left len ]

let space_y (s : spacing) =
  let class_name = "space-y-" ^ pp_spacing_suffix s in
  let len = spacing_to_length s in
  style class_name [ Css.margin_top len ]
