(** Spacing utilities for padding, margin, and gap *)

open Core

(* Value constructors *)
let rem f = `Rem f
let int n = rem (float_of_int n *. 0.25)
let one_px = `Px
let full = `Full

(* Convert int to length - used for space utilities *)
let int_to_length n =
  if n = 0 then Css.Zero else Css.Rem (float_of_int n *. 0.25)

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

(* Typed spacing functions with ' suffix *)
let p' (s : spacing) =
  let class_name = "p-" ^ pp_spacing_suffix s in
  let len = spacing_to_length s in
  style_with_vars class_name [ Css.padding len ] (spacing_vars s)

let px' (s : spacing) =
  let class_name = "px-" ^ pp_spacing_suffix s in
  let len = spacing_to_length s in
  style_with_vars class_name [ Css.padding_inline len ] (spacing_vars s)

let py' (s : spacing) =
  let class_name = "py-" ^ pp_spacing_suffix s in
  let len = spacing_to_length s in
  style_with_vars class_name [ Css.padding_block len ] (spacing_vars s)

let pt' (s : spacing) =
  let class_name = "pt-" ^ pp_spacing_suffix s in
  let len = spacing_to_length s in
  style_with_vars class_name [ Css.padding_top len ] (spacing_vars s)

let pr' (s : spacing) =
  let class_name = "pr-" ^ pp_spacing_suffix s in
  let len = spacing_to_length s in
  style_with_vars class_name [ Css.padding_right len ] (spacing_vars s)

let pb' (s : spacing) =
  let class_name = "pb-" ^ pp_spacing_suffix s in
  let len = spacing_to_length s in
  style_with_vars class_name [ Css.padding_bottom len ] (spacing_vars s)

let pl' (s : spacing) =
  let class_name = "pl-" ^ pp_spacing_suffix s in
  let len = spacing_to_length s in
  style_with_vars class_name [ Css.padding_left len ] (spacing_vars s)

(* Int-based spacing functions (convenience wrappers) *)
let p n = p' (int n)
let px n = px' (int n)
let py n = py' (int n)
let pt n = pt' (int n)
let pr n = pr' (int n)
let pb n = pb' (int n)
let pl n = pl' (int n)

(* Special padding values *)
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

(** {2 Margin Utilities} *)

(* Typed margin functions with ' suffix *)
let m' (m : margin) =
  let class_name = "m-" ^ pp_margin_suffix m in
  let len = margin_to_length m in
  style_with_vars class_name [ Css.margin len ] (margin_vars m)

let mx' (m : margin) =
  let v = margin_to_length m in
  let class_name = "mx-" ^ pp_margin_suffix m in
  style_with_vars class_name [ Css.margin_inline v ] (margin_vars m)

let my' (m : margin) =
  let v = margin_to_length m in
  let class_name = "my-" ^ pp_margin_suffix m in
  style_with_vars class_name [ Css.margin_block v ] (margin_vars m)

let mt' (m : margin) =
  let class_name = "mt-" ^ pp_margin_suffix m in
  let len = margin_to_length m in
  style_with_vars class_name [ Css.margin_top len ] (margin_vars m)

let mr' (m : margin) =
  let class_name = "mr-" ^ pp_margin_suffix m in
  let len = margin_to_length m in
  style_with_vars class_name [ Css.margin_right len ] (margin_vars m)

let mb' (m : margin) =
  let class_name = "mb-" ^ pp_margin_suffix m in
  let len = margin_to_length m in
  style_with_vars class_name [ Css.margin_bottom len ] (margin_vars m)

let ml' (m : margin) =
  let class_name = "ml-" ^ pp_margin_suffix m in
  let len = margin_to_length m in
  style_with_vars class_name [ Css.margin_left len ] (margin_vars m)

(* Int-based margin functions - now support negative values *)
let m n =
  let s = int n in
  let prefix = if n < 0 then "-" else "" in
  let class_name = prefix ^ "m-" ^ pp_spacing_suffix s in
  let len = spacing_to_length s in
  style_with_vars class_name [ Css.margin len ] [ Spacing (abs n) ]

let mx n =
  let s = int n in
  let len = spacing_to_length s in
  let prefix = if n < 0 then "-" else "" in
  let class_name = prefix ^ "mx-" ^ pp_spacing_suffix s in
  style_with_vars class_name [ Css.margin_inline len ] (spacing_vars s)

let my n =
  let s = int n in
  let len = spacing_to_length s in
  let prefix = if n < 0 then "-" else "" in
  let class_name = prefix ^ "my-" ^ pp_spacing_suffix s in
  style_with_vars class_name [ Css.margin_block len ] (spacing_vars s)

let mt n =
  let s = int n in
  let len = spacing_to_length s in
  let prefix = if n < 0 then "-" else "" in
  let class_name = prefix ^ "mt-" ^ pp_spacing_suffix s in
  style_with_vars class_name [ Css.margin_top len ] [ Spacing (abs n) ]

let mr n =
  let s = int n in
  let len = spacing_to_length s in
  let prefix = if n < 0 then "-" else "" in
  let class_name = prefix ^ "mr-" ^ pp_spacing_suffix s in
  style_with_vars class_name [ Css.margin_right len ] [ Spacing (abs n) ]

let mb n =
  let s = int n in
  let len = spacing_to_length s in
  let prefix = if n < 0 then "-" else "" in
  let class_name = prefix ^ "mb-" ^ pp_spacing_suffix s in
  style_with_vars class_name [ Css.margin_bottom len ] [ Spacing (abs n) ]

let ml n =
  let s = int n in
  let len = spacing_to_length s in
  let prefix = if n < 0 then "-" else "" in
  let class_name = prefix ^ "ml-" ^ pp_spacing_suffix s in
  style_with_vars class_name [ Css.margin_left len ] [ Spacing (abs n) ]

(* Common margin utilities *)
let m_auto = m' `Auto
let mx_auto = mx' `Auto (* Very common for centering *)
let my_auto = my' `Auto
let mt_auto = mt' `Auto
let mr_auto = mr' `Auto
let mb_auto = mb' `Auto
let ml_auto = ml' `Auto

(** {2 Gap Utilities} *)

(* Typed gap functions with ' suffix *)
let gap' (s : spacing) =
  let class_name = "gap-" ^ pp_spacing_suffix s in
  let len = spacing_to_length s in
  style_with_vars class_name [ Css.gap len ] (spacing_vars s)

let gap_x' (s : spacing) =
  let class_name = "gap-x-" ^ pp_spacing_suffix s in
  let len = spacing_to_length s in
  style_with_vars class_name [ Css.column_gap len ] (spacing_vars s)

let gap_y' (s : spacing) =
  let class_name = "gap-y-" ^ pp_spacing_suffix s in
  let len = spacing_to_length s in
  style_with_vars class_name [ Css.row_gap len ] (spacing_vars s)

(* Int-based gap functions (convenience wrappers) *)
let gap n = gap' (int n)
let gap_x n = gap_x' (int n)
let gap_y n = gap_y' (int n)

(* Special gap values *)
let gap_px = gap' `Px
let gap_full = gap' `Full

(** {2 Space Between Utilities} *)

(* Space between utilities *)
let space_x n =
  let class_name = "space-x-" ^ string_of_int n in
  style class_name [ Css.margin_left (int_to_length n) ]

let space_y n =
  let class_name = "space-y-" ^ string_of_int n in
  style class_name [ Css.margin_top (int_to_length n) ]
