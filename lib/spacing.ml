(** Spacing utilities for padding, margin, and gap

    What's included:
    - Padding: `p`, `px`, `py`, `pt`, `pr`, `pb`, `pl` (int scale, `px`, `full`)
    - Margin: `m`, `mx`, `my`, `mt`, `mr`, `mb`, `ml` (int scale, `auto`)
    - Gap: `gap`, `gap-x`, `gap-y`
    - Space between: `space-x-N`, `space-y-N`

    What's not:
    - Arbitrary values and fractional strings outside the Tailwind spacing
      scale. Extend with custom `Css` if needed, e.g. `style "p-[3.7px]" [Css.padding (Px 3.7)]`.
    - `margin-trim` and other niche properties not present in the typed `Css` API.

    Parsing contract (`of_string`):
    - Accepted tokens include:
      * ["p"; v], ["px"; v], ["py"; v], ["pt"; v], ["pr"; v], ["pb"; v], ["pl"; v]
        where v ∈ {"px", "full", non‑negative int}
      * ["m"; v], ["mx"; v], ["my"; v], ["mt"; v], ["mr"; v], ["mb"; v], ["ml"; v]
        where v ∈ {"auto", non‑negative int}
      * ["gap"; v], ["gap"; "x"; v], ["gap"; "y"; v]
        where v ∈ {"px", "full", non‑negative int}
    - Errors: returns `Error (`Msg msg)` with a short description, or
      `Error (`Msg "Not a spacing utility")` when prefix is unknown. *)

open Core
open Css
module Parse = Parse

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
(* Removed spacing_vars: variables are inferred from declarations via Css.all_vars *)

(* Helper to extract spacing variables from margin types *)
(* Removed margin_vars: variables are inferred from declarations via Css.all_vars *)

(** {2 Helper Functions} *)

(* Helper to convert int to spacing *)
let int n = `Rem (float_of_int n *. 0.25)

(** {2 Typed Padding Utilities} *)

let p' (s : spacing) =
  let class_name = "p-" ^ pp_spacing_suffix s in
  let len = spacing_to_length s in
  style class_name [ padding len ]

let px' (s : spacing) =
  let class_name = "px-" ^ pp_spacing_suffix s in
  let len = spacing_to_length s in
  style class_name [ padding_inline len ]

let py' (s : spacing) =
  let class_name = "py-" ^ pp_spacing_suffix s in
  let len = spacing_to_length s in
  style class_name [ padding_block len ]

let pt' (s : spacing) =
  let class_name = "pt-" ^ pp_spacing_suffix s in
  let len = spacing_to_length s in
  style class_name [ padding_top len ]

let pr' (s : spacing) =
  let class_name = "pr-" ^ pp_spacing_suffix s in
  let len = spacing_to_length s in
  style class_name [ padding_right len ]

let pb' (s : spacing) =
  let class_name = "pb-" ^ pp_spacing_suffix s in
  let len = spacing_to_length s in
  style class_name [ padding_bottom len ]

let pl' (s : spacing) =
  let class_name = "pl-" ^ pp_spacing_suffix s in
  let len = spacing_to_length s in
  style class_name [ padding_left len ]

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
  style class_name [ margin len ]

let mx' (m : margin) =
  let v = margin_to_length m in
  let class_name = "mx-" ^ pp_margin_suffix m in
  style class_name [ margin_inline v ]

let my' (m : margin) =
  let v = margin_to_length m in
  let class_name = "my-" ^ pp_margin_suffix m in
  style class_name [ margin_block v ]

let mt' (m : margin) =
  let class_name = "mt-" ^ pp_margin_suffix m in
  let len = margin_to_length m in
  style class_name [ margin_top len ]

let mr' (m : margin) =
  let class_name = "mr-" ^ pp_margin_suffix m in
  let len = margin_to_length m in
  style class_name [ margin_right len ]

let mb' (m : margin) =
  let class_name = "mb-" ^ pp_margin_suffix m in
  let len = margin_to_length m in
  style class_name [ margin_bottom len ]

let ml' (m : margin) =
  let class_name = "ml-" ^ pp_margin_suffix m in
  let len = margin_to_length m in
  style class_name [ margin_left len ]

(** {2 Int-based Margin Utilities} *)

let m n =
  let s = int n in
  let prefix = if n < 0 then "-" else "" in
  let class_name = prefix ^ "m-" ^ pp_spacing_suffix s in
  let len = spacing_to_length s in
  style class_name [ margin len ]

let mx n =
  let s = int n in
  let prefix = if n < 0 then "-" else "" in
  let class_name = prefix ^ "mx-" ^ pp_spacing_suffix s in
  let len = spacing_to_length s in
  style class_name [ margin_inline len ]

let my n =
  let s = int n in
  let prefix = if n < 0 then "-" else "" in
  let class_name = prefix ^ "my-" ^ pp_spacing_suffix s in
  let len = spacing_to_length s in
  style class_name [ margin_block len ]

let mt n =
  let s = int n in
  let prefix = if n < 0 then "-" else "" in
  let class_name = prefix ^ "mt-" ^ pp_spacing_suffix s in
  let len = spacing_to_length s in
  style class_name [ margin_top len ]

let mr n =
  let s = int n in
  let prefix = if n < 0 then "-" else "" in
  let class_name = prefix ^ "mr-" ^ pp_spacing_suffix s in
  let len = spacing_to_length s in
  style class_name [ margin_right len ]

let mb n =
  let s = int n in
  let prefix = if n < 0 then "-" else "" in
  let class_name = prefix ^ "mb-" ^ pp_spacing_suffix s in
  let len = spacing_to_length s in
  style class_name [ margin_bottom len ]

let ml n =
  let s = int n in
  let prefix = if n < 0 then "-" else "" in
  let class_name = prefix ^ "ml-" ^ pp_spacing_suffix s in
  let len = spacing_to_length s in
  style class_name [ margin_left len ]

(** {2 Typed Gap Utilities} *)

let gap' (s : spacing) =
  let class_name = "gap-" ^ pp_spacing_suffix s in
  let len = spacing_to_length s in
  style class_name [ gap len ]

let gap_x' (s : spacing) =
  let class_name = "gap-x-" ^ pp_spacing_suffix s in
  let len = spacing_to_length s in
  style class_name [ column_gap len ]

let gap_y' (s : spacing) =
  let class_name = "gap-y-" ^ pp_spacing_suffix s in
  let len = spacing_to_length s in
  style class_name [ row_gap len ]

(** {2 Int-based Gap Utilities} *)

let gap n = gap' (int n)
let gap_x n = gap_x' (int n)
let gap_y n = gap_y' (int n)

(** {2 Space Between Utilities} *)

let space_x n =
  let s = int n in
  let class_name = "space-x-" ^ pp_spacing_suffix s in
  let len = spacing_to_length s in
  style class_name [ margin_left len ]

let space_y n =
  let s = int n in
  let class_name = "space-y-" ^ pp_spacing_suffix s in
  let len = spacing_to_length s in
  style class_name [ margin_top len ]

(** {2 Parsing Functions} *)

let ( >|= ) = Parse.( >|= )

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
      Parse.int_pos ~name n >|= int_fn
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

let gap_of_string = function
  | [ "gap"; "px" ] -> Ok (gap' `Px)
  | [ "gap"; "full" ] -> Ok (gap' `Full)
  | [ "gap"; n ] -> Parse.int_pos ~name:"gap" n >|= gap
  | [ "gap"; "x"; "px" ] -> Ok (gap_x' `Px)
  | [ "gap"; "x"; "full" ] -> Ok (gap_x' `Full)
  | [ "gap"; "x"; n ] -> Parse.int_pos ~name:"gap-x" n >|= gap_x
  | [ "gap"; "y"; "px" ] -> Ok (gap_y' `Px)
  | [ "gap"; "y"; "full" ] -> Ok (gap_y' `Full)
  | [ "gap"; "y"; n ] -> Parse.int_pos ~name:"gap-y" n >|= gap_y
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

let gap_px = gap' `Px
let gap_full = gap' `Full
