(** Columns utilities for multi-column layout

    @see <https://tailwindcss.com/docs/columns>
      Tailwind CSS Columns documentation *)

module Handler = struct
  open Style
  open Css

  type t =
    | Columns_auto
    | Columns_count of int
    | Columns_3xs
    | Columns_2xs
    | Columns_xs
    | Columns_sm
    | Columns_md
    | Columns_lg
    | Columns_xl
    | Columns_2xl
    | Columns_3xl
    | Columns_4xl
    | Columns_5xl
    | Columns_6xl
    | Columns_7xl
    | Columns_arbitrary of int
    | Columns_bracket_var of string

  type Utility.base += Self of t

  let name = "columns"
  let priority = 12

  (* Helper to create a columns style with a container variable. Container theme
     variables are exported from Sizing module. *)
  let columns_with_var var default_value =
    let decl, ref_ = Var.binding var default_value in
    style [ decl; columns (Width (Var ref_)) ]

  let to_style = function
    | Columns_auto ->
        let v : Css.columns_value =
          Var
            (Var.theme_ref "columns-auto"
               ~default:(Auto : Css.columns_value)
               ~default_css:"auto")
        in
        style [ columns v ]
    | Columns_count n -> style [ columns (Count n) ]
    | Columns_3xs -> columns_with_var Sizing.container_3xs (Rem 16.0)
    | Columns_2xs -> columns_with_var Sizing.container_2xs (Rem 18.0)
    | Columns_xs -> columns_with_var Sizing.container_xs (Rem 20.0)
    | Columns_sm -> columns_with_var Sizing.container_sm (Rem 24.0)
    | Columns_md -> columns_with_var Sizing.container_md (Rem 28.0)
    | Columns_lg -> columns_with_var Sizing.container_lg (Rem 32.0)
    | Columns_xl -> columns_with_var Sizing.container_xl (Rem 36.0)
    | Columns_2xl -> columns_with_var Sizing.container_2xl (Rem 42.0)
    | Columns_3xl -> columns_with_var Sizing.container_3xl (Rem 48.0)
    | Columns_4xl -> columns_with_var Sizing.container_4xl (Rem 56.0)
    | Columns_5xl -> columns_with_var Sizing.container_5xl (Rem 64.0)
    | Columns_6xl -> columns_with_var Sizing.container_6xl (Rem 72.0)
    | Columns_7xl -> columns_with_var Sizing.container_7xl (Rem 80.0)
    | Columns_arbitrary n -> style [ columns (Count n) ]
    | Columns_bracket_var s ->
        let inner = Parse.extract_var_name s in
        let ref : Css.columns_value Css.var = Css.var_ref inner in
        style [ columns (Var ref) ]

  (* Tailwind sorts column utilities lexicographically by their suffix. This
     function computes a sort key that preserves lexicographic order: "1" < "10"
     < "11" < "12" < "2" < "2xl" < "2xs" < "3" < ... < "auto" < "lg" < ... Uses
     large weights so first character dominates the comparison. *)
  let string_to_sortkey s =
    let rec aux acc i =
      if i >= String.length s || i >= 4 then acc
      else
        (* Each position uses 256x smaller weight than previous *)
        let weight =
          match i with
          | 0 -> 256 * 256 * 256 (* 16777216 *)
          | 1 -> 256 * 256 (* 65536 *)
          | 2 -> 256
          | _ -> 1
        in
        aux (acc + (Char.code s.[i] * weight)) (i + 1)
    in
    aux 0 0

  let suborder t =
    let suffix =
      match t with
      | Columns_auto -> "auto"
      | Columns_count n -> string_of_int n
      | Columns_3xs -> "3xs"
      | Columns_2xs -> "2xs"
      | Columns_xs -> "xs"
      | Columns_sm -> "sm"
      | Columns_md -> "md"
      | Columns_lg -> "lg"
      | Columns_xl -> "xl"
      | Columns_2xl -> "2xl"
      | Columns_3xl -> "3xl"
      | Columns_4xl -> "4xl"
      | Columns_5xl -> "5xl"
      | Columns_6xl -> "6xl"
      | Columns_7xl -> "7xl"
      | Columns_arbitrary n -> "[" ^ string_of_int n ^ "]"
      | Columns_bracket_var _ -> "[z"
    in
    string_to_sortkey suffix

  let of_class class_name =
    let parts = Parse.split_class class_name in
    match parts with
    | [ "columns"; "auto" ] -> Ok Columns_auto
    | [ "columns"; "3xs" ] -> Ok Columns_3xs
    | [ "columns"; "2xs" ] -> Ok Columns_2xs
    | [ "columns"; "xs" ] -> Ok Columns_xs
    | [ "columns"; "sm" ] -> Ok Columns_sm
    | [ "columns"; "md" ] -> Ok Columns_md
    | [ "columns"; "lg" ] -> Ok Columns_lg
    | [ "columns"; "xl" ] -> Ok Columns_xl
    | [ "columns"; "2xl" ] -> Ok Columns_2xl
    | [ "columns"; "3xl" ] -> Ok Columns_3xl
    | [ "columns"; "4xl" ] -> Ok Columns_4xl
    | [ "columns"; "5xl" ] -> Ok Columns_5xl
    | [ "columns"; "6xl" ] -> Ok Columns_6xl
    | [ "columns"; "7xl" ] -> Ok Columns_7xl
    | [ "columns"; value ] when Parse.is_bracket_var value ->
        Ok (Columns_bracket_var (Parse.bracket_inner value))
    | [ "columns"; n ] -> (
        let len = String.length n in
        if len > 2 && n.[0] = '[' && n.[len - 1] = ']' then
          let inner = String.sub n 1 (len - 2) in
          match int_of_string_opt inner with
          | Some i -> Ok (Columns_arbitrary i)
          | None -> Error (`Msg "Invalid columns arbitrary value")
        else
          match int_of_string_opt n with
          | Some i -> Ok (Columns_count i)
          | None -> Error (`Msg "Invalid columns value"))
    | _ -> Error (`Msg "Not a columns utility")

  let to_class = function
    | Columns_auto -> "columns-auto"
    | Columns_count n -> "columns-" ^ string_of_int n
    | Columns_3xs -> "columns-3xs"
    | Columns_2xs -> "columns-2xs"
    | Columns_xs -> "columns-xs"
    | Columns_sm -> "columns-sm"
    | Columns_md -> "columns-md"
    | Columns_lg -> "columns-lg"
    | Columns_xl -> "columns-xl"
    | Columns_2xl -> "columns-2xl"
    | Columns_3xl -> "columns-3xl"
    | Columns_4xl -> "columns-4xl"
    | Columns_5xl -> "columns-5xl"
    | Columns_6xl -> "columns-6xl"
    | Columns_7xl -> "columns-7xl"
    | Columns_arbitrary n -> "columns-[" ^ string_of_int n ^ "]"
    | Columns_bracket_var s -> "columns-[" ^ s ^ "]"
end

open Handler

let () = Utility.register (module Handler)
let utility x = Utility.base (Self x)
let columns_auto = utility Columns_auto
let columns n = utility (Columns_count n)
let columns_3xs = utility Columns_3xs
let columns_2xs = utility Columns_2xs
let columns_xs = utility Columns_xs
let columns_sm = utility Columns_sm
let columns_md = utility Columns_md
let columns_lg = utility Columns_lg
let columns_xl = utility Columns_xl
let columns_2xl = utility Columns_2xl
let columns_3xl = utility Columns_3xl
let columns_4xl = utility Columns_4xl
let columns_5xl = utility Columns_5xl
let columns_6xl = utility Columns_6xl
let columns_7xl = utility Columns_7xl
