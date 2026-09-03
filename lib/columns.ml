(** Columns utilities for multi-column layout

    @see <https://tailwindcss.com/docs/columns>
      Tailwind CSS Columns documentation *)

module Css = Cascade.Css

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
    | Columns_arbitrary_len of string * Css.length (* columns-[16rem] *)
    (* The author's text beside the value it denotes: a bracket Tailwind hands
       to the declaration unvalidated, so [columns-[0x10]] is [columns: 0x10]
       and not the [16] an OCaml number reader would fold it to. *)
    | Columns_arbitrary_raw of string * string
    | Columns_bracket_var of string

  let name = "columns"
  let priority _ = 12

  (* Helper to create a columns style with a container variable. Container theme
     variables are exported from Sizing module. *)
  let columns_with_var var default_value =
    let decl, ref_ = Var.binding var default_value in
    style [ decl; columns (Width (Var ref_)) ]

  let to_style theme = function
    | Columns_auto -> (
        let var_name = "columns-auto" in
        let ref =
          Var.theme_ref var_name
            ~default:(Auto : Css.columns_value)
            ~default_css:"auto"
        in
        match Scheme.theme_value (Some theme) var_name with
        | Some value ->
            let theme_decl =
              Css.custom_property ~layer:"theme" ("--" ^ var_name) value
            in
            style [ theme_decl; columns (Var ref) ]
        (* Without a theme override Tailwind writes the keyword, not a reference
           to a token nothing declares. *)
        | None -> style [ columns Auto ])
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
    | Columns_arbitrary_len (_, l) -> style [ columns (Width l) ]
    | Columns_arbitrary_raw (_, v) -> (
        match Parse.opaque_declaration "columns" v with
        | Some declaration -> style [ declaration ]
        | None -> style [])
    | Columns_bracket_var s ->
        let inner = Parse.extract_var_name s in
        let ref : Css.columns_value Css.var = Var.bracket inner in
        style [ columns (Var ref) ]

  (* Every column utility shares one suborder. Tailwind orders the values of a
     single utility by a natural, digit-aware compare of the candidate itself
     (columns-9 before columns-10, columns-[100px] before columns-[100rem]),
     which is what Sort falls back to once the suborders tie. A numeric key
     built from a prefix of the suffix cannot express that order and, being
     consulted first, would override it. *)
  let suborder _ = 0

  let of_class _theme class_name =
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
    | [ "columns"; n ] when Parse.is_bracket_value n -> (
        (* A count, a width, or the author's text as written. *)
        let inner = Parse.bracket_inner n in
        match Parse.decimal_int inner with
        | Some i -> Ok (Columns_arbitrary i)
        | None -> (
            match Parse.arbitrary_length inner with
            | Some l -> Ok (Columns_arbitrary_len (inner, l))
            | None -> (
                match Parse.arbitrary_declaration_value inner with
                | Some v -> Ok (Columns_arbitrary_raw (inner, v))
                | None -> Error (`Msg "Invalid columns arbitrary value"))))
    | [ "columns"; n ] -> (
        match Parse.decimal_int n with
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
    | Columns_arbitrary_len (s, _) -> "columns-[" ^ s ^ "]"
    | Columns_arbitrary_raw (s, _) -> "columns-[" ^ s ^ "]"
    | Columns_bracket_var s -> "columns-[" ^ s ^ "]"

  let examples = [ Columns_auto ]
end

open Handler
module Utility_factory = Utility.Make (Handler)

let utility = Utility_factory.v
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
