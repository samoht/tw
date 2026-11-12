(** Text indent utilities. *)

module Css = Cascade.Css

module Handler = struct
  open Style
  open Css

  type t =
    | Indent of float
    | Indent_neg of float
    | Indent_px
    | Indent_neg_px
    | Indent_arbitrary of string
    | Indent_raw of string * string
    | Indent_neg_arbitrary of string

  let name = "text_indent"

  (* Tailwind ranks [text-indent] at 282, between text-align and vertical-align,
     so it sorts with them at priority 24. *)
  let priority _ = 24
  let err_not_utility = Error (`Msg "Not a text-indent utility")

  (* A data-type hint chooses which longhand a bracket lands in and says nothing
     about the value. [indent-] and [underline-offset-] each write one longhand,
     so every hint lands there and the length reader is handed what follows it.
     The bracket text kept on the constructor still carries the hint, because
     the class name is what the markup holds. *)
  let arbitrary_length_after_hint inner : Css.length option =
    Option.bind (Parse.value_after_hint inner) Parse.arbitrary_length

  let of_class theme class_name =
    let parts = Parse.split_class class_name in
    match parts with
    | [ "indent"; "px" ] -> Ok Indent_px
    | [ ""; "indent"; "px" ] -> Ok Indent_neg_px
    | [ "indent"; n ] when Parse.is_bracket_value n -> (
        let inner = Parse.bracket_inner n in
        match arbitrary_length_after_hint inner with
        | Some _ -> Ok (Indent_arbitrary inner)
        | None -> (
            match Parse.arbitrary_declaration_value inner with
            | Some raw -> Ok (Indent_raw (inner, raw))
            | None -> err_not_utility))
    | [ ""; "indent"; n ] when Parse.is_bracket_value n ->
        let inner = Parse.bracket_inner n in
        if arbitrary_length_after_hint inner = None then err_not_utility
        else Ok (Indent_neg_arbitrary inner)
    | [ "indent"; n ] -> (
        match Parse.spacing_value ~name:"indent" n with
        | Ok f when Theme.has_spacing_step ~theme f -> Ok (Indent f)
        | Ok _ | Error _ -> err_not_utility)
    | [ ""; "indent"; n ] -> (
        match Parse.spacing_value ~name:"indent" n with
        | Ok f when Theme.has_spacing_step ~theme f -> Ok (Indent_neg f)
        | Ok _ | Error _ -> err_not_utility)
    | _ -> err_not_utility

  let to_class = function
    | Indent n -> "indent-" ^ Spacing.pp_spacing_suffix (`Rem (n *. 0.25))
    | Indent_neg n -> "-indent-" ^ Spacing.pp_spacing_suffix (`Rem (n *. 0.25))
    | Indent_px -> "indent-px"
    | Indent_neg_px -> "-indent-px"
    | Indent_arbitrary s -> "indent-[" ^ s ^ "]"
    | Indent_raw (s, _) -> "indent-[" ^ s ^ "]"
    | Indent_neg_arbitrary s -> "-indent-[" ^ s ^ "]"

  let suborder = function
    (* Indent - Tailwind ranks [text-indent] at 282, between [text-align] (281)
       and [vertical-align] (283), so the family sits at priority 24 with both:
       text-align holds 1001-1006 in lib/typography.ml, vertical-align holds
       1200-1207 in lib/vertical_align.ml, and one slot in between puts every
       spelling of [indent-*] where Tailwind puts it. Bare rather than
       [Property_order.slot 282], for the same reason as [Text_wrap] in
       lib/line_breaking.ml: the neighbours it sorts against are on a
       bare-integer scale.

       The whole family shares that slot and [Sort.natural_compare] on the class
       name separates it, which is the order Tailwind emits: negatives lead on
       the [-], digit runs compare numerically so [indent-8] precedes
       [indent-10], and the bracket and [px] spellings follow the bare steps.
       One slot also keeps an [indent-<n>] of any size inside it, where an
       offset derived from [n] would eventually reach vertical-align's. *)
    | Indent _ | Indent_neg _ | Indent_px | Indent_neg_px | Indent_arbitrary _
    | Indent_raw _ | Indent_neg_arbitrary _ ->
        1100

  let text_indent_length length =
    text_indent (Indent { length; hanging = false; each_line = false })

  let indent n =
    let spacing_decl, spacing_ref =
      Var.binding Theme.spacing_var Theme.spacing_base
    in
    let base : Css.length_percentage = Length (Css.Var spacing_ref) in
    let calc : Css.length_percentage Css.calc = Expr (Val base, Mul, Num n) in
    let length : Css.length_percentage = Calc calc in
    style [ spacing_decl; text_indent_length length ]

  let indent_arbitrary s =
    match arbitrary_length_after_hint s with
    | Some len -> style [ text_indent_length (Length len) ]
    | None -> style [ text_indent_length (Length (Px 0.)) ]

  (* A negated arbitrary length is [calc(<value> * -1)], the spelling Tailwind
     writes whatever the unit. [negate_length] still folds the sign for
     [-indent-px], where the CLI writes [-1px] itself. *)
  let indent_neg_arbitrary s =
    match arbitrary_length_after_hint s with
    | Some len ->
        style
          [
            text_indent_length
              (Length (Calc (Calc.mul (Calc.length len) (Calc.float (-1.)))));
          ]
    | None -> style [ text_indent_length (Length (Px 0.)) ]

  let to_style _theme = function
    | Indent n -> indent n
    | Indent_neg n -> indent (-.n)
    | Indent_px -> style [ text_indent_length (Length (Px 1.)) ]
    | Indent_neg_px -> style [ text_indent_length (Length (Px (-1.))) ]
    | Indent_arbitrary s -> indent_arbitrary s
    | Indent_raw (_, v) ->
        style (Option.to_list (Parse.opaque_declaration "text-indent" v))
    | Indent_neg_arbitrary s -> indent_neg_arbitrary s

  let examples = [ Indent_px ]
end

open Handler
module Utility_factory = Utility.Make (Handler)

let utility = Utility_factory.v
let indent' n = utility (Indent n)
let indent n = indent' (float_of_int n)
