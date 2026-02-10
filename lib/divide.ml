(** Divide utilities for creating gaps between child elements

    @see <https://tailwindcss.com/docs/divide-width>
      Tailwind CSS Divide Width documentation *)

module Handler = struct
  open Style
  open Css

  type t = Divide_x_reverse | Divide_y_reverse
  type Utility.base += Self of t

  let name = "divide"
  let priority = 8

  (* CSS Variables for divide reverse *)
  let divide_x_reverse_var =
    Var.property_default Css.Number_percentage ~initial:(Num 0.0)
      ~universal:true ~property_order:10 "tw-divide-x-reverse"

  let divide_y_reverse_var =
    Var.property_default Css.Number_percentage ~initial:(Num 0.0)
      ~universal:true ~property_order:11 "tw-divide-y-reverse"

  (* divide-x-reverse utility sets --tw-divide-x-reverse: 1 on children *)
  let divide_x_reverse_style () =
    let selector =
      Css.Selector.(where [ class_ "divide-x-reverse" >> not [ Last_child ] ])
    in
    let decl, _ = Var.binding divide_x_reverse_var (Css.Num 1.0) in
    let property_rules =
      [ Var.property_rule divide_x_reverse_var ] |> List.filter_map Fun.id
    in
    let rule = Css.rule ~selector [ decl ] in
    style ~rules:(Some [ rule ]) ~property_rules:(Css.concat property_rules) []

  (* divide-y-reverse utility sets --tw-divide-y-reverse: 1 on children *)
  let divide_y_reverse_style () =
    let selector =
      Css.Selector.(where [ class_ "divide-y-reverse" >> not [ Last_child ] ])
    in
    let decl, _ = Var.binding divide_y_reverse_var (Css.Num 1.0) in
    let property_rules =
      [ Var.property_rule divide_y_reverse_var ] |> List.filter_map Fun.id
    in
    let rule = Css.rule ~selector [ decl ] in
    style ~rules:(Some [ rule ]) ~property_rules:(Css.concat property_rules) []

  let to_class = function
    | Divide_x_reverse -> "divide-x-reverse"
    | Divide_y_reverse -> "divide-y-reverse"

  let to_style = function
    | Divide_x_reverse -> divide_x_reverse_style ()
    | Divide_y_reverse -> divide_y_reverse_style ()

  let suborder = function Divide_x_reverse -> 0 | Divide_y_reverse -> 1

  let of_class class_name =
    match class_name with
    | "divide-x-reverse" -> Ok Divide_x_reverse
    | "divide-y-reverse" -> Ok Divide_y_reverse
    | _ -> Error (`Msg "Not a divide utility")
end

open Handler

let () = Utility.register (module Handler)
let utility x = Utility.base (Self x)
let divide_x_reverse = utility Divide_x_reverse
let divide_y_reverse = utility Divide_y_reverse
