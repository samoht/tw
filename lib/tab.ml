(** tab-size utilities *)

module Css = Cascade.Css

module Handler = struct
  open Style

  type t = Tab of int | Tab_arbitrary of string * Css.tab_size
  type Utility.base += Self of t

  let name = "tab"
  let priority = 2
  let suborder = function Tab n -> n | Tab_arbitrary _ -> 1000

  let to_class = function
    | Tab n -> "tab-" ^ string_of_int n
    | Tab_arbitrary (raw, _) -> "tab-" ^ raw

  let to_style = function
    | Tab n -> style [ Css.tab_size n ]
    | Tab_arbitrary (_, v) -> style [ Css.tab_size_value v ]

  (* [tab-[3]] is a bare number, [tab-[12px]] a length. *)
  let parse_arbitrary raw : Css.tab_size option =
    if
      String.length raw > 2
      && raw.[0] = '['
      && raw.[String.length raw - 1] = ']'
    then
      let inner = String.sub raw 1 (String.length raw - 2) in
      match int_of_string_opt inner with
      | Some n -> Some (Int n : Css.tab_size)
      | None -> (
          match Css.parse_length inner with
          | Some l -> Some (Length l : Css.tab_size)
          | None -> None)
    else None

  let of_class class_name =
    match Parse.split_class class_name with
    | [ "tab"; n ] -> (
        match int_of_string_opt n with
        | Some i -> Ok (Tab i)
        | None -> (
            match parse_arbitrary n with
            | Some v -> Ok (Tab_arbitrary (n, v))
            | None -> Error (`Msg "Not a tab utility")))
    | _ -> Error (`Msg "Not a tab utility")
end

open Handler

let () = Utility.register (module Handler)
let utility x = Utility.base (Self x)
let tab n = utility (Tab n)
