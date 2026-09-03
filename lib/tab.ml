(** tab-size utilities *)

module Css = Cascade.Css

module Handler = struct
  open Style

  (* The bracket carries either a value the tab-size grammar reads or, when it
     does not, the author's text verbatim: Tailwind hands a bracket to the
     declaration unvalidated, so [tab-[0x4]] is [tab-size: 0x4] and not the [4]
     an OCaml number reader would fold it to. *)
  type t =
    | Tab of int
    | Tab_arbitrary of string * [ `Size of Css.tab_size | `Raw of string ]

  let name = "tab"
  let priority _ = 26
  let suborder _ = 8340

  let to_class = function
    | Tab n -> "tab-" ^ string_of_int n
    | Tab_arbitrary (raw, _) -> "tab-" ^ raw

  let to_style _theme = function
    | Tab n -> style [ Css.tab_size n ]
    | Tab_arbitrary (_, `Size v) -> style [ Css.tab_size_value v ]
    | Tab_arbitrary (_, `Raw v) -> (
        match Parse.opaque_declaration "tab-size" v with
        | Some declaration -> style [ declaration ]
        | None -> style [])

  (* [tab-[3]] is a bare number, [tab-[12px]] a length, and anything else the
     bracket holds is passed through as written. *)
  let parse_arbitrary raw =
    if Parse.is_bracket_value raw then
      let inner = Parse.bracket_inner raw in
      let decoded = Parse.decode_arbitrary_value inner in
      match Parse.decimal_int decoded with
      | Some n -> Some (`Size (Int n : Css.tab_size))
      | None -> (
          match Css.parse_length decoded with
          | Some l -> Some (`Size (Length l : Css.tab_size))
          | None ->
              Option.map
                (fun v -> `Raw v)
                (Parse.arbitrary_declaration_value inner))
    else None

  let of_class _theme class_name =
    match Parse.split_class class_name with
    | [ "tab"; n ] -> (
        match Parse.decimal_int n with
        | Some i -> Ok (Tab i)
        | None -> (
            match parse_arbitrary n with
            | Some v -> Ok (Tab_arbitrary (n, v))
            | None -> Error (`Msg "Not a tab utility")))
    | _ -> Error (`Msg "Not a tab utility")

  let examples = [ Tab 4 ]
end

open Handler
module Utility_factory = Utility.Make (Handler)

let utility = Utility_factory.v
let tab n = utility (Tab n)
