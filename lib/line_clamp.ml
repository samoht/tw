(** Line clamp utilities. *)

module Css = Cascade.Css

module Handler = struct
  open Style
  open Css

  type t = Lines of int | Arbitrary of int | Raw of string * string | None

  let name = "line_clamp"

  (* line-clamp (rank 26) sits between box-sizing and the display family, so it
     shares box-sizing's priority and sorts after it on suborder. *)
  let priority _ = 3
  let err_not_utility = Error (`Msg "Not a line-clamp utility")
  let ( >|= ) = Parse.( >|= )

  let of_class _theme class_name =
    let parts = Parse.split_class class_name in
    match parts with
    | [ "line"; "clamp"; "none" ] -> Ok None
    | [ "line"; "clamp"; n ] when Parse.is_bracket_value n -> (
        let inner = Parse.bracket_inner n in
        match Parse.decimal_int inner with
        | Some i -> Ok (Arbitrary i)
        | None -> (
            match Parse.arbitrary_declaration_value inner with
            | Some value -> Ok (Raw (inner, value))
            | None -> err_not_utility))
    | [ "line"; "clamp"; n ] ->
        Parse.int_bounded ~name:"line-clamp" ~min:0 ~max:999 n >|= fun i ->
        Lines i
    | _ -> err_not_utility

  let to_class = function
    | Lines n -> "line-clamp-" ^ string_of_int n
    | Arbitrary n -> "line-clamp-[" ^ string_of_int n ^ "]"
    | Raw (spelling, _) -> "line-clamp-[" ^ spelling ^ "]"
    | None -> "line-clamp-none"

  let suborder = function
    | Lines n -> 10000 + n
    | Arbitrary _ | Raw _ -> 11000
    | None -> 12000

  let line_clamp n =
    style
      [
        webkit_line_clamp (Lines n);
        webkit_box_orient Vertical;
        display Webkit_box;
        overflow Hidden;
      ]

  let line_clamp_raw value =
    let clamp =
      Option.to_list (Parse.opaque_declaration "-webkit-line-clamp" value)
    in
    style
      (clamp
      @ [ webkit_box_orient Vertical; display Webkit_box; overflow Hidden ])

  (* A project [@theme] token is a namespace key, not a value Tailwind type-
     checks at build time: any override of [--line-clamp-none], decimal or not,
     switches the utility to the variable-driven form. Reading it with
     [int_of_string_opt] let an OCaml-only spelling like [0x3] through by
     accident and rejected a spelling like [banana] that Tailwind still honours,
     so the branch no longer parses the value at all. *)
  let line_clamp_none_style ?theme () =
    match Scheme.theme_value theme "line-clamp-none" with
    | Some value_str ->
        let decl =
          Css.custom_property ~layer:"theme" "--line-clamp-none" value_str
        in
        let ref : Css.webkit_line_clamp Css.var =
          Var.theme_ref "line-clamp-none"
            ~default:(Css.Unset : Css.webkit_line_clamp)
            ~default_css:"unset"
        in
        style
          [
            decl;
            webkit_line_clamp (Var ref);
            webkit_box_orient Vertical;
            display Webkit_box;
            overflow Hidden;
          ]
    | None ->
        style
          [
            webkit_line_clamp Unset;
            webkit_box_orient Horizontal;
            display Block;
            overflow Visible;
          ]

  let to_style theme =
    let line_clamp_none_style () = line_clamp_none_style ~theme () in
    function
    | Lines n -> line_clamp n
    | Arbitrary n -> line_clamp n
    | Raw (_, value) -> line_clamp_raw value
    | None -> line_clamp_none_style ()

  let examples = [ None ]
end

open Handler
module Utility_factory = Utility.Make (Handler)

let utility = Utility_factory.v
let line_clamp n = utility (Lines n)
