(** List style utilities: marker type, position and image. *)

module Css = Cascade.Css

module Handler = struct
  open Style
  open Css

  type t =
    | None
    | Disc
    | Decimal
    | Bracket_var of string
    | Bracket of string * Css.list_style_type
    | Inside
    | Outside
    | Image_none
    | Image_bracket_var of string
    | Image_bracket of string * Css.list_style_image
    | Image_raw of string * string
    | Image_url of string

  let name = "list_style"

  (* Tailwind ranks the list-style properties at 48, by cursor and resize at
     priority 11. *)
  let priority _ = 11
  let err_not_utility = Error (`Msg "Not a list-style utility")

  let of_class _theme class_name =
    let parts = Parse.split_class class_name in
    match parts with
    | [ "list"; value ] when Parse.is_bracket_var value ->
        Ok (Bracket_var (Parse.bracket_inner value))
    | [ "list"; value ] when Parse.is_bracket_value value -> (
        let inner = Parse.bracket_inner value in
        match
          Css.parse_list_style_type (Parse.decode_arbitrary_value inner)
        with
        | Some t -> Ok (Bracket (inner, t))
        | None -> err_not_utility)
    | [ "list"; "none" ] -> Ok None
    | [ "list"; "disc" ] -> Ok Disc
    | [ "list"; "decimal" ] -> Ok Decimal
    | [ "list"; "inside" ] -> Ok Inside
    | [ "list"; "outside" ] -> Ok Outside
    | [ "list"; "image"; "none" ] -> Ok Image_none
    | [ "list"; "image"; value ] when Parse.is_bracket_var value ->
        Ok (Image_bracket_var (Parse.bracket_inner value))
    (* The [(--x)] shorthand names a custom property, the same as
       [list-image-[var(--x)]]. *)
    | [ "list"; "image"; value ] when Parse.is_bare_var value ->
        Ok (Image_bracket_var value)
    | [ "list"; "image"; value ] when Parse.is_bracket_value value -> (
        let inner = Parse.bracket_inner value in
        match
          Css.parse_list_style_image (Parse.decode_arbitrary_value inner)
        with
        | Some i -> Ok (Image_bracket (inner, i))
        | None -> (
            match Parse.arbitrary_declaration_value inner with
            | Some raw -> Ok (Image_raw (inner, raw))
            | None -> err_not_utility))
    | "list" :: "image" :: rest when rest <> [] ->
        let url = String.concat "-" rest in
        if Parse.is_valid_theme_name url then Ok (Image_url url)
        else err_not_utility
    | _ -> err_not_utility

  let to_class = function
    | None -> "list-none"
    | Disc -> "list-disc"
    | Decimal -> "list-decimal"
    | Bracket_var s -> "list-[" ^ s ^ "]"
    | Bracket (raw, _) -> "list-[" ^ raw ^ "]"
    | Inside -> "list-inside"
    | Outside -> "list-outside"
    | Image_none -> "list-image-none"
    | Image_bracket_var s ->
        if Parse.is_bare_var s then "list-image-" ^ s
        else "list-image-[" ^ s ^ "]"
    | Image_bracket (raw, _) | Image_raw (raw, _) -> "list-image-[" ^ raw ^ "]"
    | Image_url url -> "list-image-" ^ url

  let suborder = function
    (* List utilities (priority 11) - after resize (~1M), before appearance
       (~3M). Position, type, and image form separate property bands; candidate
       names settle ties inside each band. *)
    | Inside | Outside -> 2_000_000
    | Bracket_var _ | Bracket _ | Decimal | Disc | None -> 2_010_000
    | Image_bracket_var _ | Image_bracket _ | Image_raw _ | Image_none
    | Image_url _ ->
        2_020_000

  let list_image_url url = style [ list_style_image (Image (Url url)) ]

  let list_none ?theme () =
    let var_name = "list-style-type-none" in
    let ref =
      Var.theme_ref var_name
        ~default:(None : Css.list_style_type)
        ~default_css:"none"
    in
    match Scheme.theme_value theme var_name with
    | Some value ->
        let theme_decl =
          Css.custom_property ~layer:"theme" ("--" ^ var_name) value
        in
        style [ theme_decl; list_style_type (Var ref) ]
    (* Without a theme override Tailwind writes the keyword, not a reference to
       a token nothing declares. *)
    | None -> style [ list_style_type None ]

  let list_bracket_var s =
    let inner = Parse.extract_var_name s in
    let ref : Css.list_style_type Css.var = Var.bracket inner in
    style [ list_style_type (Var ref) ]

  let list_disc = style [ list_style_type Disc ]
  let list_decimal = style [ list_style_type Decimal ]
  let list_inside = style [ list_style_position Inside ]
  let list_outside = style [ list_style_position Outside ]

  let list_image_bracket_var s =
    (* Written either as [[var(--x)]] or as the [(--x)] shorthand. *)
    let inner =
      if Parse.is_bare_var s then
        let bare = Parse.bare_var_inner s in
        Option.value ~default:bare (Parse.bare_name bare)
      else Parse.extract_var_name s
    in
    let ref : Css.list_style_image Css.var = Var.bracket inner in
    style [ list_style_image (Var ref) ]

  let list_image_none ?theme () =
    let var_name = "list-style-image-none" in
    let ref =
      Var.theme_ref var_name
        ~default:(None : Css.list_style_image)
        ~default_css:"none"
    in
    match Scheme.theme_value theme var_name with
    | Some value ->
        let theme_decl =
          Css.custom_property ~layer:"theme" ("--" ^ var_name) value
        in
        style [ theme_decl; list_style_image (Var ref) ]
    | None -> style [ list_style_image None ]

  let to_style theme =
    let list_none () = list_none ~theme () in
    let list_image_none () = list_image_none ~theme () in
    function
    | None -> list_none ()
    | Disc -> list_disc
    | Decimal -> list_decimal
    | Bracket_var s -> list_bracket_var s
    | Bracket (_, t) -> style [ list_style_type t ]
    | Inside -> list_inside
    | Outside -> list_outside
    | Image_none -> list_image_none ()
    | Image_bracket_var s -> list_image_bracket_var s
    | Image_bracket (_, i) -> style [ list_style_image i ]
    | Image_raw (_, v) ->
        style (Option.to_list (Parse.opaque_declaration "list-style-image" v))
    | Image_url url -> list_image_url url

  let examples = [ None; Image_none; Inside ]
end

open Handler
module Utility_factory = Utility.Make (Handler)

let utility = Utility_factory.v
let list_none = utility None
let list_disc = utility Disc
let list_decimal = utility Decimal
let list_inside = utility Inside
let list_outside = utility Outside
let list_image_none = utility Image_none
let list_image_url url = utility (Image_url url)
