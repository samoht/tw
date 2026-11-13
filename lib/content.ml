(** Content utilities, for the [::before] and [::after] pseudo-elements. *)

module Css = Cascade.Css

module Handler = struct
  open Style
  open Css

  type t =
    | Content_none
    | Content of string
    | Content_squote of string (* single-quoted arbitrary: content-['x'] *)
    | Content_raw of string * [ `Content of Css.content | `Raw of string ]
      (* unquoted arbitrary: content-[attr(before)]; raw class text + value *)
    | Content_named of string (* content-<token> defined in the @theme *)

  let name = "content"
  let priority _ = 35
  let err_not_utility = Error (`Msg "Not a content utility")

  let of_class theme class_name =
    let parts = Parse.split_class class_name in
    match parts with
    | [ "content"; "none" ] -> Ok Content_none
    | "content" :: rest ->
        let joined = String.concat "-" rest in
        (* Parse arbitrary value syntax: content-["value"] -> value *)
        let bracket_quoted q =
          String.length joined >= 4
          && String.get joined 0 = '['
          && String.get joined 1 = q
          && String.get joined (String.length joined - 1) = ']'
          && String.get joined (String.length joined - 2) = q
        in
        (* v4 has content-none, arbitrary content-[...], and content-<token>
           when the @theme defines --content-<token>. A bare word like
           content-wrapper with no such token is not a utility (it used to be
           wrongly accepted). An unquoted bracket (content-[attr(before)])
           carries a function value, parsed as a content list. *)
        if bracket_quoted '"' then
          let value = String.sub joined 2 (String.length joined - 4) in
          Ok (Content value)
        else if bracket_quoted '\'' then
          let value = String.sub joined 2 (String.length joined - 4) in
          Ok (Content_squote value)
        else if Scheme.theme_value (Some theme) ("content-" ^ joined) <> None
        then Ok (Content_named joined)
        else if
          String.length joined >= 2
          && String.get joined 0 = '['
          && String.get joined (String.length joined - 1) = ']'
        then begin
          let inner = String.sub joined 1 (String.length joined - 2) in
          let spaced = Parse.decode_underscores inner in
          match
            Cascade.Cursor.try_parse_full_err Css.Properties.read_content
              (Cascade.Cursor.of_string spaced)
          with
          | Ok value -> Ok (Content_raw (inner, `Content value))
          | Error _ -> (
              match Parse.arbitrary_declaration_value inner with
              | Some value -> Ok (Content_raw (inner, `Raw value))
              | None -> err_not_utility)
        end
        else err_not_utility
    | _ -> err_not_utility

  let to_class = function
    | Content_none -> "content-none"
    | Content s -> "content-[\"" ^ s ^ "\"]"
    | Content_squote s -> "content-['" ^ s ^ "']"
    | Content_raw (s, _) -> "content-[" ^ s ^ "]"
    | Content_named s -> "content-" ^ s

  let suborder = function
    (* Content *)
    | Content_none -> 13000
    | Content _ -> 10001
    | Content_squote _ -> 10001
    | Content_raw _ -> 10001
    | Content_named _ -> 10002

  (* content-none sets --tw-content: none and uses a literal [content: none], so
     it never references var(--tw-content) and Tailwind emits no @property rule
     for it (unlike the value/theme variants below, which do reference the var).
     The set-based auto-collection is suppressed for --tw-content in
     build.ml. *)
  let content_none =
    let content_decl, _content_ref = Var.binding Typography.content_var None in
    style [ content_decl; content None ]

  let content s =
    let value = Parse.decode_underscores s in
    let content_decl, content_ref =
      Var.binding Typography.content_var (String value)
    in
    let property_rules = Var.property_rules Typography.content_var in
    style ~property_rules [ content_decl; content (Css.Var content_ref) ]

  (* content-<token> referencing a --content-<token> theme value (of_class only
     builds this when the token exists in the theme). *)
  let content_named ?theme name =
    let var_name = "content-" ^ name in
    let content_decl, content_ref =
      match Scheme.theme_value theme var_name with
      | Some _ ->
          let tv = Var.theme Css.Content var_name ~order:(6, 60) in
          let theme_decl, theme_ref = Var.binding tv (String "") in
          let cd, cr = Var.binding Typography.content_var (Var theme_ref) in
          ([ theme_decl; cd ], cr)
      | None ->
          let theme_ref : Css.content Css.var =
            Var.theme_ref var_name
              ~default:(String "" : Css.content)
              ~default_css:"\"\""
          in
          let cd, cr = Var.binding Typography.content_var (Var theme_ref) in
          ([ cd ], cr)
    in
    let property_rules = Var.property_rules Typography.content_var in
    let c : Css.content = Css.Var content_ref in
    style ~property_rules (content_decl @ [ Css.content c ])

  let content_squote s =
    (* Single-quoted arbitrary content (content-['x']) keeps the single quote.
       [_] is a space and [\_] a literal underscore. *)
    let value = Parse.decode_underscores s in
    let quoted : Css.content =
      Css.Quoted { value; quote = '\''; repr = None }
    in
    let content_decl, content_ref = Var.binding Typography.content_var quoted in
    let property_rules = Var.property_rules Typography.content_var in
    let c : Css.content = Css.Var content_ref in
    style ~property_rules [ content_decl; Css.content c ]

  (* Unquoted arbitrary content (content-[attr(before)]) binds the parsed value
     verbatim. *)
  let content_raw (value : Css.content) =
    let content_decl, content_ref = Var.binding Typography.content_var value in
    let property_rules = Var.property_rules Typography.content_var in
    style ~property_rules [ content_decl; Css.content (Css.Var content_ref) ]

  let content_raw_value value =
    let content_decl =
      Css.custom_property ~layer:"utilities"
        (Var.css_name Typography.content_var)
        value
    in
    let content_ref = Var.reference Typography.content_var in
    let property_rules = Var.property_rules Typography.content_var in
    style
      ~metadata:[ Var.metadata Typography.content_var ]
      ~property_rules
      [ content_decl; Css.content (Css.Var content_ref) ]

  let to_style theme =
    let content_named name = content_named ~theme name in
    function
    | Content_none -> content_none
    | Content s -> content s
    | Content_raw (_, `Content c) -> content_raw c
    | Content_raw (_, `Raw value) -> content_raw_value value
    | Content_squote s -> content_squote s
    | Content_named name -> content_named name

  let examples = [ Content_none ]
end

open Handler
module Utility_factory = Utility.Make (Handler)

let utility = Utility_factory.v
let content_none = utility Content_none
let content s = utility (Content s)
