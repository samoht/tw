module Css = Cascade.Css

module Handler = struct
  open Style
  open Css

  type t =
    | None
    | Strict
    | Content
    | Size
    | Layout
    | Paint
    | Contain_style
    | Inline_size
    | Arbitrary of string * string
  (* the bracket as written, and the value after its hint *)

  let name = "contain"
  let priority _ = 34

  (* Single source of truth: (handler, class_suffix, suborder) for
     non-arbitrary. Composable utilities (inline-size, layout, paint, size,
     style) come first alphabetically, then arbitrary, then simple ones
     (content, none, strict) alphabetically. *)
  let contain_data =
    [
      (Inline_size, "inline-size", 0);
      (Layout, "layout", 1);
      (Paint, "paint", 2);
      (Size, "size", 3);
      (Contain_style, "style", 4);
      (Content, "content", 6);
      (None, "none", 7);
      (Strict, "strict", 8);
    ]

  let suborder = function
    | Arbitrary _ -> 5
    | t -> (
        match
          List.assoc_opt t (List.map (fun (t, _, o) -> (t, o)) contain_data)
        with
        | Some o -> o
        | None -> 100)

  let to_class = function
    | Arbitrary (s, _) -> "contain-[" ^ s ^ "]"
    | t -> (
        match
          List.assoc_opt t (List.map (fun (t, s, _) -> (t, s)) contain_data)
        with
        | Some suffix -> "contain-" ^ suffix
        | None -> "contain-none")

  (* Channel variables for composable contain. They generate @property rules and
     initial-value declarations in the properties layer. property_order groups
     them together; they use syntax: "*" with no initial-value. *)
  let tw_contain_size_var =
    Var.channel ~needs_property:true ~property_order:50 Css.Value
      "tw-contain-size"

  let tw_contain_layout_var =
    Var.channel ~needs_property:true ~property_order:50 Css.Value
      "tw-contain-layout"

  let tw_contain_paint_var =
    Var.channel ~needs_property:true ~property_order:50 Css.Value
      "tw-contain-paint"

  let tw_contain_style_var =
    Var.channel ~needs_property:true ~property_order:50 Css.Value
      "tw-contain-style"

  let all_contain_vars =
    [
      tw_contain_size_var;
      tw_contain_layout_var;
      tw_contain_paint_var;
      tw_contain_style_var;
    ]

  let contain_property_rules =
    List.filter_map Var.property_rule all_contain_vars |> concat

  (* The composable contain value using all four variables with empty fallbacks.
     This allows combining multiple contain utilities - each sets its variable,
     and the contain property references all of them. *)
  let composable_contain_value =
    let v var : Css.contain =
      Css.Var (Var.bracket ~fallback:Css.Empty (Var.name var))
    in
    Css.List
      [
        v tw_contain_size_var;
        v tw_contain_layout_var;
        v tw_contain_paint_var;
        v tw_contain_style_var;
      ]

  let composable_contain var set_value =
    let decl = Var.set var (Css.Variables.custom_value_ident set_value) in
    style ~property_rules:contain_property_rules
      [ decl; contain composable_contain_value ]

  let to_style _theme = function
    | None -> style [ contain Css.None ]
    | Strict -> style [ contain Css.Strict ]
    | Content -> style [ contain Css.Content ]
    | Size -> composable_contain tw_contain_size_var "size"
    | Inline_size -> composable_contain tw_contain_size_var "inline-size"
    | Layout -> composable_contain tw_contain_layout_var "layout"
    | Paint -> composable_contain tw_contain_paint_var "paint"
    | Contain_style -> composable_contain tw_contain_style_var "style"
    | Arbitrary (_, value) -> (
        match String.lowercase_ascii value with
        | "none" -> style [ contain Css.None ]
        | "strict" -> style [ contain Css.Strict ]
        | "content" -> style [ contain Css.Content ]
        | "size" -> style [ contain Css.Size ]
        | "layout" -> style [ contain Css.Layout ]
        | "style" -> style [ contain Css.Style ]
        | "paint" -> style [ contain Css.Paint ]
        | "inline-size" -> style [ contain Css.Inline_size ]
        | "inherit" -> style [ contain Css.Inherit ]
        | "initial" -> style [ contain Css.Initial ]
        | "unset" -> style [ contain Css.Unset ]
        | "revert" -> style [ contain Css.Revert ]
        | "revert-layer" -> style [ contain Css.Revert_layer ]
        | _ -> style (Option.to_list (Parse.opaque_declaration "contain" value))
        )

  let of_class_map =
    List.map (fun (t, s, _) -> ("contain-" ^ s, t)) contain_data

  let of_class _theme class_name =
    match List.assoc_opt class_name of_class_map with
    | Some t -> Ok t
    | None -> (
        match Parse.split_class class_name with
        | [ "contain"; v ] when Parse.is_bracket_value v -> (
            let inner = Parse.bracket_inner v in
            match Parse.arbitrary_declaration_value inner with
            | Some value -> Ok (Arbitrary (inner, value))
            | None -> Utility.unclaimed)
        | _ -> Utility.unclaimed)

  let examples = [ Strict ]
end

module Utility_factory = Utility.Make (Handler)

let utility = Utility_factory.v
let contain_none = utility Handler.None
let contain_strict = utility Handler.Strict
let contain_content = utility Handler.Content
let contain_size = utility Handler.Size
let contain_layout = utility Handler.Layout
let contain_paint = utility Handler.Paint
let contain_style = utility Handler.Contain_style
let contain_inline_size = utility Handler.Inline_size

let contain_arbitrary value =
  utility (Handler.Arbitrary (Parse.encode_underscores value, value))
