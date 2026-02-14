(** Contain utilities for CSS containment. *)

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
    | Arbitrary of string

  type Utility.base += Self of t

  let name = "contain"
  let priority = 35

  (* Single source of truth: (handler, class_suffix, suborder) for
     non-arbitrary *)
  let contain_data =
    [
      (None, "none", 0);
      (Strict, "strict", 1);
      (Content, "content", 2);
      (Size, "size", 3);
      (Inline_size, "inline-size", 4);
      (Layout, "layout", 5);
      (Paint, "paint", 6);
      (Contain_style, "style", 7);
    ]

  let suborder = function
    | Arbitrary _ -> 100
    | t -> (
        match
          List.assoc_opt t (List.map (fun (t, _, o) -> (t, o)) contain_data)
        with
        | Some o -> o
        | None -> 100)

  let to_class = function
    | Arbitrary s -> "contain-[" ^ s ^ "]"
    | t -> (
        match
          List.assoc_opt t (List.map (fun (t, s, _) -> (t, s)) contain_data)
        with
        | Some suffix -> "contain-" ^ suffix
        | None -> "contain-none")

  (* The composable contain value using all four variables with empty fallbacks.
     This allows combining multiple contain utilities - each sets its variable,
     and the contain property references all of them. *)
  let composable_contain_value =
    Css.List
      [
        Css.Var "--tw-contain-size,  ";
        Css.Var "--tw-contain-layout,  ";
        Css.Var "--tw-contain-paint,  ";
        Css.Var "--tw-contain-style,  ";
      ]

  (* Create a composable contain style that sets one variable *)
  let composable_contain var_name set_value =
    style
      [
        custom_property ~layer:"utilities" ("--" ^ var_name) set_value;
        contain composable_contain_value;
      ]

  let to_style = function
    | None -> style [ contain Css.None ]
    | Strict -> style [ contain Css.Strict ]
    | Content -> style [ contain Css.Content ]
    | Size -> composable_contain "tw-contain-size" "size"
    | Inline_size -> composable_contain "tw-contain-size" "inline-size"
    | Layout -> composable_contain "tw-contain-layout" "layout"
    | Paint -> composable_contain "tw-contain-paint" "paint"
    | Contain_style -> composable_contain "tw-contain-style" "style"
    | Arbitrary s -> (
        (* Parse the arbitrary value *)
        match String.lowercase_ascii s with
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
        | _ -> style [ contain (Css.Var s) ])

  let of_class_map =
    List.map (fun (t, s, _) -> ("contain-" ^ s, t)) contain_data

  let of_class class_name =
    match List.assoc_opt class_name of_class_map with
    | Some t -> Ok t
    | None ->
        (* Check for arbitrary value: contain-[value] *)
        if String.starts_with ~prefix:"contain-[" class_name then
          let len = String.length class_name in
          if class_name.[len - 1] = ']' then
            Ok (Arbitrary (String.sub class_name 9 (len - 10)))
          else Error (`Msg "Invalid contain arbitrary value")
        else Error (`Msg "Not a contain utility")

  let utility t = Utility.base (Self t)
end

let () = Utility.register (module Handler)

(* Convenience functions *)
let contain_none = Handler.utility None
let contain_strict = Handler.utility Strict
let contain_content = Handler.utility Content
let contain_size = Handler.utility Size
let contain_layout = Handler.utility Layout
let contain_paint = Handler.utility Paint
let contain_style = Handler.utility Contain_style
let contain_inline_size = Handler.utility Inline_size
let contain_arbitrary s = Handler.utility (Arbitrary s)
