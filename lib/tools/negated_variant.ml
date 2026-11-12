module Css = Cascade.Css

(* [not-] over a declared variant.

   Tailwind's [not-] negates what a variant's body builds, not the variant's
   name, so a project that declares [dark] itself gets the negation of its own
   body. Tailwind rewrites one node in place: sibling branches would have to be
   nested into a single conjunction, which it does not build, so a body of more
   than one refuses the candidate. Under that node the first leaf decides: the
   style rule on the way to it goes under [:not()] with [&] read as [*], and the
   condition takes a [not]. Tailwind also refuses a path through two rules or
   two conditions, a pseudo-element, which [:not()] cannot hold, and an at-rule
   other than [@media], [@supports] and [@container]. A compound [and]/[or]
   condition is refused here as well: Tailwind puts a [not] in front of it that
   makes the query invalid, so its rule never applies either. *)

let body_rules body =
  let items =
    (Cascade.Parser.block_contents (Cascade.Reader.of_string body)).value
  in
  List.fold_right
    (fun item (decls, rules) ->
      match item with
      | `Decls [] -> (decls, rules)
      | `Decls _ -> (true, rules)
      | `Rule (Cascade.Component.At { node = { name = "slot"; _ }; _ }) ->
          (decls, rules)
      | `Rule rule -> (decls, rule :: rules))
    items (false, [])

let rule_body = function
  | Cascade.Component.Qualified { node = { block; _ }; _ } ->
      Some (Cascade.Parser.string_of_components block.node.value)
  | At { node = { block; _ }; _ } ->
      Option.map
        (fun (b : Cascade.Component.block Cascade.Component.node) ->
          Cascade.Parser.string_of_components b.node.value)
        block

(* The path from [rule] down to the first node holding nothing but the slot. *)
let rec first_leaf path rule =
  let path = rule :: path in
  match rule_body rule with
  | None -> Some (List.rev path)
  | Some body -> (
      match body_rules body with
      | false, [] -> Some (List.rev path)
      | _, rules -> List.find_map (first_leaf path) rules)

let negated_selector sel =
  if Css.Selector.has_pseudo_element sel then None
  else
    let arms = Option.value ~default:[ sel ] (Css.Selector.as_list sel) in
    (* A universal beside another simple selector matches nothing more, and the
       CLI's minifier drops it there. *)
    let implied = function Css.Selector.Universal None -> false | _ -> true in
    let universal =
      Css.Selector.map (function
        | Css.Selector.Nesting -> Css.Selector.Universal None
        | Css.Selector.Compound parts -> (
            match List.filter implied parts with
            | [] -> Css.Selector.Universal None
            | [ part ] -> part
            | parts -> Css.Selector.Compound parts)
        | node -> node)
    in
    Some
      (Css.Selector.Compound
         [ Css.Selector.Nesting; Css.Selector.Not (List.map universal arms) ])

let rec negated_media : Css.Media.t -> Css.Media.t option = function
  | Cond (Not condition) -> Some (Cond condition)
  | Cond (Feature _ as condition) -> Some (Cond (Not condition))
  | Cond (And _ | Or _) | Type { prefix = Some Only; _ } -> None
  | Type ({ prefix = Some Not; _ } as media) ->
      Some (Type { media with prefix = None })
  | Type ({ prefix = None; _ } as media) ->
      Some (Type { media with prefix = Some Not })
  | List [ media ] -> negated_media media
  | List _ -> None

let negated_supports : Css.Supports.t -> Css.Supports.t option = function
  | Not condition -> Some condition
  | And _ | Or _ -> None
  | condition -> Some (Not condition)

let rec negated_container : Css.Container.t -> Css.Container.t option = function
  | Named (name, condition) ->
      Option.map
        (fun condition -> Css.Container.Named (name, condition))
        (negated_container condition)
  | Not condition -> Some condition
  | And _ | Or _ -> None
  | condition -> Some (Not condition)

(* The negated condition as the header of a template block. *)
let negated_at_rule name prelude =
  let prelude = String.trim prelude in
  let header keyword to_string negate of_string =
    Option.map
      (fun condition -> String.concat "" [ keyword; " "; to_string condition ])
      (negate (of_string prelude))
  in
  match name with
  | "media" ->
      header "@media"
        (Css.Media.to_string ~minify:false)
        negated_media Css.Media.of_string_strict
  | "supports" ->
      header "@supports"
        (Css.Supports.to_string ~minify:false)
        negated_supports Css.Supports.of_string
  | "container" ->
      header "@container"
        (Css.Container.to_string ~minify:false)
        negated_container Css.Container.of_string
  | _ -> None

let all_some options =
  List.fold_right
    (fun option acc ->
      match (option, acc) with Some x, Some xs -> Some (x :: xs) | _ -> None)
    options (Some [])

let negated_path path =
  let selectors, conditions =
    List.partition_map
      (function
        | Cascade.Component.Qualified { node = { prelude; _ }; _ } ->
            Left (Cascade.Parser.string_of_components prelude)
        | At { node = { name; prelude; _ }; _ } ->
            Right (name, Cascade.Parser.string_of_components prelude))
      path
  in
  match (selectors, conditions) with
  | _ :: _ :: _, _ | _, _ :: _ :: _ -> None
  | selectors, conditions -> (
      let selectors =
        all_some
          (List.map
             (fun s -> negated_selector (Css.Selector.of_string s))
             selectors)
      in
      let conditions =
        all_some (List.map (fun (name, p) -> negated_at_rule name p) conditions)
      in
      match (selectors, conditions) with
      | Some selectors, Some headers ->
          let slot header = header ^ " { @slot; }" in
          Some
            (String.concat " "
               (List.map
                  (fun s -> slot (Css.Selector.to_string ~minify:false s))
                  selectors
               @ List.map slot headers))
      | _ -> None)

let negated_body ~defs template =
  match body_rules (Custom_variant.expand defs template) with
  | false, [ node ] -> (
      match Option.bind (first_leaf [] node) negated_path with
      | negation -> negation
      | exception (Cascade.Cursor.Parse_error _ | Invalid_argument _ | Failure _)
        ->
          None)
  | _ -> None

let add defs =
  List.fold_left
    (fun (defs, refused) (name, template) ->
      let negation = "not-" ^ name in
      if List.mem_assoc negation defs then (defs, refused)
      else
        match negated_body ~defs template with
        | Some body -> (defs @ [ (negation, body) ], refused)
        | None -> (defs, refused @ [ negation ]))
    (defs, []) defs
