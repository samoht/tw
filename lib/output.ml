(** CSS rule t types and smart constructors *)

module Css = Cascade.Css

type t =
  | Regular of {
      selector : Css.Selector.t;
      props : Css.declaration list;
      base_class : string option; (* Base class name without the dot *)
      has_hover : bool; (* Track if this rule has hover modifier *)
      nested : Css.statement list; (* Nested statements (e.g., @media) *)
      merge_key : string option;
    }
  | Media_query of {
      condition : Css.Media.t;
      selector : Css.Selector.t;
      props : Css.declaration list;
      base_class : string option;
      nested : Css.statement list; (* Nested statements for compound modifiers *)
    }
  | Container_query of {
      condition : Css.Container.t;
      selector : Css.Selector.t;
      props : Css.declaration list;
      base_class : string option;
      nested : Css.statement list;
    }
  | Starting_style of {
      selector : Css.Selector.t;
      props : Css.declaration list;
      base_class : string option;
      nested : Css.statement list;
    }
  | Supports_query of {
      condition : Css.Supports.t;
      selector : Css.Selector.t;
      props : Css.declaration list;
      base_class : string option;
      merge_key : string option;
      nested : Css.statement list;
    }

type by_type = {
  regular : t list;
  media : t list;
  container : t list;
  starting : t list;
  supports : t list;
}

let regular ~selector ~props ?base_class ?(has_hover = false) ?(nested = [])
    ?merge_key () =
  Regular { selector; props; base_class; has_hover; nested; merge_key }

let media_query ~condition ~selector ~props ?base_class ?(nested = []) () =
  Media_query { condition; selector; props; base_class; nested }

let container_query ~condition ~selector ~props ?base_class ?(nested = []) () =
  Container_query { condition; selector; props; base_class; nested }

let starting_style ~selector ~props ?base_class ?(nested = []) () =
  Starting_style { selector; props; base_class; nested }

let supports_query ~condition ~selector ~props ?base_class ?merge_key
    ?(nested = []) () =
  Supports_query { condition; selector; props; base_class; merge_key; nested }

let base_class = function
  | Regular { base_class; _ }
  | Media_query { base_class; _ }
  | Container_query { base_class; _ }
  | Starting_style { base_class; _ }
  | Supports_query { base_class; _ } ->
      base_class

let pp = function
  | Regular { selector; _ } ->
      "Regular(" ^ Css.Selector.to_string selector ^ ")"
  | Media_query { selector; _ } ->
      "Media_query(" ^ Css.Selector.to_string selector ^ ")"
  | Container_query { selector; _ } ->
      "Container_query(" ^ Css.Selector.to_string selector ^ ")"
  | Starting_style { selector; _ } ->
      "Starting_style(" ^ Css.Selector.to_string selector ^ ")"
  | Supports_query { selector; _ } ->
      "Supports_query(" ^ Css.Selector.to_string selector ^ ")"

let is_hover_rule = function
  | Regular { has_hover; _ } -> has_hover
  | _ -> false

let classify_by_type all_rules =
  let ( regular_rules,
        media_rules,
        container_rules,
        starting_rules,
        supports_rules ) =
    List.fold_left
      (fun (reg, media, cont, start, sup) rule ->
        match rule with
        | Regular _ -> (rule :: reg, media, cont, start, sup)
        | Media_query _ -> (reg, rule :: media, cont, start, sup)
        | Container_query _ -> (reg, media, rule :: cont, start, sup)
        | Starting_style _ -> (reg, media, cont, rule :: start, sup)
        | Supports_query _ -> (reg, media, cont, start, rule :: sup))
      ([], [], [], [], []) all_rules
  in
  {
    regular = List.rev regular_rules;
    media = List.rev media_rules;
    container = List.rev container_rules;
    starting = List.rev starting_rules;
    supports = List.rev supports_rules;
  }
