(** CSS rule t types and smart constructors *)

type t =
  | Regular of {
      selector : Css.Selector.t;
      props : Css.declaration list;
      base_class : string option; (* Base class name without the dot *)
      has_hover : bool; (* Track if this rule has hover modifier *)
      nested : Css.statement list; (* Nested statements (e.g., @media) *)
      merge_key : string option;
      not_order : int; (* Variant order for not-* rules, 0 for normal rules *)
    }
  | Media_query of {
      condition : Css.Media.t;
      selector : Css.Selector.t;
      props : Css.declaration list;
      base_class : string option;
      nested : Css.statement list;
          (* Nested statements for compound modifiers *)
      not_order : int;
    }
  | Container_query of {
      condition : Css.Container.t;
      selector : Css.Selector.t;
      props : Css.declaration list;
      base_class : string option;
    }
  | Starting_style of {
      selector : Css.Selector.t;
      props : Css.declaration list;
      base_class : string option;
    }
  | Supports_query of {
      condition : Css.Supports.t;
      selector : Css.Selector.t;
      props : Css.declaration list;
      base_class : string option;
      merge_key : string option;
      not_order : int;
    }

type by_type = {
  regular : t list;
  media : t list;
  container : t list;
  starting : t list;
  supports : t list;
}

let regular ~selector ~props ?base_class ?(has_hover = false) ?(nested = [])
    ?merge_key ?(not_order = 0) () =
  Regular
    { selector; props; base_class; has_hover; nested; merge_key; not_order }

let media_query ~condition ~selector ~props ?base_class ?(nested = [])
    ?(not_order = 0) () =
  Media_query { condition; selector; props; base_class; nested; not_order }

let container_query ~condition ~selector ~props ?base_class () =
  Container_query { condition; selector; props; base_class }

let starting_style ~selector ~props ?base_class () =
  Starting_style { selector; props; base_class }

let supports_query ~condition ~selector ~props ?base_class ?merge_key
    ?(not_order = 0) () =
  Supports_query
    { condition; selector; props; base_class; merge_key; not_order }

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
