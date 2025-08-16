(* Variable tracking for CSS composition groups *)

module S = Set.Make (String)

type feature_group =
  | Transform
  | Filter
  | Backdrop
  | RingShadow
  | Gradient
  | Other (* catch-all; usually no properties layer *)

(* Map each --tw- variable to its feature group *)
let group_of_var (v : string) : feature_group =
  match v with
  | "--tw-translate-x" | "--tw-translate-y" | "--tw-translate-z" | "--tw-rotate"
  | "--tw-skew-x" | "--tw-skew-y" | "--tw-scale-x" | "--tw-scale-y"
  | "--tw-scale-z" ->
      Transform
  | "--tw-blur" | "--tw-brightness" | "--tw-contrast" | "--tw-grayscale"
  | "--tw-hue-rotate" | "--tw-invert" | "--tw-saturate" | "--tw-sepia"
  | "--tw-drop-shadow" | "--tw-drop-shadow-alpha" ->
      Filter
  | "--tw-backdrop-blur" | "--tw-backdrop-brightness" | "--tw-backdrop-contrast"
  | "--tw-backdrop-grayscale" | "--tw-backdrop-hue-rotate"
  | "--tw-backdrop-invert" | "--tw-backdrop-saturate" | "--tw-backdrop-sepia"
  | "--tw-backdrop-opacity" ->
      Backdrop
  | "--tw-ring-offset-shadow" | "--tw-ring-shadow" | "--tw-shadow"
  | "--tw-inset-shadow" | "--tw-inset-ring-shadow" | "--tw-shadow-color"
  | "--tw-inset-shadow-color" | "--tw-ring-color" | "--tw-inset-ring-color"
  | "--tw-ring-inset" | "--tw-shadow-alpha" | "--tw-inset-shadow-alpha"
  | "--tw-ring-offset-width" | "--tw-ring-offset-color" ->
      RingShadow
  | "--tw-gradient-from" | "--tw-gradient-via" | "--tw-gradient-to"
  | "--tw-gradient-stops" | "--tw-gradient-from-position"
  | "--tw-gradient-via-position" | "--tw-gradient-to-position" ->
      Gradient
  | "--tw-font-weight" | "--tw-leading" ->
      Other (* These get special handling *)
  | _ -> Other

(* Default initialisers for each group when a layer is needed *)
let defaults_for_group = function
  | Transform ->
      [
        ("--tw-translate-x", "0");
        ("--tw-translate-y", "0");
        ("--tw-translate-z", "0");
        ("--tw-rotate", "0");
        ("--tw-skew-x", "0");
        ("--tw-skew-y", "0");
        ("--tw-scale-x", "1");
        ("--tw-scale-y", "1");
        ("--tw-scale-z", "1");
      ]
  | Filter ->
      [
        ("--tw-blur", "");
        ("--tw-brightness", "");
        ("--tw-contrast", "");
        ("--tw-grayscale", "");
        ("--tw-hue-rotate", "");
        ("--tw-invert", "");
        ("--tw-saturate", "");
        ("--tw-sepia", "");
        ("--tw-drop-shadow", "");
      ]
  | Backdrop ->
      [
        ("--tw-backdrop-blur", "");
        ("--tw-backdrop-brightness", "");
        ("--tw-backdrop-contrast", "");
        ("--tw-backdrop-grayscale", "");
        ("--tw-backdrop-hue-rotate", "");
        ("--tw-backdrop-invert", "");
        ("--tw-backdrop-saturate", "");
        ("--tw-backdrop-sepia", "");
        ("--tw-backdrop-opacity", "");
      ]
  | RingShadow ->
      [
        ("--tw-shadow", "0 0 #0000");
        ("--tw-inset-shadow", "0 0 #0000");
        ("--tw-ring-shadow", "0 0 #0000");
        ("--tw-inset-ring-shadow", "0 0 #0000");
        ("--tw-ring-offset-shadow", "0 0 #0000");
        ("--tw-shadow-color", "initial");
        ("--tw-inset-shadow-color", "initial");
        ("--tw-ring-color", "initial");
        ("--tw-inset-ring-color", "initial");
        ("--tw-ring-inset", "initial");
        ("--tw-shadow-alpha", "100%");
        ("--tw-inset-shadow-alpha", "100%");
        ("--tw-ring-offset-width", "0px");
        ("--tw-ring-offset-color", "#fff");
      ]
  | Gradient ->
      [
        ("--tw-gradient-from", "transparent");
        ("--tw-gradient-to", "transparent");
        ("--tw-gradient-stops", "var(--tw-gradient-from), var(--tw-gradient-to)");
      ]
  | Other -> []

(* Collect usages while compiling utilities *)
type tally = {
  assigned : S.t; (* variables written by any used utility *)
  fallback_refs : S.t; (* variables only ever seen in fallbacks *)
}

let empty = { assigned = S.empty; fallback_refs = S.empty }
let record_assignment v t = { t with assigned = S.add v t.assigned }

let record_fallback v t =
  if S.mem v t.assigned then t
  else { t with fallback_refs = S.add v t.fallback_refs }

(* Extract assignments and references from CSS properties *)
let analyze_properties props =
  let tally = ref empty in

  (* Check for assignments (custom properties being set) *)
  List.iter
    (fun prop ->
      if Css.is_custom_property prop then
        let prop_name = Css.property_name prop in
        let name = Css.property_name_to_string prop_name in
        if String.starts_with ~prefix:"--tw-" name then
          tally := record_assignment name !tally)
    props;

  (* Check for references in property values *)
  let rec extract_refs value pos =
    if pos >= String.length value then ()
    else
      try
        let var_pos = String.index_from value pos 'v' in
        if
          var_pos + 4 <= String.length value
          && String.sub value var_pos 4 = "var("
        then (
          let var_start = var_pos + 4 in
          match String.index_from value var_start ')' with
          | exception Not_found -> ()
          | end_paren ->
              let var_content =
                String.sub value var_start (end_paren - var_start)
              in
              let var_name =
                match String.index var_content ',' with
                | exception Not_found -> String.trim var_content
                | comma -> String.trim (String.sub var_content 0 comma)
              in
              if String.length var_name > 5 && String.sub var_name 0 5 = "--tw-"
              then tally := record_fallback var_name !tally;
              extract_refs value (end_paren + 1))
        else extract_refs value (var_pos + 1)
      with Not_found -> ()
  in

  List.iter
    (fun prop ->
      let value = Css.property_value prop in
      extract_refs value 0)
    props;

  !tally

(* Decide which groups actually need a properties layer *)
let groups_needing_layer (t : tally) : feature_group list =
  (* Group the assigned vars, ignore fallback-only vars *)
  let groups =
    S.fold
      (fun v acc -> match group_of_var v with Other -> acc | g -> g :: acc)
      t.assigned []
  in
  groups |> List.sort_uniq compare

(* Generate properties layer initializers for needed groups *)
let generate_properties_layer (t : tally) : (string * string) list =
  (* Get defaults for composition groups *)
  let group_defaults =
    groups_needing_layer t |> List.concat_map defaults_for_group
  in

  (* Also add any special --tw- variables that need initialization *)
  let special_vars =
    S.fold
      (fun v acc ->
        match v with
        | "--tw-font-weight" -> ("--tw-font-weight", "initial") :: acc
        | "--tw-leading" -> ("--tw-leading", "initial") :: acc
        | _ -> acc)
      t.assigned []
  in

  group_defaults @ special_vars
  |> List.sort_uniq (fun (k1, _) (k2, _) -> String.compare k1 k2)
