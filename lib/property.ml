let split statements =
  List.partition
    (fun stmt ->
      match Css.as_property stmt with Some _ -> true | None -> false)
    statements

let dedup property_rules =
  let seen = Hashtbl.create 16 in
  List.filter
    (fun stmt ->
      match Css.as_property stmt with
      | Some (Css.Property_info { name; _ }) ->
          if Hashtbl.mem seen name then false
          else (
            Hashtbl.add seen name ();
            true)
      | None -> true)
    property_rules

let initial_values property_rules =
  List.fold_left
    (fun acc stmt ->
      match Css.as_property stmt with
      | Some (Css.Property_info info as prop_info) ->
          let value = Var.property_initial_string prop_info in
          (info.name, value) :: acc
      | None -> acc)
    [] property_rules
  |> List.rev

let sort_by_order order_of pairs =
  List.sort (fun (n1, _) (n2, _) -> compare (order_of n1) (order_of n2)) pairs
