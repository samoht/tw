module Css = Cascade.Css

let split statements =
  List.partition
    (fun stmt ->
      match Css.as_property stmt with Some _ -> true | None -> false)
    statements

(* Keeps the FIRST rule for a name. CSS Properties and Values API 1 says a later
   [@property] replaces an earlier one, so this looks backwards, and it is
   deliberate: tw's contract is Tailwind parity, and Tailwind resolves the
   collision the same way: an entrypoint that declares [@property
   --tw-shadow-color] after [@import "tailwindcss"] gets the built-in rule,
   without the author's initial-value. *)
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
          let decl = Var.property_initial_declaration prop_info in
          (info.name, decl) :: acc
      | None -> acc)
    [] property_rules
  |> List.rev
