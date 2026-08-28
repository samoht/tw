module Css = Cascade.Css

let split statements =
  List.partition
    (fun stmt ->
      match Css.as_property stmt with Some _ -> true | None -> false)
    statements

(* Keeps the FIRST rule for a name. CSS Properties and Values API 1 says a later
   [@property] replaces an earlier one, so this looks backwards, and it is
   deliberate: tw's contract is Tailwind parity, and Tailwind resolves the
   collision the same way. Measured 2026-08-28 with an entrypoint declaring
   [@property --tw-shadow-color] with its own syntax and initial-value after
   [@import "tailwindcss"]: both tw and the real CLI emit one rule for that
   name, the built-in, and neither carries the author's initial-value.

   So there is no reproducer here today. Turn this into last-wins only when one
   exists, because doing it on the spec alone would move tw off Tailwind. *)
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
