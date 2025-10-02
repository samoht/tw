(** Test helper functions for CSS comparison and minimization *)

(** Extract utilities layer from CSS *)
let extract_utilities_layer_rules css =
  let stmts = Css.statements css in
  List.find_map
    (fun stmt ->
      match Css.as_layer stmt with
      | Some (Some "utilities", rules) -> Some rules
      | _ -> None)
    stmts
  |> Option.value ~default:[]

(** Extract selectors from rules *)
let extract_rule_selectors stmts =
  List.filter_map
    (fun stmt ->
      match Css.as_rule stmt with
      | Some (selector, _, _) -> Some (Css.Selector.to_string selector)
      | None -> None)
    stmts

(** Check if utilities produce different ordering than Tailwind *)
let check_ordering_fails utilities =
  let classnames = List.map Tw.pp utilities in
  let tw_css = Tw.to_css ~base:true ~optimize:true utilities in
  let tw_utilities_rules = extract_utilities_layer_rules tw_css in
  let tw_order = extract_rule_selectors tw_utilities_rules in

  let tailwind_css_str =
    Tw_tools.Tailwind_gen.generate ~minify:true ~optimize:true classnames
  in
  let tailwind_css =
    match Css.of_string tailwind_css_str with
    | Ok css -> css
    | Error _ -> failwith "Failed to parse Tailwind CSS"
  in
  let tailwind_utilities_rules = extract_utilities_layer_rules tailwind_css in
  let tailwind_order = extract_rule_selectors tailwind_utilities_rules in

  tw_order <> tailwind_order

(** Delta Debugging (ddmin algorithm by Zeller) Minimizes a failing test case by
    binary search *)
let delta_debug check_fails lst =
  let rec ddmin lst n =
    let len = List.length lst in
    if len = 1 then lst
    else
      (* Split into n subsets *)
      let subset_size = max 1 (len / n) in
      let rec make_subsets acc remaining =
        if remaining = [] then List.rev acc
        else
          let rec take k l =
            match (k, l) with
            | 0, _ | _, [] -> ([], l)
            | k, x :: xs ->
                let taken, rest = take (k - 1) xs in
                (x :: taken, rest)
          in
          let subset, rest = take subset_size remaining in
          make_subsets (subset :: acc) rest
      in
      let subsets = make_subsets [] lst in

      (* Test 1: Try each subset alone (reduce to subset) *)
      let rec try_subsets = function
        | [] -> None
        | subset :: rest ->
            if check_fails subset then Some subset else try_subsets rest
      in

      match try_subsets subsets with
      | Some subset ->
          Fmt.epr "Reduced to %d items@." (List.length subset);
          ddmin subset 2
      | None -> (
          (* Test 2: Try removing each subset (reduce to complement) *)
          let rec try_complements idx = function
            | [] -> None
            | _ :: rest ->
                let complement =
                  List.concat (List.filteri (fun i _ -> i <> idx) subsets)
                in
                if List.length complement < len && check_fails complement then
                  Some complement
                else try_complements (idx + 1) rest
          in

          match try_complements 0 subsets with
          | Some complement ->
              Fmt.epr "Reduced to %d items@." (List.length complement);
              ddmin complement (max (n - 1) 2)
          | None ->
              (* Increase granularity *)
              if n < len then ddmin lst (min (n * 2) len) else lst)
  in
  ddmin lst 2

(** Find minimal failing pair from a list *)
let find_minimal_pair check_fails lst =
  let rec find_pair lst =
    match lst with
    | [] | [ _ ] -> None
    | a :: rest ->
        let rec try_with = function
          | [] -> find_pair rest
          | b :: rest' ->
              if check_fails [ a; b ] then Some [ a; b ] else try_with rest'
        in
        try_with rest
  in
  find_pair lst

(** Minimize a failing test case to smallest possible set Uses delta debugging
    followed by pair finding if needed *)
let minimize_failing_case check_fails initial =
  if not (check_fails initial) then None
  else
    let minimal = delta_debug check_fails initial in

    (* If we have more than 2 items, try to find a minimal pair *)
    let final =
      if List.length minimal > 2 then
        match find_minimal_pair check_fails minimal with
        | Some pair -> pair
        | None -> minimal
      else minimal
    in
    Some final
