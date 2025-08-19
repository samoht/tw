open Tw

let () =
  let styles = [ rounded_lg; shadow_md; p 6 ] in
  let css = to_css ~reset:true styles |> Css.to_string ~minify:true in

  (* Find layer for a variable *)
  let find_var_context var =
    if String.contains_s css var then
      let pos = String.index_s css var in
      let start = max 0 (pos - 100) in
      let finish = min (String.length css) (pos + 100) in
      Printf.printf "\n%s context:\n%s\n" var
        (String.sub css start (finish - start))
  in

  find_var_context "--spacing";
  find_var_context "--radius-lg"
