open Tw

let () =
  let styles = [ rounded_lg; shadow_md; p 6 ] in
  let css = to_css ~reset:true styles |> Css.to_string ~minify:true in

  (* Find layer for a variable *)
  let find_var_context var =
    if Astring.String.is_infix ~affix:var css then
      match Astring.String.find_sub ~sub:var css with
      | Some pos ->
          let start = max 0 (pos - 100) in
          let finish = min (String.length css) (pos + 100) in
          Printf.printf "\n%s context:\n%s\n" var
            (String.sub css start (finish - start))
      | None -> ()
  in

  find_var_context "--spacing";
  find_var_context "--radius-lg"
