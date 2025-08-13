let () =
  let stylesheet = Tw.to_css [ Tw.p 0 ] in
  let css = Tw.Css.to_string ~minify:false stylesheet in
  Printf.printf "=== FULL CSS ===\n%s\n=== END ===" css;

  Printf.printf "\n\n=== LINES ===\n";
  css |> String.split_on_char '\n'
  |> List.iteri (fun i line -> Printf.printf "%3d: [%s]\n" i line)
