let () =
  (* Test with reset disabled to see just the utility class *)
  let stylesheet = Tw.to_css ~reset:false [ Tw.p 0 ] in
  let css = Tw.Css.to_string ~minify:false stylesheet in
  Printf.printf "Full CSS:\n[%s]\n" css;
  Printf.printf "Lines:\n";
  css |> String.split_on_char '\n'
  |> List.iteri (fun i line -> Printf.printf "%2d: [%s]\n" i line)
