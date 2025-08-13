let () =
  let stylesheet = Tw.to_css ~reset:false [ Tw.p 0 ] in
  let css = Tw.Css.to_string ~minify:false stylesheet in
  Printf.printf "CSS Output:\n%s\n" css;
  Printf.printf "Length: %d\n" (String.length css)
