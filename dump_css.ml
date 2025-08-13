let () =
  let stylesheet = Tw.to_css [ Tw.p 0 ] in
  let css = Tw.Css.to_string ~minify:false stylesheet in
  Printf.printf "%s\n" css
