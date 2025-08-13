open Tw

let () =
  let style1 = bg (hex "#1da1f2") 0 in
  let style2 = bg (hex "1da1f2") 0 in
  Printf.printf "With hash: %s\n" (pp style1);
  Printf.printf "Without hash: %s\n" (pp style2);

  (* Also check the CSS output *)
  let css1 = to_css [ style1 ] |> Css.to_string in
  let css2 = to_css [ style2 ] |> Css.to_string in
  Printf.printf "CSS with hash:\n%s\n" css1;
  Printf.printf "CSS without hash:\n%s\n" css2
