open Tw

let () =
  let style1 = bg (hex "#1da1f2") 0 in
  let style2 = bg (hex "1da1f2") 0 in
  Printf.printf "With hash: %s\n" (pp style1);
  Printf.printf "Without hash: %s\n" (pp style2)
