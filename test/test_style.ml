open Alcotest

let test_style_pp () =
  let t = Tw.Style.style "foo" [] in
  check string "class name" "foo" (Tw.Style.pp t)

let tests = [ test_case "style/pp" `Quick test_style_pp ]
let suite = ("core", tests)
