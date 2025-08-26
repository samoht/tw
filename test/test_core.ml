open Alcotest

let test_style_pp () =
  let t = Tw.Core.style "foo" [] in
  check string "class name" "foo" (Tw.Core.pp t)

let tests = [ test_case "style/pp" `Quick test_style_pp ]
let suite = ("core", tests)
