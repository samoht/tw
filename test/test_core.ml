open Alcotest

let test_style_pp () =
  let t = Tw.Core.style "foo" [] in
  check string "class name" "foo" (Tw.Core.pp t)

let test_fun_selector () =
  let open Tw.Css.Selector in
  (* Test :is() functional pseudo-class *)
  let is_selector = fun_ "is" [ element "button"; element "input" ] in
  check string ":is() selector" ":is(button, input)" (to_string is_selector);

  (* Test :has() functional pseudo-class *)
  let has_selector = fun_ "has" [ class_ "active" ] in
  check string ":has() selector" ":has(.active)" (to_string has_selector);

  (* Test minified output *)
  let minified = fun_ "is" [ element "a"; element "button" ] in
  check string "minified :is()" ":is(a,button)"
    (to_string ~minify:true minified)

let test_fun_selector_combination () =
  let open Tw.Css.Selector in
  (* Test combining Fun selector with other selectors *)
  let combined =
    element "select"
    ++ fun_ "is" [ attribute "multiple" Presence; attribute "size" Presence ]
  in
  check string "select :is(...)" "select :is([multiple], [size])"
    (to_string combined)

let tests =
  [
    test_case "style/pp" `Quick test_style_pp;
    test_case "fun_selector" `Quick test_fun_selector;
    test_case "fun_selector_combination" `Quick test_fun_selector_combination;
  ]

let suite = ("core", tests)
