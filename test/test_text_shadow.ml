let check = Test_helpers.check_handler_roundtrip (module Tw.Text_shadow.Handler)

let test_roundtrip () =
  check "text-shadow-none";
  check "text-shadow-2xs";
  check "text-shadow-xs";
  check "text-shadow-sm";
  check "text-shadow-md";
  check "text-shadow-lg";
  check "text-shadow-md/50"

let test_invalid () =
  let bad = Test_helpers.check_invalid_input (module Tw.Text_shadow.Handler) in
  bad "text-shadow-foo";
  (* Tailwind v4 has no bare text-shadow (sizes are 2xs..lg). *)
  bad "text-shadow"

(* The sm/md/lg sizes are three-shadow stacks in v4.3.1: md = 0 1px 1px, 0 1px
   2px, 0 2px 4px at alpha .1 (#0000001a). *)
let test_md_three_shadows () =
  let css =
    match Tw.of_string "text-shadow-md" with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string ~minify:true
    | Error _ -> Alcotest.fail "could not parse text-shadow-md"
  in
  Alcotest.(check bool)
    "text-shadow-md has the deepest 2px 4px shadow" true
    (Astring.String.is_infix ~affix:"2px 4px" css);
  Alcotest.(check bool)
    "text-shadow-md is a multi-shadow stack" true
    (Astring.String.is_infix ~affix:"1px 2px" css)

let tests =
  Test_helpers.standard ~roundtrip:test_roundtrip ~invalid:test_invalid
  @ [
      Alcotest.test_case "text-shadow-md three shadows" `Quick
        test_md_three_shadows;
    ]

let suite = ("text_shadow", tests)
