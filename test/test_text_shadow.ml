let check = Test_helpers.check_handler_roundtrip (module Tw.Text_shadow.Handler)

let test_roundtrip () =
  check "text-shadow-none";
  check "text-shadow-2xs";
  check "text-shadow-xs";
  check "text-shadow-sm";
  check "text-shadow-md";
  check "text-shadow-lg"

let test_invalid () =
  Test_helpers.check_invalid_input
    (module Tw.Text_shadow.Handler)
    "text-shadow-foo";
  (* Bare `text-shadow` is not a v4 utility (the CLI emits nothing); only the
     named scale `text-shadow-{2xs,xs,sm,md,lg}` is valid. *)
  Test_helpers.check_invalid_input (module Tw.Text_shadow.Handler) "text-shadow"

let parse s = Result.get_ok (Tw.of_string s)

(* The v4.3.1 default text-shadow scale: text-shadow-2xs uses alpha .15
   (#00000026), not the .1 (#0000001a) tw emitted before theme-threading. *)
let test_default_scale () =
  let css =
    Tw.to_css ~base:false [ parse "text-shadow-2xs" ]
    |> Tw.Css.to_string ~minify:true
  in
  Alcotest.(check bool)
    "text-shadow-2xs default is #00000026 (alpha .15)" true
    (Astring.String.is_infix ~affix:"#00000026" css)

(* A threaded @theme override for the text-shadow token flows through to the
   inlined value (here .1 = #0000001a), which is impossible without
   threading. *)
let test_theme_override () =
  let theme =
    Tw.Scheme.with_overrides Tw.Scheme.default
      [ ("text-shadow-2xs", "0px 1px 0px rgb(0 0 0 / 0.1)") ]
  in
  let css =
    Tw.to_css ~theme ~base:false [ parse "text-shadow-2xs" ]
    |> Tw.Css.to_string ~minify:true
  in
  Alcotest.(check bool)
    "text-shadow-2xs @theme override flows to #0000001a" true
    (Astring.String.is_infix ~affix:"#0000001a" css)

let tests =
  Test_helpers.standard ~roundtrip:test_roundtrip ~invalid:test_invalid
  @ [
      Alcotest.test_case "default scale (v4.3.1)" `Quick test_default_scale;
      Alcotest.test_case "@theme override threads through" `Quick
        test_theme_override;
    ]

let suite = ("text_shadow", tests)
