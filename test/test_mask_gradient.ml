let check =
  Test_helpers.check_handler_roundtrip (module Tw.Mask_gradient.Handler)

let test_roundtrip () =
  check "mask-t-from-0%";
  check "mask-t-to-100%";
  check "mask-b-from-50%";
  check "mask-b-to-50%";
  check "mask-l-from-0%";
  check "mask-r-to-100%";
  check "mask-x-from-0%";
  check "mask-y-to-100%";
  check "mask-radial";
  check "mask-linear-0";
  check "mask-linear-1";
  check "mask-linear-45";
  check "-mask-linear-1";
  check "mask-conic-0";
  check "mask-conic-1";
  check "mask-conic-45";
  check "mask-linear-[3rad]"

let test_invalid () =
  Test_helpers.check_invalid_input (module Tw.Mask_gradient.Handler) "mask-foo"

(* A mask stop is a <length-percentage>: the zero spacing step keeps its unit
   (0px), not a bare 0, which is what Tailwind emits. *)
let test_from_zero_keeps_unit () =
  let css =
    match Tw.of_string "mask-t-from-0" with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.pp ~minify:true
    | Error (`Msg m) -> Alcotest.failf "mask-t-from-0: %s" m
  in
  Alcotest.(check bool)
    "from-position is 0px, not bare 0" true
    (Astring.String.is_infix ~affix:"--tw-mask-top-from-position:0px" css)

let tests =
  Test_helpers.standard ~roundtrip:test_roundtrip ~invalid:test_invalid
  @ [
      Alcotest.test_case "from-0 keeps its px unit" `Quick
        test_from_zero_keeps_unit;
    ]

let suite = ("mask_gradient", tests)
