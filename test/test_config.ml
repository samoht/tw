module Config = Tw.Config

let test_default_emits_everything () =
  Alcotest.(check bool) "base layer" true (Config.base Config.default);
  Alcotest.(check bool) "layer wrappers" true (Config.layers Config.default);
  Alcotest.(check bool)
    "forms decided from the utilities" true
    (Config.forms Config.default = None)

let test_v_replaces_only_what_it_is_given () =
  let c = Config.v ~base:false () in
  Alcotest.(check bool) "base off" false (Config.base c);
  Alcotest.(check bool) "layers untouched" true (Config.layers c);
  Alcotest.(check bool) "forms untouched" true (Config.forms c = None);
  let c = Config.v ~forms:true () in
  Alcotest.(check bool) "forms forced on" true (Config.forms c = Some true);
  Alcotest.(check bool) "base untouched" true (Config.base c)

let test_v_with_no_arguments_is_default () =
  Alcotest.(check bool) "same value" true (Config.v () = Config.default)

(* [base:false] is what the whole test suite passes to compare a single utility
   against Tailwind, so the layer it drops has to be the base one and nothing
   else. *)
let test_base_false_drops_only_the_base_layer () =
  let css cfg = Tw.to_css ~config:cfg [ Tw.p 4 ] |> Tw.Css.to_string in
  let with_base = css Config.default in
  let without = css (Config.v ~base:false ()) in
  let has s sub = Astring.String.is_infix ~affix:sub s in
  Alcotest.(check bool) "base layer present" true (has with_base "@layer base");
  Alcotest.(check bool) "base layer dropped" false (has without "@layer base");
  Alcotest.(check bool) "utilities kept" true (has without "padding");
  Alcotest.(check bool) "theme layer kept" true (has without "@layer theme")

let test_layers_false_unwraps_but_keeps_properties () =
  let css = Tw.to_css ~config:(Config.v ~layers:false ()) [ Tw.shadow_sm ] in
  let s = Tw.Css.to_string css in
  let has sub = Astring.String.is_infix ~affix:sub s in
  Alcotest.(check bool) "utilities unwrapped" false (has "@layer utilities");
  Alcotest.(check bool) "properties kept" true (has "@layer properties")

let test_to_string () =
  Alcotest.(check string)
    "default" "{base=true; forms=auto; layers=true}"
    (Config.to_string Config.default);
  Alcotest.(check string)
    "forms forced off" "{base=false; forms=false; layers=true}"
    (Config.to_string (Config.v ~base:false ~forms:false ()))

let tests =
  Alcotest.
    [
      test_case "default emits everything" `Quick test_default_emits_everything;
      test_case "v replaces only what it is given" `Quick
        test_v_replaces_only_what_it_is_given;
      test_case "v () is default" `Quick test_v_with_no_arguments_is_default;
      test_case "base:false drops only the base layer" `Quick
        test_base_false_drops_only_the_base_layer;
      test_case "layers:false keeps @layer properties" `Quick
        test_layers_false_unwraps_but_keeps_properties;
      test_case "to_string" `Quick test_to_string;
    ]

let suite = ("config", tests)
