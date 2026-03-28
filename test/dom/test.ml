(* Tests for tw.dom — verifies that Tw core works under js_of_ocaml.

   These tests run in Node.js via js_of_ocaml to verify the CSS generation and
   class name logic works correctly in a JS environment. The actual DOM
   injection (Tw_dom.use) requires a browser. *)

open Alcotest

let test_css_generation () =
  let styles = Tw.[ flex; p 4; bg Tw.blue ] in
  let css = Tw.to_css ~base:false ~optimize:true styles in
  let css_str = Css.to_string ~minify:true css in
  check bool "non-empty css" true (String.length css_str > 0);
  check bool "has flex" true (Astring.String.is_infix ~affix:"flex" css_str);
  check bool "has padding" true
    (Astring.String.is_infix ~affix:"padding" css_str)

let test_class_names () =
  let cls = Tw.to_classes Tw.[ flex; p 4; bg Tw.blue ] in
  check bool "has flex" true (Astring.String.is_infix ~affix:"flex" cls);
  check bool "has p-4" true (Astring.String.is_infix ~affix:"p-4" cls);
  check bool "has bg-blue" true (Astring.String.is_infix ~affix:"bg-blue" cls)

let test_str_parsing () =
  let styles = Tw.str "flex items-center gap-4 p-6" in
  check int "4 utilities" 4 (List.length styles);
  let cls = Tw.to_classes styles in
  check bool "has flex" true (Astring.String.is_infix ~affix:"flex" cls)

let test_dynamic_classes () =
  let color = "blue" in
  let cls = "bg-" ^ color ^ "-500 p-4 text-white" in
  let styles = Tw.str cls in
  let css = Tw.to_css ~base:false ~optimize:true styles in
  let css_str = Css.to_string ~minify:true css in
  check bool "has bg color" true
    (Astring.String.is_infix ~affix:"background-color" css_str);
  check bool "has padding" true
    (Astring.String.is_infix ~affix:"padding" css_str)

let test_incremental_accumulation () =
  let s1 = Tw.[ flex; p 4 ] in
  let s2 = Tw.[ bg Tw.blue; rounded_lg ] in
  let all = s1 @ s2 in
  let css = Tw.to_css ~base:false ~optimize:true all in
  let css_str = Css.to_string ~minify:true css in
  check bool "has flex" true (Astring.String.is_infix ~affix:"flex" css_str);
  check bool "has bg" true
    (Astring.String.is_infix ~affix:"background-color" css_str);
  check bool "has rounded" true
    (Astring.String.is_infix ~affix:"border-radius" css_str)

let () =
  run "tw_js"
    [
      ( "core",
        [
          test_case "css generation" `Quick test_css_generation;
          test_case "class names" `Quick test_class_names;
          test_case "str parsing" `Quick test_str_parsing;
          test_case "dynamic classes" `Quick test_dynamic_classes;
          test_case "incremental accumulation" `Quick
            test_incremental_accumulation;
        ] );
    ]
