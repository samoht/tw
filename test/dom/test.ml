module Css = Cascade.Css
(* Tests for tw.dom — runs in both Node.js and browser.

   In Node.js: only core tests run (no DOM). In a browser: all tests run
   including Tw_dom with real DOM injection. Open
   _build/default/test/dom/index.html to run in a browser. *)

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

let test_dom_use () =
  Tw_dom.init ~base:false ();
  let cls = Tw_dom.use Tw.[ flex; p 4; bg Tw.blue; rounded_lg ] in
  check bool "returns classes" true (String.length cls > 0);
  check bool "has flex" true (Astring.String.is_infix ~affix:"flex" cls);
  let css = Tw_dom.css () in
  check bool "css has flex" true (Astring.String.is_infix ~affix:"flex" css);
  check bool "css has padding" true
    (Astring.String.is_infix ~affix:"padding" css)

let test_dom_use_str () =
  Tw_dom.init ~base:false ();
  let cls = Tw_dom.use_str "flex items-center gap-4" in
  check string "returns input" "flex items-center gap-4" cls;
  let css = Tw_dom.css () in
  check bool "css has gap" true (Astring.String.is_infix ~affix:"gap" css)

let test_dom_dedup () =
  Tw_dom.init ~base:false ();
  ignore (Tw_dom.use Tw.[ flex; p 4 ]);
  let css1 = Tw_dom.css () in
  ignore (Tw_dom.use Tw.[ flex; p 4 ]);
  let css2 = Tw_dom.css () in
  check string "no change on reuse" css1 css2;
  ignore (Tw_dom.use Tw.[ bg Tw.red ]);
  let css3 = Tw_dom.css () in
  check bool "grows with new utility" true
    (String.length css3 > String.length css1)

let has_dom =
  (* Check if document.createElement exists — absent in Node.js *)
  try
    ignore (Brr.Document.head Brr.G.document : Brr.El.t);
    true
  with _ -> false

let dom_tests =
  if has_dom then
    [
      ( "dom",
        [
          test_case "use" `Quick test_dom_use;
          test_case "use_str" `Quick test_dom_use_str;
          test_case "dedup" `Quick test_dom_dedup;
        ] );
    ]
  else []

let () =
  run ~and_exit:false "tw_dom"
    ([
       ( "core",
         [
           test_case "css generation" `Quick test_css_generation;
           test_case "class names" `Quick test_class_names;
           test_case "str parsing" `Quick test_str_parsing;
           test_case "dynamic classes" `Quick test_dynamic_classes;
         ] );
     ]
    @ dom_tests)
