(** Tests for {!Tw_dom} — runs in both Node.js and browser.

    In Node.js only the utilities that do not touch the DOM run. In a browser
    all tests run, including {!Tw_dom} with real DOM injection. Open
    [_build/default/test/dom/index.html] to run in a browser. *)

module Css = Cascade.Css
open Alcotest

let css_generation () =
  let styles = Tw.[ flex; p 4; bg Tw.blue ] in
  let css = Tw.to_css ~base:false styles in
  let css_str = Css.to_string ~minify:true css in
  check bool "non-empty css" true (String.length css_str > 0);
  check bool "has flex" true (Astring.String.is_infix ~affix:"flex" css_str);
  check bool "has padding" true
    (Astring.String.is_infix ~affix:"padding" css_str)

let class_names () =
  let cls = Tw.to_classes Tw.[ flex; p 4; bg Tw.blue ] in
  check bool "has flex" true (Astring.String.is_infix ~affix:"flex" cls);
  check bool "has p-4" true (Astring.String.is_infix ~affix:"p-4" cls);
  check bool "has bg-blue" true (Astring.String.is_infix ~affix:"bg-blue" cls)

let str_parsing () =
  let styles = Tw.str "flex items-center gap-4 p-6" in
  check int "4 utilities" 4 (List.length styles);
  let cls = Tw.to_classes styles in
  check bool "has flex" true (Astring.String.is_infix ~affix:"flex" cls)

let dynamic_classes () =
  let color = "blue" in
  let cls = "bg-" ^ color ^ "-500 p-4 text-white" in
  let styles = Tw.str cls in
  let css = Tw.to_css ~base:false styles in
  let css_str = Css.to_string ~minify:true css in
  check bool "has bg color" true
    (Astring.String.is_infix ~affix:"background-color" css_str);
  check bool "has padding" true
    (Astring.String.is_infix ~affix:"padding" css_str)

let dom_use () =
  Tw_dom.init ~base:false ();
  let cls = Tw_dom.use Tw.[ flex; p 4; bg Tw.blue; rounded_lg ] in
  check bool "returns classes" true (String.length cls > 0);
  check bool "has flex" true (Astring.String.is_infix ~affix:"flex" cls);
  let css = Tw_dom.css () in
  check bool "css has flex" true (Astring.String.is_infix ~affix:"flex" css);
  check bool "css has padding" true
    (Astring.String.is_infix ~affix:"padding" css)

let dom_use_str () =
  Tw_dom.init ~base:false ();
  let cls = Tw_dom.use_str "flex items-center gap-4" in
  check string "returns input" "flex items-center gap-4" cls;
  let css = Tw_dom.css () in
  check bool "css has gap" true (Astring.String.is_infix ~affix:"gap" css)

let dom_dedup () =
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

(* A newly discovered batch injects as its own small element rather than by
   rewriting one growing element, so [data-tw="runtime"] can match several
   elements at once; concatenate them all in document order. *)
let style_el_text () =
  Brr.El.fold_find_by_selector
    (fun el acc -> acc ^ Jstr.to_string (Brr.El.text_content el))
    (Jstr.v "style[data-tw=\"runtime\"]")
    ""

let dom_batching () =
  (* [use] coalesces its rebuild onto a microtask, so the sheet in the document
     is only refreshed once the current task drains - or on demand. *)
  Tw_dom.init ~base:false ();
  Tw_dom.flush ();
  let before = style_el_text () in
  ignore (Tw_dom.use Tw.[ underline ]);
  check string "sheet untouched within the task" before (style_el_text ());
  Tw_dom.flush ();
  check bool "sheet refreshed on flush" true
    (Astring.String.is_infix ~affix:"underline" (style_el_text ()));
  let flushed = style_el_text () in
  Tw_dom.flush ();
  check string "flush with nothing pending is a no-op" flushed
    (style_el_text ())

let style_el_count () =
  Brr.El.fold_find_by_selector
    (fun _ acc -> acc + 1)
    (Jstr.v "style[data-tw=\"runtime\"]")
    0

(* A class registered on its own tick lands in its own small element rather than
   a full recompile over everything seen so far - the fix for the quadratic
   rebuild. Drip-feeding distinct classes one flush at a time must therefore see
   more than one element at some point; a batch large enough to dwarf whatever
   is already consolidated then folds everything - deltas and all - back into
   exactly one element, and the aggregated content must match a plain
   [Tw_dom.css ()] compile regardless of how many elements it came from. *)
let dom_incremental_consolidation () =
  Tw_dom.init ~base:false ();
  Tw_dom.flush ();
  let dripped = List.init 40 (fun i -> Fmt.str "pt-%d" (101 + i)) in
  let saw_multiple = ref false in
  List.iter
    (fun cls ->
      ignore (Tw_dom.use_str cls);
      Tw_dom.flush ();
      if style_el_count () > 1 then saw_multiple := true)
    dripped;
  check bool
    "drip-feeding distinct classes uses more than one element at some point"
    true !saw_multiple;
  let final_batch = List.init 100 (fun i -> Fmt.str "pr-%d" (201 + i)) in
  ignore (Tw_dom.use_str (String.concat " " final_batch));
  Tw_dom.flush ();
  check int
    "a batch dwarfing what is consolidated folds everything into one element" 1
    (style_el_count ());
  let all = dripped @ final_batch in
  let css = Tw_dom.css () in
  let dom_css = style_el_text () in
  List.iter
    (fun cls ->
      let selector = "." ^ cls in
      check bool
        (Fmt.str "css () has %s" cls)
        true
        (Astring.String.is_infix ~affix:selector css);
      check bool
        (Fmt.str "document has %s" cls)
        true
        (Astring.String.is_infix ~affix:selector dom_css))
    all

let has_dom =
  (* Check if document.createElement exists — absent in Node.js *)
  try
    ignore (Brr.Document.head Brr.G.document : Brr.El.t);
    true
  with Jv.Error _ -> false

let core_cases =
  [
    test_case "css generation" `Quick css_generation;
    test_case "class names" `Quick class_names;
    test_case "str parsing" `Quick str_parsing;
    test_case "dynamic classes" `Quick dynamic_classes;
  ]

let dom_cases =
  if has_dom then
    [
      test_case "use" `Quick dom_use;
      test_case "use_str" `Quick dom_use_str;
      test_case "dedup" `Quick dom_dedup;
      test_case "batching" `Quick dom_batching;
      test_case "incremental consolidation" `Quick dom_incremental_consolidation;
    ]
  else []

let suite = ("tw_dom", core_cases @ dom_cases)
