(** Tests for the HTML module *)

open Alcotest
open Tw_html

let test_txt () =
  let text = txt "Hello World" in
  let html_str = to_string text in
  check string "text content" "Hello World" html_str

(* The whole rendering is compared rather than searched: a substring says
   nothing about where the class attribute went or what else came out beside it,
   and a [check bool] failure prints neither the markup nor the subject. *)
let test_element_creation () =
  check string "utilities render into one class attribute"
    {|<div class="p-4 bg-white">Content</div>|}
    (to_string (div ~tw:Tw.[ p 4; bg white ] [ txt "Content" ]))

let test_attributes () =
  check string "the class attribute comes first, then the given ones in order"
    {|<a class="text-blue-600 hover:underline" href="/about" title="About page">About</a>|}
    (to_string
       (a
          ~at:[ At.href "/about"; At.title "About page" ]
          ~tw:Tw.[ text ~shade:600 blue; hover [ underline ] ]
          [ txt "About" ]))

let test_html_escaping () =
  check string "text content escapes <, >, & and the quote"
    "&lt;script&gt;if (a &amp;&amp; b) alert(&quot;x&quot;)&lt;/script&gt;"
    (to_string (txt "<script>if (a && b) alert(\"x\")</script>"));
  check string "an attribute value escapes the same four"
    {|<div title="5 &gt; 3 &amp; &quot;yes&quot;">t</div>|}
    (to_string (div ~at:[ At.title "5 > 3 & \"yes\"" ] [ txt "t" ]))

let test_boolean_and_data_attrs () =
  (* A boolean attribute is written with an empty value, which HTML reads as the
     attribute being present. A substring on the name alone could not tell that
     from any other spelling. *)
  check string "boolean attributes carry an empty value"
    {|<input disabled="" checked="" required="" />|}
    (to_string (input ~at:[ At.disabled; At.checked; At.required ] ()));
  check string "aria and data attributes serialise in order, values escaped"
    {|<div aria-label="Greeting" aria-hidden="true" aria-expanded="true" data-user="Tom &amp; Jerry">x</div>|}
    (to_string
       (div
          ~at:
            [
              Aria.label "Greeting";
              Aria.hidden;
              Aria.expanded true;
              At.v "data-user" "Tom & Jerry";
            ]
          [ txt "x" ]))

let test_nesting () =
  check string "children render in place, in document order"
    {|<div><h1>Title</h1><p>Paragraph with <span class="font-bold">bold</span> text.</p></div>|}
    (to_string
       (div
          [
            h1 [ txt "Title" ];
            p
              [
                txt "Paragraph with ";
                span ~tw:Tw.[ font_bold ] [ txt "bold" ];
                txt " text.";
              ];
          ]))

let test_to_tw () =
  let elem =
    div
      ~tw:Tw.[ p 4; bg white; text ~shade:900 gray; rounded_lg ]
      [ span ~tw:Tw.[ font_bold ] [ txt "Text" ] ]
  in

  let tw_classes = to_tw elem in
  check Alcotest.int "collected 5 classes" 5 (List.length tw_classes);
  check bool "has p_4" true
    (List.exists (fun tw -> Tw.pp tw = "p-4") tw_classes);
  check bool "has font_bold" true
    (List.exists (fun tw -> Tw.pp tw = "font-bold") tw_classes)

let test_pp () =
  check string "pp names the utilities alongside the rendering"
    {|<element with classes="p-2"><div class="p-2">Test</div></element>|}
    (pp (div ~tw:Tw.[ p 2 ] [ txt "Test" ]))

let test_page_cache_busting () =
  (* Test that page function generates cache-busted CSS URLs *)
  let test_page =
    page ~title:"Test Page"
      ~meta:[ ("description", "Test page for cache busting") ]
      ~tw_css:(Link "styles.css") [ (* head content *) ]
      [ div ~tw:Tw.[ p 4; bg white ] [ txt "Test content" ] ]
  in

  let html_content = html test_page in
  let css_filename, _css_stylesheet = css test_page in

  check
    Alcotest.(option string)
    "CSS filename is correct" (Some "styles.css") css_filename;

  (* The buster is an MD5 prefix over the stylesheet, so it moves with anything
     that changes the CSS. Read it back and check its shape, then compare the
     document around it whole: that is what says the link is a [<link>] in the
     head and that nothing else was emitted beside it. *)
  let hash =
    match Astring.String.find_sub ~sub:"styles.css?v=" html_content with
    | None -> fail "Cache buster not found in HTML"
    | Some idx ->
        let start = idx + String.length "styles.css?v=" in
        let rest =
          String.sub html_content start (String.length html_content - start)
        in
        let stop =
          match Astring.String.find (Char.equal '"') rest with
          | Some i -> i
          | None -> String.length rest
        in
        String.sub rest 0 stop
  in
  check int "hash length is 8 characters" 8 (String.length hash);
  let is_hex c =
    (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')
  in
  check bool "hash is hexadecimal" true (String.for_all is_hex hash);
  check string "the document links the cache-busted stylesheet"
    ({|<!DOCTYPE html>|} ^ "\n"
   ^ {|<html lang="en"><head><meta charset="utf-8" /><meta name="description" content="Test page for cache busting" /><title>Test Page</title><link rel="stylesheet" href="styles.css?v=|}
   ^ hash
   ^ {|" /></head><body><div class="p-4 bg-white">Test content</div></body></html>|}
    )
    html_content

let test_page_cache_busting_consistency () =
  (* Test that same content produces same hash *)
  let create_test_page () =
    page ~title:"Test" ~tw_css:(Link "test.css") []
      [ div ~tw:Tw.[ p 4; m 2 ] [ txt "Content" ] ]
  in

  let page1 = create_test_page () in
  let page2 = create_test_page () in

  let html1 = html page1 in
  let html2 = html page2 in

  (* Extract hash from both *)
  let extract_hash html =
    match Astring.String.find_sub ~sub:"test.css?v=" html with
    | Some idx ->
        let hash_start = idx + String.length "test.css?v=" in
        let rest = String.sub html hash_start 8 in
        rest
    | None -> fail "Hash not found"
  in

  let hash1 = extract_hash html1 in
  let hash2 = extract_hash html2 in

  check string "same content produces same hash" hash1 hash2;

  (* Different content should produce different hash *)
  let page3 =
    page ~title:"Test" ~tw_css:(Link "test.css") []
      [ div ~tw:Tw.[ p 8; m 4 ] [ txt "Different" ] ]
  in
  let html3 = html page3 in
  let hash3 = extract_hash html3 in

  check bool "different content produces different hash" false (hash1 = hash3)

let test_inline_css () =
  let test_page =
    page ~title:"Inline Style Test" ~tw_css:Inline []
      [ div ~tw:Tw.[ p 4; bg white ] [ txt "Inline content" ] ]
  in
  let html_content = html test_page in
  let css_filename, css_stylesheet = css test_page in

  (* Inline pages have no external CSS file *)
  check Alcotest.(option string) "no external CSS file" None css_filename;

  (* The stylesheet is inlined verbatim inside [<style>], a raw-text element, so
     the CSS must not be HTML-escaped ([>] stays [>], not [&gt;]). Composing the
     expected document from the stylesheet keeps this about the embedding rather
     than about the sheet, and comparing it whole is what says no [<link>] went
     out beside it. *)
  check string "stylesheet embedded verbatim, and no link tag"
    ({|<!DOCTYPE html>|} ^ "\n"
   ^ {|<html lang="en"><head><meta charset="utf-8" /><title>Inline Style Test</title><style>|}
    ^ Tw.Css.to_string ~minify:true css_stylesheet
    ^ {|</style></head><body><div class="p-4 bg-white">Inline content</div></body></html>|}
    )
    html_content

let test_class_merging () =
  (* The tw utilities and the explicit class attribute merge into one attribute,
     utilities first. Comparing the whole rendering is what says there is no
     second [class=]. *)
  check string "one class attribute, utilities then the explicit names"
    {|<div class="p-4 flex custom-class">Merged</div>|}
    (to_string
       (div
          ~at:[ At.v "class" "custom-class" ]
          ~tw:Tw.[ p 4; flex ]
          [ txt "Merged" ]))

let test_no_class_without_tw () =
  check string "no class attribute when there are no utilities"
    "<div>Plain</div>"
    (to_string (div [ txt "Plain" ]))

let test_void_elements () =
  (* A void element carries no end tag, whichever constructor built it. *)
  check string "source self-closed, with no end tag"
    {|<picture><source src="a.webp" /></picture>|}
    (to_string (picture [ source ~at:[ At.src "a.webp" ] () ]));
  check string "img self-closed" {|<img src="a.png" />|}
    (to_string (img ~at:[ At.src "a.png" ] ()));
  let br_str = to_string (br ()) in
  check string "br self-closed" "<br />" br_str;
  let hr_str = to_string (hr ()) in
  check string "hr self-closed" "<hr />" hr_str;
  let meta_str = to_string (meta ~at:[ At.charset "utf-8" ] ()) in
  check string "meta self-closed" "<meta charset=\"utf-8\" />" meta_str;
  let link_str = to_string (link ~at:[ At.rel "icon" ] ()) in
  check string "link self-closed" "<link rel=\"icon\" />" link_str;
  let input_str = to_string (input ~at:[ At.name "q" ] ()) in
  check string "input self-closed" "<input name=\"q\" />" input_str

let test_class_attribute_whitespace () =
  (* A class attribute written across lines is still a list of classes. *)
  let elem = div ~at:[ At.v "class" "flex\n  items-center\tgap-4" ] [] in
  check (list string) "every class recognised"
    [ "flex"; "items-center"; "gap-4" ]
    (List.map Tw.pp (to_tw elem));
  check string "rendered on one line"
    "<div class=\"flex items-center gap-4\"></div>" (to_string elem)

let test_document_rendering () =
  (* The whole document is written into one buffer, so the doctype, the empty
     node and raw text must each still land exactly once and in place. *)
  let doc = root [ body [ txt "x" ] ] in
  check string "doctype heads the document"
    "<!DOCTYPE html>\n<html><body>x</body></html>"
    (to_string ~doctype:true doc);
  let nested = div [ doc; empty; raw "<b>&</b>" ] in
  check string "no second doctype, empty writes nothing, raw is verbatim"
    "<div><html><body>x</body></html><b>&</b></div>"
    (to_string ~doctype:true nested)

let test_to_tw_document_order () =
  (* A node reports its own utilities before its children's, and its children's
     in document order, however deep the tree. *)
  let leaf n = span ~tw:[ Tw.p n ] [] in
  let tree =
    div
      ~tw:Tw.[ flex ]
      [
        section ~tw:Tw.[ m 1 ] [ leaf 1; div [ leaf 2 ] ];
        section ~tw:Tw.[ m 2 ] [ leaf 3 ];
      ]
  in
  check (list string) "pre-order, own before children"
    [ "flex"; "m-1"; "p-1"; "p-2"; "m-2"; "p-3" ]
    (List.map Tw.pp (to_tw tree))

let sheet p = Tw.Css.to_string ~minify:true (snd (css p))

let test_repeated_utilities_emit_one_sheet () =
  (* Repeating a utility across a page must not change the sheet: that is what
     lets the repeats be dropped before they are compiled. *)
  let markup children = page ~tw_css:Inline [] children in
  let once =
    markup
      [
        div
          ~at:[ At.v "class" "underline" ]
          ~tw:Tw.[ p 4; flex; hover [ bg white ] ]
          [ span ~tw:Tw.[ m 2 ] [ txt "a" ] ];
      ]
  in
  let repeated =
    markup
      [
        div
          ~at:[ At.v "class" "underline" ]
          ~tw:Tw.[ p 4; flex; hover [ bg white ] ]
          (List.init 20 (fun _ ->
               span
                 ~at:[ At.v "class" "underline" ]
                 ~tw:Tw.[ p 4; m 2; flex; hover [ bg white ] ]
                 [ txt "a" ]));
      ]
  in
  check string "sheet is the same" (sheet once) (sheet repeated)

(* A class the parser does not recognise still reaches the rendered attribute -
   a framework hook or a JS selector belongs there - but it is no longer
   invisible: [unknown_classes] reports it so a typo can be caught. *)
let test_unknown_class_passes_through () =
  let elem = div ~at:[ At.v "class" "flex bg-blu-500 my-app-header" ] [] in
  check string "unknown names still rendered"
    "<div class=\"flex bg-blu-500 my-app-header\"></div>" (to_string elem);
  check (list string) "only the utility compiles" [ "flex" ]
    (List.map Tw.pp (to_tw elem));
  check (list string) "unknown names reported, in source order"
    [ "bg-blu-500"; "my-app-header" ]
    (unknown_classes elem)

(* Reported the way utilities are: a node's own names before its children's,
   children in document order. *)
let test_unknown_classes_document_order () =
  let hook n = span ~at:[ At.v "class" n ] [] in
  let tree =
    div
      ~at:[ At.v "class" "outer-hook flex" ]
      [ section [ hook "a-hook"; div [ hook "b-hook" ] ]; hook "c-hook" ]
  in
  check (list string) "pre-order, own before children"
    [ "outer-hook"; "a-hook"; "b-hook"; "c-hook" ]
    (unknown_classes tree)

let suite =
  ( "tw_html",
    [
      test_case "txt" `Quick test_txt;
      test_case "element creation" `Quick test_element_creation;
      test_case "attributes" `Quick test_attributes;
      test_case "class merging" `Quick test_class_merging;
      test_case "no class without tw" `Quick test_no_class_without_tw;
      test_case "html escaping" `Quick test_html_escaping;
      test_case "boolean + aria/data attrs" `Quick test_boolean_and_data_attrs;
      test_case "nesting" `Quick test_nesting;
      test_case "document rendering" `Quick test_document_rendering;
      test_case "to_tw" `Quick test_to_tw;
      test_case "to_tw document order" `Quick test_to_tw_document_order;
      test_case "pretty printing" `Quick test_pp;
      test_case "page cache busting" `Quick test_page_cache_busting;
      test_case "cache busting consistency" `Quick
        test_page_cache_busting_consistency;
      test_case "inline css" `Quick test_inline_css;
      test_case "void elements" `Quick test_void_elements;
      test_case "class attribute whitespace" `Quick
        test_class_attribute_whitespace;
      test_case "repeated utilities emit one sheet" `Quick
        test_repeated_utilities_emit_one_sheet;
      test_case "unknown class passes through" `Quick
        test_unknown_class_passes_through;
      test_case "unknown classes document order" `Quick
        test_unknown_classes_document_order;
    ] )
