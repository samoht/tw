(** Tests for the HTML module *)

open Alcotest
open Tw_html

let test_txt () =
  let text = txt "Hello World" in
  let html_str = to_string text in
  check string "text content" "Hello World" html_str

let test_element_creation () =
  let div = div ~tw:Tw.[ p 4; bg_white ] [ txt "Content" ] in
  let html_str = to_string div in

  check bool "is div element" true
    (Astring.String.is_prefix ~affix:"<div" html_str);
  check bool "contains content" true
    (Astring.String.is_infix ~affix:"Content" html_str);
  check bool "has classes" true
    (Astring.String.is_infix ~affix:"class=" html_str)

let test_attributes () =
  let link =
    a
      ~at:[ At.href "/about"; At.title "About page" ]
      ~tw:Tw.[ text blue 600; on_hover [ underline ] ]
      [ txt "About" ]
  in
  let html_str = to_string link in

  check bool "has href" true
    (Astring.String.is_infix ~affix:"href=\"/about\"" html_str);
  check bool "has title" true
    (Astring.String.is_infix ~affix:"title=\"About page\"" html_str);
  check bool "contains text" true
    (Astring.String.is_infix ~affix:"About" html_str)

let test_nesting () =
  let nested =
    div
      [
        h1 [ txt "Title" ];
        p
          [
            txt "Paragraph with ";
            span ~tw:Tw.[ font_bold ] [ txt "bold" ];
            txt " text.";
          ];
      ]
  in
  let html_str = to_string nested in

  check bool "has h1" true (Astring.String.is_infix ~affix:"<h1>" html_str);
  check bool "has paragraph" true
    (Astring.String.is_infix ~affix:"<p>" html_str);
  check bool "has span with bold" true
    (Astring.String.is_infix ~affix:"<span" html_str);
  check bool "correct nesting" true
    (Astring.String.is_infix ~affix:"Paragraph with" html_str
    && Astring.String.is_infix ~affix:"bold" html_str
    && Astring.String.is_infix ~affix:"text." html_str)

let test_to_tw () =
  let elem =
    div
      ~tw:Tw.[ p 4; bg_white; text gray 900; rounded_lg ]
      [ span ~tw:Tw.[ font_bold ] [ txt "Text" ] ]
  in

  let tw_classes = to_tw elem in
  check Alcotest.int "collected 5 classes" 5 (List.length tw_classes);
  check bool "has p_4" true
    (List.exists (fun tw -> Tw.pp tw = "p-4") tw_classes);
  check bool "has font_bold" true
    (List.exists (fun tw -> Tw.pp tw = "font-bold") tw_classes)

let test_pp () =
  let elem = div ~tw:Tw.[ p 2 ] [ txt "Test" ] in
  let pp_output = pp elem in

  check bool "pp contains element tag" true
    (Astring.String.is_infix ~affix:"<div" pp_output);
  check bool "pp contains classes" true
    (Astring.String.is_infix ~affix:"p-2" pp_output)

let test_page_cache_busting () =
  (* Test that page function generates cache-busted CSS URLs *)
  let test_page =
    page ~title:"Test Page"
      ~meta:[ ("description", "Test page for cache busting") ]
      ~tw_css:(Link "styles.css") [ (* head content *) ]
      [ div ~tw:Tw.[ p 4; bg_white ] [ txt "Test content" ] ]
  in

  let html_content = html test_page in
  let css_filename, _css_stylesheet = css test_page in

  (* Check that the HTML contains a cache-busted CSS link *)
  check bool "HTML contains link tag" true
    (Astring.String.is_infix ~affix:"<link" html_content);
  check bool "CSS link has cache buster" true
    (Astring.String.is_infix ~affix:"styles.css?v=" html_content);
  check string "CSS filename is correct" "styles.css" css_filename;

  (* Check that the hash is 8 characters (MD5 hash prefix) *)
  match Astring.String.find_sub ~sub:"styles.css?v=" html_content with
  | Some idx ->
      let hash_start = idx + String.length "styles.css?v=" in
      let rest =
        String.sub html_content hash_start
          (String.length html_content - hash_start)
      in
      (* Find the end of the hash (until quote) *)
      let hash_end = try String.index rest '"' with Not_found -> 8 in
      let hash = String.sub rest 0 hash_end in
      check int "hash length is 8 characters" 8 (String.length hash);
      (* Check that hash is hexadecimal *)
      let is_hex c =
        (c >= '0' && c <= '9')
        || (c >= 'a' && c <= 'f')
        || (c >= 'A' && c <= 'F')
      in
      check bool "hash is hexadecimal" true (String.for_all is_hex hash)
  | None -> fail "Cache buster not found in HTML"

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

let test_inline_style () =
  let page =
    page ~title:"Inline Style Test" ~tw_css:Inline []
      [ div ~tw:Tw.[ p 4; bg_white ] [ txt "Inline style test" ] ]
  in
  let html_content = html page in

  (* Check that the HTML contains inline styles *)
  check bool "HTML contains inline style" true
    (Astring.String.is_infix ~affix:"<style>" html_content);
  check bool "Inline style has content" true
    (Astring.String.is_infix ~affix:"p-4" html_content);
  check bool "Inline style has bg-white" true
    (Astring.String.is_infix ~affix:"bg-white" html_content)

let suite =
  ( "html",
    [
      test_case "txt" `Quick test_txt;
      test_case "element creation" `Quick test_element_creation;
      test_case "attributes" `Quick test_attributes;
      test_case "nesting" `Quick test_nesting;
      test_case "to_tw" `Quick test_to_tw;
      test_case "pretty printing" `Quick test_pp;
      test_case "page cache busting" `Quick test_page_cache_busting;
      test_case "cache busting consistency" `Quick
        test_page_cache_busting_consistency;
      test_case "inline style" `Quick test_inline_style;
    ] )
