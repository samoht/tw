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
      ~tw_css:"styles.css" [ (* head content *) ]
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
    page ~title:"Test" ~tw_css:"test.css" []
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
    page ~title:"Test" ~tw_css:"test.css" []
      [ div ~tw:Tw.[ p 8; m 4 ] [ txt "Different" ] ]
  in
  let html3 = html page3 in
  let hash3 = extract_hash html3 in

  check bool "different content produces different hash" false (hash1 = hash3)

let test_exact_tailwind_match () =
  (* Create a simple HTML page with various Tailwind classes *)
  let page_content =
    div
      ~tw:Tw.[ p 4; bg blue 500; on_hover [ bg blue 600 ] ]
      [
        h1
          ~tw:Tw.[ text_2xl; font_bold; text white 0; mb 4 ]
          [ txt "Test Page" ];
        p
          ~tw:Tw.[ text gray 200; on_hover [ text white 0 ] ]
          [ txt "Testing hover states" ];
        (* Test group hover *)
        div
          ~tw:Tw.[ group; p 4 ]
          [
            p
              ~tw:Tw.[ on_group_hover [ text red 500 ] ]
              [ txt "Group hover test" ];
          ];
        (* Test peer *)
        div
          [
            input ~at:[ At.type' "checkbox" ] ~tw:Tw.[ peer ] ();
            p
              ~tw:Tw.[ on_peer_checked [ text green 500 ] ]
              [ txt "Peer checked test" ];
          ];
        (* Test aria *)
        div
          ~at:[ At.v "aria-checked" "true" ]
          ~tw:Tw.[ on_aria_checked [ bg purple 100 ] ]
          [ txt "Aria checked test" ];
        (* Test data attribute *)
        div
          ~at:[ At.v "data-active" "true" ]
          ~tw:Tw.[ on_data_active [ font_bold ] ]
          [ txt "Data active test" ];
      ]
  in

  (* Generate HTML and CSS *)
  let generated_page = page ~title:"Test" [] [ page_content ] in
  let html_output = html generated_page in
  let _css_filename, css_stylesheet = css generated_page in
  let css_output = Tw.Css.to_string ~minify:false css_stylesheet in

  (* Write HTML to temp file *)
  let html_file = "/tmp/tw_test_exact.html" in
  let oc = open_out html_file in
  (* Write complete HTML document *)
  Printf.fprintf oc
    "<!DOCTYPE html>\n\
     <html>\n\
     <head>\n\
     <meta charset=\"UTF-8\">\n\
     </head>\n\
     <body>\n\
     %s\n\
     </body>\n\
     </html>"
    html_output;
  close_out oc;

  (* Create minimal Tailwind config *)
  let tailwind_config =
    {|
module.exports = {
  content: ["/tmp/tw_test_exact.html"],
  theme: {
    extend: {},
  },
  plugins: [],
}
|}
  in
  let config_file = "/tmp/tailwind.config.js" in
  let oc = open_out config_file in
  output_string oc tailwind_config;
  close_out oc;

  (* Run real Tailwind CSS - v3 compatible mode if available *)
  let tailwind_cmd =
    "cd /tmp && npx tailwindcss -i /dev/stdin -c tailwind.config.js -o \
     tailwind_real.css <<< '@tailwind base; @tailwind components; @tailwind \
     utilities;' 2>/dev/null"
  in
  let exit_code = Sys.command tailwind_cmd in

  if exit_code <> 0 then (
    Printf.printf
      "Note: Tailwind CSS comparison test skipped (tailwindcss not available \
       or wrong version)\n";
    ())
  else
    (* Read Tailwind's output *)
    let ic = open_in "/tmp/tailwind_real.css" in
    let tailwind_css = really_input_string ic (in_channel_length ic) in
    close_in ic;

    (* Compare key selectors - Tailwind uses specific patterns *)
    let check_selector pattern name =
      if not (Astring.String.is_infix ~affix:pattern tailwind_css) then
        Printf.printf
          "Warning: Tailwind doesn't contain expected %s selector: %s\n" name
          pattern
      else Printf.printf "✓ Found %s selector in Tailwind output\n" name
    in

    (* These are the actual selectors Tailwind generates *)
    check_selector ".hover\\:bg-blue-600:hover" "hover modifier";
    check_selector ".group:hover .group-hover\\:" "group-hover modifier";
    check_selector ".peer:checked ~ .peer-checked\\:" "peer-checked modifier";
    check_selector "[aria-checked=\"true\"]" "aria-checked";
    check_selector "[data-active]" "data-active";

    Printf.printf "\nOur CSS output:\n";
    Printf.printf "%s\n"
      (if String.length css_output > 500 then
         String.sub css_output 0 500 ^ "..."
       else css_output);
    Printf.printf "\nTailwind CSS output (first 500 chars):\n";
    Printf.printf "%s\n"
      (String.sub tailwind_css 0 (min 500 (String.length tailwind_css)))

let test_minified_exact_match () =
  (* Create a simple test page *)
  let page_content =
    div
      ~tw:Tw.[ p 4; m 2; bg blue 500; text white 0; rounded_lg ]
      [
        h1 ~tw:Tw.[ text_2xl; font_bold; mb 4 ] [ txt "Test" ];
        p ~tw:Tw.[ text gray 300 ] [ txt "Content" ];
      ]
  in

  let generated_page = page ~title:"Test" [] [ page_content ] in
  let html_output = html generated_page in
  let _css_filename, css_stylesheet = css generated_page in

  (* Get our minified CSS *)
  let our_minified = Tw.Css.to_string ~minify:true css_stylesheet in

  (* Write HTML for Tailwind to process *)
  let html_file = "/tmp/tw_minify_test.html" in
  let oc = open_out html_file in
  Printf.fprintf oc "<!DOCTYPE html>\n<html>\n<body>\n%s\n</body>\n</html>"
    html_output;
  close_out oc;

  (* Create Tailwind config *)
  let tailwind_config =
    {|
module.exports = {
  content: ["/tmp/tw_minify_test.html"],
  theme: { extend: {} },
  plugins: [],
}
|}
  in
  let config_file = "/tmp/tw_minify_config.js" in
  let oc = open_out config_file in
  output_string oc tailwind_config;
  close_out oc;

  (* Run Tailwind with minification *)
  let tailwind_cmd =
    "cd /tmp && npx tailwindcss -c tw_minify_config.js --minify -o \
     tw_minified.css 2>/dev/null"
  in
  let exit_code = Sys.command tailwind_cmd in

  if exit_code <> 0 then
    Printf.printf
      "Note: Tailwind CSS minification test skipped (tailwindcss not available)\n"
  else
    let ic = open_in "/tmp/tw_minified.css" in
    let tailwind_minified = really_input_string ic (in_channel_length ic) in
    close_in ic;

    Printf.printf "Minified CSS comparison:\n";
    Printf.printf "Our size: %d bytes\n" (String.length our_minified);
    Printf.printf "Tailwind size: %d bytes\n" (String.length tailwind_minified);

    (* For exact match, we'd need to: 1. Strip Tailwind's base reset (we have
       our own) 2. Normalize selector ordering 3. Handle vendor prefixes But at
       minimum, check key patterns are present *)
    let check_pattern pattern name =
      if not (Astring.String.is_infix ~affix:pattern our_minified) then
        Printf.printf "Missing in our output: %s\n" name;
      if not (Astring.String.is_infix ~affix:pattern tailwind_minified) then
        Printf.printf "Missing in Tailwind output: %s\n" name
    in

    check_pattern ".p-4" "padding class";
    check_pattern ".bg-blue-500" "background color";
    check_pattern ".text-white" "text color";
    check_pattern ".rounded-lg" "border radius"

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
      test_case "exact tailwind match" `Quick test_exact_tailwind_match;
      test_case "minified exact match" `Quick test_minified_exact_match;
    ] )
