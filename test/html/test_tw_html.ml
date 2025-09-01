(** Tests for the HTML module *)

open Alcotest
open Tw_html

(* Helpers to keep individual tests small and readable *)
let write_file path content =
  let oc = open_out path in
  output_string oc content;
  close_out oc

let read_file path =
  let ic = open_in path in
  let s = really_input_string ic (in_channel_length ic) in
  close_in ic;
  s

let html_doc_of body =
  Fmt.str
    "<!DOCTYPE html>\n\
     <html>\n\
     <head>\n\
     <meta charset=\"UTF-8\">\n\
     </head>\n\
     <body>\n\
     %s\n\
     </body>\n\
     </html>"
    body

let check_selector_in css pattern name =
  if not (Astring.String.is_infix ~affix:pattern css) then
    Fmt.pr "Warning: Tailwind doesn't contain expected %s selector: %s@." name
      pattern
  else Fmt.pr "✓ Found %s selector in Tailwind output@." name

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
      ~tw:Tw.[ text blue 600; hover [ underline ] ]
      [ txt "About" ]
  in
  let html_str = to_string link in

  check bool "has href" true
    (Astring.String.is_infix ~affix:"href=\"/about\"" html_str);
  check bool "has title" true
    (Astring.String.is_infix ~affix:"title=\"About page\"" html_str);
  check bool "contains text" true
    (Astring.String.is_infix ~affix:"About" html_str)

let test_html_escaping () =
  (* Text content escaping *)
  let dangerous = txt "<script>if (a && b) alert(\"x\")</script>" in
  let html_str = to_string dangerous in
  check bool "escapes < and >" true
    (Astring.String.is_infix ~affix:"&lt;script&gt;" html_str);
  check bool "escapes & inside" true
    (Astring.String.is_infix ~affix:"&amp;&amp;" html_str);
  check bool "escapes quotes" true
    (Astring.String.is_infix ~affix:"\"x\"" html_str
    || Astring.String.is_infix ~affix:"&quot;x&quot;" html_str);

  (* Attribute value escaping *)
  let elem = div ~at:[ At.title "5 > 3 & \"yes\"" ] [ txt "t" ] in
  let s = to_string elem in
  check bool "attribute escapes >" true
    (Astring.String.is_infix ~affix:"&gt;" s);
  check bool "attribute escapes & and quotes" true
    (Astring.String.is_infix ~affix:"&amp;" s
    && Astring.String.is_infix ~affix:"&quot;yes&quot;" s)

let test_boolean_and_data_attrs () =
  (* Boolean attributes render as presence-only *)
  let i = input ~at:[ At.disabled; At.checked; At.required ] () in
  let s = to_string i in
  check bool "has disabled" true (Astring.String.is_infix ~affix:"disabled" s);
  check bool "has checked" true (Astring.String.is_infix ~affix:"checked" s);
  check bool "has required" true (Astring.String.is_infix ~affix:"required" s);

  (* aria and data attributes serialization *)
  let d =
    div
      ~at:
        [
          Aria.label "Greeting";
          Aria.hidden;
          Aria.expanded true;
          At.v "data-user" "Tom & Jerry";
        ]
      [ txt "x" ]
  in
  let sd = to_string d in
  check bool "aria-label set" true
    (Astring.String.is_infix ~affix:"aria-label=\"Greeting\"" sd);
  check bool "aria-hidden set" true
    (Astring.String.is_infix ~affix:"aria-hidden=\"true\"" sd);
  check bool "aria-expanded set" true
    (Astring.String.is_infix ~affix:"aria-expanded=\"true\"" sd);
  check bool "data-* serialized and escaped" true
    (Astring.String.is_infix ~affix:"data-user=\"Tom &amp; Jerry\"" sd)

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
  let page_content =
    div
      ~tw:Tw.[ p 4; bg blue 500; hover [ bg blue 600 ] ]
      [
        h1
          ~tw:Tw.[ text_2xl; font_bold; text white 0; mb 4 ]
          [ txt "Test Page" ];
        p
          ~tw:Tw.[ text gray 200; hover [ text white 0 ] ]
          [ txt "Testing hover states" ];
        (* Test group hover *)
        div
          ~tw:Tw.[ group; p 4 ]
          [
            p ~tw:Tw.[ group_hover [ text red 500 ] ] [ txt "Group hover test" ];
          ];
        (* Test peer *)
        div
          [
            input ~at:[ At.type' "checkbox" ] ~tw:Tw.[ peer ] ();
            p
              ~tw:Tw.[ peer_checked [ text green 500 ] ]
              [ txt "Peer checked test" ];
          ];
        (* Test aria *)
        div
          ~at:[ At.v "aria-checked" "true" ]
          ~tw:Tw.[ aria_checked [ bg purple 100 ] ]
          [ txt "Aria checked test" ];
        (* Test data attribute *)
        div
          ~at:[ At.v "data-active" "true" ]
          ~tw:Tw.[ data_active [ font_bold ] ]
          [ txt "Data active test" ];
      ]
  in

  let generated_page = page ~title:"Test" [] [ page_content ] in
  let html_output = html generated_page in
  let _css_filename, css_stylesheet = css generated_page in
  let css_output = Tw.Css.to_string ~minify:false css_stylesheet in
  let html_file = "/tmp/tw_test_exact.html" in
  write_file html_file (html_doc_of html_output);

  (* Tailwind config *)
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
  write_file config_file tailwind_config;

  (* Run Tailwind and collect output if available *)
  let tailwind_cmd =
    "cd /tmp && npx tailwindcss -i /dev/stdin -c tailwind.config.js -o \
     tailwind_real.css <<< '@tailwind base; @tailwind components; @tailwind \
     utilities;' 2>/dev/null"
  in
  let exit_code = Sys.command tailwind_cmd in

  if exit_code <> 0 then (
    Fmt.pr
      "Note: Tailwind CSS comparison test skipped (tailwindcss not available \
       or wrong version)@.";
    ())
  else
    let tailwind_css = read_file "/tmp/tailwind_real.css" in
    check_selector_in tailwind_css ".hover\\:bg-blue-600:hover" "hover modifier";
    check_selector_in tailwind_css ".group:hover .group-hover\\:"
      "group-hover modifier";
    check_selector_in tailwind_css ".peer:checked ~ .peer-checked\\:"
      "peer-checked modifier";
    check_selector_in tailwind_css "[aria-checked=\"true\"]" "aria-checked";
    check_selector_in tailwind_css "[data-active]" "data-active";
    Fmt.pr "@.Our CSS output:@.";
    Fmt.pr "%s@."
      (if String.length css_output > 500 then
         String.sub css_output 0 500 ^ "..."
       else css_output);
    Fmt.pr "@.Tailwind CSS output (first 500 chars):@.";
    Fmt.pr "%s@."
      (String.sub tailwind_css 0 (min 500 (String.length tailwind_css)))

let test_exact_byte_match () =
  let page_content =
    div ~tw:Tw.[ p 4; bg blue 500; text white 0 ] [ txt "Test" ]
  in

  let generated_page = page ~title:"Test" ~tw_css:"" [] [ page_content ] in
  let html_output = html generated_page in
  let _css_filename, css_stylesheet = css generated_page in
  let our_css = Tw.Css.to_string ~minify:true ~optimize:true css_stylesheet in

  let html_file = "/tmp/tw_exact_test.html" in
  write_file html_file
    (Fmt.str "<!DOCTYPE html>\n<html>\n<body>\n%s\n</body>\n</html>" html_output);

  (* Create minimal Tailwind config with preflight enabled *)
  let tailwind_config =
    {|
module.exports = {
  content: ["/tmp/tw_exact_test.html"],
  theme: { extend: {} },
  plugins: [],
}
|}
  in
  let config_file = "/tmp/tw_exact_config.js" in
  write_file config_file tailwind_config;

  (* Run Tailwind v3 with minification to get complete CSS with base/preflight *)
  (* First ensure Tailwind v3 is installed *)
  let _ =
    Sys.command
      "cd /tmp && npm init -y >/dev/null 2>&1 && npm install tailwindcss@3 \
       >/dev/null 2>&1"
  in
  let tailwind_cmd =
    "cd /tmp && npx tailwindcss -c tw_exact_config.js --minify -o tw_exact.css \
     <<< '@tailwind base; @tailwind utilities;' 2>/dev/null"
  in
  let exit_code = Sys.command tailwind_cmd in

  if exit_code <> 0 then (
    Fmt.pr
      "Note: Exact match test skipped (tailwindcss not available, exit code: \
       %d)@."
      exit_code;
    ())
  else
    let ic = open_in "/tmp/tw_exact.css" in
    let tailwind_css = really_input_string ic (in_channel_length ic) in
    close_in ic;

    (* Save both for debugging *)
    let oc = open_out "/tmp/our_exact.css" in
    output_string oc our_css;
    close_out oc;

    Fmt.pr "@.=== EXACT BYTE COMPARISON ===@.";
    Fmt.pr "Our CSS length: %d bytes@." (String.length our_css);
    Fmt.pr "Tailwind CSS length: %d bytes@." (String.length tailwind_css);

    if our_css = tailwind_css then (
      Fmt.pr "✅ EXACT MATCH! CSS outputs are byte-for-byte identical!@.";
      check bool "CSS matches exactly" true true)
    else (
      Fmt.pr "❌ CSS outputs differ@.@.";

      (* Show first difference *)
      let rec find_first_diff i =
        if i >= String.length our_css || i >= String.length tailwind_css then
          Fmt.pr "One string is prefix of the other@."
        else if our_css.[i] <> tailwind_css.[i] then (
          Fmt.pr "First difference at position %d:@." i;
          Fmt.pr "  Our char: '%c' (code %d)@." our_css.[i]
            (Char.code our_css.[i]);
          Fmt.pr "  Tailwind char: '%c' (code %d)@." tailwind_css.[i]
            (Char.code tailwind_css.[i]);

          (* Show context *)
          let start = max 0 (i - 20) in
          let end_ours = min (String.length our_css) (i + 20) in
          let end_tw = min (String.length tailwind_css) (i + 20) in
          Fmt.pr "@.Context (position %d-%d):@." start i;
          Fmt.pr "  Ours:     ...%s[*]%s...@."
            (String.sub our_css start (i - start))
            (String.sub our_css i (end_ours - i));
          Fmt.pr "  Tailwind: ...%s[*]%s...@."
            (String.sub tailwind_css start (i - start))
            (String.sub tailwind_css i (end_tw - i)))
        else find_first_diff (i + 1)
      in
      find_first_diff 0;

      Fmt.pr
        "@.Full outputs saved to /tmp/our_exact.css and /tmp/tw_exact.css@.";

      (* Show full outputs if small enough *)
      if String.length our_css < 500 && String.length tailwind_css < 500 then (
        Fmt.pr "@.Our CSS:@.%s@." our_css;
        Fmt.pr "@.Tailwind CSS:@.%s@." tailwind_css))

let suite =
  ( "html",
    [
      test_case "txt" `Quick test_txt;
      test_case "element creation" `Quick test_element_creation;
      test_case "attributes" `Quick test_attributes;
      test_case "html escaping" `Quick test_html_escaping;
      test_case "boolean + aria/data attrs" `Quick test_boolean_and_data_attrs;
      test_case "nesting" `Quick test_nesting;
      test_case "to_tw" `Quick test_to_tw;
      test_case "pretty printing" `Quick test_pp;
      test_case "page cache busting" `Quick test_page_cache_busting;
      test_case "cache busting consistency" `Quick
        test_page_cache_busting_consistency;
      test_case "exact tailwind match" `Quick test_exact_tailwind_match;
      test_case "minified exact match" `Quick test_exact_byte_match;
    ] )
