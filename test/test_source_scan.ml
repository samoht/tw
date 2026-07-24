open Alcotest

let known_classes source =
  Tw_tools.Source_scan.candidates source
  |> List.filter (fun cls -> Result.is_ok (Tw.of_string cls))

let check_strings name expected actual =
  check (list string) name expected actual

let test_split_whitespace () =
  check_strings "split whitespace"
    [ "flex"; "items-center"; "p-4"; "bg-red-500" ]
    (Tw_tools.Source_scan.split_whitespace
       " flex\titems-center\np-4\r\012bg-red-500 ")

let test_scan_html_plain_text () =
  let source =
    {|<a href="/about" title="Home" class="flex
items-center px-2.5 bg-[#0088cc]">Home</a>|}
  in
  check_strings "known classes"
    [ "bg-[#0088cc]"; "flex"; "items-center"; "px-2.5" ]
    (known_classes source)

let test_scan_js_static_classes () =
  let source =
    {|const color = error ? "text-red-600" : 'text-green-600'
const dynamic = `bg-${color}-600`
const classes = ["hover:bg-red-500", condition && "md:p-4"]|}
  in
  check_strings "known classes"
    [ "hover:bg-red-500"; "md:p-4"; "text-green-600"; "text-red-600" ]
    (known_classes source)

let test_scan_utf8_source () =
  let source = {|<div title="café" class="p-4">Привет bg-red-500 你好</div>|} in
  check_strings "known classes" [ "bg-red-500"; "p-4" ] (known_classes source)

let test_tw_str_html_space () =
  check_strings "Tw.str"
    [ "flex"; "items-center"; "p-4" ]
    (Tw.str "flex\titems-center\np-4" |> List.map Tw.pp)

(* A [ that opens an arbitrary value is followed immediately by its content, as
   in [text-[13px]]. A [ followed by whitespace is a plain array bracket, like
   the JS [rows={[ ... ]] in a docs table: consuming it as one candidate would
   swallow every class named inside the array. *)
let test_scan_bracket_before_whitespace () =
  let source =
    {|<ApiTable rows={[
    ["accent-inherit", "accent-color: inherit;"],
    ["accent-current", "accent-color: currentColor;"],
  ]} />|}
  in
  check_strings "classes inside the array survive"
    [ "accent-current"; "accent-inherit" ]
    (known_classes source)

let tests =
  [
    test_case "split whitespace" `Quick test_split_whitespace;
    test_case "scan HTML plain text" `Quick test_scan_html_plain_text;
    test_case "scan JS static classes" `Quick test_scan_js_static_classes;
    test_case "scan UTF-8 source" `Quick test_scan_utf8_source;
    test_case "Tw.str splits HTML whitespace" `Quick test_tw_str_html_space;
    test_case "bracket before whitespace is not a candidate" `Quick
      test_scan_bracket_before_whitespace;
  ]

let suite = ("source_scan", tests)
