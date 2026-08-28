let check = Test_helpers.check_handler_roundtrip (module Tw.Scrollbar.Handler)

let test_roundtrip () =
  (* scrollbar-width *)
  check "scrollbar-auto";
  check "scrollbar-none";
  check "scrollbar-thin";
  (* scrollbar-gutter *)
  check "scrollbar-gutter-auto";
  check "scrollbar-gutter-stable";
  check "scrollbar-gutter-both";
  (* scrollbar-thumb / -track colours *)
  check "scrollbar-thumb-red-500";
  check "scrollbar-track-gray-200";
  check "scrollbar-thumb-current";
  check "scrollbar-thumb-transparent";
  check "scrollbar-thumb-inherit";
  check "scrollbar-thumb-[#ff0000]"

let test_invalid () =
  Test_helpers.check_invalid_input (module Tw.Scrollbar.Handler) "scrollbar";
  Test_helpers.check_invalid_input (module Tw.Scrollbar.Handler) "scrollbar-foo";
  Test_helpers.check_invalid_input
    (module Tw.Scrollbar.Handler)
    "scrollbar-gutter-foo"

(* [scrollbar-color] reads the two custom properties the same utility declares.
   Both names were written out three times over in one file, so a rename of
   either handle would leave the property referenced but never set, and the
   scrollbar would fall back to the browser's colours. *)
let test_declares_what_it_references () =
  let sheet =
    let classes = [ "scrollbar-thumb-red-500"; "scrollbar-track-gray-200" ] in
    let styles =
      List.map
        (fun cls ->
          match Tw.of_string cls with
          | Ok u -> u
          | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m)
        classes
    in
    Tw.to_css ~base:false styles |> Tw.Css.to_string ~minify:true
  in
  (* Every [--tw-scrollbar-*] name in the sheet, split by whether it is being
     set or read. *)
  let names ~read =
    let prefix = "--tw-scrollbar-" in
    let n = String.length prefix in
    let len = String.length sheet in
    let rec at i acc =
      if i + n > len then List.sort_uniq String.compare acc
      else if String.sub sheet i n <> prefix then at (i + 1) acc
      else
        let rec stop j =
          if j >= len then j
          else match sheet.[j] with 'a' .. 'z' | '-' -> stop (j + 1) | _ -> j
        in
        let e = stop i in
        let is_read = i >= 4 && String.sub sheet (i - 4) 4 = "var(" in
        let name = String.sub sheet i (e - i) in
        at e (if is_read = read then name :: acc else acc)
    in
    at 0 []
  in
  Alcotest.(check (list string))
    "the properties read are the properties set" (names ~read:false)
    (names ~read:true)

(* Tailwind writes [transparent] into the custom property, and the optimizer
   folds a token stream to [#0000]. Spelling the keyword as text therefore
   produced the fold it was written to avoid, and the roundtrip checks above
   never looked at the value. *)
let test_transparent_keeps_its_keyword () =
  let css =
    match Tw.of_string "scrollbar-thumb-transparent" with
    | Ok u ->
        Tw.to_css ~base:false [ u ]
        |> Tw.Css.optimize ~prune_unused_custom_props:true
        |> Tw.Css.to_string ~minify:true
    | Error (`Msg m) -> Alcotest.failf "scrollbar-thumb-transparent: %s" m
  in
  Alcotest.(check bool)
    "the utility keeps the transparent keyword" true
    (Astring.String.is_infix ~affix:"--tw-scrollbar-thumb:transparent" css)

let tests =
  Test_helpers.standard ~roundtrip:test_roundtrip ~invalid:test_invalid
  @ [
      Alcotest.test_case "declares what it references" `Quick
        test_declares_what_it_references;
      Alcotest.test_case "transparent keeps its keyword" `Quick
        test_transparent_keeps_its_keyword;
    ]

let suite = ("scrollbar", tests)
