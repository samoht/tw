(* What [Parity_compare] hands the browser, and what it refuses to. *)

open Alcotest

let page = "<!doctype html><div class=\"p-4 hover:underline\"></div>"

let test_uncovered_names_what_the_page_lacks () =
  check (list string) "the class the page does not carry" [ "m-2" ]
    (Tw_tools.Parity_compare.uncovered ~html:page
       [ "p-4"; "m-2"; "hover:underline" ])

(* A refusal happens before any browser is looked for, so it holds on a machine
   with none. *)
let test_browser_refuses_an_uncovered_class () =
  match
    Tw_tools.Parity_compare.browser ~html:page ~classes:[ "m-2" ]
      ~tailwind:".m-2{margin:.5rem}" ~tw:".m-2{margin:.5rem}"
  with
  | Error reason ->
      check bool "the reason names the class" true
        (Astring.String.is_infix ~affix:"m-2" reason)
  | Ok _ -> fail "an uncovered class was compared"

(* The two sheets reach the browser as the two sides: a declaration changed on
   tw's is reported on the property it writes. *)
let test_browser_reports_a_changed_declaration () =
  match (Browser.node_binary (), Browser.chrome_binary ()) with
  | (None, _ | _, None) when Sys.getenv_opt "TW_BROWSER_TESTS" = Some "1" ->
      fail "TW_BROWSER_TESTS=1 but no node or headless Chromium"
  | None, _ | _, None -> skip ()
  | Some _, Some _ -> (
      match
        Tw_tools.Parity_compare.browser ~html:page ~classes:[ "p-4" ]
          ~tailwind:".p-4{padding:1rem}" ~tw:".p-4{padding:2rem}"
      with
      | Error reason -> fail reason
      | Ok report ->
          check bool "padding-top differs" true
            (List.exists
               (fun (d : Browser_compare.difference) ->
                 String.equal d.property "padding-top"
                 && String.equal d.first "16px"
                 && String.equal d.second "32px")
               report.differences))

let suite =
  ( "parity_compare",
    [
      test_case "uncovered names what the page lacks" `Quick
        test_uncovered_names_what_the_page_lacks;
      test_case "browser refuses an uncovered class" `Quick
        test_browser_refuses_an_uncovered_class;
      test_case "browser reports a changed declaration" `Slow
        test_browser_reports_a_changed_declaration;
    ] )
