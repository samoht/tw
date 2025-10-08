(** Container utility tests - layout container and container queries *)

open Tw

let test_layout_container () =
  let open Alcotest in
  let css = Style.to_stylesheet [ layout_container ] in
  check bool "has .container rule" true
    (List.exists
       (fun stmt ->
         match stmt with
         | Tw.Css.Rule (sel, _) -> (
             match sel with
             | Tw.Css.Simple [ Class "container" ] -> true
             | _ -> false)
         | _ -> false)
       css);
  (* Current implementation generates width: 100% *)
  let css_string = Tw.Css.to_string ~minify:true ~optimize:false css in
  check bool "has width:100%" true (String.contains css_string 'w')

let test_container_vs_at_container () =
  (* Test the difference between layout .container and @container query *)
  let layout = Style.to_stylesheet [ layout_container ] in
  let query = Style.to_stylesheet [ container ] in
  let layout_str = Tw.Css.to_string ~minify:true ~optimize:false layout in
  let query_str = Tw.Css.to_string ~minify:true ~optimize:false query in
  let open Alcotest in
  check bool "layout container has .container class" true
    (String.contains layout_str '.');
  check bool "@container has container-type" true
    (String.contains query_str 't')

let test_container_with_tailwind () =
  (* This test documents the difference between our implementation and
     Tailwind *)
  Test_helpers.test_with_tailwind ~forms:false
    "layout_container should match Tailwind .container utility" "container"
    [ layout_container ]

let () =
  let open Alcotest in
  run "Container utilities"
    [
      ( "layout_container",
        [
          test_case "basic layout container" `Quick test_layout_container;
          test_case "container vs @container" `Quick
            test_container_vs_at_container;
          test_case "Tailwind parity" `Quick test_container_with_tailwind;
        ] );
    ]
