open Alcotest
module Css = Cascade.Css

let check = Test_helpers.check_handler_roundtrip (module Tw.Forms.Handler)
let check_select = Test_helpers.check_handler_roundtrip (module Tw.Forms.Select)

let test_inputs () =
  check "form-input";
  check "form-checkbox"

let test_selects () =
  check_select "form-select";
  check_select "form-textarea";
  check_select "form-multiselect"

let test_of_string_invalid () =
  let test_invalid =
    Test_helpers.check_invalid_parts ~why:Test_helpers.Not_a_utility
      (module Tw.Forms.Handler)
  in

  test_invalid [ "form" ];
  test_invalid [ "form"; "invalid" ];
  test_invalid [ "form"; "button" ];

  test_invalid [ "input" ];
  test_invalid [ "checkbox" ];
  test_invalid [ "form"; "input"; "extra" ];
  test_invalid []

let suborder_matches_tailwind () =
  let open Tw in
  let shuffled =
    Test_helpers.shuffle
      [
        form_input;
        form_checkbox;
        form_radio;
        form_select;
        form_textarea;
        form_multiselect;
      ]
  in
  Test_helpers.check_ordering_matches ~forms:true
    ~test_name:"forms suborder matches Tailwind" shuffled

(* Tailwind writes the unprefixed property and leaves the -webkit- spelling to
   Lightning, which prefixes every [exact] and leaves [unset] alone. Its base
   rules and its .form-* utilities carry the same declarations, so the pair
   shows up under either strategy. *)
let spellings_of css selector =
  Css.fold
    (fun acc stmt ->
      match Css.as_rule stmt with
      | Some (sel, decls, _) when Css.Selector.to_string sel = selector ->
          acc
          @ List.filter_map
              (fun decl ->
                let name = Css.Declaration.property_name decl in
                if String.ends_with ~suffix:"print-color-adjust" name then
                  Some name
                else None)
              decls
      | _ -> acc)
    [] css

let prefixed = [ "-webkit-print-color-adjust"; "print-color-adjust" ]

let check_spellings css selector expected =
  Alcotest.(check (list string)) selector expected (spellings_of css selector)

let base_strategy_spellings () =
  let css = Tw.to_css ~base:true ~forms:true [] in
  check_spellings css "select" prefixed;
  check_spellings css
    "input:where([type=\"checkbox\"]), input:where([type=\"radio\"])" prefixed;
  check_spellings css
    "select:where([multiple]), select:where([size]:not([size=\"1\"]))"
    [ "print-color-adjust" ]

let class_strategy_spellings () =
  let css =
    Tw.to_css ~base:false Tw.[ form_checkbox; form_radio; form_select ]
  in
  check_spellings css ".form-checkbox" prefixed;
  check_spellings css ".form-radio" prefixed;
  check_spellings css ".form-select" prefixed;
  check_spellings css ".form-select:where([size]:not([size=\"1\"]))"
    [ "print-color-adjust" ]

let tests =
  [
    test_case "inputs" `Quick test_inputs;
    test_case "selects" `Quick test_selects;
    test_case "of_string invalid cases" `Quick test_of_string_invalid;
    test_case "forms suborder matches Tailwind" `Quick suborder_matches_tailwind;
    test_case "base strategy spells print-color-adjust twice" `Quick
      base_strategy_spellings;
    test_case "class strategy spells print-color-adjust twice" `Quick
      class_strategy_spellings;
  ]

let suite = ("forms", tests)
