module Css = Cascade.Css
open Alcotest

(* Test that @layer theme includes expected vars in stable order *)
let theme_layer_stable_order () =
  let styles = Tw.[ text_xl; text red; p 4 ] in
  let css = Tw.to_css ~base:false styles in
  let theme_layer = Css.layer_block "theme" css in
  match theme_layer with
  | None -> fail "Expected @layer theme to be present"
  | Some statements ->
      let vars = Css.vars_of_rules statements in
      (* Verify variables are sorted and deduplicated *)
      let var_names = List.map Css.any_var_name vars in
      let sorted = List.sort String.compare var_names in
      check (list string) "variables are sorted" sorted var_names;
      (* Verify no duplicates *)
      let unique = List.sort_uniq String.compare var_names in
      check (list string) "variables are unique" unique var_names

(* Test cross-module vars show up when referenced *)
let theme_cross_module_vars () =
  (* Use utilities from different modules that reference theme vars *)
  let styles =
    Tw.
      [
        text_xl;
        (* typography - font size *)
        text blue;
        (* colors *)
        p 4;
        (* spacing *)
      ]
  in
  let css = Tw.to_css ~base:false styles in
  let theme_layer = Css.layer_block "theme" css in
  match theme_layer with
  | None -> fail "Expected @layer theme"
  | Some statements ->
      (* Theme layer contains custom property declarations *)
      let rules = Css.rules_of_statements statements in
      let custom_props = Css.custom_props_of_rules rules in

      (* Verify expected theme variables are declared *)
      check (list string) "theme custom properties"
        [
          "--color-blue-500"; "--spacing"; "--text-xl"; "--text-xl--line-height";
        ]
        (List.sort String.compare custom_props)

let priority_seven_namespace_order () =
  let expected =
    [
      "--radius-3xl";
      "--drop-shadow-sm";
      "--ease-in";
      "--radius-4xl";
      "--drop-shadow-md";
      "--ease-out";
      "--drop-shadow-lg";
      "--ease-in-out";
      "--drop-shadow-xl";
      "--ease-linear";
      "--animate-none";
      "--drop-shadow-2xl";
      "--animate-spin";
      "--perspective-dramatic";
      "--animate-ping";
      "--perspective-near";
      "--animate-pulse";
      "--perspective-normal";
      "--animate-bounce";
      "--perspective-midrange";
    ]
  in
  let theme =
    { Tw.Scheme.default with token_overrides = [ ("animate-none", "none") ] }
  in
  let classes =
    [
      "rounded-3xl";
      "rounded-4xl";
      "drop-shadow-sm";
      "drop-shadow-md";
      "drop-shadow-lg";
      "drop-shadow-xl";
      "drop-shadow-2xl";
      "ease-in";
      "ease-out";
      "ease-in-out";
      "ease-linear";
      "animate-none";
      "animate-spin";
      "animate-ping";
      "animate-pulse";
      "animate-bounce";
      "perspective-dramatic";
      "perspective-near";
      "perspective-normal";
      "perspective-midrange";
    ]
  in
  let styles =
    List.map (fun cls -> Result.get_ok (Tw.of_string ~theme cls)) classes
  in
  let actual =
    match Css.layer_block "theme" (Tw.to_css ~theme ~base:false styles) with
    | None -> fail "Expected @layer theme"
    | Some statements ->
        Css.rules_of_statements statements
        |> Css.custom_props_of_rules
        |> List.filter (fun name -> List.mem name expected)
  in
  check (list string) "Tailwind namespace order at shared slots" expected actual

let tests =
  [
    test_case "theme layer stable order" `Quick theme_layer_stable_order;
    test_case "theme cross-module vars" `Quick theme_cross_module_vars;
    test_case "priority-7 namespace order" `Quick priority_seven_namespace_order;
  ]

let suite = ("theme", tests)
