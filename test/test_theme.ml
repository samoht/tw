open Alcotest

(* Test that @layer theme includes expected vars in stable order *)
let theme_layer_stable_order () =
  let styles = [ Tw.text_xl; Tw.text_red; Tw.p 4 ] in
  let css = Tw.to_css ~base:false ~optimize:false styles in
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
    [
      Tw.text_xl;
      (* typography - font size *)
      Tw.text_blue;
      (* colors *)
      Tw.p 4;
      (* spacing *)
    ]
  in
  let css = Tw.to_css ~base:false ~optimize:false styles in
  let theme_layer = Css.layer_block "theme" css in
  match theme_layer with
  | None -> fail "Expected @layer theme"
  | Some statements ->
      (* Theme layer contains custom property declarations *)
      let rules = Css.rules_from_statements statements in
      let custom_props = Css.custom_props_from_rules rules in

      (* Verify expected theme variables are declared *)
      check (list string) "theme custom properties"
        [
          "--color-blue-500";
          "--default-font-family";
          "--default-mono-font-family";
          "--font-mono";
          "--font-sans";
          "--spacing";
          "--text-xl";
          "--text-xl--line-height";
        ]
        (List.sort String.compare custom_props)

let tests =
  [
    test_case "theme layer stable order" `Quick theme_layer_stable_order;
    test_case "theme cross-module vars" `Quick theme_cross_module_vars;
  ]

let suite = ("theme", tests)
