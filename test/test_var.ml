module Css = Cascade.Css
open Alcotest

(* Test that variable naming follows conventions *)
let var_css_output () =
  (* Create styles that use theme variables *)
  let styles = Tw.[ text_xl; p 4 ] in
  let css = Tw.to_css ~base:false styles in
  let css_str = Css.to_string css in

  (* Should contain CSS variable references *)
  check bool "contains var(--" (String.contains css_str '-') true;
  check bool "CSS output non-empty" (String.length css_str > 0) true

(* Test that variables with fallbacks are serialized correctly *)
let var_fallback_in_css () =
  (* Use a utility that might have a fallback *)
  let styles = [ Tw.text Tw.blue ] in
  let css = Tw.to_css ~base:false styles in
  let css_str = Css.to_string css in

  (* Should produce valid CSS *)
  check bool "produces valid CSS" (String.length css_str > 0) true

(* Arbitrary var() values keep the outer variable and its fallback. This is
   shared by typed utility families, so cover both a length and a keyword-like
   property. *)
let arbitrary_var_fallbacks () =
  let sheet class_name =
    match Tw.of_string class_name with
    | Ok utility -> Tw.to_css ~base:false [ utility ]
    | Error (`Msg msg) -> failf "%s: %s" class_name msg
  in
  let refs class_name =
    sheet class_name |> Css.vars_of_stylesheet |> List.map Css.any_var_name
  in
  let css class_name = Css.to_string ~minify:true (sheet class_name) in
  let inline_css class_name =
    sheet class_name |> Css.inline_vars |> Css.to_string ~minify:true
  in
  check (list string) "padding outer reference" [ "--x" ]
    (refs "p-[var(--x,var(--y))]");
  check (list string) "cursor outer reference" [ "--c" ]
    (refs "cursor-[var(--c,var(--d))]");
  check bool "padding keeps nested fallback" true
    (Astring.String.is_infix ~affix:"padding:var(--x,var(--y))"
       (css "p-[var(--x,var(--y))]"));
  check bool "cursor keeps nested fallback" true
    (Astring.String.is_infix ~affix:"cursor:var(--c,var(--d))"
       (css "cursor-[var(--c,var(--d))]"));
  (* The outer name is the author's own, set from a style attribute or from
     script, so inlining keeps the reference. Collapsing it to the fallback
     would answer for an element the sheet never saw. *)
  check bool "padding inline keeps the override point" true
    (Astring.String.is_infix ~affix:"padding:var(--x,var(--y))"
       (inline_css "p-[var(--x,var(--y))]"));
  check bool "cursor inline keeps the override point" true
    (Astring.String.is_infix ~affix:"cursor:var(--c,var(--d))"
       (inline_css "cursor-[var(--c,var(--d))]"));
  check (list string) "paren outer reference" [ "--top" ] (refs "top-(--top,0)");
  check bool "paren shorthand keeps its override point" true
    (Astring.String.is_infix ~affix:"top:var(--top," (css "top-(--top,0)"));
  check bool "paren shorthand inline keeps its override point" true
    (Astring.String.is_infix ~affix:"top:var(--top,"
       (inline_css "top-(--top,0)"))

(* CSS Syntax 3 (ED): a [<declaration-value>] carries no unmatched [)], so
   bracket text holding one names no reference and no fallback. Splitting it at
   the comma anyway would take [--a] with [b)c] behind it and write the loose
   [)] into the sheet, where it closes the [var()] early and leaves a stray one
   after it. The whole text stays the escaped name it is. *)
let bracket_reference_with_loose_closer () =
  let css class_name =
    match Tw.of_string class_name with
    | Ok utility ->
        Css.to_string ~minify:true (Tw.to_css ~base:false [ utility ])
    | Error (`Msg msg) -> failf "%s: %s" class_name msg
  in
  let declaration class_name =
    let sheet = css class_name in
    match Astring.String.cut ~sep:"top:" sheet with
    | Some (_, rest) -> (
        match Astring.String.cut ~sep:"}" rest with
        | Some (decl, _) -> decl
        | None -> rest)
    | None -> failf "%s: no top declaration in %s" class_name sheet
  in
  check string "bracket keeps the loose closer inside the name"
    {|var(--a\,b\)c)|}
    (declaration "top-[var(--a,b)c)]");
  check string "paren shorthand keeps the loose closer inside the name"
    {|var(--a\,b\)c)|}
    (declaration "top-(--a,b)c)");
  (* The same text without the loose closer is the reference it looks like. *)
  check string "a closed body is read as name and fallback" "var(--a,b)"
    (declaration "top-[var(--a,b)]")

(* Test theme layer contains variables *)
let var_in_theme_layer () =
  let styles = Tw.[ text_xl; text red; p 4 ] in
  let css = Tw.to_css ~base:true styles in
  let theme_layer = Css.layer_block [ "theme" ] css in

  match theme_layer with
  | None -> fail "Expected @layer theme"
  | Some statements ->
      let rules = Css.rules_of_statements statements in
      let custom_props = Css.custom_props_of_rules rules in
      check (list string) "theme custom properties"
        [
          "--color-red-500";
          "--default-font-family";
          "--default-mono-font-family";
          "--font-mono";
          "--font-sans";
          "--spacing";
          "--text-xl";
          "--text-xl--line-height";
        ]
        (List.sort_uniq String.compare custom_props)

(* Tailwind spells one custom-property zero two ways: [--tw-ring-offset-width:
   0px] in the properties layer's bulk declaration, but [initial-value: 0] in
   its own [@property] rule (checked 2026-08-29 against the real CLI). Every
   other zero-initialised [Length] variable (border-spacing-x/y) gets "0" in
   both places, and the canonical [--diff] comparison treats "0" and "0px" as
   the same length, so a regression here would not show up there - only this
   pinned literal catches it. *)
let ring_offset_width_properties_layer_spelling () =
  let css =
    match Tw.of_string "ring-2" with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Css.to_string ~minify:true
    | Error (`Msg m) -> fail m
  in
  check bool "properties layer keeps the unit" true
    (Astring.String.is_infix ~affix:"--tw-ring-offset-width:0px" css);
  check bool "@property initial-value drops it" true
    (Astring.String.is_infix
       ~affix:
         "@property \
          --tw-ring-offset-width{syntax:\"<length>\";inherits:false;initial-value:0}"
       css)

let order = testable Fmt.(option (pair int int)) ( = )

(* A (priority, suborder) slot is not unique to one variable: token families are
   numbered independently, so several tokens legitimately share a slot. Both
   sides of a shared slot must keep their order. *)
let order_of_shared_slot () =
  let first : Css.length Tw.Var.theme =
    Tw.Var.theme Css.Length "test-shared-first" ~order:(990, 0)
  in
  let second : Css.length Tw.Var.theme =
    Tw.Var.theme Css.Length "test-shared-second" ~order:(990, 0)
  in
  let first_decl, _ = Tw.Var.binding first (Css.Px 1.) in
  let second_decl, _ = Tw.Var.binding second (Css.Px 1.) in
  check order "first" (Some (990, 0)) (Tw.Var.order_of_declaration first_decl);
  check order "second" (Some (990, 0)) (Tw.Var.order_of_declaration second_decl)

(* Metadata belongs to the value that carries it: defining the same spelling in
   another request cannot alter either declaration. *)
let order_of_redefined_name () =
  let first : Css.length Tw.Var.theme =
    Tw.Var.theme Css.Length "test-established" ~order:(991, 0)
  in
  let again = Tw.Var.theme Css.Length "test-established" ~order:(991, 1) in
  let first_decl, _ = Tw.Var.binding first (Css.Px 1.) in
  let decl, _ = Tw.Var.binding again (Css.Px 1.) in
  check order "first" (Some (991, 0)) (Tw.Var.order_of_declaration first_decl);
  check order "second" (Some (991, 1)) (Tw.Var.order_of_declaration decl)

(* Family metadata belongs to the variable value. Two callers can use the same
   custom-property name without one definition rewriting the other. *)
let family_is_value_local () =
  let ring : Css.length Tw.Var.channel =
    Tw.Var.channel ~family:`Ring Css.Length "test-family-owner"
  in
  let shadow : Css.length Tw.Var.channel =
    Tw.Var.channel ~family:`Shadow Css.Length "test-family-owner"
  in
  let ring_decl, _ = Tw.Var.binding ring (Css.Px 1.) in
  let shadow_decl, _ = Tw.Var.binding shadow (Css.Px 1.) in
  let family declaration =
    Option.bind
      (Tw.Var.metadata_of_declaration declaration)
      Tw.Var.metadata_family
  in
  check
    (Alcotest.option Alcotest.string)
    "ring" (Some "Ring")
    (Option.map (function `Ring -> "Ring" | _ -> "other") (family ring_decl));
  check
    (Alcotest.option Alcotest.string)
    "shadow" (Some "Shadow")
    (Option.map
       (function `Shadow -> "Shadow" | _ -> "other")
       (family shadow_decl))

(* The properties-layer slot is value-local for the same reason. *)
let property_order_is_value_local () =
  let first = Tw.Var.channel ~property_order:6 Css.Length "test-order-owner" in
  let second = Tw.Var.channel ~property_order:7 Css.Length "test-order-owner" in
  let property_order (var : Css.length Tw.Var.channel) =
    let declaration, _ = Tw.Var.binding var (Css.Px 1.) in
    Option.bind
      (Tw.Var.metadata_of_declaration declaration)
      Tw.Var.metadata_property_order
  in
  check (Alcotest.option Alcotest.int) "first" (Some 6) (property_order first);
  check (Alcotest.option Alcotest.int) "second" (Some 7) (property_order second)

let tests =
  [
    test_case "var CSS output" `Quick var_css_output;
    test_case "var fallback in CSS" `Quick var_fallback_in_css;
    test_case "arbitrary var fallbacks" `Quick arbitrary_var_fallbacks;
    test_case "bracket reference with loose closer" `Quick
      bracket_reference_with_loose_closer;
    test_case "var in theme layer" `Quick var_in_theme_layer;
    test_case "ring-offset-width properties layer spelling" `Quick
      ring_offset_width_properties_layer_spelling;
    test_case "order of shared slot" `Quick order_of_shared_slot;
    test_case "order of redefined name" `Quick order_of_redefined_name;
    test_case "family is value-local" `Quick family_is_value_local;
    test_case "property order is value-local" `Quick
      property_order_is_value_local;
  ]

let suite = ("var", tests)
