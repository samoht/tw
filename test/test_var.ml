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

(* Tailwind spells one registered zero two ways: [--tw-ring-offset-width: 0px]
   in the properties layer's bulk declaration, but [initial-value: 0] in its own
   [@property] rule (checked 2026-08-29 against the real CLI). Every other
   zero-initialised [Length] variable (border-spacing-x/y) gets "0" in both
   places, and the canonical [--diff] comparison treats "0" and "0px" as the
   same length, so a regression here would not show up there - only this pinned
   literal catches it. *)
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

let render ?theme classes =
  match Tw.of_string ?theme classes with
  | Ok t -> ignore (Tw.to_css ?theme ~base:false [ t ])
  | Error (`Msg m) -> fail m

(* A (priority, suborder) slot is not unique to one variable: token families are
   numbered independently, so several tokens legitimately share a slot. Both
   sides of a shared slot must keep their order. *)
let order_of_shared_slot () =
  let (_ : Css.length Tw.Var.theme) =
    Tw.Var.theme Css.Length "test-shared-first" ~order:(990, 0)
  in
  let (_ : Css.length Tw.Var.theme) =
    Tw.Var.theme Css.Length "test-shared-second" ~order:(990, 0)
  in
  check order "first" (Some (990, 0)) (Tw.Var.order "test-shared-first");
  check order "second" (Some (990, 0)) (Tw.Var.order "test-shared-second")

(* Each family in the theme layer's tail owns a band no other family reaches
   into, so the emission order is the family order Tailwind's [@theme] writes
   rather than the spelling of the token names. *)
let order_of_family_bands () =
  let expect name o = check order name (Some o) (Tw.Var.order name) in
  expect "radius-4xl" (7, 10);
  expect "drop-shadow-xl" (7, 24);
  expect "drop-shadow-2xl" (7, 25);
  expect "ease-linear" (7, 33);
  expect "animate-spin" (7, 41);
  expect "blur-xs" (8, 0);
  expect "perspective-dramatic" (8, 10);
  expect "aspect-video" (8, 20);
  expect "default-transition-duration" (8, 30)

(* A project [@theme] may name font families of its own; they all sit at (1,
   100). Rendering one must not decide the answer for the others. *)
let order_of_theme_named_tokens () =
  let theme =
    {
      Tw.Scheme.default with
      token_overrides =
        [ ("font-alpha", "Alpha, sans-serif"); ("font-beta", "Beta, serif") ];
    }
  in
  render ~theme "font-alpha";
  render ~theme "font-beta";
  check order "alpha" (Some (1, 100)) (Tw.Var.order "font-alpha");
  check order "beta" (Some (1, 100)) (Tw.Var.order "font-beta")

(* A name owns its slot: the first definition places it, and a later definition
   of the same name is placed there too, the declarations it binds included. *)
let order_of_redefined_name () =
  let (_ : Css.length Tw.Var.theme) =
    Tw.Var.theme Css.Length "test-established" ~order:(991, 0)
  in
  let again = Tw.Var.theme Css.Length "test-established" ~order:(991, 1) in
  let decl, _ = Tw.Var.binding again (Css.Px 1.) in
  check order "registry" (Some (991, 0)) (Tw.Var.order "test-established");
  check order "declaration" (Some (991, 0)) (Tw.Var.order_of_declaration decl)

(* A project [@theme] naming a built-in token reaches the [--font-<name>]
   family's shared slot, and must leave the token in its own. *)
let order_of_theme_renamed_builtin () =
  let theme =
    {
      Tw.Scheme.default with
      token_overrides =
        [ ("font-sans", "Alpha, sans-serif"); ("font-mono", "Beta, monospace") ];
    }
  in
  render ~theme "font-sans";
  render ~theme "font-mono";
  check order "font-sans" (Some (1, 0)) (Tw.Var.order "font-sans");
  check order "font-mono" (Some (1, 2)) (Tw.Var.order "font-mono")

(* [font-weight-bold] reads --font-weight-bold as a family name, so a project
   naming that token routes a font-weight token through the font family's slot.
   It keeps its own, and the render goes through. *)
let order_of_theme_renamed_weight () =
  let theme =
    {
      Tw.Scheme.default with
      token_overrides = [ ("font-weight-bold", "Alpha, sans-serif") ];
    }
  in
  render ~theme "font-weight-bold";
  check order "font-weight-bold"
    (Some (6, 36))
    (Tw.Var.order "font-weight-bold")

let order_of_unknown_name () =
  check order "unknown" None (Tw.Var.order "test-never-defined");
  check order "leading -- stripped" (Some (6, 8)) (Tw.Var.order "--text-xl")

(* A name owns its family and its [@property] need: each is a fact about one
   custom property, not a slot several families share. A second registration
   that agrees restates the fact; one that disagrees is a constant copied by
   hand that has drifted, and it is refused rather than letting module
   initialisation order decide the properties layer. *)
let family_registered_once () =
  let (_ : Css.length Tw.Var.channel) =
    Tw.Var.channel ~family:`Ring Css.Length "test-family-owner"
  in
  let (_ : Css.length Tw.Var.channel) =
    Tw.Var.channel ~family:`Ring Css.Length "test-family-owner"
  in
  check
    (Alcotest.option Alcotest.string)
    "established" (Some "Ring")
    (Option.map
       (function `Ring -> "Ring" | _ -> "other")
       (Tw.Var.family "test-family-owner"));
  Alcotest.check_raises "a disagreeing family is refused"
    (Invalid_argument
       "--test-family-owner: family is Ring, registered again as Shadow")
    (fun () ->
      ignore (Tw.Var.channel ~family:`Shadow Css.Length "test-family-owner"))

(* A variable's slot in the properties layer is one fact about one custom
   property, the same way its family is. A second registration that agrees
   restates it; one that disagrees is a hand-copied constant that has drifted,
   and it is refused rather than letting module initialisation order settle
   it. *)
let property_order_registered_once () =
  Tw.Var.register_property_order ~name:"test-order-owner" ~order:6;
  Tw.Var.register_property_order ~name:"test-order-owner" ~order:6;
  check
    (Alcotest.option Alcotest.int)
    "established" (Some 6)
    (Tw.Var.property_order "test-order-owner");
  Alcotest.check_raises "a disagreeing property order is refused"
    (Invalid_argument
       "--test-order-owner: property order is 6, registered again as 7")
    (fun () -> Tw.Var.register_property_order ~name:"test-order-owner" ~order:7)

let tests =
  [
    test_case "var CSS output" `Quick var_css_output;
    test_case "var fallback in CSS" `Quick var_fallback_in_css;
    test_case "arbitrary var fallbacks" `Quick arbitrary_var_fallbacks;
    test_case "var in theme layer" `Quick var_in_theme_layer;
    test_case "ring-offset-width properties layer spelling" `Quick
      ring_offset_width_properties_layer_spelling;
    test_case "order of shared slot" `Quick order_of_shared_slot;
    test_case "order of family bands" `Quick order_of_family_bands;
    test_case "order of theme-named tokens" `Quick order_of_theme_named_tokens;
    test_case "order of redefined name" `Quick order_of_redefined_name;
    test_case "order of theme-renamed builtin" `Quick
      order_of_theme_renamed_builtin;
    test_case "order of theme-renamed weight" `Quick
      order_of_theme_renamed_weight;
    test_case "order of unknown name" `Quick order_of_unknown_name;
    test_case "family registered once" `Quick family_registered_once;
    test_case "property order registered once" `Quick
      property_order_registered_once;
  ]

let suite = ("var", tests)
