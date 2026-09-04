module Css = Cascade.Css
open Alcotest
open Tw.Style
open Tw.Modifiers
open Tw.Padding
open Tw.Margin
open Tw.Color
open Tw.Backgrounds
open Tw.Grid_template
open Tw.Animations
open Tw.Transitions
open Tw.Borders

(* A variant decides the selector and the at-rules wrapped around it, and
   neither is a declaration, so what these tests have to compare is the sheet
   itself. Comparing it whole is what a substring cannot do: [:has(:focus)] is
   an infix of [:has(:focus-visible)], and a [check bool] failure prints neither
   the class nor the CSS. *)
let sheet cls =
  match Tw.of_string cls with
  | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string ~minify:true
  | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m

let check_sheet cls expected = Alcotest.(check string) cls expected (sheet cls)

(* Most classes here draw on no theme token, so the layers ahead of [utilities]
   are empty and only the utilities layer is worth spelling out. The comparison
   is still against the whole sheet. *)
let check_utilities cls expected =
  check_sheet cls
    ("@layer theme,components,utilities;@layer theme;@layer components;@layer \
      utilities{" ^ expected ^ "}")

(* Test responsive modifier detection *)
let test_has_responsive_modifier () =
  (* Basic styles should not have responsive modifier *)
  let style1 = p 4 in
  check bool "no responsive on basic style" false
    (has_responsive_modifier style1);

  (* Responsive styles should be detected *)
  let style2 = sm [ p 4 ] in
  check bool "has responsive (sm)" true (has_responsive_modifier style2);

  let style3 = md [ bg blue ] in
  check bool "has responsive (md)" true (has_responsive_modifier style3);

  (* Nested modifiers with responsive should be detected *)
  let style4 = hover [ sm [ bg blue ] ] in
  check bool "nested has responsive" true (has_responsive_modifier style4);

  (* Non-responsive modifiers should not be detected as responsive *)
  let style5 = hover [ p 4 ] in
  check bool "hover is not responsive" false (has_responsive_modifier style5);

  (* Group with at least one responsive should be detected *)
  let style6 = Tw.Utility.Group [ p 4; sm [ m 2 ] ] in
  check bool "group with responsive" true (has_responsive_modifier style6);

  (* Group without responsive should not be detected *)
  let style7 = Tw.Utility.Group [ p 4; m 2 ] in
  check bool "group without responsive" false (has_responsive_modifier style7)

(* Test responsive nesting validation *)
let test_validate_no_nested_responsive () =
  (* Should pass for non-responsive styles *)
  let () = validate_no_nested_responsive [ p 4; m 2 ] in

  (* Should pass for single styles without responsive modifiers *)
  let () = validate_no_nested_responsive [ p 4 ] in

  (* Should fail for nested responsive *)
  let test_nested_fail () =
    try
      validate_no_nested_responsive [ sm [ md [ p 4 ] ] ];
      fail "Should have raised exception for nested responsive"
    with Failure _ -> ()
  in
  test_nested_fail ();

  (* Should fail for already responsive styles *)
  let test_already_responsive () =
    try
      let responsive_style = sm [ p 4 ] in
      validate_no_nested_responsive [ responsive_style ];
      fail "Should have rejected responsive style"
    with Failure _ -> () (* Expected to fail *)
  in
  test_already_responsive ()

(* Helper to test that a responsive function rejects nested responsive *)
let test_responsive_rejects name outer_fn inner_content =
  try
    let _ = outer_fn [ inner_content ] in
    Alcotest.failf "%s should reject nested responsive" name
  with Failure _ -> () (* Expected to fail *)

(* Test that responsive functions reject nested responsive *)
let test_responsive_functions_reject_nesting () =
  (* Test each breakpoint rejects nested responsive modifiers *)
  test_responsive_rejects "sm" sm (md [ p 4 ]);
  test_responsive_rejects "md" md (lg [ p 4 ]);
  test_responsive_rejects "lg" lg (xl [ p 4 ]);
  test_responsive_rejects "xl" xl (xl2 [ p 4 ]);
  test_responsive_rejects "xl2" xl2 (sm [ p 4 ])

(* Test apply function *)
let test_apply () =
  (* Test single modifier *)
  let style1 = apply [ "hover" ] (p 4) in
  check bool "hover modifier applied" true
    (match style1 with
    | Some (Group [ Modified (Hover, _) ]) -> true
    | Some (Modified (Hover, _)) -> true
    | _ -> false);

  (* Test multiple modifiers - "sm:hover:..." means sm is outermost, hover is
     inner. The structure is Modified(Sm, Modified(Hover, base)) which generates
     "sm:hover:..." *)
  let style2 = apply [ "sm"; "hover" ] (bg blue) in
  check bool "multiple modifiers applied" true
    (match style2 with
    | Some (Group [ Modified (Responsive `Sm, Modified (Hover, _)) ]) -> true
    | Some (Modified (Responsive `Sm, Modified (Hover, _))) -> true
    | _ -> false);

  (* Test unknown modifier (should reject the entire class) *)
  let style3 = apply [ "unknown"; "hover" ] (p 4) in
  check bool "unknown modifier rejects class" true
    (match style3 with None -> true | _ -> false);

  (* Test responsive modifiers *)
  let style4 = apply [ "md" ] (m 2) in
  check bool "md modifier applied" true
    (match style4 with
    | Some (Group [ Modified (Responsive `Md, _) ]) -> true
    | Some (Modified (Responsive `Md, _)) -> true
    | _ -> false);

  (* Test dark mode *)
  let style5 = apply [ "dark" ] (bg ~shade:900 gray) in
  check bool "dark modifier applied" true
    (match style5 with
    | Some (Group [ Modified (Dark, _) ]) -> true
    | Some (Modified (Dark, _)) -> true
    | _ -> false);

  (* Test modifier order - modifiers are applied so that the first modifier in
     the list becomes the outermost wrapper. This matches how string parsing
     works: "hover:sm:p-4" has modifiers ["hover"; "sm"], and produces
     Modified(Hover, Modified(Sm, base)) which generates "hover:sm:p-4". Note:
     In practice, Tailwind expects responsive modifiers like "sm" to come before
     state modifiers like "hover", so valid classes are "sm:hover:p-4". *)
  let style6 = apply [ "hover"; "sm" ] (p 4) in
  check bool "modifier order correct" true
    (match style6 with
    | Some (Group [ Modified (Hover, Modified (Responsive `Sm, _)) ]) -> true
    | Some (Modified (Hover, Modified (Responsive `Sm, _))) -> true
    | _ -> false)

(* Test modifier class name format *)
let test_modifier_class_names () =
  (* Test responsive modifiers produce single colon *)
  check string "sm: single colon" "sm:p-4" (Tw.Utility.to_class (sm [ p 4 ]));

  check string "md: single colon" "md:grid-cols-2"
    (Tw.Utility.to_class (md [ grid_cols 2 ]));

  check string "lg: single colon" "lg:bg-blue-500"
    (Tw.Utility.to_class (lg [ bg blue ]));

  (* 2xl prefix formatting *)
  check string "2xl: single colon" "2xl:p-4" (Tw.Utility.to_class (xl2 [ p 4 ]));

  (* Test hover modifier *)
  check string "hover: single colon" "hover:p-4"
    (Tw.Utility.to_class (hover [ p 4 ]));

  (* Test combined modifiers *)
  check string "md:hover: single colons" "md:hover:m-2"
    (Tw.Utility.to_class (md [ hover [ m 2 ] ]));

  (* Test multiple utilities with modifiers in a list *)
  let classes =
    Tw.to_classes Tw.[ grid; grid_cols 5; md [ grid_cols 10 ]; gap 2 ]
  in
  check string "multiple utilities with md:"
    "grid grid-cols-5 md:grid-cols-10 gap-2" classes

(* Test media preference modifiers class names *)
let test_media_preference_modifiers () =
  (* Motion preference modifiers *)
  check string "motion-safe: single colon" "motion-safe:animate-pulse"
    (Tw.Utility.to_class (motion_safe [ animate_pulse ]));

  check string "motion-reduce: single colon" "motion-reduce:transition-none"
    (Tw.Utility.to_class (motion_reduce [ transition_none ]));

  (* Contrast preference modifiers *)
  check string "contrast-more: single colon" "contrast-more:border-4"
    (Tw.Utility.to_class (contrast_more [ border_4 ]));

  check string "contrast-less: single colon" "contrast-less:text-gray-600"
    (Tw.Utility.to_class (contrast_less [ text ~shade:600 gray ]));

  (* Dark mode *)
  check string "dark: single colon" "dark:bg-gray-900"
    (Tw.Utility.to_class (dark [ bg ~shade:900 gray ]))

(* Test CSS generation and parsing roundtrip for modifiers *)
let test_modifier_css_roundtrip () =
  let test_utilities =
    [
      motion_safe [ animate_pulse ];
      motion_reduce [ transition_none ];
      contrast_more [ border_4 ];
      contrast_more [ text black ];
      contrast_less [ text ~shade:600 gray ];
      dark [ bg ~shade:900 gray ];
      hover [ bg blue ];
      sm [ p 4 ];
    ]
  in

  (* Generate CSS *)
  let stylesheet = Tw.Build.to_css test_utilities in
  let css_str = Tw.Css.to_string ~minify:true stylesheet in

  (* Verify CSS was generated *)
  check bool "CSS generated" true (String.length css_str > 0);

  (* Parse it back - this would fail with double-backslash bug *)
  match Tw.Css.of_string css_str with
  | Ok _parsed_stylesheet ->
      (* Successfully parsed our own generated CSS *)
      ()
  | Error parse_err ->
      let error_msg = Cascade.Error.to_string parse_err in
      Alcotest.failf "Failed to parse generated CSS:\n%s" error_msg

(* Test that generated CSS has correct selector escaping *)
let test_selector_escaping_in_css () =
  (* Generate CSS with modifiers that need escaping *)
  let stylesheet = Tw.Build.to_css [ motion_safe [ animate_pulse ] ] in
  let css_str = Tw.Css.to_string ~minify:true stylesheet in

  (* Verify single backslash in output (not double) *)
  (* In the CSS string, we expect: .motion-safe\:animate-pulse *)
  (* Which appears as "motion-safe\\:animate-pulse" in OCaml string *)
  check bool "CSS contains escaped colon" true (String.contains css_str '\\');

  (* Count backslashes - should be exactly 1 per modifier *)
  let backslash_count =
    String.fold_left (fun n c -> if c = '\\' then n + 1 else n) 0 css_str
  in
  (* We expect 1 backslash for the motion-safe: prefix *)
  check bool "Single backslash escape (not double)" true (backslash_count >= 1);

  (* Verify it parses correctly *)
  match Tw.Css.of_string css_str with
  | Ok _ -> ()
  | Error e ->
      Alcotest.failf "Selector escaping broken - parse failed:\n%s"
        (Cascade.Error.to_string e)

(* Test combined modifiers with media preferences *)
let test_combined_media_modifiers () =
  (* Combining responsive with media preference should work *)
  check string "sm:motion-safe: works" "sm:motion-safe:animate-pulse"
    (Tw.Utility.to_class (sm [ motion_safe [ animate_pulse ] ]));

  check string "md:dark: works" "md:dark:bg-gray-900"
    (Tw.Utility.to_class (md [ dark [ bg ~shade:900 gray ] ]));

  (* Generate and parse CSS with combined modifiers *)
  let utilities =
    [ sm [ motion_safe [ animate_pulse ] ]; md [ dark [ bg ~shade:900 gray ] ] ]
  in
  let css_str = Tw.Css.to_string ~minify:true (Tw.Build.to_css utilities) in
  match Tw.Css.of_string css_str with
  | Ok _ -> ()
  | Error e ->
      Alcotest.failf "Combined modifiers CSS roundtrip failed:\n%s"
        (Cascade.Error.to_string e)

(* Media query behavior for md [...] *)

(* Test that motion-reduce:transition-none outputs transition-property: none *)
let test_motion_reduce_transition_none () =
  (* Tailwind v4 writes transition-property: none, not the transition shorthand
     with a zero duration; the whole rule is pinned, so the shorthand cannot
     come back alongside it. *)
  check_utilities "motion-reduce:transition-none"
    {|@media(prefers-reduced-motion:reduce){.motion-reduce\:transition-none{transition-property:none}}|}

(* The class a modifier renders has to be the class it was read from. This is
   the cheap guard on the two spellings staying merged: the class name comes
   from Style.pp_modifier and the selector from the table Modifiers exposes, and
   while those were two tables they disagreed on nine constructors without
   anything failing. Both directions matter - Style is the right arm for the
   supports- shorthand and not-[...], Modifiers' was for not-* and the data
   attributes. *)
let test_modifier_class_roundtrip () =
  List.iter
    (fun cls ->
      match Tw.of_string cls with
      | Error (`Msg m) -> Alcotest.failf "%s rejected: %s" cls m
      | Ok u ->
          Alcotest.(check string)
            (cls ^ " round-trips") cls (Tw.to_classes [ u ]))
    [
      "hover:p-4";
      "not-hover:p-4";
      "not-focus:p-4";
      "not-sm:p-4";
      "not-[.foo]:p-4";
      "data-[state=open]:flex";
      "data-[variant=ghost]:flex";
      "data-[foo=bar]:flex";
      "data-foo:flex";
      "data-active:p-1";
      "supports-grid:flex";
      "supports-[display:grid]:flex";
      "has-[:focus]:border-2";
      "has-checked:flex";
      "group-has-[:focus]:flex";
      "peer-checked:bg-blue-500";
      "aria-checked:p-4";
      "aria-[sort=ascending]:p-4";
      "min-[320px]:flex";
      "max-[48rem]:flex";
      "@sm:flex";
      "@[600px]:flex";
      "dark:hover:bg-gray-800";
      "md:focus:outline-none";
      "in-[.parent]:flex";
      "before:block";
      "marker:text-gray-500";
    ]

(* [min-[<px>]] and [max-[<px>]] name themselves after the bracket, so the
   bracket has to come back out spelled as the author wrote it. Re-printing the
   parsed number drops a trailing zero, a leading zero and an exponent, and the
   selector then matches nothing in the markup. *)
let test_arbitrary_breakpoint_spelling () =
  List.iter
    (fun cls ->
      match Tw.of_string cls with
      | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
      | Ok u -> Alcotest.(check string) (cls ^ " round-trips") cls (Tw.pp u))
    [
      "min-[320px]:flex";
      "min-[600.50px]:flex";
      "min-[0600px]:flex";
      "min-[1e3px]:flex";
      "min-[600]:flex";
      "min-[+600px]:flex";
      "min-[.5rem]:flex";
      "max-[48rem]:flex";
      "max-[37.50px]:flex";
      (* Units with no arm of their own in [compact_length] fell through to
         cascade's printer, which drops a leading zero, so the selector named a
         class the author never wrote. *)
      "min-[0.5ch]:flex";
      "min-[0.5vmin]:flex";
      "min-[0.5cqw]:flex";
      "min-[0.5lh]:flex";
      "max-[0.5ex]:flex";
      (* The length reader stops at the first thing it cannot use, so a value
         with a remainder came back as the prefix alone: [min-[1px/*x]] named
         [.min-\[1px\]]. *)
      "min-[1px/*x]:flex";
      "max-[1px/*x]:flex";
      "min-[0.5rem]:flex";
    ]

(* [nth-3] and [nth-[3]] are two spellings of one selector and two different
   classes, so the one the author wrote has to come back out. Deciding the
   bracket again when printing, on whether the expression is a bare number,
   named [nth-[3]:p-4] as [.nth-3\\:p-4] and the rule matched nothing. *)
let test_nth_spelling () =
  List.iter
    (fun cls ->
      match Tw.of_string cls with
      | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
      | Ok u -> Alcotest.(check string) (cls ^ " round-trips") cls (Tw.pp u))
    [
      "nth-3:flex";
      "nth-[3]:flex";
      "nth-[2n+1]:flex";
      "nth-last-2:flex";
      "nth-last-[2]:flex";
      "nth-of-type-3:flex";
      "nth-of-type-[3]:flex";
      "nth-last-of-type-4:flex";
      "nth-last-of-type-[4]:flex";
      "not-nth-3:flex";
      "not-nth-[3]:flex";
    ]

(* The bracket holds a length, so a word is not a breakpoint at all. *)
let test_arbitrary_breakpoint_rejects_non_length () =
  List.iter
    (fun cls ->
      match Tw.of_string cls with
      | Ok u -> Alcotest.failf "%s parsed as %s" cls (Tw.pp u)
      | Error (`Msg _) -> ())
    [ "min-[abc]:flex"; "max-[abc]:flex"; "min-[]:flex" ]

(* The number in front of the unit is a CSS number, not an OCaml one. Reading it
   with [float_of_string_opt] turned [min-[0x600px]] into a live [(min-width:
   1536px)] breakpoint: a query the browser honours, built out of a value
   Tailwind passes through verbatim for the browser to drop. Nothing here is a
   length either, so tw declines the whole class. *)
let test_arbitrary_breakpoint_rejects_ocaml_literals () =
  List.iter
    (fun cls ->
      match Tw.of_string cls with
      | Ok u ->
          Alcotest.failf "%s parsed as %s emitting %s" cls (Tw.pp u)
            (Tw.to_css ~base:false [ u ] |> Tw.Css.to_string ~minify:true)
      | Error (`Msg _) -> ())
    [
      "min-[0x600px]:p-4";
      "max-[0x600px]:p-4";
      "min-[0x600]:p-4";
      "min-[0o17px]:p-4";
      "min-[1_000px]:p-4";
    ]

(* A [@custom-variant] belongs to the [@theme] block that declared it. Two
   stylesheets built in one process read different themes, so a variant the
   first declared must be unknown to the second. *)
let test_custom_variant_is_theme_local () =
  let declaring : Tw.Scheme.t =
    {
      Tw.Scheme.default with
      custom_variants =
        [
          ( "is-data",
            Tw.Scheme.{ values = [ ("", "&[data-x]") ]; template = "&:is({})" }
          );
        ];
    }
  in
  check bool "the declaring theme resolves its own variant" true
    (Result.is_ok (Tw.of_string ~theme:declaring "is-data:flex"));
  check bool "a second theme does not see it" true
    (Result.is_error (Tw.of_string ~theme:Tw.Scheme.default "is-data:flex"))

(* Same for a [@custom-variant] whose body is a container query: it is held in
   its own field, and belongs to its own theme just the same. *)
let test_container_variant_is_theme_local () =
  let declaring : Tw.Scheme.t =
    {
      Tw.Scheme.default with
      container_variants = [ ("has-a", Css.Container.of_string "style(--a)") ];
    }
  in
  check bool "the declaring theme resolves its own variant" true
    (Result.is_ok (Tw.of_string ~theme:declaring "has-a:flex"));
  check bool "a second theme does not see it" true
    (Result.is_error (Tw.of_string ~theme:Tw.Scheme.default "has-a:flex"))

(* A [@theme] block that clears [--breakpoint-*] takes the built-in responsive
   variants with it: [md:] then names a breakpoint the project no longer has,
   and the candidate stops resolving the way an unknown variant does. The
   [min-], [max-] and [not-] spellings read the same breakpoint, so they go too,
   while a breakpoint the block declared for itself still resolves. *)
let test_removed_breakpoint_drops_its_variants () =
  let theme =
    Tw.Scheme.with_overrides Tw.Scheme.default
      [ ("breakpoint-*", "initial"); ("breakpoint-tablet", "40rem") ]
  in
  List.iter
    (fun cls ->
      check bool
        (cls ^ " names a breakpoint the theme removed")
        true
        (Result.is_error (Tw.of_string ~theme cls)))
    [ "md:flex"; "min-md:flex"; "max-md:flex"; "not-md:flex" ];
  check bool "the theme's own breakpoint still resolves" true
    (Result.is_ok (Tw.of_string ~theme "tablet:flex"))

(* The variant cascade the one table defines, read as a ladder: every token
   sorts strictly after the one before it. A token the table has no position for
   returns 0, and the comparator reads 0 as "this rule carries no variant" and
   puts the rule in another group entirely, so a missing rung is not a small
   ordering error. *)
let variant_cascade_ladder =
  [
    "*";
    "**";
    "not-hover";
    "group-hover";
    "peer-focus";
    "first-letter";
    "marker";
    "selection";
    "file";
    "placeholder";
    "backdrop";
    "details-content";
    "before";
    "after";
    "first";
    "last";
    "only";
    "odd";
    "even";
    "first-of-type";
    "last-of-type";
    "only-of-type";
    "visited";
    "target";
    "open";
    "default";
    "checked";
    "indeterminate";
    "placeholder-shown";
    "autofill";
    "optional";
    "required";
    "valid";
    "invalid";
    "user-valid";
    "user-invalid";
    "in-range";
    "out-of-range";
    "read-only";
    "read-write";
    "empty";
    "focus-within";
    "hover";
    "focus";
    "focus-visible";
    "active";
    "enabled";
    "disabled";
    "inert";
    "in-focus";
    "has-checked";
    "aria-checked";
    "aria-[modal]";
    "data-active";
    "data-[open]";
    "nth-3";
    "nth-last-3";
    "nth-of-type-3";
    "nth-last-of-type-3";
    "hocus";
    "supports-grid";
    "motion-safe";
    "motion-reduce";
    "contrast-more";
    "contrast-less";
    "md";
    "@md";
    "portrait";
    "landscape";
    "ltr";
    "rtl";
    "dark";
    "starting";
    "print";
    "forced-colors";
    "inverted-colors";
    "pointer-fine";
    "any-pointer-fine";
    "noscript";
    "prose-h1";
    "[&>*]";
  ]

let test_variant_cascade_ladder () =
  List.iter
    (fun token ->
      check bool
        (token ^ " has a position in the cascade")
        true
        (variant_order_of_prefix token > 0))
    variant_cascade_ladder;
  List.iteri
    (fun i token ->
      match List.nth_opt variant_cascade_ladder (i + 1) with
      | None -> ()
      | Some next ->
          check bool
            (token ^ " sorts before " ^ next)
            true
            (variant_order_of_prefix token < variant_order_of_prefix next))
    variant_cascade_ladder

(* Two tokens in the same position are separated by what they carry: a [not-]
   sorts where the variant it negates sorts, a [group-]/[peer-] where the state
   it wraps sorts. Both read the same table as the position itself. *)
let test_variant_inner_order () =
  List.iter
    (fun (token, inner) ->
      check int
        (token ^ " carries " ^ inner)
        (variant_order_of_prefix inner)
        (variant_inner_order token))
    [
      ("not-hover", "hover");
      ("not-md", "md");
      ("not-supports-grid", "supports-grid");
      ("group-focus", "focus");
      ("peer-checked", "checked");
    ];
  check int "a plain token carries nothing" 0 (variant_inner_order "hover")

(* Test suite *)
(* The [!] prefix marks the utility's own declarations !important, leaves theme
   tokens (--spacing) normal, preserves the class name, and nests under a
   modifier (md:!flex). *)
let test_important_prefix () =
  check_utilities "!flex" {|.\!flex{display:flex!important}|};
  (match Tw.of_string "!flex" with
  | Ok u -> Alcotest.(check string) "!flex class round-trips" "!flex" (Tw.pp u)
  | Error (`Msg m) -> Alcotest.fail m);
  (match Tw.of_string "md:!flex" with
  | Ok u ->
      Alcotest.(check string) "md:!flex class round-trips" "md:!flex" (Tw.pp u)
  | Error (`Msg m) -> Alcotest.fail m);
  (* v4 trailing form keeps the suffix in the class name *)
  check_utilities "flex!" {|.flex\!{display:flex!important}|};
  (match Tw.of_string "flex!" with
  | Ok u -> Alcotest.(check string) "flex! class round-trips" "flex!" (Tw.pp u)
  | Error (`Msg m) -> Alcotest.fail m);
  (* the theme binding !p-4 drags in stays normal: the whole sheet is compared,
     so an [!important] appearing on it would fail here *)
  check_sheet "!p-4"
    {|@layer theme,components,utilities;@layer theme{:root,:host{--spacing:.25rem}}@layer components;@layer utilities{.\!p-4{padding:calc(var(--spacing)*4)!important}}|}

(* [not-has-<X>] reads X as a pseudo-class. The shorthand accepted any text and
   left the selector reader to raise out of [to_css], a pure conversion, while
   the bracket form [has-[...]] validated its selector. *)
let test_not_has_shorthand_selector () =
  let rejected cls =
    match Tw.of_string cls with
    | Ok _ -> Alcotest.failf "expected %s to be rejected" cls
    | Error _ -> ()
  in
  let renders cls =
    match Tw.of_string cls with
    | Ok u -> ignore (Tw.to_css ~base:false [ u ] |> Tw.Css.to_string)
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  rejected "not-has-a\\:flex";
  renders "not-has-checked:flex";
  renders "not-has-hover:flex"

(* @tailwindcss/typography registers one variant per element it styles, and the
   variant is what puts a utility on that element. Eight of them - h5, h6, dl,
   dt, dd, table, tr, picture - were not recognised at all, so the class was
   rejected and the utility never reached the element. *)
let test_prose_element_variants () =
  (* The variant scopes the utility to a descendant of the prose root, and the
     [:not(:where([class~=not-prose] ...))] guard is what keeps it off an
     opted-out subtree, so the whole rule is pinned rather than the element name
     alone. *)
  let targets name element =
    check_utilities
      ("prose-" ^ name ^ ":underline")
      (".prose-" ^ name ^ {|\:underline :where(|} ^ element
     ^ {|):not(:where([class~=not-prose],[class~=not-prose] *)){text-decoration-line:underline}|}
      )
  in
  List.iter
    (fun n -> targets n n)
    [
      "p";
      "a";
      "blockquote";
      "figure";
      "figcaption";
      "strong";
      "em";
      "kbd";
      "code";
      "pre";
      "ol";
      "ul";
      "li";
      "dl";
      "dt";
      "dd";
      "table";
      "thead";
      "tr";
      "th";
      "td";
      "img";
      "picture";
      "video";
      "hr";
      "h1";
      "h2";
      "h3";
      "h4";
      "h5";
      "h6";
    ];
  targets "headings" "h1,h2,h3,h4,h5,h6,th";
  targets "lead" "[class~=lead]"

(* The plugin has no variant for an element it does not style, and neither has
   tw: the class is rejected rather than compiled into a selector nothing in
   Tailwind produces. *)
let test_prose_element_variant_invalid () =
  Alcotest.(check bool)
    "prose-span: is not a variant" true
    (Result.is_error (Tw.of_string "prose-span:underline"));
  Alcotest.(check bool)
    "prose-h7: is not a variant" true
    (Result.is_error (Tw.of_string "prose-h7:underline"))

let tests =
  [
    test_case "important prefix" `Quick test_important_prefix;
    test_case "has_responsive_modifier" `Quick test_has_responsive_modifier;
    test_case "validate_no_nested_responsive" `Quick
      test_validate_no_nested_responsive;
    test_case "responsive functions reject nesting" `Quick
      test_responsive_functions_reject_nesting;
    test_case "apply modifiers" `Quick test_apply;
    test_case "modifier class names" `Quick test_modifier_class_names;
    test_case "media preference modifiers" `Quick
      test_media_preference_modifiers;
    test_case "modifier CSS roundtrip" `Quick test_modifier_css_roundtrip;
    test_case "selector escaping in CSS" `Quick test_selector_escaping_in_css;
    test_case "combined media modifiers" `Quick test_combined_media_modifiers;
    test_case "motion-reduce:transition-none uses transition-property" `Quick
      test_motion_reduce_transition_none;
  ]

(* Additional tests for modifiers parsing, rendering, and variants *)

(* Test is_hover flags *)
let test_is_hover () =
  check bool "Hover is hover" true (is_hover Hover);
  check bool "Group_hover is hover" true (is_hover Group_hover);
  check bool "Peer_hover is hover" true (is_hover Peer_hover);
  check bool "Focus is not hover" false (is_hover Focus)

(* Test of_string parsing *)
let test_of_string_parsing () =
  let mods, cls = Tw.Modifiers.of_string "hover:bg-blue-500" in
  check (list string) "hover modifier parsed" [ "hover" ] mods;
  check string "base class parsed (bg-blue-500)" "bg-blue-500" cls;

  let mods, cls = Tw.Modifiers.of_string "md:hover:p-4" in
  check (list string) "md:hover parsed order" [ "md"; "hover" ] mods;
  check string "base class parsed (p-4)" "p-4" cls;

  let mods, cls = Tw.Modifiers.of_string "2xl:m-2" in
  check (list string) "2xl parsed" [ "2xl" ] mods;
  check string "base class parsed (m-2)" "m-2" cls;

  let mods, cls = Tw.Modifiers.of_string "has-[.foo>bar]:p-4" in
  check (list string) "has-[...] parsed" [ "has-[.foo>bar]" ] mods;
  check string "base class parsed (p-4)" "p-4" cls;

  let mods, cls = Tw.Modifiers.of_string "group-has-[.bar]:hover:m-1" in
  check (list string) "group-has + hover parsed"
    [ "group-has-[.bar]"; "hover" ]
    mods;
  check string "base class parsed (m-1)" "m-1" cls

(* Test pp_modifier rendering *)
let test_pp_modifier_strings () =
  check string "pp sm" "sm" (pp_modifier (Responsive `Sm));
  check string "pp container md" "@md"
    (pp_modifier (Container Tw.Style.Container_md));
  (* An unnamed width is the arbitrary form; [@600px] is not a class the parser
     reads back, and Tailwind spells it [@[600px]]. *)
  check string "pp container named width" "@[600px]"
    (pp_modifier (Container (Tw.Style.Container_named ("", 600))));
  check string "pp has[...]" "has-[.foo]" (pp_modifier (Has ".foo"));
  check string "pp group-has[...]" "group-has-[.bar]"
    (pp_modifier (Group_has (".bar", None)));
  check string "pp peer-has[...]" "peer-has-[.baz]"
    (pp_modifier (Peer_has (".baz", None)));
  check string "pp data state bracketed" "data-[state=open]"
    (pp_modifier (Data_state "open"));
  check string "pp before" "before" (pp_modifier Pseudo_before);
  check string "pp not-hover" "not-hover" (pp_modifier (Not Hover))

(* The full container-query size scale (@3xs .. @7xl) emits a container query at
   the right threshold, using Tailwind v4's range syntax ([width >= 20rem]); @xs
   and @3xl+ used to be unknown modifiers. *)
let test_container_query_scale () =
  check_utilities "@xs:flex"
    {|@container(width>=20rem){.\@xs\:flex{display:flex}}|};
  check_utilities "@3xl:flex"
    {|@container(width>=48rem){.\@3xl\:flex{display:flex}}|};
  check string "@xs round-trips" "@xs:p-4"
    (Tw.Utility.to_class (Option.get (apply [ "@xs" ] (p 4))))

(* @max-<size> negates the min query, and @min-/@max-[<len>] and bare @[<len>]
   accept arbitrary lengths. All used to be unknown modifiers. *)
let test_container_query_min_max () =
  check_utilities "@min-md:flex"
    {|@container(width>=28rem){.\@min-md\:flex{display:flex}}|};
  check_utilities "@max-md:flex"
    {|@container not (width>=28rem){.\@max-md\:flex{display:flex}}|};
  check_utilities "@min-[20rem]:flex"
    {|@container(width>=20rem){.\@min-\[20rem\]\:flex{display:flex}}|};
  check_utilities "@max-[40rem]:flex"
    {|@container not (width>=40rem){.\@max-\[40rem\]\:flex{display:flex}}|};
  check_utilities "@[480px]:flex"
    {|@container(width>=480px){.\@\[480px\]\:flex{display:flex}}|};
  (* A theme(--breakpoint-lg) arbitrary value resolves to the breakpoint the
     [lg:] variant uses (64rem). Resolving the reference does not consume it:
     the CLI writes [--breakpoint-lg: 64rem] into the theme layer as well, so a
     consumer reading the token off the sheet still finds it. *)
  check_sheet "@min-[theme(--breakpoint-lg)]:flex"
    {|@layer theme,components,utilities;@layer theme{:root,:host{--breakpoint-lg:64rem}}@layer components;@layer utilities{@container(width>=64rem){.\@min-\[theme\(--breakpoint-lg\)\]\:flex{display:flex}}}|};
  check_sheet "@max-[theme(--breakpoint-lg)]:flex"
    {|@layer theme,components,utilities;@layer theme{:root,:host{--breakpoint-lg:64rem}}@layer components;@layer utilities{@container not (width>=64rem){.\@max-\[theme\(--breakpoint-lg\)\]\:flex{display:flex}}}|};
  check string "@max-md round-trips" "@max-md:p-4"
    (Tw.Utility.to_class (Option.get (apply [ "@max-md" ] (p 4))));
  check string "@min-[20rem] round-trips" "@min-[20rem]:p-4"
    (Tw.Utility.to_class (Option.get (apply [ "@min-[20rem]" ] (p 4))));
  check string "@[480px] round-trips" "@[480px]:p-4"
    (Tw.Utility.to_class (Option.get (apply [ "@[480px]" ] (p 4))))

(* Test apply with bracketed has/group-has/peer-has modifiers *)
let test_apply_bracketed_has () =
  let u1 = apply [ "has-[.x]" ] (p 4) in
  check string "has-[.x]:p-4" "has-[.x]:p-4"
    (Tw.Utility.to_class (Option.get u1));

  let u2 = apply [ "group-has-[.y]"; "hover" ] (m 2) in
  check string "group-has + hover order" "group-has-[.y]:hover:m-2"
    (Tw.Utility.to_class (Option.get u2));

  let u3 = apply [ "peer-has-[.z]" ] (bg blue) in
  check string "peer-has class" "peer-has-[.z]:bg-blue-500"
    (Tw.Utility.to_class (Option.get u3))

(* [has-<state>] takes the same state names as the group/peer variants, not just
   checked: the name resolves to the pseudo-class that state matches, and
   has-hover keeps the pointer gate hover itself carries. *)
let test_has_state_shorthands () =
  check_utilities "has-focus:flex"
    {|.has-focus\:flex:has(:focus){display:flex}|};
  check_utilities "has-focus-visible:flex"
    {|.has-focus-visible\:flex:has(:focus-visible){display:flex}|};
  check_utilities "has-first:flex"
    {|.has-first\:flex:has(:first-child){display:flex}|};
  check_utilities "has-odd:flex"
    {|.has-odd\:flex:has(:nth-child(odd)){display:flex}|};
  check_utilities "group-has-focus:flex"
    {|.group-has-focus\:flex:is(:where(.group):has(:focus) *){display:flex}|};
  (* has-hover keeps the pointer gate hover itself carries *)
  check_utilities "has-hover:flex"
    {|@media(hover:hover){.has-hover\:flex:has(:hover){display:flex}}|};
  (* the class name round-trips through the shorthand, not the bracket form *)
  check string "has-focus round-trips" "has-focus:flex"
    (Tw.pp (Result.get_ok (Tw.of_string "has-focus:flex")))

(* A named anchor works on any state variant, not just the has/aria/data ones:
   group-hover/edit scopes to .group\/edit. And a data variant takes the bare
   attribute shorthand under group-/peer- too, keeping a class name distinct
   from the bracket spelling. *)
let test_named_anchor_and_bare_data () =
  (* group-hover gates on the pointer, as a plain hover does *)
  check_utilities "group-hover/edit:underline"
    {|@media(hover:hover){.group-hover\/edit\:underline:is(:where(.group\/edit):hover *){text-decoration-line:underline}}|};
  check_utilities "group-focus/option:underline"
    {|.group-focus\/option\:underline:is(:where(.group\/option):focus *){text-decoration-line:underline}|};
  check_utilities "peer-checked/draft:block"
    {|.peer-checked\/draft\:block:is(:where(.peer\/draft):checked~*){display:block}|};
  check_utilities "group-data-modified:italic"
    {|.group-data-modified\:italic:is(:where(.group)[data-modified] *){font-style:italic}|};
  (* the bracket spelling keeps its own class name *)
  check_utilities "group-data-[modified]:italic"
    {|.group-data-\[modified\]\:italic:is(:where(.group)[data-modified] *){font-style:italic}|}

(* [has-data-lg] matches an attribute rather than a state but spells itself the
   same way, and [not-] composes over any variant, including a scoped
   [group-has-]: the negation wraps the whole relative selector. *)
let test_has_data_and_not_composition () =
  check_utilities "has-data-lg:opacity-40"
    {|.has-data-lg\:opacity-40:has([data-lg]){opacity:.4}|};
  check_utilities "group-has-data-lg:opacity-40"
    {|.group-has-data-lg\:opacity-40:is(:where(.group):has([data-lg]) *){opacity:.4}|};
  check_utilities "not-group-has-data-lg:opacity-40"
    {|.not-group-has-data-lg\:opacity-40:not(:is(:where(.group):has([data-lg]) *)){opacity:.4}|};
  check_utilities "not-peer-has-checked:opacity-0"
    {|.not-peer-has-checked\:opacity-0:not(:is(:where(.peer):has(:checked)~*)){opacity:0}|}

(* A bracket [has-] argument is one arbitrary relative selector, so Tailwind
   wraps it in [:is()], including a bare type selector. The brackets stay in the
   class name rather than being read as a state name. And [group-not-] takes any
   variant as its inner, not just the simple states. *)
let test_has_bracket_arguments () =
  let css cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string ~minify:true
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  let has cls affix =
    check bool cls true (Astring.String.is_infix ~affix (css cls))
  in
  check_utilities "has-[[data-a],[data-b]]:block"
    {|.has-\[\[data-a\]\,\[data-b\]\]\:block:has(:is([data-a], [data-b])){display:block}|};
  check_utilities "has-[a]:block"
    {|.has-\[a\]\:block:has(:is(a)){display:block}|};
  check_utilities "has-[:focus]:block"
    {|.has-\[\:focus\]\:block:has(:focus){display:block}|};
  check_utilities "has-[.item]:block"
    {|.has-\[\.item\]\:block:has(.item){display:block}|};
  check_utilities "has-[a_.item]:block"
    {|.has-\[a_\.item\]\:block:has(:is(a .item)){display:block}|};
  check_utilities "has-[&>img]:block"
    {|.has-\[\&\>img\]\:block:has(*>img){display:block}|};
  check_utilities "group-has-[a]:block"
    {|.group-has-\[a\]\:block:is(:where(.group):has(:is(a)) *){display:block}|};
  check_utilities "peer-has-[a]:block"
    {|.peer-has-\[a\]\:block:is(:where(.peer):has(:is(a))~*){display:block}|};
  check_utilities "group-not-has-[[data-hover],[data-focus]]:block"
    {|.group-not-has-\[\[data-hover\]\,\[data-focus\]\]\:block:is(:where(.group):not(:has(:is([data-hover], [data-focus]))) *){display:block}|};
  (* The hocus shorthand really is two selectors and stays a list. It stays on a
     substring: the pinned CLI emits nothing at all for [has-hocus], so pinning
     the sheet would pin a rule only tw writes. *)
  has "has-hocus:flex" ":has(:hover,:focus)"

(* A group or peer bracket without an [&] attaches its compound to the anchor:
   group-[.is-published] scopes to a .group that also has that class. It used to
   be an unknown class, since the anchor had to be spelled. *)
let test_anchor_bracket_without_ampersand () =
  check_utilities "group-[.is-published]:block"
    {|.group-\[\.is-published\]\:block:is(:where(.group).is-published *){display:block}|};
  check_utilities "peer-[.is-dirty]:block"
    {|.peer-\[\.is-dirty\]\:block:is(:where(.peer).is-dirty~*){display:block}|};
  (* the spelled anchor still works *)
  check_utilities "group-[&:hover]:block"
    {|.group-\[\&\:hover\]\:block:is(:where(.group):hover *){display:block}|}

(* What follows the [&] anchor in a group or peer bracket is a selector, so it
   is read as one. A pseudo-class outside a short hand-written list came out as
   a class literally named [:defined], and a compound remainder was taken whole
   as an element name, so [&_p.foo] named an element [p.foo]. *)
let test_anchor_bracket_reads_remainder () =
  check_utilities "group-[&:defined]:flex"
    {|.group-\[\&\:defined\]\:flex:is(:where(.group):defined *){display:flex}|};
  check_utilities "group-[&:nth-child(2)]:flex"
    {|.group-\[\&\:nth-child\(2\)\]\:flex:is(:where(.group):nth-child(2) *){display:flex}|};
  check_utilities "group-[&_p.foo]:flex"
    {|.group-\[\&_p\.foo\]\:flex:is(:where(.group) p.foo *){display:flex}|};
  (* the spellings that already worked stay byte-identical *)
  check_utilities "group-[&:hover]:flex"
    {|.group-\[\&\:hover\]\:flex:is(:where(.group):hover *){display:flex}|};
  check_utilities "group-[&_p]:flex"
    {|.group-\[\&_p\]\:flex:is(:where(.group) p *){display:flex}|};
  check_utilities "group-[:nth-of-type(3)_&]:flex"
    {|.group-\[\:nth-of-type\(3\)_\&\]\:flex:is(:nth-of-type(3) :where(.group) *){display:flex}|};
  check_utilities "peer-[&:hover]:flex"
    {|.peer-\[\&\:hover\]\:flex:is(:where(.peer):hover~*){display:flex}|}

(* Test ARIA and data modifiers class names *)
let test_aria_and_data_modifiers () =
  check string "aria-checked:p-4" "aria-checked:p-4"
    (Tw.Utility.to_class (aria_checked [ p 4 ]));
  check string "aria-disabled:m-1" "aria-disabled:m-1"
    (Tw.Utility.to_class (aria_disabled [ m 1 ]));
  check string "data-active:p-1" "data-active:p-1"
    (Tw.Utility.to_class (data_active [ p 1 ]));
  check string "data-inactive:m-2" "data-inactive:m-2"
    (Tw.Utility.to_class (data_inactive [ m 2 ]));
  (* The class name has to be the class the selector matches, and Tailwind
     brackets a data attribute carrying a value. *)
  check string "data-[state=open]:bg-blue-500" "data-[state=open]:bg-blue-500"
    (Tw.Utility.to_class (data_state "open" (bg blue)));
  check string "data-[variant=primary]:p-3" "data-[variant=primary]:p-3"
    (Tw.Utility.to_class (data_variant "primary" (p 3)));
  check string "data-[status=on]:m-4" "data-[status=on]:m-4"
    (Tw.Utility.to_class (data_custom "status" "on" (m 4)))

(* Test before/after pseudo-element modifiers *)
let test_before_after_modifiers () =
  check string "before:p-4" "before:p-4" (Tw.Utility.to_class (before [ p 4 ]));
  check string "after:m-2" "after:m-2" (Tw.Utility.to_class (after [ m 2 ]))

(* Test nested modifier class generation *)
let test_nested_modifier_class_names () =
  (* Basic dark:hover: nesting *)
  check string "dark:hover:text-white" "dark:hover:text-white"
    Tw.(to_classes [ dark [ hover [ text white ] ] ]);

  (* Multiple utilities in nested modifier group *)
  check string "dark:[hover group with multiple items]"
    "dark:text-gray-300 dark:hover:bg-gray-700 dark:hover:text-white"
    Tw.(
      to_classes
        [
          dark
            [ text ~shade:300 gray; hover [ bg ~shade:700 gray; text white ] ];
        ]);

  (* Triple nesting: sm:dark:hover *)
  check string "sm:dark:hover:bg-blue-500" "sm:dark:hover:bg-blue-500"
    Tw.(to_classes [ sm [ dark [ hover [ bg blue ] ] ] ]);

  (* focus:before: nesting *)
  check string "focus:before:content-*" "focus:before:content-[\"'*'\"]"
    Tw.(to_classes [ focus [ before [ content "'*'" ] ] ]);

  (* md:hover: group with multiple items *)
  check string "md:hover:[multiple items]"
    "md:hover:bg-blue-500 md:hover:text-white"
    Tw.(to_classes [ md [ hover [ bg blue; text white ] ] ]);

  (* dark:focus-within: nesting *)
  check string "dark:focus-within:ring" "dark:focus-within:ring-2"
    Tw.(to_classes [ dark [ focus_within [ ring_sm ] ] ]);

  (* group-hover inside dark *)
  check string "dark:group-hover:text-white" "dark:group-hover:text-white"
    Tw.(to_classes [ dark [ group_hover [ text white ] ] ]);

  (* Complex nested group structure - this is the pattern from dashboard
     main.ml *)
  let complex_styles =
    Tw.
      [
        text ~shade:600 gray;
        hover [ bg ~shade:100 gray; text ~shade:900 gray ];
        dark [ text ~shade:300 gray; hover [ bg ~shade:700 gray; text white ] ];
      ]
  in
  check string "complex nested structure"
    "text-gray-600 hover:bg-gray-100 hover:text-gray-900 dark:text-gray-300 \
     dark:hover:bg-gray-700 dark:hover:text-white"
    (Tw.to_classes complex_styles)

(* Test CSS generation for nested modifiers *)
let test_nested_modifier_css_generation () =
  (* dark:hover: nests one media query inside the other, in the order the class
     spells them, and keeps the escaped class name. *)
  check_sheet "dark:hover:bg-blue-500"
    {|@layer theme,components,utilities;@layer theme{:root,:host{--color-blue-500:oklch(62.3%.214 259.815)}}@layer components;@layer utilities{@media(prefers-color-scheme:dark){@media(hover:hover){.dark\:hover\:bg-blue-500:hover{background-color:var(--color-blue-500)}}}}|};
  match Tw.Css.of_string (sheet "dark:hover:bg-blue-500") with
  | Ok _ -> ()
  | Error e ->
      Alcotest.failf "Nested modifier CSS parse failed:\n%s"
        (Cascade.Error.to_string e)

(* not-[<selector>] with an arbitrary selector parses [_] as a space and [&] as
   the element, which becomes the universal selector inside the negation (so
   not-[.os-macos_&]:block negates the descendant context as a :not over
   .os-macos descendants). It used to escape the bracket content as one class
   name instead. *)
let test_not_bracket_arbitrary_selector () =
  check_utilities "not-[.os-macos_&]:block"
    {|.not-\[\.os-macos_\&\]\:block:not(.os-macos *){display:block}|};
  check_utilities "not-[.foo]:block"
    {|.not-\[\.foo\]\:block:not(.foo){display:block}|}

(* The bracket is negated as a selector, so content the selector grammar cannot
   read is not a negation and the class is refused at parse time. The reader
   raises rather than answering, and it stops at the first thing it cannot use,
   so a trailing remainder is refused too rather than silently dropped. *)
let test_not_bracket_unreadable_selector_rejected () =
  List.iter
    (fun cls ->
      match Tw.of_string cls with
      | Ok u ->
          Alcotest.failf "expected %s to be rejected, got %s" cls (Tw.pp u)
      | Error (`Msg _) -> ())
    [
      "not-[a;b]:flex";
      "not-[0.5ch]:flex";
      "not-[1px}]:flex";
      "not-[a{b]:flex";
      "not-[*/]:flex";
      "not-[<value>]:flex";
      "not-[url(a;b)]:flex";
      "not-[()]:flex";
    ];
  (* the readable ones still parse *)
  List.iter
    (fun cls ->
      match Tw.of_string cls with
      | Ok _ -> ()
      | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m)
    [
      "not-[.foo]:flex";
      "not-[.os-macos_&]:flex";
      "not-[:checked]:flex";
      "not-[@media_print]:flex";
    ]

(* A group/peer arbitrary variant whose [&] anchor is preceded by a context
   (e.g. group-[:nth-of-type(3)_&]) keeps that prefix ahead of the anchor,
   rather than dropping it down to just :where(.group). *)
let test_group_arbitrary_prefix () =
  check_utilities "group-[:nth-of-type(3)_&]:block"
    {|.group-\[\:nth-of-type\(3\)_\&\]\:block:is(:nth-of-type(3) :where(.group) *){display:block}|};
  check_utilities "group-[&_p]:block"
    {|.group-\[\&_p\]\:block:is(:where(.group) p *){display:block}|}

(* An nth-* bracket holds an An+B expression and a supports- bracket holds an
   @supports condition; both are taken verbatim, so content the CSS grammar has
   no production for escapes as a parse error out of the selector or condition
   reader. They are validated where data-[ and has-[ are. *)
let test_invalid_bracket_modifiers () =
  let rejected cls =
    match Tw.of_string cls with
    | Ok _ -> Alcotest.failf "expected %s to be rejected" cls
    | Error _ -> ()
  in
  rejected "nth-[]:flex";
  rejected "nth-[abc]:flex";
  rejected "nth-last-[]:flex";
  rejected "nth-of-type-[]:flex";
  rejected "nth-last-of-type-[]:flex";
  rejected "supports-[]:flex"

(* An aria- or data- variant names an attribute, so an empty argument would
   compile to [[aria-]] or [[data-]]: a selector browsers parse and keep, and
   nothing ever matches. An underscore stands for a space, so it is empty
   too. *)
let test_empty_attribute_brackets () =
  let rejected cls =
    match Tw.of_string cls with
    | Ok u -> Alcotest.failf "expected %s to be rejected, got %s" cls (Tw.pp u)
    | Error _ -> ()
  in
  rejected "aria-[]:flex";
  rejected "aria-[_]:flex";
  rejected "aria-_:flex";
  rejected "group-aria-[]:flex";
  rejected "peer-aria-[]:flex";
  rejected "data-[]:flex";
  rejected "data-[_]:flex";
  rejected "group-data-[]:flex";
  rejected "peer-data-[]:flex";
  rejected "group-data-_:flex";
  rejected "peer-data-_:flex"

let test_padded_attribute_brackets () =
  let rejected cls =
    match Tw.of_string cls with
    | Ok u -> Alcotest.failf "expected %s to be rejected, got %s" cls (Tw.pp u)
    | Error _ -> ()
  in
  rejected "aria-[_modal_]:flex";
  rejected "data-[_state=open_]:flex"

(* The attribute spellings that do name something keep working, including the
   empty-name-with-value form Tailwind also accepts. *)
let test_attribute_brackets_still_parse () =
  check_utilities "aria-[modal]:flex"
    {|.aria-\[modal\]\:flex[aria-modal]{display:flex}|};
  check_utilities "aria-[sort=ascending]:flex"
    {|.aria-\[sort\=ascending\]\:flex[aria-sort=ascending]{display:flex}|};
  check_utilities "aria-[=true]:flex"
    {|.aria-\[\=true\]\:flex[aria-=true]{display:flex}|};
  check_utilities "data-[state=open]:flex"
    {|.data-\[state\=open\]\:flex[data-state=open]{display:flex}|};
  check_utilities "group-data-[dragging]:flex"
    {|.group-data-\[dragging\]\:flex:is(:where(.group)[data-dragging] *){display:flex}|};
  check_utilities "peer-data-[dragging]:flex"
    {|.peer-data-\[dragging\]\:flex:is(:where(.peer)[data-dragging]~*){display:flex}|};
  check_utilities "group-data-modified:flex"
    {|.group-data-modified\:flex:is(:where(.group)[data-modified] *){display:flex}|}

(* [parse_data_expr] reads the [data-[...]] bracket body into an attribute
   match: every operator ([$=], [^=], [*=], [~=], [|=], bare [=]), the
   presence-only bare form, a quoted value holding a decoded space, and a
   trailing case-sensitivity flag. This pins the selector each spelling produces
   today so a rewrite of the reader cannot silently change one. *)
let test_data_bracket_operators () =
  let css cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string ~minify:true
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  let emits affix cls =
    check bool cls true (Astring.String.is_infix ~affix (css cls))
  in
  check_utilities "data-[size~=large]:flex"
    {|.data-\[size\~\=large\]\:flex[data-size~=large]{display:flex}|};
  check_utilities "data-[foo=bar]:flex"
    {|.data-\[foo\=bar\]\:flex[data-foo=bar]{display:flex}|};
  check_utilities "data-[foo^=bar]:flex"
    {|.data-\[foo\^\=bar\]\:flex[data-foo^=bar]{display:flex}|};
  check_utilities "data-[foo$=bar]:flex"
    {|.data-\[foo\$\=bar\]\:flex[data-foo$=bar]{display:flex}|};
  check_utilities "data-[foo*=bar]:flex"
    {|.data-\[foo\*\=bar\]\:flex[data-foo*=bar]{display:flex}|};
  check_utilities "data-[foo|=bar]:flex"
    {|.data-\[foo\|\=bar\]\:flex[data-foo|=bar]{display:flex}|};
  check_utilities "data-[open]:flex"
    {|.data-\[open\]\:flex[data-open]{display:flex}|};
  (* A decoded space in the value stays on a substring: tw quotes the attribute
     value where the pinned CLI escapes the space instead ([data-foo=a\ b]). *)
  emits {|[data-foo="a b"]|} "data-[foo='a_b']:flex";
  check_utilities "data-[foo=bar_i]:flex"
    {|.data-\[foo\=bar_i\]\:flex[data-foo=bar i]{display:flex}|};
  check_utilities "data-[foo=bar_s]:flex"
    {|.data-\[foo\=bar_s\]\:flex[data-foo=bar s]{display:flex}|};
  check_utilities "group-data-[size~=large]:flex"
    {|.group-data-\[size\~\=large\]\:flex:is(:where(.group)[data-size~=large] *){display:flex}|};
  check_utilities "peer-data-[size~=large]:flex"
    {|.peer-data-\[size\~\=large\]\:flex:is(:where(.peer)[data-size~=large]~*){display:flex}|}

(* A bare [[...]] variant compounds its selector onto the utility's own class,
   so what the brackets hold has to be a compound selector. [~] is both the
   sibling combinator and the [~=] whitespace-list attribute operator, and only
   reading the bracket as a selector tells them apart: a character scan that
   rejects every [~] rejects [[data-size~=large]] along with [p_~_span]. *)
let test_bare_selector_variant_attribute_operators () =
  let rejected cls =
    match Tw.of_string cls with
    | Ok u -> Alcotest.failf "expected %s to be rejected, got %s" cls (Tw.pp u)
    | Error _ -> ()
  in
  check_utilities "[[data-size~=large]]:underline"
    {|.\[\[data-size\~\=large\]\]\:underline[data-size~=large]{text-decoration-line:underline}|};
  check_utilities "group-[[data-size~=large]]:underline"
    {|.group-\[\[data-size\~\=large\]\]\:underline:is(:where(.group)[data-size~=large] *){text-decoration-line:underline}|};
  check_utilities "peer-[[data-size~=large]]:underline"
    {|.peer-\[\[data-size\~\=large\]\]\:underline:is(:where(.peer)[data-size~=large]~*){text-decoration-line:underline}|};
  (* the attribute operators that never collided with a combinator stay put *)
  check_utilities "[[lang|=en]]:underline"
    {|.\[\[lang\|\=en\]\]\:underline[lang|=en]{text-decoration-line:underline}|};
  check_utilities "[[href^=https]]:underline"
    {|.\[\[href\^\=https\]\]\:underline[href^=https]{text-decoration-line:underline}|};
  (* a combinator really is one, and none of these is a compound *)
  rejected "[p_~_span]:underline";
  rejected "[>img]:underline";
  rejected "[.a_.b]:underline";
  rejected "[@media_print]:underline"

(* The valid spellings the validation must keep accepting. *)
let test_valid_bracket_modifiers () =
  check_utilities "nth-[2n+1]:flex"
    {|.nth-\[2n\+1\]\:flex:nth-child(odd){display:flex}|};
  check_utilities "nth-[3]:flex" {|.nth-\[3\]\:flex:nth-child(3){display:flex}|};
  check_utilities "nth-last-[2n]:flex"
    {|.nth-last-\[2n\]\:flex:nth-last-child(2n){display:flex}|};
  check_utilities "nth-of-type-[odd]:flex"
    {|.nth-of-type-\[odd\]\:flex:nth-of-type(odd){display:flex}|};
  check_utilities "supports-[display:grid]:flex"
    {|@supports(display:grid){.supports-\[display\:grid\]\:flex{display:flex}}|}

(* A [supports-<property>] test names the property the author wrote, even for a
   property browsers once shipped behind a vendor prefix: Tailwind emits
   [@supports (hyphens: var(--tw))], so the shorthand and the bracket spelling
   of one property give the same condition. *)
let test_supports_property_is_unprefixed () =
  let condition cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  let emits affix cls =
    check bool cls true (Astring.String.is_infix ~affix (condition cls))
  in
  let unprefixed cls =
    let css = condition cls in
    check bool cls false (Astring.String.is_infix ~affix:"-webkit-" css);
    check bool cls false (Astring.String.is_infix ~affix:"-moz-" css)
  in
  check_utilities "supports-hyphens:flex"
    {|@supports(hyphens:var(--tw)){.supports-hyphens\:flex{display:flex}}|};
  check_utilities "supports-user-select:flex"
    {|@supports(user-select:var(--tw)){.supports-user-select\:flex{display:flex}}|};
  check_utilities "supports-[user-select]:flex"
    {|@supports(user-select:var(--tw)){.supports-\[user-select\]\:flex{display:flex}}|};
  (* text-size-adjust stays on a substring: the pinned CLI's own prefixing turns
     the condition into three alternatives ([-webkit-], [-moz-], plain) where
     tw's leaves one, so the sheets cannot be compared whole. *)
  emits "@supports (text-size-adjust: var(--tw))"
    "supports-text-size-adjust:flex";
  check_utilities "supports-backdrop-filter:flex"
    {|@supports(backdrop-filter:var(--tw)){.supports-backdrop-filter\:flex{display:flex}}|};
  unprefixed "supports-hyphens:flex";
  unprefixed "supports-user-select:flex";
  unprefixed "supports-text-size-adjust:flex";
  unprefixed "supports-backdrop-filter:flex"

(* A variant that wraps the utility in an at-rule keeps that at-rule when the
   variant it decorates already produced a media query. [supports-grid:sm:flex]
   used to render as [.sm\:flex] inside the breakpoint alone: the feature query
   and the [supports-grid] half of the class name both disappeared, so the rule
   applied unconditionally and matched a class the author never wrote. Tailwind
   nests the two in the order the class spells them. *)
let test_at_rule_variant_over_media () =
  check_utilities "supports-grid:sm:flex"
    {|@supports(grid:var(--tw)){@media(min-width:40rem){.supports-grid\:sm\:flex{display:flex}}}|};
  check_utilities "not-supports-grid:sm:flex"
    {|@supports not (grid:var(--tw)){@media(min-width:40rem){.not-supports-grid\:sm\:flex{display:flex}}}|};
  check_utilities "starting:sm:flex"
    {|@starting-style{@media(min-width:40rem){.starting\:sm\:flex{display:flex}}}|};
  check_utilities "@md:sm:flex"
    {|@container(width>=28rem){@media(min-width:40rem){.\@md\:sm\:flex{display:flex}}}|}

(* A container query is not a [not-*] inner. Tailwind rejects the class
   outright; tw built a rule whose selector negates the utility's own class
   ([.not-\@md\:flex:not(.flex)]), which can never match anything. *)
let test_not_container_rejected () =
  let rejected cls =
    match Tw.of_string cls with
    | Ok _ -> Alcotest.failf "expected %s to be rejected" cls
    | Error _ -> ()
  in
  rejected "not-@md:flex";
  rejected "not-@lg:flex";
  rejected "not-@min-[30rem]:flex";
  (* The negatable inners still parse. *)
  check bool "not-hover still parses" true
    (Result.is_ok (Tw.of_string "not-hover:flex"));
  check bool "not-first still parses" true
    (Result.is_ok (Tw.of_string "not-first:flex"))

(* A class that parses but renders no rule at all is worse than one that is
   refused: a typo in a variant name stays silent. Tailwind emits nothing for
   these four either, so rejecting them moves no rendered output - it only turns
   silence into a message. [group-not-hover] is the shape: [rule.ml] already
   knows a group negation cannot wrap a media query and answers with no rules,
   so the parser should not have accepted it. *)
let test_silent_empty_variants_rejected () =
  let rejected cls =
    match Tw.of_string cls with
    | Ok _ -> Alcotest.failf "expected %s to be rejected" cls
    | Error _ -> ()
  in
  rejected "group-not-hover:flex";
  rejected "group-not-device-hocus:flex";
  rejected "peer-not-hover:flex";
  (* The negations that do have a selector form still parse. *)
  check bool "group-not-checked still parses" true
    (Result.is_ok (Tw.of_string "group-not-checked:flex"));
  check bool "not-hover still parses" true
    (Result.is_ok (Tw.of_string "not-hover:flex"))

(* A variant's bracket is an arbitrary value too: [_] is a space and [\_] a
   literal underscore. An attribute value, a [@supports] condition and a
   selector each carry the escape into a different part of the rule. *)
let test_variant_underscore_escape () =
  let css cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  let has cls affix =
    check bool
      (cls ^ " emits " ^ affix)
      true
      (Astring.String.is_infix ~affix (css cls))
  in
  check_utilities {|data-[foo=bar\_baz]:flex|}
    {|.data-\[foo\=bar\\_baz\]\:flex[data-foo=bar_baz]{display:flex}|};
  check_utilities {|supports-[--a\_b]:flex|}
    {|@supports(--a_b:var(--tw)){.supports-\[--a\\_b\]\:flex{display:flex}}|};
  check_utilities {|[.a\_b]:flex|} {|.\[\.a\\_b\]\:flex.a_b{display:flex}|};
  check_utilities {|group-[.a\_b_&]:flex|}
    {|.group-\[\.a\\_b_\&\]\:flex:is(.a_b :where(.group) *){display:flex}|};
  check_utilities {|nth-[2n+1_of_.a\_b]:flex|}
    {|.nth-\[2n\+1_of_\.a\\_b\]\:flex:nth-child(odd of.a_b){display:flex}|};
  (* A bare [_] still stands for a space. This one stays on a substring: tw
     quotes the attribute value where the pinned CLI escapes the space. *)
  has "data-[foo=bar_baz]:flex" {|[data-foo="bar baz"]|}

(* Extend the suite with new tests *)
let tests =
  tests
  @ [
      test_case "variant underscore escape" `Quick
        test_variant_underscore_escape;
      test_case "invalid bracket modifiers" `Quick
        test_invalid_bracket_modifiers;
      test_case "valid bracket modifiers" `Quick test_valid_bracket_modifiers;
      test_case "supports property is unprefixed" `Quick
        test_supports_property_is_unprefixed;
      test_case "at-rule variant over a media query" `Quick
        test_at_rule_variant_over_media;
      test_case "not-container is rejected" `Quick test_not_container_rejected;
      test_case "silent empty variants rejected" `Quick
        test_silent_empty_variants_rejected;
      test_case "empty attribute brackets" `Quick test_empty_attribute_brackets;
      test_case "padded attribute brackets" `Quick
        test_padded_attribute_brackets;
      test_case "attribute brackets still parse" `Quick
        test_attribute_brackets_still_parse;
      test_case "data bracket operators" `Quick test_data_bracket_operators;
      test_case "bare selector variant attribute operators" `Quick
        test_bare_selector_variant_attribute_operators;
      test_case "not-[selector] arbitrary negation" `Quick
        test_not_bracket_arbitrary_selector;
      test_case "group arbitrary prefix anchor" `Quick
        test_group_arbitrary_prefix;
      test_case "is_hover flags" `Quick test_is_hover;
      test_case "of_string parsing" `Quick test_of_string_parsing;
      test_case "pp_modifier strings" `Quick test_pp_modifier_strings;
      test_case "container query scale" `Quick test_container_query_scale;
      test_case "container query min/max" `Quick test_container_query_min_max;
      test_case "apply bracketed has variants" `Quick test_apply_bracketed_has;
      test_case "has state shorthands" `Quick test_has_state_shorthands;
      test_case "named anchor and bare data" `Quick
        test_named_anchor_and_bare_data;
      test_case "has-data and not- composition" `Quick
        test_has_data_and_not_composition;
      test_case "has bracket arguments" `Quick test_has_bracket_arguments;
      test_case "anchor bracket without ampersand" `Quick
        test_anchor_bracket_without_ampersand;
      test_case "anchor bracket reads its remainder" `Quick
        test_anchor_bracket_reads_remainder;
      test_case "ARIA and data modifiers" `Quick test_aria_and_data_modifiers;
      test_case "modifier class round-trips" `Quick
        test_modifier_class_roundtrip;
      test_case "before/after modifiers" `Quick test_before_after_modifiers;
      test_case "nested modifier class names" `Quick
        test_nested_modifier_class_names;
      test_case "nested modifier CSS generation" `Quick
        test_nested_modifier_css_generation;
      test_case "not-has shorthand selector" `Quick
        test_not_has_shorthand_selector;
      test_case "arbitrary breakpoint spelling" `Quick
        test_arbitrary_breakpoint_spelling;
      test_case "nth spelling" `Quick test_nth_spelling;
      test_case "arbitrary breakpoint rejects non-length" `Quick
        test_arbitrary_breakpoint_rejects_non_length;
      test_case "arbitrary breakpoint rejects OCaml literals" `Quick
        test_arbitrary_breakpoint_rejects_ocaml_literals;
      test_case "custom variant is theme-local" `Quick
        test_custom_variant_is_theme_local;
      test_case "container variant is theme-local" `Quick
        test_container_variant_is_theme_local;
      test_case "removed breakpoint drops its variants" `Quick
        test_removed_breakpoint_drops_its_variants;
      test_case "prose element variants" `Quick test_prose_element_variants;
      test_case "prose element variant invalid" `Quick
        test_prose_element_variant_invalid;
      test_case "variant cascade ladder" `Quick test_variant_cascade_ladder;
      test_case "variant inner order" `Quick test_variant_inner_order;
      test_case "not-[selector] unreadable content rejected" `Quick
        test_not_bracket_unreadable_selector_rejected;
    ]

let suite = ("modifiers", tests)
