module Css = Cascade.Css
open Alcotest
open Tw.Output
open Tw.Color
open Tw.Padding
open Tw.Modifiers

(* ===== Tests ===== *)

let check_extract_selector_props () =
  let rules = Tw.Rule.outputs (p 4) in
  check int "single rule extracted" 1 (List.length rules);
  match rules with
  | [ Regular { selector; _ } ] ->
      check string "correct selector" ".p-4" (Css.Selector.to_string selector)
  | _ -> fail "Expected Regular rule"

let check_extract_hover () =
  let rules = Tw.Rule.outputs (hover [ bg blue ]) in
  check int "single rule extracted" 1 (List.length rules);
  match rules with
  | [ Regular { selector; _ } ] ->
      let sel_str = Css.Selector.to_string selector in
      check string "hover selector" ".hover\\:bg-blue-500:hover" sel_str
  | _ -> fail "Expected Regular rule with hover"

let check_extract_responsive () =
  let rules = Tw.Rule.outputs (sm [ p 4 ]) in
  check int "single rule extracted" 1 (List.length rules);
  match rules with
  | [ Media_query { condition; selector; _ } ] ->
      check string "media condition" "(min-width: 40rem)"
        (Css.Media.to_string condition);
      check Test_helpers.selector_testable "sm selector"
        (Css.Selector.class_ "sm:p-4")
        selector
  | _ -> fail "Expected Media_query rule"

let check_extract_responsive_md () =
  let rules = Tw.Rule.outputs (md [ p 4 ]) in
  check int "single rule extracted" 1 (List.length rules);
  match rules with
  | [ Media_query { condition; selector; _ } ] ->
      check string "md media condition" "(min-width: 48rem)"
        (Css.Media.to_string condition);
      check Test_helpers.selector_testable "md selector"
        (Css.Selector.class_ "md:p-4")
        selector
  | _ -> fail "Expected Media_query rule for md"

let check_extract_responsive_lg () =
  let rules = Tw.Rule.outputs (lg [ p 4 ]) in
  check int "single rule extracted" 1 (List.length rules);
  match rules with
  | [ Media_query { condition; selector; _ } ] ->
      check string "lg media condition" "(min-width: 64rem)"
        (Css.Media.to_string condition);
      check Test_helpers.selector_testable "lg selector"
        (Css.Selector.class_ "lg:p-4")
        selector
  | _ -> fail "Expected Media_query rule for lg"

let check_extract_responsive_xl () =
  let rules = Tw.Rule.outputs (xl [ p 4 ]) in
  check int "single rule extracted" 1 (List.length rules);
  match rules with
  | [ Media_query { condition; selector; _ } ] ->
      check string "xl media condition" "(min-width: 80rem)"
        (Css.Media.to_string condition);
      check Test_helpers.selector_testable "xl selector"
        (Css.Selector.class_ "xl:p-4")
        selector
  | _ -> fail "Expected Media_query rule for xl"

let check_extract_responsive_2xl () =
  let rules = Tw.Rule.outputs (xl2 [ p 4 ]) in
  check int "single rule extracted" 1 (List.length rules);
  match rules with
  | [ Media_query { condition; selector; _ } ] ->
      check string "2xl media condition" "(min-width: 96rem)"
        (Css.Media.to_string condition);
      check Test_helpers.selector_testable "2xl selector"
        (Css.Selector.class_ "2xl:p-4")
        selector
  | _ -> fail "Expected Media_query rule for 2xl"

let check_escape_class_name () =
  check string "escapes brackets" "p-\\[10px\\]"
    (Tw.Rule.escape_class_name "p-[10px]");
  check string "escapes colon" "hover\\:bg-blue-500"
    (Tw.Rule.escape_class_name "hover:bg-blue-500");
  check string "escapes slash" "w-1\\/2" (Tw.Rule.escape_class_name "w-1/2");
  check string "escapes dot" "text-1\\.5" (Tw.Rule.escape_class_name "text-1.5")

let test_modifier_to_rule () =
  let rule =
    Tw.Rule.modifier_to_rule Tw.Style.Hover "bg-blue-500"
      (Css.Selector.class_ "bg-blue-500")
      [ Css.background_color (Css.hex "#3b82f6") ]
  in
  match rule with
  | Tw.Output.Regular { selector; props; has_hover; _ } ->
      (* Hover modifier uses Modifiers.to_selector which includes the prefix *)
      check string "hover selector" ".hover\\:bg-blue-500:hover"
        (Css.Selector.to_string selector);
      check int "preserves props" 1 (List.length props);
      check bool "marked as hover" true has_hover
  | _ -> fail "Expected Regular rule for hover"

(* Arbitrary selector variants whose remainder starts with a combinator
   ([&>div], [&+p], [&~p]) used to crash the renderer with a Cascade
   Parse_error: the code forced a descendant combine and re-parsed ">div", which
   the selector reader rejects. They must flatten to the combinator selector. *)
let test_arbitrary_selector_combinator () =
  let css cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  check bool "[&>div:first-child] keeps the child combinator" true
    (Astring.String.is_infix ~affix:"> div:first-child"
       (css "[&>div:first-child]:ring-2"));
  check bool "[&+p] keeps the adjacent-sibling combinator" true
    (Astring.String.is_infix ~affix:"+ p" (css "[&+p]:underline"));
  check bool "[&_p] still flattens to a descendant" true
    (Astring.String.is_infix ~affix:"]\\:underline p" (css "[&_p]:underline"))

(* Regression: an opacity color emits a progressive-enhancement @supports block.
   Under a variant, that block must stay scoped to the variant instead of
   leaking a bare base-class rule. dark:text-white/80 previously emitted a
   top-level .text-white/80 @supports rule alongside the correct
   .dark:text-white/80. *)
let test_opacity_color_variant_no_leak () =
  let css cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string ~minify:true
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  (* The base class must never appear as a standalone selector (".text-white");
     only the variant-decorated ".dark\:text-white" / ".hover\:text-red" is
     allowed. *)
  check bool "dark:text-white/80 leaks no bare .text-white" false
    (Astring.String.is_infix ~affix:".text-white" (css "dark:text-white/80"));
  check bool "hover:text-red-500/50 leaks no bare .text-red" false
    (Astring.String.is_infix ~affix:".text-red" (css "hover:text-red-500/50"))

(* Regression: a stacked hover:dark:* variant must keep the @media (hover:hover)
   wrapper (nested inside the dark media), matching Tailwind. tw used to emit a
   bare @media (dark) block, so the hover style also applied on touch
   devices. *)
let test_hover_dark_media_wrapper () =
  let css cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string ~minify:true
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  let s = css "hover:dark:text-white" in
  check bool "hover:dark keeps @media (hover:hover)" true
    (Astring.String.is_infix ~affix:"@media (hover:hover)" s
    || Astring.String.is_infix ~affix:"@media(hover:hover)" s);
  check bool "hover:dark keeps the dark media" true
    (Astring.String.is_infix ~affix:"prefers-color-scheme:dark" s
    || Astring.String.is_infix ~affix:"prefers-color-scheme: dark" s)

(* An outer variant has to find the class the inner one produced. The child
   variant buries it inside an [:is] with a child combinator and the
   pseudo-element variants report the class they prefixed, so both used to be
   invisible: the outer variant dropped out of the class name, and stacking two
   child variants collapsed to one. *)
let test_outer_variant_over_child_and_pseudo () =
  let css cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string ~minify:true
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  let has cls affix =
    check bool cls true (Astring.String.is_infix ~affix (css cls))
  in
  has "max-sm:after:block" ".max-sm\\:after\\:block:after";
  has "hover:before:underline" ".hover\\:before\\:underline:hover:before";
  has "sm:*:rotate-0" ":is(.sm\\:\\*\\:rotate-0>*)";
  has "hover:*:underline" ":is(.hover\\:\\*\\:underline:hover>*)";
  has "*:*:grow" ":is(:is(.\\*\\:\\*\\:grow>*)>*)"

(* An arbitrary variant with no [&] anchor compounds onto the utility's own
   class: [[.line]] attaches directly, a type selector goes in an [:is()] since
   it cannot follow a class. One that is not a single compound ([[>img]]) is not
   a variant at all. *)
let test_bare_arbitrary_selector_variant () =
  let css cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string ~minify:true
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  let has cls affix =
    check bool cls true (Astring.String.is_infix ~affix (css cls))
  in
  has "[.line]:block" ".\\[\\.line\\]\\:block.line";
  has "[code]:pr-4" ".\\[code\\]\\:pr-4:is(code)";
  has "**:[code]:pr-4" ":is(.\\*\\*\\:\\[code\\]\\:pr-4 *):is(code)";
  check bool "[>img] is not a variant" true
    (Result.is_error (Tw.of_string "[>img]:flex"))

(* An arbitrary variant stacks with the variants beside it. What the inner
   variant compounded onto the class belongs on the element this one makes the
   subject: [[&_p]:first:] matches the first [p], not a [p] under a first child.
   A responsive variant in the chain used to drop the arbitrary selector
   entirely, since the media path rebuilds the selector from a spelling [[svg]]
   has none of. *)
let test_arbitrary_selector_stacking () =
  let css cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string ~minify:true
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  let has cls affix =
    check bool cls true (Astring.String.is_infix ~affix (css cls))
  in
  has "[svg]:first:size-4" ".\\[svg\\]\\:first\\:size-4:is(svg):first-child";
  has "[&_p]:first:size-4" ".\\[\\&_p\\]\\:first\\:size-4 p:first-child";
  has "[svg]:sm:size-4" ".\\[svg\\]\\:sm\\:size-4:is(svg)";
  has "**:[svg]:first:sm:size-4"
    ":is(.\\*\\*\\:\\[svg\\]\\:first\\:sm\\:size-4 *):is(svg):first-child"

(* An opacity colour emits a progressive-enhancement @supports block beside its
   fallback. Under a variant the block used to carry the theme declaration a
   second time, and for the variants that set an order of their own it sorted
   ahead of the rule it enhances, so the fallback won. *)
let test_opacity_supports_under_variant () =
  let css cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string ~minify:true
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  let s = css "hover:bg-red-500/50" in
  let occurrences sub str =
    let n = String.length sub in
    let rec go i acc =
      if i + n > String.length str then acc
      else if String.sub str i n = sub then go (i + n) (acc + 1)
      else go (i + 1) acc
    in
    go 0 0
  in
  (* once in @layer theme, not again inside the @supports block *)
  check int "the theme token is declared once" 1
    (occurrences "--color-red-500:oklch" s);
  (* the enhancement has to come after the fallback to win *)
  let d = css "aria-selected:bg-red-500/50" in
  let idx affix =
    Option.get (Astring.String.find_sub ~sub:affix d) |> fun (i : int) -> i
  in
  check bool "the @supports block follows the fallback" true
    (idx "#fb2c3680" < idx "@supports")

(* An at-rule written in brackets wraps the utility rather than selecting it,
   and keeps its own spelling in the class name: reading it as the [supports-]
   variant gave a class the HTML would not match. A property test also runs
   against the prefixed spellings, as Tailwind's browser data has it. *)
let test_bracketed_at_rule_variant () =
  let css cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string ~minify:true
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  let has cls affix =
    check bool cls true (Astring.String.is_infix ~affix (css cls))
  in
  has "[@starting-style]:opacity-0"
    "@starting-style{.\\[\\@starting-style\\]\\:opacity-0";
  has "[@supports(display:grid)]:grid" "@supports(display:grid)";
  has "[@supports(display:grid)]:grid"
    ".\\[\\@supports\\(display\\:grid\\)\\]\\:grid";
  has "[@supports(backdrop-filter:blur(0))]:backdrop-blur"
    "@supports(-webkit-backdrop-filter:blur(0))or (backdrop-filter:blur(0))";
  (* an unprefixed property stays a single test *)
  has "supports-[display:grid]:flex" "@supports(display:grid)"

(* [in-focus] scopes to an ancestor in that state, the same state names the
   group and peer variants take. It used to be an unknown class: only the
   bracket and data spellings were read. *)
let test_in_state_variant () =
  let css cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string ~minify:true
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  let has cls affix =
    check bool cls true (Astring.String.is_infix ~affix (css cls))
  in
  has "in-focus:opacity-100" ":where(:focus) .in-focus\\:opacity-100";
  has "in-checked:flex" ":where(:checked) .in-checked\\:flex";
  (* in-hover gates on the pointer, as hover itself does *)
  has "in-hover:flex" "@media(hover:hover)"

(* [before:]/[after:] add the content declaration the pseudo-element needs, but
   a content-* utility already brings its own: adding a second one left it
   declared twice. A utility that sets content to something else (content-none)
   still gets the var indirection. *)
let test_pseudo_element_content_once () =
  let css cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string ~minify:true
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  let occurrences sub str =
    let n = String.length sub in
    let rec go i acc =
      if i + n > String.length str then acc
      else if String.sub str i n = sub then go (i + n) (acc + 1)
      else go (i + 1) acc
    in
    go 0 0
  in
  check int "content declared once" 1
    (occurrences "content:var(--tw-content)" (css "after:content-['x']"));
  check int "a plain utility still gets one" 1
    (occurrences "content:var(--tw-content)" (css "after:underline"))

(* An attribute-style variant (aria/data/has) builds its own selector, so it
   used to discard whatever an inner variant had already done: the [:hover], the
   child combinator, the second attribute. *)
let test_attribute_variant_keeps_inner () =
  let css cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string ~minify:true
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  let has cls affix =
    check bool cls true (Astring.String.is_infix ~affix (css cls))
  in
  has "aria-selected:hover:underline" "[aria-selected=true]:hover";
  has "data-[closed]:data-[enter]:-translate-x-8" "[data-closed][data-enter]";
  has "has-checked:hover:bg-indigo-500" ":has(:checked):hover";
  has "aria-selected:*:font-medium" "[aria-selected=true]>*";
  (* the inner [hover:] keeps its own @media gate under an outer variant *)
  has "disabled:hover:bg-indigo-500" "(hover:hover)";
  has "has-checked:hover:bg-indigo-500" "(hover:hover)"

(* [not-] over a variant that anchors the class under an ancestor negates the
   ancestor relation, and over an inner variant it keeps that variant's own
   selector work. *)
let test_not_variant_keeps_inner () =
  let css cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string ~minify:true
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  let has cls affix =
    check bool cls true (Astring.String.is_infix ~affix (css cls))
  in
  has "not-in-data-open:hidden" ":not(:where([data-open]) *)";
  has "not-checked:before:hidden" ":not(:checked):before";
  has "not-data-focus:not-has-checked:ring-inset"
    ":not([data-focus]):not(:has(:checked))"

(* The in-/named-group/peer routes build their selector from the bare class too,
   so an outer one used to drop the anchor an inner arbitrary selector had put
   in place. *)
let test_in_variant_keeps_inner () =
  let css cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string ~minify:true
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  let has cls affix =
    check bool cls true (Astring.String.is_infix ~affix (css cls))
  in
  has "in-data-stack:[:first-child>&]:underline"
    ":first-child>:is(:where([data-stack]) .in-data-stack";
  (* prose's own [:where(.prose > :last-child)] still only gets renamed *)
  has "hover:prose" ":where(.hover\\:prose>:last-child)"

(* An arbitrary-selector variant anchors the utility's class, so when an inner
   variant has already moved the subject the anchor belongs at the class's own
   position - not wrapped around everything the inner variant built. *)
let test_arbitrary_anchor_over_child () =
  let css cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string ~minify:true
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  check bool "the child variant's tail stays outermost" true
    (Astring.String.is_infix
       ~affix:":is(:last-child>:is(:where([data-stack]) .in-data-stack"
       (css "in-data-stack:[:last-child>&]:*:rounded-b-xl"))

(* [has-<variant>] takes any variant, not only a state name or a bracket: its
   own selector is what goes inside [:has()]. A scoped variant contributes its
   whole relative selector, so it composes both ways. *)
let test_has_variant () =
  let css cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string ~minify:true
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  let has cls affix =
    check bool cls true (Astring.String.is_infix ~affix (css cls))
  in
  has "has-peer-checked:underline" ":has(:is(:where(.peer):checked~*))";
  has "group-not-has-peer-not-data-active:underline"
    ":is(:where(.group):not(:has(:is(:where(.peer):not([data-active])~*))) *)";
  (* a named group scopes on the marker class, and the whole relative selector
     is what [:has()] sees *)
  has "has-group-focus/name:underline"
    ":has(:is(:where(.group\\/name):focus *))"

(* An inner media query's nested blocks hold the utility's class too, so an
   outer responsive variant has to rename it there as well: [sm:] used to drop
   out of the class name whenever [hover:] had wrapped the rule in its own media
   block. *)
let test_outer_media_renames_nested () =
  let css cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string ~minify:true
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  let has cls affix =
    check bool cls true (Astring.String.is_infix ~affix (css cls))
  in
  has "sm:motion-reduce:hover:translate-y-0"
    ".sm\\:motion-reduce\\:hover\\:translate-y-0:hover";
  has "sm:dark:hover:underline" ".sm\\:dark\\:hover\\:underline:hover"

let tests =
  [
    test_case "arbitrary selector combinator variants" `Quick
      test_arbitrary_selector_combinator;
    test_case "arbitrary selector stacks with other variants" `Quick
      test_arbitrary_selector_stacking;
    test_case "pseudo-element content once" `Quick
      test_pseudo_element_content_once;
    test_case "in-state variant" `Quick test_in_state_variant;
    test_case "bracketed at-rule variant" `Quick test_bracketed_at_rule_variant;
    test_case "opacity @supports under a variant" `Quick
      test_opacity_supports_under_variant;
    test_case "bare arbitrary selector variant" `Quick
      test_bare_arbitrary_selector_variant;
    test_case "outer variant over child and pseudo-element" `Quick
      test_outer_variant_over_child_and_pseudo;
    test_case "opacity color variant does not leak base rule" `Quick
      test_opacity_color_variant_no_leak;
    test_case "hover:dark keeps hover media wrapper" `Quick
      test_hover_dark_media_wrapper;
    test_case "attribute variant keeps the inner selector" `Quick
      test_attribute_variant_keeps_inner;
    test_case "not- variant keeps the inner selector" `Quick
      test_not_variant_keeps_inner;
    test_case "in- variant keeps the inner selector" `Quick
      test_in_variant_keeps_inner;
    test_case "arbitrary anchor over a child variant" `Quick
      test_arbitrary_anchor_over_child;
    test_case "has- takes any variant" `Quick test_has_variant;
    test_case "outer media renames the nested class" `Quick
      test_outer_media_renames_nested;
    test_case "extract selector props - basic" `Quick
      check_extract_selector_props;
    test_case "extract selector props - hover" `Quick check_extract_hover;
    test_case "extract selector props - responsive" `Quick
      check_extract_responsive;
    test_case "extract selector props - responsive md" `Quick
      check_extract_responsive_md;
    test_case "extract selector props - responsive lg" `Quick
      check_extract_responsive_lg;
    test_case "extract selector props - responsive xl" `Quick
      check_extract_responsive_xl;
    test_case "extract selector props - responsive 2xl" `Quick
      check_extract_responsive_2xl;
    test_case "escape class name" `Quick check_escape_class_name;
    test_case "modifier_to_rule" `Quick test_modifier_to_rule;
  ]

let suite = ("rule", tests)
