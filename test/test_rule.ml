module Css = Cascade.Css
open Alcotest
open Tw.Output
open Tw.Color
open Tw.Backgrounds
open Tw.Padding
open Tw.Modifiers

(* ===== Tests ===== *)

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

(* [escape_class_name] strips the dot off cascade's selector printer, so what it
   returns is the spelling that lands in the sheet. Cascade's ident escaper,
   [Parser.escape_ident], is not interchangeable with it. Both spell a name that
   decodes back to the source, so markup matches either, but they pick different
   escapes: above U+007F the printer hex-escapes where the ident escaper keeps
   the source bytes, which is the spelling Tailwind prints, and on a leading
   dash-digit, which has to be broken up or the selector re-tokenises as a
   number, the printer escapes the dash and the ident escaper the digit. *)
let check_escape_class_name_hex_escapes () =
  check string "breaks up a leading dash-digit" "\\2d 4xl"
    (Tw.Rule.escape_class_name "-4xl");
  check string "hex-escapes above U+007F" "aria-\\[\\e9 tat\\]\\:flex"
    (Tw.Rule.escape_class_name "aria-[état]:flex")

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
  check_sheet "[&>div:first-child]:ring-2"
    {|@layer properties,theme,components,utilities;@layer properties{@supports(((-webkit-hyphens:none)) and (not (margin-trim:inline)))or ((-moz-orient:inline) and (not (color:rgb(from red r g b)))){*,:before,:after,::backdrop{--tw-shadow:0 0 #0000;--tw-inset-shadow:0 0 #0000;--tw-ring-shadow:0 0 #0000;--tw-inset-ring-shadow:0 0 #0000;--tw-ring-offset-shadow:0 0 #0000;--tw-shadow-color:initial;--tw-shadow-alpha:100%;--tw-inset-shadow-color:initial;--tw-inset-shadow-alpha:100%;--tw-ring-color:initial;--tw-inset-ring-color:initial;--tw-ring-inset:initial;--tw-ring-offset-width:0px;--tw-ring-offset-color:#fff}}}@layer theme;@layer components;@layer utilities{.\[\&\>div\:first-child\]\:ring-2>div:first-child{--tw-ring-shadow:var(--tw-ring-inset,) 0 0 0 calc(2px + var(--tw-ring-offset-width)) var(--tw-ring-color,currentcolor);box-shadow:var(--tw-inset-shadow),var(--tw-inset-ring-shadow),var(--tw-ring-offset-shadow),var(--tw-ring-shadow),var(--tw-shadow)}}@property --tw-shadow{syntax:"*";inherits:false;initial-value:0 0 #0000}@property --tw-inset-shadow{syntax:"*";inherits:false;initial-value:0 0 #0000}@property --tw-ring-shadow{syntax:"*";inherits:false;initial-value:0 0 #0000}@property --tw-inset-ring-shadow{syntax:"*";inherits:false;initial-value:0 0 #0000}@property --tw-ring-offset-shadow{syntax:"*";inherits:false;initial-value:0 0 #0000}@property --tw-shadow-color{syntax:"*";inherits:false}@property --tw-shadow-alpha{syntax:"<percentage>";inherits:false;initial-value:100%}@property --tw-inset-shadow-color{syntax:"*";inherits:false}@property --tw-inset-shadow-alpha{syntax:"<percentage>";inherits:false;initial-value:100%}@property --tw-ring-color{syntax:"*";inherits:false}@property --tw-inset-ring-color{syntax:"*";inherits:false}@property --tw-ring-inset{syntax:"*";inherits:false}@property --tw-ring-offset-width{syntax:"<length>";inherits:false;initial-value:0}@property --tw-ring-offset-color{syntax:"*";inherits:false;initial-value:#fff}|};
  check_utilities "[&+p]:underline"
    {|.\[\&\+p\]\:underline+p{text-decoration-line:underline}|};
  check_utilities "[&_p]:underline"
    {|.\[\&_p\]\:underline p{text-decoration-line:underline}|}

(* [&] is the CSS nesting anchor and stands for the utility's own class, so it
   is substituted over the parsed selector. Rewriting the [&] bytes of the
   source text and re-parsing the result also rewrote an [&] that was part of a
   quoted attribute value. *)
let test_arbitrary_anchor_in_quoted_value () =
  (* This one stays on a substring. tw's library writes the attribute value
     quoted, exactly as the pinned CLI does, but the optimizer the [tw] binary
     runs respells it as [data-x=a\&b], so [--diff] reports a difference and the
     sheet cannot be pinned as agreed output. *)
  let has cls affix =
    check bool cls true (Astring.String.is_infix ~affix (sheet cls))
  in
  has {|[&[data-x="a&b"]]:flex|}
    {|.\[\&\[data-x\=\"a\&b\"\]\]\:flex[data-x="a&b"]|};
  (* the spellings that already worked stay byte-identical *)
  check_utilities "[&>*]:flex" {|.\[\&\>\*\]\:flex>*{display:flex}|};
  check_utilities "[&_p]:flex" {|.\[\&_p\]\:flex p{display:flex}|};
  check_utilities "[&:hover]:flex" {|.\[\&\:hover\]\:flex:hover{display:flex}|};
  check_utilities "[input&]:flex" {|input.\[input\&\]\:flex{display:flex}|};
  check_utilities "[p.foo]:flex" {|.\[p\.foo\]\:flex:is(p.foo){display:flex}|};
  check_utilities "[.line]:block" {|.\[\.line\]\:block.line{display:block}|}

(* Regression: an opacity color emits a progressive-enhancement @supports block.
   Under a variant, that block must stay scoped to the variant instead of
   leaking a bare base-class rule. dark:text-white/80 previously emitted a
   top-level .text-white/80 @supports rule alongside the correct
   .dark:text-white/80. *)
let test_opacity_color_variant_no_leak () =
  (* Every rule the sheet holds is named here, so a leaked [.text-white] rule
     has nowhere to hide. *)
  check_sheet "dark:text-white/80"
    {|@layer theme,components,utilities;@layer theme{:root,:host{--color-white:#fff}}@layer components;@layer utilities{@media(prefers-color-scheme:dark){.dark\:text-white\/80{color:#fffc}@supports(color:color-mix(in lab,red,red)){.dark\:text-white\/80{color:color-mix(in oklab,var(--color-white) 80%,transparent)}}}}|};
  check_sheet "hover:text-red-500/50"
    {|@layer theme,components,utilities;@layer theme{:root,:host{--color-red-500:oklch(63.7%.237 25.331)}}@layer components;@layer utilities{@media(hover:hover){.hover\:text-red-500\/50:hover{color:#fb2c3680}@supports(color:color-mix(in lab,red,red)){.hover\:text-red-500\/50:hover{color:color-mix(in oklab,var(--color-red-500) 50%,transparent)}}}}|}

(* Regression: a stacked hover:dark:* variant must keep the @media (hover:hover)
   wrapper (nested inside the dark media), matching Tailwind. tw used to emit a
   bare @media (dark) block, so the hover style also applied on touch
   devices. *)
let test_hover_dark_media_wrapper () =
  check_sheet "hover:dark:text-white"
    {|@layer theme,components,utilities;@layer theme{:root,:host{--color-white:#fff}}@layer components;@layer utilities{@media(hover:hover){@media(prefers-color-scheme:dark){.hover\:dark\:text-white:hover{color:var(--color-white)}}}}|}

(* An at-rule variant outside [group-hover:] must keep the inner variant's
   pointer-capability gate inside its own block. These wrappers used to consume
   the selector and declarations but not [has_hover], so the style matched on
   touch devices. Both named and bracketed spellings take the same route. *)
let test_at_rule_keeps_inner_hover_gate () =
  check_utilities "supports-grid:group-hover:flex-row"
    {|@supports(grid:var(--tw)){@media(hover:hover){.supports-grid\:group-hover\:flex-row:is(:where(.group):hover *){flex-direction:row}}}|};
  check_utilities "supports-[display:grid]:group-hover:flex-row"
    {|@supports(display:grid){@media(hover:hover){.supports-\[display\:grid\]\:group-hover\:flex-row:is(:where(.group):hover *){flex-direction:row}}}|};
  check_utilities "[@supports(display:grid)]:group-hover:flex-row"
    {|@supports(display:grid){@media(hover:hover){.\[\@supports\(display\:grid\)\]\:group-hover\:flex-row:is(:where(.group):hover *){flex-direction:row}}}|};
  check_utilities "starting:group-hover:flex-row"
    {|@starting-style{@media(hover:hover){.starting\:group-hover\:flex-row:is(:where(.group):hover *){flex-direction:row}}}|};
  check_utilities "[@starting-style]:group-hover:flex-row"
    {|@starting-style{@media(hover:hover){.\[\@starting-style\]\:group-hover\:flex-row:is(:where(.group):hover *){flex-direction:row}}}|};
  check_utilities "sm:supports-[display:grid]:group-hover:flex-row"
    {|@media(min-width:40rem){@supports(display:grid){@media(hover:hover){.sm\:supports-\[display\:grid\]\:group-hover\:flex-row:is(:where(.group):hover *){flex-direction:row}}}}|};
  check_utilities "first:supports-[display:grid]:group-hover:flex-row"
    {|@supports(display:grid){@media(hover:hover){.first\:supports-\[display\:grid\]\:group-hover\:flex-row:first-child:is(:where(.group):hover *){flex-direction:row}}}|};
  check_utilities "sm:starting:group-hover:flex-row"
    {|@media(min-width:40rem){@starting-style{@media(hover:hover){.sm\:starting\:group-hover\:flex-row:is(:where(.group):hover *){flex-direction:row}}}}|};
  check_utilities "first:[@starting-style]:group-hover:flex-row"
    {|@starting-style{@media(hover:hover){.first\:\[\@starting-style\]\:group-hover\:flex-row:first-child:is(:where(.group):hover *){flex-direction:row}}}|};
  check_utilities "first:@sm:group-hover:flex-row"
    {|@container(width>=24rem){@media(hover:hover){.first\:\@sm\:group-hover\:flex-row:first-child:is(:where(.group):hover *){flex-direction:row}}}|};
  check_utilities "sm:@sm:group-hover:flex-row"
    {|@media(min-width:40rem){@container(width>=24rem){@media(hover:hover){.sm\:\@sm\:group-hover\:flex-row:is(:where(.group):hover *){flex-direction:row}}}}|};
  check_utilities "supports-[display:grid]:starting:group-hover:flex-row"
    {|@supports(display:grid){@starting-style{@media(hover:hover){.supports-\[display\:grid\]\:starting\:group-hover\:flex-row:is(:where(.group):hover *){flex-direction:row}}}}|};
  check_utilities "starting:supports-[display:grid]:group-hover:flex-row"
    {|@starting-style{@supports(display:grid){@media(hover:hover){.starting\:supports-\[display\:grid\]\:group-hover\:flex-row:is(:where(.group):hover *){flex-direction:row}}}}|};
  check_utilities "supports-[display:grid]:@sm:group-hover:flex-row"
    {|@supports(display:grid){@container(width>=24rem){@media(hover:hover){.supports-\[display\:grid\]\:\@sm\:group-hover\:flex-row:is(:where(.group):hover *){flex-direction:row}}}}|}

(* A selector-building outer variant must retain the capability gate carried by
   an inner peer-hover rule. [not-[:hover]] takes a dedicated multi-rule route,
   which used to keep the transformed selector but drop [has_hover]. *)
let test_selector_variant_keeps_inner_hover_gate () =
  (* Each of these renders the same sheet but for its selector: touch-pan-x's
     property defaults, then the rule inside [@media(hover:hover)]. Spelling the
     parts they share once is what leaves the selector legible. *)
  let pan_properties =
    {|@layer properties,theme,components,utilities;@layer properties{@supports(((-webkit-hyphens:none)) and (not (margin-trim:inline)))or ((-moz-orient:inline) and (not (color:rgb(from red r g b)))){*,:before,:after,::backdrop{--tw-pan-y:initial;--tw-pinch-zoom:initial;--tw-pan-x:initial}}}@layer theme;@layer components;@layer utilities{@media(hover:hover){|}
  in
  let pan_rule =
    {|{--tw-pan-x:pan-x;touch-action:var(--tw-pan-x,) var(--tw-pan-y,) var(--tw-pinch-zoom,)}}}@property --tw-pan-y{syntax:"*";inherits:false}@property --tw-pinch-zoom{syntax:"*";inherits:false}@property --tw-pan-x{syntax:"*";inherits:false}|}
  in
  let gated cls selector =
    check_sheet cls (pan_properties ^ selector ^ pan_rule)
  in
  gated "not-[:hover]:peer-hover:touch-pan-x"
    {|.not-\[\:hover\]\:peer-hover\:touch-pan-x:not(:hover):is(:where(.peer):hover~*)|};
  gated "not-focus:peer-hover:touch-pan-x"
    {|.not-focus\:peer-hover\:touch-pan-x:not(:focus):is(:where(.peer):hover~*)|};
  gated "group-not-focus:peer-hover:touch-pan-x"
    {|.group-not-focus\:peer-hover\:touch-pan-x:is(:where(.group):not(:focus) *):is(:where(.peer):hover~*)|};
  gated "peer-not-focus:peer-hover:touch-pan-x"
    {|.peer-not-focus\:peer-hover\:touch-pan-x:is(:where(.peer):not(:focus)~*):is(:where(.peer):hover~*)|};
  gated "group-focus/foo:peer-hover:touch-pan-x"
    {|.group-focus\/foo\:peer-hover\:touch-pan-x:is(:where(.group\/foo):focus *):is(:where(.peer):hover~*)|};
  gated "peer-focus/foo:peer-hover:touch-pan-x"
    {|.peer-focus\/foo\:peer-hover\:touch-pan-x:is(:where(.peer\/foo):focus~*):is(:where(.peer):hover~*)|};
  (* These two stay on a substring. tw anchors the scope inside an [:is()] -
     [:is(:where(.parent) .in-\[\.parent\]\:peer-hover\:touch-pan-x)] - where
     the pinned CLI writes the descendant relation bare, so [--diff] reports a
     changed selector and neither sheet can be pinned as agreed output. *)
  List.iter
    (fun cls ->
      check bool cls true
        (Astring.String.is_infix ~affix:"@media(hover:hover){" (sheet cls)))
    [
      "in-[.parent]:peer-hover:touch-pan-x";
      "in-data-open:peer-hover:touch-pan-x";
    ]

(* An outer variant has to find the class the inner one produced. The child
   variant buries it inside an [:is] with a child combinator and the
   pseudo-element variants report the class they prefixed, so both used to be
   invisible: the outer variant dropped out of the class name, and stacking two
   child variants collapsed to one. *)
let test_outer_variant_over_child_and_pseudo () =
  check_sheet "max-sm:after:block"
    {|@layer properties,theme,components,utilities;@layer properties{@supports(((-webkit-hyphens:none)) and (not (margin-trim:inline)))or ((-moz-orient:inline) and (not (color:rgb(from red r g b)))){*,:before,:after,::backdrop{--tw-content:""}}}@layer theme;@layer components;@layer utilities{@media not all and (min-width:40rem){.max-sm\:after\:block:after{content:var(--tw-content);display:block}}}@property --tw-content{syntax:"*";inherits:false;initial-value:""}|};
  check_sheet "hover:before:underline"
    {|@layer properties,theme,components,utilities;@layer properties{@supports(((-webkit-hyphens:none)) and (not (margin-trim:inline)))or ((-moz-orient:inline) and (not (color:rgb(from red r g b)))){*,:before,:after,::backdrop{--tw-content:""}}}@layer theme;@layer components;@layer utilities{@media(hover:hover){.hover\:before\:underline:hover:before{content:var(--tw-content);text-decoration-line:underline}}}@property --tw-content{syntax:"*";inherits:false;initial-value:""}|};
  check_utilities "sm:*:rotate-0"
    {|@media(min-width:40rem){:is(.sm\:\*\:rotate-0>*){rotate:0deg}}|};
  check_utilities "hover:*:underline"
    {|@media(hover:hover){:is(.hover\:\*\:underline:hover>*){text-decoration-line:underline}}|};
  check_utilities "*:*:grow" {|:is(:is(.\*\:\*\:grow>*)>*){flex-grow:1}|}

(* An arbitrary variant with no [&] anchor compounds onto the utility's own
   class: [[.line]] attaches directly, a type selector goes in an [:is()] since
   it cannot follow a class. One that is not a single compound ([[>img]]) is not
   a variant at all. *)
let test_bare_arbitrary_selector_variant () =
  check_utilities "[.line]:block" {|.\[\.line\]\:block.line{display:block}|};
  check_sheet "[code]:pr-4"
    {|@layer theme,components,utilities;@layer theme{:root,:host{--spacing:.25rem}}@layer components;@layer utilities{.\[code\]\:pr-4:is(code){padding-right:calc(var(--spacing)*4)}}|};
  check_sheet "**:[code]:pr-4"
    {|@layer theme,components,utilities;@layer theme{:root,:host{--spacing:.25rem}}@layer components;@layer utilities{:is(.\*\*\:\[code\]\:pr-4 *):is(code){padding-right:calc(var(--spacing)*4)}}|};
  check bool "[>img] is not a variant" true
    (Result.is_error (Tw.of_string "[>img]:flex"))

(* An arbitrary variant stacks with the variants beside it. What the inner
   variant compounded onto the class belongs on the element this one makes the
   subject: [[&_p]:first:] matches the first [p], not a [p] under a first child.
   A responsive variant in the chain used to drop the arbitrary selector
   entirely, since the media path rebuilds the selector from a spelling [[svg]]
   has none of. *)
let test_arbitrary_selector_stacking () =
  check_sheet "[svg]:first:size-4"
    {|@layer theme,components,utilities;@layer theme{:root,:host{--spacing:.25rem}}@layer components;@layer utilities{.\[svg\]\:first\:size-4:is(svg):first-child{width:calc(var(--spacing)*4);height:calc(var(--spacing)*4)}}|};
  check_sheet "[&_p]:first:size-4"
    {|@layer theme,components,utilities;@layer theme{:root,:host{--spacing:.25rem}}@layer components;@layer utilities{.\[\&_p\]\:first\:size-4 p:first-child{width:calc(var(--spacing)*4);height:calc(var(--spacing)*4)}}|};
  check_sheet "[svg]:sm:size-4"
    {|@layer theme,components,utilities;@layer theme{:root,:host{--spacing:.25rem}}@layer components;@layer utilities{@media(min-width:40rem){.\[svg\]\:sm\:size-4:is(svg){width:calc(var(--spacing)*4);height:calc(var(--spacing)*4)}}}|};
  check_sheet "**:[svg]:first:sm:size-4"
    {|@layer theme,components,utilities;@layer theme{:root,:host{--spacing:.25rem}}@layer components;@layer utilities{@media(min-width:40rem){:is(.\*\*\:\[svg\]\:first\:sm\:size-4 *):is(svg):first-child{width:calc(var(--spacing)*4);height:calc(var(--spacing)*4)}}}|}

let occurrences sub str =
  let n = String.length sub in
  let rec go i acc =
    if i + n > String.length str then acc
    else if String.sub str i n = sub then go (i + n) (acc + 1)
    else go (i + 1) acc
  in
  go 0 0

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

(* A variant whose own effect is an at-rule wraps both halves of an opacity
   colour. The modern half still needs its inner color-mix feature query; losing
   it makes the nested output differ from Tailwind and leaves the enhancement at
   the wrong structural depth. Tailwind opens the variant's at-rule once and
   holds the fallback and the feature query inside it, in that order. *)
let test_opacity_supports_inside_at_rule_variant () =
  let css cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string ~minify:true
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  let modern = "@supports(color:color-mix(in lab,red,red)){" in
  let nested cls wrapper =
    let s = css cls in
    let opening = wrapper ^ "{" in
    check int (cls ^ ": one wrapper") 1 (occurrences opening s);
    check bool cls true
      (match
         ( Astring.String.find_sub ~sub:opening s,
           Astring.String.find_sub ~sub:modern s )
       with
      | Some outer, Some inner -> outer < inner
      | _ -> false)
  in
  nested "supports-backdrop-filter:bg-black/25"
    "@supports(backdrop-filter:var(--tw))";
  nested "@sm:bg-black/25" "@container(width>=24rem)";
  let starting = css "starting:bg-black/25" in
  let position affix = Astring.String.find_sub ~sub:affix starting in
  check bool "starting:bg-black/25" true
    (match
       ( position "@starting-style{",
         position "@supports(color:color-mix(in lab,red,red)){" )
     with
    | Some outer, Some inner -> outer < inner
    | _ -> false)

(* An at-rule written in brackets wraps the utility rather than selecting it,
   and keeps its own spelling in the class name: reading it as the [supports-]
   variant gave a class the HTML would not match. Its condition is emitted as
   written, prefixed property or not. *)
let test_bracketed_at_rule_variant () =
  check_utilities "[@starting-style]:opacity-0"
    {|@starting-style{.\[\@starting-style\]\:opacity-0{opacity:0}}|};
  check_utilities "[@supports(display:grid)]:grid"
    {|@supports(display:grid){.\[\@supports\(display\:grid\)\]\:grid{display:grid}}|};
  check_sheet "[@supports(backdrop-filter:blur(0))]:backdrop-blur"
    {|@layer properties,theme,components,utilities;@layer properties{@supports(((-webkit-hyphens:none)) and (not (margin-trim:inline)))or ((-moz-orient:inline) and (not (color:rgb(from red r g b)))){*,:before,:after,::backdrop{--tw-backdrop-blur:initial;--tw-backdrop-brightness:initial;--tw-backdrop-contrast:initial;--tw-backdrop-grayscale:initial;--tw-backdrop-hue-rotate:initial;--tw-backdrop-invert:initial;--tw-backdrop-opacity:initial;--tw-backdrop-saturate:initial;--tw-backdrop-sepia:initial}}}@layer theme;@layer components;@layer utilities{@supports(backdrop-filter:blur(0)){.\[\@supports\(backdrop-filter\:blur\(0\)\)\]\:backdrop-blur{--tw-backdrop-blur:blur(8px);-webkit-backdrop-filter:var(--tw-backdrop-blur,)var(--tw-backdrop-brightness,)var(--tw-backdrop-contrast,)var(--tw-backdrop-grayscale,)var(--tw-backdrop-hue-rotate,)var(--tw-backdrop-invert,)var(--tw-backdrop-opacity,)var(--tw-backdrop-saturate,)var(--tw-backdrop-sepia,);backdrop-filter:var(--tw-backdrop-blur,)var(--tw-backdrop-brightness,)var(--tw-backdrop-contrast,)var(--tw-backdrop-grayscale,)var(--tw-backdrop-hue-rotate,)var(--tw-backdrop-invert,)var(--tw-backdrop-opacity,)var(--tw-backdrop-saturate,)var(--tw-backdrop-sepia,)}}}@property --tw-backdrop-blur{syntax:"*";inherits:false}@property --tw-backdrop-brightness{syntax:"*";inherits:false}@property --tw-backdrop-contrast{syntax:"*";inherits:false}@property --tw-backdrop-grayscale{syntax:"*";inherits:false}@property --tw-backdrop-hue-rotate{syntax:"*";inherits:false}@property --tw-backdrop-invert{syntax:"*";inherits:false}@property --tw-backdrop-opacity{syntax:"*";inherits:false}@property --tw-backdrop-saturate{syntax:"*";inherits:false}@property --tw-backdrop-sepia{syntax:"*";inherits:false}|};
  check_utilities "supports-[display:grid]:flex"
    {|@supports(display:grid){.supports-\[display\:grid\]\:flex{display:flex}}|}

(* [in-focus] scopes to an ancestor in that state, the same state names the
   group and peer variants take. It used to be an unknown class: only the
   bracket and data spellings were read. *)
let test_in_state_variant () =
  check_utilities "in-focus:opacity-100"
    {|:where(:focus) .in-focus\:opacity-100{opacity:1}|};
  check_utilities "in-checked:flex"
    {|:where(:checked) .in-checked\:flex{display:flex}|};
  (* in-hover gates on the pointer, as hover itself does *)
  check_utilities "in-hover:flex"
    {|@media(hover:hover){:where(:hover) .in-hover\:flex{display:flex}}|}

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
  check_utilities "aria-selected:hover:underline"
    {|@media(hover:hover){.aria-selected\:hover\:underline[aria-selected=true]:hover{text-decoration-line:underline}}|};
  check_sheet "data-[closed]:data-[enter]:-translate-x-8"
    {|@layer properties,theme,components,utilities;@layer properties{@supports(((-webkit-hyphens:none)) and (not (margin-trim:inline)))or ((-moz-orient:inline) and (not (color:rgb(from red r g b)))){*,:before,:after,::backdrop{--tw-translate-x:0;--tw-translate-y:0;--tw-translate-z:0}}}@layer theme{:root,:host{--spacing:.25rem}}@layer components;@layer utilities{.data-\[closed\]\:data-\[enter\]\:-translate-x-8[data-closed][data-enter]{--tw-translate-x:calc(var(--spacing)*-8);translate:var(--tw-translate-x)var(--tw-translate-y)}}@property --tw-translate-x{syntax:"*";inherits:false;initial-value:0}@property --tw-translate-y{syntax:"*";inherits:false;initial-value:0}@property --tw-translate-z{syntax:"*";inherits:false;initial-value:0}|};
  check_sheet "has-checked:hover:bg-indigo-500"
    {|@layer theme,components,utilities;@layer theme{:root,:host{--color-indigo-500:oklch(58.5%.233 277.117)}}@layer components;@layer utilities{@media(hover:hover){.has-checked\:hover\:bg-indigo-500:has(:checked):hover{background-color:var(--color-indigo-500)}}}|};
  check_sheet "aria-selected:*:font-medium"
    {|@layer properties,theme,components,utilities;@layer properties{@supports(((-webkit-hyphens:none)) and (not (margin-trim:inline)))or ((-moz-orient:inline) and (not (color:rgb(from red r g b)))){*,:before,:after,::backdrop{--tw-font-weight:initial}}}@layer theme{:root,:host{--font-weight-medium:500}}@layer components;@layer utilities{:is(.aria-selected\:\*\:font-medium[aria-selected=true]>*){--tw-font-weight:var(--font-weight-medium);font-weight:var(--font-weight-medium)}}@property --tw-font-weight{syntax:"*";inherits:false}|};
  (* the inner [hover:] keeps its own @media gate under an outer variant *)
  check_sheet "disabled:hover:bg-indigo-500"
    {|@layer theme,components,utilities;@layer theme{:root,:host{--color-indigo-500:oklch(58.5%.233 277.117)}}@layer components;@layer utilities{@media(hover:hover){.disabled\:hover\:bg-indigo-500:disabled:hover{background-color:var(--color-indigo-500)}}}|}

(* [not-] over a variant that anchors the class under an ancestor negates the
   ancestor relation, and over an inner variant it keeps that variant's own
   selector work. *)
let test_not_variant_keeps_inner () =
  check_utilities "not-in-data-open:hidden"
    {|.not-in-data-open\:hidden:not(:where([data-open]) *){display:none}|};
  check_sheet "not-checked:before:hidden"
    {|@layer properties,theme,components,utilities;@layer properties{@supports(((-webkit-hyphens:none)) and (not (margin-trim:inline)))or ((-moz-orient:inline) and (not (color:rgb(from red r g b)))){*,:before,:after,::backdrop{--tw-content:""}}}@layer theme;@layer components;@layer utilities{.not-checked\:before\:hidden:not(:checked):before{content:var(--tw-content);display:none}}@property --tw-content{syntax:"*";inherits:false;initial-value:""}|};
  check_sheet "not-data-focus:not-has-checked:ring-inset"
    {|@layer properties,theme,components,utilities;@layer properties{@supports(((-webkit-hyphens:none)) and (not (margin-trim:inline)))or ((-moz-orient:inline) and (not (color:rgb(from red r g b)))){*,:before,:after,::backdrop{--tw-shadow:0 0 #0000;--tw-shadow-color:initial;--tw-shadow-alpha:100%;--tw-inset-shadow:0 0 #0000;--tw-inset-shadow-color:initial;--tw-inset-shadow-alpha:100%;--tw-ring-color:initial;--tw-ring-shadow:0 0 #0000;--tw-inset-ring-color:initial;--tw-inset-ring-shadow:0 0 #0000;--tw-ring-inset:initial;--tw-ring-offset-width:0px;--tw-ring-offset-color:#fff;--tw-ring-offset-shadow:0 0 #0000}}}@layer theme;@layer components;@layer utilities{.not-data-focus\:not-has-checked\:ring-inset:not([data-focus]):not(:has(:checked)){--tw-ring-inset:inset}}@property --tw-shadow{syntax:"*";inherits:false;initial-value:0 0 #0000}@property --tw-shadow-color{syntax:"*";inherits:false}@property --tw-shadow-alpha{syntax:"<percentage>";inherits:false;initial-value:100%}@property --tw-inset-shadow{syntax:"*";inherits:false;initial-value:0 0 #0000}@property --tw-inset-shadow-color{syntax:"*";inherits:false}@property --tw-inset-shadow-alpha{syntax:"<percentage>";inherits:false;initial-value:100%}@property --tw-ring-color{syntax:"*";inherits:false}@property --tw-ring-shadow{syntax:"*";inherits:false;initial-value:0 0 #0000}@property --tw-inset-ring-color{syntax:"*";inherits:false}@property --tw-inset-ring-shadow{syntax:"*";inherits:false;initial-value:0 0 #0000}@property --tw-ring-inset{syntax:"*";inherits:false}@property --tw-ring-offset-width{syntax:"<length>";inherits:false;initial-value:0}@property --tw-ring-offset-color{syntax:"*";inherits:false;initial-value:#fff}@property --tw-ring-offset-shadow{syntax:"*";inherits:false;initial-value:0 0 #0000}|}

(* The in-/named-group/peer routes build their selector from the bare class too,
   so an outer one used to drop the anchor an inner arbitrary selector had put
   in place. *)
let test_in_variant_keeps_inner () =
  check_utilities "in-data-stack:[:first-child>&]:underline"
    {|:first-child>:is(:where([data-stack]) .in-data-stack\:\[\:first-child\>\&\]\:underline){text-decoration-line:underline}|};
  (* prose's own [:where(.prose > :last-child)] still only gets renamed. This
     one stays on a substring: [prose] renders the whole typography component
     sheet, some fourteen thousand bytes of it, and the assertion is about one
     selector inside it. *)
  check bool "hover:prose" true
    (Astring.String.is_infix ~affix:{|:where(.hover\:prose>:last-child)|}
       (sheet "hover:prose"))

(* An arbitrary-selector variant anchors the utility's class, so when an inner
   variant has already moved the subject the anchor belongs at the class's own
   position - not wrapped around everything the inner variant built. *)
let test_arbitrary_anchor_over_child () =
  check_sheet "in-data-stack:[:last-child>&]:*:rounded-b-xl"
    {|@layer theme,components,utilities;@layer theme{:root,:host{--radius-xl:.75rem}}@layer components;@layer utilities{:is(:last-child>:is(:where([data-stack]) .in-data-stack\:\[\:last-child\>\&\]\:\*\:rounded-b-xl)>*){border-bottom-right-radius:var(--radius-xl);border-bottom-left-radius:var(--radius-xl)}}|}

(* [has-<variant>] takes any variant, not only a state name or a bracket: its
   own selector is what goes inside [:has()]. A scoped variant contributes its
   whole relative selector, so it composes both ways. *)
let test_has_variant () =
  check_utilities "has-peer-checked:underline"
    {|.has-peer-checked\:underline:has(:is(:where(.peer):checked~*)){text-decoration-line:underline}|};
  check_utilities "group-not-has-peer-not-data-active:underline"
    {|.group-not-has-peer-not-data-active\:underline:is(:where(.group):not(:has(:is(:where(.peer):not([data-active])~*))) *){text-decoration-line:underline}|};
  (* a named group scopes on the marker class, and the whole relative selector
     is what [:has()] sees *)
  check_utilities "has-group-focus/name:underline"
    {|.has-group-focus\/name\:underline:has(:is(:where(.group\/name):focus *)){text-decoration-line:underline}|}

(* An inner media query's nested blocks hold the utility's class too, so an
   outer responsive variant has to rename it there as well: [sm:] used to drop
   out of the class name whenever [hover:] had wrapped the rule in its own media
   block. *)
let test_outer_media_renames_nested () =
  check_utilities "sm:dark:hover:underline"
    {|@media(min-width:40rem){@media(prefers-color-scheme:dark){@media(hover:hover){.sm\:dark\:hover\:underline:hover{text-decoration-line:underline}}}}|};
  check_sheet "sm:motion-reduce:hover:translate-y-0"
    {|@layer properties,theme,components,utilities;@layer properties{@supports(((-webkit-hyphens:none)) and (not (margin-trim:inline)))or ((-moz-orient:inline) and (not (color:rgb(from red r g b)))){*,:before,:after,::backdrop{--tw-translate-x:0;--tw-translate-y:0;--tw-translate-z:0}}}@layer theme;@layer components;@layer utilities{@media(min-width:40rem){@media(prefers-reduced-motion:reduce){@media(hover:hover){.sm\:motion-reduce\:hover\:translate-y-0:hover{--tw-translate-y:0px;translate:var(--tw-translate-x)var(--tw-translate-y)}}}}}@property --tw-translate-x{syntax:"*";inherits:false;initial-value:0}@property --tw-translate-y{syntax:"*";inherits:false;initial-value:0}@property --tw-translate-z{syntax:"*";inherits:false;initial-value:0}|}

(* A hover-gated variant keeps every declaration in a nested [@media
   (hover:hover)] and leaves its wrapper carrying none. Rebuilding that
   wrapper's own rule unconditionally wrote a declarationless rule beside the
   real one, which the CLI does not emit and which no minified check sees. Each
   at-rule an outer variant can build has to skip it. *)
let test_hover_gate_leaves_no_empty_rule () =
  check_utilities "supports-[display:grid]:dark:hover:underline"
    {|@supports(display:grid){@media(prefers-color-scheme:dark){@media(hover:hover){.supports-\[display\:grid\]\:dark\:hover\:underline:hover{text-decoration-line:underline}}}}|};
  check_utilities "starting:dark:hover:underline"
    {|@starting-style{@media(prefers-color-scheme:dark){@media(hover:hover){.starting\:dark\:hover\:underline:hover{text-decoration-line:underline}}}}|};
  check_utilities "@md:dark:hover:underline"
    {|@container(width>=28rem){@media(prefers-color-scheme:dark){@media(hover:hover){.\@md\:dark\:hover\:underline:hover{text-decoration-line:underline}}}}|};
  (* Controls. [focus:] is not hover-gated, so the inner rule carries its own
     declarations and no wrapper is built empty; and an outer breakpoint over
     [@supports] already skipped it, which is why that shape never showed the
     surplus rule. *)
  check_utilities "md:dark:focus:underline"
    {|@media(min-width:48rem){@media(prefers-color-scheme:dark){.md\:dark\:focus\:underline:focus{text-decoration-line:underline}}}|};
  check_utilities "sm:supports-[display:grid]:hover:underline"
    {|@media(min-width:40rem){@supports(display:grid){@media(hover:hover){.sm\:supports-\[display\:grid\]\:hover\:underline:hover{text-decoration-line:underline}}}}|}

(* [starting:] and the bracketed at-rule variant wrap the utility in an at-rule,
   and rebuilding the selector from the bare class dropped whatever an inner
   variant had put on it. *)
let test_at_rule_keeps_inner () =
  check_utilities "starting:open:opacity-0"
    {|@starting-style{.starting\:open\:opacity-0:is([open],:popover-open,:open){opacity:0}}|};
  check_utilities "[@starting-style]:open:opacity-0"
    {|@starting-style{.\[\@starting-style\]\:open\:opacity-0:is([open],:popover-open,:open){opacity:0}}|}

(* A prose element variant rebuilt its selector from the bare class, which threw
   away whatever an inner variant had put on it: [prose-a:hover:text-red-500]
   lost its [:hover] and coloured every link, [prose-li:marker:text-green-500]
   lost its [::marker] and coloured the item's own text. *)
let test_prose_element_keeps_inner () =
  check_sheet "prose-a:hover:text-red-500"
    {|@layer theme,components,utilities;@layer theme{:root,:host{--color-red-500:oklch(63.7%.237 25.331)}}@layer components;@layer utilities{@media(hover:hover){.prose-a\:hover\:text-red-500 :where(a):not(:where([class~=not-prose],[class~=not-prose] *)):hover{color:var(--color-red-500)}}}|};
  check_sheet "prose-li:marker:text-green-500"
    {|@layer theme,components,utilities;@layer theme{:root,:host{--color-green-500:oklch(72.3%.219 149.579)}}@layer components;@layer utilities{.prose-li\:marker\:text-green-500 :where(li):not(:where([class~=not-prose],[class~=not-prose] *)) ::marker{color:var(--color-green-500)}.prose-li\:marker\:text-green-500 :where(li):not(:where([class~=not-prose],[class~=not-prose] *))::marker{color:var(--color-green-500)}.prose-li\:marker\:text-green-500 :where(li):not(:where([class~=not-prose],[class~=not-prose] *)) ::-webkit-details-marker{color:var(--color-green-500)}.prose-li\:marker\:text-green-500 :where(li):not(:where([class~=not-prose],[class~=not-prose] *))::-webkit-details-marker{color:var(--color-green-500)}}|};
  check_utilities "prose-p:first-line:uppercase"
    {|.prose-p\:first-line\:uppercase :where(p):not(:where([class~=not-prose],[class~=not-prose] *)):first-line{text-transform:uppercase}|};
  check_sheet "prose-code:after:content-['y']"
    {|@layer properties,theme,components,utilities;@layer properties{@supports(((-webkit-hyphens:none)) and (not (margin-trim:inline)))or ((-moz-orient:inline) and (not (color:rgb(from red r g b)))){*,:before,:after,::backdrop{--tw-content:""}}}@layer theme;@layer components;@layer utilities{.prose-code\:after\:content-\[\'y\'\] :where(code):not(:where([class~=not-prose],[class~=not-prose] *)):after{--tw-content:"y";content:var(--tw-content)}}@property --tw-content{syntax:"*";inherits:false;initial-value:""}|}

(* The reverse direction: a prose element variant over a rule that is already a
   media query built its selector from [Modifiers.to_selector], which returned
   the class alone, so the elements it targets went missing and the utility
   landed on the container. *)
let test_prose_element_over_media () =
  check_sheet "prose-p:md:text-lg"
    {|@layer theme,components,utilities;@layer theme{:root,:host{--text-lg:1.125rem;--text-lg--line-height:calc(1.75/1.125)}}@layer components;@layer utilities{@media(min-width:48rem){.prose-p\:md\:text-lg :where(p):not(:where([class~=not-prose],[class~=not-prose] *)){font-size:var(--text-lg);line-height:var(--tw-leading,var(--text-lg--line-height))}}}|};
  check_sheet "prose-a:dark:text-white"
    {|@layer theme,components,utilities;@layer theme{:root,:host{--color-white:#fff}}@layer components;@layer utilities{@media(prefers-color-scheme:dark){.prose-a\:dark\:text-white :where(a):not(:where([class~=not-prose],[class~=not-prose] *)){color:var(--color-white)}}}|}

(* [apply_modifier_to_rule] had no arm for a [@starting-style] rule, so an outer
   variant fell through the catch-all and went with it: [hover:starting:p-4]
   named its rule [.starting\\:p-4], a class the source never carries, and the
   rule matched nothing. Every other at-rule wrapper had an arm already. *)
let test_variant_outside_starting_style () =
  check_sheet "starting:p-4"
    {|@layer theme,components,utilities;@layer theme{:root,:host{--spacing:.25rem}}@layer components;@layer utilities{@starting-style{.starting\:p-4{padding:calc(var(--spacing)*4)}}}|};
  check_sheet "hover:starting:p-4"
    {|@layer theme,components,utilities;@layer theme{:root,:host{--spacing:.25rem}}@layer components;@layer utilities{@media(hover:hover){@starting-style{.hover\:starting\:p-4:hover{padding:calc(var(--spacing)*4)}}}}|};
  check_sheet "md:starting:p-4"
    {|@layer theme,components,utilities;@layer theme{:root,:host{--spacing:.25rem}}@layer components;@layer utilities{@media(min-width:48rem){@starting-style{.md\:starting\:p-4{padding:calc(var(--spacing)*4)}}}}|};
  check_sheet "first:starting:p-4"
    {|@layer theme,components,utilities;@layer theme{:root,:host{--spacing:.25rem}}@layer components;@layer utilities{@starting-style{.first\:starting\:p-4:first-child{padding:calc(var(--spacing)*4)}}}|};
  check_sheet "nth-3:starting:p-4"
    {|@layer theme,components,utilities;@layer theme{:root,:host{--spacing:.25rem}}@layer components;@layer utilities{@starting-style{.nth-3\:starting\:p-4:nth-child(3){padding:calc(var(--spacing)*4)}}}|};
  check_utilities "dark:starting:opacity-50"
    {|@media(prefers-color-scheme:dark){@starting-style{.dark\:starting\:opacity-50{opacity:.5}}}|}

(* The selector a variant's bracket spells reads [_] as a space and [\_] as a
   literal underscore, so a class or an attribute value carrying one is written
   with the escape. *)
let test_selector_underscore_escape () =
  check_utilities {|aria-[label=a\_b]:flex|}
    {|.aria-\[label\=a\\_b\]\:flex[aria-label=a_b]{display:flex}|};
  check_utilities {|not-[.a\_b]:flex|}
    {|.not-\[\.a\\_b\]\:flex:not(.a_b){display:flex}|};
  check_utilities {|[.a\_b_&]:flex|}
    {|.a_b .\[\.a\\_b_\&\]\:flex{display:flex}|};
  (* A bare [_] still stands for a space. This one stays on a substring: tw
     quotes the attribute value where the pinned CLI escapes the space
     ([aria-label=a\ b]), so [--diff] reports a changed selector. *)
  check bool "aria-[label=a_b]:flex" true
    (Astring.String.is_infix ~affix:{|[aria-label="a b"]|}
       (sheet "aria-[label=a_b]:flex"))

let tests =
  [
    test_case "selector underscore escape" `Quick
      test_selector_underscore_escape;
    test_case "arbitrary selector combinator variants" `Quick
      test_arbitrary_selector_combinator;
    test_case "arbitrary anchor inside a quoted value" `Quick
      test_arbitrary_anchor_in_quoted_value;
    test_case "arbitrary selector stacks with other variants" `Quick
      test_arbitrary_selector_stacking;
    test_case "pseudo-element content once" `Quick
      test_pseudo_element_content_once;
    test_case "in-state variant" `Quick test_in_state_variant;
    test_case "bracketed at-rule variant" `Quick test_bracketed_at_rule_variant;
    test_case "opacity @supports under a variant" `Quick
      test_opacity_supports_under_variant;
    test_case "opacity @supports inside an at-rule variant" `Quick
      test_opacity_supports_inside_at_rule_variant;
    test_case "bare arbitrary selector variant" `Quick
      test_bare_arbitrary_selector_variant;
    test_case "outer variant over child and pseudo-element" `Quick
      test_outer_variant_over_child_and_pseudo;
    test_case "opacity color variant does not leak base rule" `Quick
      test_opacity_color_variant_no_leak;
    test_case "hover:dark keeps hover media wrapper" `Quick
      test_hover_dark_media_wrapper;
    test_case "at-rule keeps inner hover media wrapper" `Quick
      test_at_rule_keeps_inner_hover_gate;
    test_case "selector variant keeps inner hover media wrapper" `Quick
      test_selector_variant_keeps_inner_hover_gate;
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
    test_case "a hover gate leaves no empty rule" `Quick
      test_hover_gate_leaves_no_empty_rule;
    test_case "at-rule variant keeps the inner selector" `Quick
      test_at_rule_keeps_inner;
    test_case "prose element variant keeps the inner selector" `Quick
      test_prose_element_keeps_inner;
    test_case "prose element variant over a media query" `Quick
      test_prose_element_over_media;
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
    test_case "escape class name hex escapes" `Quick
      check_escape_class_name_hex_escapes;
    test_case "modifier_to_rule" `Quick test_modifier_to_rule;
    test_case "a variant outside starting-style survives" `Quick
      test_variant_outside_starting_style;
  ]

let suite = ("rule", tests)
