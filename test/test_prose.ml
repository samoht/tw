module Css = Cascade.Css
(* Tests for prose typography utilities *)

open Tw

let test_classes () =
  Alcotest.(check string) "prose base class" "prose" (pp prose);
  Alcotest.(check string) "prose-sm class" "prose-sm" (pp prose_sm);
  Alcotest.(check string) "prose-lg class" "prose-lg" (pp prose_lg);
  Alcotest.(check string) "prose-xl class" "prose-xl" (pp prose_xl);
  Alcotest.(check string) "prose-2xl class" "prose-2xl" (pp prose_2xl);
  Alcotest.(check string) "prose-gray class" "prose-gray" (pp prose_gray);
  Alcotest.(check string) "prose-slate class" "prose-slate" (pp prose_slate)

let test_combinations () =
  let combined = to_classes [ prose; prose_lg ] in
  Alcotest.(check string) "prose with size" "prose prose-lg" combined;

  let with_theme = to_classes [ prose; prose_slate ] in
  Alcotest.(check string) "prose with theme" "prose prose-slate" with_theme;

  let complex = to_classes [ prose; prose_xl; prose_gray; mx_auto ] in
  Alcotest.(check string)
    "complex prose" "prose prose-xl prose-gray mx-auto" complex

let test_css_generation () =
  (* Test that prose generates CSS rules *)
  let css = to_css [ prose ] in
  let css_string = Css.to_string css in

  (* Check that CSS variables are included *)
  Alcotest.(check bool)
    "has prose body variable" true
    (Astring.String.is_infix ~affix:"--tw-prose-body" css_string);

  (* Check that prose class is generated *)
  Alcotest.(check bool)
    "has prose class" true
    (Astring.String.is_infix ~affix:".prose" css_string);

  (* Check that descendant selectors are generated *)
  Alcotest.(check bool)
    "has prose h1 selector" true
    (Astring.String.is_infix
       ~affix:".prose :where(h1):not(:where([class~=\"not-prose\"]" css_string)

let test_inline_styles () =
  (* Prose utilities can generate inline styles from their rules, but CSS
     variables are filtered out *)
  let inline = to_inline_style [ prose ] in
  Alcotest.(check bool)
    "prose generates inline styles" true
    (String.length inline > 0);

  (* Check that CSS variables are filtered out - no "--" should appear *)
  let has_css_vars = Astring.String.is_infix ~affix:"--" inline in
  Alcotest.(check bool) "no CSS variables in inline styles" false has_css_vars;

  (* Color variants only set CSS variables, so they have no inline styles *)
  let inline_gray = to_inline_style [ prose_gray ] in
  Alcotest.(check string) "prose-gray has no inline styles" "" inline_gray

(* prose-invert remaps the palette to the inverted vars; prose-orange overrides
   the link accent colours. Both used to be no-ops / unknown. *)
let test_color_variants () =
  let invert = Css.to_string (to_css ~base:false [ prose_invert ]) in
  Alcotest.(check bool)
    "prose-invert remaps body to the invert var" true
    (Astring.String.is_infix ~affix:"var(--tw-prose-invert-body)" invert);
  let orange = Css.to_string (to_css ~base:false [ prose_orange ]) in
  Alcotest.(check bool)
    "prose-orange sets the link accent" true
    (Astring.String.is_infix ~affix:"--tw-prose-links" orange
    && Astring.String.is_infix ~affix:"--tw-prose-invert-links" orange)

(* The colours [prose] sets on its own are the gray palette, normal and
   inverted. Nothing else in the suite reads them, so an edit to [gray_normal]
   or [gray_invert] could move the default sheet unremarked; these are the
   values it has to preserve. *)
let prose_root_variables () =
  Css.fold
    (fun acc stmt ->
      match Css.as_rule stmt with
      | Some (sel, decls, _) when Css.Selector.to_string sel = ".prose" ->
          acc
          @ List.filter
              (fun d -> String.starts_with ~prefix:"--tw-prose-" d)
              (List.map (Css.Declaration.to_string ~minify:false) decls)
      | _ -> acc)
    []
    (to_css ~base:false [ prose ])

let test_default_theme_colours () =
  Alcotest.(check (list string))
    "prose default colour variables"
    [
      "--tw-prose-body: oklch(37.3% .034 259.733)";
      "--tw-prose-headings: oklch(21% .034 264.665)";
      "--tw-prose-lead: oklch(44.6% .03 256.802)";
      "--tw-prose-links: oklch(21% .034 264.665)";
      "--tw-prose-bold: oklch(21% .034 264.665)";
      "--tw-prose-counters: oklch(55.1% .027 264.364)";
      "--tw-prose-bullets: oklch(87.2% .01 258.338)";
      "--tw-prose-hr: oklch(92.8% .006 264.531)";
      "--tw-prose-quotes: oklch(21% .034 264.665)";
      "--tw-prose-quote-borders: oklch(92.8% .006 264.531)";
      "--tw-prose-captions: oklch(55.1% .027 264.364)";
      "--tw-prose-kbd: oklch(21% .034 264.665)";
      "--tw-prose-kbd-shadows: oklab(21% -.00316127 -.0338527 / .1)";
      "--tw-prose-code: oklch(21% .034 264.665)";
      "--tw-prose-pre-code: oklch(92.8% .006 264.531)";
      "--tw-prose-pre-bg: oklch(27.8% .033 256.848)";
      "--tw-prose-th-borders: oklch(87.2% .01 258.338)";
      "--tw-prose-td-borders: oklch(92.8% .006 264.531)";
      "--tw-prose-invert-body: oklch(87.2% .01 258.338)";
      "--tw-prose-invert-headings: #ffffff";
      "--tw-prose-invert-lead: oklch(70.7% .022 261.325)";
      "--tw-prose-invert-links: #ffffff";
      "--tw-prose-invert-bold: #ffffff";
      "--tw-prose-invert-counters: oklch(70.7% .022 261.325)";
      "--tw-prose-invert-bullets: oklch(44.6% .03 256.802)";
      "--tw-prose-invert-hr: oklch(37.3% .034 259.733)";
      "--tw-prose-invert-quotes: oklch(96.7% .003 264.542)";
      "--tw-prose-invert-quote-borders: oklch(37.3% .034 259.733)";
      "--tw-prose-invert-captions: oklch(70.7% .022 261.325)";
      "--tw-prose-invert-kbd: #ffffff";
      "--tw-prose-invert-kbd-shadows: #ffffff1a";
      "--tw-prose-invert-code: #ffffff";
      "--tw-prose-invert-pre-code: oklch(87.2% .01 258.338)";
      "--tw-prose-invert-pre-bg: #00000080";
      "--tw-prose-invert-th-borders: oklch(44.6% .03 256.802)";
      "--tw-prose-invert-td-borders: oklch(37.3% .034 259.733)";
    ]
    (prose_root_variables ())

(* Real-CLI parity for the typography plugin. Until now prose had no comparison
   against the actual @tailwindcss/typography output (always enabled in
   Tailwind_gen) -- only checks of tw's own structure -- so the README's "fully
   supported" claim was unproven for the plugin. check_ordering_matches does a
   full canonical diff against the real CLI and fails on any difference, exactly
   as the forms suite does. *)
let test_size_parity () =
  Test_helpers.check_ordering_matches ~test_name:"prose sizes match Tailwind"
    [ prose; prose_sm; prose_lg; prose_xl; prose_2xl ]

(* Covers the five gray ramps (zinc/neutral/stone were stubs emitting nothing
   before this commit's parent), the invert remap and the orange accent. *)
let test_color_theme_parity () =
  Test_helpers.check_ordering_matches
    ~test_name:"prose colour themes match Tailwind"
    [
      prose_gray;
      prose_slate;
      prose_zinc;
      prose_neutral;
      prose_stone;
      prose_invert;
      prose_orange;
    ]

(* The markup the typography plugin actually targets. Nearly every rule it emits
   is a descendant selector - :where(h1), :where(a code), :where(tbody
   td:first-child), :where(.prose > ul > li p) - so an empty element matches
   none of them and comparing one says nothing about the plugin beyond its root
   declarations and --tw-prose-* bindings. Every element named in a typography
   selector appears here at least once, in a position where the structural parts
   of the selector (:first-child, + *, first-of-type, nesting) have something to
   select. *)
let prose_document =
  {html|
<h1>Heading one with <code>code</code> and <strong>strong</strong></h1>
<p class="lead">A lead paragraph.</p>
<p>Body copy with <a href="#">a link holding <code>code</code> and
  <strong>strong</strong></a>, <em>emphasis</em>, <kbd>Ctrl</kbd> and
  <code>inline code</code>.</p>
<h2>Heading two with <code>code</code> and <strong>strong</strong></h2>
<p>The paragraph directly after the h2.</p>
<h3>Heading three with <code>code</code> and <strong>strong</strong></h3>
<p>The paragraph directly after the h3.</p>
<h4>Heading four with <code>code</code> and <strong>strong</strong></h4>
<p>The paragraph directly after the h4.</p>
<blockquote>
  <p>First quoted paragraph with <code>code</code> and <strong>strong</strong>.</p>
  <p>Last quoted paragraph.</p>
</blockquote>
<pre><code>let x = 1</code></pre>
<ul>
  <li>
    <p>First paragraph of the item.</p>
    <p>Last paragraph of the item.</p>
  </li>
  <li>A bare item<ul><li>nested unordered</li></ul></li>
  <li>Another bare item<ol><li>nested ordered</li></ol></li>
</ul>
<ol>
  <li><p>Ordered item holding a paragraph.</p></li>
  <li>A bare ordered item</li>
</ol>
<ol type="A"><li>Upper alpha</li></ol>
<ol type="i"><li>Lower roman</li></ol>
<dl><dt>A term</dt><dd>Its definition.</dd></dl>
<hr>
<p>The paragraph directly after the rule.</p>
<figure><img alt=""><figcaption>A caption.</figcaption></figure>
<picture><img alt=""></picture>
<video></video>
<div class="not-prose"><p>Opted out of the plugin.</p></div>
<table>
  <thead>
    <tr>
      <th>Head with <code>code</code></th>
      <th>Head with <strong>strong</strong></th>
    </tr>
  </thead>
  <tbody>
    <tr><td>First row, first cell</td><td>First row, last cell</td></tr>
    <tr><td>Last row, first cell</td><td>Last row, last cell</td></tr>
  </tbody>
  <tfoot>
    <tr><td>Foot, first cell</td><td>Foot, last cell</td></tr>
  </tfoot>
</table>
<p>The last child of the container.</p>
|html}

(* Bare elements exercise the root declarations and the --tw-prose-* bindings
   and nothing else, which leaves the bulk of the plugin uncompared. Rendering
   the document above under both sheets and diffing every computed style of
   every node in it is what covers the descendant rules; the size variants
   rescale those rules rather than the root, so they need the same document. *)
let test_descendant_rendering () =
  Test_helpers.check_rendering_matches ~inner:prose_document
    ~test_name:"prose descendants render like Tailwind"
    [ prose; prose_sm; prose_lg; prose_xl; prose_2xl ]

(* The colour themes only write --tw-prose-* bindings; nothing on the container
   reads them, so a bare element compares the variables and stops there. What
   consumes them is the descendant rules - body copy, links, headings, code,
   quote bars, table borders - so the document is what turns a mis-bound
   variable into an observable colour. *)
let test_color_rendering () =
  Test_helpers.check_rendering_matches ~inner:prose_document
    ~test_name:"prose colours render like Tailwind"
    [ prose; prose_gray; prose_slate; prose_invert; prose_orange ]

(* The element variants are the other half of the plugin: they put a utility on
   a prose descendant, and a variant stacked under one has to reach the same
   element. Rendered against the document, prose-li:marker: lands on the bullet
   and prose-code:before: on the generated box, neither of which shows in the
   element's own computed style. *)
let element_variant cls =
  match Tw.of_string cls with
  | Ok u -> u
  | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m

let element_variants =
  List.map element_variant
    [
      "prose-h2:text-red-500";
      "prose-a:underline";
      "prose-li:marker:text-green-500";
      "prose-code:before:content-['x']";
      "prose-td:text-right";
    ]

let test_element_variant_rendering () =
  Test_helpers.check_rendering_matches ~inner:prose_document
    ~test_name:"prose element variants render like Tailwind" element_variants

(* Every variant the plugin registers, against the real CLI. Eight of them were
   not recognised at all, and the selector the rest built dropped whatever an
   inner variant had put on it. *)
let test_element_variant_parity () =
  Test_helpers.check_ordering_matches
    ~test_name:"prose element variants match Tailwind"
    (List.map element_variant
       [
         "prose-headings:text-lg";
         "prose-h1:text-lg";
         "prose-h2:text-lg";
         "prose-h3:text-lg";
         "prose-h4:text-lg";
         "prose-h5:text-lg";
         "prose-h6:text-lg";
         "prose-p:text-lg";
         "prose-a:underline";
         "prose-blockquote:italic";
         "prose-figure:mt-4";
         "prose-figcaption:text-xs";
         "prose-strong:font-black";
         "prose-em:not-italic";
         "prose-kbd:text-xs";
         "prose-code:text-xs";
         "prose-pre:text-xs";
         "prose-ol:list-decimal";
         "prose-ul:list-disc";
         "prose-li:ml-4";
         "prose-dl:mt-4";
         "prose-dt:font-bold";
         "prose-dd:ml-4";
         "prose-table:w-full";
         "prose-thead:text-left";
         "prose-tr:border-b";
         "prose-th:text-left";
         "prose-td:text-right";
         "prose-img:rounded-lg";
         "prose-picture:block";
         "prose-video:w-full";
         "prose-hr:border-dashed";
         "prose-lead:tracking-wide";
       ])

let suite =
  ( "prose",
    [
      Alcotest.test_case "classes" `Quick test_classes;
      Alcotest.test_case "default theme colours" `Quick
        test_default_theme_colours;
      Alcotest.test_case "color variants" `Quick test_color_variants;
      Alcotest.test_case "combinations" `Quick test_combinations;
      Alcotest.test_case "CSS generation" `Quick test_css_generation;
      Alcotest.test_case "inline styles" `Quick test_inline_styles;
      Alcotest.test_case "size parity with Tailwind" `Quick test_size_parity;
      Alcotest.test_case "colour theme parity with Tailwind" `Quick
        test_color_theme_parity;
      Alcotest.test_case "descendants render like Tailwind" `Quick
        test_descendant_rendering;
      Alcotest.test_case "colours render like Tailwind" `Quick
        test_color_rendering;
      Alcotest.test_case "element variant parity with Tailwind" `Quick
        test_element_variant_parity;
      Alcotest.test_case "element variants render like Tailwind" `Quick
        test_element_variant_rendering;
    ] )
