Title: Measuring parity with Tailwind

tw aims to render every page the way Tailwind CSS v4.3.3 renders it.

## The contract

- **Rendering parity.** Over the same document, tw's sheet and the sheet the
  Tailwind CLI compiles paint the same pixels at every viewport and in every
  interaction state measured. The raster is the verdict and nothing normalises
  it: what a script reads back through `getComputedStyle` is not part of the
  contract, so two spellings the browser paints alike are one render, and a
  spelling that moves a pixel is a difference.
- **Size budget.** `tw --minify` is never larger than `tailwindcss --minify`.

The reference is the compiled sheet, before lightningcss minifies it, so a
difference is tw against Tailwind and never cascade's printer against another
minifier. Whether either side is minified does not matter to the verdict.
Chromium drops a declaration it cannot parse, so a rule Tailwind writes around
one is at parity with tw writing nothing.

A disagreement is fixed where it lives:

| The browser says | The canonical diff says | It is |
|---|---|---|
| renders differently | anything | a tw bug |
| renders the same | lists an entry | a cascade over-report, fixed there with a reproducer |
| renders differently | lists nothing | a cascade under-report, the worse of the two: every gate that reads the diff passed over it |

A property that paints nothing on the page rendered, a cursor or a
transition's timing, is outside what the render measures; the canonical diff
is what reads it.

## Instruments

| Instrument | Command | What it answers |
|---|---|---|
| Browser render | `cascade diff --browser --html PAGE A.css B.css` | The verdict: the page is loaded under each sheet at every viewport width and interaction state either sheet names, captured, and compared pixel for pixel. Where a capture differs, the computed values of the elements under the differing pixels are listed, so the report names a property. |
| Canonical diff | `cascade diff --diff=canonical A.css B.css` | Every rule of both sheets, with no document: a render sees only the rules its page exercises. Every entry is a candidate for the browser to judge. It judges for the evergreen browsers `--minify` targets, so a `@supports` guard every target satisfies, the fallback before a value every target parses, and a prefix a target needs are no difference; `--enforce-spec` reports them again. |
| Order gate | `test/parity/dune` | The order of top-level statements in `@layer utilities` and `@layer components`, which the canonical diff collapses when it is cascade-neutral and Tailwind still fixes. |
| Upstream fixtures | `test/upstream/` | Tailwind's own test corpus, replayed. |
| Examples | `examples/*/dune` | Nine real entrypoints compiled by both tools and diffed canonically. |

The browser needs node and a headless Chromium (`NODE`, `CHROME`, or the usual
places); the fixtures and examples need the pinned `tailwindcss` CLI through
`npx`. Both come from `package-lock.json`.

## Checks in CI

All run under `dune runtest`. A missing tool skips a check with a line saying
so; `TW_BROWSER_TESTS=1` and `TW_TAILWIND_TESTS=1`, which CI sets, turn the
skip into a failure.

- **The corpus, `test/parity/corpus/`.** Every `NAME.css` there is a Tailwind
  entrypoint, `@import "tailwindcss"` included, compiled by both tools,
  compared canonically and rendered over `NAME.html` beside it or, without
  one, over `Test_helpers.classes_page` built from the classes its `@source
  inline("...")` names. Dropping a file in is the whole act of adding a case;
  the rule reads the directory. Each case runs the documented command,
  `tw --input-css NAME.css PAGE --diff --html PAGE`, and passes only when the
  two oracles agree the sheets are the same: a render that differs is a tw
  bug, and a disagreement between the render and the canonical diff is a
  cascade bug the output names. The cases are the idioms the class-list
  corpus never exercises: a project theme read by every family, namespaces
  reset to `initial`, declared variants between built-in ones, `@apply` in a
  component and a declared utility, variants stacked three deep, and a
  prefixed build.

- **Rendering, `check_rendering_matches`.** Nine suites render their classes
  under tw's sheet and Tailwind's through `Browser_compare`, the runner behind
  `cascade diff --browser`, and `Browser_compare.identical` is the verdict.
  Each class gets an element of its own holding `Test_helpers.box_marker`, a
  run of text and a bar as wide as the content box in the element's colour, so
  a property painting the box reaches the raster; an empty element paints
  nothing. A pair that writes a common property, or whose order cascade cannot
  prove neutral, gets one more element carrying both, which is where an
  ordering difference shows. The page and both sheets of a run stay under
  `tmp/browser/`.
- **Upstream fixtures, `test/upstream/`.** `utilities.txt` and `variants.txt`
  are generated from the v4.3.3 tag by `extract_tests.ml`; the runner rejects a
  file whose block count no longer matches the banner the extractor stamped.
  A case Tailwind has no test for belongs in the `test_<module>.ml` of the
  utility it covers. A class Tailwind accepts and tw rejects, or compiles
  differently, fails.
- **Examples, `examples/*/dune`.** Guarded by `%{bin-available:npx}`;
  `%{bin:cascade}` resolves through the workspace, never `PATH`.
- **Order gate, `test/parity/dune`.** Pairs the statement keys occurring
  exactly once on both sides and reports the fewest that must move for tw's
  order to match Tailwind's. Pinned at 0 moves for both layers and a floor on
  the pair count, and it ratchets both ways: a rising move count or a falling
  pair count fails, a dropping move count prints the figure to tighten.

## The site comparison

The class list of tailwindcss.com exercises combinations no fixture covers.
The inputs are committed under `test/parity/`:

- `classlist.txt`, every class the deployed site uses, extracted from its CSS.
- `globals.css`, the site's entrypoint with the files it imports; tw compiles
  this.
- `ref-entry.css`, the same entrypoint with `source(none)` and an explicit
  `@source "./classlist.txt"`, so Tailwind does not scan the directory and
  pick up tw's own output; Tailwind compiles this.

<!-- $MDX skip -->
```sh
sh test/parity/measure.sh                           # canonical diff and size budget
TW_PARITY_RENDER="0 50" sh test/parity/measure.sh   # plus one 50-class shard rendered
```

The script builds both binaries from the workspace, writes `ref.css`,
`ref_local.css`, `tw_all.css` and `diff.txt` under `tmp/parity`, fails when
tw's minified sheet is the larger, and prints the diff with its top-level
entries. With `TW_PARITY_RENDER`, `site_page.exe` puts each class of the shard
on an element of its own holding the box marker, inside a wrapper carrying
every `group` name the list uses and after a sibling carrying every `peer`
name, with children for a class whose variants read descendants. Both sheets
are pruned to the page with `cascade prune` first, since a rule no element
matches cannot change what the page paints. A variant that tests an attribute
or a class on an ancestor (`group-data-[checked]:`, `in-[.dark]:`) matches on
neither side, so the render covers it unmatched. A shard takes some 25
seconds and the list is 97 shards, so the whole render runs by hand.

### Current figures

tw `main` at #869, cascade `main` at #1268, tailwindcss 4.3.3:

- Canonical diff over the whole list: `CSS files are identical`, no top-level
  entry.
- Size: tw 661,153 bytes, tailwindcss 664,632 bytes.
- Render: 97 of 97 shards the same picture, 30,264 captures (twelve viewports
  by thirteen states by two sheets per shard).

A figure is comparable only against the cascade it was taken with, which is
why both shas are quoted.

## Reading a failure

<!-- $MDX skip -->
```sh
dune exec -- tw --single="hover:bg-blue-600" --diff           # one class, canonical
mkdir -p tmp && echo '<div class="bg-blue-500 hover:bg-blue-600">x</div>' > tmp/page.html
dune exec -- tw -s "bg-blue-500 hover:bg-blue-600" --diff --html tmp/page.html   # and rendered
dune exec -- tw --input-css app.css page.html --diff --html page.html            # an entrypoint
```

Use `--single=` for a class that starts with `-` or contains spaces. With
`--html`, the canonical report comes first and the render after it; the page
has to carry every class compared, and a class it lacks is refused rather than
compared on no element. With both sheets on disk,
`cascade diff --browser --html PAGE tw.css tailwind.css` is the same render:
exit 0 when the two paint alike, 1 when they do not, 2 when no browser ran,
nothing was rendered or a sheet could not be read.

An entry the canonical diff lists is a reproducer to cut, not a defect to read.
Cut it down to the classes involved, render them over a page that carries them
and the markup their variants read, and file it where the table above says.

Traps:

- **`added` means present in the second file.** `cascade diff A B` prints A as
  `---` and B as `+++`; `measure.sh` passes tw first, so `added` reads
  "Tailwind emits this and tw does not".
- **The summary line counts containers, not contents.** `3 changed
  containers` can hide a hundred rules; `--limit=none`, which `measure.sh`
  passes, keeps the tree whole.
- **`--minify` and `--optimize` reach both sides** of `--diff`: tw prints its
  sheet through cascade and the CLI runs lightningcss. A report under either
  is partly one minifier against the other; drop the flags before calling
  something a tw bug.
- **A reorder the canonical diff collapses can still be a tw bug.** A reorder
  among utilities sharing no property is cascade-neutral and reported by no
  mode, while Tailwind still fixes it. `check_class_order` in
  `test/test_sort.ml` reads byte positions out of the sheet for order inside a
  family, and the order gate reads the whole sheet for where a family sits.
- **A reorder the canonical diff lists is not thereby a rendering change.** It
  flags a same-property pair whose selectors might match a common element
  without asking whether the boxes differ or the values coincide, which is the
  right conservatism for a differ. The browser decides.
- **Use the built cascade.** `%{bin:cascade}` and `measure.sh` resolve it
  through the workspace; an installed one from an opam switch invents
  differences that do not exist.

## Recurring bug shapes

Five patterns account for most of what the site comparison has found; a new
family is worth checking against all five.

- **Invented theme token.** A utility references `var(--<family>-<name>)` that
  no `@theme` declares. Write the keyword instead.
- **Silent coercion.** An arbitrary value that does not parse falls back to a
  plausible one: `rounded-[calc(...)]` became `0`, `object-[50%]` became
  `var(--50)`. Reject the class.
- **A route that rebuilds the selector from the bare class**, discarding what
  an inner variant already did. Rebase on the incoming selector; see
  `route_regular` in `lib/rule.ml`.
- **A palette colour looked up only in `Scheme.hex_color`**, which holds
  per-render overrides and is empty by default, so the hex is never found and
  the fallback degrades.
- **A class name re-printed from the AST instead of echoed.** A class name is
  also the selector that must match the markup, so it comes back out spelled
  as the author wrote it. `min-[0.5ch]:flex` once emitted
  `.min-\[\.5ch\]\:flex` because `Pp.float` drops a leading zero; check a new
  family with a value the printer would respell, and check the selector, not
  just the declaration.

## Known limits

- **Placeholder classes.** The docs pages carry literal `<value>` and `<color>`
  placeholders, so the site list holds `blur-[<value>]`, `shadow-[<value>]`
  and some sixty more that Tailwind emits as `filter: blur(<value>)` and no
  browser accepts; `bg-[--brand-color]` (v3 syntax) and `justify-baseline` are
  of the same kind. tw emits nothing for them, Chromium drops each of
  Tailwind's declarations, and both pages render the same, which is parity.
  cascade's reader refuses the declarations as the browser does and drops
  them from the parsed reference with a parse warning each, so the canonical
  diff answers on what a browser would keep and exits 0 when that is the
  same; the warnings the script prints say where its reader had to decide.
- **A bracket value neither property can take** is placed differently:
  `border-[50%]` is `border-color: 50%` in Tailwind and `border-width: 50%` in
  tw, and `decoration-[2]` is `text-decoration-color: 2` in Tailwind and
  nothing in tw. `Css.color` has no numeric inhabitant, so tw cannot spell the
  Tailwind form without an untyped escape. Browsers drop both.
- **The size budget is one minifier against another.** cascade folds `calc()`
  only where the fold is exact, so the typography component keeps
  `line-height: calc(28/18)` where lightningcss rounds it to `1.55556`.

Anything else the site comparison reports is worth investigating, and so is
the disappearance of any of the above.
