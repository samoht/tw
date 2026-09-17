Title: Measuring parity with Tailwind

tw aims to render every page the way Tailwind CSS v4.3.3 renders it. The
contract has two halves:

- **Parity is rendering parity.** Over the same document, tw's sheet and the
  sheet the Tailwind CLI compiles give every element and pseudo-element the
  same computed style, at every viewport and in every interaction state
  measured. Two values that paint the same count as equal: a colour is
  compared as the pixel it paints, and a length to within 0.05px. The
  reference is the compiled sheet, before lightningcss minifies it, so a
  difference is tw against Tailwind and not cascade's printer against another
  minifier. Whether either side is minified does not matter.
- **`tw --minify` is never larger than `tailwindcss --minify`.** This half is
  a size budget, and the minified reference is built for that one figure.

The verdict covers the document the browser rendered and the viewports and
states it sampled, in the version that ran, and `cascade diff --browser`
prints each of these with its report. Chromium drops a declaration it cannot
parse, so a rule Tailwind writes around one is at parity with tw writing
nothing; the site's placeholder classes below are the standing example.

Two instruments measure the contract. The browser gives the verdict, through
`cascade diff --browser --html PAGE`, which renders two sheets over a
document and reports every computed value they disagree on, with whether the
two paint the same. The canonical diff, `cascade diff --diff=canonical`, needs
no browser and no document, so it reads every rule of both sheets, where a
render sees only the rules its document exercises. It stands in for the
browser wherever there is no document, and every entry it lists is a
candidate for the browser to judge.

A disagreement is fixed where it lives:

- A difference that paints differently is a tw fix.
- An entry the canonical diff lists, where the browser paints both sheets the
  same over a document that exercises it, is a cascade over-report. It is
  fixed in cascade with a standalone reproducer.
- A rendering difference the canonical diff does not list is a cascade
  under-report, fixed the same way. It is the worse of the two, because every
  gate that reads the canonical diff passed over it.
- A difference in computed values that paints the same, a sub-pixel length or
  a same-pixel colour, is not chased as an exact tw fix; it belongs in
  cascade's default precision.

Tailwind's minified sheet is not diffed against its own unminified one.

Four checks in CI measure how close tw gets, and the fuller comparison
against tailwindcss.com runs by hand.

## Checks that run in CI

All four run under `dune runtest`.

**Rendering, `check_rendering_matches`.** Nine suites render their classes in
headless Chromium under tw's sheet and Tailwind's, and compare every computed
property. Each class gets an element of its own. A pair that writes a common
property, or whose order cascade cannot prove neutral, gets one more element
carrying both, because an ordering difference shows only there. The page is
rendered by cascade's `Browser_compare`, the runner behind `cascade diff
--browser`, so the check and the manual verdict below are one oracle: every
element and its pseudo-elements are sampled at every viewport width a media
condition in either sheet names, and under every interaction state either sheet
names, applied to every element at once. The sheets load as written, so no
cascade printer stands between them and the browser, and every computed value
that differs fails, a value that paints the same included. It skips without
node and a headless Chromium, and `TW_BROWSER_TESTS=1`, which CI sets, turns
the skip into a failure. The page and both sheets of each run stay under
`tmp/browser/`.

**Upstream fixtures, `test/upstream/`.** `utilities.txt` and `variants.txt` are
Tailwind's own test corpus, extracted from the v4.3.3 tag: a class list and the
CSS Tailwind emits for it. `test/upstream/test.exe` replays 800 cases and fails
when tw rejects a class Tailwind accepts, or emits different CSS for one it
accepts. The two fixtures are generated, and `test/upstream/extract_tests.ml`
carries the command that regenerates them. Editing them by hand removes the
oracle the check depends on, and the runner rejects a file whose block count no
longer matches the banner the extractor stamped on it. A case Tailwind has no
test for lives in `handwritten.txt`, read beside the two and written by no
regeneration.

**Example pages, `examples/*/dune`.** Each of the nine examples builds its CSS
twice, once through tw and once through `npx tailwindcss`, then diffs the two
with `cascade diff --diff=canonical`. The rule is
guarded by `(enabled_if %{bin-available:npx})`, so it is skipped where npx is
absent, and `%{bin:cascade}` resolves through the dune workspace, so the diff
runs the freshly built cascade rather than whatever sits on `PATH`.

**Whole-sheet order, `test/parity/dune`.** The site inputs below feed a third
check, which takes the top-level statement sequence out of `@layer utilities`
and `@layer components` on each side and reports the fewest statements that
have to move for tw's order to match Tailwind's. Only keys occurring exactly
once on both sides are paired, so the number owes nothing to a pairing choice.
The move count is pinned at 0 for both layers and the pair count at a floor of
3900 and 45, and the gate ratchets both ways: it fails when a move count rises
or a pair count falls, and prints the new figure when a move count drops, so
the ceiling can be tightened. It reads 0 of 4016 and 0 of 50 today. The
fixture and example checks run the differ in canonical mode, which normalises
cascade-neutral rule order on purpose, and the rendering check sees order
only through the pairs it builds, so this is the only check that sees a
family emitted in the wrong band. A missing or off-version CLI skips it with a
line saying so; `TW_TAILWIND_TESTS=1`, which CI sets, turns that into a
failure.

## The site comparison

The comparison against tailwindcss.com finds most real bugs, because it
exercises class combinations no fixture covers. Its inputs are a class list
rather than the site's pages, so it runs the canonical diff over the whole
list, and renders a page built from the list in the browser shard by shard.
The inputs are committed under `test/parity/`, so anyone can re-derive the
number:

<!-- $MDX skip -->
```sh
sh test/parity/measure.sh
TW_PARITY_RENDER="0 50" sh test/parity/measure.sh
```

Almost all of the first is the differ. Measured with hyperfine on 2026-09-16,
release builds, user time: 0.58 s for Tailwind to compile the reference, 0.58
s for tw to compile `tw_all.css`, and 24 s for the canonical diff of the two,
which cascade's TODO holds. It writes the compiled
reference `ref.css`, the minified one `ref_local.css`, `tw_all.css` and
`diff.txt` under `tmp/parity`, prints the two minified sizes and fails when
tw's is the larger, then prints the diff followed by its top-level entries.
The report is not wired into `dune runtest`; the order gate above, which reads
the same inputs, is.

The second also renders the 50 classes from index 0, in the order of
`classlist.txt`, and prints the browser's report after the canonical one.
`site_page.exe` puts each class on an element of its own, inside a wrapper
carrying every `group` name the list uses and after a sibling carrying every
`peer` name, with children for a class whose variants read descendants, so
`group-*`, `peer-*`, `has-*`, `in-*` and `*:` have the markup they read. A
variant testing an attribute or a class on an ancestor (`group-data-[checked]:`,
`in-[.dark]:`) matches on neither side, so the render covers it unmatched only,
and no element carries two classes: the order a pair would expose is the order
gate's. Both sheets are pruned to the page with `cascade prune` before the
render, since a rule no element matches cannot change what the page computes.
Unpruned, every width and state the whole sheet names is sampled on every
element, and a 50-class shard did not finish in ten minutes on 2026-09-16;
pruned, it took 218 seconds on a machine at load 100 to 200. The list is 97
shards of that size, so the render runs by hand and not in CI.

The inputs are:

- `classlist.txt` is every class the deployed site uses, extracted from its
  CSS. A class name escapes every character outside `[A-Za-z0-9_-]`, so an
  unescaped `:` or `(` ends it and non-ASCII does not.
- `globals.css` is the site's entrypoint plus the files it imports,
  `search.css` and `typography.css`. tw is run against this one.
- `ref-entry.css` is the same entrypoint with `source(none)` and an explicit
  `@source "./classlist.txt"`. Without it Tailwind auto-scans the whole
  directory, picks up tw's own output, and the comparison goes circular.
  Tailwind is run against this one.

The script prints what the three commands print and derives nothing. Reading a
class name back out of either sheet is not part of the measurement: doing it
needs a full CSS-escape decoder, and the one that used to sit on the script
could not decode a unicode escape, so it reported `after:content-['_↗']` as both
missing from tw and emitted only by tw.

`measure.sh` runs `dune build` first, so both binaries come from the workspace
rather than from `PATH`.

### Current measurement

Measured 2026-09-17 on tw at 20097b0c, the tip of #847, and cascade `main`
at 377d7201, with the tailwindcss 4.3.3 that `package-lock.json` pins. The
documented command reported:

```text
minified: tw 661153 bytes, tailwindcss 664632 bytes
Changes: none classified structurally (see report below)
top-level entries of tmp/parity/diff.txt:
  (none)
```

The count is only comparable against the cascade it was taken with, which is
why the sha is quoted beside it. The measurement before it, on 2026-09-15 at
tw 6c8a0f85 and cascade f88a46f3, reported one changed container: the same
rules in the same order inside four `@media` blocks, tw nesting
`@media (prefers-color-scheme: dark)` outside the breakpoint and Tailwind
inside, which the projection then grouped differently. Cascade's TODO holds
the five-class reproducer cut from the site, and cascade `main` no longer
reports it.

The whole class list was rendered on 2026-09-16, in the 97 shards of 50
`TW_PARITY_RENDER` names, from the tip of the stack this section arrived in
and cascade #1260. 93 shards compute the same on every element. The other
four render the same and are spelled differently: `bg-top-left`,
`bg-top-right` and `bg-bottom-left` compute `background-position` as `0%`
under Tailwind and `0px` under tw, which the report marks as painting the
same, and `mask-[radial-gradient(ellipse_25%_50%_at_30%_50%,...)]` computes
without the final colour stop's `100%`, which tw's minifier drops because CSS
Images 3 puts an unpositioned last stop there. The report marks that one as
painting differently, since its paint check counts the numbers in a value;
cascade's TODO holds it.

Two decisions taken on 2026-09-15 shaped the measurement:

- **The projection judges for the browsers `--minify` targets** (cascade
  #1256). Tailwind's compiled sheet keeps a `color-mix(in srgb, ...)`
  fallback before every opacity colour and a `@supports (color: color-mix(in
  lab, ...))` twin beside it, and lightningcss and tw resolve both for the
  evergreen browsers, so the compiled reference reported every one of them,
  150 entries. A guard every target satisfies, the fallback before a value
  every target parses, and a prefix a target needs are no difference now;
  `--enforce-spec` names no browser and reports them again. tw keeps writing
  what lightningcss writes: the WebKit mask prefixes Chrome 111 to 119 need,
  the hex a fallback folds to.
- **A reorder the report lists is a reproducer to cut, not a defect to read.**
  Cutting them found four tw sort defects: a `@max-*` container variant
  after the `@min-*` one at its width (#827), an opacity colour's `@supports`
  twin after every later utility of its hover group (#829), an arbitrary
  `@max-[theme(...)]` after `@lg` and a stacked `@sm:@max-md:` past `@md`
  (#830), a negative half-step translate after the negative integers (#831),
  and `md:container`'s breakpoints after `md:max-w-2xl`
  (#833); and one cascade over-report, `svg
  :where()` against `svg *:where()` (cascade #1257).

The entries earlier measurements showed, each landed in cascade with a
reproducer, are the ones a report against the compiled output finds first:
nested `@media` order (#1240), keyframe declaration order (#1241), a signed
number's leading zero (#1242), a pixel `stroke-width` (#1243), an infinite
length (#1244), a static percentage `calc()` in `flex-basis` (#1245), moves
the cascade cannot see (#1246), a custom property's time unit (#1247), a
pass-through relative colour (#1248), a negated range bound (#1250), a
declaration a guard repeats (#1254) and a fraction folded under the
six-figure budget (#1255): `w-2/3` is `66.6667%` in tw and
`calc(2 / 3 * 100%)` in the compiled reference, and the two render within a
layout unit of each other. On tw's side the repeated
`content: var(--tw-content)` in a `before:`/`after:` colour twin went with
it (#823), and #828 writes the fraction's calc.

**A reorder surviving canonical mode does not mean it can change rendering.**
Canonical mode suppresses a reorder it can prove cascade-neutral and flags the
rest: a same-property pair whose selectors might match a common element. It
does not check whether the two boxes differ or the two values coincide, which
is the right conservatism for a differ and the wrong thing to read as a
defect. Decide in the browser, over a document carrying the moved rule and the
rule it moved across.

**The reference is the compiled sheet, and it used to be the minified one.**
Every figure older than this one was taken against `tailwindcss --minify`,
where part of what the diff reported was cascade disagreeing with lightningcss
rather than tw disagreeing with Tailwind: the minifier folds colours,
fractions and media conditions to spellings of its own. Against the compiled
sheet the report is longer, because lightningcss no longer hides tw's own
folds behind matching ones, and every entry in it is a difference in tw or
an over-report in cascade.

## Reading a failure

A single class goes through both implementations with `--diff`:

<!-- $MDX skip -->
```sh
dune exec -- tw --single="hover:bg-blue-600" --diff
```

Use `--single=` rather than `-s` for a class that starts with `-` or contains
spaces.

An entry either report lists is settled in the browser. Put the classes
involved on a page and hand it to `--diff`, which compiles both sheets the way
it always does and renders the two over the page after the canonical report:

<!-- $MDX skip -->
```sh
mkdir -p tmp
echo '<div class="bg-blue-500 hover:bg-blue-600">x</div>' > tmp/page.html
dune exec -- tw -s "bg-blue-500 hover:bg-blue-600" --diff --html tmp/page.html
```

The same works with an entrypoint, `tw --input-css app.css page.html --diff
--html page.html`, where the page is both the source scanned and the document
rendered. The page has to carry every class compared: one it lacks is refused
rather than compared on no element. With the two sheets already on
disk, `cascade diff --browser --html tmp/page.html tmp/tw.css tmp/tailwind.css`
is the same comparison. It samples every viewport width and interaction state
either sheet names, and exits 0 when neither report finds a difference, 1 when
one does, and 2 when no browser ran, nothing was sampled or a sheet could not
be read. A difference marked as painting the
same is a precision question for cascade. An entry the canonical diff listed
that renders the same is an over-report to cut down and file in cascade. The
page decides what the answer covers: a `group-hover:` or `peer-` class needs
the markup that variant reads.

Both reports have traps.

**`added` means present in the second file.** `cascade diff FILE1 FILE2` calls
FILE1 the expected side and FILE2 the actual one, and prints them as `---` and
`+++` in that order. `measure.sh` passes tw first and Tailwind second, so under
it `added` reads "Tailwind emits this and tw does not" and `removed` the
reverse. Reading the header the other way inverts every conclusion drawn from
the report.

**Use the built cascade, not the one on `PATH`.** An installed `cascade` from an
opam switch can be months old and will invent differences that do not exist.

**The summary line counts containers, not contents.** `3 changed containers`
can hide a hundred rule entries, so read the tree under it; `--limit=none`,
which `measure.sh` passes, keeps the differ from truncating it.

**`--diff` compares against the compiled output unless told otherwise.**
`--minify` and `--optimize` reach both sides: tw minifies or optimises its own
sheet, and the Tailwind CLI runs lightningcss over its one. A report under
either flag is partly cascade's printer against lightningcss, so drop the
flags before calling something a tw bug. Author custom properties are kept
even when neither generated sheet reads them: CSS outside the generated sheet
can still observe them.

**Order is compared, but only since cascade 105eea05.** `--diff=canonical`
matches rules by key rather than position, and before that commit it said
nothing about where the match sat. Every site number older than 2026-08-25 was
taken with that blindness; a move now arrives as a `reordered` entry. Block
structure surfaces as `N blocks merged into M` or `N block split into M`:
adjacent `@media` and `@container` blocks with one condition that the two
sides group differently. A rule that moved between two blocks with the same
condition arrives as a `removed` entry paired with an `added` entry carrying
those rules back, so look for the twin before treating either half as a gap.

The corollary is the trap. A reorder among utilities that share no CSS property
is cascade-neutral, so the canonical projection collapses it correctly --
nothing about the rendered page changes. Tailwind still emits those utilities in
a fixed order, so tw can carry a real sort bug that no `--diff` in any mode will
report. That is why the sort tests read byte positions out of the sheet
(`check_class_order` in `test/test_sort.ml`) instead of asking the differ:
`check_ordering_matches` goes through the differ, and the differ has nothing to
say. A priority-band bug in `lib/typography.ml` and `lib/overflow_wrap.ml` was
invisible to every per-class comparison and was found this way.
`check_class_order` names the classes it checks, so it pins order inside one
family and says nothing about where the family sits; `test/parity/dune` covers
that half by reading the whole sheet.

### Recurring bug shapes

Five patterns account for most of what the site comparison has found, so a new
family of utilities is worth checking against all five.

- **Invented theme token.** A utility references `var(--<family>-<name>)` when
  no `@theme` declares it, leaving a reference that resolves to nothing. Write
  the keyword instead.
- **Silent coercion.** An arbitrary value that does not parse falls back to a
  plausible one, so `rounded-[calc(...)]` became `0` and `object-[50%]` became
  `var(--50)`. Reject the class.
- **A route that rebuilds the selector from the bare class**, discarding what an
  inner variant already did. Rebase on the incoming selector instead; see
  `route_regular` in `lib/rule.ml`.
- **A palette colour looked up only in `Scheme.hex_color`**, which holds
  per-render overrides and is empty by default, so the palette hex is never
  found and the fallback degrades.
- **A class name re-printed from the AST instead of echoed.** A class name is
  not CSS: it has to come back out spelled the way the author wrote it, because
  it is also the selector that must match the markup. Printing it through a CSS
  printer canonicalises the number and renames the class, so the rule matches
  nothing. `min-[0.5ch]:flex` emitted `.min-\[\.5ch\]\:flex` because
  `Pp.float` drops a leading zero unconditionally, and every gate stayed green:
  no fixture covered a unit outside the handful the compact path enumerated.
  Check a new family with a value the printer would respell -- a leading zero, a
  trailing zero, an exponent -- and check the selector, not just the
  declaration.

## Limits of the canonical diff

tw rejects a class whose arbitrary value the property cannot take, where
Tailwind splices the value into CSS anyway. The docs pages carry literal
`<value>` and `<color>` placeholders, so the site's class list holds
`blur-[<value>]`, `shadow-[<value>]` and 64 more, which Tailwind emits as
`filter: blur(<value>)` and no browser accepts. `bg-[--brand-color]` and
`hover:bg-[--brand-hover-color]` emit `background-color: --brand-color` (v3
syntax), and `justify-baseline` emits a `justify-content` value CSS Box
Alignment 3 does not define. Together they are the 69 classes of the corpus that
Tailwind emits a rule for and tw does not. No class goes the other way: tw
invents nothing here. Chromium drops each of those declarations, so both
pages render the same and the 69 classes are at parity.

The site comparison no longer counts them, and a reader looking for them in the
report above will not find them. cascade's declaration reader refuses
`order: <value>` and 76 more like it, so it drops those rules out of the parsed
reference before comparing and prints a parse warning for each instead.
`--diff=canonical` answers `Cannot determine whether the CSS files are
identical` and exits 2 when a refused declaration is all that separates two
sheets, but a run that also finds real differences exits 1 and its summary drops
them without saying so. On the site corpus the 111 parse warnings the script
prints are the only signal that the counts below them are short.

Neither side's candidate extractor is a source of difference on this corpus. tw
reads `classlist.txt` with `Tw_tools.Source_scan` and Tailwind reads it with its
own extractor through `@source`, and the two agree: a reference built by
declaring all 4824 candidates an `@source inline` string can hold differs from
the scanned one only by the two `content-['Hello\_World']` spellings that form
cannot carry, and `@apply` compiles all 69 classes above, so none of them is a
spelling Tailwind's extractor declined.

A bracket value neither property can take is placed differently by the two:
`border-[50%]` is `border-color: 50%` in Tailwind and `border-width: 50%` in tw,
and `decoration-[2]` is `text-decoration-color: 2` in Tailwind and nothing in
tw. `Css.color` has no numeric inhabitant, so tw cannot spell the Tailwind form
without an untyped escape hatch. Browsers drop both declarations either way, so
the rendered result matches.

lightningcss is not part of the comparison any more, and one difference it
used to contribute is worth remembering for anyone reading an old report: it
serialises the site's `dark` variant with an empty `:where()` where the
variant expands to `:where(.dark, .dark *)`, which matches nothing and drops a
background colour that Tailwind's own compiled output gets right. Against the
compiled output the entry is gone. What it still contributes is the byte
budget: the minified sizes the script prints are cascade's printer against
lightningcss over the same sheet, and cascade folds `calc()` only where the
fold is exact, so the typography component keeps `line-height: calc(28/18)`
where lightningcss rounds it to `1.55556`.

Anything else the site comparison reports is worth investigating, and so is the
disappearance of any of the above.
