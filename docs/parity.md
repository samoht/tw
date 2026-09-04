Title: Measuring parity with Tailwind

tw aims to produce the same CSS as Tailwind v4.3.3. Three checks in CI measure
how close it gets, and a fuller comparison against tailwindcss.com runs by hand.

## Checks that run in CI

All three run under `dune runtest`.

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
the ceiling can be tightened. It reads 0 of 3961 and 0 of 50 today. Both other
checks run the differ in canonical mode, which normalises cascade-neutral rule
order on purpose, so this is the only one that sees a family emitted in the
wrong band. A missing or off-version CLI skips it with a line saying so;
`TW_TAILWIND_TESTS=1`, which CI sets, turns that into a failure.

## The site comparison

The comparison against tailwindcss.com finds most real bugs, because it
exercises class combinations no fixture covers. Its inputs are committed under
`test/parity/`, so anyone can re-derive the number:

<!-- $MDX skip -->
```sh
sh test/parity/measure.sh
```

That takes about 17 seconds on a warm build: a fifth of a second in Tailwind,
three seconds in tw, the rest in the differ. It writes `ref_local.css`,
`tw_all.css` and `diff.txt` under `tmp/parity` and prints the diff followed by
its top-level entries. The report is not wired into `dune runtest`; the order
gate above, which reads the same inputs, is.

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

Measured 2026-09-05 at 532fcc23 against cascade e2be4971, with the tailwindcss
4.3.3 that `package-lock.json` pins. The documented command completed without a
patched differ and reported:

```text
CSS: 659204 chars vs 664632 chars (0.8% diff)
Changes: 1 removed rule, 3 changed containers
├─ .DocSearch-Hit[aria-selected="true"] [title="Remove this search from favorites"]:before:where(.dark, .dark *)
├─ @media (prefers-color-scheme: dark) (9 blocks merged into 8)
├─ @layer utilities (2 reordered)
└─ @supports (color: color-mix(in lab,red,red)) (5 blocks merged into 4)
```

The count is only comparable against the cascade it was taken with, which is why
the sha is quoted beside it. Reading each entry:

- The rule entry is the empty `:where()` lightningcss writes for the site's
  `dark` variant, described under "What parity does not cover". tw keeps the
  declaration and the reference loses it, so it arrives as removed.
- The two block entries are DocSearch rules out of `search.css`. That is the
  site's own CSS rather than anything tw generated, and both sides only reprint
  it, so what they measure is cascade's printer against lightningcss.
- `@layer utilities` carries the whole of tw's own residual. Two rules sit out
  of position, `.line-y` and `.not-dark:hidden`. The containers under it differ
  in where their block boundaries fall rather than in what they hold, and the
  `@container` entries come in added/removed twins, which is the shape
  described under "Reading a failure".

Two divergences inside that layer are tw's and are worth naming, because a
canonical diff reports them only as block boundaries moving:

- **Media nesting order.** For a class stacking a breakpoint with the site's
  `dark` variant, Tailwind writes
  `@media (width >= 40rem) { @media (prefers-color-scheme: dark) { ... } }`
  and tw writes the two conditions the other way round. Nested media
  conditions are conjunctive, so the two render the same, but tw's spelling
  closes and reopens the breakpoint block around every preference block inside
  it. Tailwind's unminified output confirms the order, so this is not
  lightningcss.
- **A repeated `content`.** In the `color-mix` fallback of a `before:` or
  `after:` utility, tw repeats `content: var(--tw-content)` beside the colour.
  Tailwind nests the fallback inside the rule, so lightningcss flattens it
  carrying the colour alone. Seven rules on this corpus.

**Both sides are minified because every other configuration is noisier.** The
reference passes through lightningcss, so part of what the diff reports is
cascade disagreeing with lightningcss rather than tw disagreeing with Tailwind.
Dropping `--minify` raises that cost rather than removing it, because Tailwind's
unminified output is heavily nested and `--minify` is where lightningcss
flattens it. Measured on the site corpus at the revisions above:

| harness | top-level entries | utilities layer |
|---|---|---|
| minify both sides (current) | 4 | 2 reordered |
| neither side minified | 8 | 226 modified, 42 reordered |

The unminified run also reports `@layer theme`, `@property` and two `@keyframes`
entries that the minified one does not.

Flattening afterwards changes nothing, because the canonical comparator already
folds nesting.

## Reading a failure

A single class goes through both implementations with `--diff`:

<!-- $MDX skip -->
```sh
dune exec -- tw --single="hover:bg-blue-600" --diff
```

Use `--single=` rather than `-s` for a class that starts with `-` or contains
spaces. Both that output and the site diff have traps.

**`added` means present in the second file.** `cascade diff FILE1 FILE2` calls
FILE1 the expected side and FILE2 the actual one, and prints them as `---` and
`+++` in that order. `measure.sh` passes tw first and Tailwind second, so under
it `added` reads "Tailwind emits this and tw does not" and `removed` the
reverse. Reading the header the other way inverts every conclusion drawn from
the report.

**Use the built cascade, not the one on `PATH`.** An installed `cascade` from an
opam switch can be months old and will invent differences that do not exist.

**Pass `--depth=max`.** The default truncates the tree, and the summary line
counts containers rather than contents, so `3 changed containers` can hide a
hundred rule entries.

**`--diff` compares two minified sheets.** The CSS it attributes to Tailwind has
already been through lightningcss, so cross-check against
`tw -s "<class>" --tailwind`, which is unminified, before calling something a tw
bug. Author custom properties are kept even when neither generated sheet reads
them: CSS outside the generated sheet can still observe them.

**Order is compared, but only since cascade 105eea05.** `--diff=canonical`
matches rules by key rather than position, and before that commit it said
nothing about where the match sat. Every site number older than 2026-08-25 was
taken with that blindness; a move now arrives as a `reordered` entry. Block
structure surfaces as `N blocks merged into M`, but tw's `--minify` runs
cascade's printer without its optimizer, so most of those entries are adjacent
identical `@media` and `@container` blocks that lightningcss merged on the
reference side. The same mismatch turns a rule that
moved between two blocks with the same condition into a `removed` entry paired
with an `added` entry carrying those rules back, so look for the twin before
treating either half as a gap.

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

Four patterns account for most of what the site comparison has found, so a new
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

## What parity does not cover

tw rejects a class whose arbitrary value the property cannot take, where
Tailwind splices the value into CSS anyway. The docs pages carry literal
`<value>` and `<color>` placeholders, so the site's class list holds
`blur-[<value>]`, `shadow-[<value>]` and 64 more, which Tailwind emits as
`filter: blur(<value>)` and no browser accepts. `bg-[--brand-color]` and
`hover:bg-[--brand-hover-color]` emit `background-color: --brand-color` (v3
syntax), and `justify-baseline` emits a `justify-content` value CSS Box
Alignment 3 does not define. Together they are the 69 classes of the corpus that
Tailwind emits a rule for and tw does not. No class goes the other way: tw
invents nothing here.

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

One difference comes from lightningcss on the reference side: it serialises the
site's `dark` variant with an empty `:where()` where the variant expands to
`:where(.dark, .dark *)`, which matches nothing and drops a background colour
that Tailwind's own unminified output gets right. That is the rule entry in the
report above. Two others have gone, because tw now matches the reference: the
`@supports ((-webkit-backdrop-filter: ...) or (backdrop-filter: ...))` guard,
and the `-webkit-` prefix on `user-select`.

A last difference is between the two minifiers and shows in the sheets rather
than in the report. cascade folds `calc()` only where the fold is exact, so the
typography component keeps `line-height: calc(28/18)` where lightningcss rounds
it to `1.55556`; the canonical comparator reads the two as equal.

Anything else the site comparison reports is worth investigating, and so is the
disappearance of any of the above.
