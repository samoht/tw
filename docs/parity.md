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
check, which takes the top-level statement sequence out of `@layer utilities` on
each side and reports the fewest statements that have to move for tw's order to
match Tailwind's. Only keys occurring exactly once on both sides are paired, so
the number owes nothing to a pairing choice. It is pinned at 581 over 3885 pairs
and ratchets: the gate fails when it rises and prints the new figure when it
falls, so the ceiling can be tightened. Both other checks run the differ in
canonical mode, which normalises cascade-neutral rule order on purpose, so this
is the only one that sees a family emitted in the wrong band. A missing or
off-version CLI skips it with a line saying so; `TW_TAILWIND_TESTS=1`, which CI
sets, turns that into a failure.

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

Measured 2026-09-02 at 36b47e32 against cascade a6557326. The documented command
completed without a patched differ and reported:

```text
CSS: 662557 chars vs 664632 chars (0.3% diff)
Changes: 1 removed rule, 8 modified rules, 20 reordered rules, 5 changed containers
├─ .DocSearch-Container
├─ .with-line-numbers .line:before
├─ .DocSearch-Hit[aria-selected="true"] [title="Remove this search from favorites"]:before:where(.dark, .dark *)
├─ .dark .DocSearch-Container
├─ .dark .DocSearch-Hit--Result.DocSearch-Hit--Child:before
├─ .dark .DocSearch-SearchBar
├─ .dark :is(.DocSearch-Hit:first-child > a)
├─ .dark :is(.DocSearch-Hit-action [title="Remove this search from favorites"]):before
├─ .dark .DocSearch-Hit--Result
├─ .DocSearch-Hits mark (position 132) ↔  .DocSearch-NoResults .DocSearch-Title (position 76)
├─ .DocSearch-Hit-path mark (position 133) ↔  .DocSearch-StartScreen .DocSearch-Help (position 77)
├─ .DocSearch-Hits ma...re(.dark, .dark *) (position 134) ↔  .dark .DocSearch-Hit-path (position 78)
├─ .DocSearch-NoResults-Prefill-List ul (position 135) ↔  .DocSearch-NoResul...st .DocSearch-Help (position 79)
├─ .DocSearch-NoResul...re(.dark, .dark *) (position 136) ↔  .DocSearch-NoResul...re(.dark, .dark *) (position 80)
├─ .dark .DocSearch-Modal (position 137) ↔  .DocSearch-NoResul... + .DocSearch-Help (position 81)
├─ .dark .DocSearch-MagnifierLabel (position 141) ↔  .DocSearch-Container (position 92)
├─ .dark .DocSearch-Cancel (position 144) ↔  @media (width >= 40rem) (position 93)
├─ .DocSearch-Hit--Re...DocSearch-Hit-icon (position 145) ↔  @media (width >= 64rem) (position 95)
├─ .DocSearch-Hit--Pa...DocSearch-Hit-icon (position 146) ↔  .DocSearch-Visuall...enForAccessibility (position 96)
├─ .DocSearch-Hit--Re...cSearch-Hit-action (position 147) ↔  .DocSearch-Hit (position 97)
├─ .DocSearch-Hit-act...h from favorites"] (position 148) ↔  .DocSearch-LoadingIndicator (position 98)
├─ .DocSearch-Hit-act...rch from history"] (position 149) ↔  .DocSearch-Modal (position 99)
├─ .DocSearch-Hit-act...Save this search"] (position 150) ↔  .DocSearch-Hit--Result (position 100)
├─ .DocSearch-Hit--Ta...DocSearch-Hit-icon (position 151) ↔  .DocSearch-SearchBar (position 101)
├─ .DocSearch-Hit--Ta...cSearch-Hit-action (position 152) ↔  .DocSearch-Dropdown-Container (position 102)
├─ .DocSearch-Hit-act...cSearch-Hit-action (position 153) ↔  .DocSearch-Dropdown (position 103)
├─ .DocSearch-NoResul...Search-Screen-Icon (position 154) ↔  .DocSearch-Form (position 104)
├─ .with-line-numbers .line (position 156) ↔  .DocSearch-Hit-Container (position 109)
├─ .DocSearch-Hit[ari...favorites"]:before (position 160) ↔  .DocSearch-Hit-source (position 114)
├─ @media (width <= 40rem) (position 116 → 161)
├─ @media (prefers-color-scheme: dark) (10 block split into 11)
├─ @layer utilities (45 added, 16 modified, 164 reordered, 4 rearranged, 1 selector changed)
├─ @layer components (6 modified)
└─ @supports (color: color-mix(in lab,red,red)) (1 block split into 4)
```

The top-level entries are quoted with the summary because its five changed
containers include the utilities layer, which contains hundreds of nested
changes. The summary alone is not a useful estimate of the remaining work.

**Both sides are minified because every other configuration is noisier.** The
reference passes through lightningcss, so part of what the diff reports is
cascade disagreeing with lightningcss rather than tw disagreeing with Tailwind.
Dropping `--minify` raises that cost rather than removing it, because Tailwind's
unminified output is heavily nested and `--minify` is where lightningcss
flattens it. Measured on the site corpus:

| harness | utilities layer | components layer |
|---|---|---|
| minify both sides (current) | 45 added, 16 modified, 164 reordered, 4 rearranged, 1 selector changed | 6 modified |
| neither side minified | 45 added, 11 removed, 513 modified | 55 removed, 1 modified |
| neither minified, both flattened | 45 added, 11 removed, 513 modified | 55 removed, 1 modified |

The two unminified rows predate cascade reading rule positions back, so neither
of them carries a reordered count and they are not directly comparable to the
current row.

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
`<value>` placeholders, so the site's class list holds `blur-[<value>]`,
`shadow-[<value>]` and about forty more, which Tailwind emits as
`filter: blur(<value>)` and no browser accepts. `bg-[--brand-color]` emits
`background-color: --brand-color` (v3 syntax), `grid-cols-[1rem,1fr]` emits
`grid-template-columns: 1rem,1fr`, and `justify-baseline` emits a
`justify-content` value CSS Box Alignment 3 does not define. Together these are
the 45 rules the site comparison reports as added directly under
`@layer utilities`.

A bracket value neither property can take is placed differently by the two:
`border-[50%]` is `border-color: 50%` in Tailwind and `border-width: 50%` in tw,
and `decoration-[2]` is `text-decoration-color: 2` in Tailwind and nothing in
tw. `Css.color` has no numeric inhabitant, so tw cannot spell the Tailwind form
without an untyped escape hatch. Browsers drop both declarations either way, so
the rendered result matches.

Three more differences come from lightningcss on the reference side:

- It rewrites `@supports (backdrop-filter: var(--tw))` to accept the `-webkit-`
  spelling as well, so that guard and the rules under it arrive as added
  containers of their own.
- It autoprefixes the site's own CSS, so `user-select: none` on
  `.with-line-numbers .line::before` arrives as
  `-webkit-user-select: none; user-select: none`. cascade adds no prefix.
- It serialises the DocSearch dark-variant rules with an empty `:where()`, which
  matches nothing and drops a background colour that Tailwind's own unminified
  output gets right.

The last difference is between the two minifiers. cascade folds `calc()` only
where the fold is exact, so the typography component keeps
`line-height: calc(28 / 18)` where lightningcss rounds it to `1.55556`.

Anything else the site comparison reports is worth investigating, and so is the
disappearance of any of the above.
