Title: Measuring parity with Tailwind

tw aims to produce the CSS Tailwind v4.3.3 produces. The contract has two
halves:

- **Parity is what a browser renders.** `cascade diff --diff=canonical`
  between tw's sheet and Tailwind's compiled output reports nothing. The
  reference is the sheet the Tailwind CLI compiles, before lightningcss
  minifies it, so an entry in the report is tw against Tailwind and not
  cascade's printer against another minifier. Whether either side is minified
  does not matter to the comparison.
- **`tw --minify` is never larger than `tailwindcss --minify`.** The minified
  reference is built for that one figure.

An entry the report lists that is not a rendering difference is a cascade
bug, in the canonical projection or in the report itself, and is fixed in
cascade with a standalone reproducer. A rendering difference is a tw fix. A
difference no display shows, a sub-pixel length or a same-pixel colour, is
not chased as an exact tw fix; it belongs in cascade's default precision. tw
builds no rendering harness of its own, and Tailwind's minified sheet is not
diffed against its own unminified one.

Three checks in CI measure how close tw gets, and the fuller comparison
against tailwindcss.com runs by hand.

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
the ceiling can be tightened. It reads 0 of 4016 and 0 of 50 today. Both other
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
three seconds in tw, the rest in the differ. It writes the compiled reference
`ref.css`, the minified one `ref_local.css`, `tw_all.css` and `diff.txt` under
`tmp/parity`, prints the two minified sizes and fails when tw's is the larger,
then prints the diff followed by its top-level entries. The report is not
wired into `dune runtest`; the order gate above, which reads the same inputs,
is.

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

Measured 2026-09-15 at the tip of tw stack #832 with the
`variant-container-breakpoints` branch (#833) above it, against cascade
`canonical-lifted-subject` (the top of stack #1258, #1256 and #1257, above
`main` at ab443388), with the tailwindcss 4.3.3 that `package-lock.json`
pins. The documented command reported:

```text
minified: tw 661022 bytes, tailwindcss 664632 bytes
Changes: 1 changed container
└─ @layer utilities
   ├─ @media (prefers-color-scheme: dark) (105 blocks merged into 103)
   ├─ @media (width >= 40rem) (6 blocks merged into 5)
   ├─ @media (width >= 48rem) (3 blocks merged into 2)
   └─ @media (width < 64rem) (3 block split into 4)
```

The count is only comparable against the cascade it was taken with, which is
why the sha is quoted beside it. What is left is one shape, and it is
cascade's: both sheets write the same rules in the same order inside those
blocks, tw nesting `@media (prefers-color-scheme: dark)` outside the
breakpoint and Tailwind inside, and the projection sorts a block as one unit
keyed by whatever rules the input happened to group into it, so the two
inputs settle into different groupings. Cascade's TODO holds the five-class
reproducer cut from the site.

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
defect. Decide by reading what the moved rule sets and what it moved across.

**The reference is the compiled sheet, and it used to be the minified one.**
Every figure older than this one was taken against `tailwindcss --minify`,
where part of what the diff reported was cascade disagreeing with lightningcss
rather than tw disagreeing with Tailwind: the minifier folds colours,
fractions and media conditions to spellings of its own. Against the compiled
sheet the report is longer, because lightningcss no longer hides tw's own
folds behind matching ones, and every entry in it is one of the two things
the contract names.

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

**The summary line counts containers, not contents.** `3 changed containers`
can hide a hundred rule entries, so read the tree under it; `--limit=none`,
which `measure.sh` passes, keeps the differ from truncating it.

**`--diff` compares two minified sheets.** The CSS it attributes to Tailwind has
already been through lightningcss, so cross-check against
`tw -s "<class>" --tailwind`, which is the compiled output, before calling
something a tw bug; the site measurement compares against that output
directly. Author custom properties are kept even when neither generated sheet
reads them: CSS outside the generated sheet can still observe them.

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
