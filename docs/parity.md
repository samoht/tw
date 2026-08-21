Title: Measuring parity with Tailwind

tw aims to produce the same CSS as Tailwind v4.3.3. Two checks in CI measure how
close it gets, and a third comparison against tailwindcss.com runs by hand.

## Checks that run in CI

Both run under `dune runtest`.

**Upstream fixtures, `test/upstream/`.** `utilities.txt` and `variants.txt` are
Tailwind's own test corpus, extracted from the v4.3.3 tag: a class list and the
CSS Tailwind emits for it. `test/upstream/test.exe` replays 783 cases and fails
when tw rejects a class Tailwind accepts, or emits different CSS for one it
accepts. The two fixtures are generated, and `test/upstream/extract_tests.ml`
carries the command that regenerates them. Editing them by hand removes the
oracle the check depends on.

**Example pages, `examples/*/dune`.** Each of the nine examples builds its CSS
twice, once through tw and once through `npx tailwindcss`, then diffs the two
with `cascade diff --diff=canonical --prune-unused-custom-props`. The rule is
guarded by `(enabled_if %{bin-available:npx})`, so it is skipped where npx is
absent, and `%{bin:cascade}` resolves through the dune workspace, so the diff
runs the freshly built cascade rather than whatever sits on `PATH`.

## The site comparison

The comparison against tailwindcss.com finds most real bugs, because it
exercises class combinations no fixture covers, but it is neither committed nor
automated. It lives in `tmp/site` (gitignored), over three inputs rebuilt by
hand:

- `src/classlist.txt` is every class the deployed site uses, extracted from its
  CSS. A class name escapes every character outside `[A-Za-z0-9_-]`, so an
  unescaped `:` or `(` ends it and non-ASCII does not.
- `src/globals.css` is the site's entrypoint plus the files it imports.
- `src/ref-entry.css` is the same entrypoint with `source(none)` and an explicit
  `@source "./classlist.txt"`. Without it Tailwind auto-scans the whole
  directory, picks up tw's own output, and the comparison goes circular.

Both sheets are then generated and diffed:

<!-- $MDX skip -->
```sh
"$TW"/node_modules/.bin/tailwindcss -i src/ref-entry.css -o ref_local.css --minify
"$TW"/_build/default/bin/main.exe --input-css src/globals.css --minify \
  src/classlist.txt > tw_all.css
"$TW"/cascade/_build/default/bin/main.exe diff --diff=canonical --depth=max \
  tw_all.css ref_local.css
```

Committing it as a gate means committing about 150 KB of fixtures, so for now a
change in the reported gap is something to look at by hand.

**Both sides are minified because every other configuration is noisier.** The
reference passes through lightningcss, so part of what the diff reports is
cascade disagreeing with lightningcss rather than tw disagreeing with Tailwind.
Dropping `--minify` raises that cost rather than removing it, because Tailwind's
unminified output is heavily nested and `--minify` is where lightningcss
flattens it. Measured on the site corpus:

| harness | utilities layer | components layer |
|---|---|---|
| minify both sides | 47 added | 2 modified |
| neither side minified | 45 added, 11 removed, 513 modified | 55 removed, 1 modified |
| neither minified, both flattened | 45 added, 11 removed, 513 modified | 55 removed, 1 modified |

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
bug. `--diff` also passes `--prune-unused-custom-props`, which makes it blind to
a utility whose whole effect is a custom property nothing reads.

**Order is not compared.** `--diff=canonical` matches rules by key rather than
position, and `css_compare.mli` says it does not reason about cascade-affecting
reorderings. Block structure does surface as `N blocks merged into M`, but tw's
`--minify` runs cascade's printer without its optimizer, so most of those
entries are adjacent identical `@media` and `@container` blocks that
lightningcss merged on the reference side. The same mismatch turns a rule that
moved between two blocks with the same condition into a `removed` entry paired
with an `added` entry carrying those rules back, so look for the twin before
treating either half as a gap.

### Recurring bug shapes

Four patterns account for most of what the site comparison has found, so a new
family of utilities is worth checking against all four.

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

## What parity does not cover

tw rejects a class whose arbitrary value the property cannot take, where
Tailwind splices the value into CSS anyway. The docs pages carry literal
`<value>` placeholders, so the site's class list holds `blur-[<value>]`,
`shadow-[<value>]` and about forty more, which Tailwind emits as
`filter: blur(<value>)` and no browser accepts. `bg-[--brand-color]` emits
`background-color: --brand-color` (v3 syntax), `grid-cols-[1rem,1fr]` emits
`grid-template-columns: 1rem,1fr`, and `justify-baseline` emits a
`justify-content` value CSS Box Alignment 3 does not define. Together these are
45 of the 47 rules the site comparison reports as added.

Three more come from lightningcss on the reference side:

- It rewrites `@supports (backdrop-filter: var(--tw))` to accept the `-webkit-`
  spelling as well, which is the other two added rules.
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
