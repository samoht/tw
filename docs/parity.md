Title: Measuring parity with Tailwind

`tw` claims to produce the same CSS as Tailwind v4. This doc says how that claim
is checked, which checks run automatically, how to read their output, and why the
number does not reach zero.

## What is gated

Three checks run in CI, all through `dune runtest`.

### Upstream fixtures - `test/upstream/`

`utilities.txt` and `variants.txt` are Tailwind's own test corpus: a class list
and the CSS Tailwind emits for it. `test/upstream/test.exe` replays every case
(782 of them) and fails when tw parses a class Tailwind accepts, or emits
different CSS for it.

<!-- $MDX skip -->
```sh
dune exec test/upstream/test.exe
```

These files are generated. Never hand-edit them; regenerate from upstream.

### Example pages - `examples/*/dune`

Each of the nine examples builds its CSS twice, once through tw and once through
`npx tailwindcss`, then diffs the two:

<!-- $MDX skip -->
```sh
cascade diff --diff=canonical --prune-unused-custom-props landing.css tailwind.css
```

The rule is `(enabled_if %{bin-available:npx})`, so it is skipped where npx is
absent. `%{bin:cascade}` resolves through the dune workspace, so this is the
freshly built cascade, not whatever is on `PATH`.

### One class at a time

<!-- $MDX skip -->
```sh
dune exec -- tw --single="hover:bg-blue-600" --diff
```

Generates the class through both implementations and diffs them. Use
`--single=` rather than `-s` for a class starting with `-` or containing spaces.

## What is not gated

The whole-site comparison against tailwindcss.com is **not** automated and not
committed. It lives in `tmp/` (gitignored) and has to be reconstructed:

- `src/classlist.txt` - every class the deployed site uses, extracted from its
  CSS. A class name escapes every character outside `[A-Za-z0-9_-]`, so an
  unescaped `:` or `(` ends it, and non-ASCII does not.
- `src/globals.css` - the site's entrypoint, plus the files it imports.
- `src/ref-entry.css` - the same entrypoint with `source(none)` and an explicit
  `@source "./classlist.txt"`. Without this, Tailwind auto-scans the whole repo,
  picks up tw's own output, and the comparison goes circular.

Both sheets are then generated and diffed:

<!-- $MDX skip -->
```sh
"$TW"/node_modules/.bin/tailwindcss -i src/ref-entry.css -o ref_local.css --minify
"$TW"/_build/default/bin/main.exe --input-css src/globals.css --minify \
  src/classlist.txt > tw_all.css
"$TW"/cascade/_build/default/bin/main.exe diff --diff=canonical --depth=max \
  tw_all.css ref_local.css
```

This is the measurement that finds most real bugs, because it exercises class
combinations no fixture covers. Making it a committed gate means committing
roughly 150 KB of fixtures; until then, treat a change to the residual below as
something to investigate by hand.

## Reading the output

**Use the built cascade, not the one on `PATH`.** An installed `cascade` from an
opam switch can be months old and will invent differences that do not exist. A
stale binary once reported two byte-identical rules as added and appeared to skip
most of a layer.

**Read the whole diff.** The summary line counts *containers*, not their
contents, so `4 changed containers` can hide a hundred rule entries. Always:

<!-- $MDX skip -->
```sh
grep -nE "^├─|^└─" diff.txt
```

**`--diff` has two blind spots.** It forces `--minify --optimize`, so the value
it attributes to Tailwind is post-minification - cross-check with
`tw -s "<class>" --tailwind`, which is unminified, before calling something a tw
bug. It also passes `--prune-unused-custom-props`, which makes it blind to a
utility whose whole effect is a custom property nothing reads;
`[--checkered-bg:--alpha(...)]` reported no differences in isolation while the
site comparison caught it.

**Order is not compared.** `--diff=canonical` matches rules by key, not
position, and `css_compare.mli` says it "does not reason about cascade-affecting
rule reorderings". Sorting both utilities layers identically leaves the counts
unchanged. What *does* surface is block structure - `N blocks merged into M` -
because a rule sorting between two `@media` blocks stops them merging.

**Do not infer a cause from the differ's categories.** Two conclusions drawn that
way turned out to be wrong: that ordering caused the diff, and that a third of
the entries were byte-identical artefacts. Extract every rule from both
canonicalised sheets and diff the multisets instead.

## Why the number is not zero

### The current residual

Every entry the whole-site comparison still reports is tolerated: a class tw
rejects on purpose, a disagreement between the two minifiers, or a spelling that
selects the same elements. None of them changes what a browser renders. Treat a
new entry, or the disappearance of one of these, as something to investigate.

In the utilities layer:

- The docs pages' literal `[<value>]` placeholder classes, which Tailwind
  splices verbatim into CSS no browser accepts and tw rejects. See below.
- `after:content-['_↗']`. Tailwind leaves the arrow unescaped in the class
  selector. U+2197 is not one of the non-ASCII ident code points CSS Syntax 3
  admits, so an ident cannot carry it literally; tw writes `\2197 `. The two
  selectors match the same element.
- `supports-[backdrop-filter]:*`. Both sides keep the `@supports` guard;
  lightningcss rewrites the condition to also accept `-webkit-backdrop-filter`.
- `hue-rotate-0` and `backdrop-hue-rotate-0`. Both sides hold the value in
  `--tw-hue-rotate`. cascade folds `hue-rotate(0deg)` to `hue-rotate()` inside
  the custom property; lightningcss leaves custom-property values alone.
- `@container` block splits, from container variants written as function calls.
  Given `@min-[theme(--breakpoint-lg)]`, tw resolves the token and sorts the
  block by the width it denotes; Tailwind sorts by the bracket text. The blocks
  land in different places, so different neighbours merge with them.

In the components layer:

- `calc(28 / 18)`, which cascade will not fold.
- `-webkit-text-decoration-color`, which lightningcss emits twice.

The four minifier entries are in the table under *Two minifiers*, the
placeholders under *Placeholder classes*.

### Placeholder classes

The docs pages contain literal `<value>` placeholders, so the site's class list
includes `blur-[<value>]`, `shadow-[<value>]` and about forty more. Tailwind
emits them as CSS - `filter: blur(<value>)` - which no browser accepts. tw
rejects the class. This is deliberate: an arbitrary value the property cannot
take is not a utility, and coercing it to a plausible fallback (`center`, `auto`,
`0px`, the zero shadow) produces a silently wrong declaration instead of an
error. These account for most of the residual.

The same applies to `bg-[--brand-color]` (v3 syntax, emits
`background-color: --brand-color`), `grid-cols-[1rem,1fr]`, and
`justify-baseline` - CSS Box Alignment 3 gives `justify-content` no
`<baseline-position>`, so `justify-content: baseline` is dropped by browsers.

### Two minifiers

What the harness compares is `cascade(tw)` against `lightningcss(tailwind)`, so
some differences are between the two minifiers rather than between the two
implementations:

| difference | cascade | lightningcss |
|---|---|---|
| `--tw-hue-rotate: hue-rotate(0deg)` | folds to `hue-rotate()` | leaves it |
| `@supports (backdrop-filter: ...)` | as written | also accepts `-webkit-` |
| `text-decoration-color` | no prefix | adds `-webkit-` |
| `-webkit-text-decoration-color` | one declaration | two |
| `calc(28 / 18)` | keeps it | folds to `1.55556` |

The `hue-rotate` row is about custom properties, not about the filter fold.
lightningcss performs that fold too, unconditionally and at every target it
supports, so on a bare `filter` declaration the two agree. Both generators put
the value in `--tw-hue-rotate` instead, tw in `hue_rotate` in `lib/filters.ml`,
and there they diverge: lightningcss treats a custom-property value as opaque
and will not touch inside it, while cascade folds substreams whose type it can
prove (`hue_rotate_zero_argument` in cascade's `lib/properties.ml`).

The `@supports` row is not a threshold difference. Neither minifier elides the
guard, and neither should: `backdrop-filter` has been Baseline newly available
only since September 2024, short of cascade's greenfield bar, and the guard
survives Tailwind's floor of Safari 16.4 too, because Safari needs
`-webkit-backdrop-filter` up to 17.6. What lightningcss does is rewrite the
condition to `((-webkit-backdrop-filter: ...) or (backdrop-filter: ...))`, which
widens a test the author wrote against the unprefixed property. cascade does no
target-driven prefixing, so it leaves the condition as written.

The `calc` refusal is a precision commitment - the quotient does not survive
cascade's serialisation rounding.

The two decoration rows are one difference seen from either side. Tailwind's
unminified output carries no prefix; lightningcss adds one while minifying and
emits it twice, so `decoration-sky-500` arrives as
`-webkit-text-decoration-color; -webkit-text-decoration-color;
text-decoration-color`. tw writes the prefixed declaration itself, once, and
cascade neither adds nor removes it.

The two sheets also spell a negated media query differently, and that one is
generator-side. tw writes `not all and (min-width: 48rem)`, Tailwind writes
`not (width >= 48rem)`. The legacy shape comes from neither minifier: under
Tailwind's own lightningcss options the query is printed back verbatim, and
lightningcss has no path that produces `not all and (X)`. It is a deliberate
choice in `negate_media` in `lib/rule.ml`, taken to match Tailwind. cascade's
canonical comparator equates the two forms, so it does not reach the residual.

With no targets set lightningcss goes the other way, shortening
`not (width >= X)` to `(width < X)`. That collapse is not worth copying. Media
Queries 4 makes a feature that does not apply to the current media type evaluate
to false, so on such a medium `not (width >= 100px)` is true while
`(width < 100px)` is false. They are not the same query.

There is also one real bug in the Tailwind minification path. The site's search
CSS contains:

```css
.DocSearch-Hit[aria-selected="true"] [title="Remove this search from favorites"]::before {
  @apply bg-sky-100 dark:bg-gray-700/40;
}
```

Tailwind's unminified output correctly expands the dark variant with
`:where(.dark, .dark *)`. After `--minify`, lightningcss serialises both the
direct dark rule and its system-dark counterpart with an empty `:where()`:

```css
.DocSearch-Hit[aria-selected=true] [title="Remove this search from favorites"]:before:where() {
  background-color: #36415366;
}
```

The selector no longer matches, so this is not an equivalent minifier choice:
the dark background is lost. tw's output retains `:where(.dark, .dark *)`, and
the unminified Tailwind output is correct, which locates the corruption in the
Tailwind/lightningcss minification step rather than in tw or cascade.

Removing lightningcss from the comparison does not help. Measured on the site
corpus:

| harness | utilities layer |
|---|---|
| minify both sides (current) | 47 added, 1 removed, **8 modified** |
| neither side minified | 45 added, 12 removed, **527 modified** |
| neither minified, both flattened | 45 added, 8 removed, **162 modified** |

Tailwind's unminified output is heavily nested, and `--minify` is where
lightningcss flattens it, so dropping it trades six known differences for
hundreds of structural ones. Minifying both sides is the cleanest comparison
available.

This means parity as measured includes a minifier-agreement component that will
not reach zero unless cascade adopts lightningcss's choices - and for four of
the five, cascade's choice is the better one. The exception is the `@supports`
condition, where the two are weighing an author's feature test against a
browser-target fact and neither answer is wrong.

## Recurring bug shapes

Four patterns account for most of what the site comparison has found. When a new
family of utilities is added, check it against all four.

- **Invented theme token.** A utility references `var(--<family>-<name>)` when no
  `@theme` declares it, leaving a reference that resolves to nothing. Write the
  keyword instead.
- **Silent coercion.** An arbitrary value that does not parse falls back to a
  plausible one, so `rounded-[calc(...)]` became `0` and `object-[50%]` became
  `var(--50)`. Reject the class.
- **A route that rebuilds the selector from the bare class**, discarding what an
  inner variant already did. Rebase on the incoming selector instead; see
  `route_regular` in `lib/rule.ml`.
- **A palette colour looked up only in `Scheme.hex_color`**, which holds
  per-render overrides and is empty by default, so the palette hex is never found
  and the fallback degrades.
