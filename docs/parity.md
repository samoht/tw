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

```
dune exec test/upstream/test.exe
```

These files are generated. Never hand-edit them; regenerate from upstream.

### Example pages - `examples/*/dune`

Each of the nine examples builds its CSS twice, once through tw and once through
`npx tailwindcss`, then diffs the two:

```
cascade diff --diff=canonical --prune-unused-custom-props landing.css tailwind.css
```

The rule is `(enabled_if %{bin-available:npx})`, so it is skipped where npx is
absent. `%{bin:cascade}` resolves through the dune workspace, so this is the
freshly built cascade, not whatever is on `PATH`.

### One class at a time

```
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
| `hue-rotate(0deg)` | `hue-rotate()` | keeps it |
| `@supports (backdrop-filter: ...)` | keeps the guard | elides it |
| `@media not (X)` | Level 4 form | `not all and (X)` |
| `grid-auto-flow: row dense` | keeps `row` | `dense` |
| `text-decoration-color` | no prefix | adds `-webkit-` |
| `calc(28 / 18)` | keeps it | folds to `1.55556` |

The guard and the downlevelling are threshold differences: cascade's bar is
Baseline "widely available", lightningcss's is Tailwind's browserslist. The
`calc` refusal is a precision commitment - the quotient does not survive
cascade's serialisation rounding.

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
not reach zero unless cascade adopts lightningcss's choices - and for at least
three of the six, cascade's choice is the better one.

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
