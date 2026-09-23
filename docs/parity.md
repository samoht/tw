# Measuring parity with Tailwind

`tw` aims to style every page exactly as Tailwind CSS v4.3.3 would. This page
explains what "exactly" means, how it is checked, and what to do when a check
fails. It is written for contributors; users only need `tw --diff`, described
in the [README](../README.md).

## What counts as the same

Comparing the two stylesheets byte for byte is too strict. `tw` prints CSS
through [cascade](https://github.com/samoht/cascade), which spells colours,
numbers and selectors differently from Tailwind, and most of those differences
change nothing on the page. Comparing them by eye is too loose. So parity has
two parts:

- **The page renders the same.** Load one HTML document under Tailwind's sheet
  and under `tw`'s, at every viewport width and interaction state (hover,
  focus, and so on) the sheets mention, and the screenshots must match pixel
  for pixel. The browser is the judge. If two spellings paint the same pixels,
  they are the same, whatever `getComputedStyle` reports.
- **The output is no larger.** `tw --minify` must never produce a bigger file
  than `tailwindcss --minify`.

The reference is Tailwind's compiled sheet before lightningcss minifies it, so
a difference is always `tw` against Tailwind, never one minifier against
another. Chromium ignores a declaration it cannot parse, so if Tailwind emits
one and `tw` emits nothing, the two are at parity.

A render only sees what it paints. Properties such as `cursor` or a
transition's timing leave no pixels, so for those the CSS-level comparison
below is what catches a mistake.

## The two comparisons

Two tools answer the question, and they check each other:

- **The browser render**, `cascade diff --browser --html PAGE A.css B.css`,
  loads the page under each sheet, takes screenshots and compares them. Where
  pixels differ, it lists the computed values of the elements under them, so
  the report points at a property. This is the verdict, but it only covers
  the rules the page actually uses.
- **The canonical diff**, `cascade diff --diff=canonical A.css B.css`, needs no
  page. It normalises both sheets and reports every rule or declaration that
  could compute differently. It covers the whole sheet, but it can be wrong in
  either direction, so each entry it lists is a candidate for the browser to
  confirm. By default it assumes the evergreen browsers `cascade --minify`
  targets, and `--enforce-spec` drops that assumption.

When the two disagree, the table says where the bug is:

| The browser | The canonical diff | Where to fix it |
|---|---|---|
| renders differently | anything | `tw` |
| renders the same | lists a difference | `cascade` over-reports; file a reproducer there |
| renders differently | lists nothing | `cascade` under-reports; this is the worse case, since every check that trusts the diff missed it |

## What runs in `dune runtest`

The browser checks need node and a headless Chromium (found through `NODE`
and `CHROME`, or in the usual places). The Tailwind checks need the pinned
`tailwindcss` CLI, which `npm ci` installs from `package-lock.json`. A check
whose tool is missing is skipped with a message. CI sets `TW_BROWSER_TESTS=1`
and `TW_TAILWIND_TESTS=1`, which turn those skips into failures.

- **Project entrypoints, `test/parity/corpus/`.** Each `NAME.css` there is a
  complete Tailwind entrypoint. Both tools compile it, and the result is
  compared canonically and rendered over `NAME.html`, or, when there is no
  such file, over a page built from the classes its `@source inline("...")`
  names. To add a case, drop a file in the directory. The cases cover what
  single classes do not: a project theme, theme namespaces reset to
  `initial`, custom variants, `@apply`, variants stacked three deep, and a
  prefixed build.
- **Rendering per class.** Nine test suites render their classes under both
  sheets. Each class gets its own element containing some text and a bar as
  wide as its content box, so a property that affects the box shows up in the
  screenshot; an empty element would paint nothing. Two classes that set the
  same property also share an element, so an ordering mistake shows up there. The page and sheets of each run are kept under `tmp/browser/`.
- **Tailwind's own tests, `test/upstream/`.** `utilities.txt` and
  `variants.txt` are extracted from the v4.3.3 tag by `extract_tests.ml`, and
  every class Tailwind accepts must compile the same way in `tw`. A class
  Tailwind has no test for gets one in the `test_<module>.ml` of its utility.
- **Examples, `examples/*/dune`.** Nine example projects are compiled by both
  tools and compared canonically.
- **Rule order, `test/parity/dune`.** Tailwind fixes the order of utilities
  even where the order cannot change the page, and the canonical diff ignores
  such reorders. This check counts how many rules would have to move for `tw`'s
  order to match Tailwind's in `@layer utilities` and `@layer components`. The
  count is pinned at zero.

## Comparing against tailwindcss.com

The Tailwind website uses far more class combinations than any fixture. Its
class list and entrypoint are committed under `test/parity/`: `classlist.txt`
holds every class the site uses, `globals.css` is the entrypoint `tw`
compiles, and `ref-entry.css` is the same entrypoint told to read only
`classlist.txt`, which Tailwind compiles.

<!-- $MDX skip -->
```sh
sh test/parity/measure.sh                           # canonical diff and size
TW_PARITY_RENDER="0 50" sh test/parity/measure.sh   # also render classes 0 to 49
```

The script builds both tools from the workspace and writes its outputs to
`tmp/parity/`. It prints the diff, and it fails if `tw`'s minified sheet is
the larger one.
With `TW_PARITY_RENDER`, it also renders one batch of 50 classes. Each class
sits on its own element, inside a wrapper carrying every `group` name the list
uses and after a sibling carrying every `peer` name. Variants that test an
ancestor's attribute or class, such as `group-data-[checked]:` or
`in-[.dark]:`, match on neither side, so the render does not exercise them. A
batch takes about 25 seconds and the list has 97 batches, so the full render is
run by hand.

The last full run used tw at #869, cascade at #1268 and tailwindcss 4.3.3.
The canonical diff reported the two sheets identical, `tw` produced 661,153
bytes against Tailwind's 664,632, and all 97 batches rendered the same across
30,264 screenshots (twelve viewports, thirteen states, two sheets). A newer
cascade can change these figures, so compare them only against a run with the
same revisions.

## Investigating a difference

Start small:

<!-- $MDX skip -->
```sh
dune exec -- tw --single="hover:bg-blue-600" --diff
mkdir -p tmp && echo '<div class="bg-blue-500 hover:bg-blue-600">x</div>' > tmp/page.html
dune exec -- tw -s "bg-blue-500 hover:bg-blue-600" --diff --html tmp/page.html
dune exec -- tw -i app.css page.html --diff --html page.html
```

Use `--single=` for a class that starts with `-` or contains spaces. With
`--html`, the canonical report comes first and the render second, and every
class compared must appear in the page. With both sheets already on disk,
`cascade diff --browser --html PAGE tw.css tailwind.css` runs the same render.
It exits 0 when the pages match, 1 when they differ, and 2 when no browser ran
or a sheet could not be read.

An entry in the canonical diff is a lead, not yet a bug. Cut it down to the
classes involved, render them over a page that contains them (and the markup
their variants look for), and file it according to the table above.

A few things regularly mislead:

- **`added` means "only in the second file".** `cascade diff A B` shows A as
  `---` and B as `+++`. `measure.sh` passes `tw` first, so `added` means
  Tailwind emits something `tw` does not.
- **The summary counts containers, not rules.** "3 changed containers" can
  hide a hundred rules. `--limit=none` prints them all.
- **`--minify` and `--optimize` apply to both sides of `--diff`.** `tw` then
  minifies with cascade and Tailwind with lightningcss, so part of any
  difference is one minifier against the other. Drop the flags before calling
  something a `tw` bug.
- **A reorder the canonical diff ignores can still be a `tw` bug**, because
  Tailwind fixes an order the page does not depend on. `check_class_order` in
  `test/test_sort.ml` checks order within a family, and the rule-order check
  above checks where each family sits.
- **A reorder the canonical diff lists may paint nothing.** The diff flags two
  rules that set the same property and might match the same element, without
  checking whether they do. The browser decides.
- **Use the cascade built in this workspace.** `%{bin:cascade}` and
  `measure.sh` do; an older cascade from an opam switch reports differences
  that are not there.

## Common causes

Most bugs the site comparison has found had one of five causes, so a new
utility family is worth checking against each:

- **A theme variable that does not exist.** The utility writes
  `var(--<family>-<name>)` but no `@theme` declares it. Write the literal
  value instead.
- **A bad value quietly replaced by a plausible one.** `rounded-[calc(...)]`
  once became `0`, and `object-[50%]` became `var(--50)`. An arbitrary value
  that does not parse should reject the class.
- **A selector rebuilt from the bare class name**, which throws away what an
  inner variant had already added. Build on the incoming selector instead, as
  `route_regular` in `lib/rule.ml` does.
- **A palette colour looked up only in `Scheme.hex_color`.** That table holds
  per-render overrides and is empty by default, so the lookup fails and the
  fallback is wrong.
- **A class name printed back from its parsed value.** The class name is also
  the selector that must match the HTML, so it must come out exactly as
  written. `min-[0.5ch]:flex` once produced `.min-\[\.5ch\]\:flex`, because
  the number printer drops the leading zero. Test a new family with a value
  the printer would respell, and check the selector as well as the
  declaration.

## Known differences

- **Placeholder classes on the Tailwind site.** Its documentation contains
  literal placeholders, so the class list includes `blur-[<value>]`,
  `shadow-[<value>]` and about sixty similar names, plus `bg-[--brand-color]`
  (v3 syntax) and `justify-baseline`. Tailwind emits declarations for them
  that no browser accepts, and `tw` emits nothing. Both pages render the same.
  cascade drops those declarations as a browser would, with a parse warning
  each, so the canonical diff still reports no difference.
- **Arbitrary values that fit neither property.** `border-[50%]` becomes
  `border-color: 50%` in Tailwind and `border-width: 50%` in `tw`, and
  `decoration-[2]` becomes `text-decoration-color: 2` in Tailwind and nothing
  in `tw`. `tw`'s typed colour values cannot hold a number, and browsers ignore
  both forms anyway.
- **The size comparison is also a minifier comparison.** cascade folds
  `calc()` only when the result is exact, so the typography plugin keeps
  `line-height: calc(28/18)` where lightningcss writes `1.55556`.

Anything else the site comparison reports is worth investigating, and so is
any of these differences disappearing.
