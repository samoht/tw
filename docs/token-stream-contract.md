# The token-stream contract for arbitrary utilities

An arbitrary utility emits its bracket contents verbatim, including values
invalid for the target property, in every family, wherever the CLI emits. Where
Tailwind emits nothing, tw emits nothing. Whether a class emits must not depend
on which family it belongs to.

Where Tailwind emits a declaration a browser drops, tw emitting it too costs
nothing: the browser drops it either way, and production optimisation drops it
as well. Refusing the class instead costs the selector, which is the one thing
the browser would have kept.

`Parse.opaque_declaration` is how a family holds such a value. It skips the
typed grammar and keeps the component stream the author wrote, so a family need
not model a value it cannot read. The one thing it refuses is a value that
would end the declaration or swallow what follows it, and Tailwind emits
nothing for those too, so the two agree.

## Reading a family off the CLI

Each family decides for itself which longhand an unreadable bracket lands in,
and nothing about a neighbour predicts it. Measure the family:

<!-- $MDX skip -->
```sh
printf '@import "tailwindcss" source(none);\n.probe { @apply mask-[url(x.png)_center]; }\n' > probe.css
npx tailwindcss -i probe.css -o -
```

`@apply` hard-errors on a utility that does not exist, so an error is the CLI
saying it emits nothing, and that is the condition under which tw must refuse
too. The probe file has to sit inside the project, or the CLI exits for an
unrelated reason and reads as a false refusal.

Scanning a file is the weaker probe: Tailwind's extractor declines some class
spellings before any utility is consulted, and a class it declines is not a
class Tailwind refuses. `--diff` over a file compares strings, and
`--diff-mode=canonical` is ignored there; only `-s` honours it.

## Reading a value off the CLI

Which longhand a bracket lands in is the same minified or not, so `@apply`
settles it. The value written into that longhand is not. The CLI's `--minify`
runs Lightning CSS, which rewrites values as well as whitespace, and the
snapshots in `test/upstream/utilities.txt` come from a minified run too. So a
value read off an unminified CLI sheet is not on its own a target.

Four families differ that way, and tw is right in each.
`outline-[2]` writes `outline-width: 2` unminified and `outline-width: 2px`
minified; `stroke-2` and `stroke-[1.5]` write `2` and `1.5` against `2px` and
`1.5px`; `text-[length:12]` writes `font-size: 12` against `12px`; and
`decoration-[10%]` writes `text-decoration-thickness: 10%` against `.1em`. tw
writes the minified spelling in all four, `--diff` finds no difference because
it minifies both sides, and the corpus agrees. Minify both sheets before
calling one of these a divergence, or what you report is the minifier's.

## What routes a bracket

A family tries its readers in order and writes what none of them took into a
last-resort longhand. The order is the family's own, and the value's validity
plays no part in choosing it: `mask-[red]` is a `mask-image` because no reader
took it, not because `red` is an image.

Two rules cross families. Tailwind reads a bracket holding a math function as a
length, so a value carrying `calc()`, `min()` or `clamp()` anywhere in it takes
the length-shaped branch however the rest of it reads: `mask-[foo_calc(1+2)]`
is a `mask-position` where `mask-[foo_10px]` is a `mask-image`.

And a bracket may open with a data-type hint, which chooses the longhand and
says nothing at all about the value. The name is a run of `a`-`z` and `-`
closed by a `:` and nothing wider, so `mask-[10px:2em]`, `mask-[FOO:2em]` and
`mask-[a1:2em]` hold their brackets whole, and a `:` inside a function call
belongs to the value, so `mask-position-[url(http://x/a.png)]` is a URL. Only
the leading run is the hint's: `z-[color:red:blue]` writes `red:blue`. A hint a
family does not know still counts as one, and drops to that family's last
resort with no reading tried on what follows: `mask-[bogus:2em]` is a
`mask-image` where the bare `mask-[2em]` is a `mask-position`. A class that
names its longhand already, such as `mask-size-[…]`, takes any hint and uses
the value after it. An empty hint names no utility at all, and neither does a
bracket left holding nothing but blank space, so `z-[:5]` and `z-[_]` are
refusals.

The hint has to stay in the class name. Dropping it from `to_class` writes a
selector no markup carries, which is the same defect as slicing an arbitrary
value: the declaration is right and nothing matches it.

A bracket whose math folds to a bare number is where the two spellings part.
`mask-[calc(1+2)]` makes `mask-position: calc(1 + 2)` in Tailwind's generator
and in tw. Tailwind's minifier then folds it to `3px`, a unit the calculation
never had, and cascade declines to read `calc(1 + 2)` back, because a bare
number is no length-percentage. `--diff` forces minification, so it compares
tw's `calc(1 + 2)` against Lightning CSS's `3px` and reports a difference on
those classes. Refusing the class would report a difference too, so tw emits
it and the residual stays.

## Family longhands

For each family, the table gives the longhand the CLI writes a bracket into
when none of the family's readers takes it, read off `@apply` against
tailwindcss v4.3.3. Five value shapes probe a family. They are a bare
identifier (`foo`), a colour keyword (`red`), a dashed identifier (`--c`), a
unitless `calc(1+2)`, and a `url()` with a word after it (`url(x.png)_center`);
a length such as `10px` is a sixth. To check a family, run the shapes through
`tw --single=... --variables --no-base` and count the declarations tw emits.

| family | longhand Tailwind falls through to |
| --- | --- |
| `accent-[…]` | `accent-color` |
| `align-[…]` | `vertical-align` |
| `backdrop-filter-[…]` | `-webkit-backdrop-filter` |
| `basis-[…]` | `flex-basis` |
| `bg-[…]` | `background-color` |
| `bg-position-[…]` | `background-position` |
| `bg-size-[…]` | `background-size` |
| `block-[…]` | `block-size` |
| `bottom-[…]` | `bottom` |
| `caret-[…]` | `caret-color` |
| `col-[…]` | `grid-column` |
| `col-end-[…]` | `grid-column-end` |
| `col-span-[…]` | `grid-column` |
| `col-start-[…]` | `grid-column-start` |
| `cursor-[…]` | `cursor` |
| `decoration-[…]` | `text-decoration-color` |
| `fill-[…]` | `fill` |
| `filter-[…]` | `filter` |
| `font-[…]` | `font-family` |
| `font-features-[…]` | `font-feature-settings` |
| `gap-[…]` | `gap` |
| `gap-x-[…]` | `column-gap` |
| `gap-y-[…]` | `row-gap` |
| `h-[…]` | `height` |
| `indent-[…]` | `text-indent` |
| `inline-[…]` | `inline-size` |
| `inset-[…]` | `inset` |
| `inset-be-[…]` | `inset-block-end` |
| `inset-bs-[…]` | `inset-block-start` |
| `inset-e-[…]` | `inset-inline-end` |
| `inset-s-[…]` | `inset-inline-start` |
| `inset-x-[…]` | `inset-inline` |
| `inset-y-[…]` | `inset-block` |
| `left-[…]` | `left` |
| `list-[…]` | `list-style-type` |
| `list-image-[…]` | `list-style-image` |
| `m-[…]` | `margin` |
| `mask-b-from-[…]` | `mask-image` |
| `mask-b-to-[…]` | `mask-image` |
| `mask-conic-from-[…]` | `mask-image` |
| `mask-conic-to-[…]` | `mask-image` |
| `mask-l-from-[…]` | `mask-image` |
| `mask-l-to-[…]` | `mask-image` |
| `mask-linear-from-[…]` | `mask-image` |
| `mask-linear-to-[…]` | `mask-image` |
| `mask-r-from-[…]` | `mask-image` |
| `mask-r-to-[…]` | `mask-image` |
| `mask-radial-at-[…]` | `(custom properties only)` |
| `mask-radial-from-[…]` | `mask-image` |
| `mask-radial-to-[…]` | `mask-image` |
| `mask-t-from-[…]` | `mask-image` |
| `mask-t-to-[…]` | `mask-image` |
| `mask-x-from-[…]` | `mask-image` |
| `mask-x-to-[…]` | `mask-image` |
| `mask-y-from-[…]` | `mask-image` |
| `mask-y-to-[…]` | `mask-image` |
| `max-block-[…]` | `max-block-size` |
| `max-h-[…]` | `max-height` |
| `max-inline-[…]` | `max-inline-size` |
| `max-w-[…]` | `max-width` |
| `mb-[…]` | `margin-bottom` |
| `mbe-[…]` | `margin-block-end` |
| `mbs-[…]` | `margin-block-start` |
| `me-[…]` | `margin-inline-end` |
| `min-block-[…]` | `min-block-size` |
| `min-h-[…]` | `min-height` |
| `min-inline-[…]` | `min-inline-size` |
| `min-w-[…]` | `min-width` |
| `ml-[…]` | `margin-left` |
| `mr-[…]` | `margin-right` |
| `ms-[…]` | `margin-inline-start` |
| `mt-[…]` | `margin-top` |
| `mx-[…]` | `margin-inline` |
| `my-[…]` | `margin-block` |
| `object-[…]` | `object-position` |
| `order-[…]` | `order` |
| `origin-[…]` | `transform-origin` |
| `outline-[…]` | `outline-color` |
| `outline-offset-[…]` | `outline-offset` |
| `p-[…]` | `padding` |
| `pb-[…]` | `padding-bottom` |
| `pbe-[…]` | `padding-block-end` |
| `pbs-[…]` | `padding-block-start` |
| `pe-[…]` | `padding-inline-end` |
| `perspective-[…]` | `perspective` |
| `perspective-origin-[…]` | `perspective-origin` |
| `pl-[…]` | `padding-left` |
| `pr-[…]` | `padding-right` |
| `ps-[…]` | `padding-inline-start` |
| `pt-[…]` | `padding-top` |
| `px-[…]` | `padding-inline` |
| `py-[…]` | `padding-block` |
| `right-[…]` | `right` |
| `rotate-x-[…]` | `transform` |
| `rotate-y-[…]` | `transform` |
| `rotate-z-[…]` | `transform` |
| `rounded-[…]` | `border-radius` |
| `rounded-b-[…]` | `border-bottom-right-radius` |
| `rounded-bl-[…]` | `border-bottom-left-radius` |
| `rounded-br-[…]` | `border-bottom-right-radius` |
| `rounded-e-[…]` | `border-start-end-radius` |
| `rounded-ee-[…]` | `border-end-end-radius` |
| `rounded-es-[…]` | `border-end-start-radius` |
| `rounded-l-[…]` | `border-top-left-radius` |
| `rounded-r-[…]` | `border-top-right-radius` |
| `rounded-s-[…]` | `border-start-start-radius` |
| `rounded-se-[…]` | `border-start-end-radius` |
| `rounded-ss-[…]` | `border-start-start-radius` |
| `rounded-t-[…]` | `border-top-left-radius` |
| `rounded-tl-[…]` | `border-top-left-radius` |
| `rounded-tr-[…]` | `border-top-right-radius` |
| `row-[…]` | `grid-row` |
| `row-end-[…]` | `grid-row-end` |
| `row-span-[…]` | `grid-row` |
| `row-start-[…]` | `grid-row-start` |
| `scroll-m-[…]` | `scroll-margin` |
| `scroll-mb-[…]` | `scroll-margin-bottom` |
| `scroll-mbe-[…]` | `scroll-margin-block-end` |
| `scroll-mbs-[…]` | `scroll-margin-block-start` |
| `scroll-me-[…]` | `scroll-margin-inline-end` |
| `scroll-ml-[…]` | `scroll-margin-left` |
| `scroll-mr-[…]` | `scroll-margin-right` |
| `scroll-ms-[…]` | `scroll-margin-inline-start` |
| `scroll-mt-[…]` | `scroll-margin-top` |
| `scroll-mx-[…]` | `scroll-margin-inline` |
| `scroll-my-[…]` | `scroll-margin-block` |
| `scroll-p-[…]` | `scroll-padding` |
| `scroll-pb-[…]` | `scroll-padding-bottom` |
| `scroll-pbe-[…]` | `scroll-padding-block-end` |
| `scroll-pbs-[…]` | `scroll-padding-block-start` |
| `scroll-pe-[…]` | `scroll-padding-inline-end` |
| `scroll-pl-[…]` | `scroll-padding-left` |
| `scroll-pr-[…]` | `scroll-padding-right` |
| `scroll-ps-[…]` | `scroll-padding-inline-start` |
| `scroll-pt-[…]` | `scroll-padding-top` |
| `scroll-px-[…]` | `scroll-padding-inline` |
| `scroll-py-[…]` | `scroll-padding-block` |
| `size-[…]` | `width` |
| `stroke-[…]` | `stroke` |
| `text-[…]` | `color` |
| `text-shadow-[…]` | `text-shadow` |
| `top-[…]` | `top` |
| `transform-[…]` | `transform` |
| `translate-[…]` | `translate` |
| `translate-x-[…]` | `translate` |
| `translate-y-[…]` | `translate` |
| `underline-offset-[…]` | `text-underline-offset` |
| `w-[…]` | `width` |
| `will-change-[…]` | `will-change` |
| `zoom-[…]` | `zoom` |

Three rows are not a fall-through to one longhand. `mask-radial-at-[…]`,
`col-span-[…]` and `row-span-[…]` write custom properties or a shorthand, so
read the CLI directly before changing them.

## Token streams and hints

`grep -l opaque_declaration lib/*.ml` names the families that write a token
stream. The mask family is the worked example. `lib/masks.ml` routes every
bracket its readers decline into `mask-image`, `mask-position` or `mask-size`
according to the hint the class carries, and `test/test_masks.ml` pins each one
against the CLI.

The hint comes off in two places and nowhere else.
`Parse.arbitrary_declaration_value` peels it for a family falling through to a
token stream, so such a family passes the whole bracket and gets the value
back. `Parse.value_after_hint` peels it for a family whose reader is typed, so
the value stays typed where a last resort would not keep it. The sizing,
padding, margin, gap, inset and scroll families call it, as do `rounded-[…]`,
`outline-offset-[…]` and the four colour families in `lib/color.ml`.
`Parse.data_type_hint` is the same scan on its own, for a family that routes on
which hint was written. Peeling in the family and passing the remainder peels
twice, and `mask-[bogus:foo:2em]` is where that shows.

A family that writes one longhand takes any hint, so `w-[foo:10px]` is a width
and `accent-[foo:red]` an accent colour. A family that writes several routes on
the name, and the names are the family's own. `border-[…]` reads `length:` and
`line-width:` as the width and every other hint as the colour. `outline-[…]`
reads `length:`, `number:` and `percentage:` as the width and does not know
`line-width:`, so `outline-[line-width:2px]` is an `outline-color`. `bg-[…]`
spells its position hint two ways, `position:` and `percentage:`. Read the
family off the CLI before adding a name to it.

The colour families (`text-`, `outline-`, `border-` and its per-side longhands,
`fill-`, `stroke-`, `accent-`, `caret-`, `placeholder-` and `bg-`) write a
bracket no reader took into their colour, which is each one's last resort. So
`text-[foo:1.25rem]`, `text-[notacolour]`, `outline-[foo:red]` and a malformed
hex all reach the sheet. `bg-[…]` runs its bracket through a size, a position,
an image and a url reader, each forwarding its own value, so only an unhinted
bracket reaches the colour. That fall-through takes one bracket value only, or
`bg-[10px][20px]` would be accepted as a single declaration. An opacity
modifier over a value that is not a colour is refused everywhere, because the
CLI writes a `color-mix()` around the raw token and cascade's colour types hold
colours, not token streams.

The width hints in `lib/typography.ml`, `lib/borders.ml` and `lib/svg.ml`
carry a variant holding `(spelling, value)`. A value the width reader declines
goes through `opaque_declaration` on the longhand the hint named. Where the
family writes a style declaration beside the width (`border-`, `outline-`),
that declaration travels with the raw value, so the browser drops the width
and keeps the style.

## Open gaps

These families refuse shapes the CLI writes out. Each is a defect against the
contract.

- `col-[…]`, `col-start-[…]`, `col-end-[…]`, `row-[…]`, `row-start-[…]` and
  `row-end-[…]` refuse a length and a `url()` with a word after it.
- `font-[…]`, `list-[…]` and `will-change-[…]` refuse a length, `calc(1+2)`
  and a `url()` with a word after it.
- `decoration-[…]` refuses an identifier, a dashed identifier, `calc(1+2)` and
  a `url()` with a word after it.
- `text-shadow-[…]` refuses every shape but a colour keyword.

Two mask readings are wrong in a way this contract does not reach, because
their readers accept the bracket. `mask-[30%_50%,70%_50%]` is a `mask-size` to
Tailwind and a `mask-position` to tw, which also drops half of each pair.
`mask-[image-set(url(a.png)_1x)]` lacks the `-webkit-mask-image:
-webkit-image-set(...)` fallback Tailwind writes beside it.
