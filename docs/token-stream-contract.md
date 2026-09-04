Title: The token-stream contract for arbitrary utilities

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
settles it. The value written into that longhand is not: the CLI's `--minify`
runs Lightning CSS, which rewrites values as well as whitespace, and the
snapshots in `test/upstream/utilities.txt` come from a run that was minified
too. So a value read off an unminified CLI sheet is not on its own a target.

Three families already differ that way and are right as they stand.
`outline-[2]` writes `outline-width: 2` unminified and `outline-width: 2px`
minified; `stroke-2` and `stroke-[1.5]` write `2` and `1.5` against `2px` and
`1.5px`; `decoration-[10%]` writes `text-decoration-thickness: 10%` against
`.1em`. tw writes the minified spelling in all three, `--diff` finds no
difference because it minifies both sides, and the corpus agrees. Minify both
sheets before calling one of these a divergence, or what you report is the
minifier's.

## What routes a bracket

A family tries its readers in order and writes what none of them took into a
last-resort longhand. The order is the family's own, and the value's validity
plays no part in choosing it: `mask-[red]` is a `mask-image` because no reader
took it, not because `red` is an image.

Two rules cross families. Tailwind reads a bracket holding a math function as a
length, so a value carrying `calc()`, `min()` or `clamp()` anywhere in it takes
the length-shaped branch however the rest of it reads: `mask-[foo_calc(1+2)]`
is a `mask-position` where `mask-[foo_10px]` is a `mask-image`.

And a bracket may open with a data-type hint, `<ident>:`, which chooses the
longhand and says nothing at all about the value. The name has to be an
identifier: `mask-[10px:2em]` holds `10px:2em` whole, and a `:` inside a
function call belongs to the value, so `mask-position-[url(http://x/a.png)]` is
a URL. A hint a family does not know still counts as one, and drops to that
family's last resort with no reading tried on what follows: `mask-[bogus:2em]`
is a `mask-image` where the bare `mask-[2em]` is a `mask-position`. A class
that names its longhand already, such as `mask-size-[…]`, takes any hint and
uses the value after it.

The hint has to stay in the class name. Dropping it from `to_class` writes a
selector no markup carries, which is the same defect as slicing an arbitrary
value: the declaration is right and nothing matches it.

A bracket whose math folds to a bare number is where the two spellings part,
and no one in the chain is wrong but Tailwind. `mask-[calc(1+2)]` makes
`mask-position: calc(1 + 2)`, which is what Tailwind's generator writes and
what tw now writes. Tailwind's minifier then folds it to `3px`, inventing a
unit the calculation never had, and cascade declines to read `calc(1 + 2)`
back at all, correctly, because a bare number is no length-percentage. `--diff`
forces minification, so it compares tw's `calc(1 + 2)` against Lightning CSS's
`3px` and reports a difference on those classes. Refusing the class instead
reported one too, so emitting is the better of the two; expect the residual
rather than chasing it.

## What each family does

The families below still refuse a bracket the CLI writes out; the mask family,
which no longer does, is the worked example at the end. The longhand column is
what the CLI writes it into, measured through `@apply` against tailwindcss
v4.3.3. Five value shapes cover most of it: a bare identifier
(`foo`), a colour keyword (`red`), a dashed identifier (`--c`), a unitless
`calc(1+2)`, and a `url()` with a word after it (`url(x.png)_center`).

There are 150 of them. 96 refuse the same five shapes:

| family | longhand Tailwind falls through to |
| --- | --- |
| `basis-[…]` | `flex-basis` |
| `bg-position-[…]` | `background-position` |
| `bg-size-[…]` | `background-size` |
| `block-[…]` | `block-size` |
| `bottom-[…]` | `bottom` |
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
| `m-[…]` | `margin` |
| `mask-radial-at-[…]` | `(custom properties only)` |
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
| `origin-[…]` | `transform-origin` |
| `outline-offset-[…]` | `outline-offset` |
| `p-[…]` | `padding` |
| `pb-[…]` | `padding-bottom` |
| `pbe-[…]` | `padding-block-end` |
| `pbs-[…]` | `padding-block-start` |
| `pe-[…]` | `padding-inline-end` |
| `perspective-origin-[…]` | `perspective-origin` |
| `pl-[…]` | `padding-left` |
| `pr-[…]` | `padding-right` |
| `ps-[…]` | `padding-inline-start` |
| `pt-[…]` | `padding-top` |
| `px-[…]` | `padding-inline` |
| `py-[…]` | `padding-block` |
| `right-[…]` | `right` |
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
| `top-[…]` | `top` |
| `underline-offset-[…]` | `text-underline-offset` |
| `w-[…]` | `width` |

The remaining 54 refuse a different set:

| family | longhand | shapes tw refuses |
| --- | --- | --- |
| `accent-[…]` | `accent-color` | ident, length, dashed, calc, url+word |
| `align-[…]` | `vertical-align` | ident, colour, length, dashed, calc, url+word |
| `backdrop-filter-[…]` | `-webkit-backdrop-filter` | ident, colour, length, dashed, calc, url+word |
| `bg-[…]` | `background-color` | ident, dashed, calc, url+word |
| `caret-[…]` | `caret-color` | ident, length, dashed, calc, url+word |
| `col-[…]` | `grid-column` | length, calc, url+word |
| `col-end-[…]` | `grid-column-end` | length, calc, url+word |
| `col-span-[…]` | `grid-column` | calc |
| `col-start-[…]` | `grid-column-start` | length, calc, url+word |
| `cursor-[…]` | `cursor` | ident, colour, length, dashed, calc, url+word |
| `decoration-[…]` | `text-decoration-color` | ident, dashed, calc, url+word |
| `fill-[…]` | `fill` | ident, length, dashed, calc, url+word |
| `filter-[…]` | `filter` | ident, colour, length, dashed, calc, url+word |
| `font-[…]` | `font-family` | length, calc, url+word |
| `font-features-[…]` | `font-feature-settings` | ident, colour, length, dashed, calc, url+word |
| `list-[…]` | `list-style-type` | ident, colour, length, dashed, calc, url+word |
| `list-image-[…]` | `list-style-image` | ident, colour, length, dashed, calc, url+word |
| `mask-b-from-[…]` | `mask-image` | dashed |
| `mask-b-to-[…]` | `mask-image` | dashed |
| `mask-conic-from-[…]` | `mask-image` | dashed |
| `mask-conic-to-[…]` | `mask-image` | dashed |
| `mask-l-from-[…]` | `mask-image` | dashed |
| `mask-l-to-[…]` | `mask-image` | dashed |
| `mask-linear-from-[…]` | `mask-image` | dashed |
| `mask-linear-to-[…]` | `mask-image` | dashed |
| `mask-r-from-[…]` | `mask-image` | dashed |
| `mask-r-to-[…]` | `mask-image` | dashed |
| `mask-radial-from-[…]` | `mask-image` | dashed |
| `mask-radial-to-[…]` | `mask-image` | dashed |
| `mask-t-from-[…]` | `mask-image` | dashed |
| `mask-t-to-[…]` | `mask-image` | dashed |
| `mask-x-from-[…]` | `mask-image` | dashed |
| `mask-x-to-[…]` | `mask-image` | dashed |
| `mask-y-from-[…]` | `mask-image` | dashed |
| `mask-y-to-[…]` | `mask-image` | dashed |
| `order-[…]` | `order` | ident, colour, length, dashed, url+word |
| `outline-[…]` | `outline-color` | ident, dashed, calc, url+word |
| `perspective-[…]` | `perspective` | ident, colour, dashed, var, calc, url+word |
| `rotate-x-[…]` | `transform` | ident, colour, length, dashed, var, calc, url+word |
| `rotate-y-[…]` | `transform` | ident, colour, length, dashed, var, calc, url+word |
| `rotate-z-[…]` | `transform` | ident, colour, length, dashed, var, calc, url+word |
| `row-[…]` | `grid-row` | length, calc, url+word |
| `row-end-[…]` | `grid-row-end` | length, calc, url+word |
| `row-span-[…]` | `grid-row` | calc |
| `row-start-[…]` | `grid-row-start` | length, calc, url+word |
| `stroke-[…]` | `stroke` | ident, dashed, calc, url+word |
| `text-[…]` | `color` | ident, dashed, calc, url+word |
| `text-shadow-[…]` | `text-shadow` | ident, length, dashed, calc, url+word |
| `transform-[…]` | `transform` | ident, colour, length, dashed, calc, url+word |
| `translate-[…]` | `translate` | ident, colour, dashed, var, calc, url+word |
| `translate-x-[…]` | `translate` | ident, colour, dashed, var, calc, url+word |
| `translate-y-[…]` | `translate` | ident, colour, dashed, var, calc, url+word |
| `will-change-[…]` | `will-change` | length, calc, url+word |
| `zoom-[…]` | `zoom` | ident, colour, length, dashed, calc, url+word |

Three of those rows are not a fall-through at all. `mask-radial-at-[…]`,
`col-span-[…]` and `row-span-[…]` write custom properties or a shorthand rather
than one longhand, so read the CLI directly before converting them.

## What already holds

`grep -l opaque_declaration lib/*.ml` names the families that already do. The
mask family is the worked example: `lib/masks.ml` routes every bracket its
readers decline into `mask-image`, `mask-position` or `mask-size` according to
the hint the class carries, and `test/test_masks.ml` pins each one against the
CLI.

Two mask readings are wrong in a way this contract does not reach, because
both take a bracket their readers accept. `mask-[30%_50%,70%_50%]` is a
`mask-size` to Tailwind and a `mask-position` to tw, which also drops half of
each pair; `mask-[image-set(url(a.png)_1x)]` writes the inner URL quoted and
without the `-webkit-image-set` fallback beside it.

Two things are still open and neither is a family's own fault. The `color:`
data-type hint is read in one place for every colour-bearing utility,
`parse_bracket_hint` in `lib/color.ml`, so `text-[color:1.25rem]` and its
siblings need an opaque case threaded through each family's variant rather than
a change where the hint is read. And the `length:` hint in `lib/typography.ml`,
`lib/borders.ml` and `lib/svg.ml` refuses a value its width reader declines,
which is the same one-line shape in three modules.
