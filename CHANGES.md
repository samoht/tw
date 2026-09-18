## 1.1.0 (unreleased)

### Breaking changes

- Variable ordering and `@property` metadata travel with `Var` values and
  `Style.t` rather than through process-global registries. `Var.order`,
  `Var.family`, `Var.property_order`, `Var.needs_property`,
  `Var.register_property_order` and `Var.resolve_theme_refs` are removed, and
  the public `Style.Style` record gains a `metadata` field. Construction,
  parsing and rendering are safe to run from several OCaml domains without a
  mutex, and the typed `property_default` rules are generated (#653).
- `bg_transparent`, `bg_current` and the background colour constructors live
  in `Backgrounds`, and `border_color`, `border_transparent` and
  `border_current` in `Color`; the copies `Color` and `Borders` carried are
  gone. Building one from OCaml and parsing the same class name reached
  different handlers with different sort slots (#518).
- Every spacing utility takes an `int`. `space_x`, `space_y`, `indent`,
  `border_spacing` and the `scroll_m*` and `scroll_p*` families took a
  `float`; a primed variant takes one now, so `space_x 2.5` becomes
  `space_x' 2.5` (#492).
- An unrecognised class no longer raises. `Tw.str` raised, `Tw_html` copied the
  name in silently, and `Tw_dom.use_str` crashed the browser render. Unknown
  names pass through everywhere, never raise from a rendering path, and are
  reportable instead: `Tw.of_classes` returns them alongside the utilities, and
  `Tw_html.unknown_classes` and `Tw_dom.unknown_classes` expose the same list.
  Code that caught the exception to find a typo reads that list. `Tw.of_string`
  is unchanged, so the CLI still reports a deliberately typed class as an error
  (#514).
- `-safe` alignment resolves on `center` and `end` only, as in Tailwind. The
  `-start-safe` classes stop being emitted, and `Alignment.content_start_safe`
  and `Alignment.place_items_start_safe` are removed (#679).
- A project build emits the typography plugin's `prose` utilities only when
  its entrypoint declares `@plugin "@tailwindcss/typography"`, as Tailwind
  does. `tw src/` with no entrypoint wrote the plugin's whole stylesheet for
  any `prose` in the markup; `tw -s prose` still compiles the class on its own
  (#142).
- `re` is a regular dependency rather than a test-only one. The CLI and the
  dev tools use it to search text no one typed; it stays out of the `tw.dom`
  bundle, which links neither (#774).

### Tailwind CSS 4.3.3

- Track Tailwind CSS 4.3.3, from 4.3.1. `font-sans` carries the 4.3.2 system
  stack, preflight scopes `:-moz-focusring` to non-iframe elements, and an
  achromatic colour writes its powerless hue as `none` (#128, #129, #130, #132,
  #147).
- The mauve, mist, olive and taupe palettes, declared between `stone` and
  `black` and sorting among the rest where Tailwind puts them (#153, #696).

### Project stylesheets

- `tw` compiles a project's CSS entrypoint. `--input-css` fed the file to
  Tailwind for `--diff` and read a few `@theme` overrides off it; now the whole
  stylesheet is the input, as it is for `tailwindcss`: the directives below
  expand, the author's own rules go through in their own spelling with the
  browser-compatibility prefixes the CLI writes, and Tailwind's at-rules
  (`@theme`, `@source`, `@plugin`, `@config`, `@reference`, `@tailwind`) stay
  out of the sheet a browser reads. The `@supports`-guarded initial values of
  the utility variables are written once, whatever the entrypoint pulls in
  (#136, #138, #139, #140, #141, #143, #195, #206, #361, #665, #687).
- `@import "tailwindcss"` takes its options. `theme(static)` declares the
  whole theme, `@keyframes` included, once; `source(none)` and
  `source("../src")` decide what is scanned; `prefix(tw)` puts `tw:` in front
  of every candidate, on the rule a stacked variant nests under its query and
  on the `group`/`peer` anchors, and `--tw-` in front of the theme's own
  tokens, the `--tw-*` channels a utility sets and a `var(--brand)` the
  author wrote keeping their names; `important` marks every utility
  declaration `!important`, variants and declared `@utility` rules included,
  while author CSS stays as written; and `layer(…)` puts an imported file's
  rules in that layer with its theme tokens, `@property` registrations and
  keyframes at the top of the sheet. `tailwindcss/theme.css`,
  `tailwindcss/utilities.css` and `@tailwind utilities` each bring their own
  part of the sheet, and `@reference "tailwindcss"` puts the theme in scope
  for `@apply` and emits none of it, each token an applied utility reads
  carrying its value as the `var()` fallback. A file the entrypoint imports
  from beside it is read under `--diff` and `--tailwind` too (#781, #788,
  #793, #794, #796, #801, #816, #844, #849).
- A project's `@theme` reaches every utility generated from the markup: its
  colours, spacing, shadows, blur radii, timing functions, font weights, line
  heights, letter spacing, corner radii, font sizes, perspectives, aspect
  ratios, max-widths, fonts, breakpoints, container sizes, `@keyframes` and
  animations, and a token a utility reads is declared in the theme layer. A
  size carries the `--text-<name>--line-height`, `--letter-spacing` and
  `--font-weight` declared beside it, a family its `--font-feature-settings`
  and `--font-variation-settings`, a container size of the project's own
  names a query (`@hero:`, `@min-hero:`, `@hero/main:`) sorted among the
  built-in sizes by width, a breakpoint of the project's own sorts with the
  scale, and a project shadow takes an opacity (#170, #179,
  #193, #221, #227, #229, #230, #261, #315, #320, #447, #450, #457, #458,
  #459, #460, #461, #462, #463, #464, #510, #725, #847). `--name: initial`
  removes a token and `--namespace-*: initial` a whole namespace, in every
  family: a removed breakpoint or container size names no variant, and a
  removed colour, font, size, radius, shadow, animation or any other token no
  utility reading it, as Tailwind compiles nothing for the candidate; a size
  query reads the width the block binds to `--container-<size>` (#507, #515,
  #847). The bare `--*: initial` reaches the `--default-*` tokens the way
  Tailwind reads them, as `--theme(--default-<x>, <fallback>)`: preflight
  writes its font stacks and `normal` themselves and the transition family
  `ease` and `0s` (#847). `@theme static` declares its tokens whether or not
  a utility reads them; `@theme inline` folds a token's value into every
  utility that reads
  it, `p-4` writing `calc(.25rem * 4)` under an inline `--spacing` and
  `text-brand` writing `var(--brand)` under the shadcn-style `--color-brand:
  var(--brand)`, in every colour family and opacity form; and `@theme
  reference` declares nothing, a utility reading its token carrying the value
  as the `var()` fallback (#783, #790, #795, #826, #847). A project's own
  `@property` gets the `@supports`-guarded initial value in `@layer
  properties` that Tailwind writes for its own (#791).
- `@utility` declares a utility, static or functional. `@utility tab-*`
  resolves `--value()` and `--modifier()` with their data-type and theme
  arguments, every declaration made for one name applies, a body may `@apply`
  another utility or nest `@variant`, and the utility sorts after the
  built-ins of its family, as Tailwind puts it, so on an element carrying both
  the declared one wins. A candidate a functional declaration declines falls
  to the built-in utility of its root, as Tailwind tries every one registered,
  a `!` on a declared utility marks its declarations `!important`, a
  malformed declaration costs only its own class, and `tw -s CLASS
  --input-css ENTRY` generates a declared utility as the scanning form does
  (#516, #526, #550, #554, #650, #651, #655, #785, #798, #847).
- `@apply` pulls a utility's declarations into the author's rule, variants
  included, a run of plain utilities landing in one rule as Tailwind writes
  them. The theme tokens, `@property` registrations and `@keyframes` the
  applied utilities read go at the top of the sheet, and `@apply prose` keeps
  the `.prose` the typography plugin scopes its nested-list spacing to (#779,
  #799).
- `@custom-variant` declares a variant, with a selector or at-rule body in the
  `@slot` or the shorthand form, and `@variant NAME { … }` uses it, or a
  built-in one, in author CSS. A declared `dark` wins over the built-in and
  keeps its sort slot, one built on a named `@container` query keeps the name,
  `not-` negates the project's declaration (a body of two branches is refused,
  as Tailwind refuses it), a class the variant routes is generated once by
  the utility that owns it, and the variant sits where the candidate wrote it
  among the built-in ones, as Tailwind applies them left to right:
  `hover:dark:text-white` is `.x:hover:where(.dark, .dark *)` and
  `dark:hover:text-white` `.x:where(.dark, .dark *):hover` (#564, #672, #673,
  #836, #839, #847).
- `@source "<path>"` scans a path relative to the stylesheet: a directory is
  walked, a glob matches under its root, and `@source not` takes files back
  out. `@source inline("…")` safelists its classes with Tailwind's brace
  patterns (`{hover:,}bg-red-{500,600}`, `p-{0..8..4}`) and `@source not
  inline("…")` blocks them, markup included (#787, #806).
- `@plugin "@tailwindcss/forms"` writes the plugin's base reset of native form
  controls, in `@layer base` even when the entrypoint imports Tailwind without
  preflight, unless its options ask for `strategy: "class"` (#804, #817).
- `theme()`, `--theme()`, `--spacing()` and `--alpha()` expand in author CSS.
  `theme(--color-red-500)` and the v3 `theme(colors.red.500)` give the token's
  value; `--theme(--x)` a `var()` the theme layer declares, with its fallback,
  or the value itself under `inline` and in an at-rule prelude; `--spacing(4)`
  the spacing arithmetic; and `--alpha(red / 50%)` the `color-mix()` it
  spells, with the legacy fallback and `@supports` arm beside it (#780, #783,
  #792).
- An entrypoint carrying a v3 `@config` is refused: `tw` exits with an error
  naming the file and pointing at `@theme` (#800).

### Command line

- `tw` takes the Tailwind CLI's flags, so `tw -i src/app.css -o dist/app.css
  --watch` runs where `tailwindcss` did: `-i`, `-o`, `-m`, `-w`, `--poll`,
  `--cwd` and `--silent`. With no path given, sources are detected from the
  working directory as the import's `source()` says, and a `.css` path to scan
  is refused rather than read as markup (#837).
- Class scanning covers Markdown, MDX, JavaScript and TypeScript, and finds
  where a candidate ends with cascade's tokeniser rather than by guessing: a
  bracket before whitespace is not a candidate, a candidate stops at the end of
  its line, a `/modifier` opening on `-` or `_` is none, and a recursive scan
  stays inside the source tree (#137, #145, #208, #288, #318, #321, #564,
  #772).
- Compiling a project is fast: tailwindcss.com's class list takes 0.3 s of
  CPU, where `tailwindcss` takes 0.6 s (#843, #845, #846).
- `tw --diff --html PAGE` also renders both stylesheets over `PAGE` in a
  headless Chromium, compares the pixels, and says when the render and the
  structural diff disagree: an entry listed over a page that paints alike is
  named an over-report of the differ, and a render that differs where nothing
  was listed an under-report, each a cascade bug to file (#841, #873).
- `tw --diff` exits 1 when the two sheets differ and 2 when it cannot read one
  of them, so a CI job can gate on it. It printed the differences and exited 0
  (#810).
- `tw --tailwind` and `tw --diff` find the pinned CLI from a directory with no
  `node_modules` above it, no longer compile the working directory before
  they start, and say when a CLI of the right version cannot compile from the
  working directory. The reference they build no longer reports a difference
  for a class Tailwind's own extractor declines to read back, or for a
  `theme(--x)` read the CLI binds only for a candidate found in a file (#702,
  #705, #712, #727, #821).
- `tw --inline` resolves the spacing token and the arithmetic it leaves: `p-4`
  prints `padding: 1rem` and no theme block. It kept `--spacing` declared and
  wrote `calc(var(--spacing) * 4)` (#820).
- Conflicting backend flags are refused rather than one being picked silently
  (#317).

### Utilities

- A fraction resolves exactly, as Tailwind's `calc(1/3 * 100%)` does: `w-1/3`
  rendered 106.984px against 107px in a 321px container. The sizing, inset,
  flex and basis families share it (#828).
- Sizing accepts the whole scale. The container scale reaches the logical
  families and `basis-*`, both viewport axes and the `px` step work on the
  width and height families, and a fraction takes any denominator, including
  zero and improper ones. `w-3/8`, `min-w-7/12`, `max-h-dvw`, `size-px`,
  `basis-7xl` and `@container-size` all resolve (#146, #151, #152, #154, #155,
  #156, #159, #180, #207, #216, #564).
- Position and translate read a spacing step in either sign and a fraction of
  any shape, and a negated arbitrary inset accepts a parenthesised calc body,
  so `-left-6/5`, `-top-2.5`, `-left-[(var(--a)+var(--b))]` and `translate-2`
  work alongside the numeric steps, and `-translate-y-0.5` sorts in the
  negative band of its axis with the integers. A negated bracket translate is
  negated as Tailwind writes it on every axis, `-translate-z-[4px]` giving
  `calc(4px * -1)` and `-translate-x-[calc(1rem+2px)]` the calc negated, where
  the z axis read the bracket as a variable name and any bracket that was not
  a plain length became a `var()` of its own text. A negated bracket scale,
  skew and rotate go the same way on every axis, `-scale-[1.5]` giving
  `calc(1.5 * -1)` and `-skew-x-[10deg]` `skewX(calc(10deg * -1))`; the
  first two were unknown classes, and `-rotate-[.5turn]` wrote its angle
  un-negated (#160, #166, #172, #186, #210, #646, #831, #861, #865).
- Every inset side carries the whole scale, under either sign, `start-*`,
  `end-*` and the logical `inset-s-*`, `inset-e-*`, `inset-bs-*` and
  `inset-be-*` included: a spacing step, `px`, a fraction, an arbitrary length,
  and a name the theme binds through `--inset-<name>` or `--spacing-<name>`.
  `start-px`, `start-1/2`, `-top-[4px]`, `-inset-bs-[4px]`, `inset-s-0.5`,
  `inset-bs-1/2` and `-bottom-3/4` all reach the sheet (#677, #691, #708).
- Transforms, backgrounds, grids and typography take the keywords Tailwind
  documents: `translate-none`, `rotate-none`, `scale-none`, `perspective-near`,
  `duration-initial`, `ease-initial`, `via-none`, `grow-3`, `indent-px` and a
  negative `-indent-4`. A zero translate keeps its unit, `aspect-[1.333]` takes
  a bare number, `grid-cols-[min(50%,20rem)]` takes a math function,
  `bg-position-[center_2rem]` keeps both axes, and `content-[attr(before)]`
  takes an unquoted function (#134, #157, #164, #171, #174, #175, #178, #181,
  #183, #184, #218, #223).
- Borders and masks cover their logical and arbitrary forms: axis and
  single-side widths and colours (`border-x-16`, `border-bs-red-500`), any
  integer width or outline offset, a mask colour stop, a bracket mask image, a
  zero mask stop that keeps its unit, and a bracket mask stop written as
  Tailwind decodes it, `mask-linear-from-[calc(1px+2px)]` giving
  `calc(1px + 2px)` where the undecoded text was a value browsers drop. A
  mask stop reads as Tailwind classifies it: a bracket colour, an `--alpha()`
  or a `color:` hint name the stop's colour and a length, a `--x` or a
  `--spacing(4)` its position, a bare step counts in quarters, a percentage
  is a whole number, and a colour stop takes an opacity modifier,
  `mask-linear-from-red-500/50` mixing as every colour family does (#148,
  #161, #162, #163, #165, #182, #222, #265, #860, #866).
- `transition-behavior-normal` and `transition-behavior-allow-discrete` are
  refused, as Tailwind compiles nothing for them. They compiled to a rule for
  `transition-normal` and `transition-discrete`, which no markup written the
  long way matches (#797).
- A v3 opacity utility says so. `bg-opacity-50` and the `text-`, `border-`,
  `divide-`, `ring-` and `placeholder-` spellings are still refused, as
  Tailwind refuses them, but the message names the v4 replacement
  (`bg-<color>/50`) instead of reporting an unfamiliar name, and the CLI's
  `Warning:` line carries what the parser said rather than always `Unknown
  class` (#777).

### Arbitrary values

- The `(--name)` shorthand reaches every place a bracket does: `text-sm/(--lh)`
  sets the line height beside the size, and a colour's `(--c)` takes an opacity
  modifier, `bg-(--c)/50`, `text-(--c)/50` and `bg-(--c)/(--o)` compiling to
  Tailwind's `color-mix()` under `@supports`. All were unknown classes (#811,
  #812).
- A data-type hint comes off the front of any bracket, whatever the family does
  with what follows, and the value after it is what reaches the sheet, the hint
  kept in the class name. `z-[integer:5]` crashed the run,
  `text-[length:1.25rem]` wrote `font-size: var(--1\.25rem)` and `col-[foo:2]`
  wrote `grid-column: foo`; all now write the value the author meant, as do
  `divide-[color:red]`, `shadow-[length:3px]`, `rotate-[angle:45deg]`,
  `aspect-[ratio:16/9]`, `border-[line-width:2px]`, `bg-[percentage:50%]`,
  `transform-[foo:scaleX(2)]` and every other family. A hint's name is a run
  of `a`-`z` and `-`, so `mask-[FOO:2em]` holds its bracket whole, and a
  bracket whose hint is empty or which holds nothing but blank space names no
  utility, as in Tailwind (#706, #714, #718, #720, #729, #730, #731, #732,
  #733).
- A bracket no reader takes goes to the longhand the class names, as Tailwind
  writes it, rather than being refused, which dropped the rule and left an
  element carrying the class with nothing at all. `text-[length:red]`,
  `text-[notacolour]`, `stroke-[1zz]`, `bg-[image:nope]`, `p-[foo]`,
  `top-[foo]`, `w-[foo]`, `rounded-[foo]`, `mask-[foo]` and a malformed hex in
  any colour family reach the sheet as declarations the browser discards; a
  bare number after a width hint is pixels, the spelling Tailwind's minifier
  ships; and `bg-size-[foo]` no longer writes `background-size: auto`, a value
  the class never asked for. Every one of the 150 families the token-stream
  contract inventories does this, against 38 before (#761 through #771).
- An arbitrary value reads through the same CSS grammar as the rest of the
  sheet, so a length, angle, colour, shadow, ease, blur, tracking, line-height,
  stroke width, border-spacing or gradient stop takes every unit and math
  function CSS allows, and a compact `calc()`, a `var()` with its fallback, a
  `theme()` in dot notation, a `--spacing()` call, an `--alpha()` call, a grid
  track and a list style mean the same thing wherever they appear: `ml-[50%]`,
  `left-[calc(5%-2px)]`, `py-[calc(--spacing(2)+1px)]`, `list-[upper-roman]`,
  `gap-[calc(1px_+_1px)]`, `mx-[--spacing(4)]`, `flex-[calc(1+2)]`,
  `z-[calc(1+2)]`, `delay-[calc(1s+2s)]`, `text-[--alpha(red/0.2)]` and
  `origin-[--spacing(4)_--spacing(2)]` all resolve, a bracket colour is read
  as CSS before the palette is consulted, and a value the target property
  cannot take is still written through as Tailwind writes it. An `--alpha()`
  expands anywhere in a value, as `--spacing()` does, its alpha a number,
  a percentage or a `var()`: `shadow-[0_0_0_1px_--alpha(red/50%)]`,
  `bg-[linear-gradient(--alpha(red/0.5),blue)]` and
  `text-[--alpha(red/var(--o))]` resolve, a bare number scales as Tailwind
  scales it, `0.2` being 20%, and a call missing either half names no
  utility. A named opacity on a shadow's size, `shadow-lg/half`, sets no
  alpha, since Tailwind reads none there (#168, #176, #177, #187, #188, #189,
  #190, #191, #192, #205, #212, #217, #236, #241, #262, #277, #278, #325,
  #371, #372, #373, #375, #376, #377, #378, #404, #417, #418, #420, #465,
  #503, #504, #509, #522, #667, #683, #688, #689, #690, #863, #869).
- A bracket only OCaml's number reader accepts is no longer folded to a
  different value. `tab-[0x4]` wrote `tab-size: 4`, `flex-[0x4]` wrote `flex: 4`
  under the class name `.flex-\[4\]`, `grid-cols-[0x4]` wrote `4px`,
  `aspect-[0x4]` was refused, and `stroke-0x4` emitted a `.stroke-4` nobody
  wrote, `/0x50` rode the opacity modifier onto every colour utility and
  `min-[0x600px]` manufactured a working 1536px breakpoint. A class suffix is
  plain decimal, `aspect-[…]` emits its bracket verbatim as Tailwind does, and
  one fraction reader serves the sizing, position, flex and translate
  families, so `top-1/7` and `basis-0/2` read like `w-1/7` (#684, #689, #690,
  #696).
- `object-[...]` takes the whole CSS `<position>` grammar, so `object-[top]`,
  `object-[left_top]` and `object-[right_2rem]` reach the sheet where
  `object-[top]` wrote `object-position: var(--top)` (#734).
- A negated arbitrary length works on every family and in every unit:
  `-mt-[2em]` was an unknown class, as was every unit outside a handful (#735).
- `theme(--x)` and `--theme(--x)`, v4's own spelling of a theme lookup, resolve
  in an arbitrary value, every namespace of the default theme answers, and a
  `theme()` naming a key the theme does not carry makes the class no utility,
  the way Tailwind emits no rule for it, a fallback argument standing in for
  the missing key. `p-[theme(--spacing)]` was an unknown class,
  `rounded-[theme(--radius-lg)]` wrote `0px`, and
  `shadow-[0_0_0_1px_theme(a_b)]` wrote the call through into the declaration.
  A `theme()` alpha survives a hex-bound palette entry, and a resolved value
  keeps its underscores, so a palette entry bound to `var(--brand_red)` no
  longer names `var(--brand red)` (#508, #687, #688, #701, #710).
- An underscore that spells a name stays one. The argument of a `url()`, the
  first argument of a `var()` or a `theme()`, and a `\_` escape all keep it,
  so `list-image-[url(a_b.png)]` and `content-[url(a_b.png)]` name their
  file, `[--x:var(--my_var)]` no longer references `--my var`,
  `shadow-[0_0_0_var(--my_var)]` is no longer truncated to `var(--my)`, and
  `font-['My\_Font']` and `data-[foo=bar\_baz]:flex` reach the sheet as
  written; the underscore outside them still becomes a space (#676, #688,
  #692, #695).
- A closing bracket the value quotes or escapes belongs to the value, and a
  `url()` is read whole by the CSS tokeniser: `bg-[url(a\]b)]` names the file
  the class means where it emitted `url("a\\]b")`, `bg-[url('a]b')]`,
  `font-['My]Font']`, `[content:'a]b']` and `after:content-['a]b']` reach the
  sheet, `mask-[url(x.png)_center]` is refused rather than sliced into a
  selector no markup carries, and a string the value leaves open still refuses
  the class, as it does in Tailwind (#689, #692, #695).
- An arbitrary value is spelled in the class name the way the author wrote it,
  so the selector matches the markup. `min-[0.5ch]:flex` emitted
  `.min-\[\.5ch\]\:flex`, a rule nothing on the page could match, and
  `basis-[...]`, `perspective-[...]`, `underline-offset-[...]` and an `nth-*`
  argument keep their spelling too (#412, #413, #415, #489, #490, #532, #543,
  #564).
- A class that cannot mean anything is refused at parse time, with the reason,
  instead of compiling to a dead selector or a declaration with no value. An
  invalid palette shade, a value the property cannot take, a bracket value that
  escapes its declaration, trailing text after the value, a colour channel with
  no byte, a non-canonical number, an unbalanced paren (`-left-[0)/*1]`
  became `left: calc(0 * -1)`), a spacing step that is not a multiple of 0.25
  on the inset and sizing families (`w-1.7`), and an unreadable animation,
  order, z-index, grid line, `not-has` shorthand, data expression, gradient
  interpolation or `not-[...]` content are all rejected, and a unitless
  decoration colour declares nothing rather than guessing (#127, #234, #237,
  #282, #284, #285, #307, #309, #405, #406, #407, #408, #410, #421, #422,
  #496, #532, #548, #691).
- A bad arbitrary value no longer takes the run down. Six bracket spellings
  raised past the result-typed `of_string`, nine `to_style` sites answered an
  empty style instead of an error, and a candidate that raised while rendering
  aborted the run, so one class could crash the renderer or silently emit
  nothing (#144, #257, #266, #287).
- Bracketed `has`, `group-has` and `peer-has` variants keep Tailwind's
  `:is(...)` wrapper for bare type and complex selectors, and a `var()`
  reference is read to its end inside a bracket value, so one carrying its own
  parentheses or a fallback is not truncated (#564, #658).

### Colours and effects

- An opacity modifier reaches every colour family. A ring, a ring offset, a
  per-side border, a shadow, a drop shadow, a decoration and a stroke all take
  one, the alpha can itself be a variable or a named `--opacity-*` token in
  every one of them, `currentcolor` and a bracket `var()` included
  (`bg-cyan-400/(--my-alpha-value)`, `shadow-red-500/half`,
  `decoration-current/(--o)`, `ring-[var(--c)]/half`,
  `scrollbar-thumb-current/(--o)`), and `transparent` and `inherit` take one
  everywhere. Shadeless names such as `shadow-white`, `stroke-white`,
  `ring-offset-white` and `ring-offset-black` work, a colour the project's
  `@theme` declares names a ring, a ring offset and an inset ring as it names
  a shadow, a `--text-shadow-color-*` token names a text shadow's,
  `light-dark()` and an arbitrary shadow colour resolve, and a drop shadow
  keeps both of its default layers under an opacity (#169, #185, #201, #202,
  #209, #214, #225, #231, #244, #254, #281, #308, #322, #323, #813, #847,
  #859, #868).
- An opacity modifier over a bracket colour paints the colour the class named,
  as a `color-mix()`, across all thirteen colour families.
  `text-[rebeccapurple]/50` and its siblings were unknown classes or rendered
  black; `bg-[red]/50` read the palette red-500 instead of CSS red; and
  elsewhere the mix resolved to that colour's `oklab()` channels rather than
  staying a mix, going out with no unguarded fallback where the alpha read a
  custom property, so a browser without `color-mix()` painted nothing.
  `decoration-`, `divide-` and `stroke-` accept the modifier at all now, and a
  colour the browser resolves at use time keeps the `@supports` fallback
  Tailwind writes (#508, #517, #711).
- An arbitrary colour reaches CSS in the spelling the class wrote. `bg-[#f00]`
  gave `#ff0000`, `bg-[#ffffffff]` gave `#ffffff` and `bg-[#FF0000]` lost its
  case, where Tailwind writes back what the bracket held (#700).
- An arbitrary shadow keeps the colour the class named, and whatever the value
  reader accepted with it: a colour keyword, `currentcolor`, a layer list, a
  leading `inset`. A bracket naming a colour CSS knows came out as
  `inset-shadow-none` under `inset-shadow-`, and as `shadow-none` under an
  opacity modifier, so `inset-shadow-[0_0_0_1px_red]`, `shadow-[0_0_red]/50`
  and `inset-shadow-[0_0_0_1px_red]/50` reach the sheet now. Where the alpha
  reads a custom property, `shadow-` and `text-shadow-` keep the authored
  colour as the unguarded fallback instead of folding it through oklab at full
  opacity, which painted an opaque shadow in a browser with no relative
  colours (#711, #716, #717). Under a modifier the alpha replaces the
  colour's own, as Tailwind's `oklab(from … / 50%)` does, where
  `shadow-[0_1px_2px_#0000001a]/50` kept the colour's 10%, and an arbitrary
  drop shadow takes the modifier too, `drop-shadow-[0_1px_2px_red]/50` was
  an unknown class. `inset-shadow-[inset_0_1px_red]`, a bracket spelling the
  `inset` the utility supplies itself, is refused: Tailwind writes `inset
  inset 0 1px red`, which no browser draws, where tw drew the shadow the
  author did not get (#847).
- A shadow list under a modifier is written the way Tailwind writes it, in
  every family: a `var()` colour or a `var()` alpha keeps every layer's
  authored colour in the open and takes the alpha behind the relative colour
  guard, a `currentcolor` layer through `color-mix()` behind its own guard
  nested inside, and a colour no hex spells takes the alpha in place of its
  own, where a `color-mix()` multiplied the two. A trailing `var()` is the
  colour in a project token as in a bracket, so `--shadow-card: 0 1px 2px
  var(--c)` paints with `--c` rather than reading it as a spread. A project
  `--text-shadow-<name>` is a utility, `--drop-shadow-<name>` and
  `shadow-inner` take a modifier, a sized drop shadow reads its project
  override, and `shadow-lg/[25]` records the author's own number (#852).
- A shadow read whole from a custom property takes a modifier in every
  family, `shadow-[var(--s)]/50`, `inset-shadow-[var(--s)]/50`,
  `drop-shadow-[var(--s)]/50` and `text-shadow-[var(--s)]/50`, under the
  `shadow:` hint too: the alpha channel is set and the value kept, as Tailwind
  writes it, where the class was refused or the modifier silently dropped.
  `ring-[3px]` declares the inset toggle and colour it reads, as `ring-2`
  does, where the sheet dropped both reads; a width takes no opacity, and
  `ring-[3px]/50` is refused (#853).
- Palette box, inset-box and text shadows keep Tailwind's authored OKLCH value
  as their unguarded fallback instead of converting it to sRGB hex, and a
  gradient stop with an opacity keeps its theme colour as the palette declares
  it: `to-gray-950/40` registered `--color-gray-950` as `#030712`, which moved
  every other use of the token on the page off the palette's oklch (#657,
  #822).
- `--color-black` and `--color-white` are written `#000` and `#fff`, the three
  digits Tailwind spells them in, in the theme block and in every colour
  family's unguarded fallback (#711).

### Variants

- Variants compose in the combinations Tailwind allows. `group-*` and `peer-*`
  take any state and a name (`peer-checked/draft`), `has-*`, `group-has-*` and
  `peer-has-*` take any variant, a bare data attribute or a bracket selector
  (`has-[a]`, `has-peer-checked`, `group-has-data-[state=open]`), `not-*`
  composes over any variant, `in-*` scopes to an ancestor in a given state
  (`in-focus`), a container query nests with the variants around it and reads
  a theme token (`@min-[theme(--breakpoint-lg)]`), a bracket `@media` variant
  wraps the utility in that query (`[@media_print]:underline`), and an
  arbitrary variant works with no `&` anchor (`[code]:pr-4`). A hover gate
  nests inside a wrapping media or container query, and an inner selector
  survives an at-rule variant such as `[@supports(display:grid)]:grid`. A
  variant over a media inner reaches the rule the inner query nests under
  its own gate, `in-focus:md:hover:flex` putting `:where(:focus)` in front
  of the class inside the hover media and `hover:md:hover:flex` its second
  `:hover`, where the nested rule kept the bare class; an ancestor variant
  puts the rest of a compound onto the class, `:where(:focus) .x:hover` for
  `in-focus:hover:flex`; and `group-not-[.a]:` negates the bracket rather
  than losing the negation. `before:` and `after:` over an inner variant nest
  as Tailwind nests them, `content` declared once on `.x::before` under no
  condition and the utility's declarations on `.x::before<inner>` under the
  inner's own at-rules, where the two were fused under the inner's condition
  with its selector dropped; `marker:` and `selection:` put the inner's
  selector after each pseudo-element; and `@starting-style` nests with a
  container or supports query in either order (#167, #173, #196, #197, #198,
  #199, #200, #203, #204, #211, #213, #215, #219, #224, #231, #232, #233,
  #235, #238, #280, #314, #700, #814, #815, #850, #851, #855).
- `not-` negates what it wraps. `not-[:target]`, `not-[:nth-child(2)]` and
  `not-[:has(.x)]` negate the pseudo-class, where only nine were tabled and the
  rest negated a class literally named `:target`; `not-[@supports(…)]` negates
  the condition, `not-[@supports(display:grid)]:flex` wrapping the utility in
  `@supports not (display:grid)`, where it negated the utility's own class;
  and `has-` and `not-` around an arbitrary `data-[…]` or `aria-[…]` variant
  test the attribute, where `has-data-[state=open]:ring-2` read
  `:has(.ring-2)`. `not-@md:` negates a container query as Tailwind 4.3.3
  writes it, `@container not (width >= 28rem)`, the container's name kept
  outside the negation, where the class was refused. A negated hover
  variant carries its `@media not (hover: hover)` twin whatever form the
  hover takes - `not-group-hover:`, `not-peer-hover/x:`, `not-in-hover:`,
  `not-has-hover:` - and the twin keeps the leaf the inner variants built,
  so `not-hover:focus:flex` styles `.x:focus` on a touch device;
  `not-group-hover:flex` emitted the selector half alone and
  `not-in-hover:` negated the utility's own class. `has-`, `in-` and a
  second `not-` take any variant with a selector, `has-not-focus:`,
  `in-group-hover:`, `in-nth-3:` and `not-not-md:` included. A compound
  condition, a negation with two halves or of a pure at-rule under `has-`,
  `in-` or `not-`, and a media or container inner inside a group or peer
  negation are refused, as Tailwind refuses them (#488, #493, #775, #803,
  #818, #850, #851).
- `supports-[…]` emits the test the author wrote, as a typed condition rather
  than a string reparsed after assembly: a bare property expands to a feature
  test, `not(display:grid)` becomes `@supports not (display:grid)` and
  `selector(:has(a))` passes through, where the first crashed with an uncaught
  `Failure` (exit 125), and a malformed condition is refused (#135, #389,
  #484, #808).
- A `!` on a utility that writes rules of its own reaches their selectors, and
  marks the variables the utility sets. `space-x-4!`, `container!`, `prose!`
  and `form-input!` styled the class without the `!`, which no element
  carries, so nothing applied; `shadow-md!` left `--tw-shadow` normal, so a
  plain `shadow-lg` on the same element still chose the shadow drawn (#802,
  #807).
- A variant no longer costs the rule inside it, or writes an empty one beside
  it. A hover gate survives an at-rule variant, a peer hover gate survives a
  selector variant, a variant stays wrapped around a `@starting-style` rule, a
  class a variant renames keeps the default transition theme, and
  `sm:dark:hover:underline` names its own class in the dark media block, where
  it emitted a declarationless `.sm\:dark\:hover\:underline:hover {}` beside
  a rule named `.dark\:hover\:underline` (#564, #703).
- An opacity colour keeps its progressive-enhancement `@supports` guard when
  wrapped in a supports, container or starting-style variant. The modern
  `color-mix()` declaration was left unguarded inside that wrapper (#666).

### Ordering and structure

- Utilities land where Tailwind puts them across the sheet. A colour's
  `@supports` rule stays with its fallback, container variants order by width,
  the logical sizing families sort last, line-clamp sorts with box-sizing, and
  isolation, float, clear, divide, masks, outline colours, the drop-shadow
  sizes, inset start and end, the basis fractions, border-spacing and the theme
  namespaces each follow Tailwind's order. An unvarianted utility comes before
  the `not-*` group, and a variable whose slot was already taken is no longer
  dropped from the sheet (#242, #243, #249, #250, #251, #253, #263, #264,
  #267, #268, #269, #291, #292, #310, #311, #312, #670).
- More property families emit in Tailwind's band: fill and stroke ahead of
  object-fit, aspect ratio before the dimensions, tab size inside typography,
  field sizing after display, logical block margins before the physical sides,
  ring widths in numeric order, `text-indent` beside the other text
  properties, `text-wrap`, `overflow-wrap`, `word-break` and `hyphens` before
  white-space, the late typography colour block and the priority-7 theme tail
  in Tailwind's order, `divide-x-reverse`, container queries and column values
  where Tailwind puts them, and the transform, filter, gradient, gap, delay and
  list-style families throughout (#429, #443, #494, #523, #541, #552, #564).
- Stacked and compound variants sort by what they contain rather than by their
  prefix text, from one table. A compound carries its inner value, a recursive
  compound follows its whole path, an arbitrary variant orders by its selector,
  data variants group by predicate, a negated breakpoint retains its responsive
  order, repeated element variants collapse to one key, and a `peer-` variant
  sorts inside the peer group by the variant it wraps, as `group-` does:
  Tailwind writes `peer-checked`, then `peer-hover`, then `peer-focus`, where
  `peer-hover` sorted in front of every other `peer-` spelling. An unrecognised
  prefix used to return 0, which put the rule in a different sort bucket
  entirely rather than merely out of order (#520, #564, #672, #673, #773).
- An opacity colour under `hover:` keeps its `@supports` twin beside its
  fallback, as Tailwind writes the pair. The twin sorted after every later
  utility of the group, so `hover:bg-white/50` came after `hover:text-white`
  (#829).
- `md:container` keeps its breakpoint rules beside it, as Tailwind writes them,
  so `md:max-w-2xl` follows them and wins on an element carrying both. The
  nested breakpoints sorted as a stacked variant, after every plain rule of the
  `md` block (#833).
- Container variants sort as Tailwind groups them: every `@max-*` before every
  `@*` and `@min-*`, whatever value either names, and `@lg:` with `@min-lg:`
  in one container block. `@lg:flex @max-lg:hidden` wrote the `@lg` block
  first, `@max-[theme(...)]` came after `@lg`, and a stacked `@sm:@max-md:`
  sorted past `@md` instead of under `@sm` (#783, #827, #830).
- Blocks group the way Tailwind groups them. Adjacent `@media`, `@container`
  and `@supports` blocks with one condition are a single wrapper, at every
  level of nesting, so `sm:p-4 sm:m-2` is one breakpoint block and
  `sm:dark:p-4 sm:dark:m-2` one breakpoint block holding one dark block; a run
  of `@starting-style` utilities emits as one block; a pseudo-element declares
  its content once, `before:bg-red-500/50` no longer repeating it in the
  `@supports` colour twin; and a project's own utilities interleave with the
  built-in family writing the same property (#194, #220, #226, #228, #255,
  #256, #271, #313, #316, #319, #324, #668, #682, #726, #823).

### OCaml API

- Typed construction covers clear, background attachment, clip, origin,
  position, repeat and size, outline width and colour, ring offsets, and
  background blend modes (#649).
- The typed `divide` constructors, from `divide_x` to `divide_style`: only the
  two reverse utilities were exposed, so the rest of the family was reachable
  from a class string but not from OCaml. `divide_x_length` accepts a
  line-width keyword, which the parser already did and the constructor raised
  on (#239, #522, closes #5).
- The typed padding and gap constructors raise `Invalid_argument` on a
  negative size. Neither has a negative form, and `p (-3)` printed `p-3`, a
  different utility, with nothing to say so (#809).
- `Var.needs_property_rule` answers `false` for a variable carrying metadata tw
  did not create, where it failed an assertion (#707).
- `Modifiers.not_variant_order` and `Modifiers.prose_element_inner_selector`
  are gone: nothing read the first, and the second was the module's own
  helper. A caller of either uses `Modifiers.variant_order_of_prefix` and
  `Modifiers.to_selector` (#871).

### Packaging

- Require cascade 1.2.0. While it remains unreleased, CI pins cascade's main
  branch so builds and tests follow upstream rather than an exact development
  revision (#297, #302, #305, #646).
- Follow cascade's grammar tightening. `-webkit-mask-clip` and
  `-webkit-mask-origin` carry only the three boxes WebKit's own grammar has, so
  `mask-clip-fill`, `mask-clip-stroke`, `mask-clip-view`, `mask-no-clip` and
  the three `mask-origin-*` siblings emit the unprefixed declaration alone,
  which is what every browser kept of the twin Tailwind writes. `outline-[50%]`
  emits its `outline-style` alone, the declaration a browser keeps once it
  drops the `outline-width: 50%` Tailwind writes beside it; `border-[50%]`,
  whose Tailwind form is `border-color: 50%`, is refused rather than emitting
  a live `border-style` behind a dropped width. `list-[<name>]` reads any
  `<counter-style-name>`, and a `[color:theme(...)]` whose alpha names no
  number is refused rather than resolved without it (#721).

## 1.0.0

- Initial public release candidate. Type-safe Tailwind CSS v4 in OCaml,
  with parity against the upstream v4 compiler (core utilities plus the
  official `forms` and `typography` plugins).
