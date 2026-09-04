## 1.1.0 (unreleased)

### Breaking changes

- Variable ordering and `@property` metadata now travels with `Var` values and
  `Style.t`, rather than through process-global registries. The registry lookup
  functions (`Var.order`, `Var.family`, `Var.property_order`,
  `Var.needs_property`, `Var.register_property_order`, and
  `Var.resolve_theme_refs`) are removed, and the public `Style.Style` record
  gains a `metadata` field. Construction, parsing, and rendering are safe to run
  from multiple OCaml Domains without a mutex; typed `property_default` rules
  are generated automatically (#653).
- `bg_transparent`, `bg_current` and the background colour constructors move to
  `Backgrounds`; `border_color`, `border_transparent` and `border_current` move
  to `Color`. Building one from OCaml and parsing the same class name used to
  reach different handlers with different sort slots (#518).
- Every spacing utility takes an `int`, and a primed variant takes a float, so
  the common call needs no conversion. A float call site becomes `p' 2.5`
  (#492).
- An unrecognised class no longer raises. `Tw.str` raised, `Tw_html` copied the
  name in silently, and `Tw_dom.use_str` crashed the browser render. Unknown
  names pass through everywhere now, never raise from a rendering path, and are
  reportable instead: `Tw.of_classes` returns them alongside the utilities, and
  `Tw_html.unknown_classes` and `Tw_dom.unknown_classes` expose the same list.
  Code that caught the exception to find a typo reads that list. `Tw.of_string`
  is unchanged, so the CLI still reports a deliberately typed class as an error
  (#514).
- `-safe` alignment resolves on `center` and `end` only, as in Tailwind. The
  `-start-safe` classes stop being emitted, and `Alignment.content_start_safe`
  and `Alignment.place_items_start_safe` are removed (#679).

### Tailwind CSS 4.3.3

- Track Tailwind CSS 4.3.3. `font-sans` carries the 4.3.2 system stack,
  preflight scopes `:-moz-focusring` to non-iframe elements, and an achromatic
  colour writes its powerless hue as `none` (#128, #129, #130, #132, #147).
- Add the mauve, mist, olive and taupe palettes. Their theme tokens are declared
  between `stone` and `black`, and their utilities sort among the rest, where
  Tailwind puts them; all four shared one unranked slot after `white` (#153,
  #696).

### Project stylesheets

- `tw` compiles a whole CSS entrypoint instead of reading only its `@theme`.
  `@import`, `@apply`, `@utility`, `@variant`, `@custom-variant`, `--spacing()`
  and `theme()` all expand in author CSS, down to a declared utility's own
  `@apply` and `@variant` (#136, #138, #139, #140, #141, #143, #195, #206).
- Authored input receives browser-compatibility prefixes even when full CSS
  optimization is disabled, preserving the CLI's target coverage (#665).
- Merge adjacent media queries with identical conditions during utility
  construction, avoiding redundant wrappers without enabling full stylesheet
  optimization (#668).
- Merge adjacent `@container` and `@supports` blocks with identical preludes
  the same way, so a run of utilities sharing one condition is a single wrapper
  and the sheet is smaller (#682).
- A project's own theme reaches class generation, so its variants, keyframes,
  layers, static scales, custom breakpoints and v3 dotted `theme()` paths apply
  to the utilities tw generates from the markup, and a routed utility survives a
  selector that is not a bare class (#170, #179, #193, #221, #227, #229, #230,
  #315, #320).
- Class scanning covers Markdown, MDX, JavaScript and TypeScript, and finds
  where a candidate ends with cascade's tokeniser rather than by guessing. A
  bracket before whitespace is not a candidate, a candidate stops at the end of
  its line, and a recursive scan stays inside the source tree (#137, #145,
  #208, #288, #318, #321, #564).
- Prose utilities are emitted only when the typography plugin is enabled, and a
  candidate that raises while rendering an arbitrary value is dropped rather
  than aborting the run (#142, #144).
- A functional `@utility name-*` resolves `--value()` and `--modifier()`, and
  `@apply` of one produces its declarations instead of nothing. A token a
  utility only reads through `var()` is now declared in the theme layer whatever
  its family, `--spacing(N)` is multiplied out where the project inlines
  `--spacing`, and an `@theme reference` block is represented (#554).
- Functional utilities follow Tailwind's declaration-count ordering, parity
  comparisons keep author custom properties, and a folded zero-spacing utility
  omits the internal `--spacing` carrier it no longer reads (#650, #651, #655).
- Every `@utility` declared for one name applies, not only the first (#550).
- A routed candidate keeps the utility that owns it, so a class a declared
  variant routes is generated once by the right handler (#564).
- Reject conflicting CLI backends instead of silently selecting one (#317).
- Keep Tailwind's own at-rules out of the generated CSS: `@theme`, `@source`,
  `@plugin`, `@config`, `@reference`, and `@tailwind` used to reach the
  browser, which has no meaning for any of them (#361).

### Utility coverage

- Typed construction covers clear, background attachment/clip/origin/position,
  repeat and size, outline width/colour, ring offsets, and background blend
  modes (#649).
- Sizing accepts the whole scale. The container scale reaches the logical
  families and `basis-*`, both viewport axes and the `px` step work on the width
  and height families, and a fraction takes any denominator, including zero and
  improper ones. `w-3/8`, `min-w-7/12`, `max-h-dvw`, `size-px`, `basis-7xl` and
  `@container-size` all resolve (#146, #151, #152, #154, #155, #156, #159, #180,
  #207, #216, #564).
- Position and translate read a spacing step in either sign and a fraction of
  any shape, and a negated arbitrary inset accepts a parenthesised calc body,
  so `-left-6/5`, `-top-2.5`, `-left-[(var(--a)+var(--b))]` and `translate-2`
  work alongside the numeric steps (#160, #166, #172, #186, #210, #646).
- `start-*` and `end-*` carry the same inset scale as the physical sides and
  keep the `calc(var(--spacing) * n)` Tailwind writes for every step, so
  `start-px`, `start-0.5` and `start-1/2` resolve and `start-0` no longer drops
  `--spacing` from the theme layer (#677).
- Every inset side reads an arbitrary length under either sign, and `start-*`
  and `end-*` read one at all. `start-[4px]`, `end-[4px]`, `-top-[4px]` and
  `-inset-bs-[4px]` reach the sheet, as does a name the theme binds on the
  logical inline sides (#691).
- A named inset takes its value from the theme, reading `--inset-<name>` and
  falling back to `--spacing-<name>`; a theme declaring only `--spacing-lg` used
  to get `top: var(--inset-lg)` over a length tw made up (#708).
- Every inset side reads a named token under a minus, so `-top-header` and
  `-inset-bs-lg` reach the sheet (#708).
- `inset-s-*`, `inset-e-*`, `inset-bs-*` and `inset-be-*` carry the whole scale
  `start`/`end` do, and a fraction resolves on every inset side: `inset-s-0.5`,
  `inset-bs-1/2`, `inset-s-px`, `inset-y-1/2` and `-bottom-3/4` (#708).
- Transforms, backgrounds, grids and typography take the keywords Tailwind
  documents: `translate-none`, `rotate-none`, `scale-none`, `perspective-near`,
  `duration-initial`, `ease-initial`, `via-none`, `grow-3`, `indent-px` and a
  negative `-indent-4`. A zero translate keeps its unit, `aspect-[1.333]` takes
  a bare number, `grid-cols-[min(50%,20rem)]` takes a math function,
  `bg-position-[center_2rem]` keeps both axes, `content-[attr(before)]` takes an
  unquoted function, and a font family the project named in its `@theme`
  resolves (#134, #157, #164, #171, #174, #175, #178, #181, #183, #184, #218,
  #223).
- Borders and masks cover their logical and arbitrary forms: axis and
  single-side widths and colours (`border-x-16`, `border-bs-red-500`), any
  integer width or outline offset, a mask colour stop, a bracket mask image, and
  a zero mask stop that keeps its unit (#148, #161, #162, #163, #165, #182,
  #222, #265).
- A named spacing token declares the variable it references. Padding and gap
  emitted `var(--spacing-<name>)` with nothing declaring it (#261).
- Border-spacing candidates keep their property order across the whole theme
  scale, including arbitrary values (#670).
- Accept a value the project named in its own `@theme` wherever Tailwind does:
  shadows, blur radii, timing functions, font weights, line heights, letter
  spacing, corner radii, font sizes, perspectives, aspect ratios and max-widths
  (#447, #450, #457, #458, #459, #460, #461, #462, #463, #464).
- Honour a `@theme` that removes a token. A namespace reset (`--color-*:
  initial`) now reaches tw at all, `--spacing: initial` drops the multiplier,
  a removed breakpoint stops resolving its variant, and a removed palette
  entry no longer leaves a utility referencing a variable nothing declares
  (#507, #515).
- Keep `@keyframes` when a `@theme` redefines an animation. The keyframes
  follow the animation the value names, so retiming `--animate-ping` no longer
  emitted an animation that never ran (#510).

### Arbitrary values and validation

- A data-type hint comes off the front of any bracket, not only the ones a
  family reads: `z-[integer:5]` writes `z-index: 5` rather than
  `z-index: integer:5`, and `divide-[color:red]`, `shadow-[length:3px]`,
  `rotate-[angle:45deg]` and `aspect-[ratio:16/9]` write their values the same
  way (#PR).
- A bracket whose hint is empty, and one left holding nothing but blank space,
  name no utility, as in Tailwind. `z-[:5]` and `z-[_]` put a declaration with
  no value into the sheet (#PR).
- The hint's name is a run of `a`-`z` and `-`, so `mask-[FOO:2em]` and
  `mask-[a1:2em]` hold their bracket whole. The mask family read a wider name
  than Tailwind does and sliced the value away (#PR).
- The mask family writes an arbitrary bracket no reader takes into the longhand
  the class names, as Tailwind does, so `mask-[foo]`, `mask-[url(x.png)_center]`,
  `mask-position-[foo]`, `mask-size-[foo]` and every `mask-[<hint>:...]` whose
  value the hint declines reach the sheet (#PR).
- A data-type hint reads the value written after it instead of naming a custom
  property, so `text-[length:1.25rem]` sets `font-size: 1.25rem` rather than
  `font-size: var(--1\.25rem)`. Every hint tw recognises is affected (#706).
- `aspect-[...]` emits its bracket verbatim, as Tailwind does: nothing inside is
  validated, so `aspect-[foo]`, `aspect-[-1]`, `aspect-[calc(1+2)]` and
  `aspect-[1.23/4.56]` reach the sheet. `aspect-[0x4]` writes `0x4` rather than
  `4`, `aspect-[1_0]` writes `1 0` rather than `10`, and `aspect-[16/9]` keeps
  its spelling instead of being re-printed as `16 / 9` (#696).
- `z-[...]`, `opacity-[...]`, `col-span-[...]`, `row-span-[...]`,
  `grid-cols-[...]`, `grid-rows-[...]`, `auto-cols-[...]`, `auto-rows-[...]`,
  `columns-[...]`, `tab-[...]`, `scale-x-[...]` and `scale-y-[...]` read their
  bracket through the arbitrary-value pipeline, so `z-[calc(1+2)]` and its
  siblings reach the sheet. A bracket only OCaml's number reader accepts is no
  longer folded to a different value: `tab-[0x4]` writes `tab-size: 0x4` rather
  than `4`, and `grid-cols-[0x4]` writes `0x4` rather than `4px` (#690).
- `gap-`, `margin-` and the inset families read a bracket value through the
  whole arbitrary decoder rather than its last stage alone, so
  `gap-[calc(1px_+_1px)]`, `mx-[--spacing(4)]` and `top-[calc(1px_+_1px)]`
  reach the sheet (#688).
- `flex-`, `grow-`, `shrink-`, `order-`, `origin-`, `perspective-origin-` and
  `transform-` decode their bracket the way every other family does, so
  `flex-[calc(1+2)]` and `origin-[--spacing(4)_--spacing(2)]` reach the sheet.
  Reading the text with OCaml's number reader instead folded `flex-[0x4]` to
  `flex: 4` under the class name `.flex-\[4\]` (#689).
- A `url()` in an arbitrary value keeps the underscores of its argument, which
  name a file rather than spelling spaces, so `mask-`, `list-image-`, `content-`
  and an arbitrary property stop naming a file they did not mean; the underscore
  outside the `url()` still becomes a space (#688, #692).
- `theme(--x)` and `--theme(--x)`, v4's own spelling of a theme lookup, resolve
  in an arbitrary value. `p-[theme(--spacing)]` and its siblings were rejected
  as unknown classes; only the v3 dot paths resolved (#701).
- Every namespace Tailwind's default `@theme` declares answers a `theme()` and
  comes out under `theme(static)`. The radius, container, ease, tracking, blur,
  aspect, animate, perspective, font, font-weight, drop-shadow and
  default-transition scales kept their defaults to themselves, so
  `rounded-[theme(--radius-lg)]` and its siblings were unknown classes (#710).
- The first argument of a `var()` or a `theme()` in an arbitrary value keeps its
  underscores, which spell the name of a custom property rather than spaces.
  `[--x:var(--my_var)]` referenced `--my var`, and `shadow-[0_0_0_var(--my_var)]`
  truncated the reference to `var(--my)` without saying so; a later argument
  still decodes (#695).
- `bg-[url(a\]b)]` names the file the class means. The escaped bracket reached
  the value as a backslash of its own and emitted `url("a\\]b")`; the whole
  `url()` is now read by the CSS tokeniser, which resolves its quotes and
  escapes (#692).
- A closing bracket the arbitrary value quotes or escapes belongs to the value,
  so `bg-[url('a]b')]`, `font-['My]Font']`, `shadow-[0_0_0_'a]b']`,
  `[content:'a]b']` and `after:content-['a]b']` reach the sheet. A string the
  value leaves open still refuses the class, as it does in Tailwind (#689,
  #692).
- `delay-[...]` takes the arbitrary token streams `duration-[...]` already
  took, so `delay-[calc(1s+2s)]` and `delay-[--spacing(1)]` reach the sheet, and
  a `var()` fallback in either family decodes its underscores (#683).
- Preserve Tailwind's declaration-safe token-stream contract for arbitrary
  animation, background, divide, filter, shadow, ring, scrollbar, table,
  transform, transition and typography values, including values that are
  invalid for the target property (#667).
- A `theme()` naming a key the resolved theme does not carry makes the class no
  utility, the way Tailwind emits no rule for it. `shadow-[0_0_0_1px_theme(a_b)]`
  compiled with the call written through into the declaration, and a fallback
  argument now stands in for the missing key (#688).
- `mask-[url(...)]` reads the whole `url()` with the CSS tokeniser, so a bracket
  carrying anything after it is refused rather than sliced.
  `mask-[url(x.png)_center]` emitted `mask-image: url("x.png)_cente")` under
  `.mask-\[url\(x\.png\)_cente\)\]`, a selector no markup carries (#695).
- An arbitrary value keeps the underscore its `\_` escape spells, so
  `font-['My\_Font']`, `[--my\_var:red]` and `data-[foo=bar\_baz]:flex` reach
  the sheet as written instead of carrying the backslash into the value (#676).
- A `theme()` call resolves to the value the project bound, underscores and
  all. The resolved value was written back into the class string unescaped, so
  a palette entry bound to `var(--brand_red)` named `var(--brand red)` and an
  arbitrary property carrying it was dropped (#687).
- Bracketed `has`, `group-has` and `peer-has` variants retain Tailwind's
  `:is(...)` wrapper for bare type and complex selectors (#658).
- An arbitrary length in a variant's class name is spelled as the author wrote
  it, so the selector matches the markup. `min-[0.5ch]:flex` emitted
  `.min-\[\.5ch\]\:flex`, a rule nothing on the page could match, for every
  unit outside a handful (#543).
- A spacing step is a non-negative multiple of 0.25 on the inset and sizing
  families, as it already was on padding, margin and gap, so `top-1.7` and
  `w-1.7` stop being utilities Tailwind never emits (#691).
- A class whose arbitrary value has an unbalanced paren is rejected rather than
  compiled. The value was re-parsed inside a `calc()` the code wrapped around
  it, so the added `)` silently closed the author's stray one and
  `-left-[0)/*1]` became `left: calc(0 * -1)` (#548).
- An arbitrary value reads through the same CSS parsers as the rest of the
  sheet, so a length, a compact `calc()`, a `var()` with its fallback, a
  `theme()` in dot notation, a `--spacing()` call, a grid track, a colour and a
  list style mean the same thing wherever they appear: `ml-[50%]`,
  `left-[calc(5%-2px)]`, `py-[calc(--spacing(2)+1px)]` and `list-[upper-roman]`
  all resolve, and a bracket colour is read as CSS before the palette is
  consulted (#168, #176, #177, #187, #188, #189, #190, #191, #192, #205, #212,
  #217, #236, #241, #262, #277, #278, #325).
- A bad arbitrary value no longer takes the run down. Six bracket spellings
  raised past the result-typed `of_string`, and nine `to_style` sites answered
  an empty style instead of an error, so a class could crash the renderer or
  silently emit nothing (#257, #266, #287).
- A class that cannot mean anything is refused at parse time instead of
  compiling to a dead selector. An invalid palette shade, a value the property
  cannot take, a bracket value that escapes its declaration, trailing text
  after the value, a colour channel with no byte and a non-canonical number are
  all rejected, and a unitless decoration colour declares nothing rather than
  guessing (#127, #234, #237, #282, #284, #285, #307, #309, #532).
- Read an arbitrary value through cascade's own grammar rather than a private
  unit table, so a length, angle, colour, shadow, ease, blur, tracking,
  line-height, stroke width, border-spacing or gradient stop takes every unit
  and math function CSS allows (#371, #372, #373, #375, #376, #377, #378,
  #404, #417, #418, #420, #465, #503, #504, #509, #522).
- Refuse an unreadable arbitrary value at parse time, with the reason, instead
  of accepting the class and emitting nothing usable: animations, order,
  z-index, grid lines, the `not-has` shorthand, data expressions, gradient
  interpolation, a bracket carrying two brackets, and `not-[...]` content no
  selector reader can read (#405, #406, #407, #408, #410, #421, #422, #496,
  #532).
- Spell an arbitrary value in the class name the way the author wrote it, so a
  class the parser produced reads back. `basis-[...]`, `perspective-[...]` and
  a `min-[...]` breakpoint keep the author's spelling, `underline-offset-[...]`
  gets its own class rather than sharing one, and an `nth-*` argument keeps its
  bracket (#412, #413, #415, #489, #490, #532, #564).
- A `var()` reference is read to its end wherever it appears, including inside
  a bracket value, so one carrying its own parentheses or a fallback is not
  truncated (#564).
- A numeric class suffix is read as plain decimal rather than as an OCaml
  literal: `stroke-0x4` emitted a `.stroke-4` nobody wrote, `/0x50` rode the
  opacity modifier onto every colour utility, and `min-[0x600px]` manufactured
  a working 1536px breakpoint. One fraction reader serves the sizing, position,
  flex and translate families, so `top-1/7` and `basis-0/2` read like `w-1/7`
  (#684).

### Colours and effects

- Palette box, inset-box and text shadows keep Tailwind's authored OKLCH value
  as their unguarded fallback instead of converting it to sRGB hex (#657).
- An opacity modifier reaches every colour family. A ring, a per-side border, a
  shadow, a drop shadow, a decoration and a stroke all take one, the alpha can
  itself be a variable (`bg-cyan-400/(--my-alpha-value)`), and `transparent` and
  `inherit` take one everywhere. Shadeless names such as `shadow-white` and
  `stroke-white` work, `light-dark()` and an arbitrary shadow colour resolve,
  and a drop shadow keeps both of its default layers under an opacity (#169,
  #185, #201, #202, #209, #214, #225, #231, #244, #254, #281, #308, #322,
  #323).
- A bracket colour carrying an opacity modifier no longer renders black.
  `text-[rebeccapurple]/50` and its siblings kept the parsed colour rather than
  re-reading the bracket text through the palette, whose failure arm answered
  black; `bg-[red]/50` separately read the palette red-500 instead of CSS red.
  `decoration-`, `divide-` and `stroke-` accept the modifier at all now, and a
  colour the browser resolves at use time keeps the `@supports` fallback
  Tailwind writes (#508, #517).
- An arbitrary colour reaches CSS in the spelling the class wrote. `bg-[#f00]`
  gave `#ff0000`, `bg-[#ffffffff]` gave `#ffffff` and `bg-[#FF0000]` lost its
  case, where Tailwind writes back what the bracket held (#700).
- An opacity modifier over a bracket colour stays a `color-mix()` on the colour
  the class named, across all thirteen colour families. It resolved to that
  colour's `oklab()` channels instead, and where the alpha read a custom
  property the mix went out with no unguarded fallback, so a browser without
  `color-mix()` painted nothing (#711).
- An arbitrary shadow whose alpha reads a custom property keeps the authored
  colour as its unguarded fallback. `shadow-` and `text-shadow-` folded it
  through oklab at full opacity, so a browser with no relative colours painted
  an opaque shadow where Tailwind paints the colour the class named (#711).
- `--color-black` and `--color-white` are written `#000` and `#fff`, the three
  digits Tailwind spells them in, in the theme block and in every colour
  family's unguarded fallback (#711).
- An arbitrary inset shadow keeps its lengths, its spread and its colour when
  that colour is one CSS knows by name. `inset-shadow-[0_0_0_1px_red]` and every
  other bracket carrying a named colour came out as `inset-shadow-none`, where
  the same bracket under `shadow-` was read correctly (#PR).
- An arbitrary shadow under an opacity modifier keeps whatever the value reader
  accepted: a colour keyword, `currentcolor`, a layer list, a leading `inset`.
  `shadow-[0_0_red]/50` and `inset-shadow-[0_0_0_1px_red]/50` came out as
  `shadow-none`, dropping the `--tw-*-alpha` declaration with the rest (#PR).
- A `theme()` alpha survives a hex-bound palette entry. It was applied by
  chopping the colour's closing paren, so it vanished whenever the entry was a
  hex rather than an `oklch()` (#508).
- Each colour family has one owning handler. Five classes were accepted by two,
  and which answered was decided by dune link order, so `bg-red-500` could sort
  before or after its neighbours depending on an unrelated build edit (#518).

### Variants and selectors

- Variants compose in the combinations Tailwind allows. `group-*` and `peer-*`
  take any state and a name (`peer-checked/draft`), `has-*` takes any variant,
  a bare data attribute or a bracket selector (`has-[a]`, `has-peer-checked`),
  `not-*` composes over any variant, `in-*` scopes to an ancestor in a given
  state (`in-focus`), a container query nests with the variants around it, and
  an arbitrary variant works with no `&` anchor (`[code]:pr-4`). A hover gate
  nests inside a wrapping media or container query, an inner selector survives
  an at-rule variant such as `[@supports(display:grid)]:grid`, and a project's
  `@custom-variant` selector shorthand is read (#167, #173, #196, #197, #198,
  #199, #200, #203, #204, #211, #213, #215, #219, #224, #231, #232, #233, #235,
  #238, #280, #314).
- A variant no longer costs the rule inside it. A hover gate survives an
  at-rule variant, a peer hover gate survives a selector variant, a variant
  stays wrapped around a `@starting-style` rule, and a class a variant renames
  keeps the default transition theme (#564).
- A variant wrapped around a hover gate no longer writes an empty rule beside
  the real one. `sm:dark:hover:underline` emitted a declarationless
  `.sm\:dark\:hover\:underline:hover {}` into the dark media block (#703).
- An opacity colour keeps its progressive-enhancement `@supports` guard when
  wrapped in a supports, container or starting-style variant. The modern
  `color-mix()` declaration was previously left unguarded inside that wrapper
  (#666).
- A container query reading a theme token still writes the token's binding.
  `@min-[theme(--breakpoint-lg)]:flex` resolved the reference into the query and
  left `--breakpoint-lg` out of the theme layer, so a consumer reading it off
  the sheet found nothing (#700).
- `[attr~=value]` attribute selectors work in arbitrary variants. The gate
  rejected any bracket containing `~`, reading the whitespace-list operator as
  a sibling combinator (#509).
- `supports-*` emits the test the author wrote, as a typed condition rather
  than a string reparsed after assembly, and a bare property expands to a
  feature test instead of aborting the run (#135, #389, #484).
- A container query is refused as a `not-*` inner, and a media inner is refused
  inside a group or peer negation, matching what Tailwind accepts (#488, #493).

### CSS ordering and structure

- Utilities land where Tailwind puts them across the sheet. A colour's
  `@supports` rule stays with its fallback, container variants order by width,
  the logical sizing families sort last, line-clamp sorts with box-sizing, and
  isolation, float, clear, divide, masks, outline colours, the drop-shadow
  sizes, inset start and end, the basis fractions and the theme namespaces each
  follow Tailwind's order. An unvarianted utility comes before the `not-*`
  group, and a variable whose slot was already taken is no longer dropped from
  the sheet (#242, #243, #249, #250, #251, #253, #263, #264, #267, #268, #269,
  #291, #292, #310, #311, #312).
- More property families emit in Tailwind's band: fill and stroke ahead of
  object-fit, aspect ratio before the dimensions, tab size inside typography,
  field sizing after display, logical block margins before the physical sides,
  ring widths in numeric order, and the transform, filter, gradient, gap, delay
  and list-style families throughout (#564).
- Stacked and compound variants sort by what they contain rather than by their
  prefix text. A compound carries its inner value, a recursive compound follows
  its whole path, an arbitrary variant orders by its selector, data variants
  group by predicate, a negated breakpoint retains its responsive order, a
  project dark override keeps the built-in dark slot, and repeated element
  variants collapse to one key (#564, #672, #673).
- Blocks group the way Tailwind groups them. A run of `@starting-style`
  utilities emits as one block, the utilities one `@apply` pulls in land in a
  single rule, the `@property` an applied utility brings is hoisted and
  deduplicated, a pseudo-element declares its content once, and a project's own
  utilities interleave with the built-in family writing the same property
  (#194, #220, #226, #228, #255, #256, #271, #313, #316, #319, #324).
- A declared `@utility` sorts after the built-ins of its family, as Tailwind
  puts it, so on an element carrying both the declared one wins; and a
  multi-rule one keeps its nesting rather than being flattened and scattered
  (#516).
- The variant cascade comes from one table. An unrecognised prefix used to
  return 0, which put the rule in a different sort bucket entirely rather than
  merely out of order (#520).
- The late typography colour block and the priority-7 theme tail emit in
  Tailwind's order; `divide-x-reverse`, container queries and column values
  sort where Tailwind puts them (#429, #443, #494, #523).
- One malformed declared `@utility` costs only its own class. The re-parse
  answered an error for the whole buffer, so a single unclosed brace silently
  dropped every routed utility in the sheet (#526).
- `text-indent` and the late text families emit where Tailwind puts them.
  `text-indent` sat a whole priority band late, and `text-wrap`,
  `overflow-wrap`/`word-break` and `hyphens` came after the decoration block
  instead of before white-space (#541, #552).
- A declared utility's own rules come before the ones its variants wrap in an
  at-rule, and one whose first property has no order slot sorts among the
  built-ins instead of opening a second `@layer utilities` (#550).
- The initial values of the utility variables emit as one `@supports` block.
  Every `@apply` and every declared utility hoisted a block of its own beside
  the generated sheet's, so a project stylesheet re-declared variables the
  sheet had already initialised (#687).

### Public OCaml API

- Add the typed `divide` constructors, from `divide_x` to `divide_style`: only
  the two reverse utilities were exposed, so the rest of the family was
  reachable from a class string but not from OCaml (#239, closes #5).
- `divide_x_length` accepts a line-width keyword. Tailwind renders
  `divide-x-[thin]`, and the parser already did, but the typed constructor
  raised on it (#522).
- `Var.needs_property_rule` answers `false` for a variable carrying metadata tw
  did not create, where it failed an assertion (#707).

### Parity and packaging

- Require cascade 1.2.0 for the released package pairing. While it remains
  unreleased, CI pins cascade's main branch so builds and tests follow upstream
  rather than an exact development revision (#297, #302, #305, #646).
- Parity is measured over whole sheets and in a real browser. The ordering gate
  compares every statement in both sheets rather than the handful a test names,
  the upstream suite takes its expected values from committed fixtures rather
  than from Tailwind's own output, the typography plugin is exercised against
  real markup, and CI installs the browser the rendering comparison drives,
  which it had been skipping silently (#158, #258, #259, #270, #286, #301,
  #512, #513, #519).
- `tw --tailwind` and `tw --diff` no longer compile the working directory
  before they start. Identifying the pinned CLI ran with Tailwind's source
  detection on, which costs minutes in a large tree (#702).
- `tw --diff` no longer invents a difference for a class Tailwind compiles.
  Building the reference by scanning a file lost every class Tailwind's own
  extractor declines to read back, `group-hover/-2a:underline` among them
  (#705).
- `tw --diff` no longer invents a difference for a `theme(--x)` class. The CLI
  counts that read as a theme dependency only for a candidate it found in a
  file, so a reference built through `@source inline` alone came back without
  the token's binding in `@layer theme` (#712).

## 1.0.0

- Initial public release candidate. Type-safe Tailwind CSS v4 in OCaml,
  with parity against the upstream v4 compiler (core utilities plus the
  official `forms` and `typography` plugins).
