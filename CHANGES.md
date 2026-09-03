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
- Add the mauve, mist, olive and taupe palettes (#153).

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
  reach the sheet (#PR).
- `delay-[...]` takes the arbitrary token streams `duration-[...]` already
  took, so `delay-[calc(1s+2s)]` and `delay-[--spacing(1)]` reach the sheet, and
  a `var()` fallback in either family decodes its underscores (#PR).
- Preserve Tailwind's declaration-safe token-stream contract for arbitrary
  animation, background, divide, filter, shadow, ring, scrollbar, table,
  transform, transition and typography values, including values that are
  invalid for the target property (#667).
- `mask-[url(...)]` keeps the bare underscore a file name carries, the way
  `bg-[url(...)]` already does: it named a different file, with a space in it
  (#PR).
- An arbitrary value keeps the underscore its `\_` escape spells, so
  `font-['My\_Font']`, `[--my\_var:red]` and `data-[foo=bar\_baz]:flex` reach
  the sheet as written instead of carrying the backslash into the value (#676).
- A `theme()` call resolves to the value the project bound, underscores and
  all. The resolved value was written back into the class string unescaped, so
  a palette entry bound to `var(--brand_red)` named `var(--brand red)` and an
  arbitrary property carrying it was dropped (#687).
- Bracketed `has`, `group-has` and `peer-has` variants retain Tailwind's
  `:is(...)` wrapper for bare type and complex selectors.
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
  (#678).

### Colours and effects

- Palette box, inset-box and text shadows keep Tailwind's authored OKLCH value
  as their unguarded fallback instead of converting it to sRGB hex.
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
- An opacity colour keeps its progressive-enhancement `@supports` guard when
  wrapped in a supports, container or starting-style variant. The modern
  `color-mix()` declaration was previously left unguarded inside that wrapper
  (#666).
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
- Around forty more property families emit in Tailwind's band: fill and stroke
  ahead of object-fit, aspect ratio before the dimensions, tab size inside
  typography, field sizing after display, logical block margins before the
  physical sides, ring widths in numeric order, and the transform, filter,
  gradient, gap, delay and list-style families throughout (#564).
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

## 1.0.0

- Initial public release candidate. Type-safe Tailwind CSS v4 in OCaml,
  with parity against the upstream v4 compiler (core utilities plus the
  official `forms` and `typography` plugins).
