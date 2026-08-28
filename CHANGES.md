## 1.1.0

### Tailwind CSS 4.3.3

- Track Tailwind CSS 4.3.3, update the system font stack and preflight, write
  powerless hues as `none`, add the mauve, mist, olive, and taupe palettes,
  and regenerate the upstream parity corpus
  (#128, #129, #130, #132, #147, #153).

### CLI and project stylesheets

- Compile complete CSS entrypoints and imports; scan Markdown, MDX,
  JavaScript, and TypeScript; and expand author-written `@variant`,
  `@custom-variant`, `--spacing()`, `theme()`, `@apply`, and `@utility`
  constructs (#136, #137, #138, #139, #140, #141, #143, #195, #206).
- Carry the entrypoint theme, declared variants, routed utilities, keyframes,
  layers, properties, static theme scales, and v3 theme paths through class
  generation (#170, #179, #193, #194, #221, #226, #227, #228, #229, #230,
  #315, #320).
- Gate typography utilities on the plugin, ignore candidates that cannot
  render, recognize source boundaries correctly, tokenize entrypoint rewrites,
  and make recursive content scans safe
  (#142, #144, #145, #208, #288, #318, #321).
- Reject conflicting CLI backends instead of silently selecting one (#317).
- Keep Tailwind's own at-rules out of the generated CSS: `@theme`, `@source`,
  `@plugin`, `@config`, `@reference`, and `@tailwind` used to reach the
  browser, which has no meaning for any of them (#361).

### Utility coverage

- Complete container, viewport, fractional, and logical sizing; support the
  full position and spacing scales, arbitrary fraction denominators, and
  consistent basis, translate, and size generation
  (#146, #151, #152, #154, #155, #156, #159, #160, #166, #172, #180, #186,
  #207, #210, #216, #293).
- Expand transform, background, grid, typography, transition, and general
  keyword coverage, including `none`, perspective, text indent, aspect ratio,
  content, theme-named fonts, and arbitrary grid tracks
  (#134, #157, #164, #171, #174, #175, #178, #181, #183, #184, #212, #218,
  #223).
- Add logical and axis border utilities, arbitrary integer border widths,
  mask stops and images, mask gradients, and typed divide utilities
  (#148, #161, #162, #163, #165, #182, #222, #239, #265, #296).

- Accept a value the project named in its own `@theme` wherever Tailwind does:
  shadows, blur radii, timing functions, font weights, line heights, letter
  spacing, corner radii, font sizes, perspectives, aspect ratios and max-widths
  (#447, #450, #457, #458, #459, #460, #461, #462, #463, #464).
- Honour a `@theme` that REMOVES a token. A namespace reset (`--color-*:
  initial`) now reaches tw at all, `--spacing: initial` drops the multiplier,
  a removed breakpoint stops resolving its variant, and a removed palette
  entry no longer leaves a utility referencing a variable nothing declares
  (#507, #515).
- Keep `@keyframes` when a `@theme` redefines an animation. The keyframes
  follow the animation the value names, so retiming `--animate-ping` no longer
  emitted an animation that never ran (#510).

### Arbitrary values and validation

- Parse arbitrary lengths, calculations, theme and spacing functions, grid
  tracks, colours, list styles, and custom properties through the shared CSS
  parsers while preserving their original meaning
  (#168, #171, #174, #175, #176, #177, #178, #187, #188, #189, #190, #191,
  #192, #205, #212, #217, #218, #236, #241, #257, #266, #277, #278, #287,
  #325).
- Report invalid palette shades and arbitrary values as unknown classes rather
  than crashing or emitting dead selectors; reject trailing text, malformed
  channels, unitless decoration colours, and non-canonical numeric spellings
  (#127, #234, #237, #257, #282, #284, #285, #307, #309).

- Read an arbitrary value through cascade's own grammar rather than a private
  unit table, so a length, angle, colour, shadow, ease, blur, tracking,
  line-height, stroke width, border-spacing or gradient stop takes every unit
  and math function CSS allows (#371, #372, #373, #375, #376, #377, #378,
  #404, #417, #418, #420, #465, #503, #504, #509, #522).
- Refuse an unreadable arbitrary value at parse time, with the reason, instead
  of accepting the class and emitting nothing usable: animations, order,
  z-index, grid lines, the `not-has` shorthand, data expressions, gradient
  interpolation, and a bracket carrying two brackets (#405, #406, #407, #408,
  #410, #421, #422, #496).
- Spell an arbitrary value in the class name the way the author wrote it, so a
  class the parser produced reads back (#412, #413, #415, #489, #490).

### Colours and effects

- Apply alpha modifiers consistently to theme variables, borders, rings,
  decorations, strokes, shadows, and drop shadows; support shadeless and
  `light-dark()` colours and preserve both default drop-shadow layers
  (#169, #185, #201, #202, #209, #214, #225, #231, #244, #254, #281, #308,
  #322, #323).

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

- Compose arbitrary, compound, negated, group, peer, `has`, `in`, container,
  and at-rule variants with the correct anchors and nesting; parse selector
  content structurally and support custom-variant selector shorthand
  (#135, #167, #173, #196, #197, #198, #199, #200, #203, #204, #211, #213,
  #215, #219, #220, #224, #231, #232, #233, #235, #238, #280, #314).

- `[attr~=value]` attribute selectors work in arbitrary variants. The gate
  rejected any bracket containing `~`, reading the whitespace-list operator as
  a sibling combinator (#509).
- `supports-*` emits the test the author wrote, as a typed condition rather
  than a string reparsed after assembly (#389, #484).
- A container query is refused as a `not-*` inner, and a media inner is refused
  inside a group or peer negation, matching what Tailwind accepts (#488, #493).

### CSS ordering and structure

- Match Tailwind's ordering for supports fallbacks, container variants,
  logical sizing, line clamp, layout, drop shadows, divide, masks, outlines,
  insets, basis fractions, theme tokens, and routed variants
  (#242, #243, #249, #250, #251, #253, #261, #262, #263, #264, #267, #268,
  #269, #291, #292, #310, #311, #312).
- Group `@apply`, `@property`, `@starting-style`, and layer output correctly,
  and interleave project utilities with the built-in property family they
  implement (#194, #220, #226, #228, #255, #256, #271, #313, #316, #319,
  #324).

- A declared `@utility` sorts after the built-ins of its family, as Tailwind
  puts it, so on an element carrying both the declared one wins; and a
  multi-rule one keeps its nesting rather than being flattened and scattered
  (#516).
- The variant cascade comes from one table instead of four copies on three
  numeric scales. An unrecognised prefix used to return 0, which put the rule
  in a different sort bucket entirely rather than merely out of order (#520).
- The late typography colour block and the priority-7 theme tail emit in
  Tailwind's order; `divide-x-reverse`, container queries and column values
  sort where Tailwind puts them (#429, #443, #494, #523).
- One malformed declared `@utility` costs only its own class. The re-parse
  answered an error for the whole buffer, so a single unclosed brace silently
  dropped every routed utility in the sheet (#526).

### Public OCaml API

- Add the typed `divide` constructors, from `divide_x` to `divide_style`: only
  the two reverse utilities were exposed, so the rest of the family was
  reachable from a class string but not from OCaml (#239, closes #5).
- An unrecognised class has one behaviour everywhere. `Tw.str` raised,
  `Tw_html` copied the name in silently, and `Tw_dom.use_str` crashed the
  browser render. Unknown names now pass through everywhere, never raise from a
  rendering path, and are reportable: `Tw.of_classes` returns them alongside
  the utilities, and `Tw_html.unknown_classes` and `Tw_dom.unknown_classes`
  expose the same list. `Tw.of_string` is unchanged, so the CLI still reports a
  deliberately typed class as an error (#514).
- **Breaking:** `bg_transparent`, `bg_current` and the background colour
  constructors move to `Backgrounds`; `border_color`, `border_transparent` and
  `border_current` move to `Color`. Building one from OCaml and parsing the
  same class name used to reach different handlers with different sort slots
  (#518).
- Every spacing utility takes an `int`, with a primed variant for a float, so
  the common call needs no conversion (#492).
- `divide_x_length` accepts a line-width keyword. Tailwind renders
  `divide-x-[thin]`, and the parser already did, but the typed constructor
  raised on it (#522).

### Documentation, compatibility, and release quality

- Document how Tailwind parity is measured and update the contributor
  documentation to match the current code layout (#240, #283).
- Strengthen the fixture, ordering, browser-rendering, and suite-trust gates,
  including structural and layer-order comparisons
  (#158, #258, #259, #270, #286, #301).
- Require Cascade 1.1.0 and its released comparison API, removing the moving
  CI dependency (#297, #302).
- The browser rendering comparison actually runs in CI. `npm ci` installs
  Playwright but not the browser it drives, and the check skipped silently
  without one, so eight suites reported no rendering difference because they
  never looked. CI installs Chromium now, and a missing browser fails rather
  than skips (#513).
- The upstream parity suite no longer takes its expected values from Tailwind's
  own output. Breaking four built-in defaults used to leave it passing every
  case; the same breakage now fails it (#512).
- The typography plugin's descendant rules are exercised against real markup
  rather than bare divs, which is most of the largest plugin (#519).

## 1.0.0

- Initial public release candidate. Type-safe Tailwind CSS v4 in OCaml,
  with parity against the upstream v4 compiler (core utilities plus the
  official `forms` and `typography` plugins).
