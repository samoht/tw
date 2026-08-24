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

### Colours and effects

- Apply alpha modifiers consistently to theme variables, borders, rings,
  decorations, strokes, shadows, and drop shadows; support shadeless and
  `light-dark()` colours and preserve both default drop-shadow layers
  (#169, #185, #201, #202, #209, #214, #225, #231, #244, #254, #281, #308,
  #322, #323).

### Variants and selectors

- Compose arbitrary, compound, negated, group, peer, `has`, `in`, container,
  and at-rule variants with the correct anchors and nesting; parse selector
  content structurally and support custom-variant selector shorthand
  (#135, #167, #173, #196, #197, #198, #199, #200, #203, #204, #211, #213,
  #215, #219, #220, #224, #231, #232, #233, #235, #238, #280, #314).

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

### Public OCaml API

- Add the typed `divide` constructors, from `divide_x` to `divide_style`: only
  the two reverse utilities were exposed, so the rest of the family was
  reachable from a class string but not from OCaml (#239, closes #5).

### Documentation, compatibility, and release quality

- Document how Tailwind parity is measured and update the contributor
  documentation to match the current code layout (#240, #283).
- Strengthen the fixture, ordering, browser-rendering, and suite-trust gates,
  including structural and layer-order comparisons
  (#158, #258, #259, #270, #286, #301).
- Require Cascade 1.1.0 and its released comparison API, removing the moving
  CI dependency (#297, #302).

## 1.0.0

- Initial public release candidate. Type-safe Tailwind CSS v4 in OCaml,
  with parity against the upstream v4 compiler (core utilities plus the
  official `forms` and `typography` plugins).
