# TODO

## Bracket-value parity gaps

- Parity gaps #257 left as-is (pre-existing, behaviour unchanged):
  `decoration-[2]` is a colour in Tailwind (`text-decoration-color: 2`) but a
  thickness here - the only one visible in `--diff`; `border-[3]` and
  `outline-[3]` emit `3px` where Tailwind keeps raw `3`; `decoration-[50%]`
  is `50%` in Tailwind but `.5em` here.

## Wrong CSS emitted (audit 2026-07-31, verified against source)

- [-] Claimed 2026-08-02 (branch modifier-silent-empties).
  Modifier-level silent empties (found 2026-08-01 by a corpus scan over
  test/upstream/variants.txt while fixing the utility-level cluster, tw PR
  #287): `group-not-hover:flex`, `group-not-device-hocus:flex`,
  `data-[foo_^_=_"bar"]:flex` and `aria-checked/foo:flex` are accepted by
  of_string but render no rule. Same fix shape as #287: validate at parse time.

A project `@theme { --drop-shadow-glow: 0 0 8px #ff0000 }` re-declared by
`drop-shadow-glow` emits `#f00` where Tailwind writes `red` - an optimizer
colour-spelling difference on passed-through theme values (found 2026-08-01,
pre-existing). Decide whether tw should keep theme value strings verbatim.

Arbitrary-property findings from the provenance work (2026-08-01, verified
against Tailwind while gating PR #284). [-] Claimed 2026-08-02 (branch
arbitrary-colour-decode), both bullets:

- Underscore-encoded colour values are rejected on the opacity path: the value
  is not run through `Parse.decode_arbitrary_value` before `Css.parse_color`,
  so `[border-color:oklch(0.5_0.2_250)]/[var(--x)]` errors - and that exact
  class is the documented example in arbitrary.mli:4 - as does
  `[color:rgb(255_0_0)]/50`. Tailwind emits both. Decode at the two
  `Css.parse_color` call sites; widens the acceptance surface for every
  arbitrary colour value, so it needs its own review.
- The rejection message at tw.ml:268 says plain `[--name:value]` declarations
  and non-colour properties "are not yet supported", but `[--foo:bar]` and
  `[mask-type:luminance]` both work today; the message is wrong guidance for
  the classes that actually hit it (e.g. `[color:red]xyz`).

Colour-family findings from the decoration work (2026-08-01, verified against
Tailwind while gating PR #281). [-] Claimed 2026-08-02 (branch
colour-family-opacity), all four bullets:

- `*-transparent/50` and `*-inherit/50` are rejected family-wide (bg, text,
  border, decoration, ...) while Tailwind emits `color-mix(in oklab,
  transparent 50%, transparent)` / `... inherit 50% ...`. `Color.color` has no
  Transparent/Inherit constructor - each family models them as separate
  variants with no opacity path - so this is one cross-family change, not
  per-family patches. `*-current/50` already works.
- `*-white-500/50` is wrongly accepted family-wide and renamed to `*-white/50`:
  `Color.is_valid_shade` short-circuits to `true` for shadeless colours, so
  `shade_and_opacity_of_strings` accepts any shade segment. Tailwind rejects
  `bg-white-500/50`. Reject a shade segment on a shadeless colour.
- `decoration-<custom-theme-colour>/<opacity>` still fails to parse: divide and
  backgrounds fall back to `Theme_named` when `Color.of_string` fails,
  decoration does not.
- grid_template.ml:192 and modifiers.ml:1068 still call
  `Css.parse_length (Parse.decode_arbitrary_value ...)` inline; same
  `Parse.arbitrary_length` consolidation as PR #281.

The 16 nullary `Border_t_0 .. Border_l_8` constructors still sit beside
`Border_side_width` - the ~130-line four-table collapse (to_style :674,
suborder :806, to_class :1037, parse) is open.

The has-[]/not-[] bytewise `&`->`*` rewrites (rule.ml:481/:1426,
modifiers.ml:1322) are still string-based, unlike the anchor substitution that
now goes through Nest.substitute; port them to the typed selector API.

- `extract_var_name` (parse.ml:64-68) slices `"var(--"` by hand at 103 sites
  across ~20 modules, and drops everything after a comma:
  `p-[var(--x,var(--y))]` emits `padding: var(--y)` and
  `cursor-[var(--c,var(--d))]` emits `cursor: var(--d)` where Tailwind keeps
  `var(--x, var(--y))`. `var( --x )` fails the exact prefix test and falls
  through unchanged. See the blocked-on-cascade section for the shared fix.

Residuals found gating PR #282 (2026-08-01), same symptom on other paths:

- Several utilities read their suffix with a direct `int_of_string_opt` /
  `float_of_string_opt` instead of `Parse`, so non-decimal spellings still
  pass: `z-0x10` -> `.z-16` and `-z-0x10` -> `.-z-16` (layout.ml),
  `order-1_0`, `columns-0x2`, `grid-cols-0x2`, `grid-rows-0x2`, `basis-1_0`,
  `flex-0x2`, `w-0x4`, `h-0x4`, `border-0x2`. A blanket substitution is not
  safe - some of these readers also serve bracket values, where CSS does allow
  `1e3` - so each site needs its class-suffix path separated first.
- Redundant-zero decimals are accepted although Tailwind rejects them:
  `p-04` -> `.p-4`, `p-4.0` -> `.p-4`, `p-1.50` -> `.p-1.5`; likewise
  `aria-[_modal_]` -> `[aria-modal]` where Tailwind rejects the padded
  spelling. Canonical-form questions rather than base/separator bugs.

## Sort and priority

Still open:

- Within a family, a named theme value sorts before an arbitrary/var() value:
  `max-w-2xl max-w-(--breakpoint-md)` renders named-first where Tailwind orders
  by candidate string (`(` before `2`); surfaced on the site as the
  `@media not (width >= 80rem)` position pair (2026-08-01 measurement). Likely
  the same shared value-sort model as the digit-led named sizes below.
- `not-supports-*:X` sorts beside its base utility's property family instead of
  into the `not-*` variant group: `px-4 not-supports-hanging-punctuation:px-4
  flex not-supports-[display:grid]:flex` interleaves where Tailwind puts all
  `not-*` rules after the unvarianted utilities (probe with `--tailwind`;
  single-class `--diff` is blind). 2026-08-01 measurement.
- `**:[svg]:first:sm:size-4` sorts far too early: tw places it directly after
  `.container`'s breakpoints where Tailwind puts it last of the varianted
  rules; the stacked `**:`/`first:` order key outranks the `sm:` media
  grouping. Probe with `tw -s "container sm:bg-top **:[svg]:first:sm:size-4
  md:block" --tailwind`. 2026-08-01 measurement.
- Digit-led named sizes sort wrongly family-wide: Tailwind natural-sorts the
  value string (`basis-2xl` lands between `basis-2` and `basis-10`); tw puts
  named sizes after all numerics. `max-w-2xl` and `w-2xl` are each wrong in
  their own way, so this is one shared value-sort model, not per-family patches.
- The priority-7 theme-layer emission order differs from real Tailwind behind
  a differ blind spot: ten overlapping slot collisions across
  radius/drop-shadow/ease/animate/perspective, and upstream emits
  `radius-4xl, drop-shadow-md, ease-out` where tw emits
  `drop-shadow-md, ease-out, radius-4xl`. The canonical differ treats the
  reorder as equivalent, so `--diff` cannot see it; renumbering changes
  output. Found gating PR #292 (2026-08-01).
- `is_outline_utility` is provably inert on the current corpus (replacing it
  with `fun _ -> false` keeps all tests and diffs green) - deletion candidate.
- The other 13 family-derived priority constants are invisible to both oracles
  (byte-parity cosmetics; fix opportunistically). ~100 lines of provably dead
  sort code; the suborder channel multiplexes 4 offset schemes in one int with
  a reachable collision mis-sorting not-* variants. Full report:
  scratchpad/ordering-audit.md (session dir).
- The variant cascade order is written four times on three incompatible scales:
  `not_variant_order` (modifiers.ml:2200, 100-12100, keyed by constructor),
  `variant_order_of_prefix` (:2300, 500-110000, keyed by class-name string),
  `variant_order_of_media_cond` (:2402, restating 20000/50000/50100/50200/50300/
  70000/70100/90000/91000/92000/93100) and `compute_variant_order`
  (rule.ml:1147-1148, hardcoding before=1600/after=1601). The catch-all
  `| _ -> 5500` at modifiers.ml:2293 drops any new constructor into the middle
  of the not-* table instead of failing to compile, and an unrecognised prefix
  returns 0, which is not a small ordering error: sort.ml:1129-1132 partitions
  on `variant_order > 0`, so the rule lands in a different bucket entirely.
  Derive all four from one `(constructor, prefix, media-condition, rank)` table
  and drop the catch-all.

## Entrypoint preprocessor (bin/main.ml)

- `@custom-variant NAME (selector);` - the paren shorthand - is silently
  unsupported: `take_named_defs` only matches the `{ ... }` block form, so the
  paren form yields no definition, the class is not routed, `Tw.of_string`
  rejects it, and the utility is dropped from the output with no warning.
  Found building the PR #291 probe (2026-08-01).

The at-rule locators now lex through cascade (tw PR #288), but four
byte-scanners survive in bin/main.ml, still blind to strings, comments and
escapes: `strip_tailwind_import_options` (paren counting),
`hoist_theme_keyframes` (its own `block_end`), `expand_apply` (a `}` in a
string inside an `@apply` line still misfires) and `fill_slots` (`@slot`
literal scan). Port them to the `Scan` module #288 added. Found 2026-08-01.

`drop_unread_inline_tokens` (:975) decides `@theme inline` token liveness by
asking whether the minified sheet contains the literal needle
`"var(" ^ name ^ ")"` (:980), so any reference with a fallback is invisible:
with `@theme inline { --default-font-family: "Satoshi", sans-serif; }` the token
is dropped from `:root` even though the base layer reads
`var(--default-font-family, -apple-system, ...)`, and the fallback silently
wins. The same trick decides self-reference on the next lines. cascade owns this
analysis - `Variables.var_refs_in_value_string` (variables.mli:21, whose
docstring says explicitly that it beats a textual scan), `vars_of_declarations`,
`declaration_uses_var`, `any_var_name` - and tw already passes
`~prune_unused_custom_props:true` to `Css_compare.diff` at :1120.

- `@theme inline` is smuggled through as the selector `:root inline`:
  `theme_blocks_as_root` (:137) rewrites the raw text and
  `theme_overrides_of_css` recovers the modifier at :199-201 with
  `String.ends_with ~suffix:"inline"` on the printed selector. The comment at
  :64-68 justifying it ("cascade drops its body") is false - stylesheet.ml:3059
  preserves prelude and block. `hoist_theme_keyframes` (:75) exists only to
  repair damage this rewrite causes and string-matches `"@theme"`/`"@keyframes"`
  anywhere in the text, including inside comments and strings, on the emitted
  entrypoint path (:1038). `theme_blocks_as_root` itself is confined to override
  extraction, so it cannot corrupt emitted CSS.
- `declared_name` (:931-945) serialises a statement it already holds back to CSS
  and string-matches `"@layer "`, bailing whenever the name list contains a
  comma. `@layer bb;` + `@layer bb { ... }` hoists; `@layer aa, bb;` +
  `@layer bb { ... }` does not. `Css.layer_statement_name_list` (css.mli:261)
  returns exactly what it wants.
- `nest_on_ampersand` (:520-548) prints a typed selector, splits it with a
  hand-written top-level comma scanner (:496), re-implements CSS escape skipping
  in `class_end` (:522-529, whose delimiter set omits `]`, `,` and `"`), splices
  `&` in, and re-parses with `Cascade.Selector.of_string` with no handler. No
  failure reproduced (the `:where(.stack > :not(:last-child))` case the comment
  calls out round-trips), so this is fragility, not a live bug. Do the rewrite
  on the AST: `Selector.as_list`, `Selector.map` replacing the `Class` node with
  `Nesting`, `Selector.list`.
- `tw -s CLASS --input-css app.css` ignores the project theme: the Native branch
  calls `parse_classes ~warn:false class_str` (:1154) and
  `Tw.to_css ~base:include_base styles` (:1159) with no `~theme`, while the Diff
  branch (:1115/:1117) and `native_files` (:1474/:1481) pass
  `~theme:opts.theme`. So `tw -s foo --input-css app.css --diff` reports no
  differences while the plain form emits default-theme CSS, and a token-
  dependent class can be rejected outright. Debugging affordance only; the file
  scan is theme-correct. Thread the theme through both calls and through
  `unknown_class_error` (:1105).
- `:1698` reconciles the three "mutually exclusive" backend modes with an `if`
  over two bools: `--tailwind --diff` silently drops `--tailwind`.
- `files path patterns` (:18-27) recurses on `Sys.is_directory` (follows
  symlinks) with no exclusion list, no visited-inode set, no depth bound and no
  `try` around `Sys.readdir`; the only handler is the whole-run catch at :1497.
  A symlinked ancestor makes the walk non-terminating and one unreadable
  subdirectory aborts the run with no path named. Scanning `_build` cannot
  double the class set (`collect_files` ends in `List.sort_uniq`, :1465) - it
  costs time and pulls stale candidates. Skip `_build`/`node_modules`/`.git`/
  dotfiles, refuse symlinked directories, degrade on `Sys_error`.
- Roughly 1000 of the file's 1722 lines (:65-1030) are a library-grade
  Tailwind-source preprocessor with no CLI concern, in an `(executable)` with no
  `.mli`, so none of it can be unit-tested; the only coverage is six cram files
  grepping minified substrings. Move it to `lib/tools/entrypoint.ml` with an
  `.mli` and unit-test the scanners (brace depth, quoting, the recursion caps of
  8 at :349 and 4 at :823), keeping cram as smoke coverage. Pure structure
  finding - no failing input demonstrated.

## Suite trust

- test/test_tw.ml carries a second copy of the upstream harness -
  `is_allowed_canonicalization_diff` (:19-64), `split_classes` (:240-261),
  `upstream_config`/`_of_string` (:223-238), `extract_root_vars` (:394),
  `extract_var_fallbacks` (:404), `is_scheme_typed_var` (:419), the scheme
  builder (:433-462) - reading the same fixtures with a different parser
  (Astring `cuts ~sep:"<<<>>>"` vs a line state machine). Its tolerance still
  allows `--text-*--line-height` changes (:22-25) and prose selector
  permutations (:29-41) that test/upstream/test.ml:40-48 records as dropped.
  Both runners are under `dune runtest` and the strict one still gates the
  fixtures, so this is stale dead allowances plus duplication, not a masked
  regression. Share one fixture reader and delete the dead allowances.

## Differential browser rendering - blind spots

The harness (`Test_helpers.check_rendering_matches`, all 14 suites) cannot
catch:

- Shorthand/longhand overlap: `same_property_pairs` pairs on the declared
  property NAME, so `inset-4` vs `top-4` and `flex-1` vs `grow` never share an
  element. Hand-probed clean over position (1176 pairs) and flex (780). Fix
  idea: pair on property overlap (cascade's differ has a shorthand-overlap
  model).
- Prose descendant rules: prose elements are bare divs, so `:where(h1)` etc.
  are unexercised; only root declarations and the `--tw-prose-*` vars are
  covered.
- Deliberate calibration, not gaps: palette theme tokens excluded (`tw --diff`
  reports the hex-vs-oklch difference once), custom properties compared with
  whitespace/quotes dropped, `basis-px` not a tw class, fixed 1280x800
  viewport, one engine.

## Custom @utility residuals (slot rework, PRs #271/#272)

- `Utility.order_of_property` returns `None` for whole families (padding,
  margin, colour, ...): `build_property_slots` reads a handler example's FIRST
  declaration, which for those families is the theme variable
  (`--spacing`, `--color-*`), and the `Key (Custom_property _)` guard skips
  it - so a declared `@utility` setting `padding` lands in the layer's tail.
  Candidate fix is one line (first non-custom-property declaration in
  lib/utility.ml) but it moves where every declared @utility sorts, so it
  needs its own PR with the family-placement probe re-run (was 92/95).
  Found gating PR #291 (2026-08-01).

- A declared `border-style` lands on the border-width member of its family:
  `border-2` writes `border-style: var(--tw-border-style)` as a carrier and no
  cascade API distinguishes a bare var() carrier from a real value. Filed in
  cascade's TODO (2026-08-01).
- Declaration-only utilities land at the HEAD of their family; Tailwind orders
  them within it by class name.
- A multi-rule declared utility's base and pseudo-element rules are not
  contiguous; Tailwind nests them into one block, so the shapes differ either
  way.
- tw emits `.line-a:before` twice for a `@variant before` body inside a
  declared @utility (pre-existing, reproduces on main).

## Type safety and state

- `Tw.to_css` raises on values `of_string` accepted: ~70 failwith/invalid_arg in
  to_style/Rule.outputs. Six are reachable from ordinary input (see Crashes);
  most of the rest are unreachable-after-validation - move the reachable ones
  back into of_class as Error, make to_style total.
- Three global mutable variant registries: `custom_breakpoints`
  (modifiers.ml:107), `custom_variants` (:119), `container_variants` (:175),
  written only from test_tw.ml:385/387/472 and upstream/test.ml:357/810/812/827,
  never cleared, leaking across Alcotest cases. `Modifiers.of_string` takes no
  theme, so the parse path reads the global (:1712-1728) while the render path
  resolves the same information from the threaded `Scheme.breakpoints`
  (rule.ml:262/318/430) - a manual sync with two failure modes: bin/main.ml
  never calls `register_custom_breakpoints`, so `--input-css` declaring
  `--breakpoint-10xl` then `-s "10xl:flex"` gives "Unknown modifier" and a file
  scan silently drops the class; and registering `10xl` globally then calling
  `Tw.to_css ~theme:Scheme.default` raises
  `Failure "unknown custom breakpoint: 10xl"` out of a pure conversion. The six
  register/clear functions are public (modifiers.mli:26-52), so two stylesheets
  built in one process from different `@theme` blocks clobber each other.
  `Scheme.t` already carries `breakpoints` (scheme.mli:39). Carry all three on
  Scheme.t and delete the API; at minimum move it to a `For_tests` submodule.
- `?initial_css` (var.ml:13 global side table, one caller effects.ml:257) -
  model as serialisation, delete the table.
- Residuals from PR #292 (var registry): the name->order table is still a
  mutable global filled at definition evaluation - a static table means moving
  ~200 name/order pairs into var.ml and cannot cover the `@theme`-named
  families, whose names exist only at render time (documented in var.mli). And
  `register_property_order`/`family`/`needs_property` are still silent
  last-write-wins; give them the same loudness as `register_variable`.
- `supports-<property>` is encoded in-band by appending the literal
  `": var(--tw)"` (modifiers.ml:1492, :1742), re-synthesised at rule.ml:1019 and
  :1047, and decoded at three sites with `String.ends_with` plus a hardcoded
  `- 11` (style.ml:624-629, rule.ml:1055-1057, rule.ml:1166-1168). The emitted
  `@supports (prop: var(--tw))` matches Tailwind and is intentional, so this is
  maintenance fragility, not wrong output: changing the sentinel yields class
  names like `supports-gri:flex` rather than a compile error. Split into
  `Supports_property` / `Supports_condition` and all four sites disappear.
- `drop_shadow_arbitrary_impl` (filters.ml:689-710) splits on spaces, builds
  `"drop-shadow(" ^ non_color ^ " var(--tw-drop-shadow-color, " ^ color ^ "))"`
  and re-parses it, in a file that builds the same shape typed at :621, :666 and
  :673. The variable name is hand-spelled although `drop_shadow_color_ref`
  exists, so a rename breaks it silently. `of_class` already gates the bracket
  (:1391-1397), so the `| Error _ -> style []` at :710 is mostly unreachable.
- `gradient_position_decl` (backgrounds.ml:451-455) takes a string, tries
  `Css.Gradient_direction.of_string`, and falls back to
  `Css.custom_property ~layer:"utilities" "--tw-gradient-position"` with the
  name hand-spelled beside the typed `gradient_position_var`. Callers
  concatenate (`string_of_int angle_deg ^ "deg " ^ interp_css` at :824,
  `"calc(" ^ ... ^ "deg * -1) " ^ interp_css` at :839) while
  `bg_linear_angle_neg_interp'` builds the same negation typed at :833 for
  `base_decl`: two spellings of one value with no type-checker link. Take a
  typed `Css.gradient_direction` and go through `Var.binding`.
- forms.ml reads four ring variables by raw name (`var_ref
  "tw-ring-offset-width"` :26, `"tw-ring-offset-color"` :27, `"tw-ring-color"`
  :41, `~inset_var:"tw-ring-inset"` :28/:42) while writing the same variables
  through the typed, exported handles in the same file (:45-62,
  effects.mli:110-123). `"tw-ring-inset"` is also spelled literally at
  effects.ml:1445/1613/1741/2199 - six unchecked sites. A rename leaves forms
  emitting `var()` against a property nothing declares.
- `supports_property_test` (rule.ml:1001-1007) concatenates
  `"(" ^ prefix ^ prop ^ ":" ^ value ^ ")"` and joins with `" or "`;
  `normalize_supports_condition` (:1009-1048) does index-of-`:` splitting and
  `is_balanced` bracket counting; the result is re-parsed by
  `Css.Supports.of_string` at :1065, :1312 and :1802. `Supports.property`,
  `Or` and `Not` exist (supports.mli:55/:52/:51). No miscompile reproduced -
  the mis-split cases tried all round-trip correctly - so this is lost parse
  errors and a string round trip. Keep the vendor-prefix table (:991-997) in tw.
- `property_info_to_declaration_value` (var.ml:495-502) overrides two arms of
  cascade's `Variables.pp_value` and delegates the rest. `| Number -> Pp.float v
  ^ "%"` turns a `<number>` initial value into a percentage; unreachable today
  because `property_typed` only emits Universal/Length/Percentage/Color/
  Length_percentage, and the `Length -> Zero -> "0"` divergence is deliberate
  (upstream writes `initial-value: 0`, test/upstream/utilities.txt:3317). Fix
  the Number arm; the string round trip into `Css.custom_property` at
  build.ml:1090 is the cascade-side ask.
- rule.ml:960-962 decides a `content` declaration is already present by
  `Css.declaration_value d = "var(--tw-content)"` (build.ml:770 has the
  analogous `= "var(--" ^ t ^ ")"`). Do NOT apply the obvious fix: checking
  `declaration_name = "content"` alone breaks parity, because Tailwind really
  does emit `content: var(--tw-content); --tw-content: none; content: none;` for
  `before:content-none`. Only the `var(--tw-content)` form is deduped, and that
  is correct. Fragility note only - match on the referenced var name if it is
  touched at all.

## API residuals (from the #273-#275 pass)

- `open Cascade` in 21 public .mli files (the roll-up ask is cascade-side).
- The int-vs-float spacing scale split (`p : int` vs `space_x : float`): one
  numeric type across p/m/gap/space/scroll_*/indent/border_spacing is a
  behaviour decision, not a rename.
- `data_state`/`data_variant`/`data_custom` taking `t` rather than `t list`.
- `At.void` rendering `<div ="">` and `page`'s two unlabelled `t list`
  positionals (tw_html, not tw).
- The ~70 `failwith`/`invalid_arg` reachable from `to_css`: that is the type
  safety section's job, and only the reachable ones should move into `of_class`.

## Duplication to collapse (each has already drifted into a bug)

- Bracket-colour-hint dispatch copy-pasted ~12x (effects.ml:2372..., svg.ml,
  color.ml, typography.ml, text_shadow.ml, backgrounds.ml); effects.ml:2358
  holds a weaker private fork of Color.parse_bracket_color. One
  `Color.parse_bracket_hint`.
- `opacity_suffix` 6 identical private copies + `Color.pp_opacity` disagreeing
  on floats (`opacity-[25]` -> `[25.]`); arbitrary.ml:346 inlines a divergent
  `opacity_of_string`.
- String-prefix checks in 3 idioms (hand-rolled length/sub with an off-by-one at
  sort.ml:794, Parse.has_prefix, String.starts_with). OCaml >= 5.2: standardise
  on the stdlib, delete Parse.has_prefix.
- Variant-colon splitting in 4 spellings (build.ml:16, sort.ml:828/:846/:861);
  keep one bracket-aware splitter in Parse.
- [-] Claimed 2026-08-01 (branch mask-gradient-typed).
  mask_gradient.ml builds CSS by string concatenation: `mask_linear_decl` (:140),
  `gradient_for_direction` (:145), `radial_stops_decl` (:153),
  `conic_stops_decl` (:162), `linear_stops_decl` (:170) assemble
  `"linear-gradient(to " ^ dir ^ ", var(--tw-mask-" ^ ...` and feed 47
  `custom_property` calls (:340-427), with a concatenated variable NAME at :331
  and typed-value-to-string round trips at :115, :462/:467, :566, :748, :798;
  text_shadow.ml:61 similar. cascade's own `custom_property` docstring
  (css.mli:7563) says to prefer `Css.var`, and the typed gradient constructors
  exist (`Linear_gradient`, `Linear_gradient_var`, css.mli:3000-3014), so tw is
  not blocked. Port to Var.binding + typed ctors, byte-identical output gated on
  `--diff`.
- color.ml:52-252 is a private CSS Color 4 stack - `linearize_channel` (:52),
  `gamma_correct` (:56), `linear_rgb_to_oklab` (:66), `rgb_to_oklch` (:90),
  `oklab_to_linear_srgb` (:115), `linear_srgb_to_oklab` (:153),
  `gamut_map_chroma` (:205), `oklch_to_rgb` (:235) - with a counterpart for
  every function in cascade's public, mdx-checked `Color_space` (linear_of_srgb
  :20, srgb_of_linear :24, oklab_of_linear_srgb :113, linear_srgb_of_oklab :116,
  oklch_of_oklab :119, oklab_distance :125, srgb_bytes_of_linear :129).
  `grep Color_space lib/*.ml` is empty although test/upstream/test.ml:485 uses
  it. tw even carries two mutually inconsistent OKLab paths (direct LMS matrix
  at :66, via-XYZ at :153). The matrix constants differ from cascade's by ~1e-9
  (color.ml:147 vs color_space.ml:47), far too small to flip an 8-bit channel -
  no output disagreement demonstrated, so this is maintenance cost. Delete and
  call `Cascade.Color_space`; the one piece cascade lacks is the OKLCh
  chroma-binary-search gamut map (`srgb_bytes_of_linear` returns `None` out of
  gamut), filed on the cascade side.
- `sort_properties_by_order` (build.ml:1059-1072) and
  `sort_property_rules_by_usage` (:1211-1225) contain byte-identical
  `family_order` / `get_family_order` (default 1000) / `get_first_usage`
  (default 10000) blocks feeding the same `compare_property_vars`. The two must
  stay in lockstep - one orders the `@layer properties` initial values, the
  other the `@property` rules - so fixing one alone produces a sheet whose
  `@property` order contradicts its properties layer, exactly the parity bug
  class this code exists to prevent. Extract one comparator.
- [-] Claimed 2026-08-01 (branch e334-dequeue, stacked on PR #294).
  Treat the 16 E334 exclusions merlint.toml still carries as a work queue, per
  module: first try deleting the mirror enum outright by carrying the typed
  Css value in one constructor (byte-identity gated); else rename prefix-free
  with explicit `Css.` qualification at collision sites; keep an exclusion
  only where neither is worth the churn, with a per-module reason. Skip
  text_shadow.ml (PR #289 open), mask_gradient.ml (port in flight) and
  sizing.ml (PR #293 open) - leave their entries with a note.
- `Property.split` (property.ml:3-8) and `Property.dedup` (:9-20) are generic
  partition/dedup over `Css.as_property`, and the identical dedup loop is
  inlined again at var.ml:535-547. Note before collapsing: tw keeps the FIRST
  `@property` per name, the spec and cascade's canonicaliser keep the LAST
  (rule_order.mli:32-38, CSS Properties and Values API 1 sec. 2). Thin in
  practice - tw's duplicates come from the same Var and are identical.
- `parse_utility_order` splits the class on `-` (build.ml:52-54) purely so
  `Utility.base_of_strings` can rejoin it with `String.concat "-"`
  (utility.ml:136-139, marked "keep for backward compatibility with tests");
  build.ml:341-343 repeats it on the path the comment at :328 calls "the
  expensive part". Call `Utility.base_of_class` directly at both sites.
- The theme-token idiom `match Scheme.theme_value theme name with Some v ->
  Css.custom_property ~layer:"theme" ("--" ^ name) v | None -> <fallback>`
  appears at 19 sites (columns.ml:74, cursor.ml:133, layout.ml:529,
  grid_template.ml:81, margin.ml:49/:91, flex_props.ml:116/:151,
  grid_item.ml:12-13, spacing.ml:48, typography.ml:2758/:2798,
  transforms.ml:940/:1018, ...) while filters.ml:254 has the factored
  `theme_decl_if_set` that nothing else uses. Lower value than it looks: the
  sites differ materially and some divergence is deliberate and documented
  (columns.ml:77, layout.ml:534 explain why the no-theme branch writes the
  keyword), grid_item emits in both branches, and margin/spacing carry a value
  rather than a token name. No defect at any cited line. Pure refactor.

## Blocked on cascade API (the cascade half is filed in cascade's TODO)

At the next cascade pin bump, five `No_diff _` patterns need the `_` dropped
(cascade PR #290 made the constructor nullary): bin/main.ml:1162,
test/test_tw.ml:152, test/tools/test_tailwind_gen.ml:50 and :84,
test/helpers/test_helpers.ml:105. The parity oracle also becomes strictly
stricter - a pair that passed on a canonical byte residual now fails, which is
the intended effect. Noted 2026-08-02.

- `shadow-`/`text-shadow-` with `/opacity` and a colour that has no sRGB hex
  (`oklch(...)`, `rgb(var(--x) 0 0)`) emit the plain colour where Tailwind
  emits `oklab(from <c> l a b / 50%)`. Needs a typed relative-colour
  constructor - cascade's `Relative_color` carries a verbatim string body -
  filed in cascade's TODO (2026-08-01, from PR #285 gating).

- CSS math and colour function-name tables exist in three divergent copies:
  `is_css_math_function` (parse.ml:114, 11 names, uniquely has `calc-size`,
  missing the trig/pow/sqrt/exp/log family), cascade's `is_math_function`
  (properties.ml:21089, 14) and `math_function_names` (:21437, 21); likewise
  `is_css_color_fn` (parse.ml:246, 11 prefixes, omits `light-dark`) against
  cascade's `is_color_function` (properties.ml:21043, 12). None of cascade's
  tables are exported, so tw cannot reuse them. Demonstrated at the call site:
  `tw --single="stroke-[light-dark(red,blue)]"` is Unknown class because
  svg.ml:449 routes it to `parse_bracket_stroke_width`. Needs
  `is_math_function` + `is_color_function` exported from cascade. Do NOT delete
  `normalize_css_math_operators` (parse.ml:124) along with them: Tailwind class
  syntax legitimately produces `calc(100%-2rem)`, which no spec-conformant calc
  reader can parse, so the pre-pass is required until cascade grows a lenient
  mode.
- `Parse.is_ident` (parse.ml:223-234) accepts a leading `-` followed by anything
  when the string is longer than 1, and treats any byte >= 0x80 as valid
  byte-wise. `tw --single="transition-[-9foo]"` emits
  `transition-property: -9foo`, which is not a valid CSS ident (transitions.ml:568
  gates on this predicate precisely to keep invalid declarations out). cascade
  exports only per-char predicates (`Syntax.is_ascii_ident_start` :6,
  `is_ascii_ident_continue` :10, `Lexer.spec_non_ascii_ident_cp` :53), so tw
  needs a whole-string `Syntax.is_ident` before it can delete its copy.
- Consecutive `@starting-style` blocks are merged by hand in
  `statements_of_sorted_rules` (build.ml:470-494), whose own comment names the
  gap: cascade merges layers/media/supports/containers
  (`Block.merge_consecutive_*`, wired at optimize.ml:110-114 and :219-222) and
  only descends into `Starting_style` (:552, :661, :1034) without merging
  siblings. Adjacent `@starting-style` blocks are unconditionally mergeable -
  generic CSS, wrong repo. Once cascade merges them, delete the `take_run`/`go`
  loop and let `indexed_rule_to_statement` emit one block per rule.
- `compact_length`/`compact_calc` (modifiers.ml:72-101) print cascade's length
  and calc AST into class-name tokens, with a fourth private `format_float`
  (:68, `Float.to_string`: 12 significant digits, exponent notation) and a
  `Css.Pp.to_string` fallback at :84 and :100, so the same class name can be
  rendered by two formatters with different digits. Not a raw-var()-in-CSS
  violation - these are class-name tokens - and not a duplicate of `pp_calc`,
  which must emit spec-legal spacing while this deliberately emits `1px+2px`.
  Needs a compact printer mode in cascade (`Pp.ctx` flag or
  `Css.to_class_token`); then tw keeps only the `[...]` wrapping.
- tw compares cascade values by serialising them: `merge_named_layers` keys a
  hashtable on `Css.to_string ~minify:true (Css.v [ st ])` (main.ml:904),
  `nested_utilities` dedupes hoisted `@property` blocks the same way (:648,
  :657-663), `merge_same_selector` compares printed selectors (:632-633).
  cascade exports no `equal`/`compare` on `Selector.t` or `statement`. Both are
  concrete variants so structural equality is available today and comparing two
  outputs of one printer is stable - this is API shape and per-statement
  serialisation cost, not fragility. Separately, `from_utility` (:612-614) tests
  `String.contains s '.'` on the printed selector where `Selector.first_class`
  (selector.mli:221, already used at build.ml:67) is exact and descends into
  `:is`/`:where`.
- `@property` initial-value serialisation round-trips through a string into
  `Css.custom_property` at build.ml:1090; the typed entry point
  (`Css.property_initial_declaration`, plus teaching cascade that a registered
  `<length>` zero serialises as bare `0`) is the cascade-side ask. See the
  Number-arm bug under Type safety for the part tw can fix now.

## Performance

None of these are load-bearing on their own; the first two are worth doing
together, the rest are cleanups to take while nearby.

- `Utility.to_style` builds the full `Style.t` tree three times per class per
  render: `Rule.outputs` (rule.ml:2436), `layers` (build.ml:1413, whose result
  feeds only `collect_keyframes`) and `extract_vars_and_rules` (build.ml:1287),
  called from `layers` on the same list two lines later. It dispatches through
  the 53-handler list and allocates the whole declaration tree;
  `Tw.to_inline_style` (one pass) is 1.39ms per 1000 utilities against 4.53ms
  for a full `to_css`. `extract_vars_and_rules` uses `theme` for nothing else,
  so passing the already-built `styles` is a two-line fix.
- Duplicate utilities are compiled end to end before dedup:
  `Tw_html.to_tw` returns one `Tw.t` per class occurrence and nothing
  deduplicates before `Build.to_css`; `deduplicate_typed_triples`
  (build.ml:302) runs after all the work. 1000 unique utilities repeated k
  times, byte-identical output every time: k=1 5.6ms, k=2 6.5ms, k=4 13.3ms,
  k=8 25.8ms, k=16 46.7ms. Tens of milliseconds per page, so it matters when
  generating many pages. The CLI is protected by `List.sort_uniq` in
  `Source_scan.candidates` (:151). Dedup on `Utility.to_class` in `page_impl`
  or at the top of `Build.to_css`; do not use `Utility.deduplicate`
  (utility.ml:196), which is `List.mem` over a growing list with structural
  equality.
- `el_with_tw` (tw_html.ml:249) rebuilds its whole subtree list per node
  (`all_tw_styles @ List.concat_map to_tw children`), so collection is
  O(nodes x depth). Dramatic only at unrealistic depth (8000 = 714ms); on a
  realistic shape (10k nodes, 30-deep spine) it is 13.6ms against 6.7ms flat,
  i.e. ~7ms of avoidable copying per page. Accumulate with `List.rev_append` and
  reverse once, or store children as a list-of-lists flattened in `page_impl`.
- `class_of_base` (utility.ml:114-124) walks all 53 handlers with `List.iter`
  and a `ref` even after matching, unlike the sibling short-circuiting loops.
  ~60-70ns per call against ~230ns per utility for `to_classes`. Use
  `List.find_map`.
- `resolve_placeholder_selector` (rule.ml:2285) renders a full selector to a
  string to test it against `"._"`, at three call sites. Only runs for utilities
  with an explicit `rule_list` (prose, forms, gradients), and
  `Selector.to_string` is 54-207ns, so well under a millisecond even on a
  prose-heavy page. Match the AST: `function Css.Selector.Class "_" -> sel | s -> s`.
- `compute_variant_order` (rule.ml:1146) re-serialises a selector that
  `Build.add_index` rendered two lines earlier (build.ml:400-402) and scans it
  with the `String.sub`-per-index `has_substring` (:1112). The fallback is only
  reached for classes with no variant prefix, whose selectors are short:
  ~0.07ms per 1200 rules. Pass `selector_str` in.
- Polymorphic `compare` on the `(int * int)` `order` field at sort.ml:383, 536,
  809 and 1134, plus `(int * int)` keys at :965 and :980. 48 000 compares cost
  0.359ms against 0.063ms for two `Int.compare`s - ~0.3ms against a 4.5ms
  `to_css`. The 10% `compare_val` profile share is not attributable to these six
  lines. Mechanical; `compare_by_priority_index` already shows the style.
- The comparator recomputes per-rule keys the `indexed_rule` record exists to
  precompute: `selector_modifier_depth` (sort.ml:184, called at :356, :469),
  `extract_media_sort_key` (:214 at :393) despite `media_key`,
  `is_state_modifier_rule` (:436 at :647), `is_outline_utility` (:563 at :628),
  `nested_order` (:1011 at :1073). They sit on narrow conditional branches, not
  the common path, and are individually cheap. Tidy-up, low payoff.
- `var_names_of_sorted_rules` (build.ml:528) repeats the
  `filter_utility_properties r.props` that `indexed_rule_to_statement` already
  does (:263) and sorts each rule's vars with a comparator calling
  `Var.property_order` (which allocates a `String.sub` per call) on both
  operands. The per-rule var lists are tiny, so this is well under a millisecond
  per render. Share the filtered props and decorate-sort-undecorate.
- `decoded_utf_8` (source_scan.ml:25-37) folds a whole file into two int lists
  before converting to arrays - ~12 words per character, and the lists stay live
  until the arrays are built. Real files are kilobytes. Grow the arrays
  directly, or scan bytes and treat >= 0x80 as non-candidate (the predicates
  only test ASCII).

## Tooling and house rules

- [-] Claimed 2026-08-01 (branch shadow-warning-45, stacked on PR #295).
  Enable the shadowing warnings as errors in the tracked root `dune` env
  (verified scoped to tw only: cascade's nested dune-project resolves
  `:standard` against the workspace env), staged by measured fallout
  (`dune build @check` counts, 2026-08-01): warning 45 first (44 sites - the
  exact reported bug class, e.g. cursor.ml:10 `open Css` shadows `None` and
  accessibility.ml:31's `forced_color_adjust None` means `Css.None`), then 44
  (401), then 40 (296). Warning 42 stays OFF deliberately: 4237 sites, all the
  codebase's intended type-directed idiom, no safety payoff. Warning 41 (111)
  is largely unfixable by qualifying - `Css.None` is ambiguous within
  Cascade.Css itself (scrollbar_width, appearance, overscroll_behavior,
  touch_action, svg_paint, ...) - filed as a cascade-side ask.
  PR #295 (mergeable now) already moved `-w -58` into the tracked root dune;
  a fresh worktree no longer needs the dune-workspace copy. Caveat recorded
  there: on a flambda switch cascade built through tw's workspace would need
  its own tracked -58.



- Merlint findings still open after PR #294, each in a file that was
  off-limits during that work (2026-08-01): lib/text_shadow.ml's
  `Text_shadow_*` E334 family (PR #289 renames only the `Arb_*` one, so the
  exclusion entry cannot be dropped when it lands); lib/mask_gradient.ml +
  .mli (18 E334); lib/var.mli (2 doc findings, fixed on the PR #292 branch);
  bin/main.ml (5 long functions, 2 non-mutual `let rec` siblings). Also 4
  E956 findings in test/upstream/dune that appear or not depending on which
  _build artefacts exist (seen 2026-08-01 gating PR #296) - make them
  deterministic, then fix.
- `mask-linear-[100grad]` escapes angle conversion through a suffix-test bug
  the typed port deliberately preserved to hold byte-identity (PR #296
  restricts conversion to rad/turn and pins the current behaviour): the old
  code matched "rad" inside "grad", failed to parse "100g", and passed the
  input through. Converting grad like rad/turn is probably right; needs a
  --tailwind probe plus the upstream fixture as oracle.
- tw's `_opam/bin/ocamlformat` is still a symlink into cascade's switch;
  now both switches are 5.4.1, `opam install ocamlformat.0.29.0` in tw's
  switch is the clean fix.
- Scratch dune projects under `tmp/` break `dune build` for everyone. They only
  rebuild when `lib/` changes, so they look fine until the first library edit,
  and then a stale one (`tmp/verify0`, `tmp/verify_bench`, and `dune fmt`
  tripping over `tmp/perfaudit/bench/phases.ml`) fails the build and the
  repo-wide `dune fmt` in `.git/hooks/pre-commit` with it. Either keep scratch
  executables out of the dune scan or treat them as code that has to keep
  compiling; today the pre-commit hook is unusable whenever one has rotted.
- Local gates can pass against a cascade revision CI does not use: the
  `cascade` symlink is a live checkout other sessions move, while CI resolves
  its own pin. On 2026-08-01 that hid a hard break for hours (cascade made
  `Css_compare.No_diff` nullary; every local gate green, main's CI red on
  warning 28 - tw PR #297). Make the mismatch visible: have the build or the
  test harness report the cascade revision against the pin, or gate on it.
- The merlint pre-commit hook blocks any commit touching bin/main.ml on 7
  pre-existing findings (5 over-long functions, 2 needless `let rec ... and`),
  so unrelated one-line fixes there are forced onto `--no-verify` (hit by tw
  PR #297). Fix those findings so the hook stops training people to bypass it.
- `.gitignore` has `node_modules/` which does not match the node_modules
  SYMLINK every worktree setup creates, so it shows as untracked noise in every
  worktree (three agent sessions noted it 2026-08-01). Add `node_modules` (no
  slash).
- lib/tools uses Fmt (`Stats.print_stats` :197-215, `Fmt.str` at :269) and
  lib/tools/dune depends on it, against the no-Fmt-under-lib rule. The rule's
  rationale does not bite here - the module already depends on unix and drives
  `Sys.command`, and tw_tools is referenced only from bin/dune and the three
  test dunes, never from lib/ or a jsoo target - so this is directory placement:
  move it to test/ or dev-tools/. While there, quote `dir` in the command at
  :269 as :249 already does (always `tmp/tw_gen_XXXXXX`, so no injection risk).

## Reproducible parity measurement

The whole-site comparison against tailwindcss.com is not committed, so its
numbers cannot be re-derived by anyone else. `docs/parity.md` says the inputs
"have to be reconstructed"; that judgement was made on a 148K cost and it was
wrong, given a whole session where numbers moved and neither side could tell
which were real.

Commit `classlist.txt`, `globals.css`, `ref-entry.css`, `search.css`,
`typography.css` and the script, running exactly the three commands
`docs/parity.md` documents and printing their output - no derived counts. The
class-name extraction currently bolted onto the script is not part of the
documented measurement and cannot decode CSS escapes, so it double-counts
`after:content-['_/2197']` as both missing and extra.

Open question: whether it also becomes a `dune runtest` gate with the residual
pinned, which makes any movement fail CI but adds a slow dependency on
`npx tailwindcss`.

## Docs repointing

Stale paths survive in source comments (found gating PR #283, not covered by
it): lib/var.ml:5 (`todo/vars.md`), lib/var.ml:146, lib/var.mli:445,
lib/color.ml:1336/:2670/:2800 and lib/containers.ml:40 still say
`rules.ml`/`rules.mli`.
