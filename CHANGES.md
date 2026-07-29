## Unreleased

- Reject an arbitrary value the property cannot take instead of falling back to
  a plausible one: `bg-position-[...]`, `bg-size-[...]`, `cursor-[...]`,
  `indent-[...]`, `mask-size-[...]`, `mask-position-[...]`, `object-[...]` and
  `text-shadow-[...]` emitted `center`, `auto`, `0px`, `none` or
  `var(--<value>)` for a value they could not read (#234)
- Keep the outer variant in the class name when an inner one has nested a
  media block of its own: `sm:motion-reduce:hover:translate-y-0` emitted the
  rule as `.motion-reduce\:hover\:translate-y-0`, without the `sm:` (#235)
- Read the `--theme(--x)` spelling of the theme lookup in a container query,
  alongside `theme(--x)`: `@min-[--theme(--breakpoint-sm)]:` was rejected
  (#236)
- Reject an arbitrary value the property cannot take in seven more utilities:
  `shadow-[...]`, `inset-shadow-[...]` and `drop-shadow-[...]` fell back to the
  zero shadow, `font-[...]` quoted the text into a family name, and
  `from-[...]`, `via-[...]` and `to-[...]` read it as the stop position `0%`
  (#237)
- Keep the inner variant's selector under `starting:` and a bracketed at-rule
  variant: `starting:open:opacity-0` lost `open:`'s
  `:is([open], :popover-open, :open)` (#238)

- Anchor an arbitrary-selector variant at the class's own position when an
  inner variant has moved the subject:
  `in-data-stack:[:last-child>&]:*:rounded-b-xl` put `:last-child >` in front
  of the whole selector rather than of the class (#233)
- Accept any variant as the argument of `has-`, not only a state name or a
  bracket: `has-peer-checked` and `group-not-has-peer-not-data-active` were
  rejected as unknown modifiers (#232)

- Reject an arbitrary value that is not what the property takes:
  `col-span-[<value>]`, `row-span-[<value>]`, `transition-[<value>]`,
  `will-change-[<value>]` and `font-features-[<value>]` emitted invalid CSS
  (#231)
- Read the underscore in `font-features-["liga"_0]` as a space (#231)

- Sort the `@container` utilities and `pointer-events-*` before the layout
  group, where Tailwind's utility order puts them (#231)

- Read a shadow list in `shadow-[...]`; the single-shadow reading dropped the
  spread and swallowed the second layer (#231)

- Keep the inner variant's selector under `in-*`, `not-*` and the named
  group/peer variants: `in-data-stack:[:first-child>&]:rounded-t-xl` lost its
  anchor (#231)

- Read a comma-separated layer list in `mask-[...]` and `mask-position-[...]`;
  two `url()` layers collapsed into one malformed `url()`, and a position list
  fell back to `center` (#231)

- Resolve Tailwind's `--alpha(C/P)` in an arbitrary value, and render a
  reference to a palette token from the palette so the fallback carries a
  colour instead of the bare reference (#231)
- Read the position in `mask-radial-at-[30%_30%]`; it reached the sheet with
  its underscore (#231)

- Reject an arbitrary filter amount that is not a number, a percentage or a
  var; `brightness-[abc]` emitted `brightness(0)` (#231)

- `shadow-inner` sets `--tw-shadow` and composes like the other shadow
  shapes; it wrote `box-shadow` directly, so a ring or inset shadow beside it
  was dropped and its `@property` rules were missing (#231)
- Read `--spacing(1)` as `var(--spacing)`, the form Tailwind writes (#231)

- Write the colour itself for a `/100` modifier, which emitted a no-op
  `color-mix` and its `@supports` fallback (#231)
- Apply `!` inside the `@supports` colour override, so `bg-white/75!` no longer
  has its fallback outrank the modern value (#231)
- Emit the fallback and `@supports` pair for a gradient stop with opacity, as
  in `from-white/10` (#231)

- Read `object-[50%]` as a position; it emitted `object-position: var(--50)`
  (#231)
- Write `z-index: auto` for `z-auto`, which referenced a variable nothing
  declares (#231)
- Expand Tailwind's `--spacing(N)` in an arbitrary property value, as in
  `[--gap:--spacing(10)]` (#231)

- Fix `not-in-data-*`, which negated the utility's own class instead of the
  ancestor relation, and keep the inner variant's selector under `not-*`
  (#231)

- Keep the inner variant's selector under an `aria-*`, `data-*` or `has-*`
  variant: `aria-selected:hover:underline` lost its `:hover`,
  `data-[closed]:data-[enter]:` its second attribute (#231)
- Keep the `@media (hover:hover)` gate when an outer variant wraps `hover:`,
  as in `disabled:hover:bg-indigo-500` (#231)

- Support the `@sm/main:` container-query variant, which aims a size query at a
  named container instead of the nearest one (#231)

- Read `calc()` and `var()` in `rounded-[...]`; anything but a plain length
  silently became `border-radius: 0` (#231)

- Declare `--container-*` in `@layer theme` for `basis-sm` and friends; the
  utility read a variable nothing declared (#231)

- Write the keyword for `object-*`, `origin-*` and `perspective-origin-*`
  when no `@theme` defines the token; they referenced a variable nothing
  declared (#231)

- Accept any CSS length in `mask-position-[...]`; only `px` and `%` were
  read, so `mask-position-[8rem_2rem]` fell back to `center` (#231)

- Write the keyword for `list-none`, `list-image-none` and `columns-auto`
  when no `@theme` defines the token; they referenced a variable nothing
  declared (#231)
- Accept the `(--x)` shorthand in `list-image-(--x)` (#231)

- Read `\_` in an arbitrary value as a literal underscore, not an escaped
  space: `content-['Hello\_World']` gave `"Hello World"` (#231)
- Accept any CSS length in `bg-size-[...]`; only `px` and `%` were read,
  so `bg-size-[8rem]` fell back to `auto` (#231)
- Emit a plain `0px` for the zero spacing step, as Tailwind does (#231)

- Emit the shadow, inset-shadow, text-shadow and leading scales under
  `theme(static)`, and derive `--default-font-feature-settings` from the
  `--font-sans--font-feature-settings` a project declares (#231)
- Drop the declaration of a token declared in an `@theme inline` block
  unless something still reads it; the utility carries the value, with the
  font-feature settings declared beside it, and a project override wins
  over the built-in default (#231)

- Honour `theme(static)` on the package import: the whole theme comes out,
  not only the variables a utility used (#230)
- Emit a `@keyframes` declared inside a project's `@theme`; the animation
  it defines was dropped (#230)
- Emit one `@property` per custom property, beside the utilities rather
  than inside each rule that applied them: a sheet using `@apply` carried
  the same `@property` once per applying rule (#228)
- Expand every built-in `@variant` in author CSS, not just `dark`:
  `@variant sm { ... }` was dropped along with the declarations it
  guarded (#229)
- `@apply` puts the utilities it pulls in into one rule instead of one
  rule per utility (#226)
- Read an alpha from a custom property in every colour family:
  `bg-cyan-400/(--my-alpha)` and `bg-cyan-400/[var(--my-alpha)]` kept the
  colour but dropped the alpha (#225)
- Resolve `theme()` with a v3 dotted path, such as `theme("fontSize.sm")`
  (#227)
- Read a font family the project named in its own `@theme`, including
  `@theme inline` semantics: `font-source` was an unknown class (#223)
- Stack an arbitrary-selector variant with the variants beside it:
  `[svg]:first:size-4` lost its `:first-child`, and `[svg]:sm:size-4`
  lost the arbitrary selector (#224)
- Take a colour as a mask gradient stop (`mask-r-from-black`) and any
  image in a mask bracket (`mask-[radial-gradient(...)]`) (#222)
- Reject shades the palette does not define: `bg ~shade:250 gray` raises
  `Invalid_argument` when the value is constructed, and the CLI reports
  `bg-gray-250` as an unknown class instead of failing with an internal
  error (#127)

## 1.0.0

- Initial public release candidate. Type-safe Tailwind CSS v4 in OCaml,
  with parity against the upstream v4 compiler (core utilities plus the
  official `forms` and `typography` plugins).
