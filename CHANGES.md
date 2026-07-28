## Unreleased

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
