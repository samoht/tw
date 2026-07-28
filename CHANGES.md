## Unreleased

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
