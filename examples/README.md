# Examples

These runnable pages demonstrate `tw` and `tw.html`. Each example generates an
`index.html` page and a stylesheet from the typed utility API; its Dune
`runtest` alias compares that stylesheet with Tailwind CSS output.

## Inventory

- `landing/` — Marketing page with gradients and a call to action.
- `dashboard/` — Responsive analytics and administration layout.
- `prose/` — Article typography, size variants, themes, and `not-prose`.
- `forms/` — Form controls, input states, and validation.
- `colors/` — Palette swatches, alpha variants, and contrasting text.
- `layout/` — Flexbox and Grid layout patterns.
- `modifiers/` — State, group, peer, `has`, and ARIA modifiers.
- `animations/` — Keyframes, starting styles, and transitions.
- `accessibility/` — Contrast, reduced-motion, and focus preferences.

The manual page generated from `examples/index.ml` links to and embeds all nine
examples.

## Build

Build the manual and every example:

```sh
opam exec -- dune build @examples/default
```

Build one example's generated page and project stylesheet:

```sh
opam exec -- dune build \
  _build/default/examples/landing/index.html \
  _build/default/examples/landing/landing.css
```

Run its strict Tailwind parity check (replace `landing` with another inventory
directory as needed):

```sh
TW_TAILWIND_TESTS=1 opam exec -- dune build @examples/landing/runtest
```

Run every example parity check with `@examples/runtest`.
