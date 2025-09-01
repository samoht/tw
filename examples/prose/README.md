# Prose Example

A compact showcase of the Tailwind Typography plugin equivalent implemented in `Tw.Prose`.

What this demo includes:
- Base `.prose` container styling with comprehensive child selectors.
- Size variants: `prose-sm`, `prose-lg`, `prose-xl`, `prose-2xl`.
- Color themes: `prose-gray`, `prose-slate`.
- `not-prose` exclusion usage inside a `.prose` container.

How to build and view:

```bash
# Build and generate prose.html + prose.css
dune build examples/prose:prose.html examples/prose:prose.css

# Open prose.html in your browser
open _build/default/examples/prose/prose.html
```

Notes:
- This example generates CSS by collecting used Tw classes from the HTML and emitting the required rules (including the descendant rules provided by `Tw.Prose`).
- Additional prose color themes (`zinc`, `neutral`, `stone`) can be added after implementation.
