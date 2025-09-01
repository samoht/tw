# Examples

This folder contains runnable examples demonstrating different parts of the library.

Build and test
- Build all examples: `dune build`
- Run CSS parity tests vs. the v4 CLI: `dune test`
  - Each example generates `index.html` and its own CSS
  - A `tailwind.css` is compiled with the CLI from `index.html`
  - We compare our CSS to the CLI output using a structural diff

- `landing/` — Small landing page showcasing core utilities and layout.
- `prose/` — Typography (prose) utilities with sizes, themes, and not-prose.
- `colors/` — Color palette swatches and text-on-background examples.
- `layout/` — Flexbox and Grid layouts with responsive examples.
- `navigation/` — Responsive navigation bar.
- `hero/` — Hero section with heading and CTAs.
- `cards/` — Responsive cards grid.
- `modifiers/` — Group, group-has, peer, peer-has, has, focus-within, focus-visible.
- `dark_mode/` — Color scheme preferences via the `dark` modifier.
- `accessibility/` — High contrast and reduced motion preferences.
- `animations/` — Starting styles and transitions.
Suggested improvements:
- Split large demos into smaller, utility-focused examples (e.g., `examples/borders`, `examples/forms`).
- Add parity checks comparing generated CSS with a reference output for each example.

Build any example HTML/CSS:

```bash
# Landing
dune build examples/landing:index.html examples/landing:landing.css && dune build examples/landing:tailwind.css

# Prose
dune build examples/prose:index.html examples/prose:prose.css && dune build examples/prose:tailwind.css

# Colors
dune build examples/colors:index.html examples/colors:colors.css && dune build examples/colors:tailwind.css

# Layout
dune build examples/layout:index.html examples/layout:layout.css && dune build examples/layout:tailwind.css

# Navigation
dune build examples/navigation:index.html examples/navigation:navigation.css && dune build examples/navigation:tailwind.css

# Hero
dune build examples/hero:index.html examples/hero:hero.css && dune build examples/hero:tailwind.css

# Cards
dune build examples/cards:index.html examples/cards:cards.css && dune build examples/cards:tailwind.css

# Modifiers
dune build examples/modifiers:index.html examples/modifiers:modifiers.css && dune build examples/modifiers:tailwind.css

# Dark mode
dune build examples/dark_mode:index.html examples/dark_mode:dark_mode.css && dune build examples/dark_mode:tailwind.css

# Accessibility
dune build examples/accessibility:index.html examples/accessibility:accessibility.css && dune build examples/accessibility:tailwind.css

# Animations
dune build examples/animations:index.html examples/animations:animations.css && dune build examples/animations:tailwind.css
```
