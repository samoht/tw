# tw -- Type-safe Tailwind CSS v4 in OCaml

`tw` is a type-safe OCaml implementation of [Tailwind CSS v4](https://tailwindcss.com/).
It compiles to CSS through a strongly-typed variable system that mirrors
Tailwind's pipeline, producing **byte-for-byte identical output**.

```ocaml
open Tw

let card = [
  flex; flex_col; gap 4.; p 6;
  bg white 0; rounded_lg; shadow_sm;
  border; border_color gray 200;
  hover [ shadow_md ];
  dark [ bg gray 800; text gray 100 ];
]
```

## Features

- **Tailwind v4 parity**: 765 upstream utility tests, 140 variant tests pass
  against real Tailwind CSS output
- **Full plugin support**: `@tailwindcss/typography` (prose) and
  `@tailwindcss/forms` fully implemented
- **Type-safe API**: Invalid classes caught at compile time
- **String parsing**: `Tw.str "flex p-4 bg-blue-500"` for dynamic class lists
- **OCaml 4.14+**: Builds on OCaml 4.14 through 5.4
- **Pure OCaml**: No JavaScript, PostCSS, or Node.js required
- **86k lines** of OCaml

## Installation

```bash
opam pin add tw.dev https://github.com/samoht/tw.git
opam install tw
```

## Quick Start

### Typed API

```ocaml
open Tw

(* Spacing uses float multipliers: 4 = 1rem *)
let layout = [ flex; items_center; gap 4.; p 6; mx_auto; max_w_4xl ]

(* Colors take a color and shade *)
let colors = [ bg blue 500; text white 0; border_color gray 300 ]

(* Responsive and state modifiers *)
let responsive = [
  p 4.; md [ p 8. ]; lg [ p 12. ];
  bg blue 500; hover [ bg blue 600 ];
  text white 0; dark [ text gray 300 ];
]

(* Typography plugin *)
let article = [ prose; prose_lg; max_w_4xl; mx_auto ]

(* Prose element variants *)
let styled_prose = [
  prose;
  prose_headings [ text blue 600 ];
  prose_a [ text blue 500 ];
  prose_code [ bg gray 100 ];
  prose_pre [ bg gray 800 ];
]

(* Generate CSS *)
let css = Build.to_css ~base:true ~minify:true ~optimize:true layout
```

### String API

```ocaml
(* Parse class strings -- raises Invalid_argument on unknown classes *)
let styles = Tw.str "flex items-center gap-4 p-6 bg-white rounded-lg shadow-sm"

(* Single class parsing *)
let ok = Tw.of_string "hover:bg-blue-600"  (* Ok t *)
let err = Tw.of_string "invalid-class"     (* Error (`Msg "...") *)
```

### CLI

```bash
# Generate CSS for specific classes
tw -s "flex p-4 bg-blue-500 hover:bg-blue-600"

# Scan files for classes
tw src/ > styles.css

# Compare with real Tailwind CSS
tw -s "prose mb-4 dark:text-gray-300" --diff

# Generate with Tailwind for comparison
tw -s "flex p-4" --tailwind

# Options
tw --minify --optimize --base src/
tw --variables -s "p-4"        # CSS variables mode (default for files)
tw --inline -s "p-4"           # Inline values (default for -s)
```

## Supported Utilities

All Tailwind CSS v4 core utilities and both official plugins:

| Category | Examples |
|----------|----------|
| Layout | `flex`, `grid`, `block`, `hidden`, `container` |
| Spacing | `p-4`, `mx-auto`, `gap-4`, `space-x-2.5` |
| Sizing | `w-full`, `h-screen`, `size-4`, `max-w-4xl` |
| Typography | `text-sm`, `font-bold`, `leading-tight`, `tracking-wide` |
| Colors | `bg-blue-500`, `text-white`, `border-gray-300` |
| Opacity | `bg-blue-500/50`, `text-white/80` |
| Borders | `border`, `rounded-lg`, `divide-x`, `ring-2` |
| Effects | `shadow-sm`, `opacity-50`, `blur-lg` |
| Transforms | `scale-50`, `rotate-45`, `translate-x-1/2` |
| Transitions | `transition-colors`, `duration-200`, `ease-in` |
| Interactivity | `cursor-pointer`, `select-none`, `scroll-smooth` |
| Filters | `backdrop-blur`, `grayscale`, `invert` |
| Tables | `table-auto`, `border-collapse`, `border-spacing-4` |
| Prose | `prose`, `prose-lg`, `prose-sm`, `prose-headings:`, `prose-pre:` |
| Forms | `form-input`, `form-select`, `form-checkbox` |

### Variants

| Variant | Example |
|---------|---------|
| Responsive | `sm:`, `md:`, `lg:`, `xl:`, `2xl:` |
| State | `hover:`, `focus:`, `active:`, `disabled:` |
| Dark mode | `dark:` |
| Group/Peer | `group-hover:`, `peer-checked:` |
| Pseudo-elements | `before:`, `after:`, `marker:`, `placeholder:` |
| Prose elements | `prose-headings:`, `prose-p:`, `prose-a:`, `prose-code:` |
| Stacked | `dark:hover:`, `md:focus:`, `sm:dark:hover:` |

## Development

```bash
# Build (both OCaml 5.4 and 4.14)
dune build

# Run all tests
dune runtest

# Run specific test suites
dune exec test/upstream/test.exe     # 765 Tailwind parity tests
dune exec test/css/test.exe          # 458 CSS parser tests
dune exec test/test.exe              # 334 main tests

# Compare single utility with Tailwind
dune exec -- tw -s "bg-blue-500 hover:bg-blue-600" --diff

# Fuzz testing
dune exec fuzz/fuzz.exe
```

## License

ISC
