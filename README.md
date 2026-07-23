# tw — Type-safe Tailwind CSS v4 in OCaml

`tw` is an OCaml implementation of [Tailwind CSS v4](https://tailwindcss.com/).
You assemble utilities as ordinary OCaml values and compile them to CSS through
the same typed variable-and-layer pipeline Tailwind uses, producing
byte-for-byte identical output. Unknown utilities are caught at compile time,
and nothing depends on Node.js, PostCSS, or the Tailwind CLI at run time.

**Tracked version:** `tw` targets **Tailwind CSS v4.3.3** (the latest release).
Its generated CSS is checked byte-for-byte against that exact version.

```ocaml
open Tw

(* A set of utilities is a value of type [Tw.t list]. *)
let card =
  [
    flex; flex_col; gap 4; p 6;
    bg white; rounded_lg; shadow_sm;
    border; border_color ~shade:200 gray;
    hover [ shadow_md ];
    dark [ bg ~shade:800 gray; text ~shade:100 gray ];
  ]
```

## Features

- **Tailwind v4 parity** — output is checked byte-for-byte against the real
  Tailwind CSS CLI across the upstream utility and variant test suites.
- **Both official plugins** — `@tailwindcss/typography` (`prose`) and
  `@tailwindcss/forms` are fully implemented.
- **Type-safe** — utilities are typed constructors, so invalid combinations are
  compile errors rather than silent no-ops.
- **String parsing** — `Tw.str "flex p-4 bg-blue-500"` turns class strings into
  typed utilities for dynamic class lists.
- **Pure OCaml** — no JavaScript toolchain required; the library also compiles
  to JavaScript with js_of_ocaml.

## Installation

Requires OCaml `>= 5.2`.

<!-- $MDX skip -->
```bash
opam install tw
```

## Usage

### Typed utilities

```ocaml
open Tw

(* Spacing scales: the integer is multiplied by 0.25rem, so [p 4] is 1rem. *)
let layout = [ flex; items_center; gap 4; p 6; mx_auto; max_w_4xl ]

(* Colors default to shade 500; [~shade] picks another, [~opacity] adds alpha. *)
let colors = [ bg blue; text white; border_color ~shade:300 gray; bg ~opacity:50 white ]

(* Variants nest as functions: responsive breakpoints, state, and dark mode. *)
let responsive =
  [ p 4; md [ p 8 ]; lg [ p 12 ]; bg blue; hover [ bg ~shade:600 blue ]; dark [ text ~shade:300 gray ] ]

(* The typography plugin, including per-element variants. *)
let article =
  [ prose; prose_lg; mx_auto; prose_headings [ text ~shade:600 blue ]; prose_code [ bg ~shade:100 gray ] ]
```

### Rendering to CSS

```ocaml
open Tw

(* [to_classes] returns the value for an HTML [class] attribute. *)
let class_attr = to_classes [ flex; p 4; bg blue ]

(* [to_css] builds a stylesheet; [Css.to_string] renders it to text. *)
let stylesheet = to_css [ flex; p 4; bg blue; hover [ bg ~shade:600 blue ] ]
let css = Css.to_string ~minify:true stylesheet
```

### Parsing class strings

```ocaml
(* [Tw.str] parses a class string into utilities (raises on unknown classes). *)
let styles = Tw.str "flex items-center gap-4 p-6 bg-white rounded-lg shadow-sm"

(* [Tw.of_string] parses a single class and returns a result. *)
let parsed = Tw.of_string "hover:bg-blue-600" (* Ok _ *)
let rejected = Tw.of_string "not-a-class" (* Error (`Msg _) *)
```

### CLI

<!-- $MDX skip -->
```bash
# Generate CSS for specific classes
tw -s "flex p-4 bg-blue-500 hover:bg-blue-600"

# Scan files for classes
tw src/ > styles.css

# Compare with real Tailwind CSS
tw -s "prose mb-4 dark:text-gray-300" --diff
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

### Prerequisites

The `--diff` and `--tailwind` CLI modes require the `tailwindcss` standalone
binary for comparison testing:

<!-- $MDX skip -->
```bash
# macOS (Apple Silicon)
curl -sLO https://github.com/tailwindlabs/tailwindcss/releases/latest/download/tailwindcss-macos-arm64
chmod +x tailwindcss-macos-arm64
sudo mv tailwindcss-macos-arm64 /usr/local/bin/tailwindcss

# Linux (x64)
curl -sLO https://github.com/tailwindlabs/tailwindcss/releases/latest/download/tailwindcss-linux-x64
chmod +x tailwindcss-linux-x64
sudo mv tailwindcss-linux-x64 /usr/local/bin/tailwindcss
```

### Building

<!-- $MDX skip -->
```bash
dune build         # build the library and CLI
dune runtest       # run all tests, including README examples

# Compare a single utility with Tailwind
dune exec -- tw -s "bg-blue-500 hover:bg-blue-600" --diff
```

## License

ISC — see [LICENSE.md](LICENSE.md).
