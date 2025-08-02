# tw - Type-Safe CSS Generation for OCaml

`tw` is a CSS generation library and command-line tool for OCaml,
inspired by [Tailwind CSS](https://tailwindcss.com/).
It provides a type-safe DSL for utility-first CSS with minimal runtime overhead,
working seamlessly on both server-side and client-side with the same code.

## Features

- Type-safe CSS generation with compile-time guarantees
- Command-line tool for extracting classes from source files
- Responsive modifiers, pseudo-classes, and typography utilities
- Works seamlessly with js_of_ocaml for dynamic client-side styling
- Test utilities for comparing output with real Tailwind CSS

## Installation

```bash
opam install tw
```

## Command-Line Usage

Generate CSS for specific classes:
```bash
tw index.html src/
tw src/ > styles.css
tw --no-reset --minify src/
```

Options:
- `-s <classes>`: Generate CSS for a space-separated list of classes (no reset by default)
- `--reset`/`--no-reset`: Include CSS reset (default: yes for file scanning, no for single class)
- `--minify`: Minify the output CSS

## Library Usage

### Basic Example

```ocaml
open Tw

let button_styles = [
  bg ~shade:500 blue;
  text white;
  px (int 4);
  py (int 2);
  rounded md;
]

(* Convert to CSS *)
let css = to_css button_styles |> stylesheet_to_string
```

### Responsive and State Modifiers

```ocaml
let responsive_button = [
  bg ~shade:500 blue;
  on_hover [ bg ~shade:700 blue ];
  on_sm [ px (int 2) ];
  on_md [ px (int 4) ];
  on_lg [ px (int 6) ];
]
```

### Prose Typography

```ocaml
(* Beautiful typography for markdown content *)
let article_styles = [
  prose;           (* Base prose styles *)
  prose_lg;        (* Larger variant *)
  prose_slate;     (* Slate color theme *)
  max_w `Xl_4;
  mx auto;
]

(* Inline usage *)
let prose_css = to_css article_styles |> stylesheet_to_string
```

### Dynamic Styling with js_of_ocaml

`tw` avoids heavy dependencies like Format to keep bundle sizes small:

```ocaml
(* Dynamic style generation in the browser *)
let update_theme dark_mode =
  let styles = [
    if dark_mode then bg gray ~shade:900 else bg white;
    if dark_mode then text white else text ~shade:900 gray;
  ] in
  let css = to_css styles |> stylesheet_to_string in
  inject_styles css
```

## API

The main module provides:
- `Tw.of_string : string -> (Tw.t, string) result` - Parse a Tailwind class string
- `Tw.to_css : ?reset:bool -> Tw.t list -> Tw.Css.stylesheet` - Generate CSS from styles
- `Tw.stylesheet_to_string : ?minify:bool -> Tw.Css.stylesheet -> string` - Serialize CSS

See the [API documentation](https://ocaml.org/p/tw/latest/doc/index.html) for details.

## Acknowledgments

This project is inspired by [Tailwind CSS](https://tailwindcss.com/), a utility-first CSS framework. Tailwind CSS is licensed under the [MIT License](https://github.com/tailwindlabs/tailwindcss/blob/master/LICENSE).

Please consider [supporting the original Tailwind CSS project](https://tailwindcss.com/sponsor) or purchasing [Tailwind Plus](https://tailwindcss.com/plus).

## Contributing

Issues and pull requests are welcome at https://github.com/samoht/tw

## Development

This project uses AI-assisted development. Code generation and
refactoring were performed with [Claude Code](https://claude.ai/code).

### Running Tests

```bash
dune test
```

For comparing output with real Tailwind CSS:
```bash
# Install Tailwind CSS locally (optional, for testing)
npm install -D tailwindcss

# Run tests with Tailwind comparison
dune test
```

## License

ISC - see LICENSE.md