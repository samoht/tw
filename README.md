# tw - OCaml DSL for Utility-First CSS

`tw` is a pure OCaml implementation of utility-first CSS, inspired by
Tailwind CSS v3 concepts but designed specifically for OCaml's type system.
Write styles like `bg_blue`, `p 4`, and `on_hover [bg blue 700]`
with compile-time type safety. The companion `tw.html` library provides
type-safe HTML generation with integrated CSS, working seamlessly on both
server-side and client-side.

**Note**: While `tw` is inspired by Tailwind CSS v3, it is not a direct
port. We cherry-pick concepts that work well with OCaml's type system and
add our own innovations where it makes sense.

## Features

For (OCaml) library users:

- Type-safe HTML+CSS generation with compile-time guarantees
- Responsive modifiers, pseudo-classes, and typography utilities
- Works seamlessly with js_of_ocaml for dynamic client-side styling

For (non-OCaml) CLI users:

- Command-line tool for extracting classes from source files
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
tw --no-base --minify src/
```

Options:

- `-s <classes>`: Generate CSS for a space-separated list of classes
  (no base styles by default)
- `--base`/`--no-base`: Include the Base layer (Preflight CSS reset and
  semantic defaults). Default: yes for file scanning, no for single class.
- `--variables` / `--inline`: CSS generation mode switches. `--variables`
  emits CSS variables plus layered output (`@layer` theme/components/utilities,
  and base if enabled). `--inline` resolves values (no variables) and emits
  raw rules without layers. Defaults: `--inline` for single-class, `--variables`
  for file/directory scanning.
- `--minify`: Minify the output CSS

### Behavior

- Variables mode: Always emits layered output (`@layer` theme, components,
  utilities). The Base layer is included only when `--base` is set. With
  `--no-base`, you still get layers, just without `@layer base`.
- Inline mode: Resolves values (no CSS variables) and emits raw rules without
  any `@layer` blocks.
- Defaults: Single-class generation defaults to `--no-base --inline`;
  file/directory scanning defaults to `--base --variables`.

## Library Usage

### Key Differences from Tailwind CSS

While `tw` uses the same class names as Tailwind CSS, it has some
important differences:

1. **Type-safe API**: Instead of strings, `tw` uses OCaml functions
2. **Simplified spacing**: Functions like `p`, `mx`, `gap` accept
   integers directly (e.g., `p 4` instead of `p-4`)
3. **Pure OCaml**: No JavaScript, PostCSS, or Node.js dependencies
4. **Compile-time guarantees**: Invalid classes are caught at compile time

### Basic Example

```ocaml
open Tw

let button_styles = [
  bg ~shade:500 blue;
  text white;
  px 4;        (* Generates "px-4" = 1rem horizontal padding *)
  py 2;        (* Generates "py-2" = 0.5rem vertical padding *)
  rounded md;
]

(* Special variants for non-numeric values *)
let special_styles = [
  m_auto;      (* margin: auto *)
  w_full;      (* width: 100% *)
  h_screen;    (* height: 100vh *)
  gap_px;      (* gap: 1px *)
]

(* Convert to CSS *)
let css = to_css button_styles |> Css.to_string
```

### Responsive and State Modifiers

```ocaml
let responsive_button = [
  bg ~shade:500 blue;
  on_hover [ bg ~shade:700 blue ];
  on_sm [ px 2 ];
  on_md [ px 4 ];
  on_lg [ px 6 ];
]
```

### Prose Typography

```ocaml
(* Beautiful typography for markdown content *)
let article_styles = [
  prose;           (* Base prose styles *)
  prose_lg;        (* Larger variant *)
  prose_slate;     (* Slate color theme *)
  max_w_4xl;
  mx_auto;
]

(* Inline usage *)
let prose_css = to_css article_styles |> Css.to_string
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
  let css = to_css styles |> Css.to_string in
  inject_styles css
```

## HTML Generation with tw.html

The `tw.html` library provides type-safe HTML generation with
integrated CSS support using `tw` classes. It automatically collects
all `tw` utility classes used in your HTML tree and generates the
required CSS.

### Complete Page Generation

The most powerful feature is the `page` function that generates
complete HTML pages with integrated CSS:

```ocaml
open Tw_html

let my_page =
  page
    ~title:"My Beautiful Page"
    ~meta:[("description", "A page built with tw.html")]
    ~tw_css:"main.css"  (* CSS filename - automatically linked in HTML head *)
    (* Head content *)
    [
      link ~at:[At.rel "icon"; At.href "/favicon.ico"] ();
    ]
    (* Body content *)
    [
      div ~tw:Tw.[max_w_4xl; mx_auto; py 8] [
        header ~tw:Tw.[mb 8] [
          h1 ~tw:Tw.[text_3xl; font_bold; text_center] [
            txt "Welcome to my site"
          ]
        ];

        main [
          div ~tw:Tw.[grid; grid_cols 2; gap 6] [
            article ~tw:Tw.[bg white; rounded lg; shadow md; p 6] [
              h2 ~tw:Tw.[text_xl; font_semibold; mb 4] [
                txt "Article Title"
              ];
              p ~tw:Tw.[text ~shade:600 gray; leading_relaxed] [
                txt "This article demonstrates how tw.html automatically collects all utility classes and generates the corresponding CSS."
              ]
            ];

            aside ~tw:Tw.[bg ~shade:50 blue; rounded lg; p 4] [
              h3 ~tw:Tw.[font_medium; mb 2] [ txt "Quick Links" ];
              ul ~tw:Tw.[space_y 2] [
                li [ a ~at:[At.href "/about"] ~tw:Tw.[text blue; on_hover [underline]] [ txt "About" ] ];
                li [ a ~at:[At.href "/contact"] ~tw:Tw.[text blue; on_hover [underline]] [ txt "Contact" ] ];
              ]
            ]
          ]
        ];

        footer ~tw:Tw.[mt 12; pt 6; border_t; text_center] [
          p ~tw:Tw.[text ~shade:500 gray] [
            txt "© 2024 My Site. Built with OCaml and tw.html."
          ]
        ]
      ]
    ]

(* Extract HTML and CSS *)
let () =
  let html_str = html my_page in
  let (css_filename, css_stylesheet) = css my_page in
  let css_str = Tw.Css.to_string ~minify:true css_stylesheet in

  (* Write files *)
  let oc_html = open_out "index.html" in
  output_string oc_html html_str;
  close_out oc_html;

  let oc_css = open_out css_filename in
  output_string oc_css css_str;
  close_out oc_css;

  Printf.printf "Generated index.html and %s\n" css_filename
```

### Component-Based Development

Create reusable components with integrated styling:

```ocaml
open Tw_html

let button ?(variant=`Primary) ~onclick children =
  let styles = match variant with
    | `Primary -> Tw.[bg blue; text white; on_hover [bg ~shade:700 blue]]
    | `Secondary -> Tw.[bg ~shade:200 gray; text ~shade:800 gray; on_hover [bg ~shade:300 gray]]
    | `Danger -> Tw.[bg red; text white; on_hover [bg ~shade:700 red]]
  in
  button
    ~at:[At.onclick onclick]
    ~tw:Tw.(px 4 :: py 2 :: rounded md :: font_medium :: styles)
    children

let card ~title children =
  div ~tw:Tw.[bg white; rounded lg; shadow md; p 6] [
    h3 ~tw:Tw.[text_lg; font_semibold; mb 4] [txt title];
    div children
  ]

(* Usage *)
let my_component =
  card ~title:"User Profile" [
    p [txt "Welcome back!"];
    div ~tw:Tw.[flex; gap 2; mt 4] [
      button ~variant:`Primary ~onclick:"saveProfile()" [txt "Save"];
      button ~variant:`Secondary ~onclick:"cancel()" [txt "Cancel"];
    ]
  ]
```

### Key Features

- **Automatic CSS Generation**: All `tw` utility classes are automatically
    collected and CSS is generated
- **Type Safety**: Full OCaml type checking for HTML structure and
    attributes
- **Component Composition**: Build reusable components with integrated styling
- **Complete HTML5 Support**: All HTML5 elements and attributes with
    proper typing
- **Accessibility**: Built-in support for ARIA attributes and semantic
    HTML

For more examples and API details, see the [tw.html
documentation](https://ocaml.org/p/tw.html/latest/doc/index.html).

See also: docs/variables-and-layers.md for how utilities define variables and how layers are emitted (using Typography as a reference pattern).

## Examples

The `examples/` directory contains complete examples demonstrating
tw.html usage.

Run the example:
```bash
dune exec examples/simple_page.exe
# Generates simple_page.html and simple.css in current directory
```

## API Reference

### Core Functions

The main module provides:
- `Tw.of_string : string -> (Tw.t, string) result` - Parse utility class strings (e.g., "p-4", "bg-blue-500")
- `Tw.to_css : ?base:bool -> ?mode:Tw.Css.mode -> Tw.t list -> Tw.Css.stylesheet` - Generate CSS from styles
- `Tw.Css.to_string : ?minify:bool -> Tw.Css.stylesheet -> string` - Serialize CSS

### Spacing Functions

All spacing functions accept integers (which map to rem values):
- `p`, `px`, `py`, `pt`, `pr`, `pb`, `pl` - Padding (e.g., `p 4` = 1rem)
- `m`, `mx`, `my`, `mt`, `mr`, `mb`, `ml` - Margin (e.g., `m 2` = 0.5rem)
- `gap`, `gap_x`, `gap_y` - Gap for flexbox/grid

Special variants:
- `*_auto` - Auto margins (e.g., `mx_auto` for centering)
- `*_px` - 1px values (e.g., `p_px`, `gap_px`)
- `*_full` - 100% values (e.g., `w_full`, `h_full`)

### Size Functions

Width and height functions also accept integers:
- `w`, `h` - Width/height (e.g., `w 64` = 16rem)
- `min_w`, `min_h`, `max_w`, `max_h` - Min/max dimensions

See the [API documentation](https://ocaml.org/p/tw/latest/doc/index.html) for the complete reference.

## Acknowledgments

### Tailwind CSS Inspiration

This project is inspired by [Tailwind CSS](https://tailwindcss.com/),
a utility-first CSS framework. While `tw` uses the same class names
and utility-first approach, it is a completely separate OCaml
implementation, not a wrapper or binding. Tailwind CSS is licensed
under the [MIT License](https://github.com/tailwindlabs/tailwindcss/blob/master/LICENSE).

Please consider [supporting the original Tailwind CSS
project](https://tailwindcss.com/sponsor) if you find the utility-first
approach valuable.

The HTML generation module includes a minimal implementation adapted
from [htmlit](https://github.com/dbuenzli/htmlit) by Daniel
Bünzli. Please consider [supporting his
work](https://github.com/sponsors/dbuenzli).

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
