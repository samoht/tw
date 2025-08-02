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

- `-s <classes>`: Generate CSS for a space-separated list of classes
  (no reset by default)
- `--reset`/`--no-reset`: Include CSS reset (default: yes for file
  scanning, no for single class)
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
let css = to_css button_styles |> Css.to_string
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
  max_w xl_4;
  mx auto;
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

The `tw.html` library provides type-safe HTML generation with integrated Tailwind CSS support. It automatically collects all Tailwind classes used in your HTML tree and can generate the required CSS.

### Complete Page Generation

The most powerful feature is the `page` function that generates complete HTML pages with integrated CSS:

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
      div ~tw:Tw.[container; mx auto; py (int 8)] [
        header ~tw:Tw.[mb (int 8)] [
          h1 ~tw:Tw.[text_3xl; font_bold; text_center] [
            txt "Welcome to my site"
          ]
        ];
        
        main [
          div ~tw:Tw.[grid; grid_cols 2; gap (int 6)] [
            article ~tw:Tw.[bg white; rounded lg; shadow md; p (int 6)] [
              h2 ~tw:Tw.[text_xl; font_semibold; mb (int 4)] [
                txt "Article Title"
              ];
              p ~tw:Tw.[text ~shade:600 gray; leading_relaxed] [
                txt "This article demonstrates how tw.html automatically collects all Tailwind classes and generates the corresponding CSS."
              ]
            ];
            
            aside ~tw:Tw.[bg ~shade:50 blue; rounded lg; p (int 4)] [
              h3 ~tw:Tw.[font_medium; mb (int 2)] [ txt "Quick Links" ];
              ul ~tw:Tw.[space_y (int 2)] [
                li [ a ~at:[At.href "/about"] ~tw:Tw.[text blue; on_hover [underline]] [ txt "About" ] ];
                li [ a ~at:[At.href "/contact"] ~tw:Tw.[text blue; on_hover [underline]] [ txt "Contact" ] ];
              ]
            ]
          ]
        ];
        
        footer ~tw:Tw.[mt (int 12); pt (int 6); border_t; text_center] [
          p ~tw:Tw.[text ~shade:500 gray] [
            txt "© 2024 My Site. Built with OCaml and Tailwind CSS."
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
    ~tw:Tw.(px (int 4) :: py (int 2) :: rounded md :: font_medium :: styles)
    children

let card ~title children =
  div ~tw:Tw.[bg white; rounded lg; shadow md; p (int 6)] [
    h3 ~tw:Tw.[text_lg; font_semibold; mb (int 4)] [txt title];
    div children
  ]

(* Usage *)
let my_component = 
  card ~title:"User Profile" [
    p [txt "Welcome back!"];
    div ~tw:Tw.[flex; gap (int 2); mt (int 4)] [
      button ~variant:`Primary ~onclick:"saveProfile()" [txt "Save"];
      button ~variant:`Secondary ~onclick:"cancel()" [txt "Cancel"];
    ]
  ]
```

### Key Features

- **Automatic CSS Generation**: All Tailwind classes are automatically collected and CSS is generated
- **Type Safety**: Full OCaml type checking for HTML structure and attributes
- **Component Composition**: Build reusable components with integrated styling
- **Complete HTML5 Support**: All HTML5 elements and attributes with proper typing
- **Accessibility**: Built-in support for ARIA attributes and semantic HTML

For more examples and API details, see the [tw.html documentation](https://ocaml.org/p/tw.html/latest/doc/index.html).

## API

The main module provides:
- `Tw.of_string : string -> (Tw.t, string) result` - Parse a Tailwind class string
- `Tw.to_css : ?reset:bool -> Tw.t list -> Tw.Css.stylesheet` - Generate CSS from styles
- `Tw.Css.to_string : ?minify:bool -> Tw.Css.stylesheet -> string` - Serialize CSS

See the [API documentation](https://ocaml.org/p/tw/latest/doc/index.html) for details.

## Acknowledgments

This project is inspired by [Tailwind CSS](https://tailwindcss.com/),
a utility-first CSS framework. Tailwind CSS is licensed under the [MIT
License](https://github.com/tailwindlabs/tailwindcss/blob/master/LICENSE).

Please consider [supporting the original Tailwind CSS
project](https://tailwindcss.com/sponsor) or purchasing [Tailwind
Plus](https://tailwindcss.com/plus).

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