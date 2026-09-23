# tw: Tailwind CSS v4 in OCaml

`tw` is an implementation of [Tailwind CSS v4](https://tailwindcss.com/) in
OCaml. It compiles Tailwind classes to CSS without Node.js, and it can be used
in two ways. As a command-line tool, it builds an existing Tailwind v4 project
from its CSS entrypoint, whatever language the project is written in. As an
OCaml library, it lets a component carry its styles as typed values, so the
same program can generate the HTML and the CSS.

`tw` tracks Tailwind CSS 4.3.3. Its output is checked against that release on
Tailwind's own utility and variant fixtures and on whole-project stylesheets;
[docs/parity.md](docs/parity.md) explains how, and lists the differences that
remain.

## Install

With Homebrew, for the command-line tool:

<!-- $MDX skip -->
```sh
brew install samoht/tap/tw
```

With opam, for the library and the tool (OCaml 5.2 or later):

<!-- $MDX skip -->
```sh
opam install tw
```

## Building a Tailwind project

`tw` reads the same CSS entrypoint as the `tailwindcss` CLI and takes the same
main flags:

<!-- $MDX skip -->
```sh
tw -i src/app.css -o dist/app.css
tw -i src/app.css -o dist/app.css --watch
```

The entrypoint can use `@theme`, `@source`, `@apply`, `@utility`,
`@custom-variant`, and the two official plugins, `@tailwindcss/typography` and
`@tailwindcss/forms`. `tw` does not run JavaScript, so a project still
configured through `tailwind.config.js` needs to move that configuration into
its CSS entrypoint first, as Tailwind v4 recommends.

To try `tw` on a project, compare its output with Tailwind's:

<!-- $MDX skip -->
```sh
tw -i src/app.css --diff
tw -i src/app.css --diff --html=public/index.html
```

`--diff` compiles the project with both tools and compares the two stylesheets
with [cascade](https://github.com/samoht/cascade), which reports differences by
rule and declaration rather than by bytes. With `--html`, it also renders a page
under both stylesheets in headless Chromium and compares the pixels. This needs
the reference compiler at exactly version 4.3.3: `tw` looks for
`node_modules/.bin/tailwindcss` in the project, then `tailwindcss` on the
`PATH`, then `npx`. The ordinary build does not need any of them.

A few smaller commands are useful while writing styles:

<!-- $MDX skip -->
```sh
tw -s "flex p-4 bg-blue-500 hover:bg-blue-600"   # CSS for some classes
tw -s "prose mb-4" --diff                         # compare them with Tailwind
tw src/ > styles.css                              # scan files, no entrypoint
```

`tw --help` lists the rest.

## Using the OCaml library

In OCaml, a set of utilities is a value of type `Tw.t list`. A misspelt
utility is a type error, and a variant such as `hover` or `md` is a function:

```ocaml
open Tw

let card =
  [
    flex; flex_col; gap 4; p 6;
    bg white; rounded_lg; shadow_sm;
    border; border_color ~shade:200 gray;
    hover [ shadow_md ];
    dark [ bg ~shade:800 gray; text ~shade:100 gray ];
  ]
```

Spacing takes the Tailwind scale, so `p 4` is `1rem`. Colours default to shade
500, `~shade` picks another, and `~opacity` adds transparency:

```ocaml
open Tw

let layout = [ flex; items_center; gap 4; p 6; mx_auto; max_w_4xl ]
let colors = [ bg blue; text white; border_color ~shade:300 gray; bg ~opacity:50 white ]

let responsive =
  [ p 4; md [ p 8 ]; lg [ p 12 ]; hover [ bg ~shade:600 blue ] ]

let article =
  [ prose; prose_lg; mx_auto; prose_headings [ text ~shade:600 blue ] ]
```

`to_classes` gives the value of an HTML `class` attribute, and `to_css` the
stylesheet those classes need:

```ocaml
open Tw

let class_attr = to_classes [ flex; p 4; bg blue ]
let stylesheet = to_css [ flex; p 4; bg blue; hover [ bg ~shade:600 blue ] ]
let css = Css.to_string ~minify:true stylesheet
```

Class strings from elsewhere, such as a template or a CMS, can be parsed too.
`Tw.of_classes` returns the names it did not recognise, so a typo can be
reported rather than silently dropped:

```ocaml
let styles = Tw.str "flex items-center gap-4 p-6 bg-white rounded-lg"
let utilities, unknown = Tw.of_classes "flex bg-blu-500 my-app-header"
let parsed = Tw.of_string "hover:bg-blue-600" (* Ok _ *)
let rejected = Tw.of_string "not-a-class" (* Error (`Msg _) *)
```

### Generating HTML and CSS together

The `tw.html` library builds HTML with the styles attached to each element.
Reusing a component brings its CSS along, and `Tw_html.page` collects the
stylesheet for everything the page uses, with no source scanning or safelist:

```ocaml
open Tw_html

let card ~title ~body =
  article ~tw:Tw.[ flex; flex_col; gap 4; p 6; rounded_lg ]
    [ h2 ~tw:Tw.[ text_xl; font_semibold ] [ txt title ];
      p [ txt body ] ]

let page =
  Tw_html.page ~title:"Hello" []
    [ card ~title:"Hello" ~body:"This card carries its own styles." ]

let html = Tw_html.html page
let _file, stylesheet = Tw_html.css page
```

The library also compiles to JavaScript with js_of_ocaml, so the same classes
can be compiled in the browser.

## What is covered

`tw` implements the Tailwind v4 core utilities and variants, and the two
official plugins. The table gives a few examples of each family:

| Family | Examples |
|---|---|
| Layout | `flex`, `grid`, `block`, `hidden`, `container` |
| Spacing | `p-4`, `mx-auto`, `gap-4`, `space-x-2.5` |
| Sizing | `w-full`, `h-screen`, `size-4`, `max-w-4xl` |
| Typography | `text-sm`, `font-bold`, `leading-tight`, `tracking-wide` |
| Colours and opacity | `bg-blue-500`, `text-white/80`, `border-gray-300` |
| Borders and effects | `rounded-lg`, `divide-x`, `ring-2`, `shadow-sm`, `blur-lg` |
| Transforms and transitions | `rotate-45`, `translate-x-1/2`, `duration-200` |
| Typography plugin | `prose`, `prose-lg`, `prose-headings:`, `prose-pre:` |
| Forms plugin | `form-input`, `form-select`, `form-checkbox` |
| Variants | `md:`, `hover:`, `dark:`, `group-hover:`, `before:`, `sm:dark:hover:` |

## Development

<!-- $MDX skip -->
```sh
npm ci            # the pinned Tailwind 4.3.3 used by --diff and the tests
dune build
dune runtest      # includes the examples in this README
```

The parity tests need the pinned Tailwind from `npm ci`. Adding a utility is
described in [docs/adding-a-new-utility.md](docs/adding-a-new-utility.md).

## Licence

ISC. See [LICENSE.md](LICENSE.md).
