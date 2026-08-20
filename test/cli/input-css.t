The project's CSS entrypoint is compiled, not just read for its @theme:
its own rules and its relative @imports belong in the output, and
[@import "tailwindcss"] marks where the generated sheet goes.

  $ cat > sidebar.css <<EOF
  > .sidebar { color: red }
  > EOF

  $ cat > app.css <<EOF
  > @import "tailwindcss" theme(static);
  > @import "./sidebar.css";
  > .page { display: grid }
  > EOF

  $ cat > index.html <<EOF
  > <div class="flex"></div>
  > EOF

The entrypoint's own rule, and the rule it imports, both survive:

  $ tw --minify --input-css app.css index.html | grep -c '\.page{display:grid}'
  1
  $ tw --minify --input-css app.css index.html | grep -c '\.sidebar{color:red}'
  1

The generated utility is spliced in where the package import sat:

  $ tw --minify --input-css app.css index.html | grep -c '\.flex{display:flex}'
  1

A utility named after a custom @theme token resolves against the entrypoint's
theme, so an animate-* utility uses the project's --animate-* value:

  $ cat > anim.css <<EOF
  > @import "tailwindcss";
  > @theme { --animate-wiggle: wiggle 1s ease-in-out infinite; }
  > EOF

  $ cat > page.html <<EOF
  > <div class="animate-wiggle"></div>
  > EOF

  $ tw --minify --input-css anim.css page.html | grep -c '\.animate-wiggle{animation:var(--animate-wiggle)}'
  1

A project [@theme] can name font families of its own, and [font-<name>] reads
the token it declared:

  $ cat > font.css <<EOF
  > @import "tailwindcss" theme(static);
  > @theme {
  >   --font-source: Georgia, serif;
  > }
  > EOF
  $ cat > font.html <<EOF
  > <div class="font-source font-awesome"></div>
  > EOF
  $ tw --minify --input-css font.css font.html | grep -cF '.font-source{font-family:var(--font-source)}'
  1
  $ tw --minify --input-css font.css font.html | grep -c 'font-awesome'
  0
  [1]

The single-class path reads that same project theme instead of parsing against
the defaults:

  $ tw --minify --input-css font.css -s font-source | grep -cF '.font-source{font-family:var(--font-source)}'
  1

An [@theme inline] token has no declaration of its own: the utility carries
the value. A self-referential one is the exception, since inlining it would
leave the reference dangling.

  $ cat > inline.css <<EOF
  > @import "tailwindcss" theme(static);
  > @theme inline {
  >   --font-a: var(--font-a);
  >   --font-b: var(--font-ext), system-ui;
  > }
  > EOF
  $ cat > inline.html <<EOF
  > <div class="font-a font-b"></div>
  > EOF
  $ tw --minify --input-css inline.css inline.html | grep -cF '.font-b{font-family:var(--font-ext),system-ui}'
  1
  $ tw --minify --input-css inline.css inline.html | grep -c -- '--font-b:'
  0
  [1]
  $ tw --minify --input-css inline.css inline.html | grep -cF -- '--font-a:var(--font-a)'
  1

[theme()] also takes the dotted path of a v3 config, which names the same
token under its old namespace. The [spacing] and [lineHeight] scales are the
spacing step times the key, which v4 keeps no token for, so those are computed:

  $ cat > v3.css <<EOF
  > @import "tailwindcss" theme(static);
  > .a { font-size: theme("fontSize.sm"); line-height: theme("lineHeight.6") }
  > .b { margin: theme(spacing.4); width: theme("screens.sm") }
  > EOF
  $ tw --minify --input-css v3.css index.html | grep -cF '.a{font-size:.875rem;line-height:1.5rem}'
  1
  $ tw --minify --input-css v3.css index.html | grep -cF '.b{margin:1rem;width:40rem}'
  1

An unknown namespace is left alone, the same as an unknown token:

  $ cat > v3bad.css <<EOF
  > @import "tailwindcss" theme(static);
  > .u { color: theme("nope.not-a-namespace") }
  > EOF
  $ tw --minify --input-css v3bad.css index.html | grep -c 'theme("nope.not-a-namespace")'
  1

An imported file's [@layer components { ... }] fills the slot the generated
sheet declared, and the [@keyframes] the utilities bring go at the end of the
document, both the way Tailwind emits them:

  $ cat > comp.css <<EOF
  > @import "tailwindcss" theme(static);
  > @import "./card.css";
  > .after { color: red }
  > EOF
  $ cat > card.css <<EOF
  > @layer components { .card { padding: 1rem } }
  > EOF
  $ cat > comp.html <<EOF
  > <div class="animate-spin after"></div>
  > EOF
  $ tw --minify --input-css comp.css comp.html | grep -oE '@layer components\{\.card\{padding:1rem\}\}|\.after\{color:red\}|@keyframes spin' | head -3
  @layer components{.card{padding:1rem}}
  .after{color:red}
  @keyframes spin

A project can declare [@keyframes] inside its [@theme], beside the [--animate-*]
token that names it. The theme block becomes a [:root] rule, where a nested
[@keyframes] would be invalid, so it is lifted to the top level:

  $ cat > kf.css <<EOF
  > @import "tailwindcss" theme(static);
  > @theme {
  >   --animate-flash: flash 2s forwards;
  >   @keyframes flash {
  >     0% { opacity: 1 }
  >     100% { opacity: 0 }
  >   }
  > }
  > EOF
  $ tw --minify --input-css kf.css index.html | grep -cF '@keyframes flash{0%{opacity:1}to{opacity:0}}'
  1
  $ tw --minify --input-css kf.css index.html | grep -c ':root{[^}]*@keyframes'
  0
  [1]

[@import "tailwindcss" theme(static)] asks for the whole theme, not only the
variables a utility used, so the palette comes out even for colours nothing
references:

  $ cat > static.css <<EOF
  > @import "tailwindcss" theme(static);
  > EOF
  $ tw --minify --input-css static.css index.html | grep -c -- '--color-fuchsia-300:'
  1
  $ tw --minify --input-css static.css index.html | grep -c -- '--breakpoint-sm:'
  1

Without it only what the sheet uses is emitted:

  $ cat > dynamic.css <<EOF
  > @import "tailwindcss";
  > EOF
  $ tw --minify --input-css dynamic.css index.html | grep -c -- '--color-fuchsia-300:'
  0
  [1]

The shadow, text-shadow and leading scales come out under [theme(static)] too,
and the default font-feature settings are derived from the sans and mono tokens
the project declared:

  $ cat > scales.css <<EOF
  > @import "tailwindcss" theme(static);
  > @theme {
  >   --font-sans--font-feature-settings: "cv02";
  > }
  > EOF
  $ tw --minify --input-css scales.css index.html | grep -c -- '--shadow-md:0 4px 6px -1px'
  1
  $ tw --minify --input-css scales.css index.html | grep -c -- '--text-shadow-sm:'
  1
  $ tw --minify --input-css scales.css index.html | grep -cF -- '--default-font-feature-settings:"cv02"'
  1

An [@theme inline] token gets no declaration of its own unless something still
reads it: the utility carries the value instead, along with the font-feature
settings declared beside it. A project override wins over the built-in default:

  $ cat > inl.css <<EOF
  > @import "tailwindcss" theme(static);
  > @theme inline {
  >   --font-sans: var(--font-inter), system-ui;
  >   --font-sans--font-feature-settings: "cv02";
  > }
  > EOF
  $ cat > inl.html <<EOF
  > <div class="font-sans"></div>
  > EOF
  $ tw --minify --input-css inl.css inl.html | grep -cF '.font-sans{font-family:var(--font-inter),system-ui;font-feature-settings:"cv02"}'
  1
  $ tw --minify --input-css inl.css inl.html | grep -c -- '--font-sans:'
  0
  [1]
  $ tw --minify --input-css inl.css inl.html | grep -cF -- '--default-font-family:var(--font-inter),system-ui'
  1

A fallback reference is still a real read. The base layer reads the default
font token with a fallback, so an inline project override must remain live:

  $ cat > inl-default.css <<EOF
  > @import "tailwindcss" theme(static);
  > @theme inline {
  >   --default-font-family: "Satoshi", sans-serif;
  > }
  > EOF
  $ tw --minify --input-css inl-default.css index.html | grep -cF -- '--default-font-family:"Satoshi",sans-serif'
  1
