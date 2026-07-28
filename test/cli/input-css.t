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
