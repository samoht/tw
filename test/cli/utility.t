Tailwind's [@utility NAME { ... }] declares a class of the project's own.
It is not CSS, so a parser drops the declaration, and [Tw.of_string] has
never heard of the class either: without expansion the class generates
nothing at all.

  $ cat > app.css <<EOF
  > @import "tailwindcss" theme(static);
  > @utility line-t {
  >   @apply border-t border-dashed;
  >   border-color: red;
  > }
  > @utility line-y {
  >   @apply border-y;
  > }
  > EOF

  $ cat > index.html <<EOF
  > <div class="line-t line-y flex"></div>
  > EOF

The declared class lands in the utilities layer, with its [@apply] expanded
and its own declarations kept:

  $ tw --minify --input-css app.css index.html | grep -c '\.line-t{border-color:red}'
  1
  $ tw --minify --input-css app.css index.html | grep -cF '.line-t{--tw-border-style:dashed;border-style:dashed}'
  1
  $ tw --minify --input-css app.css index.html | grep -cF '.line-y{border-block-style:var(--tw-border-style);border-block-width:1px}'
  1

The declaration itself is not emitted, and neither is a class the project
never declared:

  $ tw --minify --input-css app.css index.html | grep -c '@utility'
  0
  [1]
  $ tw --minify --input-css app.css index.html | grep -c 'line-b'
  0
  [1]

[@property] and the [@layer properties] initial values the applied utilities
bring come out beside the utilities layer, not inside it, and only once:

  $ tw --minify --input-css app.css index.html | grep -c '@layer utilities{[^}]*@property'
  0
  [1]
  $ tw --minify --input-css app.css index.html | grep -oF '@property --tw-border-style' | grep -c .
  1

A variant the project declared wraps the utility it declared:

  $ cat > dark.css <<EOF
  > @import "tailwindcss" theme(static);
  > @custom-variant dark { &:where(.dark, .dark *) { @slot; } }
  > @utility line-t { border-color: red }
  > EOF
  $ cat > dark.html <<EOF
  > <div class="dark:line-t"></div>
  > EOF
  $ tw --minify --input-css dark.css dark.html | grep -cF '.dark\:line-t:where(.dark,.dark *){border-color:red}'
  1
