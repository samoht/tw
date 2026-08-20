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
  $ tw --minify --input-css app.css index.html | grep -cF '.line-t{border-top-style:var(--tw-border-style);border-top-width:1px;--tw-border-style:dashed;border-style:dashed}'
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

A declared utility's body is author CSS in its own right: it can [@apply]
another declared utility, and guard declarations with a built-in [@variant]
that only [Tw.of_string] knows.

  $ cat > line.css <<EOF
  > @import "tailwindcss" theme(static);
  > @utility line-t {
  >   @apply relative;
  >   @variant before { @apply absolute top-0 h-px; }
  > }
  > @utility line-y {
  >   @apply line-t relative;
  > }
  > EOF
  $ cat > line.html <<EOF
  > <div class="line-y hover:line-t"></div>
  > EOF
  $ tw --minify --input-css line.css line.html | grep -cF '.line-y:before{'
  1
  $ tw --minify --input-css line.css line.html | grep -cF '.hover\:line-t:hover:before{'
  1

Declaration-only utilities join the property family they write. Within that
family Tailwind orders every utility by class name, whether it is built in or
declared by the project:

  $ cat > order.css <<EOF
  > @import "tailwindcss" theme(static);
  > @utility alpha { padding: 1rem }
  > @utility zebra { padding: 2rem }
  > EOF
  $ cat > order.html <<EOF
  > <div class="p-1 p-4 alpha zebra"></div>
  > EOF
  $ tw --minify --input-css order.css order.html | grep -oE '\.(alpha|p-1|p-4|zebra)\{[^}]*\}'
  .alpha{padding:1rem}
  .p-1{padding:var(--spacing)}
  .p-4{padding:calc(var(--spacing)*4)}
  .zebra{padding:2rem}

A border-width utility carries [border-style: var(--tw-border-style)] before
its width. That carrier does not make it a border-style utility: declared
border-style utilities join the real style family after the width family.

  $ cat > border-style.css <<EOF
  > @import "tailwindcss" theme(static);
  > @utility custom-alpha { border-style: groove }
  > @utility custom-zebra { border-style: ridge }
  > EOF
  $ cat > border-style.html <<EOF
  > <div class="border border-2 border-dashed border-double custom-alpha custom-zebra"></div>
  > EOF
  $ tw --minify --input-css border-style.css border-style.html | grep -oE '\.(custom-(alpha|zebra)|border(-[a-z0-9]+)?)\{[^}]*\}'
  .border{border-style:var(--tw-border-style);border-width:1px}
  .border-2{border-style:var(--tw-border-style);border-width:2px}
  .border-dashed{--tw-border-style:dashed;border-style:dashed}
  .border-double{--tw-border-style:double;border-style:double}
  .custom-alpha{border-style:groove}
  .custom-zebra{border-style:ridge}
