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

A declared utility takes its place among the built-in ones rather than landing
after them, at the slot of whatever its body [@apply]s: [bar] applies a
position, so it sorts ahead of every other class here, and [foo] applies a
background colour, which puts it between [block] and [underline]. Tailwind
v4.3.3 emits exactly this order for the same input.

  $ cat > sort.css <<EOF
  > @import "tailwindcss" theme(static);
  > @utility foo { @apply bg-red-500; }
  > @utility bar { @apply relative; }
  > EOF
  $ cat > sort.html <<EOF
  > <div class="bar z-10 ml-auto box-border block foo underline"></div>
  > EOF
  $ tw --minify --input-css sort.css sort.html | grep -oE '\.(bar|foo|z-10|ml-auto|box-border|block|underline)\{'
  .bar{
  .z-10{
  .ml-auto{
  .box-border{
  .block{
  .foo{
  .underline{

A body that only declares CSS is placed the same way, by the family that writes
the property: [pad-thing] sorts with [p-8] and [col-thing] with [text-black].
It lands at the head of that family, where Tailwind orders it against the
family's own members by class name.

  $ cat > decl.css <<EOF
  > @import "tailwindcss" theme(static);
  > @utility pad-thing { padding: 1rem; }
  > @utility col-thing { color: red; }
  > EOF
  $ cat > decl.html <<EOF
  > <div class="underline pad-thing p-8 block col-thing text-black"></div>
  > EOF
  $ tw --minify --input-css decl.css decl.html | grep -oE '\.(pad-thing|col-thing|p-8|block|text-black|underline)\{'
  .block{
  .pad-thing{
  .p-8{
  .col-thing{
  .text-black{
  .underline{
