[@variant] applies a variant inside author CSS, and [@custom-variant]
declares one. Both are Tailwind syntax that a CSS parser drops, taking the
guarded declarations with it.

  $ cat > app.css <<EOF
  > @import "tailwindcss";
  > .box {
  >   color: red;
  >   @variant dark { color: blue }
  > }
  > EOF
  $ cat > index.html <<EOF
  > <div class="box flex"></div>
  > EOF

The built-in [dark] expands to a preference query, flattened as Tailwind does:

  $ tw --minify --input-css app.css index.html | grep -c '@media(prefers-color-scheme:dark){\.box{color:blue}}'
  1

A project [@custom-variant] wins over the built-in, and is not emitted itself:

  $ cat > custom.css <<EOF
  > @import "tailwindcss";
  > @custom-variant dark { &:where(.dark, .dark *) { @slot; } }
  > .box { @variant dark { color: blue } }
  > EOF
  $ tw --minify --input-css custom.css index.html | grep -c '\.box:where(\.dark,\.dark \*){color:blue}'
  1
  $ tw --minify --input-css custom.css index.html | grep -c 'custom-variant'
  0
  [1]

The redefinition governs a scanned class-list utility too, not only author
CSS: [dark:flex] uses the project's selector, and a compound keeps its pseudo.
No [prefers-color-scheme] query is emitted for this selector-only override:

  $ cat > darkclass.css <<EOF
  > @import "tailwindcss";
  > @custom-variant dark { &:where(.dark, .dark *) { @slot; } }
  > EOF
  $ cat > darkidx.html <<EOF
  > <div class="dark:flex dark:hover:block"></div>
  > EOF
  $ tw --minify --input-css darkclass.css darkidx.html | grep -cF '.dark\:flex:where(.dark,.dark *){display:flex}'
  1
  $ tw --minify --input-css darkclass.css darkidx.html | grep -cF '.dark\:hover\:block:where(.dark,.dark *):hover{display:block}'
  1
  $ tw --minify --input-css darkclass.css darkidx.html | grep -c 'prefers-color-scheme'
  0
  [1]

Routing through the declared variant leaves the utilities layer in the order
the built-in generator uses: each family at its own slot, and inside a family
the scale in numeric order.

  $ cat > darkorder.html <<EOF
  > <div class="dark:text-zinc-400 dark:text-blue-400 dark:text-sky-400 dark:p-2 dark:p-10 dark:p-1 dark:flex"></div>
  > EOF
  $ tw --minify --input-css darkclass.css darkorder.html | grep -oE '\.dark..[a-z0-9-]+:where'
  .dark\:flex:where
  .dark\:p-1:where
  .dark\:p-2:where
  .dark\:p-10:where
  .dark\:text-blue-400:where
  .dark\:text-sky-400:where
  .dark\:text-zinc-400:where

A routed utility does not displace the plain one it shares a name with:
[table] keeps the slot its own value gives it, behind [flex].

  $ cat > darkshare.html <<EOF
  > <div class="block contents flex table dark:table"></div>
  > EOF
  $ tw --minify --input-css darkclass.css darkshare.html | grep -oE '\.[a-z\\:-]+[^{]*\{display'
  .block{display
  .contents{display
  .flex{display
  .table{display
  .dark\:table:where(.dark,.dark *){display

The declared variant is honoured wherever it sits in the chain, not only when
it leads: a built-in responsive prefix in front of it keeps its media query
around the project's selector.

  $ cat > lgdark.html <<EOF
  > <div class="lg:dark:flex lg:dark:hover:block"></div>
  > EOF
  $ tw --minify --input-css darkclass.css lgdark.html | grep -cF '@media(min-width:64rem){.lg\:dark\:flex:where(.dark,.dark *){display:flex}}'
  1
  $ tw --minify --input-css darkclass.css lgdark.html | grep -cF '.lg\:dark\:hover\:block:where(.dark,.dark *):hover{display:block}'
  1
  $ tw --minify --input-css darkclass.css lgdark.html | grep -c 'prefers-color-scheme'
  0
  [1]

[--spacing(N)] is Tailwind shorthand for the spacing scale, so a CSS parser
rejects the declaration and it drops out of the output entirely.

  $ cat > sp.css <<EOF
  > @import "tailwindcss";
  > .gap { margin-left: --spacing(6) }
  > EOF
  $ tw --minify --input-css sp.css index.html | grep -c '\.gap{margin-left:calc(var(--spacing)\*6)}'
  1

[theme(--token)] inlines the token's value; a media query condition could not
hold a var() anyway.

  $ cat > th.css <<EOF
  > @import "tailwindcss";
  > @media (width <= theme(--breakpoint-sm)) { .b { color: red } }
  > EOF
  $ tw --minify --input-css th.css index.html | grep -c '@media(width<=40rem){\.b{color:red}}'
  1

An unknown token is left alone rather than guessed at:

  $ cat > un.css <<EOF
  > @import "tailwindcss";
  > .u { color: theme(--nope-not-a-token) }
  > EOF
  $ tw --minify --input-css un.css index.html | grep -c 'theme(--nope-not-a-token)'
  1

A routed utility whose own selector is not a bare class survives too: the
[divide-*] family wraps its class in a [:where(... > :not(:last-child))], and
that class is the one the declared variant has to decorate.

  $ cat > dv.html <<EOF
  > <div class="dark:divide-gray-800"></div>
  > EOF
  $ tw --minify --input-css darkclass.css dv.html | grep -cF ':where(.dark\:divide-gray-800:where(.dark,.dark *)>:not(:last-child))'
  1
  $ tw --minify --input-css darkclass.css dv.html | grep -c 'divide-gray-800:where'
  1

Only [dark] had a built-in template, so every other built-in [@variant] used in
author CSS was dropped along with the declarations it guarded. The template is
derived from tw's own output for the named variant, so a responsive one wraps
its body in the breakpoint's media query:

  $ cat > sm.css <<EOF
  > @import "tailwindcss";
  > .box { color: red; @variant sm { font-size: 0.875rem } }
  > EOF
  $ tw --minify --input-css sm.css index.html | grep -cF '@media(min-width:40rem){.box{font-size:.875rem}}'
  1
  $ tw --minify --input-css sm.css index.html | grep -cF '.box{color:red}'
  1

A variant that names nothing tw knows leaves the block alone rather than
guessing:

  $ cat > nope.css <<EOF
  > @import "tailwindcss";
  > .box { @variant not-a-variant { color: red } }
  > EOF
  $ tw --minify --input-css nope.css index.html | grep -c 'color:red'
  0
  [1]
