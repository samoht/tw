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
