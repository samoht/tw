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
