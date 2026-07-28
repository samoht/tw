Tailwind's [@apply] pulls a utility's declarations into an author rule.
It is not CSS, so without expansion the at-rule drops out and takes the
now-empty rule with it.

  $ cat > app.css <<EOF
  > @import "tailwindcss" theme(static);
  > @custom-variant dark {
  >   &:where(.dark, .dark *) { @slot; }
  > }
  > .card { @apply flex items-center; }
  > .card svg { @apply ml-2; }
  > .note { @apply dark:text-gray-400; }
  > EOF

  $ cat > index.html <<EOF
  > <div class="p-4"></div>
  > EOF

A utility's declarations land on the author's own selector:

  $ tw --minify --input-css app.css index.html | grep -c '\.card{[^}]*display:flex'
  1
  $ tw --minify --input-css app.css index.html | grep -c '\.card svg{margin-left:'
  1

The author's selector keeps its structure, so a descendant stays a
descendant rather than becoming the utility's own class.

  $ tw --minify --input-css app.css index.html | grep -c '\.ml-2'
  0
  [1]

A variant the project declared wraps the applied utility, rather than
falling back to the built-in of the same name:

  $ tw --minify --input-css app.css index.html | grep -c '\.note:where(\.dark'
  1
  $ tw --minify --input-css app.css index.html | grep -c 'prefers-color-scheme:dark){\.note'
  0
  [1]

A utility that sets one of tw's own variables brings an [@layer properties]
block holding its initial value on the universal selector. That block cannot
nest inside the author's rule, where the leading [*] would come out as a
descendant of it, so it is hoisted to the top level:

  $ cat > border.css <<EOF
  > @import "tailwindcss" theme(static);
  > .box { @apply border-t border-dashed; }
  > EOF
  $ tw --minify --input-css border.css index.html | grep -cF '*,:before,:after,::backdrop{--tw-border-style:solid}'
  1
  $ tw --minify --input-css border.css index.html | grep -c '\.box \*'
  0
  [1]

An unknown utility is skipped rather than aborting the sheet:

  $ cat > bad.css <<EOF
  > @import "tailwindcss" theme(static);
  > .x { @apply not-a-utility; color: red }
  > EOF
  $ tw --minify --input-css bad.css index.html | grep -c '\.x{color:red}'
  1

One [@apply] names several utilities, and they all decorate the same rule, so
their declarations belong together the way Tailwind emits them rather than in
one rule of the author's selector per utility. A variant among them still gets
a rule of its own, since it carries a selector the others do not:

  $ cat > many.css <<EOF
  > @import "tailwindcss" theme(static);
  > .title { @apply truncate leading-6 text-gray-700 dark:text-gray-400; }
  > EOF
  $ tw --minify --input-css many.css index.html | grep -cF '.title{text-overflow:ellipsis;white-space:nowrap;overflow:hidden;--tw-leading:calc(var(--spacing)*6);line-height:calc(var(--spacing)*6);color:var(--color-gray-700)}'
  1
  $ tw --minify --input-css many.css index.html | grep -cF '.title{color:var(--color-gray-400)}'
  1
