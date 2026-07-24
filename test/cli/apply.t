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

An unknown utility is skipped rather than aborting the sheet:

  $ cat > bad.css <<EOF
  > @import "tailwindcss" theme(static);
  > .x { @apply not-a-utility; color: red }
  > EOF
  $ tw --minify --input-css bad.css index.html | grep -c '\.x{color:red}'
  1
