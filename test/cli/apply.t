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

A [@property] the applied utilities bring is hoisted beside them, not nested in
the rule that applied them, and it is emitted once however many rules or
utilities set the same variable:

  $ cat > props.css <<EOF
  > @import "tailwindcss" theme(static);
  > .one { @apply border-t border-dashed; }
  > .two { @apply border-b border-dotted; }
  > EOF
  $ tw --minify --input-css props.css index.html | grep -oF '@property --tw-border-style' | grep -c .
  1
  $ tw --minify --input-css props.css index.html | grep -c '\.one{[^}]*@property'
  0
  [1]

and it comes after the author's own rules, where Tailwind puts it, rather than
at the [@import] it was spliced into:

  $ tw --minify --input-css props.css index.html | grep -oE '\.two\{[^}]*\}|@property' | head -2
  .two{border-bottom-style:var(--tw-border-style);border-bottom-width:1px;--tw-border-style:dotted;border-style:dotted}
  @property

The [@layer properties] block each applied utility brings holds initial values
on the universal selector. Tailwind emits one; hoisting them per [@apply] used
to leave a block per distinct set:

  $ tw --minify --input-css props.css index.html | grep -oF '@layer properties' | grep -c .
  1
  $ tw --minify --input-css props.css index.html | grep -oF '@layer utilities' | grep -c .
  1

A class that starts with a digit is escaped as a hex code point, and the escape
runs to a space: [2xl:flex] prints as [.\32 xl\:flex]. The class is read off the
selector's syntax tree, so that space does not cut the class in two:

  $ cat > digit.css <<EOF
  > @import "tailwindcss" theme(static);
  > .card { @apply 2xl:flex; }
  > EOF
  $ tw --minify --input-css digit.css index.html | grep -oF '@media(min-width:96rem){.card{display:flex}}'
  @media(min-width:96rem){.card{display:flex}}

An [in-*] variant heads the selector with the ancestor's class, so the utility's
own class is not the leftmost one. It is picked out by name, and the ancestor's
class stays a class:

  $ cat > ancestor.css <<EOF
  > @import "tailwindcss" theme(static);
  > .item { @apply in-[.group]:flex; }
  > EOF
  $ tw --minify --input-css ancestor.css index.html | grep -oF ':where(.group) .item{display:flex}'
  :where(.group) .item{display:flex}

A [group-*] variant names the group's class beside the utility's own, and only
the utility's is nested:

  $ cat > group.css <<EOF
  > @import "tailwindcss" theme(static);
  > .row { @apply group-hover:flex; }
  > EOF
  $ tw --minify --input-css group.css index.html | grep -oF '.row:is(:where(.group):hover *){display:flex}'
  .row:is(:where(.group):hover *){display:flex}
