A class can parse yet fail to render, as the docs' [prop-[<value>]]
placeholders do: [animate-[<value>]] and [backdrop-blur-[<value>]] are
accepted by their handlers but raise when they render an arbitrary value
they cannot serialise. Such a class produces no rule, so it is dropped
rather than aborting the whole sheet.

  $ cat > docs.mdx <<'EOF'
  > A table of utilities:
  >   ["animate-[<value>]", "animation: <value>;"],
  >   ["backdrop-blur-[<value>]", "backdrop-filter: blur(<value>);"],
  > And a real class: <div class="p-4"></div>
  > EOF

The placeholder classes do not crash generation, and the real class
still lands in the output:

  $ tw --minify --no-base docs.mdx | grep -c '\.p-4{padding:'
  1

An arbitrary value that is valid still renders:

  $ cat > ok.html <<'EOF'
  > <div class="animate-[wiggle_1s_ease-in-out_infinite]"></div>
  > EOF
  $ tw --minify --no-base ok.html | grep -c 'animation:wiggle'
  1
