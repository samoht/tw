Documentation placeholders are safe arbitrary-value token streams. Tailwind
emits them even when the browser will reject the value for the property, so tw
must preserve them too.

  $ cat > docs.mdx <<'EOF'
  > A table of utilities:
  >   ["animate-[<value>]", "animation: <value>;"],
  >   ["backdrop-blur-[<value>]", "backdrop-filter: blur(<value>);"],
  > And a real class: <div class="p-4"></div>
  > EOF

The placeholder classes and the real class all land in the output:

  $ tw --minify --no-base docs.mdx > generated.css
  $ grep -c '\.animate-.*{animation:<value>}' generated.css
  1
  $ grep -c '\.backdrop-blur-.*--tw-backdrop-blur:blur(<value>)' generated.css
  1
  $ grep -c '\.p-4{padding:' generated.css
  1

An arbitrary value that is valid still renders:

  $ cat > ok.html <<'EOF'
  > <div class="animate-[wiggle_1s_ease-in-out_infinite]"></div>
  > EOF
  $ tw --minify --no-base ok.html | grep -c 'animation:wiggle'
  1
