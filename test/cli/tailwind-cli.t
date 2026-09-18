tw takes the flags of Tailwind's own CLI, so a build script written for
`tailwindcss` runs unchanged with `tw` in its place.

  $ mkdir -p src
  $ cat > src/app.css <<EOF
  > @import "tailwindcss";
  > EOF
  $ cat > index.html <<EOF
  > <div class="p-4 underline"></div>
  > EOF

`-i` names the entrypoint and `-o` the file written, its directory created if
need be. With no path on the command line, the sources are detected from the
working directory, as Tailwind detects them:

  $ tw -i src/app.css -o dist/css/app.css
  $ grep -o '\.underline {' dist/css/app.css
  .underline {

The long forms read the same, `-o -` is standard output, and `-m` minifies:

  $ tw --input src/app.css --output - -m | grep -o '\.p-4{[^}]*}'
  .p-4{padding:calc(var(--spacing)*4)}

An entrypoint that turns detection off scans nothing it does not name:

  $ cat > src/none.css <<EOF
  > @import "tailwindcss" source(none);
  > EOF
  $ tw -i src/none.css -m | grep -c '\.underline{'
  0
  [1]

`source("dir")` on the import moves the base detection starts from, relative
to the stylesheet:

  $ mkdir -p pages
  $ echo '<p class="italic"></p>' > pages/a.html
  $ cat > src/pages.css <<EOF
  > @import "tailwindcss" source("../pages");
  > EOF
  $ tw -i src/pages.css -m | grep -o '\.italic{[^}]*}'
  .italic{font-style:italic}
  $ tw -i src/pages.css -m | grep -c '\.underline{'
  0
  [1]

`--cwd` runs from another directory: `-i` and `-o` are read against it, and
detection starts there.

  $ mkdir -p site/src
  $ echo '<p class="uppercase"></p>' > site/page.html
  $ cp src/app.css site/src/app.css
  $ tw --cwd site -i src/app.css -o out.css
  $ grep -o '\.uppercase {' site/out.css
  .uppercase {
  $ grep -c '\.underline {' site/out.css
  0
  [1]

A stylesheet given as a path to scan is refused, since scanning it as markup
finds nothing:

  $ tw src/app.css
  tw: src/app.css is a stylesheet, not markup to scan; pass it with -i to use
      it as the entrypoint
  [124]

`--watch` builds, then rebuilds on change until standard input closes, so a
closed one builds once and exits:

  $ tw -i src/app.css -o watched.css --watch < /dev/null
  $ grep -c '\.underline {' watched.css
  1
