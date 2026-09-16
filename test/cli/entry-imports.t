The reference `--tailwind` and `--diff` compile is the project's entrypoint
written into a scratch directory beside the scanned sources, so a relative path
in it has to be rooted at the entrypoint's own directory, or it names a file
that is not there. A package name is not a path and stays as written. A stub
stands in for the CLI and hands back the entrypoint it was given:

  $ mkdir -p node_modules/.bin css
  $ cat > node_modules/.bin/tailwindcss <<'EOF'
  > #!/bin/sh
  > in=input.css
  > out=output.css
  > while [ $# -gt 0 ]; do
  >   case "$1" in
  >     -i) in=$2; shift 2 ;;
  >     -o) out=$2; shift 2 ;;
  >     *) shift ;;
  >   esac
  > done
  > if [ "$out" = "-" ]; then
  >   echo "/*! tailwindcss v4.3.3 | MIT License */"
  >   exit 0
  > fi
  > if [ -n "$TW_STUB_FAIL" ]; then
  >   echo "Error: $TW_STUB_FAIL" >&2
  >   exit 1
  > fi
  > cat "$in" > "$out"
  > EOF
  $ chmod +x node_modules/.bin/tailwindcss

  $ cat > css/app.css <<EOF
  > @import "tailwindcss" source(none);
  > @import "./extra.css" layer(components);
  > @source "../src";
  > @plugin "@tailwindcss/typography";
  > EOF
  $ echo '.x { color: red }' > css/extra.css
  $ echo '<div class="p-4"></div>' > page.html

  $ tw --tailwind --input-css css/app.css page.html > reference.css
  $ grep -E '^@(import|source|plugin) "' reference.css | sed "s|$(pwd -P)|ROOT|g"
  @import "tailwindcss" source(none);
  @import "ROOT/css/./extra.css" layer(components);
  @source "ROOT/css/../src";
  @plugin "@tailwindcss/typography";
  @source "./input.html";

The last line is the harness's own, naming the file it wrote beside the
entrypoint, so it stays relative to the scratch directory.

When the CLI fails, what it said is part of the error rather than lost:

  $ TW_STUB_FAIL="Can't resolve './missing.css'" tw --tailwind --input-css css/app.css page.html 2>&1 | grep -c "Can't resolve './missing.css'"
  1
