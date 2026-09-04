Backend selectors are mutually exclusive. Asking for both is an input error,
not an implicit preference for one of them:

  $ tw -s flex --tailwind --diff 2>&1 | grep -c 'mutually exclusive'
  1

Optimization must preserve public custom-property declarations. They are a
runtime API even when no declaration in the generated sheet reads them:

  $ cat > custom-property.html <<EOF
  > <div class="[--anchor-gap:8px]"></div>
  > EOF
  $ tw --no-base --minify --optimize custom-property.html | grep -o -- '--anchor-gap:8px'
  --anchor-gap:8px

Authored CSS gets the same compatibility prefixes as generated utilities,
even when the rest of the optimizer is disabled:

  $ tw --quiet --no-base --minify --input-css prefixes.css prefixes.html
  .a{-webkit-user-select:none;user-select:none}@supports(-webkit-backdrop-filter:blur(0))or (backdrop-filter:blur(0)){.b{-webkit-backdrop-filter:blur(1px);backdrop-filter:blur(1px)}}

The Tailwind backend generates against the project's own entrypoint, so a
[--tailwind] run over files reads the same [@theme] the native run does.
A stub CLI stands in for the real one: it echoes the entrypoint it was given,
and answers the identification probe, which asks for the banner on stdout and
leaves a copy of whatever it was asked to compile. tw takes the CLI from the
nearest [node_modules] above the working directory, so the stub goes there; on
[PATH] the installed one still wins and the stub never runs.

  $ mkdir -p node_modules/.bin
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
  >   cp "$in" probe-entrypoint.css
  >   echo "/*! tailwindcss v4.3.3 | MIT License */"
  >   exit 0
  > fi
  > if [ -n "$TW_STUB_CSS" ]; then cat "$TW_STUB_CSS" > "$out"
  > else cat "$in" > "$out"; fi
  > EOF
  $ chmod +x node_modules/.bin/tailwindcss

  $ cat > app.css <<EOF
  > @import "tailwindcss";
  > @theme { --text-huge: 9rem; }
  > EOF
  $ cat > index.html <<EOF
  > <div class="text-huge"></div>
  > EOF

  $ tw --tailwind --input-css app.css index.html | grep -c -- '--text-huge: 9rem;'
  1

The single-class path already did:

  $ tw --tailwind --input-css app.css -s text-huge | grep -c -- '--text-huge: 9rem;'
  1

Identifying the CLI must cost the same from any directory. The pinned CLI has
no version flag: [--version] compiles a stylesheet like any other run, and with
no entrypoint of its own that means scanning the current directory for classes,
which under dune is the whole build tree. The probe names an entrypoint that
turns source detection off, so it reads nothing around it:

  $ cat probe-entrypoint.css
  @import "tailwindcss" source(none);

The sheet [--diff] compares is the sheet tw generates. A project's own
[@utility] declarations and its entrypoint belong to both, so handing the
stub tw's own output must come back with nothing to report:

  $ cat > decl.css <<EOF
  > @import "tailwindcss";
  > @utility line-t {
  >   @apply border-t;
  >   border-color: red;
  > }
  > .page { display: grid }
  > EOF
  $ cat > decl.html <<EOF
  > <div class="line-t flex"></div>
  > EOF

  $ tw --minify --input-css decl.css decl.html > expected.css
  $ grep -c '\.line-t{border-color:red}' expected.css
  1
  $ TW_STUB_CSS="$PWD/expected.css" tw --diff --input-css decl.css decl.html
  ✓ No differences found
