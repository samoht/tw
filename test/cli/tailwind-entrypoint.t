The Tailwind backend generates against the project's own entrypoint, so a
[--tailwind] run over files reads the same [@theme] the native run does.
A stub CLI stands in for the real one: it echoes the entrypoint it was given,
keeps the arguments of its last compile in [last-args.txt], and answers the
identification probe, which asks for the banner on stdout and leaves a copy of
whatever it was asked to compile. tw takes the CLI from the
nearest [node_modules] above the working directory, so the stub goes there; on
[PATH] the installed one still wins and the stub never runs.

  $ mkdir -p node_modules/.bin
  $ cat > node_modules/.bin/tailwindcss <<'EOF'
  > #!/bin/sh
  > in=input.css
  > out=output.css
  > args="$*"
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
  > echo "$args" > last-args.txt
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

A run that finds differences says so in its exit status, so [--diff] can gate a
CI job: 1 when the two sheets differ, 0 when they do not, as above.

  $ sed 's/border-color:red/border-color:blue/' expected.css > different.css
  $ TW_STUB_CSS="$PWD/different.css" tw --diff --input-css decl.css decl.html > diff.txt
  [1]
  $ grep -c 'Differences found' diff.txt
  1
  $ TW_STUB_CSS="$PWD/different.css" tw --diff -s flex > single.txt
  [1]
  $ grep -c 'Differences found' single.txt
  1

The reference is Tailwind's sheet before lightningcss minifies it, so a
difference [--diff] reports is always tw's: the CLI is asked to minify only
when the run asks for it, and both sheets are then minified, each by its own
tool.

  $ tw -s flex --variables --base --minify > flex.css
  $ TW_STUB_CSS="$PWD/flex.css" tw --diff -s flex
  ✓ No differences found between Tailwind and tw for 'flex'
  $ grep -c -- --minify last-args.txt
  0
  [1]
  $ TW_STUB_CSS="$PWD/flex.css" tw --diff --minify -s flex
  ✓ No differences found between Tailwind and tw for 'flex'
  $ grep -c -- --minify last-args.txt
  1
