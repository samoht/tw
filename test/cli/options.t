Backend selectors are mutually exclusive. Asking for both is an input error,
not an implicit preference for one of them:

  $ tw -s flex --tailwind --diff 2>&1 | grep -c 'mutually exclusive'
  1

The Tailwind backend generates against the project's own entrypoint, so a
[--tailwind] run over files reads the same [@theme] the native run does.
A stub CLI stands in for the real one and echoes the entrypoint it was given:

  $ mkdir -p stub
  $ cat > stub/tailwindcss <<'EOF'
  > #!/bin/sh
  > if [ "$1" = "--version" ]; then echo "tailwindcss v4.3.3"; exit 0; fi
  > in=input.css
  > out=output.css
  > while [ $# -gt 0 ]; do
  >   case "$1" in
  >     -i) in=$2; shift 2 ;;
  >     -o) out=$2; shift 2 ;;
  >     *) shift ;;
  >   esac
  > done
  > cat "$in" > "$out"
  > EOF
  $ chmod +x stub/tailwindcss
  $ export PATH="$PWD/stub:$PATH"

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
