`--tailwind` and `--diff` hand the Tailwind CLI an entrypoint tw writes, and
the CLI resolves its `@import "tailwindcss"` against the nearest
`node_modules` above that file, so the file has to sit inside the project.
It must not leave anything behind there.

A binary built in a checkout carrying the pinned CLI keeps its scratch files
in that checkout's `tmp/`, and only an installed one writes into the caller's
project. The test runs inside the build tree, so the binary and the projects
it runs in are copied out of it, to a directory of their own under the
system's temporary one. A stub CLI stands in for the real one; it logs the
entrypoint it is given and answers the version probe.

  $ root=$(mktemp -d "${TMPDIR:-/tmp}/tw-scratch.XXXXXX")
  $ mkdir -p "$root/inst/bin"
  $ cp "$(command -v tw)" "$root/inst/bin/tw"
  $ tw="$root/inst/bin/tw"
  $ cat > "$root/stub" <<'SH'
  > #!/bin/sh
  > in=; out=
  > while [ $# -gt 0 ]; do
  >   case "$1" in
  >     -i) in=$2; shift 2 ;;
  >     -o) out=$2; shift 2 ;;
  >     *) shift ;;
  >   esac
  > done
  > echo "$in" >> "$TW_STUB_LOG"
  > if [ "$out" = "-" ]; then
  >   echo "/*! tailwindcss v4.3.3 | MIT License */"
  >   exit 0
  > fi
  > cat "$in" > "$out"
  > SH
  $ chmod +x "$root/stub"
  $ export TW_STUB_LOG="$root/log"

In a project with a `node_modules`, the scratch files go under its
`.cache/tw`, the directory JavaScript tools keep such files in, and tw removes
what it created when it exits:

  $ mkdir -p "$root/proj/node_modules/.bin"
  $ cp "$root/stub" "$root/proj/node_modules/.bin/tailwindcss"
  $ "$tw" --cwd "$root/proj" --tailwind -s flex | grep -c 'inline("flex")'
  1
  $ grep -c '/proj/node_modules/\.cache/tw/' "$root/log"
  2
  $ ls -A "$root/proj"
  node_modules
  $ ls -A "$root/proj/node_modules"
  .bin

A `.cache` another tool made is left in place, and only tw's own directory
goes:

  $ mkdir -p "$root/proj/node_modules/.cache/other"
  $ "$tw" --cwd "$root/proj" --tailwind -s flex > /dev/null
  $ ls -A "$root/proj/node_modules/.cache"
  other

With no `node_modules` above the working directory there is no project tree to
satisfy, and the files go under a hidden `.tw-scratch` there, removed the same
way:

  $ rm "$root/log"
  $ mkdir -p "$root/bare" "$root/stubbin"
  $ cp "$root/stub" "$root/stubbin/tailwindcss"
  $ PATH="$root/stubbin:$PATH" "$tw" --cwd "$root/bare" --tailwind -s flex | grep -c 'inline("flex")'
  1
  $ grep -c '/bare/\.tw-scratch/' "$root/log"
  2
  $ ls -A "$root/bare"

  $ rm -rf "$root"
