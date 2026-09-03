#!/bin/sh
# Whole-site parity measurement: tw against Tailwind v4 over the class list of
# tailwindcss.com. Runs the three commands docs/parity.md documents over the
# inputs committed beside this script and prints what they print.
#
# It derives no counts of its own. The figure to quote is the differ's summary
# line together with the top-level entries listed under it: the summary counts
# containers rather than their contents, so `@layer utilities (47 added)` is one
# container hiding 47 rules.
#
# The reference is built from ref-entry.css, not globals.css. ref-entry.css pins
# `source(none)` plus an explicit `@source "./classlist.txt"`; without that
# Tailwind v4 auto-scans the whole repository, picks up tw's own output, and the
# comparison goes circular.
#
# Both binaries come from the workspace build, never from PATH: an installed
# `cascade` can be months old and invents differences that do not exist. The
# `dune build` below is what keeps that from happening silently.

set -e

here=$(cd "$(dirname "$0")" && pwd)
root=$(cd "$here/../.." && pwd)
out=${TW_PARITY_OUT:-$root/tmp/parity}
export LC_ALL=C

mkdir -p "$out"
dune build --root "$root" bin/main.exe cascade/bin/main.exe

"$root"/node_modules/.bin/tailwindcss \
  -i "$here"/ref-entry.css -o "$out"/ref_local.css --minify

"$root"/_build/default/bin/main.exe \
  --input-css "$here"/globals.css --minify "$here"/classlist.txt > "$out"/tw_all.css

# The differ exits 0 when the sheets are identical and 1 when they differ, and
# both are measurements. Any other status is the differ failing -- an unusable
# argument, an input it could not read, a crash -- which writes a short or empty
# report that reads as parity. Propagate it instead.
status=0
"$root"/_build/default/cascade/bin/main.exe \
  diff --diff=canonical --limit=none "$out"/tw_all.css "$out"/ref_local.css \
  > "$out"/diff.txt 2>&1 || status=$?

if [ "$status" -gt 1 ]; then
  cat "$out"/diff.txt
  echo "cascade diff failed with status $status" >&2
  exit "$status"
fi

cat "$out"/diff.txt

echo
echo "top-level entries of $out/diff.txt:"
grep -nE "^├─|^└─" "$out"/diff.txt || echo "  (none)"
