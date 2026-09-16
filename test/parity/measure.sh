#!/bin/sh
# Whole-site parity measurement: tw against Tailwind v4 over the class list of
# tailwindcss.com. Runs the commands docs/parity.md documents over the inputs
# committed beside this script and prints what they print.
#
# The contract has two halves. Parity is what a browser renders: the canonical
# diff between tw's sheet and Tailwind's compiled output reports nothing. The
# reference is the compiled sheet, before lightningcss, so what the report
# lists is tw against Tailwind and not cascade against another minifier.
# Separately, tw's minified sheet is never larger than Tailwind's minified one,
# which is the one figure the minified reference is built for.
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
#
# TW_PARITY_RENDER="FIRST COUNT" also renders the COUNT classes from index FIRST
# of classlist.txt in a headless Chromium, under both sheets, and prints the
# browser's report after the canonical one. The page comes from site_page.exe,
# and both sheets are pruned to it first with `cascade prune`: a rule no element
# of the page matches cannot change what the page computes, and without the
# pruning every width and state the whole sheet names is sampled on every
# element. A shard of 50 classes takes minutes, so the whole list is rendered by
# hand, shard by shard, and not in CI.

set -e

here=$(cd "$(dirname "$0")" && pwd)
root=$(cd "$here/../.." && pwd)
out=${TW_PARITY_OUT:-$root/tmp/parity}
export LC_ALL=C

mkdir -p "$out"
dune build --root "$root" bin/main.exe cascade/bin/main.exe \
  test/parity/site_page.exe

"$root"/node_modules/.bin/tailwindcss \
  -i "$here"/ref-entry.css -o "$out"/ref.css
"$root"/node_modules/.bin/tailwindcss \
  -i "$here"/ref-entry.css -o "$out"/ref_local.css --minify

"$root"/_build/default/bin/main.exe \
  --input-css "$here"/globals.css --minify "$here"/classlist.txt > "$out"/tw_all.css

tw_bytes=$(wc -c < "$out"/tw_all.css | tr -d ' ')
ref_bytes=$(wc -c < "$out"/ref_local.css | tr -d ' ')
echo "minified: tw $tw_bytes bytes, tailwindcss $ref_bytes bytes"
if [ "$tw_bytes" -gt "$ref_bytes" ]; then
  echo "tw --minify is larger than tailwindcss --minify" >&2
  exit 1
fi

# The differ's exit status carries three measurements and one failure.
#
#   0  identical, and every declaration was readable
#   2  identical in what could be read, but a side held a declaration the
#      reader refused, so equivalence is undetermined rather than established
#   1  different
#
# 2 is not a crash and not a pass. An equivalence checker that answered 0 here
# would be claiming a property it cannot prove: a refused declaration is absent
# from both parsed sheets, so two sheets differing only inside one compare
# equal. tw's own sheet carries such declarations today -- the `<value>`
# placeholder classes the corpus scrapes from the docs -- so this fires on a
# clean run and must not read as a differ failure.
#
# Anything above 2 is the differ failing -- an unusable argument, an input it
# could not read at all, a crash -- which writes a short or empty report that
# reads as parity. Propagate that instead.
status=0
"$root"/_build/default/cascade/bin/main.exe \
  diff --diff=canonical --limit=none "$out"/tw_all.css "$out"/ref.css \
  > "$out"/diff.txt 2>&1 || status=$?

if [ "$status" -gt 2 ]; then
  cat "$out"/diff.txt
  echo "cascade diff failed with status $status" >&2
  exit "$status"
fi

if [ "$status" -eq 2 ]; then
  echo "cascade diff could not read every declaration; equivalence is" \
       "undetermined rather than established" >&2
fi

cat "$out"/diff.txt

echo
echo "top-level entries of $out/diff.txt:"
grep -nE "^├─|^└─" "$out"/diff.txt || echo "  (none)"

[ -n "${TW_PARITY_RENDER:-}" ] || exit 0

# shellcheck disable=SC2086 # FIRST and COUNT are two words on purpose.
set -- $TW_PARITY_RENDER
cascade="$root"/_build/default/cascade/bin/main.exe
"$root"/_build/default/test/parity/site_page.exe "$here"/classlist.txt "$1" "$2" \
  > "$out"/page.html
"$cascade" prune "$out"/page.html "$out"/tw_all.css > "$out"/tw_page.css \
  2> "$out"/prune.txt
"$cascade" prune "$out"/page.html "$out"/ref.css > "$out"/ref_page.css \
  2>> "$out"/prune.txt

echo
echo "browser render of classes $1 to $(($1 + $2 - 1)):"
render=0
"$cascade" diff --browser --html "$out"/page.html \
  "$out"/ref_page.css "$out"/tw_page.css > "$out"/render.txt 2>&1 || render=$?
cat "$out"/render.txt
# 1 is a measurement, the differences it lists; anything else is no render.
if [ "$render" -gt 1 ]; then
  echo "the browser render failed with status $render" >&2
  exit "$render"
fi
