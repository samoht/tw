#!/usr/bin/env bash
# Benchmark `tw` vs `tailwindcss` on a source tree.
#
# Usage:
#   scripts/bench.sh <source-dir> [runs]
#
# Example:
#   scripts/bench.sh ~/git/ocaml.org/src/ocamlorg_frontend
#   scripts/bench.sh ~/my/app/src 20
#
# Both tools produce a variables+base stylesheet with optimize and minify
# enabled, so they do comparable work (parse files, detect classes, emit
# CSS, optimise, minify). Timings are wall-clock, taken with python
# time.perf_counter() for sub-ms resolution.
#
# tailwindcss runs in an isolated dir with `@import "tailwindcss" source(none)`
# so its auto-detection doesn't pick up stray files in cwd (previous outputs,
# node_modules, etc.) and pollute the measurement.
#
# After timing, we run the repo's `cssdiff` tool to verify how similar the
# two generated stylesheets are.
#
# Artefacts go under tmp/bench/ (never /tmp).

set -eu

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
BENCH="$ROOT/tmp/bench"
TL_WORK="$BENCH/tailwindcss-work"
mkdir -p "$BENCH" "$TL_WORK"

SRC="${1:-}"
RUNS="${2:-5}"

if [ -z "$SRC" ] || [ ! -d "$SRC" ]; then
  echo "usage: $0 <source-dir> [runs]" >&2
  exit 1
fi
SRC="$(cd "$SRC" && pwd)"

TW_BIN="$ROOT/_build/install/default/bin/tw"
if [ ! -x "$TW_BIN" ]; then
  (cd "$ROOT" && dune build) >/dev/null
fi
TAILWIND_BIN="${TAILWIND_BIN:-$(command -v tailwindcss || true)}"
CSSDIFF_BIN="${CSSDIFF_BIN:-$(command -v cssdiff || true)}"
if [ -z "$CSSDIFF_BIN" ] && [ -x "$ROOT/_opam/bin/cssdiff" ]; then
  CSSDIFF_BIN="$ROOT/_opam/bin/cssdiff"
fi

if [ ! -x "$TW_BIN" ]; then
  echo "tw binary not found at $TW_BIN" >&2; exit 1
fi
if [ -z "$TAILWIND_BIN" ] || [ ! -x "$TAILWIND_BIN" ]; then
  echo "tailwindcss not found on PATH (set TAILWIND_BIN to override)" >&2
  exit 1
fi

# tailwindcss input: disable cwd auto-scan, point only at SRC.
TW_INPUT="$TL_WORK/input.css"
cat > "$TW_INPUT" <<EOF
@import "tailwindcss" source(none);
@source "$SRC";
EOF

TW_OUT="$BENCH/out.tw.css"
TAILWIND_OUT="$BENCH/out.tailwind.css"

# Warm-up (not counted): paging, fs cache, node JIT.
"$TW_BIN" "$SRC" --variables --base --optimize --minify -q > "$TW_OUT" 2>/dev/null || true
(cd "$TL_WORK" && "$TAILWIND_BIN" -i input.css -o "$TAILWIND_OUT" --minify --optimize) >/dev/null 2>&1 || true

run_timed () {
  local label="$1"; shift
  python3 - "$label" "$RUNS" "$@" <<'PY'
import subprocess, sys, time
label, runs, *cmd = sys.argv[1:]
runs = int(runs)
ts = []
for _ in range(runs):
    t0 = time.perf_counter()
    r = subprocess.run(cmd, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    ts.append(time.perf_counter() - t0)
    if r.returncode != 0:
        print(f"{label}: non-zero exit {r.returncode}", file=sys.stderr)
ts.sort()
mn, md, mx = ts[0], ts[len(ts)//2], ts[-1]
mean = sum(ts)/len(ts)
print(f"{label}\t{mn*1000:.1f}\t{md*1000:.1f}\t{mean*1000:.1f}\t{mx*1000:.1f}")
PY
}

# tailwindcss must run from $TL_WORK (so `tailwindcss` npm resolves and we
# control auto-scan). Wrap it in a small script for subprocess-based timing.
TL_RUN="$TL_WORK/run.sh"
cat > "$TL_RUN" <<EOF
#!/usr/bin/env bash
cd "$TL_WORK"
exec "$TAILWIND_BIN" -i input.css -o "$TAILWIND_OUT" --minify --optimize
EOF
chmod +x "$TL_RUN"

echo "Source:  $SRC"
FILES=$(find "$SRC" -type f \( -name "*.html" -o -name "*.eml" -o -name "*.ml" -o -name "*.re" -o -name "*.jsx" -o -name "*.tsx" -o -name "*.vue" -o -name "*.svelte" \) | wc -l | tr -d ' ')
echo "Scanned: $FILES files"
echo "Runs:    $RUNS (warm-up excluded)"
echo
printf "| tool         | min (ms) | median | mean  | max   | output bytes |\n"
printf "|--------------|---------:|-------:|------:|------:|-------------:|\n"

TW_LINE=$(run_timed "tw" "$TW_BIN" "$SRC" --variables --base --optimize --minify -q)
TAILWIND_LINE=$(run_timed "tailwindcss" "$TL_RUN")

TW_SIZE=$(wc -c < "$TW_OUT" | tr -d ' ')
TAILWIND_SIZE=$(wc -c < "$TAILWIND_OUT" | tr -d ' ')

fmt_row () {
  local line="$1" size="$2"
  awk -v sz="$size" -F'\t' '{ printf "| %-12s | %8s | %6s | %5s | %5s | %12s |\n", $1, $2, $3, $4, $5, sz }' <<<"$line"
}

fmt_row "$TW_LINE" "$TW_SIZE"
fmt_row "$TAILWIND_LINE" "$TAILWIND_SIZE"

echo
echo "## Output similarity (cssdiff)"
echo
if [ -z "$CSSDIFF_BIN" ] || [ ! -x "$CSSDIFF_BIN" ]; then
  echo "cssdiff not found; skipping structural comparison." >&2
else
  # Summary: exit 0 means identical, non-zero means differ.
  # Show the first 40 lines of output so the terminal doesn't get flooded.
  set +e
  NO_COLOR=1 "$CSSDIFF_BIN" --color=never "$TAILWIND_OUT" "$TW_OUT" > "$BENCH/cssdiff.out" 2>&1
  rc=$?
  set -e
  if [ $rc -eq 0 ]; then
    echo "cssdiff: outputs are identical."
  else
    echo "cssdiff: outputs differ (first 40 lines):"
    head -40 "$BENCH/cssdiff.out" | sed 's/^/    /'
    total=$(wc -l < "$BENCH/cssdiff.out" | tr -d ' ')
    echo "    ... ($total lines total in $BENCH/cssdiff.out)"
  fi
fi

echo
echo "## Artefacts"
echo "  tw          -> $TW_OUT"
echo "  tailwindcss -> $TAILWIND_OUT"
echo "  cssdiff     -> $BENCH/cssdiff.out"
echo "  rerun diff  : cssdiff $TAILWIND_OUT $TW_OUT | less -R"
