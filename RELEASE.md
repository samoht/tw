# tw release criteria

The bar a tagged release must clear. tw's contract is byte-parity with the
Tailwind CSS v4 compiler for the utilities and plugins it claims, so most gates
compare against the real tool rather than against a fixture we wrote.

Reproduce every measurement from a worktree with the freshly built binaries.
An installed `tw` on `PATH` can be months old and will invent differences that
do not exist.

## Correctness (hard gates - all must pass)

1. **Suites.** `dune exec test/test.exe`, `dune exec test/upstream/test.exe`,
   and `dune exec test/tools/test.exe` all green. The upstream suite is strict
   by default; there is no tolerance switch to forget.
2. **Rendering.** `TW_BROWSER_TESTS=1` set, so a missing Chromium fails rather
   than skips. Without it the eight suites calling `check_rendering_matches`
   report no difference because they never looked - which is what CI did until
   #513.
3. **Tailwind oracle.** `TW_TAILWIND_TESTS=1` set, so a missing CLI, or one
   whose version differs from `Tailwind_gen.required_version`, fails rather
   than skips. Without it 159 parity tests skip and the run still says
   "Test Successful"; the skip lines never reach a `dune runtest` log.
4. **Doc examples and cram.** `dune build @runtest`. It compiles the MDX
   examples in the `.mli` files and the README, and runs `test/cli/*.t`, which
   covers entrypoint behaviour no Alcotest suite reaches. Read the HEAD of its
   output: a cram diff prints first and `| tail` hides it.
5. **Sort fuzzer.** Clean across several seeds
   (`TEST_SEED=<n> dune exec test/test.exe -- test sort`). It is authoritative;
   a case it reports is a real ordering bug, never something to skip.
6. **Gates.** `dune build`, `dune build @fmt`, and `merlint` clean.

## Parity (the contract)

7. **Upstream corpus.** `test/upstream/` replays Tailwind's own fixtures and
   must pass without an allowlist. The suite may not take an expected value
   from Tailwind's output and hand it back to tw - breaking a built-in default
   has to fail it (see #512).
8. **Whole-site measurement.** `sh test/parity/measure.sh` runs tw against
   Tailwind over the class list of tailwindcss.com. Quote the differ's summary
   line together with the top-level entries under it; the summary counts
   containers rather than their contents, so one `@layer` entry can hide a
   thousand rules. Last measured 2026-09-02: 0.3% diff, 1 removed rule,
   8 modified, 20 reordered, 5 changed containers.

## Quality (target, non-blocking)

9. **No partial function reachable from a class.** `lib/` still holds
   `failwith`/`invalid_arg` sites; none is reachable from ordinary input
   (the corpus runs `of_string` then `to_css` over 6491 classes and raises on
   none). Track the count down rather than gate on it.
10. **No private cascade module.** tw must compile against cascade as an
   *installed* library, not only against the source tree beside it. A name from
   a module in `cascade/lib/dune`'s `private_modules` resolves locally and fails
   CI; check the public `.mli` before using one.

## Hygiene

11. **Changelog + version.** A `CHANGES.md` entry for the version, every `#N`
    resolving to a merged PR, and the tag on the current `main` lineage.
12. **Cascade bound.** `dune-project` and `tw.opam` name a cascade version CI
    can resolve. CI pins cascade's `main` branch; a local build compiles the
    sibling checkout from source, so confirm both are on the same revision
    before reading a difference as a tw regression.
