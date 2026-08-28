# OCaml × Tailwind v4 — Contributor Guide (for Claude Code)

## 1) Purpose

This repo provides a type-safe Tailwind v4 implementation in OCaml. Utilities are compiled to CSS through a strongly-typed variable system and layered rules that mirror Tailwind's pipeline. The goal: **no raw `"var(--...)"` strings; no assembled CSS token strings; spec-faithful output.**

### Scope

All core Tailwind v4 utilities and all official plugins are in scope, including:
- `@tailwindcss/forms` — **fully supported**
- `@tailwindcss/typography` — **fully supported**

---

## 2) Core principles

1. **Type safety over strings.** Never write `"var(--name)"` and never hand `Css.custom_property` an assembled token string; represent variables and properties via `Var` and typed constructors. **NEVER add `Raw of string` or similar escape hatches to CSS types** — if a pattern cannot be expressed with existing types, extend the type system properly.
2. **Spec-driven tests.** CSS behaviour is tested against MDN/W3C where applicable; utilities against Tailwind v4 output.
3. **Variables follow four patterns.** See patterns below and `docs/adding-a-new-utility.md`.
4. **Respect layers.** `properties → theme → base → components → utilities`. Utilities must not leak into theme or properties.

---

## 3) Project structure

```
lib/           core (utilities, class parsing, layer assembly)
  var.ml       variable system (critical)
  utility.ml   utility registry and the module shape every family follows
  rule.ml      selector routing for variants
  sort.ml      cascade order inside @layer utilities
  build.ml     layer assembly
  tools/       source scanning and the tailwindcss shell-out
test/          Alcotest suites, one test_<module>.ml per lib/<module>.ml
  upstream/    replay of Tailwind's own fixture corpus
cascade/       sibling repo, symlinked in: CSS AST, printer, optimizer, differ
docs/          adding-a-new-utility.md (start here for new utils)
               parity.md (how Tailwind parity is measured and read)
```

The CSS type system lives in cascade, not here. `Cascade.Css` is aliased as
`Css` at the top of each `lib/` module.

Keep examples small and targeted; each test file should focus on one concept.

---

## 4) Quick start (build, run, compare)

<!-- $MDX skip -->
```bash
# Build
dune build

# Run all tests (verbose helps when debugging mismatches)
ALCOTEST_VERBOSE=1 dune exec test/test.exe

# Generate CSS for a snippet (variables only, then with base)
dune exec -- tw -s "p-4" --variables
dune exec -- tw -s "p-4" --variables --base

# For classes starting with -, use --single="..." to avoid CLI flag parsing
dune exec -- tw --single="-mt-4" --variables

# Compare with real Tailwind CSS
## Method 1: Direct comparison using --diff (recommended)
dune exec -- tw -s "p-4" --diff

## Method 2: Generate with real Tailwind via --tailwind
dune exec -- tw -s "p-4" --tailwind
```

The negative-class form, run against a `tw` already on `PATH`:

```sh
$ tw --single="-mt-4" --variables
@layer theme, components, utilities;
@layer theme {
  :root, :host {
    --spacing: .25rem;
  }
}
@layer components;
@layer utilities {
  .-mt-4 {
    margin-top: calc(var(--spacing) * -4);
  }
}
```

### Backend modes (mutually exclusive)

The `tw` binary supports three backend modes:

- **Native** (default): Our OCaml implementation
- **`--tailwind`**: Use real tailwindcss tool to generate CSS
- **`--diff`**: Compare our output with real Tailwind and show differences

**Important**: `--diff` mode always uses:
- Variables mode (ignores `--inline` flag)
- Base layer included (ignores `--no-base` flag)
- Minified output (forces `--minify`; `--optimize` is passed through as given)
- Canonical comparison with unused custom properties pruned, so the two sheets
  are compared for equivalence rather than byte-for-byte. `--diff-mode` selects
  a stricter mode (`tree` reports regrouping, `string` reports text).

Example diff output:
<!-- $MDX skip -->
```bash
$ dune exec -- tw --single="supports-[backdrop-filter]:bg-black/50" --diff
Differences found between Tailwind and tw for 'supports-[backdrop-filter]:bg-black/50':

--- Tailwind
+++ tw
└─ @layer utilities (1 removed)
   ├─ .supports-\[backdrop-filter\]\:bg-black\/50
   │     - background-color color-mix(in oklab, var(--color-black) 50%, #0000)
   └─ @supports (backdrop-filter: var(--tw)) (added)
      └─ .supports-\[backdrop-filter\]\:bg-black\/50 (added)
```

That one is a known minifier difference, not a tw bug; `docs/parity.md` lists
the residual the site comparison tolerates.

> Note: Use `tmp/` (repo-local) for debug artefacts; do **not** use `/tmp`. Update any older scripts accordingly.

---

## 5) Variable system

The variable system is documented in **`lib/var.mli`**. Key points:

- **Four patterns**: `theme`, `property_default`, `channel`, `ref_only`
- Always use `Var.binding` to create declaration + reference pairs
- Never write raw `"var(--name)"` strings

See `lib/var.mli` for detailed documentation and examples.

---

## 6) Layers and rule ordering

Layer architecture is documented in **`lib/build.mli`**, the sort order in
**`lib/sort.mli`**. Key points:

- **Layer order**: `properties → theme → base → components → utilities`
- **Utilities layer**: Sorted by (priority, suborder) for conflict resolution; each
  utility module supplies the pair through `Utility.S` (`priority`, `suborder`)
- **Media queries**: Modifier-based media comes after regular rules; built-in media stays grouped with its utility

See `lib/build.mli` for the layer model and `lib/sort.mli` for the comparator
and the fields it keys on.

---

## 7) Adding a new utility — minimal workflow

1. **Read** `docs/adding-a-new-utility.md`. Identify the variable pattern.
2. **Implement** the utility in `lib/<area>.ml` using typed `Var` and the chosen
   pattern, following the module shape in `lib/utility.mli`.
3. **Check** the layer it lands in; `lib/build.ml` assembles them from the
   utility's `(priority, suborder)` and its declarations.
4. **Test**:

   * CSS spec behaviour (if relevant) belongs in cascade, under `cascade/test/`.
   * Tailwind parity under `test/`, in the `test_<module>.ml` matching the
     `lib/<module>.ml` you touched. Use a *single* concept per file.
5. **Compare** against Tailwind:

   ```bash
   # Quick comparison for any class
   dune exec -- tw -s "border-2 border-solid" --diff

   # Or for test files
   ALCOTEST_VERBOSE=1 dune exec test/test.exe -- test borders 10
   ```

---

## 8) Debugging checklist

* **Output differs from Tailwind?**

  ```bash
  # Quick comparison showing exact differences
  dune exec -- tw -s "your-class" --diff
  ```

  This shows structural CSS differences between our implementation and real Tailwind.

* **Wrong layer?**

  ```bash
  dune exec -- tw -s "border-solid" --variables | grep -A5 "@layer"
  ```

  Confirm constructor ↔ pattern ↔ layer alignment.

* **Missing `@property`?**

  ```bash
  dune exec -- tw -s "font-bold" --variables | grep "@property"
  ```

  Ensure `~property_rules` is passed and `Var.property_rule` invoked.

* **Unexpected `@property` being emitted?**

  `@property` rules are only generated for variables that are **SET** (via `Var.binding` creating
  a Custom_declaration), not for variables that are merely **referenced** with a fallback.

  Example: `text-2xl` uses `line-height: var(--tw-leading, var(--text-2xl--line-height))` but
  does NOT emit `@property --tw-leading` because it only references the variable with a fallback.
  However, `leading-relaxed` sets `--tw-leading: var(--leading-relaxed)` and DOES emit `@property`.

  The logic is in `collect_all_property_rules` in `lib/build.ml` which filters by `set_var_names`
  extracted from Custom_declarations.

* **Wrong property order in `@layer properties`?**

  A variable's slot comes from the `~property_order` argument it was declared
  with (`Var.property_default` or `Var.channel`), read back through
  `Var.property_order`. `sort_properties_by_order` in `lib/build.ml` combines it
  with `canonical_property_rank`, which ranks a `--tw-*` variable by the CSS
  property it feeds: transform 16, background-image 28, line-height 39,
  font-weight 40, letter-spacing 41, box-shadow 51, filter 53, transition 56,
  text-shadow 59 (last), and 1000 for a variable with no family. Transform,
  gradient, duration and typography families order by first usage instead;
  `order_by_first_usage` decides which rule applies to a given pair.

* **Order mismatch in `@layer theme`?**

  ```bash
  grep "~order:" lib/typography.ml
  ```

  Theme order is a `(group, index)` pair: the group buckets the family in
  Tailwind's `@theme` order, the index places the token inside it. Match the
  neighbours of the token you are adding; `Var.order` reads the pair back and
  `lib/var.ml` refuses a second registration for an already-taken slot.

* **Targeted test run:**

  ```bash
  ALCOTEST_VERBOSE=1 dune exec test/test.exe -- test <suite> <N>
  ```

  Suite names match the `lib/` module (`borders`, `sort`, `build`, …); run
  `dune exec test/test.exe -- list` for the full set. Differ tests live in
  cascade:

  ```bash
  ALCOTEST_VERBOSE=1 dune exec cascade/test/test.exe -- test css_compare 12
  ```

* **Example test failures (e.g., `dune runtest` in `examples/prose/`):**

  When example tests fail, follow this workflow to isolate and fix the issue:

  1. **Reproduce with minimal utilities**: Use `tw -s '<utilities>' --diff` to identify the problematic ordering/generation
     ```bash
     # Start with the full set of utilities that fail
     dune exec -- tw -s 'md:grid-cols-2 max-w-4xl prose' --diff

     # Remove utilities one by one until you find the minimal set
     dune exec -- tw -s 'md:grid-cols-2 max-w-4xl' --diff
     ```

  2. **Add a unit test**: Codify the expected behaviour in the `test_<module>.ml`
     of the utility family involved, or in `test/test_sort.ml` when the bug is in
     the comparator itself
     ```ocaml
     let test_your_case () =
       let utilities = Tw.[ md [ grid_cols 2 ]; max_w_4xl ] in
       Test_helpers.check_ordering_matches
         ~test_name:"regular before media" utilities
     ```

  3. **Fix the issue**: Modify the relevant code (usually in `lib/sort.ml`)
     - Pay attention to comparison functions and ordering logic
     - Ensure symmetric comparisons negate results when arguments are swapped
     - Regular rules should come before media queries at the same priority

  4. **Verify**: Run both the unit test and the original example test
     ```bash
     ALCOTEST_VERBOSE=1 dune exec test/test.exe -- test sort <N>
     dune runtest
     ```

* **Common CSS diff patterns and fixes:**

  When `cssdiff` shows differences, here's how to interpret and fix them:

  1. **"blocks merged into 1"** - Multiple media blocks with same condition should be merged:
     ```
     @media (prefers-contrast:more) (2 blocks merged into 1)
       - Block at position 14: .contrast-more\:border-4
       - Block at position 26: .contrast-more\:text-black
       + Block at position 27: .contrast-more\:border-4, .contrast-more\:text-black
     ```
     **Fix**: Media query merging is cascade's. `merge_consecutive_media` lives in
     `cascade/lib/block.ml`, but `block` is one of cascade's `private_modules` and
     `optimize.mli` does not re-export it, so tw cannot call it: `lib/build.ml` runs no
     merge of its own, and `bin/main.ml` reaches cascade's optimizer only under
     `--optimize`. A plain `--minify` sheet therefore gets no merging at all. The ask is
     filed in cascade's TODO; until it lands, a rule sorting between two blocks is still
     the usual cause worth checking first.

  2. **"blocks at different positions"** - Media query appears at wrong location:
     ```
     @media (prefers-reduced-motion:no-preference) (1 blocks at different positions)
       - Block at position 8: .motion-safe\:animate-pulse
       + Block at position 25: .motion-safe\:animate-pulse
     ```
     **Fix**: Check `lib/sort.ml` comparison functions. The issue is usually in:
     - `compare_regular_vs_media`: Regular rules vs Media rules ordering
     - `compare_media_rules`: Media rules vs Media rules ordering
     - `extract_media_sort_key`: How media queries are sorted (responsive vs preference)

  3. **"reordered"** - Rules within a container are in wrong order:
     ```
     └─ @layer utilities (1 reordered)
        └─ .flex-col ↔  @media (min-width:48rem)
     ```
     **Fix**: This is a Regular vs Media ordering issue. Check `compare_regular_vs_media` in
     `lib/sort.ml`. It reads the rule's `has_modifier_colon` field, set from
     `Css.Selector.contains_modifier_colon` when `lib/build.ml` indexes the rule - modifier media
     (like `.md\:grid-cols-2`) comes after Regular rules, but built-in media (like container's
     breakpoints) stays grouped with its base utility via priority comparison.

  4. **Property value differences**:
     ```
     .leading-relaxed
       * --tw-leading: 1.625 -> var(--leading-relaxed)
     ```
     **Fix**: Check if we're using the right variable pattern. Tailwind v4 often uses
     theme variables like `var(--leading-relaxed)` instead of raw values.

  5. **Missing/added properties**:
     ```
     *, ::before, ::after, ::backdrop
       + --tw-ring-color
       + --tw-ring-shadow
     ```
     **Fix**: Check `properties_layer` in `lib/build.ml` and ensure the utility
     properly declares its variables using `Var.property_rule`.

---

## 9) Common pitfalls (and fixes)

1. Writing raw `"var(--name)"` or building a `Css.custom_property` value by
   concatenation → model it in `Var`, extend types if missing.
2. Setting variables in the wrong utility → only **style** utilities set style vars.
3. Forgetting `~property_rules` on referrers → `@property` never emitted.
4. Spreading tests across multiple concerns → one concept per file.
5. Silencing warnings with `OCAMLPARAM=_,w=-32` → address them; do not suppress.
6. Using non-existent `Css.kind` constructors → check `type _ kind` in
   `cascade/lib/properties_intf.ml` for valid kinds. If the kind genuinely needs its own
   type, add it there. Otherwise, use an existing kind (e.g., `Length` for letter-spacing
   since it takes length values).
7. Adding `Raw of string` or similar escape hatches to bypass type safety → **NEVER**. Always add
   the properly typed properties/variants you need. Extend the type system rather than escape it.

---

## 10) Tests — organisation & rules

* `cascade/test/`: CSS parsing, printing and optimizing, MDN/W3C conformance
  where feasible.
* `test/`: Tailwind v4 parity by utility, one `test_<module>.ml` per
  `lib/<module>.ml`.
* `test/upstream/`: replay of Tailwind's own fixture corpus. Generated; never
  hand-edit.
* **Rules:**

  1. Define named test functions (`let test_foo () = ...`).
  2. Do not inline anonymous fns in `test_case`.
  3. Cover invalid inputs with `try_parse`.
  4. Document any ambiguous spec choices in comments.

---

## 11) Critical instructions (please read once)

* Use **`tmp/`** for all test/debug files; never `/tmp/`.
* Prefer **editing** existing docs over creating new ones unless explicitly requested.

---

## 12) CSS comparison tool bugs — MUST FIX IMMEDIATELY

⚠️ **CRITICAL**: If `--diff` or cssdiff reports **"No differences found"** but there ARE actual differences in the CSS output, this is **NOT** a known limitation to work around. It is a **bug in the cssdiff tool that MUST be fixed before doing any other work**.

**Do NOT:**
- Mark tests as "effectively passing" when cssdiff fails to detect real differences
- Proceed with utility implementation while cssdiff is broken
- Treat cssdiff bugs as acceptable limitations

**Instead:**
1. **STOP** all other work immediately
2. **FIX** the differ in `cascade/lib/diff/css_compare.ml` (with `tree_diff.ml` and
   `string_diff.ml` alongside it), add a case to `cascade/test/test_css_compare.ml`,
   and land it in cascade. CI here pins cascade to its `main`, so the fix arrives
   with the next run; bump the version bound in `dune-project` and `tw.opam` if it
   needs a release
3. **VERIFY** it correctly detects the differences it was missing
4. **THEN** resume your original task

The differ is cascade's, and tw only consumes it. Never work around a differ bug
on the tw side: a comparison hack here hides the bug from every other cascade
consumer and leaves the differ reporting the same false negative.

The one thing to check before calling it a bug is that you are comparing what you
think you are. `--diff` defaults to canonical mode, which is deliberately blind to
selector regrouping, cascade-neutral reordering and a few equivalent spellings; a
difference it drops on purpose is not a false negative. Pass `--diff-mode=tree` or
`--diff-mode=string` to see the layer below, and compare the raw sheets when
duplication or placement is what you are chasing.

The differ is the foundation of our Tailwind parity testing. If it cannot reliably
detect differences, we cannot trust any of our tests. Fixing it is always the
highest priority.

---

## 13) PR checklist (copy into your description)

* [ ] Variable pattern chosen and used consistently (link line numbers).
* [ ] Correct layer(s) touched; no cross-layer leakage.
* [ ] No raw `"var(--...)"` or assembled `Css.custom_property` values.
* [ ] `@property` emitted when required (`~property_rules` present).
* [ ] Tests: one concept/file; includes invalid input.
* [ ] Tailwind parity checked with `tw --diff`.
* [ ] Warnings addressed; no suppression flags.
* [ ] Commands in docs/scripts write to `tmp/` only.
