# OCaml × Tailwind v4 — Contributor Guide (for Claude Code)

## 1) Purpose

This repo provides a type-safe Tailwind v4 implementation in OCaml. Utilities are compiled to CSS through a strongly-typed variable system and layered rules that mirror Tailwind's pipeline. The goal: **no raw `"var(--...)"` strings; no `Css.custom`; spec-faithful output.**

### Scope

All core Tailwind v4 utilities and all official plugins are in scope, including:
- `@tailwindcss/forms` — **fully supported**
- `@tailwindcss/typography` — **fully supported**

---

## 2) Core principles

1. **Type safety over strings.** Never write `"var(--name)"` or `Css.custom`; represent variables and properties via `Var` and typed constructors. **NEVER add `Raw of string` or similar escape hatches to CSS types** — if a pattern cannot be expressed with existing types, extend the type system properly.
2. **Spec-driven tests.** CSS behaviour is tested against MDN/W3C where applicable; utilities against Tailwind v4 output.
3. **Variables follow four patterns.** See patterns below and `docs/adding-a-new-utility.md`.
4. **Respect layers.** `theme → properties → base → components → utilities`. Utilities must not leak into theme or properties.

---

## 3) Project structure

```
lib/           core (utilities, CSS generation)
  css/         CSS AST + emission
  var.ml       variable system (critical)
  rules.ml     layer assembly
test/          Alcotest suites (CSS + Tailwind parity)
docs/          adding-a-new-utility.md (start here for new utils)
```

Keep examples small and targeted; each test file should focus on one concept.

---

## 4) Quick start (build, run, compare)

```bash
# Build
dune build

# Run all tests (verbose helps when debugging mismatches)
ALCOTEST_VERBOSE=1 dune exec test/test.exe

# Generate CSS for a snippet (variables only, then with base)
dune exec -- tw -s "p-4" --variables
dune exec -- tw -s "p-4" --variables --base

# For classes starting with -, use --single="..." to avoid CLI flag parsing
dune exec -- tw --single="-content-around" --variables

# Compare with real Tailwind CSS
## Method 1: Direct comparison using --diff (recommended)
dune exec -- tw -s "p-4" --diff

## Method 2: Generate with real Tailwind via --tailwind
dune exec -- tw -s "p-4" --tailwind
```

### Backend modes (mutually exclusive)

The `tw` binary supports three backend modes:

- **Native** (default): Our OCaml implementation
- **`--tailwind`**: Use real tailwindcss tool to generate CSS
- **`--diff`**: Compare our output with real Tailwind and show differences

**Important**: `--diff` mode always uses:
- Variables mode (ignores `--inline` flag)
- Base layer included (ignores `--no-base` flag)
- This ensures consistent comparison conditions

Example diff output:
```bash
$ dune exec -- tw -s "prose-sm" --diff
Differences found for 'prose-sm':

--- Tailwind
+++ tw
- .prose-sm:
    - modify: line-height 1.7142857 -> 1.71429
```

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

Layer architecture is documented in **`lib/rules.mli`**. Key points:

- **Layer order**: `properties → theme → base → components → utilities`
- **Utilities layer**: Sorted by (priority, suborder) for conflict resolution
- **Media queries**: Modifier-based media comes after regular rules; built-in media stays grouped with its utility

See `lib/rules.mli` for the complete layer model documentation.

---

## 7) Adding a new utility — minimal workflow

1. **Read** `docs/adding-a-new-utility.md`. Identify the variable pattern.
2. **Implement** the utility in `lib/<area>.ml` using typed `Var` and the chosen pattern.
3. **Place** declarations in the correct layer via `rules.ml`.
4. **Test**:

   * CSS spec behaviour (if relevant) under `test/css/`.
   * Tailwind parity under `test/`. Use a *single* concept per file.
5. **Compare** against Tailwind:

   ```bash
   # Quick comparison for any class
   dune exec -- tw -s "border-2 border-solid" --diff

   # Or for test files
   ALCOTEST_VERBOSE=1 dune exec test/test.exe test tw 10
   diff -u tmp/css_debug/test_10_tw.css tmp/css_debug/test_10_tailwind.css
   ```

   (Paths under `tmp/css_debug/` are created by tests.)

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

  The logic is in `collect_all_property_rules` in `rules.ml` which filters by `set_var_names`
  extracted from Custom_declarations.

* **Wrong property order in `@layer properties`?**

  Check `property_order` in `rules.ml`. Properties are sorted by category:
  - Transform (0-9): `tw-scale-x`, `tw-scale-y`, etc.
  - Gradients (10-19): `tw-gradient-*`
  - Borders (25): `tw-border-style`
  - Typography (28-32): `tw-leading`, `tw-font-weight`, `tw-tracking`
  - Animation (40-41): `tw-duration`, `tw-ease`
  - Default (100): everything else

* **Order mismatch?**

  ```bash
  grep "~order:" lib/typography.ml
  ```

  Match canonical order constants in `var.ml`.

* **Targeted test run:**

  ```bash
  ALCOTEST_VERBOSE=1 dune exec test/test.exe test tw <N>
  ```

  For tools tests (css diff, tree diff, etc.):

  ```bash
  # Note the required -- before args
  ALCOTEST_VERBOSE=1 dune exec test/tools/test.exe -- test css_compare 12
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

  2. **Add a unit test**: Create a test in `test/test_rules.ml` that codifies the expected behavior
     ```ocaml
     let test_your_case () =
       let utilities = Tw.[ md [ grid_cols 2 ]; max_w_4xl ] in
       Test_helpers.check_ordering_matches
         ~test_name:"regular before media" utilities
     ```

  3. **Fix the issue**: Modify the relevant code (usually in `lib/rules.ml`)
     - Pay attention to comparison functions and ordering logic
     - Ensure symmetric comparisons negate results when arguments are swapped
     - Regular rules should come before media queries at the same priority

  4. **Verify**: Run both the unit test and the original example test
     ```bash
     ALCOTEST_VERBOSE=1 dune exec test/test.exe test rules <N>
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
     **Fix**: Check `lib/css/optimize.ml` for media query merging. Consecutive `@media` blocks
     with the same condition should be merged. The `merge_consecutive_media` function handles this.

  2. **"blocks at different positions"** - Media query appears at wrong location:
     ```
     @media (prefers-reduced-motion:no-preference) (1 blocks at different positions)
       - Block at position 8: .motion-safe\:animate-pulse
       + Block at position 25: .motion-safe\:animate-pulse
     ```
     **Fix**: Check `lib/rules.ml` comparison functions. The issue is usually in:
     - `compare_regular_vs_media`: Regular rules vs Media rules ordering
     - `compare_media_rules`: Media rules vs Media rules ordering
     - `extract_media_sort_key`: How media queries are sorted (responsive vs preference)

  3. **"reordered"** - Rules within a container are in wrong order:
     ```
     └─ @layer utilities (1 reordered)
        └─ .flex-col ↔  @media (min-width:48rem)
     ```
     **Fix**: This is a Regular vs Media ordering issue. Check `compare_regular_vs_media` in
     `lib/rules.ml`. Uses `is_modifier_selector` to detect `\:` in CSS selector - modifier media
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
     **Fix**: Check `lib/rules.ml` `build_properties_layer` and ensure the utility
     properly declares its variables using `Var.property_rule`.

---

## 9) Common pitfalls (and fixes)

1. Writing raw `"var(--name)"` or `Css.custom` → model it in `Var`, extend types if missing.
2. Setting variables in the wrong utility → only **style** utilities set style vars.
3. Forgetting `~property_rules` on referrers → `@property` never emitted.
4. Spreading tests across multiple concerns → one concept per file.
5. Silencing warnings with `OCAMLPARAM=_,w=-32` → address them; do not suppress.
6. Using non-existent `Css.kind` constructors → check `lib/css/declaration_intf.ml` for valid kinds.
   If the kind genuinely needs its own type, add it to `declaration_intf.ml`. Otherwise, use an
   existing kind (e.g., `Length` for letter-spacing since it takes length values).
7. Adding `Raw of string` or similar escape hatches to bypass type safety → **NEVER**. Always add
   the properly typed properties/variants you need. Extend the type system rather than escape it.

---

## 10) Tests — organisation & rules

* `test/css/`: MDN/W3C conformance where feasible.
* `test/`: Tailwind v4 parity by utility.
* Output artefacts live under `tmp/css_debug/`.
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

⚠️ **CRITICAL**: If `--diff` or cssdiff reports **"No structural differences"** but there ARE actual differences in the CSS output, this is **NOT** a known limitation to work around. It is a **bug in the cssdiff tool that MUST be fixed before doing any other work**.

**Do NOT:**
- Mark tests as "effectively passing" when cssdiff fails to detect real differences
- Proceed with utility implementation while cssdiff is broken
- Treat cssdiff bugs as acceptable limitations

**Instead:**
1. **STOP** all other work immediately
2. **FIX** the cssdiff tool in `lib/tools/css_compare.ml`
3. **VERIFY** it correctly detects the differences it was missing
4. **THEN** resume your original task

**Known cssdiff bugs that need fixing** (not "limitations" — actual bugs):
- Reports string diffs instead of structural diffs when selectors are fundamentally different (e.g., missing pseudo-elements like `::marker`)
- Shows "no structural differences" when property values differ (e.g., `clip-path: inset(50%)` syntax)
- Does not detect when entire selector chains are missing (e.g., `.hover\:prose:hover :where(ol>li)::marker` vs `.hover\:prose:hover`)

The cssdiff tool is the foundation of our Tailwind parity testing. If it cannot reliably detect differences, we cannot trust any of our tests. Fixing it is always the highest priority.

---

## 13) PR checklist (copy into your description)

* [ ] Variable pattern chosen and used consistently (link line numbers).
* [ ] Correct layer(s) touched; no cross-layer leakage.
* [ ] No raw `"var(--...)"` or `Css.custom`.
* [ ] `@property` emitted when required (`~property_rules` present).
* [ ] Tests: one concept/file; includes invalid input.
* [ ] Tailwind parity diff checked under `tmp/css_debug/`.
* [ ] Warnings addressed; no suppression flags.
* [ ] Commands in docs/scripts write to `tmp/` only.
