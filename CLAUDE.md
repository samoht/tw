# OCaml × Tailwind v4 — Contributor Guide (for Claude Code)

## 1) Purpose

This repo provides a type-safe Tailwind v4 implementation in OCaml. Utilities are compiled to CSS through a strongly-typed variable system and layered rules that mirror Tailwind's pipeline. The goal: **no raw `"var(--...)"` strings; no `Css.custom`; spec-faithful output.**

---

## 2) Core principles

1. **Type safety over strings.** Never write `"var(--name)"` or `Css.custom`; represent variables and properties via `Var` and typed constructors.
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

## 5) Variable system (the four patterns)

All variables are declared and referenced via `Var`. Pick **one** pattern and apply it consistently.

### Pattern 1 — `theme` (design tokens)

Use for named scales: font sizes, spacing, colours.

```ocaml
let v = Var.theme kind "text-xl" ~order:N
let decl, ref_ = Var.binding v (Css.rem 1.25)
style "text-xl" [ decl; prop (Var ref_) ]
```

Typical users: typography, spacing. Order must match Tailwind canonical ordering in `var.ml`.

### Pattern 2 — `property_default`

For variables with `@property` defaults that utilities may override.
**Setter:**

```ocaml
let v = Var.property_default kind ~initial:default "border-style"
let decl, ref_ = Var.binding v Css.solid
style "border-solid" [ decl; prop (Var ref_) ]
```

**Referrer:**

```ocaml
let ref_ = Var.reference v
let props = match Var.property_rule v with Some r -> r | None -> Css.empty
style "border" ~property_rules:props [ prop (Var ref_) ]
```

Ensure the referrer passes `~property_rules` so `@property` is emitted.

### Pattern 3 — `channel` (compositional)

For transform/filters channels (e.g. `--tw-rotate`).

```ocaml
let v = Var.channel kind "rotate"
let decl, _ = Var.binding v (Css.deg 45)
style "rotate-45" [ decl ]
```

The final composed property is built elsewhere from channels.

### Pattern 4 — `ref_only`

Referenced with a fallback; never set by the utility.

```ocaml
let v = Var.ref_only kind "blur" ~fallback:(Css.px 0)
let r = Var.reference v
style "backdrop" [ prop (Var r) ]
```

Useful when the value originates upstream (theme/properties).


---

## 6) Layers (how we emit CSS)

`rules.ml` builds layers in order:

1. **theme** (tokens)
2. **properties** (`@property` and defaults)
3. **base**
4. **components**
5. **utilities**

Utilities must not declare tokens; theme must not reference utility vars. See `compute_theme_layer`and`build_properties_layer`.

### Rule ordering within utilities layer

Within the utilities layer, rules are sorted by `compare_indexed_rules` in `rules.ml`:

1. **Rule types are grouped**: Regular, Media, Container, Starting (via `rule_type_order`)
2. **Within Regular rules**: sorted by (priority, suborder, alphabetical)
3. **Within Media rules**: preference media (hover, dark, contrast) before responsive media (sm, md, lg)
4. **Regular vs Media comparison** (`compare_regular_vs_media`):
   - Same `base_class` → preserve original order (keeps utility groups together)
   - Media is **responsive** (has `:` in base_class like `md:grid-cols-2`) → Regular always first
   - Media is **non-responsive** (like container's media with base_class `container`) → compare by priority

**Key insight**: Container's media queries (priority 0) must stay grouped with `.container`, while
responsive media queries like `md:grid-cols-2` should come after ALL regular utilities.

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

* **Order mismatch?**

  ```bash
  grep "~order:" lib/typography.ml
  ```

  Match canonical order constants in `var.ml`.

* **Targeted test run:**

  ```bash
  ALCOTEST_VERBOSE=1 dune exec test/test.exe test tw <N>
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
     `lib/rules.ml`. Key insight: responsive media (with `:` like `md:`) should come after
     all Regular rules, but non-responsive media (like container's media) should stay
     grouped with their base utility via priority comparison.

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

## 12) Known issues

* **CSS comparison tool limitations**:
  - Reports string diffs instead of structural diffs when selectors are fundamentally different (e.g., missing pseudo-elements like `::marker`)
  - May show "no structural differences" when only property values differ
  - Does not detect when entire selector chains are missing (e.g., `.hover\:prose:hover :where(ol>li)::marker` vs `.hover\:prose:hover`)
  - Issue tracked in `lib/tools/css_compare.ml`. Fix required for proper CI gating.

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