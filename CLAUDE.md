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

# Compare with Tailwind (requires npx tailwindcss)
echo '<div class="p-4">X</div>' > tmp/test.html
npx tailwindcss --content tmp/test.html --minify --optimize 2>/dev/null
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
   ALCOTEST_VERBOSE=1 dune exec test/test.exe test tw 10
   diff -u tmp/css_debug/test_10_tw.css tmp/css_debug/test_10_tailwind.css
   ```

   (Paths under `tmp/css_debug/` are created by tests.)

---

## 8) Debugging checklist

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

* **CSS comparison tool**: may report "no structural differences" while char counts differ. The issue is tracked in `lib/tools/css_compare.ml`. Prioritise a fix before relying on negative diffs for CI gating.

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