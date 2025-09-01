Title: Utility variables and layers (typography as reference)

This doc explains how utilities define and use CSS custom properties (variables) and how those become part of Tailwind-style layers. It uses Typography as the concrete example and outlines a repeatable pattern you can apply to other utilities.


Why this matters

- Variables allow theme tokens (font sizes, colors, radii…) to be referenced consistently from utilities.
- The Rules engine emits layers to match Tailwind v4: `@layer theme`, optional `@layer properties`, `@layer base`, `@layer components`, and `@layer utilities`.
- You do not manually write layers in utility modules. You attach variable declarations and optional `@property` registrations; the Rules engine assembles them into the right layers.

Key building blocks

- `Var.theme`: defines a theme-scoped variable and returns
  - a declaration to define the variable (include in the style that introduces it), and
  - a typed variable reference to use in CSS values.
  Declarations collected from all used utilities are emitted under `@layer theme` in `:root,:host` with canonical ordering.

- `Var.utility`: defines a per-utility variable assignment and returns
  - a declaration to set the variable when the utility is present,
  - a typed variable reference for use in CSS values,
  - optional `~fallback` value (used if the variable isn’t set elsewhere).

- `Var.property`: registers an `@property` at‑rule for a variable (syntax, inheritance, initial) so browsers can interpolate/transition correctly.
  - Effect 1: defaults for those custom properties are emitted inside a gated `@supports(...)` block under `@layer properties`.
  - Effect 2: the `@property` at‑rules themselves are emitted once at the stylesheet level (outside layers) and deduplicated.

- `style ~property_rules`: attach the `Var.property` registrations to any utilities that rely on those variables so both effects above are triggered when the utility is used.

How Typography does it

1) Theme variables for font sizes and line heights

- Each text size uses `Var.theme` twice: one var for the font size and one for its corresponding line-height token.
- Example (simplified):

  - `let size_def, size_var = Var.theme Var.Text_xl (Rem 1.25)`
  - `let lh_def,   lh_var   = Var.theme Var.Text_xl_line_height (Calc (…))`

- The `text-xl` utility includes both declarations and then uses the vars:

  - `style "text-xl" [ size_def; lh_def; font_size (Var size_var); line_height (Var leading_var) ]`

2) Per-utility variable with fallback for “leading”

- Typography exposes a shared `--tw-leading` variable used by all text sizes.
- Each `text-*` utility creates it with `Var.utility Var.Leading ~fallback:(Var text_*_lh_var) Zero` and then sets `line_height (Var leading_var)`.
- No `@property` is required for `--tw-leading` because it is a length-like var used directly; it still participates in ordering and fallbacks.

3) Font weight: utility var + @property

- A typed variable `--tw-font-weight` is set per utility via `Var.utility Var.Font_weight …`, and related utilities attach `~property_rules` so that:

  - Defaults are emitted inside `@layer properties` within the gated `@supports(...)` block.
  - A single deduped `@property --tw-font-weight ...` is emitted at the stylesheet level.

  Example:

  - In typography: `let property_rules = [ Var.property Var.Font_weight ~syntax:"*" ~inherits:false ~initial:"initial" ]`
  - Utilities like `font-bold` pass `~property_rules` to `style`.

  **Important**: The `initial` value must be "initial" (the string), not an empty string.

4) Theme defaults for font families

- `Var.theme Var.Font_sans [...]`, `Var.theme Var.Font_mono [...]`, and two "default" font family vars are defined for `@layer theme` and referenced by utilities (`font-sans`, etc.).

5) Font-variant-numeric with trailing comma syntax (Tailwind v4)

- Font-variant-numeric in Tailwind v4 uses a special trailing comma syntax: `var(--tw-ordinal,)var(--tw-slashed-zero,)`
- To achieve this, use `~fallback:Css.Empty` when defining empty variables:
  ```ocaml
  let _, empty_ordinal_var =
    Var.utility Var.Font_variant_ordinal ~fallback:Css.Empty Css.Normal_numeric
  ```
 - The `Empty` token is supported in `font_variant_numeric_token` for composition
  - Reminder: `Empty` renders to an empty string, so using it as the fallback in `var(--name, )` produces the required trailing comma form used by Tailwind's composed values
- This pattern ensures CSS generates `var(--name,)` with a trailing comma for proper composition

How Rules turns this into layers

- `compute_theme_layer` scans all used styles, extracts custom declarations (from `Var.theme`), adds a small set of default vars (font families), and emits them under `@layer theme` in `:root,:host` with a stable order.
  
  **Variable Ordering**: The `order` function in `var.ml` determines the canonical ordering of variables in the theme layer. Each variable type has a numeric order value that ensures consistent output matching Tailwind v4. For example:
  - Font families: 0-10
  - Font sizes and line heights: 100-199  
  - Font weights: 200-209
  - Border radius: 300-307
  - Default font families: 400-405
  
- `build_properties_layer` collects all `property_rules` attached to used utilities and emits an `@layer properties` that contains a single `@supports(...) { ... }` block with default custom-property values targeting `*, :before, :after, ::backdrop` (not the `@property` rules themselves).
- `build_base_layer` wraps Preflight rules under `@layer base` and applies the placeholder support shim.
- Components is left empty by default; utilities render to `@layer utilities`.
- When minifying, consecutive empty layers are merged into a single declaration, e.g., `@layer components,utilities;` to match Tailwind output.

**Rule Ordering and Conflict Groups**:
The Rules module uses a sophisticated conflict group system to ensure proper CSS cascade behavior:

- **Conflict Groups**: Utilities are grouped by their conflict relationships (e.g., all padding utilities conflict with each other)
- **Stable Sort**: Within each conflict group, `List.stable_sort` preserves the original order while applying the group's sorting function
- **Suborder Functions**: Fine-grained control within groups using the `suborder` field in conflict definitions
- **Example**: For prose utilities, the suborder ensures `.prose` comes before `.prose :where(...)` child selectors:
  ```ocaml
  suborder = (fun core -> 
    if core = "prose" then 0                        (* .prose first *)
    else if String.starts_with ~prefix:"prose " then 1  (* .prose :where(...) second *)
    else 2);                                         (* others last *)
  ```

**CSS Minification Considerations**:
- The minifier must preserve spaces in descendant combinators (`.prose :where()` not `.prose:where()`)
- Special handling for pseudo-classes: spaces before `:where()`, `:not()`, etc. are significant
- Test with `--minify` flag to ensure selector specificity isn't altered

Applying this pattern to a new utility

- Decide variable scope:
  - Theme token (shared across utilities)? Use `Var.theme` in the module that owns the token. Include the returned declaration in the first utility that introduces it.
  - Per-utility setting with override behavior? Use `Var.utility` inside the utility.

- If browsers need `@property` registration (e.g., animated/typed variables like scales, transforms, some numeric font variants):
  - Define `let property_rules = [ Var.property Var.My_var ~syntax:"…" ~inherits:false ~initial:"…"; (* possibly more *) ]`.
  - Pass `~property_rules` to every `style` that uses `Var.My_var`.

- Reference variables in CSS values using the typed `Var` returned by `Var.theme`/`Var.utility`, e.g., `transform (Var scale_x_var)` or `letter_spacing (Var my_var)`.

- Provide sensible fallbacks:
  - For `Var.utility`, set `~fallback` to a static value or another `Var` you know will exist (typography uses the size-specific line-height var as fallback for `--tw-leading`).

Minimal checklist

- Add a `Var` constructor in `lib/var.ml` (type, `to_string`, and `order`).
- If it is a theme token, use `Var.theme` and include the declaration where first used.
- If it is per-utility, use `Var.utility` with a good fallback.
- If needed, register `@property` via `Var.property` and attach it using `style ~property_rules`.
- Use the typed `Var` in the CSS declarations of your utility.
- Rely on `Tw.Rules.to_css` to assemble the correct layers automatically.

Good references

- Typography font sizes and leading: `lib/typography.ml` (look for `Var.theme` Text_* and `Var.utility Var.Leading`).
- Typography font weight: `lib/typography.ml` (look for `property_rules` and `Var.Font_weight`).
- Borders shared var + `@property`: `lib/borders.ml` (search `Var.Border_style`).
- Layer assembly: `lib/rules.ml` (`compute_theme_layer`, `build_properties_layer`).

Debugging utilities

To debug CSS generation issues, use the tw CLI tool:

```bash
# Generate CSS for a class without the base layer (useful for test comparisons)
dune exec -- tw -s <class> --variables

# Include the base layer
dune exec -- tw -s <class> --variables --base

# Test with minification (critical for finding selector issues)
dune exec -- tw -s <class> --variables --minify

# Examples:
dune exec -- tw -s shadow-sm --variables  # See shadow-sm without base layer
dune exec -- tw -s border-none --variables # Debug border utilities
dune exec -- tw -s "prose prose-lg" --variables --minify # Test complex selectors
```

This helps you:
- Compare your output with Tailwind's expected output
- See which layers are being generated
- Debug variable ordering issues
- Understand which @property rules are being included
- Find minification issues with selectors
- Debug rule ordering within utilities

**Debugging Missing CSS Content**:
When output is shorter than expected:
1. Check if all child selectors are being generated
2. Verify rule ordering (parent before children)
3. Look for dropped rules due to incorrect conflict groups
4. Compare character counts to identify missing sections
5. Use `diff` tools to find exact differences

Quick template (copy/paste)

- var.ml: add your variable constructor, map it in `to_string` and `order`.
- In your utility module:
  - Theme token (if needed):
    ```ocaml
    let my_token_def, my_token_var = Var.theme Var.My_token initial_value
    ```
  - Per-utility var with fallback:
    ```ocaml
    let my_def, my_var = Var.utility Var.My_var ~fallback:my_fallback fallback_value
    ```
  - Optional @property registration (if variable benefits from registration):
    ```ocaml
    let property_rules = [ Var.property Var.My_var ~syntax:"*" ~inherits:false ~initial:"initial" ]
    ```
  - Utility style:
    ```ocaml
    let my_util =
      style "my-util" ~property_rules [
        my_token_def;    (* only if you use a theme token here *)
        my_def;          (* the utility var assignment *)
        Css.some_property (Var my_var);
      ]
    ```

Common pitfalls and solutions

**Rule Ordering Issues**
- **Problem**: Child selectors appearing before parent selectors (e.g., `.prose :where(p)` before `.prose`)
- **Solution**: Adjust the conflict group's suborder function to explicitly order selectors
- **Debugging**: Add print statements in `rules.ml` to see the order before and after sorting
- **Key insight**: The order matters for CSS cascade - parent rules should come first

**CSS Minification Breaking Selectors**
- **Problem**: Minifier removing spaces in descendant combinators, changing selector meaning
- **Solution**: Update `minify_selector` in `css.ml` to preserve spaces before pseudo-classes
- **Example**: `.prose :where(p)` (descendant) vs `.prose:where(p)` (direct) have different meanings
- **Testing**: Always test with `--minify` flag to catch these issues early

**CRITICAL: Never use var(...) in string literals or Css.custom**
- **Never do this**: `Css.custom "box-shadow" "var(--tw-shadow)"` 
- **Never do this**: `box_shadow (Raw "var(--tw-shadow)")`
- **Why**: 
  - String literals containing `var(...)` break the Rules engine's ability to track dependencies, resolve variables, and generate correct layers
  - `Css.custom` is a code smell - it bypasses the type system and makes the code fragile
- **Instead**: Always use typed `Var` references from `Var.theme` or `Var.utility`
- **Exception**: When a CSS variable's actual value contains `var(...)` references (like Tailwind v4's `--tw-gradient-stops`), the string is the proper CSS value, not a reference. In these cases:
  - The variable type should be `string t` in var.ml
  - Use `Css.Computed_stops` or similar typed constructors when available
  - Document clearly that this is the CSS value, not a variable reference
- **If you need new functionality**: Extend the type system properly:
  - Add new constructors to existing types following the standard pattern:
    ```ocaml
    type box_shadow =
      | Shadow of shadow
      | Shadows of shadow list  
      | None
      | Var of box_shadow var  (* Standard pattern: Var (no suffix) with typed variable *)
    ```
  - The naming convention is always `Var` (not `Var_list`, `Var_composition`, etc.)
  - The `Var` constructor holds a typed variable reference created by `Var.utility` or `Var.theme`
  - Never work around the type system with strings

1. **Border utilities setting variables incorrectly**
   - **Problem**: Border width utilities (border, border-2, etc.) were incorrectly setting the `--tw-border-style` variable instead of just using it
   - **Solution**: Only border style utilities (border-solid, border-dashed, etc.) should set the variable. Width utilities should only reference it with `Var border_style_var`
   - **Pattern**: Style utilities define the variable, width/size utilities consume it

2. **Wrong CSS layer generation**
   - **Problem**: Style utilities like `border-none` were generating `@layer properties` when they should be in `@layer utilities`
   - **Solution**: Only pass `~property_rules` to utilities that need `@property` registration. Style utilities that just set variables shouldn't have property rules.
   - **Key insight**: `~property_rules` triggers `@layer properties` generation, so use it judiciously

3. **@property syntax for enumerated values**
   - **Problem**: Using enumerated syntax like `"solid" | "dashed" | "dotted"` for `@property` registration
   - **Solution**: Use `syntax:"*"` for generic/untyped properties that can accept any value
   - **Example**: `Var.property Var.Border_style ~syntax:"*" ~inherits:false ~initial:"solid"`

4. **Variable ordering in theme layer**
   - **Problem**: Variables appearing in wrong order in `@layer theme`, causing test failures
   - **Solution**: Ensure the `order` function in `var.ml` assigns correct numeric values to match Tailwind v4's output
   - **Note**: The `canonical_theme_order` list is often redundant if the `order` function provides complete ordering

5. **Font-variant-numeric empty fallback handling**
   - **Problem**: Font-variant-numeric variables need trailing commas in CSS but normal fallback generates wrong syntax
   - **Solution**: Use `~fallback:Css.Empty` and add an `Empty` constructor to handle this special case
   - **Result**: Generates `var(--tw-ordinal,)` with trailing comma as required by Tailwind v4

6. **Helper functions to reduce duplication**
   - **Problem**: Repetitive code for similar utilities (e.g., all font-variant-numeric utilities had similar structure)
   - **Solution**: Create helper functions with optional parameters using OCaml's labeled argument pattern
   - **Example**: `?(param : type = default_value)` allows callers to override only what they need
   - **Key**: Pass the variable reference (from `Var.utility`) to the helper, not the definition
