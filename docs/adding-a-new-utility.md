Title: CSS Variables in Utilities - The Variable Pattern System

This doc explains the CSS variable patterns used in Tailwind CSS v4 and how to implement them correctly in OCaml. There are 4 core variable types (`Var.theme`, `Var.property_default`, `Var.channel`, `Var.ref_only`) with 5 usage patterns. Each pattern has specific use cases and constraints.

## The Variable Patterns

Every CSS variable in our system follows one of these patterns (4 variable types, 5 usage patterns):

### Pattern 1: Theme Variables
**Purpose**: Design tokens set once in theme layer, referenced by utilities
**Examples**: `--text-xl: 1.25rem`, `--font-weight-bold: 700`, `--color-blue-500: #3b82f6`
**Layer**: `@layer theme` in `:root,:host`
**Usage**: `Var.theme kind name ~order:(group, index)` and set via `Var.binding` once

### Pattern 2: Property_default Variables
**Purpose**: Variables with @property defaults that utilities can either SET or REFERENCE
**Examples**: `--tw-border-style` (default: solid), `--tw-border-opacity` (default: 1)
**Layer**: `@layer properties` for defaults, `@property` rules at top level
**Usage**: Use `Var.property_default kind ~initial ~inherits? ~universal? name` (initial is mandatory). Referencing utilities call `Var.reference` and include `~property_rules:(Var.property_rules var)`

### Pattern 3: Channel Variables
**Purpose**: Composition variables where multiple utilities contribute to a single CSS property
**Examples**: `--tw-translate-x`, `--tw-rotate`, `--tw-scale` (for transforms)
**Layer**: Set in `@layer utilities` by individual utilities
**Usage**: Declare with `Var.channel kind name`. Contributing utilities use `Var.binding`; aggregator references the channels (with fallbacks for inline mode)

### Pattern 4: Ref_only Variables
**Purpose**: Variables that utilities only reference (with fallback), never set
**Examples**: `--tw-shadow-color`, `--tw-ring-color`
**Layer**: Not set by referencing utilities (set elsewhere)
**Usage**: Create with `Var.ref_only kind name ~fallback:value`, then use `Var.reference var` (fallback is built-in); never set in the referencing module

### Pattern 5: Always-set Variables (variant of Pattern 3)
**Purpose**: Variables that are always set when used
**Examples**: `--tw-font-weight` in font utilities
**Layer**: Set in `@layer utilities`
**Usage**: Uses `Var.channel` like Pattern 3, but every utility both sets and uses the variable (no separate aggregator)
**Note**: This is technically a usage pattern of `Var.channel`, not a distinct variable type

## Choosing the Right Pattern

Use this decision tree:
1. **Is it a design token shared across utilities?** → Pattern 1 (Theme)
2. **Does it need a default that some utilities override?** → Pattern 2 (Property_default)
3. **Do multiple utilities contribute to compose a value?** → Pattern 3 (Channel)
4. **Do utilities only reference it, never set it?** → Pattern 4 (Ref_only)
5. **Is it always set when used?** → Pattern 5 (Always-set)

## Key Building Blocks

- `Var.theme`: defines a Theme variable (design token) with explicit `~order`
- `Var.property_default`: defines a Utility variable with typed @property defaults
- `Var.channel`: defines a Utility composition variable without @property
- `Var.ref_only`: defines a variable that's only referenced (fallback built-in)

- `Var.binding`: creates both declaration and reference (Patterns 1, 3, 5)
  - Returns `(declaration, var_ref)` tuple
  - Declaration sets the variable
  - Reference uses it

- `Var.reference`: creates only reference without declaration (Pattern 2)
  - For `property_default` variables - uses built-in @property default
  - For `ref_only` variables - uses built-in fallback

- `Var.reference_with_fallback`: creates reference with explicit fallback (Patterns 3, 4)
  - Use for `channel` variables when you need a fallback value
  - Signature: `Var.reference_with_fallback var fallback_value`

- `Var.property_rule`: generates @property rule (Patterns 2 and 3)
  - Returns `Some rule` if variable has property metadata
  - Pass to `Style.style ~property_rules` to generate @property
  - `Var.property_rules` is the same thing for a `property_default`, already
    defaulted to `Css.empty`

## Pattern Examples

`Style.style` takes declarations only. The class name lives in the module's
`Handler`, which maps a variant to its class through `to_class` and back through
`of_class`; `lib/utility.mli` shows the whole shape.

### Pattern 1: Theme Variables (Typography)
```ocaml
(* Define theme variables with explicit ordering *)
let text_xl_size_var = Var.theme Css.Length "text-xl" ~order:(6, 8)
let text_xl_lh_var = Var.theme Css.Line_height "text-xl--line-height" ~order:(6, 9)

(* Set theme values once *)
let size_def, size_var = Var.binding text_xl_size_var (Rem 1.25)
let lh_def, lh_var = Var.binding text_xl_lh_var (Rem 1.75)

(* Utility references theme variables *)
let text_xl =
  Style.style [
    size_def; lh_def;
    font_size (Var size_var);
    line_height (Var lh_var)
  ]
```

### Pattern 2: Property_default Variables (Borders)
```ocaml
(* Variable with @property default *)
let border_style_var =
  Var.property_default Css.Border_style ~initial:Solid "tw-border-style"

(* Setting utility - changes the variable *)
let border_solid =
  let decl, var_ref = Var.binding border_style_var Solid in
  Style.style [ decl; border_style (Var var_ref) ]

(* Referencing utility - uses @property default *)
let border =
  let var_ref = Var.reference border_style_var in
  Style.style ~property_rules:(Var.property_rules border_style_var) [
    border_style (Var var_ref);
    border_width (Px 1.)
  ]
```

### Pattern 3: Channel Variables (Transforms)
```ocaml
(* Individual channel variables *)
let translate_x_var = Var.channel Css.Length "tw-translate-x"
let rotate_var = Var.channel Css.Angle "tw-rotate"
(* For channels that need @property defaults, use property_default instead *)
let scale_var = Var.property_default Css.Number_percentage ~initial:(Num 1.0) "tw-scale"

(* Contributing utilities set their channels *)
let translate_x_4 =
  let decl, _ = Var.binding translate_x_var (Rem 1.) in
  Style.style [ decl ]

let rotate_45 =
  let decl, _ = Var.binding rotate_var (Deg 45.) in
  Style.style [ decl ]

(* Aggregator utility combines all channels - use reference_with_fallback for channels *)
let transform_all =
  let tx_ref = Var.reference_with_fallback translate_x_var Zero in
  let rot_ref = Var.reference_with_fallback rotate_var (Deg 0.) in
  let scale_ref = Var.reference scale_var in  (* property_default has built-in default *)
  Style.style [
    transform (List [
      Translate_x (Var tx_ref);
      Rotate (Var rot_ref);
      Scale_x (Var scale_ref)
    ])
  ]
```

### Pattern 4: Ref_only Variables (Shadows)
```ocaml
(* Variable that's only referenced, never set by shadows - use Var.channel *)
let shadow_color_var = Var.channel Css.Color "tw-shadow-color"

(* Shadow utilities reference with fallback using reference_with_fallback *)
let shadow_sm =
  let color_ref = Var.reference_with_fallback shadow_color_var (Css.hex "#0000001a") in
  Style.style [
    box_shadow (Shadow {
      h_offset = Px 0.; v_offset = Px 1.;
      blur = Some (Px 3.); spread = Some (Px 0.);
      color = Some (Var color_ref)
    })
  ]

(* Color utilities set the variable (in different module) *)
let shadow_red_500 =
  let decl, _ = Var.binding shadow_color_var (Css.hex "#ef4444") in
  Style.style [ decl ]
```

### Pattern 5: Always-set Variables (Font Weight)
```ocaml
(* Variable always set when used *)
let font_weight_var = Var.channel Css.Font_weight "tw-font-weight"

(* Every utility sets and uses it *)
let font_bold =
  let decl, var_ref = Var.binding font_weight_var (Weight 700.) in
  Style.style [
    decl;
    font_weight (Var var_ref)
  ]

let font_thin =
  let decl, var_ref = Var.binding font_weight_var (Weight 100.) in
  Style.style [
    decl;
    font_weight (Var var_ref)
  ]
```

How Build turns this into layers

- `theme_layer_of` scans used styles, extracts custom declarations (from `Var.theme`), adds a small set of default vars (font families), and emits them under `@layer theme` in `:root,:host` with a stable order.

  **Variable Ordering**: the `~order:(group, index)` pair travels with a theme
  variable and every declaration made from it. The group buckets the family in
  the order Tailwind emits its `@theme` block (1 font families, 3 spacing, 5
  container widths, 6 font sizes, 7 animations, 8 blurs, 9 defaults) and the
  index places the token inside its group. A build collects this metadata into a
  local immutable index; constructing a variable does not mutate process-global
  state. Read the neighbours of the token you are adding rather than guessing a
  group.

- `properties_layer` generates exact typed `@property` rules from variables used by a style and emits an `@layer properties` containing a single `@supports(...) { ... }` block with default custom-property values targeting `*, :before, :after, ::backdrop`. Use explicit `property_rules` only when one utility deliberately publishes a wider group than its declarations reference; pass the matching `Var.metadata` values to `Style.style` with them.
- `base_layer` wraps Preflight rules under `@layer base` and applies the placeholder support shim.
- Components is left empty by default; utilities render to `@layer utilities`.
- When minifying, consecutive empty layers are merged into a single declaration, e.g., `@layer components,utilities;` to match Tailwind output.

All four live in `lib/build.ml`; `lib/build.mli` documents the assembled result.

**Rule Ordering and Conflict Groups**:
Cascade position comes from a `(priority, suborder)` pair, so utilities that
conflict resolve in Tailwind's order:

- **Priority**: `Utility.S.priority` groups a whole family (all padding utilities
  share one). `lib/utility.mli` lists the bands.
- **Suborder**: `Utility.S.suborder` orders variants inside the family. It is a
  plain function on the module's variant type, as in `box_sizing.ml`'s
  `let suborder = function Border -> 0 | Content -> 1`.
- **Lookup**: `Build.conflict_order` resolves the pair for a selector, and
  `Sort.compare_indexed_rules` sorts on it with the source index as a stable
  tiebreaker.
- **Variants**: a rule carrying a modifier prefix does not sort on the pair at
  all until every variant key it holds has tied; `Sort.compare_indexed_rules`
  reads `variant_orders` first, and `lib/sort.mli` describes the key.

**CSS Minification Considerations**:
- Minification is cascade's; `Css.Selector.to_string ~minify:true` is what tw calls
- Descendant combinators must survive it (`.prose :where()`, not `.prose:where()`),
  because the space before `:where()`, `:not()` and friends is significant
- Test with `--minify` to ensure selector specificity isn't altered

## Applying the Patterns to New Utilities

### Step 1: Choose Your Pattern

Ask yourself these questions in order:
1. **Is it a design token?** (font size, color, spacing) → Pattern 1 (Theme)
2. **Does it have a default that some utilities override?** (border-style: solid) → Pattern 2 (Property_default)
3. **Do multiple utilities contribute parts?** (transforms, filters) → Pattern 3 (Channel)
4. **Do utilities only reference it, never set it?** (shadow-color, ring-color) → Pattern 4 (Ref_only)
5. **Otherwise** → Pattern 5 (Always-set)

### Step 2: Implement According to Pattern

**Pattern 1 (Theme)**:
- Use `Var.theme kind name ~order:(group, index)`
- Set value once with `Var.binding`
- Reference in utilities with `Var` constructor

**Pattern 2 (Property_default)**:
- Use `Var.property_default kind ~initial name`
- Setting utilities: `Var.binding`
- Referencing utilities: `Var.reference` with `~property_rules`

**Pattern 3 (Channel)**:
- Use `Var.channel kind name`
- Contributing utilities: `Var.binding` to set channel
- Aggregator: `Var.reference` with fallback to compose

**Pattern 4 (Ref_only)**:
- Use `Var.ref_only kind name ~fallback`
- Reference with `Var.reference`
- Never set (only referenced with built-in fallback)

**Pattern 5 (Always-set)**:
- Use `Var.channel kind name`
- Always use `Var.binding` to set and reference

### Step 3: Let Build Handle Layers

`lib/build.ml` automatically:
- Collects Pattern 1 variables into `@layer theme`
- Generates `@layer properties` for Pattern 2 defaults
- Emits `@property` rules when needed
- Places utilities in `@layer utilities`

Good references

- Typography font sizes: `lib/typography.ml` (Theme variables with `Var.theme`)
- Typography font weight: `lib/typography.ml` (@property registration pattern)
- Borders pattern: `lib/borders.ml` (setting vs referencing utilities)
- Shadow utilities: `lib/effects.ml` (using `Var.reference` with fallback)
- Layer assembly: `lib/build.ml` (`theme_layer_of`, `properties_layer`)
- Module shape for a utility family: `lib/utility.mli`

Debugging utilities

To debug CSS generation issues, use the tw CLI tool. The commands below call `tw`
directly; from a checkout without it on `PATH`, prefix them with `dune exec --`.

<!-- $MDX skip -->
```sh
# Generate CSS for a class without the base layer (useful for test comparisons)
tw -s <class> --variables

# Include the base layer
tw -s <class> --variables --base

# Test with minification (critical for finding selector issues)
tw -s <class> --variables --minify
```

A style utility sets its variable and reads it back in the same rule:

```sh
$ tw -s border-none --variables
@layer theme, components, utilities;
@layer theme {

}
@layer components;
@layer utilities {
  .border-none {
    --tw-border-style: none;
    border-style: none;
  }
}
```

Minified, which is where a selector bug shows up:

```sh
$ tw -s border-none --variables --minify
@layer theme,components,utilities;@layer theme;@layer components;@layer utilities{.border-none{--tw-border-style:none;border-style:none}}
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

## Quick Templates for Each Pattern

### Pattern 1: Theme Variable Template
```ocaml
(* Define theme token with explicit order *)
let my_size_var =
  Var.theme Css.Length "my-size" ~order:(4, 900)

(* Set value once in theme *)
let size_def, size_var = Var.binding my_size_var (Rem 2.5)

(* Reference in utility *)
let my_utility =
  Style.style [
    size_def;  (* Include definition *)
    width (Var size_var);
    height (Var size_var)
  ]
```

### Pattern 2: Property_default Template
```ocaml
(* Variable with @property default *)
let my_content_var =
  Var.property_default Css.Content ~initial:(String "") "tw-my-content"

(* Setting utility *)
let my_setter value =
  let decl, var_ref = Var.binding my_content_var value in
  Style.style [ decl; content (Var var_ref) ]

(* Referencing utility with @property *)
let my_referencer =
  let var_ref = Var.reference my_content_var in
  Style.style ~property_rules:(Var.property_rules my_content_var) [
    content (Var var_ref)
  ]
```

### Pattern 3: Channel Template
```ocaml
(* Channel variables for composition *)
let channel_blur_var = Var.channel Css.Length "tw-channel-blur"
let channel_hue_var = Var.channel Css.Angle "tw-channel-hue"

(* Contributing utilities *)
let set_channel_blur value =
  let decl, _ = Var.binding channel_blur_var value in
  Style.style [ decl ]

(* Aggregator utility - use reference_with_fallback for channels *)
let aggregate =
  let blur_ref = Var.reference_with_fallback channel_blur_var Zero in
  let hue_ref = Var.reference_with_fallback channel_hue_var (Deg 0.) in
  Style.style [
    filter (List [ Blur (Var blur_ref); Hue_rotate (Var hue_ref) ])
  ]
```

### Pattern 4: Ref_only Template
```ocaml
(* Variable only referenced, never set here - use Var.channel *)
let color_override_var = Var.channel Css.Color "tw-my-color"

(* Reference with fallback using reference_with_fallback *)
let my_colored_thing =
  let color_ref =
    Var.reference_with_fallback color_override_var (Css.hex "#0000001a")
  in
  Style.style [
    background_color (Var color_ref)
  ]

(* Set elsewhere (different module) *)
let my_thing_red =
  let decl, _ = Var.binding color_override_var (Css.hex "#ff0000") in
  Style.style [ decl ]
```

### Pattern 5: Always-set Template
```ocaml
(* Variable always set when used *)
let my_value_var =
  Var.channel Css.Length "tw-my-value"

(* Every utility sets and uses *)
let my_small =
  let decl, var_ref = Var.binding my_value_var (Px 10.) in
  Style.style [ decl; padding [ Var var_ref ] ]

let my_large =
  let decl, var_ref = Var.binding my_value_var (Px 40.) in
  Style.style [ decl; padding [ Var var_ref ] ]
```

Common pitfalls and solutions

**Rule Ordering Issues**
- **Problem**: Child selectors appearing before parent selectors (e.g., `.prose :where(p)` before `.prose`)
- **Solution**: Adjust the family's `suborder` function to order the variants explicitly
- **Debugging**: `Build.indexed_rules` and `Build.compare_rules` are exposed so the comparator can be exercised on the values it really sees; `test/test_sort.ml` does this
- **Key insight**: The order matters for CSS cascade - parent rules should come first

**CSS Minification Breaking Selectors**
- **Problem**: Minifier removing spaces in descendant combinators, changing selector meaning
- **Solution**: Fix it in cascade, in `Selector.to_string ~minify:true`, not by rewriting the selector here
- **Example**: `.prose :where(p)` (descendant) vs `.prose:where(p)` (direct) have different meanings
- **Testing**: Always test with `--minify` flag to catch these issues early

**CRITICAL: Never use var(...) in string literals or assembled token strings**
- **Never do this**: `Css.custom_property "--tw-shadow" "var(--tw-shadow-color)"`
- **Never do this**: `box_shadow (Raw "var(--tw-shadow)")`
- **Why**:
  - String literals containing `var(...)` break the ability to track dependencies, resolve variables, and generate correct layers
  - Assembling a value as a string bypasses the type system and makes the code fragile
- **Instead**: Always use typed `Var` references from `Var.theme`, `Var.channel`, or `Var.property_default`
- **Exception**: A variable whose value legitimately holds a stop list or a token stream (Tailwind v4's `--tw-gradient-stops`) still gets a typed kind. `backgrounds.ml` declares it `Var.property_default Gradient_stop ~universal:true`, which registers `syntax: "*"` while keeping the OCaml value typed
- **If you need new functionality**: Extend the type system properly. Add new
  constructors to existing types following the standard pattern, which every
  cascade value type follows:

```ocaml
type my_shadow =
  | Shadow of shadow
  | Shadows of shadow list
  | None
  | Var of my_shadow var  (* Standard pattern: Var (no suffix), typed variable *)

let _ : my_shadow = Shadows []
```

The naming convention is always `Var`, never `Var_list` or `Var_composition`,
and the constructor holds a typed variable reference created by `Var.channel`,
`Var.property_default` or `Var.theme`. Never work around the type system with
strings.

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
   - **Solution**: Pass `~universal:true` to `Var.property_default`, which registers `syntax: "*"` while the OCaml value stays typed
   - **Example**: `Var.property_default Css.Border_style ~initial:Solid ~universal:true "tw-border-style"`

4. **Variable ordering in theme layer**
   - **Problem**: Variables appearing in wrong order in `@layer theme`, causing test failures
   - **Solution**: Fix the `~order:(group, index)` pair on the variable definition; the build reads it from the declaration's metadata
   - **Note**: Equal pairs are allowed. The build resolves a tie from the active theme's declaration order, so one request cannot affect another

5. **Font-variant-numeric empty fallback handling**
   - **Problem**: Font-variant-numeric variables need trailing commas in CSS but a normal fallback generates the wrong syntax
   - **Solution**: Pass `~fallback:Css.Empty` to `Var.binding` (or use `Var.reference_with_empty_fallback` where there is nothing to bind)
   - **Result**: Generates `var(--tw-ordinal,)` with trailing comma as required by Tailwind v4

6. **Helper functions to reduce duplication**
   - **Problem**: Repetitive code for similar utilities (e.g., all font-variant-numeric utilities had similar structure)
   - **Solution**: Create helper functions with optional parameters using OCaml's labeled argument pattern
   - **Example**: `?(param : type = default_value)` allows callers to override only what they need
   - **Key**: Pass the variable reference (from `Var.channel` or similar) to the helper, not the definition
