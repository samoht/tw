Title: CSS Variables in Utilities - The Variable Pattern System

This doc explains the CSS variable patterns used in Tailwind CSS v4 and how to implement them correctly in OCaml. There are 4 core variable types (`Var.theme`, `Var.property_default`, `Var.channel`, `Var.ref_only`) with 5 usage patterns. Each pattern has specific use cases and constraints.

## The Variable Patterns

Every CSS variable in our system follows one of these patterns (4 variable types, 5 usage patterns):

### Pattern 1: Theme Variables
**Purpose**: Design tokens set once in theme layer, referenced by utilities
**Examples**: `--text-xl: 1.25rem`, `--font-weight-bold: 700`, `--color-blue-500: #3b82f6`
**Layer**: `@layer theme` in `:root,:host`
**Usage**: `Var.theme kind name ~order:N` and set via `Var.binding` once

### Pattern 2: Property_default Variables
**Purpose**: Variables with @property defaults that utilities can either SET or REFERENCE
**Examples**: `--tw-border-style` (default: solid), `--tw-border-opacity` (default: 1)
**Layer**: `@layer properties` for defaults, `@property` rules at top level
**Usage**: Use `Var.property_default kind ~initial ~inherits? ~universal? name` (initial is mandatory). Referencing utilities call `Var.reference` and include `~property_rules:(Var.property_rule var)`

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

- `Var.property_rule`: generates @property rule (Pattern 2 only)
  - Returns `Some rule` if variable has property metadata
  - Pass to `style ~property_rules` to generate @property

## Pattern Examples

### Pattern 1: Theme Variables (Typography)
```ocaml
(* Define theme variables with explicit ordering *)
let text_xl_size_var = Var.theme Css.Length "text-xl" ~order:110
let text_xl_lh_var = Var.theme Css.Length "text-xl--line-height" ~order:111

(* Set theme values once *)
let size_def, size_var = Var.binding text_xl_size_var (Rem 1.25)
let lh_def, lh_var = Var.binding text_xl_lh_var (Rem 1.75)

(* Utility references theme variables *)
style "text-xl" [
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
  style "border-solid" [ decl; border_style (Var var_ref) ]

(* Referencing utility - uses @property default *)
let border =
  let var_ref = Var.reference border_style_var in
  let property_rule = match Var.property_rule border_style_var with
    | Some rule -> rule | None -> Css.empty in
  style "border" ~property_rules:property_rule [
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
  style "translate-x-4" [ decl ]

let rotate_45 =
  let decl, _ = Var.binding rotate_var (Deg 45.) in
  style "rotate-45" [ decl ]

(* Aggregator utility combines all channels - use reference_with_fallback for channels *)
let transform =
  let tx_ref = Var.reference_with_fallback translate_x_var Zero in
  let rot_ref = Var.reference_with_fallback rotate_var (Deg 0.) in
  let scale_ref = Var.reference scale_var in  (* property_default has built-in default *)
  style "transform" [
    transform (Transform [
      TranslateX (Var tx_ref);
      Rotate (Var rot_ref);
      Scale (Var scale_ref)
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
  style "shadow-sm" [
    box_shadow (Shadow [0; 1; 3; 0; Var color_ref])
  ]

(* Color utilities set the variable (in different module) *)
let shadow_red_500 =
  let decl, _ = Var.binding shadow_color_var (Css.hex "#ef4444") in
  style "shadow-red-500" [ decl ]
```

### Pattern 5: Always-set Variables (Font Weight)
```ocaml
(* Variable always set when used *)
let font_weight_var = Var.channel Css.Font_weight "tw-font-weight"

(* Every utility sets and uses it *)
let font_bold =
  let decl, var_ref = Var.binding font_weight_var (Int 700) in
  style "font-bold" [
    decl;
    font_weight (Var var_ref)
  ]

let font_thin =
  let decl, var_ref = Var.binding font_weight_var (Int 100) in
  style "font-thin" [
    decl;
    font_weight (Var var_ref)
  ]
```

How Rules turns this into layers

- `compute_theme_layer` scans used styles, extracts custom declarations (from `Var.theme`), adds a small set of default vars (font families), and emits them under `@layer theme` in `:root,:host` with a stable order.
  
  **Variable Ordering**: The `order` function in `var.ml` determines the canonical ordering of variables in the theme layer. Each variable type has a numeric order value that ensures consistent output matching Tailwind v4. For example:
  - Font families: 0-10
  - Font sizes and line heights: 100-199  
  - Font weights: 200-209
  - Border radius: 300-307
  - Default font families: 400-405
  
- `build_properties_layer` collects all `property_rules` attached to used utilities (from `Var.property_rule ...`) and emits an `@layer properties` that contains a single `@supports(...) { ... }` block with default custom-property values targeting `*, :before, :after, ::backdrop` (not the `@property` rules themselves).
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
- Use `Var.theme kind name ~order:N`
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

### Step 3: Let Rules Handle Layers

The Rules engine automatically:
- Collects Pattern 1 variables into `@layer theme`
- Generates `@layer properties` for Pattern 2 defaults
- Emits `@property` rules when needed
- Places utilities in `@layer utilities`

Good references

- Typography font sizes: `lib/typography.ml` (Theme variables with `Var.theme`)
- Typography font weight: `lib/typography.ml` (@property registration pattern)
- Borders pattern: `lib/borders.ml` (setting vs referencing utilities)
- Shadow utilities: `lib/effects.ml` (using `Var.reference` with fallback)
- Layer assembly: `lib/rules.ml` (`compute_theme_layer`, `build_properties_layer`)

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

## Quick Templates for Each Pattern

### Pattern 1: Theme Variable Template
```ocaml
(* Define theme token with explicit order *)
let my_size_var =
  Var.theme Css.Length "my-size" ~order:150

(* Set value once in theme *)
let size_def, size_var = Var.binding my_size_var (Rem 2.5)

(* Reference in utility *)
let my_utility =
  style "my-class" [
    size_def;  (* Include definition *)
    width (Var size_var);
    height (Var size_var)
  ]
```

### Pattern 2: Property_default Template
```ocaml
(* Variable with @property default *)
let my_style_var =
  Var.property_default Css.My_type ~initial:default_value "tw-my-style"

(* Setting utility *)
let my_setter value =
  let decl, var_ref = Var.binding my_style_var value in
  style "my-setter" [ decl; my_property (Var var_ref) ]

(* Referencing utility with @property *)
let my_referencer =
  let var_ref = Var.reference my_style_var in
  let property_rule = match Var.property_rule my_style_var with
    | Some rule -> rule | None -> Css.empty in
  style "my-ref" ~property_rules:property_rule [
    my_property (Var var_ref)
  ]
```

### Pattern 3: Channel Template
```ocaml
(* Channel variables for composition *)
let channel_a_var = Var.channel Css.Length "tw-channel-a"
let channel_b_var = Var.channel Css.Angle "tw-channel-b"

(* Contributing utilities *)
let set_channel_a value =
  let decl, _ = Var.binding channel_a_var value in
  style "channel-a" [ decl ]

(* Aggregator utility - use reference_with_fallback for channels *)
let aggregate =
  let a_ref = Var.reference_with_fallback channel_a_var Zero in
  let b_ref = Var.reference_with_fallback channel_b_var (Deg 0.) in
  style "aggregate" [
    my_composite_property [Var a_ref; Var b_ref]
  ]
```

### Pattern 4: Ref_only Template
```ocaml
(* Variable only referenced, never set here - use Var.channel *)
let color_override_var = Var.channel Css.Color "tw-my-color"

(* Reference with fallback using reference_with_fallback *)
let my_colored_thing =
  let color_ref = Var.reference_with_fallback color_override_var default_color in
  style "my-thing" [
    background_color (Var color_ref)
  ]

(* Set elsewhere (different module) *)
let my_thing_red =
  let decl, _ = Var.binding color_override_var (Css.hex "#ff0000") in
  style "my-thing-red" [ decl ]
```

### Pattern 5: Always-set Template
```ocaml
(* Variable always set when used *)
let my_value_var =
  Var.channel Css.Length "tw-my-value"

(* Every utility sets and uses *)
let my_small =
  let decl, var_ref = Var.binding my_value_var (Px 10.) in
  style "my-small" [ decl; padding (Var var_ref) ]

let my_large =
  let decl, var_ref = Var.binding my_value_var (Px 40.) in
  style "my-large" [ decl; padding (Var var_ref) ]
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
- **Instead**: Always use typed `Var` references from `Var.theme`, `Var.channel`, or `Var.property_default`
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
  - The `Var` constructor holds a typed variable reference created by `Var.channel`, `Var.property_default`, or `Var.theme`
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
   - **Key**: Pass the variable reference (from `Var.channel` or similar) to the helper, not the definition
