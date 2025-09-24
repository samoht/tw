# Variable API Design Issue: @Property Initial Values

## Problem

Our current variable API has a mismatch when dealing with @property initial values. The issue occurs when:

1. A variable has `~property:(Some value, inherits)` defining an @property initial value
2. Some utilities should rely on this @property default without setting the variable
3. Our API requires explicit `Var.binding` calls, making inline mode semantics unclear

**Example from borders:**
```ocaml
(* Variable with @property initial value *)
let border_style_var = Var.create Border_style "tw-border-style" ~layer:Utility
  ~property:(Some Solid, false)

(* Utility that should rely on @property default - CURRENT PROBLEM *)
let border =
  (* What should we do here? *)
  (* - Can't ignore declaration (violates three rules) *)
  (* - Setting declaration defeats the purpose of @property default *)
  (* - Inline mode needs a clear value *)
```

**Tailwind v4 expects:**
- Variables mode: `.border { border-style: var(--tw-border-style); }` (relies on @property)
- Inline mode: `.border { border-style: solid; }` (uses concrete value)

## Root Cause

The three-rule policy assumes every variable reference has a corresponding declaration. But @property variables have a different model: some utilities set the variable, others rely on the @property default.

## Solution A: Add `Var.reference` Function (Implemented)

Extend the API with a new function that handles @property defaults correctly:

```ocaml
val reference : 'a t -> 'a Css.var
(** [reference var] creates a variable reference that uses the @property
    initial value for inline mode. Only works for variables with ~property.

    - Variables mode: generates var(--name)
    - Inline mode: uses the @property initial value directly
*)
```

## Solution B: Explicit Property Rules at Binding Site (Better?)

Make property rules explicit where variables are bound, rather than scattered across utilities:

```ocaml
(* Modify binding to return property rules when needed *)
val binding : 'a t -> ?fallback:'a Css.fallback -> 'a ->
  Css.declaration * 'a Css.var * Css.t option

(* Or separate functions for different use cases *)
val binding_with_property : 'a t -> ?fallback:'a Css.fallback -> 'a ->
  Css.declaration * 'a Css.var * Css.t

val binding_no_property : 'a t -> ?fallback:'a Css.fallback -> 'a ->
  Css.declaration * 'a Css.var
```

**Usage Example:**
```ocaml
(* Utility that needs @property registration *)
let shadow_sm =
  let decl, var_ref, property_rules = Var.binding_with_property shadow_var value in
  style "shadow-sm" ~property_rules [decl; box_shadow (Var var_ref)]

(* Utility that sets variable but doesn't need @property *)
let shadow_lg =
  let decl, var_ref = Var.binding_no_property shadow_var lg_value in
  style "shadow-lg" [decl; box_shadow (Var var_ref)]

(* Utility that only references with @property default *)
let border =
  let var_ref = Var.reference border_style_var in
  style "border" [border_style (Var var_ref)]
```

## Implementation Plan

1. **Add `Var.reference` function** that:
   - Takes a variable with `~property` metadata
   - Returns just the `'a Css.var` (no declaration)
   - Uses @property initial value for inline mode fallback
   - Validates that the variable has `~property` metadata

2. **Update the three rules** to be:
   - Rule 1: Use `Var.binding` when you need declaration + variable
   - Rule 2: Use `Var.reference` when you need only variable reference with @property default
   - Rule 3: Pass as function parameters otherwise

3. **Example usage:**
```ocaml
(* Variable with @property *)
let border_style_var = Var.create Border_style "tw-border-style" ~layer:Utility
  ~property:(Some Solid, false)

(* Utility that relies on @property default *)
let border =
  let border_ref = Var.reference border_style_var in
  style "border" [ border_style (Var border_ref); border_width (Px 1.) ]

(* Utility that sets the variable *)
let border_dashed =
  let decl, var_ref = Var.binding border_style_var Dashed in
  style "border-dashed" [ decl; border_style (Var var_ref) ]
```

4. **Inline mode behavior:**
   - `border`: generates `border-style: solid; border-width: 1px`
   - `border-dashed`: generates `border-style: dashed`

## Benefits

### Solution A Benefits:
- Maintains simple inline mode semantics (single source of truth)
- Preserves the three-rule philosophy with clear extension
- Handles @property defaults correctly
- No more "ignoring declarations" anti-patterns
- Clear distinction between utilities that set vs. reference variables

### Solution B Benefits (More Explicit):
- **Explicit intent**: Clear at the binding site whether @property is needed
- **No scattered property_rules**: Property emission is tied to variable usage, not utility creation
- **Better composability**: Functions can choose whether they need @property registration
- **Clearer API**: Three distinct use cases with three distinct functions
  - `binding_with_property`: "I'm setting this variable AND need @property"
  - `binding_no_property`: "I'm setting this variable but don't need @property"
  - `reference`: "I'm only referencing with @property default"
- **Easier reasoning**: Property emission is predictable and localized

## Current Implementation Issues

### Problem with `let decl, _var_ref = Var.binding`

The current borders.ml implementation has a concerning pattern:

```ocaml
(* Helper for border style utilities that set the variable *)
let border_style_util class_name border_style_value =
  let decl, _var_ref = Var.binding border_style_var border_style_value in
  style class_name (decl :: [ border_style border_style_value ])
```

**Issues:**
1. **Violates three-rule policy**: We're ignoring the `_var_ref` part
2. **Unclear inline semantics**: What should happen in inline mode?
3. **Inconsistent patterns**: Some utilities use `Var.reference`, others ignore binding results

### Current Test Behavior
- `border-none` alone: Generates NO `@property` rule ✓ (correct)
- `border` alone: Generates `@property` rule ✓ (correct)
- `border` uses: `border-style:var(--tw-border-style)` ✓ (correct)
- `border-none` uses: `--tw-border-style:none; border-style:none` ✓ (correct)

### The Pattern That's Emerging
```ocaml
(* Utilities that REFERENCE with @property default - include property_rules *)
let border =
  let border_ref = Var.reference border_style_var in
  let property_rule = match Var.property_rule border_style_var with
    | Some rule -> rule | None -> Css.empty in
  style "border" ~property_rules:property_rule [border_style (Var border_ref)]

(* Utilities that SET the variable - ignore var_ref *)
let border_none =
  let decl, _var_ref = Var.binding border_style_var None in
  style "border-none" [decl; border_style None]
```

**This works but violates our principles!**

## Solution B Implementation Plan

Replace the current pattern with explicit binding types:

```ocaml
(* For utilities that need @property registration *)
val binding_with_property : 'a t -> ?fallback:'a Css.fallback -> 'a ->
  Css.declaration * 'a Css.var * Css.t

(* For utilities that set variables without @property *)
val binding_no_property : 'a t -> ?fallback:'a Css.fallback -> 'a ->
  Css.declaration * 'a Css.var

(* For utilities that only reference with @property defaults *)
val reference : 'a t -> 'a Css.var  (* already implemented *)
```

**Clean usage:**
```ocaml
(* Reference with @property - explicit property rule returned *)
let border =
  let var_ref, property_rules = Var.reference_with_property border_style_var in
  style "border" ~property_rules [border_style (Var var_ref)]

(* Set without @property - use BOTH declaration and var_ref *)
let border_none =
  let decl, var_ref = Var.binding_no_property border_style_var None in
  style "border-none" [decl; border_style (Var var_ref)]
  (* ☝️ CRITICAL: Use var_ref, don't duplicate the None value *)
```

This eliminates the `_var_ref` ignoring pattern and makes @property emission explicit and predictable.

## Migration

1. Implement `binding_with_property` and `binding_no_property`
2. Replace current patterns with explicit binding types
3. Ensure no more `let decl, _ignored = Var.binding` patterns