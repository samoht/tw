# CSS Variable Patterns - Pure API Design

## Core Problem

We have 5 distinct variable patterns but our current API makes them easy to misuse. We need a pure, simple API that encodes intent without global state or complex migration machinery.

## The Five Patterns (Simplified)

### 1. Property_default Variables (@property pattern)
**Example**: `--tw-border-style` with @property initial value of `solid`
- Some utilities SET it: `border-solid` sets `--tw-border-style: solid`
- Other utilities REFERENCE it: `border` uses `var(--tw-border-style)` relying on @property default
- **Current API**: Variable has `~property:(Some Solid, false)`, referencing utilities use `Var.reference` with `~property_rules`

### 2. Composition Variables (always set before use)
**Example**: `--tw-translate-x`, `--tw-rotate` in transforms
- Each utility sets its own variable: `translate-x-4` sets `--tw-translate-x: 1rem`
- Aggregator utility combines them: `.transform` uses all transform variables
- **Current API**: Regular `Var.binding`, no @property needed

### 3. Ref_only Variables (reference-only with fallback)
**Example**: `--tw-shadow-color` in shadow utilities
- Shadow utilities reference with fallback: `var(--tw-shadow-color, #0000001a)`
- Never set by shadow utilities themselves (set by `shadow-red-500` etc.)
- **Use case**: Variables that utilities only reference but never set
- **Current API**: `Var.reference` with `~fallback` parameter, no @property

### 4. Always-Set Variables (set when used)
**Example**: `--tw-font-weight` in typography
- Every utility that uses it also sets it: `font-bold` both sets and uses `--tw-font-weight`
- **Current API**: Standard `Var.binding` pattern

### 5. Theme Tokens (set once, referenced many)
**Example**: `--text-xl: 1.25rem` in theme layer
- Defined once in theme layer, referenced by utilities
- **Current API**: `Var.create` with `~layer:Theme`, one place does `Var.binding`

## Current API Problems

1. **Too many ways to misuse variables**:
   - Can ignore declarations (`let _, var_ref = Var.binding`)
   - Can forget to pass `~property_rules`
   - Can create Theme variables without `~order`
   - Can mix patterns incorrectly

2. **No way to define never-set variables**:
   - Extension variables that are only referenced, never set by core
   - User theme variables that we reference but don't define

3. **Unclear semantics**:
   - When should you use `Var.binding` vs `Var.reference`?
   - What's the relationship between @property and fallback?

## Minimal Pure API with Phantom Types

```ocaml
module Var : sig
  (* Phantom types for roles - zero runtime cost *)
  type property_default = [`Property_default]
  type channel = [`Channel]
  type ref_only = [`Ref_only]
  type token = [`Token]

  (* Subtyping for operations *)
  type settable = [property_default | channel | token]
  type any_role = [settable | ref_only]

  (* Variable type with phantom role parameter *)
  type ('a, 'role) t

  (* Internal storage for inline mode *)
  type 'a inline_value =
    | Initial of 'a         (* From @property initial *)
    | Identity of 'a        (* Channel identity value *)
    | Fallback of 'a        (* Reference fallback *)
    | Theme of 'a           (* Theme token value *)

  (* Core constructors return role-specific types *)
  val property_default :
    'a Css.kind -> string -> initial:'a -> ?inherits:bool -> unit ->
    ('a, property_default) t

  val channel :
    'a Css.kind -> string -> identity:'a -> unit ->
    ('a, channel) t

  val ref_only :
    'a Css.kind -> string -> fallback:'a -> unit ->
    ('a, ref_only) t

  val token :
    'a Css.kind -> string -> value:'a -> order:int -> unit ->
    ('a, token) t
    (* value is the theme default, used in both theme layer and inline mode *)

  (* Operations respect roles via phantom types *)
  val set : ('a, [> settable]) t -> 'a -> Css.declaration option
    (* Compile-time error if used on ref_only *)

  val ref : ('a, 'role) t -> 'a Css.var
    (* Works on all roles - var carries inline_value for inline mode *)

  val property_rule : ('a, property_default) t -> 'a Css.property_rule
    (* Only available for property_default *)

  val order : ('a, token) t -> int
    (* Only available for tokens *)

  (* Combinator for user-defined variables *)

  (* Heterogeneous collections via existentials *)
  type any = Any : ('a, 'role) t -> any
  val to_any : ('a, 'role) t -> any

  (* Helper for extracting inline values *)
  val inline_value : ('a, 'role) t -> 'a
    (* Returns the appropriate value based on role:
       - property_default -> initial
       - channel -> identity
       - ref_only -> fallback
       - token -> theme value *)
end
```

## Usage Examples

```ocaml
(* Pattern 1: Property_default - variables with @property defaults *)
let border_style =
  Var.property_default Css.Border_style "tw-border-style"
    ~initial:Solid ()
(* Type: (Border_style.t, Var.property_default) Var.t *)

(* Setting utility - set works because property_default is settable *)
let border_solid =
  match Var.set border_style Solid with
  | Some decl -> style "border-solid" [ decl; border_style (Var (Var.ref border_style)) ]
  | None -> assert false  (* Can't happen - type system guarantees settable *)

(* Referencing utility - property_rule only available for property_default *)
let border =
  let var_ref = Var.ref border_style in
  let prop = Var.property_rule border_style in  (* No option - guaranteed by type *)
  style "border" ~extra_rules:[prop] [
    border_style (Var var_ref); border_width (Px 1.)
  ]

(* Pattern 2: Channel *)
let translate_x =
  Var.channel Css.Length "tw-translate-x" ~identity:Zero ()
(* Type: (Length.t, Var.channel) Var.t *)

let translate_x_4 =
  match Var.set translate_x (Rem 1.) with
  | Some decl -> style "translate-x-4" [ decl ]
  | None -> assert false  (* Can't happen - channels are settable *)

(* Pattern 3: Ref_only - reference with fallback *)
let shadow_color =
  Var.ref_only Css.Color "tw-shadow-color"
    ~fallback:(Css.hex "#0000001a") ()
(* Type: (Color.t, Var.ref_only) Var.t *)

(* Shadow utilities just reference it - can't set it *)
let shadow_sm =
  let color_ref = Var.ref shadow_color in
  style "shadow-sm" [
    box_shadow (Shadow [...(Var color_ref)...])
  ]
(* Note: Var.set shadow_color would be a compile-time error! *)

(* Color utilities can set the variable (they use a different pattern) *)
let shadow_red_500 =
  (* This would be a separate settable variable *)
  style "shadow-red-500" [
    (* Sets --tw-shadow-color: #ef4444 *)
  ]

(* Pattern 4: Token with theme value *)
let text_xl_token =
  Var.token Css.Length "text-xl" ~value:(Rem 1.25) ~order:110 ()
(* Type: (Length.t, Var.token) Var.t *)

(* Theme layer generation uses the value *)
let theme_layer =
  let decl = Var.set text_xl_token (Var.inline_value text_xl_token) in
  style ":root" [ Option.get decl ]
  (* Generates: :root { --text-xl: 1.25rem; } *)

(* Utility references it *)
let text_xl =
  let size_ref = Var.ref text_xl_token in
  style "text-xl" [ font_size (Var size_ref) ]

(* Heterogeneous collection for property rules *)
let collect_properties vars =
  vars
  |> List.filter_map (fun (Var.Any var) ->
      match var with
      | (var : (_, Var.property_default) Var.t) ->
          Some (Var.property_rule var)
      | _ -> None)
```


## Pure Rendering Pipeline

No global state, no caches, just a pure rendering pass:

```ocaml
type render_config = {
  support_property : bool;  (* Browser supports @property *)
  mode : [ `Variables | `Inline ];
}

(* Collect all property rules from stylesheet - pure *)
let collect_properties stylesheet =
  stylesheet
  |> List.filter_map (fun style ->
      style.vars |> List.filter_map Var.property_rule)
  |> List.flatten
  |> dedup_by (fun p -> (p.name, p.kind, p.inherits, p.initial))
  |> List.sort compare  (* Deterministic order *)

(* Render based on config - pure transformation *)
let render config stylesheet =
  let properties =
    if config.support_property then
      collect_properties stylesheet
    else [] in

  match config.mode with
  | `Variables ->
      (* Emit properties at top, then stylesheet *)
      Css.concat [
        Css.properties properties;
        Css.styles stylesheet
      ]
  | `Inline ->
      (* Transform var() to concrete values *)
      stylesheet
      |> List.map (inline_transform config)
      |> Css.styles
```

## Why This Works

### No Global State
- Variables are pure values with roles
- Property collection happens in render pass
- Deduplication via Set during rendering
- Deterministic output through sorting

### Theme Ordering With Explicit Order
```ocaml
(* Theme tokens require explicit order - enforced by API *)
let font_sans = Var.token Css.String "font-sans" ~order:0 ()
let font_mono = Var.token Css.String "font-mono" ~order:1 ()
let text_xs = Var.token Css.Length "text-xs" ~order:100 ()
let text_sm = Var.token Css.Length "text-sm" ~order:101 ()
let text_base = Var.token Css.Length "text-base" ~order:102 ()

(* Render pass sorts by order *)
let render_theme tokens =
  tokens
  |> List.filter_map (fun t ->
      match Var.order t with
      | Some o -> Some (o, t)
      | None -> None)
  |> List.sort (fun (o1,_) (o2,_) -> Int.compare o1 o2)
  |> List.map snd
```

This gives us:
- **Explicit intent**: Order is part of token definition
- **Type safety**: Can't create theme token without order
- **Deterministic output**: Always sorted by explicit order
- **No surprises**: Order is visible at definition site

### Inline Mode Without Complexity
```ocaml
let inline_transform config style =
  style.declarations
  |> List.map (fun decl ->
    match decl with
    | Css.Var var when config.mode = `Inline ->
        (* Use inline_value helper - it knows what value to use based on role *)
        Var.inline_value var
    | other -> other)

(* The Var.ref function returns a var that carries the inline value: *)
let ref : type a role. (a, role) t -> a Css.var = fun var ->
  (* The returned var reference knows its inline value based on role *)
  match var with
  | Property_default { initial; _ } -> make_var ~name ~inline:initial
  | Channel { identity; _ } -> make_var ~name ~inline:identity
  | Ref_only { fallback; _ } -> make_var ~name ~inline:fallback
  | Token { value; _ } -> make_var ~name ~inline:value
```
## Key Insights

### Simplicity Wins
- One `Var.t` type with roles is cleaner than many abstract types
- Pure rendering pass handles all dedup/ordering
- No global state means easier testing and reasoning

### Pattern Enforcement via Linting
Instead of complex type machinery, use simple warnings:
```ocaml
let lint_ignored_declaration style =
  List.iter (fun var ->
    match Var.set var dummy_value with
    | Some decl when not (List.mem decl style.declarations) ->
        warn "Declaration for %s created but not used" var.name
    | _ -> ()
  ) style.vars

let lint_missing_property style =
  List.iter (fun var ->
    match Var.property_rule var with
    | Some _ when not style.has_property_rules ->
        warn "%s needs @property but ~extra_rules not passed" var.name
    | _ -> ()
  ) style.vars
```


## Enforcing Correct Usage Through Combinators

Instead of complex types, use combinators that guide correct patterns:

```ocaml
(* For property_default - combinator ensures property rule is handled *)
let with_default var f =
  match Var.property_rule var with
  | Some prop -> f ~prop (Var.ref var)
  | None -> f ~prop:Css.empty (Var.ref var)

(* Usage - can't forget property rule *)
let border =
  with_default border_style_var (fun ~prop var_ref ->
    style "border" ~extra_rules:[prop] [
      border_style (Var var_ref);
      border_width (Px 1.)
    ])

(* For settable variables - combinator ensures declaration is used *)
let with_set var value f =
  match Var.set var value with
  | Some decl -> f decl (Var.ref var)
  | None -> failwith (var.name ^ " is not settable")

(* Usage - can't ignore declaration *)
let border_solid =
  with_set border_style_var Solid (fun decl var_ref ->
    style "border-solid" [ decl; border_style (Var var_ref) ])

(* For channels - combinator for contribution *)
let contribute_to channel value f =
  match Var.set channel value with
  | Some decl -> f decl
  | None -> failwith "channels must be settable"

(* Usage *)
let translate_x_4 =
  contribute_to translate_x_var (Rem 1.) (fun decl ->
    style "translate-x-4" [ decl ])
```

## Summary

### The Pure Approach
- **No global state**: Everything is pure values + rendering pass
- **Simple API**: One `Var.t` type with roles, not many abstract types
- **Deterministic**: Dedup and sort in render pass, not via caches
- **Testable**: Pure functions are easy to test
- **Safe by construction**: Combinators enforce correct patterns

### What We Need
1. **Single variable type** with role to encode intent
2. **Pure render pass** that collects and dedupes @property rules
3. **Smart combinators** that make wrong usage impossible
4. **Theme tokens with required order** (enforced at construction)

### What We Don't Need
- Global property caches
- Complex migration machinery
- Dependency graphs (unless cycles become a real problem)
- Many abstract types
- Heavy type machinery

## Implementation Plan

1. **Add role to current Var.t**:
   ```ocaml
   type 'a t = {
     kind : 'a Css.kind;
     name : string;
     role : 'a role;
   }
   ```

2. **Update operations to respect roles**:
   - `set` returns `None` for `ref_only`
   - `property_rule` returns `Some` only for `property_default`

3. **Add convenience constructors** for common patterns

4. **Implement pure render pass** for property collection

5. **Add simple linter** for common mistakes

This gives us the benefits of pattern safety without the complexity of global state or heavy abstractions.

## Inline Mode Semantics

How each pattern behaves when generating inline CSS (no variables):

### Pattern 1: Property_default Variables
**Variables mode**:
- Setting: `.border-solid { --tw-border-style: solid; border-style: solid }`
- Referencing: `.border { border-style: var(--tw-border-style) }`

**Inline mode**:
- Setting: `.border-solid { border-style: solid }`
- Referencing: `.border { border-style: solid }` (uses @property initial value)

**Semantics**: The @property initial value serves as the inline default

### Pattern 2: Composition Variables
**Variables mode**:
- `.translate-x-4 { --tw-translate-x: 1rem }`
- `.rotate-45 { --tw-rotate: 45deg }`
- `.transform { transform: translateX(var(--tw-translate-x)) rotate(var(--tw-rotate)) }`

**Inline mode**:
- `.translate-x-4 { /* nothing - just marks presence */ }`
- `.rotate-45 { /* nothing - just marks presence */ }`
- `.transform { transform: translateX(1rem) rotate(45deg) }` (combines active utilities)

**Semantics**: The aggregator utility collects values from all active utilities

### Pattern 3: Ref_only Variables
**Variables mode**:
- `.shadow-sm { box-shadow: ... var(--tw-shadow-color, #0000001a) ... }`

**Inline mode**:
- `.shadow-sm { box-shadow: ... #0000001a ... }` (uses fallback value)

**Semantics**: The fallback value becomes the inline value. Utilities reference the variable but never set it.

### Pattern 4: Always-Set Variables
**Variables mode**:
- `.font-bold { --tw-font-weight: 700; font-weight: var(--tw-font-weight) }`

**Inline mode**:
- `.font-bold { font-weight: 700 }`

**Semantics**: The value passed to binding becomes the inline value

### Pattern 5: Theme Tokens
**Variables mode**:
- Theme layer: `:root { --text-xl: 1.25rem }`
- Utility: `.text-xl { font-size: var(--text-xl) }`

**Inline mode**:
- `.text-xl { font-size: 1.25rem }` (uses theme value directly)

**Semantics**: The theme value is inlined directly

## Tailwind Variable Overriding Patterns

Tailwind v4 uses several patterns for variable overriding that we need to support:

### 1. Direct Override Pattern (shadow-color)
**Tailwind behavior**:
```css
.shadow-sm {
  --tw-shadow: 0 1px 3px 0 var(--tw-shadow-color, #0000001a);
  box-shadow: var(--tw-shadow);
}
.shadow-red-500 {
  --tw-shadow-color: #ef4444;
}
```

**Our approach**: Use Pattern 3 (ref_only)
- Shadow utilities reference `--tw-shadow-color` with fallback
- Color utilities set `--tw-shadow-color`
- No declaration in shadow utilities (they only reference)
- This creates an extension point for user customization

### 2. Composition Override Pattern (transforms)
**Tailwind behavior**:
```css
.translate-x-4 { --tw-translate-x: 1rem; }
.scale-110 { --tw-scale: 1.1; }
.transform {
  transform: translateX(var(--tw-translate-x)) scale(var(--tw-scale));
}
```

**Our approach**: Use Pattern 2 (composition)
- Each utility sets its own variable
- Transform utility composes all variables
- Order matters: last utility wins if same variable set twice

### 3. Cascade Override Pattern (text-size/leading)
**Tailwind behavior**:
```css
.text-xl {
  --tw-leading: 1.75rem;  /* default for text-xl */
  font-size: 1.25rem;
  line-height: var(--tw-leading);
}
.leading-tight {
  --tw-leading: 1.25;  /* overrides text-xl's default */
}
```

**Our approach**: Modified Pattern 4
- Text utilities set `--tw-leading` with their default
- Leading utilities override `--tw-leading`
- Uses CSS cascade: later class wins

### 4. Property Channel Pattern (border-style)
**Tailwind behavior**:
```css
/* Default via @property */
@property --tw-border-style {
  initial-value: solid;
}
.border { border-style: var(--tw-border-style); }
.border-dashed { --tw-border-style: dashed; }
```

**Our approach**: Use Pattern 1 (property_default)
- @property provides default
- Style utilities override
- Width utilities reference current value

## Controlling Declarations in Our Scheme

To maintain control over declarations while supporting Tailwind patterns:

### Principle: Single Source of Truth
Each variable has exactly ONE of:
1. **@property initial value** (for property_default pattern)
2. **Runtime fallback** (for ref_only pattern)
3. **Explicit binding** (for always-set pattern)
4. **Theme definition** (for theme tokens)
5. **Aggregated from utilities** (for composition pattern)

### Declaration Control Rules

1. **Never duplicate values**:
   ```ocaml
   (* BAD: Duplicates the value *)
   let border_solid =
     let decl, _ = Var.binding border_style_var Solid in
     style "border-solid" [ decl; border_style Solid ]

   (* GOOD: Uses the var reference *)
   let border_solid =
     let decl, var_ref = Var.binding border_style_var Solid in
     style "border-solid" [ decl; border_style (Var var_ref) ]
   ```

2. **Explicit declaration ownership**:
   - Setting utilities own declarations
   - Referencing utilities never create declarations
   - Theme layer owns theme token declarations

3. **Fallback hierarchy**:
   - @property initial value (compile-time default)
   - Runtime fallback in var() (runtime default)
   - Explicit value from utility (user choice)

### Implementation Strategy

```ocaml
(* Enforce declaration control *)
module Var : sig
  (* Sealed types prevent misuse *)
  type 'a setter    (* Can create declarations *)
  type 'a reference (* Can only reference *)

  (* Clear conversion points *)
  val as_reference : 'a setter -> 'a reference
  val set : 'a setter -> 'a -> Css.declaration * 'a Css.var
  val ref : 'a reference -> 'a Css.var
end
```

This ensures:
- Can't accidentally create declarations from references
- Clear ownership of who sets variables
- Type system enforces correct patterns

## Gaps and Risks to Address

### Browser @property Support
**Risk**: Pattern 1 assumes @property registration. Without it, `var(--x)` with no fallback degrades to property's initial value.

**Solution**:
- Add `~runtime_fallback:bool` to `defaulted` constructor
- When true, generates `var(--x, default)` even with @property
- Provides graceful degradation for older browsers

### Composition in Inline Mode
**Risk**: "Marks presence" requires knowing all active utilities at composition time.

**Solution**:
- Document that inline mode requires complete class list upfront
- Build pipeline must track active utilities before composition
- Consider error if composition attempted without complete context

### Order Determinism
**Risk**: Auto-computed order for theme tokens needs stability across builds.

**Solution**: Centralized order registry with validated ranges
```ocaml
module Order_registry = struct
  type range = {
    prefix : string;
    min : int;
    max : int;
    description : string;
  }

  let ranges = [
    { prefix = "font"; min = 0; max = 99;
      description = "Font families and features" };
    { prefix = "text"; min = 100; max = 199;
      description = "Text sizes and line heights" };
    { prefix = "font-weight"; min = 200; max = 209;
      description = "Font weights" };
    { prefix = "color"; min = 300; max = 399;
      description = "Color definitions" };
    { prefix = "spacing"; min = 400; max = 499;
      description = "Spacing scale" };
    { prefix = "radius"; min = 500; max = 549;
      description = "Border radius" };
    { prefix = "shadow"; min = 550; max = 599;
      description = "Shadow definitions" };
  ]

  let validate name order =
    match List.find_opt (fun r -> String.starts_with ~prefix:r.prefix name) ranges with
    | None -> Error (`No_range name)
    | Some r when order < r.min || order > r.max ->
        Error (`Out_of_range (name, order, r))
    | Some _ -> Ok ()

  let suggest name =
    match List.find_opt (fun r -> String.starts_with ~prefix:r.prefix name) ranges with
    | None -> None
    | Some r -> Some (r.min + hash(name) mod (r.max - r.min))

  let pp ppf () =
    Fmt.pf ppf "@[<v>%a@]"
      (Fmt.list ~sep:Fmt.cut (fun ppf r ->
        Fmt.pf ppf "%s: %d-%d (%s)"
          r.prefix r.min r.max r.description))
      ranges
end
```

### Reference Cycles
**Risk**: Variables referencing themselves through dependencies.

**Solution**:
```ocaml
module Internal = struct
  type dep_graph = {
    deps : string list StringMap.t;
    mutable visited : StringSet.t;
  }

  let detect_cycle graph name =
    if StringSet.mem name graph.visited then
      failwith (Printf.sprintf "Cycle detected in variable '%s'" name)
    else
      graph.visited <- StringSet.add name graph.visited
end
```

### Property Rule Deduplication
**Risk**: Same @property emitted multiple times.

**Solution**:
```ocaml
module Property_cache = struct
  (* Key: (name, kind, inherits, default_hash) *)
  type key = string * string * bool * int
  let cache : (key, Css.t) Hashtbl.t = Hashtbl.create 32

  let get_or_create key create_fn =
    match Hashtbl.find_opt cache key with
    | Some rule -> rule
    | None ->
        let rule = create_fn () in
        Hashtbl.add cache key rule;
        rule
end
```

### Dark Mode and Scoping
**Risk**: Theme tokens need different values per scope (root, dark mode, etc).

**Solution**:
- Add `~scope` parameter to `theme_token`
- Generate appropriate selectors:
  ```css
  :root { --text-xl: 1.25rem; }
  @media (prefers-color-scheme: dark) { :root { --text-xl: 1.35rem; } }
  [data-theme="custom"] { --text-xl: 1.5rem; }
  ```

## Comparison of Approaches

### Current API (Status Quo)
**Pros:**
- Already implemented and working
- Flexible - can handle any pattern

**Cons:**
- Too many footguns (ignoring declarations, forgetting property_rules)
- Unclear when to use binding vs reference
- Exposes implementation details
- No enforcement of patterns

### Pattern Helpers (Incremental)
**Pros:**
- Can be added without breaking changes
- Gradual migration path
- Validates patterns before bigger refactor

**Cons:**
- Still exposes underlying types
- Can't prevent misuse completely
- Two ways to do things during transition

### Hidden Pattern-Based API (Full Redesign)
**Pros:**
- Complete type safety - impossible to misuse
- Hidden implementation allows evolution
- Self-documenting through types
- Automatic layer/order management
- Better error messages

**Cons:**
- Requires complete rewrite
- Breaking change for all existing code
- More complex initial implementation

## Summary of Second Pass Improvements

### Critical Fixes Applied

1. **Pattern-aware migration**: Classify variables before transforming, avoid blind replacements
2. **Enhanced errors**: Added location info, constructor sites, pattern mismatches
3. **Type-tied properties**: `'a property_rule` prevents kind/syntax mismatches
4. **Mandatory initial**: Pattern 1 requires initial for sane inline defaults
5. **Order registry**: Centralized ranges with validation and suggestions
6. **Better testing**: Coverage for all patterns, modes, and fallback paths

### Key Architectural Decisions

1. **@property is global**: Cannot be scoped to themes/variants - warn if attempted
2. **Identity defaults**: Channels must use identity values (0, 0deg, 1)
3. **Deterministic output**: Property bundles sorted by (name, syntax)
4. **Single emission**: All properties via `emit_properties()` once
5. **Inline requires AST**: Composition needs complete class context

## Recommendation

**Short term (v4.1):** Implement **Pattern Helpers with Registry**
- Add order registry with validated ranges
- Pattern detector for safe migration
- Enhanced error messages with locations
- Type-tied property rules

**Medium term (v5.0):** Migrate to **Hidden Pattern-Based API**
- Once patterns validated through helpers
- Module-by-module migration
- Automated codemods with dry-run

**Long term:** Full pattern enforcement
- Remove old API completely
- All variables follow one of 5 patterns
- Type system prevents all misuse
- Deterministic, cacheable output

This staged approach ensures:
1. **Safety**: Pattern detection prevents wrong transformations
2. **Clarity**: Locations and context in all errors
3. **Determinism**: Stable output across builds
4. **Type safety**: Kind-tied properties, enforced patterns

## Testing Strategy

### Edge Cases to Test

1. **@property fallback behavior**:
   - Pattern 1 without browser support
   - Runtime fallback vs compile-time default
   - Multiple fallback levels

2. **Composition ordering**:
   - Multiple utilities setting same channel
   - Partial channel activation
   - Cascade resolution

3. **Theme scoping**:
   - Root vs media query precedence
   - Data attribute overrides
   - Variant layer interactions

4. **Reference cycles**:
   - Self-referential variables
   - Mutual dependencies
   - Transitive cycles

5. **Inline mode correctness**:
   - All patterns generate expected inline CSS
   - No var() references in inline mode (except ref_only)
   - Composition aggregation works

### Golden Tests
```ocaml
let test_patterns () =
  let test_cases = [
    (* Transforms - partial channels, re-sets *)
    "transforms", ["translate-x-4"; "scale-110"; "translate-x-2"; "transform"];

    (* Shadows - with and without color overrides *)
    "shadows", ["shadow-sm"; "shadow-red-500"; "shadow-lg"];

    (* Borders - registered defaults *)
    "borders", ["border"; "border-solid"; "border-dashed"; "border-2"];

    (* Typography - theme tokens with overrides *)
    "typography", ["text-xl"; "leading-tight"; "font-bold"];

    (* Dark mode scoping *)
    "dark", ["text-xl"; "dark:text-xl"; "data-theme-custom:text-xl"];
  ] in

  List.iter (fun (name, classes) ->
    (* Variables mode *)
    let vars_css = generate ~mode:Variables classes in
    assert_equal vars_css (load_fixture (name ^ "_vars.css"))
      ~printer:(fun s -> Fmt.str "@[<v>%s@]" s)
      ~msg:(Fmt.str "Variables mode: %s" name);

    (* Inline mode *)
    let inline_css = generate ~mode:Inline classes in
    assert_equal inline_css (load_fixture (name ^ "_inline.css"))
      ~msg:(Fmt.str "Inline mode: %s" name);

    (* No-@property mode (fallback validation) *)
    let no_prop_css = generate ~mode:Variables ~property_support:false classes in
    assert_equal no_prop_css (load_fixture (name ^ "_no_property.css"))
      ~msg:(Fmt.str "No @property: %s" name);

    (* Tailwind parity *)
    let tw_css = generate_tailwind classes in
    assert_structural_equal vars_css tw_css
      ~msg:(Fmt.str "Tailwind parity: %s" name)
  ) test_cases
```

## Developer Experience

### Debug Tooling
```ocaml
module Debug = struct
  type var_info = {
    name : string;
    pattern : string;
    owner : string option;
    references : string list;
    property_emitted : bool;
    scope : string;
  }

  val report : unit -> var_info list
  val visualize_deps : unit -> string (* DOT graph *)
  val explain_var : string -> string (* Human-readable explanation *)
end
```

### Migration Tooling
```ocaml
(* Pattern detection first *)
module Pattern_detector = struct
  type pattern =
    | Border_like  (* Has @property default *)
    | Shadow_like  (* Reference with fallback *)
    | Always_set   (* Set when used *)
    | Channel      (* Composition *)
    | Theme        (* Theme token *)
    | Unknown

  let classify var_name usage_sites =
    (* Analyze usage to determine pattern *)
    match usage_sites with
    | sites when List.exists has_property_rule sites -> Border_like
    | sites when List.exists has_fallback sites -> Shadow_like
    | sites when List.for_all sets_and_uses sites -> Always_set
    | _ -> Unknown

  let report classifications =
    Fmt.pr "@[<v>Pattern classification:@,%a@]"
      Fmt.(list ~sep:cut (pair string pp_pattern)) classifications
end

(* Pattern-aware transformations *)
let migrate_to_patterns ~dry_run file =
  let classifications = Pattern_detector.classify_all file in
  let transforms = List.map (function
    | name, Border_like ->
        Transform.to_use_default name ~add_property_rules:true
    | name, Shadow_like ->
        Transform.to_reference_with_fallback name
    | name, Always_set ->
        Transform.keep_setter name (* Don't change *)
    | name, Unknown ->
        Transform.warn name "Ambiguous pattern - manual review needed"
  ) classifications in

  if dry_run then
    Fmt.pr "@[<v>Would apply %d transformations:@,%a@]"
      (List.length transforms)
      pp_summary transforms
  else
    let backup = file ^ ".bak" in
    apply_with_backup ~backup transforms file
```

### Error Messages
```ocaml
type error_context = {
  loc : Location.t;
  var_name : string;
  utility : string option;
  constructor_site : Location.t option;
}

let pp_error ppf = function
  | Missing_order (ctx, name) ->
      Fmt.pf ppf
        "@[<v>%a: Theme variable '%s' requires explicit order.@,\
         Constructor at: %a@,\
         Hint: Use ~order:N from registry:@,\
         @[<v 2>  %a@]@]"
        Location.pp ctx.loc name
        Fmt.(option Location.pp) ctx.constructor_site
        pp_order_registry ()

  | Ignored_declaration (ctx, var) ->
      Fmt.pf ppf
        "@[<v>%a: Declaration for '%s' was created but not used%a.@,\
         Hint: Either use the declaration or switch to Var.ref@]"
        Location.pp ctx.loc var
        Fmt.(option (any " in utility '" ++ string ++ any "'")) ctx.utility

  | Pattern_mismatch (ctx, expected, actual) ->
      Fmt.pf ppf
        "@[<v>%a: Cannot %s a %s variable '%s'.@,\
         This variable can only be %s.@]"
        Location.pp ctx.loc
        (action_of actual) (pattern_name expected) ctx.var_name
        (allowed_actions expected)

  | Cycle_detected (ctx, cycle) ->
      let minimal = find_minimal_cycle cycle in
      Fmt.pf ppf
        "@[<v>%a: Circular dependency in '%s':@,\
         Minimal cycle: %a@,\
         Hint: Split into separate variables or precompute in theme@]"
        Location.pp ctx.loc ctx.var_name
        Fmt.(list ~sep:(any " -> ") string) minimal
```

## CSS.mli Changes Needed

To properly support the pattern-based API, we need to distinguish property rules:

```ocaml
module Css : sig
  (* Existing types *)
  type t
  type declaration

  (* NEW: Distinguish @property rules - tied to kind *)
  type 'a property_rule

  (* Property rules derive syntax from kind - no mismatches *)
  val property_of_kind :
    'a kind ->
    name:string ->
    initial:'a ->  (* Mandatory for sane defaults *)
    ?inherits:bool ->
    unit -> 'a property_rule

  (* Convert to CSS - note @property is always global/top-level *)
  val css_of_property : 'a property_rule -> t

  (* Deterministic combination - sorted by (name, syntax) *)
  val property_bundle : 'a property_rule list -> t

  (* Property emission control *)
  val emit_properties : unit -> t  (* Emit all registered, deduped *)
  val has_properties : t -> bool
  val extract_properties : t -> t * t  (* (properties, rest) *)

  (* Global @property warning *)
  val warn_scoped_property : Location.t -> string -> unit
    (* Warns if trying to use @property in theme/variant scope *)
end
```

### Key Changes from Feedback:

1. **Type-tied properties**: `'a property_rule` ensures syntax matches kind
2. **Mandatory initial**: Pattern 1 requires initial value for inline mode
3. **Global-only**: @property cannot be scoped - warn if attempted
4. **Deterministic**: `property_bundle` sorts for stable output
5. **Single emission point**: `emit_properties` for all registered properties

### Pattern-Specific Rules:

```ocaml
(* Pattern 1: Must have initial *)
let property_default kind name ~init ?(inherits=false) () =
  let prop = Css.property_of_kind kind ~name ~initial:init ~inherits () in
  Property_default { var; prop }

(* Pattern 2: Identity defaults enforced *)
let channel kind name ~zero () =
  assert (is_identity zero);  (* 0, 0deg, 1, etc *)
  Channel { var; zero }

(* Pattern 3: Fallback type-checked *)
let ref kind name ~fb ?(inline=`Use_fb) () =
  assert (kind_matches fb);  (* Same syntax *)
  Ref { var; fb; inline }
```