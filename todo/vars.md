# CSS Variable Patterns - Pure API Design

## Core Problem

We have 5 distinct variable patterns but our current API makes them easy to misuse. We need a pure, simple API that encodes intent without global state or complex migration machinery.

## The Five Patterns

### Pattern 1: Theme Variables
**Design tokens set once in theme layer, referenced by utilities**
- **Example**: `--text-xl: 1.25rem`, `--font-weight-bold: 700`
- **Theme layer**: `:root { --text-xl: 1.25rem; }`
- **Utility layer**: `.text-xl { font-size: var(--text-xl); }`
- **Current API**: `Var.create` with `~layer:Theme`, one place does `Var.binding`
- **New API**: `Var.theme` with mandatory `~order` parameter

### Pattern 2: Property_default Variables
**Variables with @property defaults, allowing utilities to either SET or REFERENCE**
- **Example**: `--tw-border-style` with @property initial value of `solid`
- **@property**: `@property --tw-border-style { syntax: "*"; inherits: false; initial-value: solid; }`
- **Setting utilities**: `.border-solid { --tw-border-style: solid; border-style: solid; }`
- **Referencing utilities**: `.border { border-style: var(--tw-border-style); }`
- **Current API**: Variable has `~property:(Some Solid, false)`, referencing utilities use `Var.reference` with `~property_rules`
- **New API**: `Var.property_default` with mandatory `~initial` parameter

### Pattern 3: Channel Variables
**Composition variables where multiple utilities contribute to a single CSS property**
- **Example**: `--tw-translate-x`, `--tw-rotate`, `--tw-scale` in transforms
- **Contributing utilities**: `.translate-x-4 { --tw-translate-x: 1rem; }`
- **Aggregator utility**: `.transform { transform: translateX(var(--tw-translate-x)) rotate(var(--tw-rotate)); }`
- **Current API**: Regular `Var.binding`, no @property needed
- **New API**: `Var.channel` with mandatory `~default` parameter (identity value)

### Pattern 4: Ref_only Variables
**Variables that utilities only reference (with fallback), never set**
- **Example**: `--tw-shadow-color` in shadow utilities
- **Referencing utilities**: `.shadow-sm { box-shadow: 0 1px 3px var(--tw-shadow-color, #0000001a); }`
- **Setting utilities**: `.shadow-red-500 { --tw-shadow-color: #ef4444; }` (different module)
- **Current API**: `Var.reference` with `~fallback` parameter, no @property
- **New API**: `Var.ref_only` with mandatory `~fallback` parameter

### Pattern 5: Always-set Variables
**Variables that are always set when used (no separate reference)**
- **Example**: `--tw-font-weight` in typography utilities
- **Usage**: `.font-bold { --tw-font-weight: 700; font-weight: var(--tw-font-weight); }`
- **Current API**: Standard `Var.binding` pattern
- **New API**: Regular `Var.channel` or continue using current API (this pattern is already safe)

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

## Type-Safe Variable API with GADTs

```ocaml
module Var : sig
  (* GADT for CSS kinds prevents syntax mismatches at compile time *)
  type _ kind =
    | Length : Length.t kind
    | Color : Color.t kind
    | Float : float kind
    | Percentage : Percentage.t kind
    | Border_style : Border_style.t kind
    | Gradient_stops : Gradient_stop.t list kind
    (* ... other kinds *)

  (* Phantom types for variable patterns - zero runtime cost *)
  type ('a, 'role) t

  (* Pattern-specific constructors enforce constraints *)

  (* Theme variables - order is mandatory *)
  val theme : 'a kind -> string -> value:'a -> order:int -> ('a, [`Theme]) t

  (* Utility patterns with enforced constraints *)
  val property_default : 'a kind -> string -> initial:'a -> ?inherits:bool ->
                         ('a, [`Property_default]) t
                         (* initial is mandatory for sane inline rendering *)

  val channel : 'a kind -> default:'a -> string -> ('a, [`Channel]) t
                (* For composition variables like transforms - default required for inline mode *)

  val ref_only : 'a kind -> string -> fallback:'a -> ('a, [`Ref_only]) t
                 (* Variables that are only referenced, never set *)

  (* Core operations - same as current API but type-safe *)

  (* Binding - same as current `binding` but with type constraints *)
  val binding : ('a, [> `Theme | `Property_default | `Channel]) t ->
                ?fallback:'a Css.fallback -> 'a -> Css.declaration * 'a Css.var

  (* Referencing - same as current `reference` *)
  val reference : ('a, _) t -> ?fallback:'a Css.fallback -> 'a Css.var

  (* Property rules - only for property_default variables *)
  val property_rule : ('a, [`Property_default]) t -> Css.t option

  (* Heterogeneous collections via existentials *)
  type any_var = Any_var : ('a, 'role) t -> any_var

  (* @property emission control - single point, deterministic *)
  val properties : any_var list -> Css.t
  (* Global-only, deduplicated by (name, kind, inherits, initial) *)
end
```

## Critical Inline Mode Requirement

**ALL variables must always have a default value for inline mode to work correctly.**

Each pattern constructor must store the appropriate default:
- `property_default`: stores `initial` value
- `channel`: stores `default` value (Zero, 0deg, 1.0, etc.)
- `ref_only`: stores `fallback` value
- `theme`: stores `value`

Behind the scenes, `Var.reference` always calls `Css.var ~default:stored_value`, ensuring every variable reference has a concrete value for inline rendering.

**No changes needed to current inline mode** - the existing `~default` parameter in `Css.var` already handles this. We just need to ensure it's always provided by making the pattern constructors store the required values.

## Usage Examples

```ocaml
(* Pattern 1: Property_default with mandatory initial *)
let border_style_var =
  Var.property_default Var.Border_style "tw-border-style" ~initial:Solid
  (* initial is mandatory for sane inline rendering *)

(* Referencing utility - needs property rule *)
let border =
  let prop = match Var.property_rule border_style_var with
    | Some rule -> rule | None -> Css.empty in
  let var_ref = Var.reference border_style_var in
  style "border" ~property_rules:[prop] [
    border_style (Var var_ref);
    border_width (Px 1.)
  ]

(* Setting utility - same as current binding *)
let border_solid =
  let decl, var_ref = Var.binding border_style_var Solid in
  style "border-solid" [ decl; border_style (Var var_ref) ]

(* Pattern 2: Channel variables for composition *)
let translate_x_var =
  Var.channel Var.Length ~default:Zero "tw-translate-x"

let translate_x_4 =
  let decl, var_ref = Var.binding translate_x_var (Rem 1.) in
  style "translate-x-4" [ decl ]

(* Pattern 3: Ref_only - reference with fallback *)
let shadow_color_var =
  Var.ref_only Var.Color "tw-shadow-color" ~fallback:(Css.hex "#0000001a")
  (* Can never be set - ref_only at type level *)

let shadow_sm =
  let color_var = Var.reference shadow_color_var in
  style "shadow-sm" [
    box_shadow (Shadow [...(Var color_var)...])
  ]

(* Pattern 4: Theme token with explicit order *)
let text_xl_var =
  Var.theme Var.Length "text-xl" ~value:(Rem 1.25) ~order:110

let text_xl =
  let var_ref = Var.reference text_xl_var in
  style "text-xl" [ font_size (Var var_ref) ]

(* Transform utility - same pattern as current API *)
let transform =
  let tx_ref = Var.reference translate_x_var in
  let rot_ref = Var.reference rotate_var in
  let scale_ref = Var.reference scale_var in
  style "transform" [
    transform (Transform [
      TranslateX (Var tx_ref);
      Rotate (Var rot_ref);
      Scale (Var scale_ref);
    ])
  ]

(* @property emission - single point, deterministic *)
let all_properties =
  Var.properties [
    Var.Any_var border_style_var;
    Var.Any_var text_xl_var;
    (* ... *)
  ]
  (* Deduplicated by (name, kind, inherits, initial), sorted by (name, kind) *)
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

### Inline Mode Centralization
```ocaml
(* All inline decisions happen in Var.Ref.make - single source of truth *)
let inline_transform config style =
  (* No separate logic needed - Var.Ref.make already handles inline mode *)
  match config.mode with
  | `Variables -> style  (* Use variables as-is *)
  | `Inline -> style     (* Variables already carry inline values *)

(* Var.Ref.make is where ALL inline decisions are made *)
module Ref = struct
  let make : type a. a t -> a Css.var = fun ref_var ->
    match ref_var with
    | Property_default_ref { initial; name; _ } ->
        create_var ~name ~inline:(Some initial)
    | Channel_ref { kind; name; _ } ->
        let identity = identity_for kind in
        create_var ~name ~inline:(Some identity)
    | Ref_only_ref { fallback; name; _ } ->
        create_var ~name ~inline:(Some fallback)
    | Token_ref { value; name; _ } ->
        create_var ~name ~inline:(Some value)
end

```
## Key Insights

### Type-Driven Safety Wins
- GADTs + phantom types prevent all misuse at compile time
- Split modules eliminate option-returning operations
- Single source of truth for inline decisions
- No global state means easier testing and reasoning

### Smart Combinators Prevent Misuse
Type-safe combinators guide users to correct patterns:

```ocaml
(* Pattern-specific combinators ensure correct usage *)
let with_default :
    ('a, property_default) Setter.t ->
    (Css.property_rule -> 'a Ref.t -> 'b) -> 'b =
  fun setter f ->
    let prop = Setter.property_rule setter in
    let ref_var = Ref.of_setter setter in
    f prop ref_var

let with_channels :
    ('a, channel) Setter.t list ->
    (('a, channel) Setter.t list -> 'a Ref.t list -> 'b) -> 'b =
  fun setters f ->
    let refs = List.map Ref.of_setter setters in
    f setters refs

(* Location.t capture enables actionable error messages *)
let missing_initial_error ~loc var_name =
  error ~loc "Property_default variable '%s' requires explicit initial value for inline rendering" var_name

let scoped_property_error ~loc var_name =
  error ~loc "@property for '%s' must be global - cannot be scoped to themes/variants" var_name

(* Lint mode catches remaining footguns at CI time *)
let lint_stylesheet stylesheet =
  (* Check for common mistakes that type system can't prevent *)
  check_property_scoping stylesheet;
  check_missing_aggregators stylesheet;
  check_order_collisions stylesheet
```


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

## Implementation Strategy

### Phase 1: Pattern Detection & Validation
**Immediate**: Build pattern detector to analyze current usage
- Classify all existing variables into the 5 patterns
- Identify any variables that don't fit cleanly
- Generate migration report showing what needs to change

### Phase 2: New Pattern-Based API
**Next Release**: Implement the new type-safe API
- Pattern-specific constructors with mandatory parameters
- Type-level enforcement of variable roles
- Automated codemod tools for migration
- **Complete removal of old API** - no deprecation period

The new API makes misuse impossible at compile time:
- Can't create theme variables without order
- Can't ignore property_default initial values
- Can't accidentally set ref_only variables
- Can't mix patterns incorrectly

### Why No Backward Compatibility

Keeping deprecated APIs creates:
- **API confusion**: Two ways to do the same thing
- **Maintenance burden**: Supporting parallel implementations
- **Documentation complexity**: Explaining old vs new
- **Testing overhead**: Coverage for both paths

Instead, we provide:
- **Clean migration**: Automated codemods handle the conversion
- **Clear patterns**: Only one right way to use variables
- **Better errors**: Focus on making the new API excellent
- **Simpler codebase**: No legacy code paths

## Key Architectural Decisions

1. **@property is global**: Cannot be scoped to themes/variants - warn if attempted
2. **Identity defaults**: Channels must use identity values (0, 0deg, 1)
3. **Deterministic output**: Property bundles sorted by (name, syntax)
4. **Single emission**: All properties via `emit_properties()` once
5. **Inline requires defaults**: Every variable stores its inline value

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