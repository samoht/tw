# TW - OCaml Tailwind CSS Implementation

## Project Overview
OCaml DSL for generating Tailwind CSS v4-compatible utility classes with type safety.

## Key Documentation
- `docs/adding-a-new-utility.md` - Guide for adding new utilities with CSS variables and layers

## Project Structure
```
lib/           - Core library modules (utilities, CSS generation)
  css/         - CSS AST and generation
  html/        - HTML DSL with integrated CSS
  tools/       - CLI tools (tw command, tailwind_gen)
  var.ml       - CSS variable system (CRITICAL - defines all variable types)
  rules.ml     - Layer assembly and CSS generation
  core.ml      - Base types and style function
test/          - Alcotest-based tests
  test_*.ml    - Individual test modules
  tools/       - Testing utilities
scripts/       - Helper scripts for CSS comparison
```

## Critical Concepts

### CSS Variables Pattern (NEVER violate these)
1. **NEVER use var(...) in string literals**: Always use typed `Var` from `Var.theme`/`Var.utility`
2. **NEVER use Css.custom**: It bypasses the type system
3. Variables must be defined in `var.ml` with proper ordering
4. Use `Var.theme` for shared theme tokens
5. Use `Var.utility` for per-utility variables with fallbacks
6. Use `Var.property` for @property registration when needed

### Layer System
- `@layer theme` - CSS variables in :root,:host (from Var.theme)
- `@layer properties` - @property defaults (from property_rules)
- `@layer base` - Preflight reset styles
- `@layer components` - Component styles (usually empty)
- `@layer utilities` - Utility classes

### Common Patterns
```ocaml
(* Theme variable *)
let size_def, size_var = Var.theme Var.Text_xl (Rem 1.25)

(* Utility variable with fallback *)
let my_def, my_var = Var.utility Var.My_var ~fallback:fallback_value value

(* Property registration for animations/transitions *)
let property_rules = [
  Var.property Var.My_var ~syntax:"*" ~inherits:false ~initial:"initial"
]

(* Utility definition *)
let my_util = style "my-util" ~property_rules [
  my_def;
  Css.some_property (Var my_var);
]
```

## Build & Test Commands

### Building
```bash
dune build                    # Build everything
dune build lib/tw.cma        # Build library
dune exec tw -- <args>       # Run tw CLI tool
```

### Testing
```bash
dune test                    # Run all tests
dune exec test/test.exe      # Run test suite directly
dune exec test/test.exe test <suite> <test_num>  # Run specific test

# With verbose output
ALCOTEST_VERBOSE=1 dune exec test/test.exe

# Common test suites: tw, core_tests, rules, Color
```

### Debugging CSS Generation
```bash
# Generate CSS for a class without base layer
dune exec -- tw -s "bg-blue-500 p-4" --variables

# Include base layer
dune exec -- tw -s "shadow-sm" --variables --base

# Compare with Tailwind output
dune exec -- tw -s "border-solid" --variables | head -20

# Generate from HTML files
dune exec -- tw examples/simple.html --minify
```

### CSS Comparison Tools
```bash
# Compare generated CSS with Tailwind
dune exec scripts/compare_css.exe <our_css> <tailwind_css>

# Generate Tailwind CSS for comparison (requires npx tailwindcss)
# ALWAYS use --minify and --optimize flags for accurate comparisons
echo '<div class="p-4 bg-blue-500">Test</div>' > /tmp/test.html
tailwindcss --content '/tmp/test.html' --minify --optimize 2>/dev/null
```

## Code Style & Conventions

1. **Type-first approach**: Define types in CSS modules, use Var constructors
2. **No string manipulation**: Use typed constructors, not string concatenation
3. **Follow existing patterns**: Check similar utilities before implementing
4. **Order matters**: Variables must follow canonical ordering in var.ml
5. **Test everything**: Write tests comparing output with Tailwind v4

## Common Pitfalls to Avoid

1. **Setting variables in wrong utilities**: Only style utilities should set style variables
2. **Wrong layer generation**: Don't pass ~property_rules unless needed for @property
3. **String-based var() references**: Always use typed Var, never "var(--name)"
4. **Missing fallbacks**: Provide sensible fallbacks for Var.utility
5. **Wrong @property syntax**: Use syntax:"*" for generic properties
6. **NEVER USE OCAMLPARAM=_,w=-32**: Don't silence warnings - fix them properly

## Quick Debugging Checklist
1. Check var.ml for variable definition and ordering
2. Verify layer generation with `tw -s <class> --variables`
3. Compare with Tailwind output using tailwindcss CLI
4. Check test failures with ALCOTEST_VERBOSE=1
5. Look for Css.custom or string var() usage (code smell)

## Helper Scripts Location
- tailwind_gen: lib/tools/tailwind_gen.ml
- compare_css: scripts/compare_css.ml
- tw CLI: lib/tools/tw.ml

## Testing Strategy
- Each utility module has corresponding test_*.ml
- Tests compare output with expected Tailwind CSS
- Use --variables mode for v4 compatibility testing
- Run specific failing tests with test suite and number
- Test output files saved to `/tmp/css_debug/` for debugging:
  - `<test_name>_tw.css`: Our generated output
  - `<test_name>_tailwind.css`: Expected Tailwind output
  - Use `diff -u` to compare and identify mismatches