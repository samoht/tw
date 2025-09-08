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

### CSS Variables Pattern

Core principles:
- **Type safety first**: Always use typed `Var` constructors, never string literals
- **Define in var.ml**: All variables must be defined with proper ordering
- **Theme vs Utility**: Use `Var.theme` for shared tokens, `Var.utility` for specific utilities
- **Property registration**: Use `Var.property` for animations/transitions requiring @property

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
dune exec -- tw examples/simple.html --minify --optimize
```

### CSS Comparison Tools
```bash
# Compare generated CSS with Tailwind
dune exec scripts/cssdiff.exe <our_css> <tailwind_css>

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
5. **Test everything**: Write tests comparing output with Tailwind CSS
6. **Type annotations**: Add type annotations to helper functions for clarity
7. **Pattern matching**: Start with `Some` cases to avoid type ambiguity

## Common Pitfalls to Avoid

1. **String-based var() references**: Always use typed `Var`, never `"var(--name)"`
2. **Css.custom usage**: Never use - it bypasses the type system
3. **Setting variables in wrong places**: Only style utilities should set style variables
4. **Wrong layer generation**: Don't pass `~property_rules` unless needed for @property
5. **Missing fallbacks**: Always provide sensible fallbacks for `Var.utility`
6. **Wrong @property syntax**: Use `syntax:"*"` for generic properties
7. **Silencing warnings**: Never use `OCAMLPARAM=_,w=-32` - fix warnings properly

## Quick Debugging Checklist

When something doesn't work:
1. Check `var.ml` for variable definition and ordering
2. Verify layer generation: `dune exec -- tw -s "<class>" --variables`
3. Compare with Tailwind: `tailwindcss --content test.html --minify --optimize`
4. Debug test failures: `ALCOTEST_VERBOSE=1 dune test`
5. Look for anti-patterns: `Css.custom` or string `"var(--...)"` usage

## Key Files & Tools

- **tw CLI**: `lib/tools/tw.ml` - Main CLI for CSS generation
- **tailwind_gen**: `lib/tools/tailwind_gen.ml` - Code generator for utility modules
- **cssdiff**: `scripts/cssdiff.ml` - CSS diff tool
- **var.ml**: Variable definitions and ordering (CRITICAL)
- **rules.ml**: Layer assembly and CSS output generation

## Testing Strategy
- Each utility module has corresponding test_*.ml
- Tests compare output with expected Tailwind CSS
- Use --variables mode for CSS variable testing
- Run specific failing tests with test suite and number
- Test output files saved to `/tmp/css_debug/` for debugging:
  - `<test_name>_tw.css`: Our generated output
  - `<test_name>_tailwind.css`: Expected Tailwind output
  - Use `diff -u` to compare and identify mismatches

## Test File Organization

### test/css directory structure:
- **test_values.ml** - CSS value types (lengths, colors, angles, calc, etc.)
- **test_declaration.ml** - CSS declaration parsing
- **test_properties.ml** - CSS property types and values
- **test_selector.ml** - CSS selector parsing and construction
- **test_reader.ml** - Low-level CSS reader/parser functions
- **test_pp.ml** - Pretty printing and minification
- **test_css.ml** - CSS optimization and rule merging (uses ocaml-crunch for embedded test files)

### Test Focus:
- Each test file should focus on testing against the SPEC, not implementation
- test_css.ml focuses ONLY on optimization algorithms, not basic CSS functionality
- Basic CSS features are tested in their respective files (values, properties, etc.)
- Avoid duplicate tests - each concept should be tested in ONE place
- Use ocaml-crunch to embed CSS test files for portability

## Test Structure Convention

### For CSS Property Types (test/css/test_properties.ml)

1. **Generic check function**:
   ```ocaml
   let check_value name pp reader ?expected input =
     (* Handles parse/print roundtrip testing *)
   ```

2. **One-liner check functions for each type**:
   ```ocaml
   let check_display = check_value "display" Css.Properties.pp_display Css.Properties.read_display
   let check_position = check_value "position" Css.Properties.pp_position Css.Properties.read_position
   let check_overflow = check_value "overflow" Css.Properties.pp_overflow Css.Properties.read_overflow
   ```
   - Function name: `check_<type>` where `<type>` matches the OCaml type name exactly
   - Tests individual values of that specific type

3. **Test functions use check functions**:
   ```ocaml
   let test_display () =
     check_display "none";
     check_display "block";
     check_display "inline";
     check_display ~expected:"inline-block" "inline-block";  (* Use ~expected when output differs *)
     ...
   ```

4. **Test function definitions**:
   - ALWAYS define named test functions: `let test_foo () = ...`
   - NEVER use inline anonymous functions in test_case
   - Bad: `test_case "foo" \`Quick (fun () -> ...)`
   - Good: `test_case "foo" \`Quick test_foo`

5. **Naming conventions**:
   - `check_<type>`: Tests a single value of type `<type>`
   - `test_<property>`: Tests all values for a CSS property
   - Never invent names - use exact type names from properties_intf.ml

6. **Coverage requirements**:
   - Test all valid enum values for each type
   - Include edge cases (empty strings, whitespace, case variations)
   - Test complex structured types thoroughly (shadows, gradients, transforms)

7. **Testing negative cases and ambiguous specs**:
   - Always test invalid inputs that should fail parsing
   - Test edge cases where CSS/MDN specs might be ambiguous
   - Use `try_parse` to verify that invalid values are properly rejected
   - Example:
     ```ocaml
     let test_negative_values () =
       let neg reader s label =
         let r = Css.Reader.of_string s in
         check bool label true (Option.is_none (try_parse reader r))
       in
       neg read_angle "90" "angle without unit should fail";
       neg read_duration "100" "duration without unit should fail";
       neg read_color "notacolor" "invalid color keyword should fail"
     ```
   - When specs are ambiguous, document the chosen behavior and test it
   - Test boundary conditions (e.g., what happens with negative margins, zero values)

## When Fixing Tests
- **test/css/** tests: Always refer to the CSS/MDN spec. If the test is compliant with the spec and it fails: fix the code. Otherwise fix the test.
- **test/** tests (non-css): Always refer to the Tailwind v4 manual (and/or to the tailwindcss v4.x.x CLI tool). If the test is compliant, fix the code. Otherwise fix the test.

## Important Testing Principles
- **Tests should validate the SPEC, not the current implementation**
- CSS tests must verify compliance with CSS specifications (W3C/MDN)
- Tailwind tests must verify output matches Tailwind v4 behavior
- Never write tests that just confirm current code behavior - always test against the expected specification
- If a test passes but violates the spec, the test is wrong
- If code passes tests but violates the spec, write better tests
