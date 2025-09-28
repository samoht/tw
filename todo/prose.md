# CSS Cascade Semantics Violation in `rules.ml/of_grouped`

## Summary
The sorting in `lib/rules.ml/of_grouped` violates fundamental CSS cascade semantics by reordering utilities based on conflict groups rather than preserving source order. This breaks CSS's core principle that **when selectors have equal specificity, the last one in source order wins**.

Test tw 27 (prose basic) fails as a symptom of this bug - the optimizer merges `.prose` rules that get incorrectly grouped together by the sorting.

## Critical Issue: CSS Cascade Violation

### The CSS Cascade Rule (per MDN/W3C spec)
When two CSS rules have **equal specificity**, the **last one in source order wins**. This is fundamental to how CSS works and how developers override styles.

### How tw Violates This Rule
The `of_grouped` function in `lib/rules.ml` sorts ALL utilities by `conflict_group`, which:
1. **Reorders rules** based on utility type (display, position, margin, etc.), not source order
2. **Groups identical selectors together** even when intentionally separated
3. **Breaks user intent** for ANY utility where order matters for overrides
4. **Affects all utilities universally** - not just prose

## Root Cause Analysis

### 1. Initial Hypothesis (CONFIRMED)
- Both Tailwind and tw generate multiple `.prose` rules with identical selectors
- tw is reordering rules so the `.prose` rules end up adjacent
- The optimizer merges adjacent rules with identical selectors

### 2. Evidence Found

#### In `lib/prose.ml` (lines 808-819):
Two separate `.prose` rules are intentionally created:
```ocaml
let main_rule =
  Css.rule
    ~selector:(Css.Selector.class_ "prose")
    [ color (Css.Var prose_body_v); max_width (Ch 65.0) ]

let variables_rule =
  Css.rule
    ~selector:(Css.Selector.class_ "prose")
    (css_variables @ [ font_size (Rem 1.0); line_height (Num 1.75) ])
```

These are placed far apart in the rule list (lines 1026-1030):
```ocaml
[ main_rule ] @ paragraph_and_text_rules @ link_and_strong_rules
@ list_and_marker_rules @ structural_element_rules @ heading_rules base
@ media_rules base @ kbd_rules base @ code_rules base @ table_rules base
@ figure_rules base
@ [ variables_rule ]  (* Much later in the list *)
@ additional_rules base
```

#### In `lib/rules.ml` (of_grouped function):
All utilities are sorted by `conflict_group`, which groups rules with the same selector:
```ocaml
let sorted_list =
  List.sort
    (fun (sel1, _) (sel2, _) ->
      let prio1, sub1 = conflict_group (Css.Selector.to_string sel1) in
      let prio2, sub2 = conflict_group (Css.Selector.to_string sel2) in
      ...)
    grouped_list
```

#### In `lib/css/optimize.ml` (merge_rules function):
Adjacent rules with identical selectors are merged:
```ocaml
let merge_rules (rules : Stylesheet.rule list) : Stylesheet.rule list =
  (* Only merge truly adjacent rules with the same selector *)
  ...
  if prev.selector = rule.selector then
    (* Same selector immediately following - safe to merge *)
    let merged : Stylesheet.rule = {
      selector = prev.selector;
      declarations = deduplicate_declarations
        (prev.declarations @ rule.declarations);
      ...
    }
```

## The Bug Chain
1. `prose.ml` creates two `.prose` rules intentionally separated by many other rules
2. `rules.ml/of_grouped` sorts all utilities by conflict_group, causing both `.prose` rules to become adjacent
3. `optimize.ml/merge_rules` sees adjacent rules with identical selectors and merges them
4. The merged rule differs from Tailwind's output where the rules remain separate

## Output Comparison
In tw's output (tmp/css_debug/test_27_tw.css):
- Line 217: First `.prose` rule with `color` and `max-width`
- Line 221: Second `.prose` rule with CSS variables and font properties
- These get merged by the optimizer

In Tailwind's output:
- The `.prose` rules remain separate (though exact structure needs verification with unminified output)

## Test Cases Added to Demonstrate Universal Cascade Violations

Three new tests in `test/test_rules.ml` demonstrate this affects ALL utilities:

### Test 1: `test_cascade_order_violation` - Spacing Override
- User writes `[ p 4; p 2 ]` expecting p-2 to override p-4
- After sorting by conflict groups: order may be reversed
- **Impact**: Wrong padding applied - violates CSS cascade

### Test 2: `test_cascade_color_override` - Background Color Override
- User writes `[ bg blue 500; text white; bg red 500 ]` expecting red background
- After sorting by conflict groups: bg utilities get reordered
- **Impact**: Blue wins instead of red - **completely wrong color!**

### Test 3: `test_cascade_prose_separation` - Complex Utility Merging
- Any utility generating multiple rules with same selector gets affected
- After sorting: identical selectors group together and merge
- **Impact**: Different CSS structure than intended

These are just examples - **EVERY utility type is affected** when users intentionally override values.

## Potential Solutions

### Option 1: Remove sorting entirely (RECOMMENDED)
- **Remove the `List.sort` in `of_grouped`**
- Preserve source order to respect CSS cascade
- This is what Tailwind v4 does - it preserves declaration order

### Option 2: Sort only within non-conflicting groups
- Group utilities by selector first
- Sort only within groups that don't share selectors
- Preserves relative order of rules with same selector

### Option 3: Add stable sort with source position
- Track original position of each rule
- Use position as secondary sort key
- Ensures deterministic order while preserving cascade

### Option 4: Disable merging for specific cases
- Keep sorting but prevent merging of certain rule patterns
- Complex and error-prone

## Recommendation
**Option 1 is the correct fix.** The sorting in `of_grouped` serves no essential purpose and actively breaks CSS semantics. Tailwind v4 preserves source order because that's what CSS requires. The conflict groups can still be used for other purposes (like variable ordering) without sorting the actual CSS rules.

## Update: Stable Sort Fix Applied

### Solution Implemented
The user implemented **Option 3**: stable sort with original index tracking in `lib/rules.ml/of_grouped`. This preserves the cascade semantics while maintaining conflict group organization.

### Implementation
```ocaml
let indexed =
  List.mapi (fun i (sel, props) -> (i, sel, props)) grouped_list
in
let sorted_indexed =
  List.sort
    (fun (i1, sel1, _) (i2, sel2, _) ->
      let prio1, sub1 = conflict_group (Css.Selector.to_string sel1) in
      let prio2, sub2 = conflict_group (Css.Selector.to_string sel2) in
      let prio_cmp = Int.compare prio1 prio2 in
      if prio_cmp <> 0 then prio_cmp
      else
        let sub_cmp = Int.compare sub1 sub2 in
        if sub_cmp <> 0 then sub_cmp else Int.compare i1 i2)
    indexed
```

### Results
- ✅ Cascade violation tests pass
- ✅ Source order is preserved within conflict groups
- ❌ Test tw 27 still fails (different issue)

## New Investigation: Rule Separation vs Merging

### Hypothesis
After fixing the cascade violation, test tw 27 still fails because:
1. tw outputs TWO separate `.prose` rules
2. Tailwind outputs a SINGLE merged `.prose` rule

### Experimental Evidence

#### Test Setup
```bash
echo '<div class="prose"></div>' > tmp/test.html
dune exec -- tw -s "prose" --variables > tmp/tw_prose.css
npx tailwindcss --content tmp/test.html > tmp/tailwind_prose.css
```

#### Observation 1: tw outputs TWO `.prose` rules
In `tmp/tw_prose.css`:
- Line 18: `.prose { color: var(--tw-prose-body); max-width: 65ch }`
- Line 313: `.prose { --tw-prose-body: oklch(...); ... font-size: 1rem; line-height: 1.75 }`

#### Observation 2: Rule creation in prose.ml
From `lib/prose.ml` lines 765-776:
```ocaml
let main_rule =
  Css.rule
    ~selector:(Css.Selector.class_ "prose")
    [ color (Css.Var prose_body_v); max_width (Ch 65.0) ]

let variables_rule =
  Css.rule
    ~selector:(Css.Selector.class_ "prose")
    (css_variables @ [ font_size (Rem 1.0); line_height (Num 1.75) ])
```

These are intentionally placed far apart in the rule list (lines 983-988):
```ocaml
[ main_rule ] @ paragraph_and_text_rules @ link_and_strong_rules
@ list_and_marker_rules @ structural_element_rules @ heading_rules base
@ media_rules base @ kbd_rules base @ code_rules base @ table_rules base
@ figure_rules base
@ [ variables_rule ]  (* Much later in the list *)
@ additional_rules base
```

### Analysis
1. The stable sort fix **correctly preserves** the separation of the two `.prose` rules
2. This separation is **intentional** in the code design
3. However, the test expects these rules to be merged (like Tailwind does)

### Root Cause
The issue is NOT the cascade violation anymore. The issue is a **design decision**:
- tw creates two separate `.prose` rules intentionally
- The optimizer would merge them if they were adjacent
- The stable sort keeps them separated (as intended by the code)
- But the test expects a single merged rule (like Tailwind outputs)

### Secondary Issues Fixed
1. **Color value error**: `prose_td_borders` had wrong color (fixed: line 256)
2. **Invert variables in base**: Removed prose_invert_* from base CSS variables (they should only appear in .prose-invert)

### Conclusion
The cascade violation is fixed. The remaining test failure is due to an architectural decision about whether `.prose` rules should be:
1. Kept separate (current implementation intent)
2. Merged into one (what Tailwind does, what test expects)

This requires a design decision: Should prose rules be refactored to output as a single rule, or should the test be updated to accept the separated rules?