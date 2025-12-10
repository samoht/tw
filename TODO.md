# Type Safety Improvements

This file tracks string-based operations that should be replaced with proper CSS types.

## High Priority

### ~~1. Store structured types in AST instead of strings~~ DONE

The AST now stores `Media.t` and `Container.t` directly instead of strings.

### ~~2. Add Supports.t structured type~~ DONE

The AST now stores `Supports.t` directly instead of strings (`lib/css/supports.ml`).

### ~~3. Add Keyframe.position type~~ DONE

The AST now stores `Keyframe.selector` instead of strings (`lib/css/keyframe.ml`).

## Medium Priority

### ~~4. Type font-face descriptors~~ PARTIALLY DONE

Created `lib/css/font_face.ml` with typed descriptors:
- `Src` now uses `Font_face.src` (with `Url`, `Local`, `Raw` variants)
- `Size_adjust` now uses `Font_face.size_adjust` (float percentage)
- `Ascent_override`, `Descent_override`, `Line_gap_override` now use
  `Font_face.metric_override` (`Normal | Percent of float`)

Still string-based (TODO):
- `Font_variant` - complex CSS property with many sub-values
- `Font_feature_settings` - uses existing Properties type
- `Font_variation_settings` - uses existing Properties type

### ~~5. Type import_rule fields~~ DONE

Import rules now use typed fields:
- `supports : Supports.t option` instead of `string option`
- `media : Media.t option` instead of `string option`

## Low Priority

### 6. Review other string escapes

- `Selector.Raw` - Sometimes needed for complex selectors
- `Container.Raw` - Escape hatch for unparsed conditions
- Various `Css.custom` usages in test files

## Completed

- [x] Add structured `Media.t` type (`lib/css/media.ml`)
- [x] Add structured `Container.t` type (`lib/css/container.ml`)
- [x] Add structured `Supports.t` type (`lib/css/supports.ml`)
- [x] Add `Keyframe.selector` type (`lib/css/keyframe.ml`)
- [x] Add `Font_face` types for src, metric overrides, size-adjust (`lib/css/font_face.ml`)
- [x] Type `import_rule` fields with `Supports.t` and `Media.t`
