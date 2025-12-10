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

## High Priority (New)

### 7. Type `will_change` property

**File**: `lib/interactivity.ml:137-140`

Currently uses raw strings like `"auto"`, `"scroll-position"`, `"contents"`, `"transform"`.

```ocaml
type will_change =
  | Auto
  | Scroll_position
  | Contents
  | Transform
  | Property of string list  (* For custom property names *)
```

### 8. Type `perspective_origin` property

**File**: `lib/transforms.ml:174-178`

Currently uses raw strings like `"center"`, `"top"`, `"bottom"`.

Should reuse `position_value` type or create a simpler variant.

### 9. Type `clip` property (deprecated)

**File**: `lib/layout.ml:134, 147`

Currently uses raw strings like `"rect(0, 0, 0, 0)"` and `"auto"`.

```ocaml
type clip =
  | Auto
  | Rect of { top: length; right: length; bottom: length; left: length }
```

### 10. Type `clip_path` property

**File**: `lib/clipping.ml:16-30`

Currently builds `"polygon(...)"` via string concatenation.

```ocaml
type clip_path =
  | None
  | Url of string
  | Inset of length * length * length * length
  | Circle of length option * position_value option
  | Ellipse of length * length * position_value option
  | Polygon of (length * length) list
  | Path of string  (* SVG path data *)
```

## Low Priority

### 11. Review other string escapes

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
