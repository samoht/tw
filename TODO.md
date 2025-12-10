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

### 4. Type font-face descriptors

**Status**: Pending

Font-face has many string-based descriptors marked with TODOs:

```ocaml
(* In font_face_descriptor *)
| Src of string  (* TODO: url(), local(), format() *)
| Font_variant of string  (* TODO: proper variant type *)
| Font_feature_settings of string
| Font_variation_settings of string
| Font_named_instance of string
```

### 5. Type import_rule fields

**Status**: Pending

Import rules use strings for supports and media:

```ocaml
type import_rule = {
  url: string;
  layer: string option;
  supports: string option;  (* Should be Supports.t option *)
  media: string option;     (* Should be Media.t option *)
}
```

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
