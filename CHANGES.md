## Unreleased

- Reject shades the palette does not define: `bg ~shade:250 gray` raises
  `Invalid_argument` when the value is constructed, and the CLI reports
  `bg-gray-250` as an unknown class instead of failing with an internal
  error (#127)

## 1.0.0

- Initial public release candidate. Type-safe Tailwind CSS v4 in OCaml,
  with parity against the upstream v4 compiler (core utilities plus the
  official `forms` and `typography` plugins).
