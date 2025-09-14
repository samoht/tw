#!/usr/bin/env python3
"""
Compare enum-like types in *_intf.ml with MDN CSS property pages.

What it does
- Parses lib/css/properties_intf.ml to extract:
  - Enum constructors for each type (zero-arg variants only)
  - Mapping from property constructors (e.g., Display) to the type used (e.g., display)
- For properties backed by enum-like types, fetches the MDN page for that property
  and extracts candidate keyword values from the Syntax and Values sections.
- Compares sets to report:
  - Missing in code vs MDN
  - Extra in code vs MDN
  - Potential duplicates or collisions in code after normalization

Notes
- Network access is required to fetch MDN pages.
- Parsing MDN pages is heuristic; it aims to flag likely gaps/weirdness, not be perfect.
- Complex properties with parameterized variants (e.g., lengths) are skipped by default.

Usage
  python3 scripts/compare_intf_with_mdn.py [--property display --property position ...]
  If no --property is provided, checks all enum-like properties found.
"""

from __future__ import annotations

import argparse
import os
import re
import sys
import textwrap
from dataclasses import dataclass
from typing import Dict, List, Optional, Set, Tuple

try:
    import requests  # type: ignore
    from bs4 import BeautifulSoup  # type: ignore
except Exception as e:  # pragma: no cover - helpful error if deps missing
    print(
        "ERROR: This script requires 'requests' and 'beautifulsoup4' (pip install requests beautifulsoup4)",
        file=sys.stderr,
    )
    raise


PROJECT_ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
INTF_PATH = os.path.join(PROJECT_ROOT, "lib", "css", "properties_intf.ml")


@dataclass
class TypeInfo:
    name: str
    zero_arity: List[str]
    has_param_variants: bool


def read(path: str) -> str:
    with open(path, "r", encoding="utf-8") as f:
        return f.read()


def ocaml_ident_to_css_token(ctor: str) -> str:
    """Convert an OCaml constructor like Inline_flex to a CSS token inline-flex.

    We default to hyphen-separated lowercase for readability. Comparison will
    also happen on a more aggressive normalization that strips separators.
    """
    return ctor.lower().replace("_", "-")


def normalize_for_compare(s: str) -> str:
    """Normalization used for set comparison.

    - Lowercases
    - Removes hyphens, underscores, and spaces
    - Collapses multiple whitespace
    """
    return re.sub(r"[-_\s]+", "", s.strip().lower())


def parse_types(intf_content: str) -> Dict[str, TypeInfo]:
    """Parse OCaml type definitions capturing zero-arity ctors and whether
    any param variants are present.
    """
    types: Dict[str, TypeInfo] = {}

    # Rough parser: find blocks starting with `type <name> =` and collect lines until a blank line
    type_re = re.compile(r"^type\s+([a-zA-Z0-9_\']+)\s*=", re.M)
    for m in type_re.finditer(intf_content):
        name = m.group(1)
        start = m.end()
        # Capture until next top-level 'type ' or end of file
        next_m = type_re.search(intf_content, pos=start)
        block = intf_content[start : next_m.start() if next_m else len(intf_content)]

        zero_arity: List[str] = []
        has_param = False

        # Normalize block and split by '|' to capture single-line variants too
        compact = re.sub(r"\(\*[\s\S]*?\*\)", " ", block)  # strip comments
        compact = compact.replace("\n", " ")
        parts = [p.strip() for p in compact.split("|")]
        for part in parts:
            if not part:
                continue
            m_ctor = re.match(r"^[^A-Za-z]*([A-Z][A-Za-z0-9_]*)\b(.*)$", part)
            if not m_ctor:
                continue
            ctor = m_ctor.group(1)
            tail = m_ctor.group(2)
            if ctor == "Var":
                has_param = True
                continue
            if re.search(r"\bof\b", tail):
                has_param = True
                continue
            zero_arity.append(ctor)

        types[name] = TypeInfo(name=name, zero_arity=zero_arity, has_param_variants=has_param)

    return types


def parse_property_type_map(intf_content: str) -> Dict[str, str]:
    """Parse the big `'a property` variant to map PropertyName -> type name.
    Example line: `| Display : display property`
    """
    # Locate the `'a property =` block
    prop_start = re.search(r"^type\s+'a\s+property\s*=", intf_content, re.M)
    if not prop_start:
        return {}
    start = prop_start.end()
    # End at two consecutive newlines followed by non-indented or next type
    segment = intf_content[start:]
    # Extract lines of the form: | Name : type property
    prop_re = re.compile(r"^\s*\|\s*([A-Z][A-Za-z0-9_']*)\s*:\s*([a-z0-9_']+)\s+property", re.M)
    mapping: Dict[str, str] = {}
    for m in prop_re.finditer(segment):
        prop_ctor, type_name = m.group(1), m.group(2)
        mapping[prop_ctor] = type_name
    return mapping


def property_ctor_to_css_name(prop_ctor: str) -> str:
    # Handle vendor prefixes first
    if prop_ctor.startswith("Webkit_"):
        base = prop_ctor[len("Webkit_"):]
        return "-webkit-" + base.lower().replace("_", "-")
    if prop_ctor.startswith("Moz_"):
        base = prop_ctor[len("Moz_"):]
        return "-moz-" + base.lower().replace("_", "-")
    if prop_ctor.startswith("Ms_"):
        base = prop_ctor[len("Ms_"):]
        return "-ms-" + base.lower().replace("_", "-")
    # Default: lowercase and hyphenate
    return prop_ctor.lower().replace("_", "-")


def fetch_mdn_tokens(property_css_name: str) -> Set[str]:
    """Fetch candidate tokens for a CSS property from MDN.

    Heuristics:
    - Parse the Syntax section's code block for alternatives (split on '|').
    - Parse headings under the Values section; many pages use <h3> with keywords.
    - Return a set of tokens as they appear (lowercased, stripped), allowing spaces or hyphens.
    """
    url = f"https://developer.mozilla.org/en-US/docs/Web/CSS/{property_css_name}"
    resp = requests.get(url, timeout=20)
    if resp.status_code != 200:
        raise RuntimeError(f"MDN page not found or not accessible: {url} ({resp.status_code})")

    soup = BeautifulSoup(resp.text, "html.parser")
    tokens: Set[str] = set()

    # 1) Syntax block
    syntax_header = None
    for h in soup.find_all(["h2", "h3"]):
        if h.get_text(strip=True).lower() == "syntax":
            syntax_header = h
            break
    if syntax_header:
        # Find first code/pre following the header
        code_block = syntax_header.find_next(["pre", "code"])
        if code_block:
            syntax = code_block.get_text("\n").strip()
            # Remove angle-bracketed non-terminals like <length>
            syntax = re.sub(r"<[^>]+>", "", syntax)
            # Replace newlines and tabs with spaces
            syntax = re.sub(r"[\n\t]+", " ", syntax)
            # Split on common separators
            parts = re.split(r"\||/|,|\(|\)|\[|\]|\{|\}|&&|\+|\*|\?", syntax)
            for p in parts:
                p = p.strip()
                if not p:
                    continue
                # Keep only keyword-like tokens (letters, hyphens, spaces)
                if re.fullmatch(r"[a-zA-Z][a-zA-Z\-\s]*", p):
                    tokens.add(p.lower())

    # 2) Values section: prefer inline <code> tokens. Fall back to headings.
    values_header = None
    for h in soup.find_all(["h2", "h3"]):
        if h.get_text(strip=True).lower() == "values":
            values_header = h
            break
    if values_header:
        # collect inline code tokens first
        for code in values_header.find_all_next("code"):
            # stop at next h2
            parent_h2 = code.find_previous("h2")
            if parent_h2 and parent_h2 is not values_header:
                break
            t = code.get_text(strip=True)
            if re.fullmatch(r"[a-zA-Z][a-zA-Z\-\s]*", t):
                tokens.add(t.lower())
        # also scan subsequent headings and definition terms until next h2
        for el in values_header.find_all_next():
            if el.name == "h2":  # stop at next main section
                break
            if el.name in ("h3", "dt"):
                text = el.get_text(" ", strip=True)
                # Extract first keyword-ish token
                m = re.match(r"([A-Za-z][A-Za-z\-\s]*)", text)
                if m:
                    t = m.group(1).strip().lower()
                    if t:
                        tokens.add(t)

    # Remove generic words that often appear
    for junk in [
        "initial",
        "inherit",
        "unset",
        "revert",
        "revert-layer",
        "global values",
        "keyword values",
        "default value",
        "non-standard syntax",
        "basic keywords",
        "distributed alignment",
        "positional alignment",
        "overflow alignment",
        "baseline alignment",
        "keyword value",
        "value",
        "for positional alignment only",
        "legacy alignment",
        "align-items does not take left and right values",
        "pack items around the center",
        "pack items from the end",
        "pack items from the left",
        "pack items from the right",
        "pack items from the start",
        "pack item around the center",
        "pack item from the end",
        "pack item from the left",
        "pack item from the right",
        "pack item from the start",
    ]:
        # We'll still compare globals separately if desired; drop here to focus on primary enums
        if junk in tokens:
            tokens.remove(junk)

    # Keep multi-word tokens; normalization handles hyphens/spaces later

    # Special-case: display page is atypical; if empty, scan more broadly
    if property_css_name == "display":
        for code in soup.find_all("code"):
            t = code.get_text(strip=True)
            if re.fullmatch(r"[a-zA-Z][a-zA-Z\-\s]*", t):
                tokens.add(t.lower())
        # Whitelist common display values to reduce noise
        display_whitelist = {
            "block",
            "inline",
            "inline-block",
            "flex",
            "inline-flex",
            "grid",
            "inline-grid",
            "flow-root",
            "table",
            "inline-table",
            "table-row",
            "table-cell",
            "table-caption",
            "table-column",
            "table-column-group",
            "table-header-group",
            "table-footer-group",
            "table-row-group",
            "list-item",
            "contents",
            "none",
        }
        tokens = {t for t in tokens if t in display_whitelist}

    # Special-case: grid-auto-flow supports compound keywords "row dense" and "column dense"
    if property_css_name == "grid-auto-flow":
        if "row" in tokens and "dense" in tokens:
            tokens.add("row dense")
        if "column" in tokens and "dense" in tokens:
            tokens.add("column dense")

    return tokens


def main() -> int:
    ap = argparse.ArgumentParser(formatter_class=argparse.RawDescriptionHelpFormatter,
        description=__doc__)
    ap.add_argument(
        "--property",
        action="append",
        help="Limit to specific property CSS names (kebab-case). Example: display",
    )
    ap.add_argument(
        "--include-param-types",
        action="store_true",
        help="Include properties whose type has parameterized variants (best-effort).",
    )
    args = ap.parse_args()

    if not os.path.exists(INTF_PATH):
        print(f"Cannot find {INTF_PATH}", file=sys.stderr)
        return 2

    content = read(INTF_PATH)
    types = parse_types(content)
    prop_map = parse_property_type_map(content)

    if not prop_map:
        print("No property mapping found in properties_intf.ml", file=sys.stderr)
        return 2

    # Build list of target properties
    targets: List[Tuple[str, str]] = []  # (css-name, type-name)
    for prop_ctor, type_name in prop_map.items():
        css_name = property_ctor_to_css_name(prop_ctor)
        if css_name.startswith(("-webkit-", "-moz-", "-ms-", "-o-")):
            continue
        if args.property and css_name not in args.property:
            continue
        tinfo = types.get(type_name)
        if not tinfo:
            # Type might be defined elsewhere or be an alias; skip for now
            continue
        if (not args.include_param_types) and tinfo.has_param_variants:
            # Skip complex types by default
            continue
        targets.append((css_name, type_name))

    if not targets:
        print("No matching enum-like properties to check.")
        return 0

    exit_code = 0
    for css_name, type_name in sorted(targets):
        tinfo = types[type_name]
        # Build code tokens and drop global keywords to reduce noise
        globals_drop = {"inherit", "initial", "unset", "revert", "revert-layer"}
        code_tokens_raw = [
            ocaml_ident_to_css_token(c)
            for c in tinfo.zero_arity
            if ocaml_ident_to_css_token(c) not in globals_drop
        ]
        code_norm = {normalize_for_compare(c): c for c in code_tokens_raw}
        # Detect internal collisions
        collisions: Dict[str, List[str]] = {}
        for c in code_tokens_raw:
            k = normalize_for_compare(c)
            collisions.setdefault(k, []).append(c)
        collisions = {k: v for k, v in collisions.items() if len(v) > 1}

        # Fetch MDN tokens
        try:
            mdn_tokens_raw = list(fetch_mdn_tokens(css_name))
        except Exception as e:
            print(f"[WARN] {css_name}: failed to fetch/parse MDN: {e}")
            exit_code = max(exit_code, 1)
            continue
        mdn_norm: Dict[str, str] = {normalize_for_compare(x): x for x in mdn_tokens_raw}

        # Compare sets by normalized keys
        missing_in_code = [mdn_norm[k] for k in mdn_norm.keys() - code_norm.keys()]
        extra_in_code = [code_norm[k] for k in code_norm.keys() - mdn_norm.keys()]

        print(f"\nProperty: {css_name} (type {type_name})")
        print(f"  Code enum tokens ({len(code_tokens_raw)}): {', '.join(sorted(code_tokens_raw))}")
        print(f"  MDN tokens ({len(mdn_tokens_raw)}): {', '.join(sorted(mdn_tokens_raw))}")

        if collisions:
            print("  Weirdness: potential duplicates after normalization:")
            for k, vs in collisions.items():
                print(f"    - {' / '.join(sorted(vs))} -> '{k}'")
            exit_code = max(exit_code, 1)

        if missing_in_code:
            print("  Missing in code (present on MDN):")
            for x in sorted(missing_in_code):
                print(f"    - {x}")
            exit_code = max(exit_code, 1)

        if extra_in_code:
            print("  Extra in code (not found on MDN):")
            for x in sorted(extra_in_code):
                print(f"    - {x}")
            exit_code = max(exit_code, 1)

        if not (collisions or missing_in_code or extra_in_code):
            print("  ✓ No issues detected (heuristic)")

    return exit_code


if __name__ == "__main__":
    sys.exit(main())
