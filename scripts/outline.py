#!/usr/bin/env python3
"""
Quick .mli outline extractor

Parses an OCaml interface (.mli) file and extracts:
  - Sections/subsections from ocamldoc headings ({1, {2, ...})
  - Top-level and nested module outline
  - Types and values (val) declarations
  - Summary statistics (counts, empties, max module depth)

Usage:
  python3 scripts/outline.py path/to/file.mli [--json]

Notes:
  - This is a best-effort, regex-based parser intended for quick analysis.
  - It recognizes ocamldoc headings inside comments and common declaration forms.
  - Nested modules are tracked via `module X : sig` / `end` heuristics.
"""

from __future__ import annotations

import argparse
import json
import re
from dataclasses import dataclass, field
from pathlib import Path
from typing import List, Optional, Dict, Any


HEADING_RE = re.compile(r"\{(?P<level>[1-6])(?:\:(?P<label>[^}\s]+))?\s+(?P<title>[^}]+)\}")
OCAMLDOC_COMMENT_RE = re.compile(r"\(\*\*.*?\*\)", re.DOTALL)

# Code parsing regexes (approximate)
RE_MODULE_SIG = re.compile(r"^\s*module\s+(?:type\s+)?(?P<name>[A-Z][A-Za-z0-9_']*)\s*:\s*sig\b")
RE_MODULE_ALIASED = re.compile(r"^\s*module\s+(?:type\s+)?(?P<name>[A-Z][A-Za-z0-9_']*)\s*=\b")
RE_END = re.compile(r"^\s*end\s*$")
RE_VAL = re.compile(r"^\s*val\s+(?P<name>[a-zA-Z_][a-zA-Z0-9_']*)\b")
RE_TYPE = re.compile(r"^\s*type\s+(?:nonrec\s+)?(?:(?:'[a-zA-Z0-9_]+\s+)+)?(?P<name>[a-zA-Z_][a-zA-Z0-9_']*)\b")


@dataclass
class Heading:
    level: int
    title: str
    label: Optional[str] = None
    start: int = 0  # start index in file content
    end: int = 0    # end index of this heading token
    content_after: str = ""  # text until next heading (inside comments only)

    def is_empty(self) -> bool:
        # A section is considered empty if there is no non-whitespace text
        # after the heading up to the next heading, excluding Markdown-like bullets
        txt = self.content_after
        txt = re.sub(r"[\s\n\r]+", " ", txt).strip()
        return txt == ""


@dataclass
class Decl:
    kind: str  # 'module' | 'type' | 'val'
    name: str
    path: List[str] = field(default_factory=list)
    line: int = 0


def extract_headings(content: str) -> List[Heading]:
    headings: List[Heading] = []
    for m in OCAMLDOC_COMMENT_RE.finditer(content):
        block = m.group(0)
        block_start = m.start()
        for h in HEADING_RE.finditer(block):
            headings.append(
                Heading(
                    level=int(h.group("level")),
                    label=h.group("label"),
                    title=h.group("title").strip(),
                    start=block_start + h.start(),
                    end=block_start + h.end(),
                )
            )

    # Fill content_after by looking until next heading within comments
    headings.sort(key=lambda x: x.start)
    comment_spans = [(m.start(), m.end()) for m in OCAMLDOC_COMMENT_RE.finditer(content)]
    for i, hd in enumerate(headings):
        # Find the end bound: next heading start or end of same comment block
        next_start = headings[i + 1].start if i + 1 < len(headings) else None
        # Limit to same comment block
        end_bound = None
        for (cs, ce) in comment_spans:
            if cs <= hd.start < ce:
                end_bound = ce
                break
        if end_bound is None:
            continue
        cut_end = min(end_bound, next_start) if next_start is not None else end_bound
        hd.content_after = content[hd.end:cut_end]

    return headings


def strip_comments(content: str) -> str:
    # Remove ocamldoc and regular comments to parse declarations
    # Handle nested comments very roughly by greedy removal
    return re.sub(r"\(\*[\s\S]*?\*\)", " ", content)


def extract_decls(content: str) -> List[Decl]:
    code = strip_comments(content)
    decls: List[Decl] = []
    stack: List[str] = []
    lines = code.splitlines()
    for idx, line in enumerate(lines, start=1):
        m = RE_MODULE_SIG.match(line)
        if m:
            name = m.group("name")
            decls.append(Decl(kind="module", name=name, path=stack.copy(), line=idx))
            stack.append(name)
            continue

        m = RE_MODULE_ALIASED.match(line)
        if m:
            name = m.group("name")
            decls.append(Decl(kind="module", name=name, path=stack.copy(), line=idx))
            # Do not push: aliased/eq form doesn't open a sig
            continue

        if RE_END.match(line):
            if stack:
                stack.pop()
            continue

        m = RE_TYPE.match(line)
        if m:
            decls.append(Decl(kind="type", name=m.group("name"), path=stack.copy(), line=idx))
            continue

        m = RE_VAL.match(line)
        if m:
            decls.append(Decl(kind="val", name=m.group("name"), path=stack.copy(), line=idx))
            continue

    return decls


def compute_stats(headings: List[Heading], decls: List[Decl]) -> Dict[str, Any]:
    by_level: Dict[int, int] = {}
    empties = 0
    for h in headings:
        by_level[h.level] = by_level.get(h.level, 0) + 1
        if h.is_empty():
            empties += 1

    type_count = sum(1 for d in decls if d.kind == "type")
    val_count = sum(1 for d in decls if d.kind == "val")
    module_count = sum(1 for d in decls if d.kind == "module")

    max_depth = 0
    for d in decls:
        max_depth = max(max_depth, len(d.path))

    return {
        "headings": {str(k): v for k, v in sorted(by_level.items())},
        "empty_sections": empties,
        "types": type_count,
        "values": val_count,
        "modules": module_count,
        "max_module_depth": max_depth,
        "total_headings": len(headings),
        "total_decls": len(decls),
    }


def print_outline(headings: List[Heading], decls: List[Decl]) -> None:
    print("== Headings ==")
    for h in headings:
        lab = f" [{h.label}]" if h.label else ""
        empt = " (empty)" if h.is_empty() else ""
        print(f"{h.level*'#'} {h.title}{lab}{empt}")

    print("\n== Outline ==")
    for d in decls:
        indent = "  " * len(d.path)
        path = "::".join(d.path)
        prefix = f"{path}::" if path else ""
        print(f"{indent}{d.kind:6} {prefix}{d.name}  (line {d.line})")


def main():
    ap = argparse.ArgumentParser(description=".mli outline and stats extractor")
    ap.add_argument("mli", type=Path, help="Path to .mli file")
    ap.add_argument("--json", action="store_true", help="Output JSON only")
    args = ap.parse_args()

    content = args.mli.read_text(encoding="utf-8")
    headings = extract_headings(content)
    decls = extract_decls(content)
    stats = compute_stats(headings, decls)

    if args.json:
        out = {
            "file": str(args.mli),
            "headings": [h.__dict__ | {"empty": h.is_empty()} for h in headings],
            "decls": [d.__dict__ for d in decls],
            "stats": stats,
        }
        print(json.dumps(out, indent=2))
    else:
        print_outline(headings, decls)
        print("\n== Stats ==")
        for k, v in stats.items():
            print(f"{k:>18}: {v}")


if __name__ == "__main__":
    main()

