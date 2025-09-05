#!/usr/bin/env python3
"""
Scaffold section modules from outline.txt.

Reads outline.txt (produced by scripts/outline.py) and creates a pair of files
for each level-2 section (## Title [label]) under lib/css/sections/:
  - <Module>.mli
  - <Module>.ml

No build changes are made; this is purely scaffolding.

Module name strategy:
  - Prefer the ocamldoc label if present (e.g., [css_selectors])
  - Otherwise derive from Title by lowercasing and replacing non-alnum with '_'
  - Convert to PascalCase for the module filename

Each stub exposes a minimal signature for future merge:
  - type t (placeholder)
  - val parse_property : name:string -> value:string -> Css.declaration option
  - val readers : unit -> unit  (placeholder for parser readers consolidation)

Usage:
  python3 scripts/split_scaffold.py outline.txt
"""

from __future__ import annotations
import argparse
import re
from pathlib import Path


HEADING_L2_RE = re.compile(r"^##\s+(?P<title>[^\[]+?)(?:\s*\[(?P<label>[^\]]+)\])?\s*$")


def pascal_from_label(label: str) -> str:
    parts = re.split(r"[^a-zA-Z0-9]+", label)
    parts = [p for p in parts if p]
    return "".join(p.capitalize() for p in parts)


def derive_module_name(title: str, label: str | None) -> str:
    base = label or title.strip().lower()
    return pascal_from_label(base)


MLI_TEMPLATE = """(** {{b Section}}: {title}

    This module corresponds to the "{title}" section of css.mli.
    It will own types, parsers, and readers for this domain.
*)

(** Placeholder domain state (replace or remove as needed). *)
type t

val parse_property : name:string -> value:string -> Css.declaration option
(** Parse a single property/value belonging to this section into a typed
    declaration, or [None] if it doesn't apply. *)

val readers : unit -> unit
(** Placeholder to host domain-specific reader helpers during consolidation. *)
"""

ML_TEMPLATE = """(* Auto-generated scaffold for {modname}. *)

type t = unit

let readers () = ()

let parse_property ~name ~value =
  let _ = value in
  match String.lowercase_ascii name with
  | _ -> None
"""


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("outline", type=Path)
    args = ap.parse_args()

    lines = args.outline.read_text(encoding="utf-8").splitlines()
    sections: list[tuple[str, str | None]] = []
    in_headings = False
    for ln in lines:
        if ln.strip() == "== Headings ==":
            in_headings = True
            continue
        if in_headings and ln.strip() == "":
            # stop at first blank after headings block + some content
            # keep reading until "== Outline =="
            pass
        if in_headings and ln.strip() == "== Outline ==":
            break
        if in_headings:
            m = HEADING_L2_RE.match(ln)
            if m:
                title = m.group("title").strip()
                label = m.group("label")
                sections.append((title, label))

    target_dir = Path("lib/css/sections")
    target_dir.mkdir(parents=True, exist_ok=True)

    created = []
    for title, label in sections:
        modname = derive_module_name(title, label)
        ml = target_dir / f"{modname}.ml"
        mli = target_dir / f"{modname}.mli"
        if not mli.exists():
            mli.write_text(MLI_TEMPLATE.format(title=title), encoding="utf-8")
        if not ml.exists():
            ml.write_text(ML_TEMPLATE.format(modname=modname), encoding="utf-8")
        created.append(modname)

    print(f"Created/kept {len(created)} section modules under {target_dir}")
    for m in created:
        print(" -", m)


if __name__ == "__main__":
    main()
