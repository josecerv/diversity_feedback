"""32_repack.py — zip /c/tmp/ch1_edit back into the pptx."""
from __future__ import annotations
import os
import zipfile
from pathlib import Path

SCRATCH = Path(r"C:/tmp/ch1_edit")
OUT = Path(r"C:/Users/jcerv/Jose/diversity_feedback/Presentations/Chapter_I/Cervantez_Chapter_I_DiversityFeedback.pptx")

# Preserve [Content_Types].xml first in archive (helpful for some readers)
root = SCRATCH
content_types = "[Content_Types].xml"

with zipfile.ZipFile(OUT, "w", compression=zipfile.ZIP_DEFLATED) as zf:
    # Write content_types first
    zf.write(root / content_types, content_types)
    # Walk the rest
    for p in root.rglob("*"):
        if p.is_dir():
            continue
        rel = p.relative_to(root).as_posix()
        if rel == content_types:
            continue
        zf.write(p, rel)
print(f"Wrote {OUT}  ({OUT.stat().st_size:,} bytes)")

# Sanity: reopen and list
with zipfile.ZipFile(OUT, "r") as zf:
    names = zf.namelist()
    slides = sorted(n for n in names if n.startswith("ppt/slides/slide") and n.endswith(".xml"))
    print(f"Contains {len(slides)} slides, e.g. {slides[:3]}{'...' if len(slides)>3 else ''}")
