"""
34_enlarge_thumbs.py — bump grid thumbnails back up toward Qualtrics
proportions (100px in 140px cell = 71% of cell width). My previous pass
left them at 34% which reads as tiny.

Changes (slide 9 only):
  - grid_photo_XX cx/cy: 400_000 -> 780_000  (~66% of cell width)
  - grid_photo_XX y inside bg: +10_000 from top
  - grid_label_XX y inside bg: +800_000 (just below photo)
  - grid_label_XX cy: 710_000 -> 360_000  (tighter, normAutofit handles overflow)
  - grid_label_XX x offset and cx unchanged; bodyPr keeps normAutofit.

Does NOT touch:
  - grid_bg_XX positions (user moved those; keep intact)
  - sel_hl_X / sel_check_X (positions track bg and check sits OVER the photo,
    which is the correct "selected" look)
  - text_col, feedback_card, title, hr lines
  - Animation timeline
"""
from __future__ import annotations

import re
import shutil
import zipfile
from pathlib import Path

DECK = Path(r"C:/Users/jcerv/Jose/diversity_feedback/Presentations/Chapter_I/Cervantez_Chapter_I_DiversityFeedback.pptx")
SCRATCH = Path(r"C:/tmp/ch1_edit")

# --- unpack current pptx ----------------------------------------------
if SCRATCH.exists():
    shutil.rmtree(SCRATCH)
SCRATCH.mkdir(parents=True)
with zipfile.ZipFile(DECK, "r") as zf:
    zf.extractall(SCRATCH)

s9_path = SCRATCH / "ppt/slides/slide9.xml"
s9 = s9_path.read_text(encoding="utf-8")

# --- collect grid_bg positions (source of truth) ----------------------
bg_positions = {}
for m in re.finditer(
    r'<p:cNvPr id="\d+" name="grid_bg_(\d{2})"/><p:cNvSpPr/><p:nvPr/></p:nvSpPr>'
    r'<p:spPr><a:xfrm><a:off x="(\d+)" y="(\d+)"/>',
    s9,
):
    idx = int(m.group(1))
    bg_positions[idx] = (int(m.group(2)), int(m.group(3)))
assert len(bg_positions) == 25, f"expected 25 grid_bg, got {len(bg_positions)}"

# --- new geometry inside each cell ------------------------------------
# Cell is 1_179_576 × 1_170_000. Aim for Qualtrics ratio (photo ≈ 66% of
# cell width) and leave room for the stimulus text beneath.
PHOTO_CXCY = 780_000
PHOTO_X_OFFSET = (1_179_576 - PHOTO_CXCY) // 2        # 199_788
PHOTO_Y_OFFSET = 10_000                                # small top gutter
LABEL_CX = 1_124_712
LABEL_X_OFFSET = (1_179_576 - LABEL_CX) // 2          # 27_432
LABEL_Y_OFFSET = 800_000                               # just below photo
LABEL_CY = 360_000                                     # normAutofit shrinks if needed

# --- grid_photo rewrite ------------------------------------------------
photo_re = re.compile(
    r'(<p:pic><p:nvPicPr><p:cNvPr id="\d+" name="grid_photo_(\d{2})"[^/]*/>'
    r'.*?<a:xfrm>)<a:off x="\d+" y="\d+"/><a:ext cx="\d+" cy="\d+"/>',
    re.DOTALL,
)
def _photo_sub(m):
    idx = int(m.group(2))
    bg_x, bg_y = bg_positions[idx]
    return (
        f'{m.group(1)}'
        f'<a:off x="{bg_x + PHOTO_X_OFFSET}" y="{bg_y + PHOTO_Y_OFFSET}"/>'
        f'<a:ext cx="{PHOTO_CXCY}" cy="{PHOTO_CXCY}"/>'
    )
s9, n_ph = photo_re.subn(_photo_sub, s9)
assert n_ph == 25, f"photo rewrites expected 25 got {n_ph}"
print(f"Enlarged {n_ph} photos to {PHOTO_CXCY} EMU (~0.81\", 66% of cell width)")

# --- grid_label reposition (x/cx/cx/cy only; keep txBody + normAutofit)
label_re = re.compile(
    r'(<p:sp><p:nvSpPr><p:cNvPr id="\d+" name="grid_label_(\d{2})"/>'
    r'<p:cNvSpPr txBox="1"/><p:nvPr/></p:nvSpPr><p:spPr><a:xfrm>)'
    r'<a:off x="\d+" y="\d+"/><a:ext cx="\d+" cy="\d+"/>',
)
def _label_sub(m):
    idx = int(m.group(2))
    bg_x, bg_y = bg_positions[idx]
    return (
        f'{m.group(1)}'
        f'<a:off x="{bg_x + LABEL_X_OFFSET}" y="{bg_y + LABEL_Y_OFFSET}"/>'
        f'<a:ext cx="{LABEL_CX}" cy="{LABEL_CY}"/>'
    )
s9, n_lb = label_re.subn(_label_sub, s9)
assert n_lb == 25, f"label rewrites expected 25 got {n_lb}"
print(f"Repositioned {n_lb} labels (y_offset={LABEL_Y_OFFSET}, cy={LABEL_CY}, normAutofit on)")

# --- write back
s9_path.write_text(s9, encoding="utf-8")

# --- repack pptx
with zipfile.ZipFile(DECK, "w", compression=zipfile.ZIP_DEFLATED) as zf:
    zf.write(SCRATCH / "[Content_Types].xml", "[Content_Types].xml")
    for p in SCRATCH.rglob("*"):
        if p.is_dir():
            continue
        rel = p.relative_to(SCRATCH).as_posix()
        if rel == "[Content_Types].xml":
            continue
        zf.write(p, rel)
print(f"\nWrote {DECK} ({DECK.stat().st_size:,} bytes)")
