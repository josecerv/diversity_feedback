"""
33_fix_clipping.py — second pass polish after user feedback.

Changes (applied on top of 30_apply_edits.py + 31_edit_slide9.py output):

  Slide 8 caption  : drop the first-paragraph restatement ("Gender feedback
                     has the largest effect..."). Keep only the Wald-tests
                     line (add N=1,000 to it since that context is lost).
  Slide 10 caption : same — drop first-paragraph restatement, keep Wald-tests
                     line with N=1,000.
  Slide 9          : (a) truncate hr_main / hr_sub to stop before the grid
                         (slide-5 style, preserves stroke weights);
                     (b) grow cell cy from 1,033,271 to 1,170,000 and push
                         the grid up so it fills the slide vertically;
                     (c) shrink photo to 0.42" so label has room;
                     (d) set label height to 725,000 + enable normAutofit so
                         the stimulus text never clips.

Reads the edited pptx (assumes 30_/31_/32_ already ran) and writes back in-place.
"""
from __future__ import annotations

import re
import shutil
import zipfile
from pathlib import Path

ROOT = Path(r"C:/Users/jcerv/Jose/diversity_feedback")
DECK = ROOT / "Presentations/Chapter_I/Cervantez_Chapter_I_DiversityFeedback.pptx"
SCRATCH = Path(r"C:/tmp/ch1_edit")

# --- unpack current pptx into scratch ----------------------------------
if SCRATCH.exists():
    shutil.rmtree(SCRATCH)
SCRATCH.mkdir(parents=True)
with zipfile.ZipFile(DECK, "r") as zf:
    zf.extractall(SCRATCH)

# =======================================================================
# SLIDE 8 — drop first caption paragraph
# =======================================================================
s8_path = SCRATCH / "ppt/slides/slide8.xml"
s8 = s8_path.read_text(encoding="utf-8")

# Find the caption shape and drop its first <a:p>
# The caption's first paragraph contains "Gender feedback has the largest effect".
# Keep everything from the 2nd <a:p> (Wald tests).
# Pattern: <a:lstStyle/><a:p>...first-para...</a:p><a:p>...second-para...
old8 = ('<a:lstStyle/>'
        '<a:p><a:pPr algn="ctr"/>'
        '<a:r><a:rPr sz="1400" b="0" i="1"><a:solidFill><a:srgbClr val="57606C"/></a:solidFill><a:latin typeface="Calibri"/></a:rPr>'
        '<a:t>Gender feedback has the largest effect of any attribute </a:t></a:r>'
        '<a:r><a:rPr sz="1400" b="1" i="1"><a:solidFill><a:srgbClr val="C5093B"/></a:solidFill><a:latin typeface="Calibri"/></a:rPr>'
        '<a:t>(20.1% \u2192 40.6%, +20.6 pp, p &lt; .001, N = 1,000).</a:t></a:r></a:p>'
        '<a:p>')
new8 = ('<a:lstStyle/>'
        '<a:p>')
assert old8 in s8, "slide 8: caption first-paragraph anchor missing"
s8 = s8.replace(old8, new8)

# Also add N=1,000 to the Wald-tests line (since it's now the only line)
s8 = s8.replace(
    "all p\u2019s &lt; .001).",
    "all p\u2019s &lt; .001, N = 1,000).",
)

s8_path.write_text(s8, encoding="utf-8")
print("slide8.xml: first caption paragraph dropped; N=1,000 appended to Wald line")

# =======================================================================
# SLIDE 10 — drop first caption paragraph
# =======================================================================
s10_path = SCRATCH / "ppt/slides/slide10.xml"
s10 = s10_path.read_text(encoding="utf-8")

old10 = ('<a:lstStyle/>'
         '<a:p><a:pPr algn="ctr"/>'
         '<a:r><a:rPr sz="1100" b="0" i="1"><a:solidFill><a:srgbClr val="57606C"/></a:solidFill><a:latin typeface="Calibri"/></a:rPr>'
         '<a:t>Race feedback has the largest effect of any attribute </a:t></a:r>'
         '<a:r><a:rPr sz="1100" b="1" i="1"><a:solidFill><a:srgbClr val="C5093B"/></a:solidFill><a:latin typeface="Calibri"/></a:rPr>'
         '<a:t>(28.2% \u2192 41.6%, +13.4 pp, p &lt; .001, N = 1,000).</a:t></a:r></a:p>'
         '<a:p>')
new10 = ('<a:lstStyle/>'
         '<a:p>')
assert old10 in s10, "slide 10: caption first-paragraph anchor missing"
s10 = s10.replace(old10, new10)

# The slide-10 caption already has "all p's < .025" — append N=1,000
s10 = s10.replace(
    "all p\u2019s &lt; .025).",
    "all p\u2019s &lt; .025, N = 1,000).",
)

s10_path.write_text(s10, encoding="utf-8")
print("slide10.xml: first caption paragraph dropped; N=1,000 appended to Wald line")

# =======================================================================
# SLIDE 9 — grid enlargement + hr truncation + label auto-fit
# =======================================================================
s9_path = SCRATCH / "ppt/slides/slide9.xml"
s9 = s9_path.read_text(encoding="utf-8")

# -- (1) clip hr_main / hr_sub so they stop right where the grid begins
#        (user: "clip it so the horizontal line ends at the study 2 stimuli")
# Grid starts at x = 5961888. hr starts at x = 457200.
# Set cx = 5961888 - 457200 = 5504688 so the line runs up to the grid edge.
HR_CX = 5961888 - 457200  # 5504688
s9 = s9.replace(
    '<p:cNvPr id="2" name="s_hr_main"/><p:cNvCxnSpPr/><p:nvPr/></p:nvCxnSpPr>'
    '<p:spPr><a:xfrm><a:off x="457200" y="777240"/><a:ext cx="11274552" cy="0"/></a:xfrm>',
    f'<p:cNvPr id="2" name="s_hr_main"/><p:cNvCxnSpPr/><p:nvPr/></p:nvCxnSpPr>'
    f'<p:spPr><a:xfrm><a:off x="457200" y="777240"/><a:ext cx="{HR_CX}" cy="0"/></a:xfrm>',
)
s9 = s9.replace(
    '<p:cNvPr id="3" name="s_hr_sub"/><p:cNvCxnSpPr/><p:nvPr/></p:nvCxnSpPr>'
    '<p:spPr><a:xfrm><a:off x="457200" y="832104"/><a:ext cx="11274552" cy="0"/></a:xfrm>',
    f'<p:cNvPr id="3" name="s_hr_sub"/><p:cNvCxnSpPr/><p:nvPr/></p:nvCxnSpPr>'
    f'<p:spPr><a:xfrm><a:off x="457200" y="832104"/><a:ext cx="{HR_CX}" cy="0"/></a:xfrm>',
)
print(f"  hr_main / hr_sub clipped to cx={HR_CX} (runs up to grid edge)")

# -- (2) new grid geometry --------------------------------------------
# Old row y: 1069848, 2139696, 3209544, 4279392, 5349240 (delta 1069848, cy 1033271)
# New row y: 810000,  2005000, 3200000, 4395000, 5590000 (delta 1195000, cy 1170000)
# Cell cx unchanged at 1179576 (col positions unchanged).
OLD_ROW_YS = [1069848, 2139696, 3209544, 4279392, 5349240]
NEW_ROW_YS = [810000, 2005000, 3200000, 4395000, 5590000]
OLD_CELL_CY = 1033271
NEW_CELL_CY = 1170000

PHOTO_CXCY = 400000
PHOTO_X_OFFSET = (1179576 - PHOTO_CXCY) // 2   # 389788
PHOTO_Y_OFFSET = 25000
LABEL_CX = 1124712
LABEL_X_OFFSET = (1179576 - LABEL_CX) // 2     # 27432
LABEL_Y_OFFSET = 445000
LABEL_CY = 710000

old_to_new_y = dict(zip(OLD_ROW_YS, NEW_ROW_YS))

def _migrate_y(old_y: int) -> int:
    if old_y in old_to_new_y:
        return old_to_new_y[old_y]
    raise ValueError(f"unexpected row y {old_y}")

# -- (2a) rewrite grid_bg geometry (25 shapes)
bg_re = re.compile(
    r'(<p:sp><p:nvSpPr><p:cNvPr id="\d+" name="grid_bg_\d{2}"/>'
    r'<p:cNvSpPr/><p:nvPr/></p:nvSpPr><p:spPr><a:xfrm>)'
    r'<a:off x="(\d+)" y="(\d+)"/><a:ext cx="\d+" cy="\d+"/>'
)
def _bg_sub(m):
    old_x = int(m.group(2))
    old_y = int(m.group(3))
    new_y = _migrate_y(old_y)
    return (
        f'{m.group(1)}'
        f'<a:off x="{old_x}" y="{new_y}"/><a:ext cx="1179576" cy="{NEW_CELL_CY}"/>'
    )
s9, n_bg = bg_re.subn(_bg_sub, s9)
assert n_bg == 25, f"expected 25 bg rewrites, got {n_bg}"
print(f"  {n_bg} grid_bg shapes resized to cy={NEW_CELL_CY} and repositioned")

# -- (2b) rewrite grid_photo geometry (photos anchored to new bg positions)
photo_re = re.compile(
    r'(<p:pic><p:nvPicPr><p:cNvPr id="\d+" name="grid_photo_(\d{2})"[^/]*/>'
    r'.*?<a:xfrm>)<a:off x="(\d+)" y="(\d+)"/><a:ext cx="\d+" cy="\d+"/>',
    re.DOTALL,
)
def _photo_sub(m):
    head = m.group(1)
    old_y = int(m.group(4))
    # photo's old y was bg_y + 54864 in original, or bg_y + 30000 in my first pass.
    # Locate matching bg row by subtracting various offsets.
    for off in (30000, 54864, 25000):
        if (old_y - off) in OLD_ROW_YS:
            bg_y_new = _migrate_y(old_y - off)
            break
    else:
        raise ValueError(f"photo y={old_y} does not resolve to any known row")
    # x: recompute from old_x by locating bg_x
    old_x = int(m.group(3))
    # Old photo x was bg_x + 228600 (original) or bg_x + 339788 (first pass).
    bg_x = None
    for bg_off in (228600, 339788, 309788, 389788):
        maybe_bg_x = old_x - bg_off
        if maybe_bg_x in (5961888, 7178040, 8394192, 9610344, 10826496):
            bg_x = maybe_bg_x
            break
    assert bg_x is not None, f"photo x={old_x} does not resolve to any col"
    return (
        f'{head}'
        f'<a:off x="{bg_x + PHOTO_X_OFFSET}" y="{bg_y_new + PHOTO_Y_OFFSET}"/>'
        f'<a:ext cx="{PHOTO_CXCY}" cy="{PHOTO_CXCY}"/>'
    )
s9, n_photo = photo_re.subn(_photo_sub, s9)
assert n_photo == 25, f"expected 25 photo rewrites, got {n_photo}"
print(f"  {n_photo} grid_photo shapes resized to {PHOTO_CXCY} and repositioned")

# -- (2c) rewrite grid_label positions + enable normAutofit ------------
# The label's current xfrm uses bg_x + 27432 for x and bg_y + LABEL_Y_OFFSET_old
# for y. We rewrite both coord & add normAutofit so the stimulus text can
# auto-shrink if needed.
label_re = re.compile(
    r'(<p:sp><p:nvSpPr><p:cNvPr id="\d+" name="grid_label_\d{2}"/>'
    r'<p:cNvSpPr txBox="1"/><p:nvPr/></p:nvSpPr><p:spPr><a:xfrm>)'
    r'<a:off x="(\d+)" y="(\d+)"/><a:ext cx="\d+" cy="\d+"/>'
    r'</a:xfrm>'
    r'(<a:prstGeom prst="rect"><a:avLst/></a:prstGeom><a:noFill/></p:spPr>)'
    r'<p:txBody><a:bodyPr[^>]*/>',
    re.DOTALL,
)
def _label_sub(m):
    old_x = int(m.group(2))
    old_y = int(m.group(3))
    # label's old y offset inside bg: 795528 (original, 1865376-1069848) or
    # 545000 (first pass)
    for off in (795528, 545000, 605000, 615000, 620000):
        if (old_y - off) in OLD_ROW_YS:
            bg_y_new = _migrate_y(old_y - off)
            break
    else:
        raise ValueError(f"label y={old_y} does not resolve to any known row")
    # label x offset inside bg: 27432
    bg_x = old_x - 27432
    if bg_x not in (5961888, 7178040, 8394192, 9610344, 10826496):
        # Try other offsets
        for bg_off in (27432, 13716):
            if (old_x - bg_off) in (5961888, 7178040, 8394192, 9610344, 10826496):
                bg_x = old_x - bg_off
                break
        else:
            raise ValueError(f"label x={old_x} does not resolve")
    return (
        f'{m.group(1)}'
        f'<a:off x="{bg_x + LABEL_X_OFFSET}" y="{bg_y_new + LABEL_Y_OFFSET}"/>'
        f'<a:ext cx="{LABEL_CX}" cy="{LABEL_CY}"/>'
        f'</a:xfrm>'
        f'{m.group(4)}'
        f'<p:txBody><a:bodyPr wrap="square" lIns="14000" tIns="5000" rIns="14000" bIns="5000" anchor="t">'
        f'<a:normAutofit fontScale="100000" lnSpcReduction="0"/></a:bodyPr>'
    )
s9, n_label = label_re.subn(_label_sub, s9)
assert n_label == 25, f"expected 25 label rewrites, got {n_label}"
print(f"  {n_label} grid_label shapes repositioned, cy={LABEL_CY}, normAutofit on")

# -- (2d) update sel_hl_X and sel_check_X y coordinates ----------------
# sel_hl x,y matches some bg cell; sel_check top-right corner matches that bg.
# Migrate y-values.
def _sel_hl_sub(m):
    old_y = int(m.group(2))
    new_y = _migrate_y(old_y)
    return f'{m.group(1)}<a:off x="{m.group(3)}" y="{new_y}"/><a:ext cx="1179576" cy="{NEW_CELL_CY}"/>'

sel_hl_re = re.compile(
    r'(<p:sp><p:nvSpPr><p:cNvPr id="\d+" name="sel_hl_\d+"/>'
    r'<p:cNvSpPr/><p:nvPr/></p:nvSpPr><p:spPr><a:xfrm>)'
    r'<a:off x="(\d+)"',
)
# Need a different approach: capture x and y
sel_hl_re = re.compile(
    r'(<p:sp><p:nvSpPr><p:cNvPr id="\d+" name="sel_hl_\d+"/>'
    r'<p:cNvSpPr/><p:nvPr/></p:nvSpPr><p:spPr><a:xfrm>)'
    r'<a:off x="(\d+)" y="(\d+)"/><a:ext cx="\d+" cy="\d+"/>',
)
def _sel_hl_sub2(m):
    old_x = int(m.group(2))
    old_y = int(m.group(3))
    new_y = _migrate_y(old_y)
    return (
        f'{m.group(1)}<a:off x="{old_x}" y="{new_y}"/>'
        f'<a:ext cx="1179576" cy="{NEW_CELL_CY}"/>'
    )
s9, n_hl = sel_hl_re.subn(_sel_hl_sub2, s9)
print(f"  {n_hl} sel_hl shapes repositioned to new rows")

# sel_check: top-right corner of bg. Old y = bg_y + 27431. New y = new_bg_y + 27431.
sel_check_re = re.compile(
    r'(<p:sp><p:nvSpPr><p:cNvPr id="\d+" name="sel_check_\d+"/>'
    r'<p:cNvSpPr/><p:nvPr/></p:nvSpPr><p:spPr><a:xfrm>)'
    r'<a:off x="(\d+)" y="(\d+)"/><a:ext cx="(\d+)" cy="(\d+)"/>',
)
def _sel_check_sub(m):
    old_x = int(m.group(2))
    old_y = int(m.group(3))
    # sel_check y is bg_y + ~27431 (measured in original deck)
    found_off = None
    for off in (27431, 27432, 18288, 18287):
        if (old_y - off) in OLD_ROW_YS:
            found_off = off
            bg_y_new = _migrate_y(old_y - off)
            break
    if found_off is None:
        raise ValueError(f"sel_check y={old_y} does not resolve")
    return (
        f'{m.group(1)}<a:off x="{old_x}" y="{bg_y_new + found_off}"/>'
        f'<a:ext cx="{m.group(4)}" cy="{m.group(5)}"/>'
    )
s9, n_check = sel_check_re.subn(_sel_check_sub, s9)
print(f"  {n_check} sel_check shapes repositioned to new rows")

# -- write back
s9_path.write_text(s9, encoding="utf-8")
print(f"slide9.xml: written ({len(s9):,} chars)")

# =======================================================================
# Re-zip back into pptx
# =======================================================================
with zipfile.ZipFile(DECK, "w", compression=zipfile.ZIP_DEFLATED) as zf:
    zf.write(SCRATCH / "[Content_Types].xml", "[Content_Types].xml")
    for p in SCRATCH.rglob("*"):
        if p.is_dir():
            continue
        rel = p.relative_to(SCRATCH).as_posix()
        if rel == "[Content_Types].xml":
            continue
        zf.write(p, rel)
print(f"\nWrote {DECK}  ({DECK.stat().st_size:,} bytes)")
