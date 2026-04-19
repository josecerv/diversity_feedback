"""
30_apply_edits.py — Targeted polish pass over Ch 1 deck.

Edits applied:
  Slide 8 (Study 1 results)    : embed regenerated figure (0-75% y-axis),
                                 add caption + systems-test line.
  Slide 9 (Study 2 task)       : strip Cover story/Prompt/Feedback-condition
                                 labels, keep only Instructions + Attributes at
                                 slide-5 sizes; add author attributes beneath
                                 each name in the 5x5 grid.
  Slide 10 (Study 2 mediation) : append systems-test F-stats to existing caption.
  Slide 14 (Key Findings)      : remove em dashes, tighten spacing, bump body 22pt.
  Slide 15 (Discussion)        : swap "impression management" -> "reputational
                                 and self-image concerns", same spacing fix.

Run from repo root or anywhere; script resolves paths absolutely.
"""
from __future__ import annotations

import json
import re
import shutil
import zipfile
from pathlib import Path

ROOT = Path(r"C:/Users/jcerv/Jose/diversity_feedback")
DECK_BACKUP = ROOT / "Presentations/Chapter_I/Cervantez_Chapter_I_DiversityFeedback.backup_before_edits.pptx"
NEW_FIGURE = ROOT / "Study-3B/Figure-Study3B-wide.png"

# --- unpack pristine backup into a scratch dir (idempotent rebuild) ----
SCRATCH = Path(r"C:/tmp/ch1_edit")
if SCRATCH.exists():
    shutil.rmtree(SCRATCH)
SCRATCH.mkdir(parents=True)
with zipfile.ZipFile(DECK_BACKUP, "r") as zf:
    zf.extractall(SCRATCH)

# --- copy regenerated figure over image8.png ----------------------------
shutil.copyfile(NEW_FIGURE, SCRATCH / "ppt/media/image8.png")

# =======================================================================
# SLIDE 8 — add caption with Study 1 numbers + systems-test line
# =======================================================================
s8_path = SCRATCH / "ppt/slides/slide8.xml"
s8 = s8_path.read_text(encoding="utf-8")

# Shrink figure slightly to make room for caption at bottom.
# Current: off y=1102962, ext cy=5154995 -> ends at 6257957
# Target : off y=1102962, ext cy=4700000 -> ends at 5802962, leaves room
s8 = s8.replace(
    '<a:off x="274320" y="1102962"/><a:ext cx="11640312" cy="5154995"/>',
    '<a:off x="274320" y="1020000"/><a:ext cx="11640312" cy="4900000"/>',
)

# Add caption before </p:spTree>
caption_s8 = (
    '<p:sp><p:nvSpPr><p:cNvPr id="200" name="caption_s8"/><p:cNvSpPr txBox="1"/><p:nvPr/></p:nvSpPr>'
    '<p:spPr><a:xfrm><a:off x="411480" y="6000000"/><a:ext cx="11300000" cy="540000"/></a:xfrm>'
    '<a:prstGeom prst="rect"><a:avLst/></a:prstGeom><a:noFill/></p:spPr>'
    '<p:txBody><a:bodyPr wrap="square" lIns="45720" tIns="27432" rIns="45720" bIns="27432"><a:spAutoFit/></a:bodyPr><a:lstStyle/>'
    '<a:p><a:pPr algn="ctr"/>'
    '<a:r><a:rPr sz="1400" b="0" i="1"><a:solidFill><a:srgbClr val="57606C"/></a:solidFill><a:latin typeface="Calibri"/></a:rPr>'
    '<a:t>Gender feedback has the largest effect of any attribute </a:t></a:r>'
    '<a:r><a:rPr sz="1400" b="1" i="1"><a:solidFill><a:srgbClr val="C5093B"/></a:solidFill><a:latin typeface="Calibri"/></a:rPr>'
    '<a:t>(20.1% \u2192 40.6%, +20.6 pp, p &lt; .001, N = 1,000).</a:t></a:r></a:p>'
    '<a:p><a:pPr algn="ctr"/>'
    '<a:r><a:rPr sz="1200" b="0" i="1"><a:solidFill><a:srgbClr val="57606C"/></a:solidFill><a:latin typeface="Calibri"/></a:rPr>'
    '<a:t>Systems-of-equations Wald tests confirm the gender-feedback effect is larger than feedback on any other attribute </a:t></a:r>'
    '<a:r><a:rPr sz="1200" b="1" i="1"><a:solidFill><a:srgbClr val="011F5B"/></a:solidFill><a:latin typeface="Calibri"/></a:rPr>'
    '<a:t>(budget F = 65.1, politics F = 57.7, release year F = 12.4; all p\u2019s &lt; .001).</a:t></a:r></a:p>'
    '</p:txBody></p:sp>'
)
s8 = s8.replace("</p:spTree>", caption_s8 + "</p:spTree>")

s8_path.write_text(s8, encoding="utf-8")
print("slide8.xml: caption + systems-test added, figure re-sized")

# =======================================================================
# SLIDE 10 — append systems-test line to existing caption
# =======================================================================
s10_path = SCRATCH / "ppt/slides/slide10.xml"
s10 = s10_path.read_text(encoding="utf-8")

# Current caption ends at "(28.2% -> 41.6%, +13.4 pp, p < .001, N = 1,000)."
# Append a second paragraph with systems-test Wald F-stats.
old_caption_tail = '<a:t>(28.2% \u2192 41.6%, +13.4 pp, p &lt; .001, N = 1,000).</a:t></a:r></a:p></p:txBody>'
new_caption_tail = (
    '<a:t>(28.2% \u2192 41.6%, +13.4 pp, p &lt; .001, N = 1,000).</a:t></a:r></a:p>'
    '<a:p><a:pPr algn="ctr"/>'
    '<a:r><a:rPr sz="1100" b="0" i="1"><a:solidFill><a:srgbClr val="57606C"/></a:solidFill><a:latin typeface="Calibri"/></a:rPr>'
    '<a:t>Wald tests: race-feedback effect larger than every other attribute </a:t></a:r>'
    '<a:r><a:rPr sz="1100" b="1" i="1"><a:solidFill><a:srgbClr val="011F5B"/></a:solidFill><a:latin typeface="Calibri"/></a:rPr>'
    '<a:t>(poetry F = 6.2, &gt;10 books F = 5.5, 1800s F = 5.3; all p\u2019s &lt; .025).</a:t></a:r>'
    '</a:p></p:txBody>'
)
assert old_caption_tail in s10, "slide 10: expected caption anchor not found"
s10 = s10.replace(old_caption_tail, new_caption_tail)

# Also widen the caption shape height a bit to fit the second line
# Current: off y=6080760, ext cy=320040 -> needs ~500000 cy
s10 = s10.replace(
    '<a:off x="411480" y="6080760"/><a:ext cx="6217920" cy="320040"/>',
    '<a:off x="411480" y="5960000"/><a:ext cx="6217920" cy="540000"/>',
)

s10_path.write_text(s10, encoding="utf-8")
print("slide10.xml: systems-test line appended to caption")

# =======================================================================
# SLIDE 14 — remove em dashes, tighten spacing, bump body to 22pt
# =======================================================================
s14_path = SCRATCH / "ppt/slides/slide14.xml"
s14 = s14_path.read_text(encoding="utf-8")

# Replace em dash runs with commas / colon rephrasing
# Bullet 1: "image and reputational concerns — e.g., low selection of women or racial minorities — increases the likelihood"
#   -> "image and reputational concerns (e.g., low selection of women or racial minorities) increases the likelihood"
assert " \u2014 e.g., low selection of women or racial minorities \u2014 " in s14, "slide 14: bullet 1 em-dash anchor missing"
s14 = s14.replace(" \u2014 e.g., low selection of women or racial minorities \u2014 ", " (e.g., low selection of women or racial minorities) ")
# Bullet 4: "reduces the likelihood of their selection — decision makers strive for parity in both directions."
#   -> "reduces the likelihood of their selection; decision makers strive for parity in both directions."
assert " \u2014 decision makers strive for parity in " in s14, "slide 14: bullet 4 em-dash anchor missing"
s14 = s14.replace(" \u2014 decision makers strive for parity in ", "; decision makers strive for parity in ")

# Bump body text from 20pt to 22pt (sz="2000" inside runs)
# Only in the content TextBox (id 9) — bullet glyph (2400) stays
s14 = re.sub(
    r'(<a:rPr[^>]*?)\s+sz="2000"',
    r'\1 sz="2200"',
    s14,
)

# Tighten spacing: spcBef 2600 -> 1800
s14 = s14.replace('<a:spcPts val="2600"/>', '<a:spcPts val="1800"/>')
# Tighten line spacing: 125% -> 115%
s14 = s14.replace('<a:spcPct val="125000"/>', '<a:spcPct val="115000"/>')

s14_path.write_text(s14, encoding="utf-8")
print("slide14.xml: em-dashes replaced, body 22pt, spacing tightened")

# =======================================================================
# SLIDE 15 — swap "impression management" -> "reputational and self-image
#            concerns", same spacing treatment as slide 14
# =======================================================================
s15_path = SCRATCH / "ppt/slides/slide15.xml"
s15 = s15_path.read_text(encoding="utf-8")

s15 = s15.replace(
    "feedback, impression management, and the motivation to respond without prejudice",
    "feedback, reputational and self-image concerns, and the motivation to respond without prejudice",
)

# Upgrade bullet 3 phrasing to match paper ("minimally invasive yet effective intervention")
s15 = s15.replace(
    "is a </a:t></a:r><a:r><a:rPr sz=\"2000\" b=\"1\" i=\"0\"><a:solidFill><a:srgbClr val=\"011F5B\"/></a:solidFill><a:latin typeface=\"Calibri\"/></a:rPr><a:t>minimally invasive intervention</a:t>",
    "is a </a:t></a:r><a:r><a:rPr sz=\"2000\" b=\"1\" i=\"0\"><a:solidFill><a:srgbClr val=\"011F5B\"/></a:solidFill><a:latin typeface=\"Calibri\"/></a:rPr><a:t>minimally invasive yet effective intervention</a:t>",
)

# Body size 20pt -> 22pt (don't touch bullet-glyph size 2800)
s15 = re.sub(
    r'(<a:rPr[^>]*?)\s+sz="2000"',
    r'\1 sz="2200"',
    s15,
)

# spcBef 3600 -> 2000  (slide 15 had wider gaps than slide 14)
s15 = s15.replace('<a:spcPts val="3600"/>', '<a:spcPts val="2000"/>')
# line spacing 125% -> 115%
s15 = s15.replace('<a:spcPct val="125000"/>', '<a:spcPct val="115000"/>')

s15_path.write_text(s15, encoding="utf-8")
print("slide15.xml: reputational-concerns swap + spacing fix")

print("\nDone editing (slide 9 done in second pass).")
