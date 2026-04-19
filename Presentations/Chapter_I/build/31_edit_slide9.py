"""
31_edit_slide9.py — Slide 9 (Study 2 task & stimuli) polish pass, v2.

v2 changes:
  - Fold author attributes (b.YYYY / best-known work / book count) INTO the
    existing grid_label_XX shape. No new shapes are created, so the
    entrance-animation timeline (clickEffect on grid_bg_00, withEffect for
    every other grid shape) stays intact.
  - Use the full QSF stimulus text so the deck matches what participants saw.

Pulls stimulus text directly from `Study-4A/study4A.qsf` (no API call needed).
"""
from __future__ import annotations

import json
import re
import sys
from pathlib import Path
from xml.sax.saxutils import escape

ROOT = Path(r"C:/Users/jcerv/Jose/diversity_feedback")
QSF = ROOT / "Study-4A/study4A.qsf"
SCRATCH = Path(r"C:/tmp/ch1_edit")

# ---------------------------------------------------------------------
# Parse QSF to get each author's full stimulus text
# ---------------------------------------------------------------------
data = json.loads(QSF.read_text(encoding="utf-8"))
qid40 = None
for el in data.get("SurveyElements", []):
    if el.get("Element") == "SQ" and el.get("Payload", {}).get("QuestionID") == "QID40":
        qid40 = el["Payload"]["QuestionText"]
        break
assert qid40 is not None, "QID40 not found"

tds = re.findall(r"<td[^>]*>(.*?)</td>", qid40, re.DOTALL)
authors = []
for td in tds:
    plain = re.sub(r"<[^>]+>", "", td)
    # Normalize whitespace & decode entities
    plain = plain.replace("&nbsp;", " ").strip()
    m = re.match(r"^(.+?)\((.+)\)\.?\s*$", plain, re.DOTALL)
    if not m:
        continue
    name = re.sub(r"\s+", " ", m.group(1)).strip()
    desc = re.sub(r"\s+", " ", m.group(2)).strip()
    # desc is semicolon-separated:
    #   b. 1890; best-known work: Murder on the Orient Express;
    #   authored 66 books and 15 short-story collections
    parts = [p.strip() for p in desc.split(";")]
    born = next((p for p in parts if p.startswith("b.")), "")
    work = next((p for p in parts if "best-known work" in p.lower()), "")
    auth = next((p for p in parts if p.lower().startswith("authored")), "")
    # Strip "best-known work: " prefix
    if ":" in work:
        work = work.split(":", 1)[1].strip()
    authors.append({
        "name": name,
        "born": born,
        "work": work,
        "authored": auth,
    })

print(f"Parsed {len(authors)} authors from QSF")
for a in authors[:3]:
    print(f"  {a['name']}: {a['born']} | {a['work']} | {a['authored']}")

author_by_name = {a["name"]: a for a in authors}

# ---------------------------------------------------------------------
# Edit slide 9 XML
# ---------------------------------------------------------------------
s9_path = SCRATCH / "ppt/slides/slide9.xml"
s9 = s9_path.read_text(encoding="utf-8")

# Layout targets (EMU). Shrink photos & extend labels to fit name + 3 attr
# lines. All sizes preserve bg cy=1033271 and leave gutter at the bottom.
PHOTO_CXCY = 500000
PHOTO_X_OFFSET = (1179576 - PHOTO_CXCY) // 2   # 339788
PHOTO_Y_OFFSET = 30000
LABEL_CX = 1124712
LABEL_X_OFFSET = (1179576 - LABEL_CX) // 2     # 27432
LABEL_Y_OFFSET = 545000                        # just below photo
LABEL_CY = 470000                              # tall enough for 4 lines

# ---- helper: build new txBody for a label, with name + 3 attribute lines
def build_label_txbody(author):
    name = escape(author["name"])
    born = escape(author["born"])
    work = escape(author["work"])
    auth = escape(author["authored"])

    return (
        '<p:txBody>'
        '<a:bodyPr wrap="square" lIns="18288" tIns="9144" rIns="18288" bIns="9144" anchor="t"/>'
        '<a:lstStyle/>'
        # Name
        '<a:p><a:pPr algn="ctr"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc></a:pPr>'
        '<a:r><a:rPr sz="900" b="1" i="0">'
        '<a:solidFill><a:srgbClr val="111111"/></a:solidFill>'
        '<a:latin typeface="Calibri"/></a:rPr>'
        f'<a:t>{name}</a:t></a:r></a:p>'
        # Born
        '<a:p><a:pPr algn="ctr"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc>'
        '<a:spcBef><a:spcPts val="150"/></a:spcBef></a:pPr>'
        '<a:r><a:rPr sz="700" b="0" i="1">'
        '<a:solidFill><a:srgbClr val="57606C"/></a:solidFill>'
        '<a:latin typeface="Calibri"/></a:rPr>'
        f'<a:t>{born}</a:t></a:r></a:p>'
        # Best-known work (italic, smaller if needed)
        '<a:p><a:pPr algn="ctr"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc>'
        '<a:spcBef><a:spcPts val="100"/></a:spcBef></a:pPr>'
        '<a:r><a:rPr sz="700" b="0" i="1">'
        '<a:solidFill><a:srgbClr val="57606C"/></a:solidFill>'
        '<a:latin typeface="Calibri"/></a:rPr>'
        f'<a:t>{work}</a:t></a:r></a:p>'
        # Authored
        '<a:p><a:pPr algn="ctr"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc>'
        '<a:spcBef><a:spcPts val="100"/></a:spcBef></a:pPr>'
        '<a:r><a:rPr sz="700" b="0" i="1">'
        '<a:solidFill><a:srgbClr val="57606C"/></a:solidFill>'
        '<a:latin typeface="Calibri"/></a:rPr>'
        f'<a:t>{auth}</a:t></a:r></a:p>'
        '</p:txBody>'
    )

# ---- Process each grid cell by regex (non-parsing, to preserve byte order)
# Pattern: find each grid_photo and its associated bg y/x offsets; then find
# the matching grid_label shape and rewrite its position + txBody.

# First: determine bg x,y for each grid index by scanning
bg_re = re.compile(
    r'<p:sp>\s*<p:nvSpPr>\s*<p:cNvPr id="(\d+)" name="grid_bg_(\d{2})"/>.*?'
    r'<a:off x="(\d+)" y="(\d+)"/>',
    re.DOTALL,
)
bg_positions = {}
for m in bg_re.finditer(s9):
    idx = int(m.group(2))
    bg_positions[idx] = (int(m.group(3)), int(m.group(4)))
assert len(bg_positions) == 25, f"expected 25 bg shapes, got {len(bg_positions)}"

# --- update photo geometry (cx/cy 722375 -> PHOTO_CXCY; x/y anchored to bg)
def resize_photo(match):
    pid = int(match.group(1))  # cNvPr id
    idx = int(match.group(2))
    bg_x, bg_y = bg_positions[idx]
    new_off_x = bg_x + PHOTO_X_OFFSET
    new_off_y = bg_y + PHOTO_Y_OFFSET
    header = match.group(0).split("<a:off")[0]
    return (
        f'{header}<a:off x="{new_off_x}" y="{new_off_y}"/>'
        f'<a:ext cx="{PHOTO_CXCY}" cy="{PHOTO_CXCY}"/>'
    )

# Match each grid_photo_XX shape up to (and including) its <a:off> + <a:ext>
photo_re = re.compile(
    r'(<p:pic>\s*<p:nvPicPr>\s*<p:cNvPr id="(\d+)" name="grid_photo_(\d{2})"[^/]*/>'
    r'.*?<a:xfrm>)\s*<a:off [^/]+/>\s*<a:ext [^/]+/>',
    re.DOTALL,
)

def _photo_sub(m):
    idx = int(m.group(3))
    bg_x, bg_y = bg_positions[idx]
    head = m.group(1)
    return (
        f'{head}<a:off x="{bg_x + PHOTO_X_OFFSET}" y="{bg_y + PHOTO_Y_OFFSET}"/>'
        f'<a:ext cx="{PHOTO_CXCY}" cy="{PHOTO_CXCY}"/>'
    )

s9, n_photos = photo_re.subn(_photo_sub, s9)
assert n_photos == 25, f"expected 25 photo rewrites, got {n_photos}"
print(f"Resized {n_photos} photos")

# --- Now rewrite each grid_label shape: new xfrm + new txBody ------
label_re = re.compile(
    r'<p:sp>\s*<p:nvSpPr>\s*<p:cNvPr id="(\d+)" name="grid_label_(\d{2})"/>'
    r'.*?</p:sp>',
    re.DOTALL,
)

def _label_sub(m):
    idx = int(m.group(2))
    bg_x, bg_y = bg_positions[idx]
    shape_id = m.group(1)

    # Extract author name from old label to look up attrs
    name_m = re.search(r"<a:t>([^<]+)</a:t>", m.group(0))
    old_name = name_m.group(1).strip() if name_m else ""
    author = author_by_name.get(old_name)
    if not author:
        # Fallback: minimal replacement
        print(f"  WARN: author {old_name!r} not found in QSF")
        return m.group(0)

    tx = build_label_txbody(author)
    new_label = (
        f'<p:sp>'
        f'<p:nvSpPr>'
        f'<p:cNvPr id="{shape_id}" name="grid_label_{idx:02d}"/>'
        f'<p:cNvSpPr txBox="1"/>'
        f'<p:nvPr/>'
        f'</p:nvSpPr>'
        f'<p:spPr>'
        f'<a:xfrm>'
        f'<a:off x="{bg_x + LABEL_X_OFFSET}" y="{bg_y + LABEL_Y_OFFSET}"/>'
        f'<a:ext cx="{LABEL_CX}" cy="{LABEL_CY}"/>'
        f'</a:xfrm>'
        f'<a:prstGeom prst="rect"><a:avLst/></a:prstGeom>'
        f'<a:noFill/>'
        f'</p:spPr>'
        f'{tx}'
        f'</p:sp>'
    )
    return new_label

s9, n_labels = label_re.subn(_label_sub, s9)
assert n_labels == 25, f"expected 25 label rewrites, got {n_labels}"
print(f"Rewrote {n_labels} labels with name + stimulus attributes")

# ---------------------------------------------------------------------
# Rewrite text_col: Sample + Instructions + Attributes layout
# (slide-5 style). Use direct string replacement to preserve surrounding
# anim/bldLst structure.
# ---------------------------------------------------------------------
# Find start of text_col shape
tc_start = s9.find('<p:cNvPr id="47" name="text_col"/>')
assert tc_start > 0, "text_col shape not found"
# Go back to the opening <p:sp>
sp_open = s9.rfind("<p:sp>", 0, tc_start)
sp_close = s9.find("</p:sp>", tc_start) + len("</p:sp>")
assert sp_open > 0 and sp_close > sp_open

new_text_col = (
    '<p:sp>'
    '<p:nvSpPr>'
    '<p:cNvPr id="47" name="text_col"/>'
    '<p:cNvSpPr txBox="1"/>'
    '<p:nvPr/>'
    '</p:nvSpPr>'
    '<p:spPr>'
    '<a:xfrm><a:off x="411480" y="1051560"/><a:ext cx="5303520" cy="5349240"/></a:xfrm>'
    '<a:prstGeom prst="rect"><a:avLst/></a:prstGeom>'
    '<a:noFill/>'
    '</p:spPr>'
    '<p:txBody>'
    '<a:bodyPr wrap="square" lIns="73152" tIns="36576" rIns="73152" bIns="36576"><a:spAutoFit/></a:bodyPr>'
    '<a:lstStyle/>'
    # Sample
    '<a:p><a:pPr algn="l"/>'
    '<a:r><a:rPr sz="2000" b="1" i="0" dirty="0">'
    '<a:solidFill><a:srgbClr val="011F5B"/></a:solidFill>'
    '<a:latin typeface="Calibri"/></a:rPr>'
    '<a:t>Sample.  </a:t></a:r>'
    '<a:r><a:rPr sz="2000" b="0" i="0" dirty="0">'
    '<a:solidFill><a:srgbClr val="111111"/></a:solidFill>'
    '<a:latin typeface="Calibri"/></a:rPr>'
    '<a:t>N = 1,000 (MTurk, pre-registered).</a:t></a:r></a:p>'
    # Instructions header
    '<a:p><a:pPr algn="l"><a:spcBef><a:spcPts val="1400"/></a:spcBef></a:pPr>'
    '<a:r><a:rPr b="1" i="0" dirty="0">'
    '<a:solidFill><a:srgbClr val="011F5B"/></a:solidFill>'
    '<a:latin typeface="Calibri"/></a:rPr>'
    '<a:t>Instructions</a:t></a:r></a:p>'
    # Instructions body
    '<a:p><a:pPr algn="l">'
    '<a:lnSpc><a:spcPct val="115000"/></a:lnSpc>'
    '<a:spcBef><a:spcPts val="600"/></a:spcBef></a:pPr>'
    '<a:r><a:rPr sz="1800" b="0" i="1" dirty="0">'
    '<a:solidFill><a:srgbClr val="111111"/></a:solidFill>'
    '<a:latin typeface="Calibri"/></a:rPr>'
    '<a:t>You\u2019ve been tasked with creating a summer fiction reading list for your local library. Please select six authors to include on the list.</a:t>'
    '</a:r></a:p>'
    # Attributes header
    '<a:p><a:pPr algn="l"><a:spcBef><a:spcPts val="1600"/></a:spcBef></a:pPr>'
    '<a:r><a:rPr b="1" i="0" dirty="0">'
    '<a:solidFill><a:srgbClr val="011F5B"/></a:solidFill>'
    '<a:latin typeface="Calibri"/></a:rPr>'
    '<a:t>Attributes</a:t></a:r></a:p>'
    # Bullets
    '<a:p><a:pPr lvl="1" algn="l"><a:spcBef><a:spcPts val="600"/></a:spcBef></a:pPr>'
    '<a:r><a:rPr sz="1800" b="1" i="0"><a:solidFill><a:srgbClr val="3C5A99"/></a:solidFill><a:latin typeface="Calibri"/></a:rPr><a:t>\u2022  </a:t></a:r>'
    '<a:r><a:rPr sz="1600" b="0" i="0"><a:solidFill><a:srgbClr val="111111"/></a:solidFill><a:latin typeface="Calibri"/></a:rPr><a:t>Wrote poetry</a:t></a:r></a:p>'
    '<a:p><a:pPr lvl="1" algn="l"><a:spcBef><a:spcPts val="600"/></a:spcBef></a:pPr>'
    '<a:r><a:rPr sz="1800" b="1" i="0"><a:solidFill><a:srgbClr val="3C5A99"/></a:solidFill><a:latin typeface="Calibri"/></a:rPr><a:t>\u2022  </a:t></a:r>'
    '<a:r><a:rPr sz="1600" b="0" i="0"><a:solidFill><a:srgbClr val="111111"/></a:solidFill><a:latin typeface="Calibri"/></a:rPr><a:t>Wrote more than 10 books</a:t></a:r></a:p>'
    '<a:p><a:pPr lvl="1" algn="l"><a:spcBef><a:spcPts val="600"/></a:spcBef></a:pPr>'
    '<a:r><a:rPr sz="1800" b="1" i="0"><a:solidFill><a:srgbClr val="3C5A99"/></a:solidFill><a:latin typeface="Calibri"/></a:rPr><a:t>\u2022  </a:t></a:r>'
    '<a:r><a:rPr sz="1600" b="0" i="0"><a:solidFill><a:srgbClr val="111111"/></a:solidFill><a:latin typeface="Calibri"/></a:rPr><a:t>Born in the 1800s</a:t></a:r></a:p>'
    '<a:p><a:pPr lvl="1" algn="l"><a:spcBef><a:spcPts val="600"/></a:spcBef></a:pPr>'
    '<a:r><a:rPr sz="1800" b="1" i="0"><a:solidFill><a:srgbClr val="3C5A99"/></a:solidFill><a:latin typeface="Calibri"/></a:rPr><a:t>\u2022  </a:t></a:r>'
    '<a:r><a:rPr sz="1600" b="1" i="0"><a:solidFill><a:srgbClr val="C5093B"/></a:solidFill><a:latin typeface="Calibri"/></a:rPr><a:t>Racial minorities</a:t></a:r>'
    '<a:r><a:rPr sz="1200" b="0" i="1"><a:solidFill><a:srgbClr val="C5093B"/></a:solidFill><a:latin typeface="Calibri"/></a:rPr><a:t>   (focal condition)</a:t></a:r></a:p>'
    '</p:txBody>'
    '</p:sp>'
)

s9 = s9[:sp_open] + new_text_col + s9[sp_close:]
print("Rewrote text_col (Instructions + Attributes layout)")

# Write back
s9_path.write_text(s9, encoding="utf-8")
print(f"Wrote slide9.xml ({len(s9):,} chars)")
