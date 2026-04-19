"""
35_match_qualtrics.py — rewrite slide 9 stimulus cells to match the Qualtrics
QID40 HTML verbatim.

Source format (pulled from Study-4A/study4A.qsf, QID40):
    <img .../>
    <strong><br>Agatha Christie<br></strong>
    (b. 1890; best-known work: <u>Murder on the Orient Express</u>;
     authored 66 books and 15 short-story collections).

Previous pass rendered the attributes as 3 separate lines without the
underline or the parenthesized single-paragraph flow. This pass:

  1. Reshapes each grid_label to hold two paragraphs exactly:
       P1: bold author name (~8.5pt)
       P2: "(b. YYYY; best-known work: <u>Title</u>; authored N books ...)."
  2. Underlines the best-known work title (u="sng" in the OOXML run).
  3. Adjusts photo vs label proportions so the parenthesized paragraph
     fits without tiny autofit rescaling (photo 680_000, label 450_000).
  4. Changes grid_bg border color from pale gray (#C8CCD3) to black
     to match the Qualtrics table border.
  5. Leaves bg positions, sel_hl/sel_check, title, hr, text_col,
     feedback_card, and animation timeline untouched.

Text is pulled live from study4A.qsf so what lands in the slide is exactly
what participants saw.
"""
from __future__ import annotations

import json
import re
import shutil
import zipfile
from html import escape
from pathlib import Path

ROOT = Path(r"C:/Users/jcerv/Jose/diversity_feedback")
QSF = ROOT / "Study-4A/study4A.qsf"
DECK = ROOT / "Presentations/Chapter_I/Cervantez_Chapter_I_DiversityFeedback.pptx"
SCRATCH = Path(r"C:/tmp/ch1_edit")

# ---------------------------------------------------------------------
# 1. Parse Qualtrics stimulus markup
# ---------------------------------------------------------------------
qsf_json = json.loads(QSF.read_text(encoding="utf-8"))
qid40 = None
for el in qsf_json.get("SurveyElements", []):
    if el.get("Element") == "SQ" and el.get("Payload", {}).get("QuestionID") == "QID40":
        qid40 = el["Payload"]["QuestionText"]
        break
assert qid40 is not None

# Grab each <td> raw inner HTML; within it the pattern is:
#   <b|strong><br>Name<br></b>(b. YYYY; best-known work: <u>Title</u>;
#   authored ... books ...).
tds = re.findall(r"<td[^>]*>(.*?)</td>", qid40, re.DOTALL)
assert len(tds) == 25

author_records = []
for td in tds:
    name_m = re.search(r"<(?:strong|b)>\s*<br\s*/?>\s*([^<]+?)\s*<br", td, re.DOTALL)
    name = re.sub(r"\s+", " ", name_m.group(1)).strip() if name_m else ""

    # After the </(strong|b)> closing, comes the parenthesized span with
    # one or more <u>…</u> spans embedded. Extract the full parenthesized
    # text plus the underlined substring.
    after = re.split(r"</(?:strong|b)>", td, maxsplit=1)[1]
    # Strip leading/trailing whitespace and any stray <br>
    # We want to reconstruct: before_u, u_text, after_u
    u_m = re.search(r"<u>(.+?)</u>", after, re.DOTALL)
    u_text = u_m.group(1).strip() if u_m else ""

    # Full parenthesized plain text (with html entities decoded minimally)
    plain = re.sub(r"<[^>]+>", "", after)
    plain = plain.replace("&nbsp;", " ")
    plain = re.sub(r"\s+", " ", plain).strip()
    # Sometimes trailing period is outside the <span>; normalize
    # Keep from first '(' to last ')' inclusive, plus any trailing '.'
    start = plain.find("(")
    end = plain.rfind(")")
    if start >= 0 and end > start:
        paren = plain[start:end + 1]
        # Include a trailing '.' if present
        if end + 1 < len(plain) and plain[end + 1] == ".":
            paren += "."
    else:
        paren = plain
    # Some entries place "<u>" around part of the title sloppily (e.g.
    # "The <u>Call of the Wild</u>"); compute prefix/suffix around u_text.
    if u_text and u_text in paren:
        i = paren.index(u_text)
        prefix = paren[:i]
        suffix = paren[i + len(u_text):]
    else:
        prefix, suffix = paren, ""
    author_records.append({
        "name": name,
        "prefix": prefix,
        "underline": u_text,
        "suffix": suffix,
    })

for r in author_records[:3]:
    print(f"  {r['name']}: prefix={r['prefix'][:40]!r} u={r['underline']!r} suffix={r['suffix'][:40]!r}")

# ---------------------------------------------------------------------
# 2. Unpack current deck
# ---------------------------------------------------------------------
if SCRATCH.exists():
    shutil.rmtree(SCRATCH)
SCRATCH.mkdir(parents=True)
with zipfile.ZipFile(DECK, "r") as zf:
    zf.extractall(SCRATCH)

s9_path = SCRATCH / "ppt/slides/slide9.xml"
s9 = s9_path.read_text(encoding="utf-8")

# ---------------------------------------------------------------------
# 3. Collect current bg positions (user-modified)
# ---------------------------------------------------------------------
bg_positions: dict[int, tuple[int, int]] = {}
for m in re.finditer(
    r'<p:cNvPr id="\d+" name="grid_bg_(\d{2})"/><p:cNvSpPr/><p:nvPr/></p:nvSpPr>'
    r'<p:spPr><a:xfrm><a:off x="(\d+)" y="(\d+)"/>',
    s9,
):
    bg_positions[int(m.group(1))] = (int(m.group(2)), int(m.group(3)))
assert len(bg_positions) == 25

# ---------------------------------------------------------------------
# 4. Recolor grid_bg border: pale gray (#C8CCD3) -> black (#000000) 1pt
#    and thicken stroke 9525 -> 12700.
# ---------------------------------------------------------------------
s9 = re.sub(
    r'(<p:sp><p:nvSpPr><p:cNvPr id="\d+" name="grid_bg_\d{2}"/>.*?)'
    r'<a:ln w="9525"><a:solidFill><a:srgbClr val="C8CCD3"/></a:solidFill></a:ln>',
    r'\1<a:ln w="12700"><a:solidFill><a:srgbClr val="000000"/></a:solidFill></a:ln>',
    s9,
    flags=re.DOTALL,
)
bg_black_count = s9.count('<a:ln w="12700"><a:solidFill><a:srgbClr val="000000"/></a:solidFill></a:ln>')
print(f"Recolored {bg_black_count} grid_bg borders to black")

# ---------------------------------------------------------------------
# 5. Resize grid_photo and give label real room.
#    Cell is 1_170_000 EMU tall. Longest stimulus paragraph (Agatha
#    Christie, ~110 chars) wraps to 4-5 lines in a 1.17" wide cell.
#    At 6pt body + 8pt bold name, 5 lines of paragraph + name =
#    ~48pt ≈ 610_000 EMU. We allocate 560_000 EMU for the label so the
#    common case (3-4 line paragraph) fits at nominal size and rare
#    overflow is gently caught by normAutofit.
# ---------------------------------------------------------------------
PHOTO_CXCY = 580_000
PHOTO_X_OFFSET = (1_179_576 - PHOTO_CXCY) // 2
PHOTO_Y_OFFSET = 15_000
LABEL_CX = 1_124_712
LABEL_X_OFFSET = (1_179_576 - LABEL_CX) // 2
LABEL_Y_OFFSET = 605_000
LABEL_CY = 555_000  # 10k bottom gutter

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
assert n_ph == 25
print(f"Resized {n_ph} photos to {PHOTO_CXCY} EMU (~0.71\", 58% of cell width)")

# ---------------------------------------------------------------------
# 6. Rewrite each grid_label with Qualtrics-style two-paragraph layout.
# ---------------------------------------------------------------------
NAME_PT = 800      # 8pt bold
BODY_PT = 600      # 6pt body (compact to keep long paragraphs within label)
GRAY = "111111"
NAVY = "011F5B"

def build_label_xml(bg_x: int, bg_y: int, shape_id: int, idx: int, author: dict) -> str:
    name = escape(author["name"])
    prefix = escape(author["prefix"])
    u_text = escape(author["underline"])
    suffix = escape(author["suffix"])

    # Build the parenthesized paragraph runs
    paren_runs = []
    if prefix:
        paren_runs.append(
            f'<a:r><a:rPr sz="{BODY_PT}" b="0" i="0">'
            f'<a:solidFill><a:srgbClr val="{GRAY}"/></a:solidFill>'
            f'<a:latin typeface="Calibri"/></a:rPr>'
            f'<a:t>{prefix}</a:t></a:r>'
        )
    if u_text:
        paren_runs.append(
            f'<a:r><a:rPr sz="{BODY_PT}" b="0" i="0" u="sng">'
            f'<a:solidFill><a:srgbClr val="{GRAY}"/></a:solidFill>'
            f'<a:latin typeface="Calibri"/></a:rPr>'
            f'<a:t>{u_text}</a:t></a:r>'
        )
    if suffix:
        paren_runs.append(
            f'<a:r><a:rPr sz="{BODY_PT}" b="0" i="0">'
            f'<a:solidFill><a:srgbClr val="{GRAY}"/></a:solidFill>'
            f'<a:latin typeface="Calibri"/></a:rPr>'
            f'<a:t>{suffix}</a:t></a:r>'
        )

    return (
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
        f'<p:txBody>'
        # normAutofit without explicit fontScale = PowerPoint auto-computes
        # the shrink factor when a cell's paragraph overflows.
        f'<a:bodyPr wrap="square" lIns="18288" tIns="5000" rIns="18288" bIns="5000" anchor="t">'
        f'<a:normAutofit/>'
        f'</a:bodyPr>'
        f'<a:lstStyle/>'
        # Paragraph 1: bold name
        f'<a:p><a:pPr algn="ctr"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc></a:pPr>'
        f'<a:r><a:rPr sz="{NAME_PT}" b="1" i="0">'
        f'<a:solidFill><a:srgbClr val="{GRAY}"/></a:solidFill>'
        f'<a:latin typeface="Calibri"/></a:rPr>'
        f'<a:t>{name}</a:t></a:r></a:p>'
        # Paragraph 2: (b. YYYY; best-known work: <u>Title</u>; authored...).
        f'<a:p><a:pPr algn="ctr">'
        f'<a:lnSpc><a:spcPct val="100000"/></a:lnSpc>'
        f'<a:spcBef><a:spcPts val="200"/></a:spcBef>'
        f'</a:pPr>'
        f'{"".join(paren_runs)}'
        f'</a:p>'
        f'</p:txBody>'
        f'</p:sp>'
    )

# Replace each grid_label block in-place
label_re = re.compile(
    r'<p:sp><p:nvSpPr><p:cNvPr id="(\d+)" name="grid_label_(\d{2})"/>'
    r'.*?</p:sp>',
    re.DOTALL,
)
def _label_sub(m):
    shape_id = int(m.group(1))
    idx = int(m.group(2))
    bg_x, bg_y = bg_positions[idx]
    return build_label_xml(bg_x, bg_y, shape_id, idx, author_records[idx])
s9, n_lb = label_re.subn(_label_sub, s9)
assert n_lb == 25, f"label rewrites expected 25 got {n_lb}"
print(f"Rewrote {n_lb} labels in Qualtrics two-paragraph format with underline")

# ---------------------------------------------------------------------
# 7. Write back + repack
# ---------------------------------------------------------------------
s9_path.write_text(s9, encoding="utf-8")

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
