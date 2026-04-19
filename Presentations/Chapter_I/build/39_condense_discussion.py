"""Condense slides 14 (Key Findings) + 15 (Discussion) into a single
Discussion slide.

Slide 14 is rewritten in place (title -> "Discussion", 3 merged bullets with
inline-bolded navy keywords) so Jose's manual line-spacing/font tweaks on
slide 14 are preserved structurally.

Slide 15 is deleted from the pptx:
  - ppt/slides/slide15.xml and its .rels removed from zip
  - relationship removed from ppt/_rels/presentation.xml.rels
  - sldId entry removed from ppt/presentation.xml (sldIdLst + p14:section)
  - Override removed from [Content_Types].xml

Slide 16 (Thank You) stays as slide16.xml on disk but becomes the new slide
15 in presentation order. It has no footer slide-number text, so no footer
updates are needed.
"""
from __future__ import annotations
import os
import re
import zipfile
from lxml import etree

HERE = os.path.dirname(os.path.abspath(__file__))
PPTX = os.path.abspath(os.path.join(HERE, "..",
                                    "Cervantez_Chapter_I_DiversityFeedback.pptx"))
TMP = PPTX + ".tmp"

NS_A = "http://schemas.openxmlformats.org/drawingml/2006/main"
NS_P = "http://schemas.openxmlformats.org/presentationml/2006/main"
NSMAP = {"a": NS_A, "p": NS_P}

# -------------------- new bullets --------------------
# Each bullet is a list of (text, is_bold) tuples. Bold runs render navy,
# non-bold render near-black, per feedback_bulleted_slides memory.
BULLETS = [
    [  # 1 — mechanism
        ("Descriptive feedback about ", False),
        ("low selection of women or racial minorities", True),
        (" raises ", False),
        ("reputational concerns", True),
        (" and activates the ", False),
        ("motivation to respond without prejudice", True),
        (", increasing their future selection.", False),
    ],
    [  # 2 — boundary + overrepresentation
        ("The effect is specific: attributes without ", False),
        ("reputational stakes", True),
        (" (e.g., ", False),
        ("experience", True),
        (", ", False),
        ("release year", True),
        (") produce a ", False),
        ("much weaker effect", True),
        (", and feedback about ", False),
        ("overrepresentation reduces selection", True),
        (" \u2014 revealing ", False),
        ("internalized norms", True),
        (" about proper representation.", False),
    ],
    [  # 3 — implication
        ("Descriptive feedback is a ", False),
        ("minimally invasive yet effective", True),
        (" intervention to ", False),
        ("enhance diversity in selection decisions", True),
        (", even amid the current ", False),
        ("DEI backlash", True),
        (".", False),
    ],
]

COLOR_KEYWORD = "011F5B"   # navy
COLOR_BODY = "111111"      # near-black
COLOR_BULLET = "C5093B"    # burgundy bullet marker


def run_xml(text: str, is_bold: bool) -> str:
    color = COLOR_KEYWORD if is_bold else COLOR_BODY
    b = "1" if is_bold else "0"
    # Escape XML special chars in text
    text = (text.replace("&", "&amp;")
                .replace("<", "&lt;")
                .replace(">", "&gt;"))
    return (
        f'<a:r><a:rPr sz="2200" b="{b}" i="0" dirty="0">'
        f'<a:solidFill><a:srgbClr val="{color}"/></a:solidFill>'
        f'<a:latin typeface="Calibri"/>'
        f'</a:rPr><a:t>{text}</a:t></a:r>'
    )


def bullet_xml(runs, is_first: bool) -> str:
    pPr = '<a:pPr algn="l"><a:lnSpc><a:spcPct val="115000"/></a:lnSpc>'
    if not is_first:
        pPr += '<a:spcBef><a:spcPts val="1800"/></a:spcBef>'
    pPr += "</a:pPr>"
    marker = (
        '<a:r><a:rPr sz="2400" b="1" i="0" dirty="0">'
        f'<a:solidFill><a:srgbClr val="{COLOR_BULLET}"/></a:solidFill>'
        '<a:latin typeface="Calibri"/>'
        '</a:rPr><a:t>\u2022  </a:t></a:r>'
    )
    body = "".join(run_xml(text, bold) for text, bold in runs)
    return f'<a:p xmlns:a="{NS_A}">{pPr}{marker}{body}</a:p>'


# -------------------- slide 14 rewrite --------------------
def rewrite_slide14(xml_bytes: bytes) -> bytes:
    tree = etree.fromstring(xml_bytes)
    for sp in tree.findall(".//p:sp", NSMAP):
        cnv = sp.find("p:nvSpPr/p:cNvPr", NSMAP)
        if cnv is None:
            continue
        name = cnv.get("name", "")
        if name == "s_title":
            # Replace title text runs with a single run saying "Discussion"
            txBody = sp.find("p:txBody", NSMAP)
            # Strip existing paragraphs
            for p in txBody.findall("a:p", NSMAP):
                txBody.remove(p)
            title_p = etree.fromstring(
                f'<a:p xmlns:a="{NS_A}"><a:pPr algn="l"/>'
                f'<a:r><a:rPr sz="3000" b="1">'
                f'<a:solidFill><a:srgbClr val="{COLOR_KEYWORD}"/></a:solidFill>'
                f'<a:latin typeface="Calibri"/>'
                f'</a:rPr><a:t>Discussion</a:t></a:r></a:p>'
            )
            txBody.append(title_p)
        elif name == "TextBox 8":
            txBody = sp.find("p:txBody", NSMAP)
            for p in txBody.findall("a:p", NSMAP):
                txBody.remove(p)
            for i, bullet in enumerate(BULLETS):
                txBody.append(etree.fromstring(bullet_xml(bullet, is_first=(i == 0))))
    return etree.tostring(tree, xml_declaration=True, encoding="UTF-8",
                          standalone=True)


# -------------------- slide 15 deletion --------------------
def delete_slide15(prs_rels: str, prs_xml: str, ct_xml: str):
    # Find rId for slide15.xml
    m = re.search(
        r'<Relationship Id="(rId\d+)"[^>]*Target="slides/slide15\.xml"\s*/>',
        prs_rels)
    if not m:
        raise SystemExit("no <Relationship> pointing to slides/slide15.xml")
    slide15_rid = m.group(1)

    # Find the sldId entry's numeric id so we can also remove the p14:sldId
    m2 = re.search(rf'<p:sldId id="(\d+)" r:id="{slide15_rid}"/>', prs_xml)
    if not m2:
        raise SystemExit(f"no <p:sldId> for {slide15_rid}")
    sld_id_num = m2.group(1)

    # Redactions
    prs_rels_new = re.sub(
        rf'<Relationship Id="{slide15_rid}"[^>]*/>', "", prs_rels)
    prs_xml_new = prs_xml
    prs_xml_new = re.sub(
        rf'<p:sldId id="{sld_id_num}" r:id="{slide15_rid}"/>', "",
        prs_xml_new)
    prs_xml_new = re.sub(
        rf'<p14:sldId id="{sld_id_num}"/>', "", prs_xml_new)
    ct_xml_new = re.sub(
        r'<Override PartName="/ppt/slides/slide15\.xml"[^>]*?/>', "", ct_xml)

    return prs_rels_new, prs_xml_new, ct_xml_new, slide15_rid


def main():
    with zipfile.ZipFile(PPTX, "r") as zin:
        slide14 = zin.read("ppt/slides/slide14.xml")
        prs_xml = zin.read("ppt/presentation.xml").decode("utf-8")
        prs_rels = zin.read("ppt/_rels/presentation.xml.rels").decode("utf-8")
        ct_xml = zin.read("[Content_Types].xml").decode("utf-8")

    slide14_new = rewrite_slide14(slide14)
    prs_rels_new, prs_xml_new, ct_xml_new, rid = delete_slide15(
        prs_rels, prs_xml, ct_xml)

    skip_files = {"ppt/slides/slide15.xml", "ppt/slides/_rels/slide15.xml.rels"}

    with zipfile.ZipFile(PPTX, "r") as zin, \
         zipfile.ZipFile(TMP, "w", zipfile.ZIP_DEFLATED) as zout:
        for item in zin.infolist():
            name = item.filename
            if name in skip_files:
                continue
            if name == "ppt/slides/slide14.xml":
                zout.writestr(item, slide14_new)
            elif name == "ppt/presentation.xml":
                zout.writestr(item, prs_xml_new.encode("utf-8"))
            elif name == "ppt/_rels/presentation.xml.rels":
                zout.writestr(item, prs_rels_new.encode("utf-8"))
            elif name == "[Content_Types].xml":
                zout.writestr(item, ct_xml_new.encode("utf-8"))
            else:
                zout.writestr(item, zin.read(name))
    os.replace(TMP, PPTX)
    print(f"OK: slide 14 rewritten, slide 15 deleted (was {rid})")


if __name__ == "__main__":
    main()
