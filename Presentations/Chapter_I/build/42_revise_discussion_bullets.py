"""Second-pass revision of the Discussion slide (slide 14) bullets.

Changes from the first pass:
  - Remove em-dashes (Jose disliked them).
  - Lift phrasing from Manuscript_DoesFeedbackEnhanceDiversity.docx:
      * "reputational and self-image concerns" (p. 161)
      * "motivation to respond without prejudice" (throughout)
      * "underrepresentation" / "overrepresented"
      * "strive for parity" (p. 159)
      * "minimally invasive yet effective intervention" (p. 163)
      * "quotas or heavy-handed policies" / "backlash" (pp. 14, 163)

Operates on the CURRENT slide 14 (which was already retitled "Discussion"
by 39_condense_discussion.py). Only the content placeholder (TextBox 8) is
rewritten — title, shape geometry, footer, and bldLst are untouched.
"""
from __future__ import annotations
import os
import zipfile
from lxml import etree

HERE = os.path.dirname(os.path.abspath(__file__))
PPTX = os.path.abspath(os.path.join(HERE, "..",
                                    "Cervantez_Chapter_I_DiversityFeedback.pptx"))
TMP = PPTX + ".tmp"

NS_A = "http://schemas.openxmlformats.org/drawingml/2006/main"
NS_P = "http://schemas.openxmlformats.org/presentationml/2006/main"
NSMAP = {"a": NS_A, "p": NS_P}

# Navy for bolded keywords, near-black for normal text, burgundy bullet.
COLOR_KEYWORD = "011F5B"
COLOR_BODY = "111111"
COLOR_BULLET = "C5093B"

BULLETS = [
    [  # 1 — mechanism (no em-dash, manuscript wording)
        ("Descriptive feedback revealing ", False),
        ("underrepresentation", True),
        (" of women or racial minorities elicits ", False),
        ("reputational and self-image concerns", True),
        (" and activates the ", False),
        ("motivation to respond without prejudice", True),
        (", increasing their selection in future decisions.", False),
    ],
    [  # 2 — specificity + overrepresentation reversal
        ("The effect is specific to ", False),
        ("DEI-relevant attributes", True),
        (": feedback about ", False),
        ("experience", True),
        (" or ", False),
        ("release year", True),
        (" has much weaker influence, and when women or racial minorities are ", False),
        ("overrepresented", True),
        (", the effect reverses as people ", False),
        ("strive for parity", True),
        (".", False),
    ],
    [  # 3 — implication / practical takeaway
        ("Descriptive feedback is a ", False),
        ("minimally invasive yet effective intervention", True),
        (" that increases the representation of ", False),
        ("underrepresented groups", True),
        (" in selection decisions, without relying on ", False),
        ("quotas or heavy-handed policies", True),
        (" that can trigger ", False),
        ("backlash", True),
        (".", False),
    ],
]


def run_xml(text: str, bold: bool) -> str:
    color = COLOR_KEYWORD if bold else COLOR_BODY
    b = "1" if bold else "0"
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
    body = "".join(run_xml(t, b) for t, b in runs)
    return f'<a:p xmlns:a="{NS_A}">{pPr}{marker}{body}</a:p>'


def main():
    with zipfile.ZipFile(PPTX, "r") as zin:
        slide14 = zin.read("ppt/slides/slide14.xml")

    tree = etree.fromstring(slide14)
    content = None
    for sp in tree.findall(".//p:sp", NSMAP):
        cnv = sp.find("p:nvSpPr/p:cNvPr", NSMAP)
        if cnv is not None and cnv.get("name") == "TextBox 8":
            content = sp
            break
    if content is None:
        raise SystemExit("TextBox 8 not found on slide 14")

    txBody = content.find("p:txBody", NSMAP)
    for p in txBody.findall("a:p", NSMAP):
        txBody.remove(p)
    for i, bullet in enumerate(BULLETS):
        txBody.append(etree.fromstring(bullet_xml(bullet, is_first=(i == 0))))

    new_xml = etree.tostring(tree, xml_declaration=True, encoding="UTF-8",
                              standalone=True)
    with zipfile.ZipFile(PPTX, "r") as zin, \
         zipfile.ZipFile(TMP, "w", zipfile.ZIP_DEFLATED) as zout:
        for item in zin.infolist():
            data = zin.read(item.filename)
            if item.filename == "ppt/slides/slide14.xml":
                data = new_xml
            zout.writestr(item, data)
    os.replace(TMP, PPTX)
    print("OK: Discussion bullets revised (manuscript language, no em-dashes)")


if __name__ == "__main__":
    main()
