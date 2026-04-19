"""Move the "e.g., 4% of the authors..." italic subtitle OUT of box_root on
slide 3. Instead, show it as an appear-and-disappear callout below the box,
triggered as the FIRST two animation clicks of the slide — per Jose's request
mirroring the WBL deck.

Sequence change:
  Before: box_root contains a tiny italic example; clicks 1..N reveal the
          decision tree + feedback + outcome.
  After:  box_root holds the clean bold title only. Click 1 pops the
          italic example below the box. Click 2 hides it. Clicks 3..N+2
          continue the existing decision-tree reveal (unchanged).

Idempotent-ish: if run twice it will strip any existing second <a:p> in
box_root and will not add a second example_text shape (checks by name).
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

EXAMPLE_SHAPE_ID = 80
EXAMPLE_NAME = "example_text"
EXAMPLE_TEXT = 'e.g., "4% of the authors you cited on your last paper were\u2026"'

# Position: ABOVE box_root in the empty upper-left area (between the hr
# rule and the top of box_root at y=2,526,065). Below the box would
# overlap box_tg_no which sits at y≈4.7" extending across the same x
# range. Above the box is a clean zone where neither tg_yes/tg_no nor
# any arrows will collide.
EX_X = 150000
EX_Y =  900000        # ≈ 1.0" — sits below the horizontal rule (y≈0.85)
EX_W = 4100000        # 4.48"
EX_H =  950000        # 1.04"

# New cTn ids. Slide 3 uses 1-52 today; use 53-60 for the two new clicks.
CTN = dict(
    a_outer=53, a_inner=54, a_eff=55, a_set=56,
    b_outer=57, b_inner=58, b_eff=59, b_set=60,
)


def find_sp_by_name(tree, name):
    for sp in tree.findall(".//p:sp", NSMAP):
        cnv = sp.find("p:nvSpPr/p:cNvPr", NSMAP)
        if cnv is not None and cnv.get("name") == name:
            return sp
    return None


def build_example_shape() -> str:
    return (
        f'<p:sp xmlns:a="{NS_A}" xmlns:p="{NS_P}">'
        f'<p:nvSpPr>'
        f'<p:cNvPr id="{EXAMPLE_SHAPE_ID}" name="{EXAMPLE_NAME}"/>'
        f'<p:cNvSpPr txBox="1"/><p:nvPr/>'
        f'</p:nvSpPr>'
        f'<p:spPr>'
        f'<a:xfrm><a:off x="{EX_X}" y="{EX_Y}"/>'
        f'<a:ext cx="{EX_W}" cy="{EX_H}"/></a:xfrm>'
        f'<a:prstGeom prst="rect"><a:avLst/></a:prstGeom>'
        f'<a:noFill/>'
        f'</p:spPr>'
        f'<p:txBody>'
        f'<a:bodyPr wrap="square" lIns="36576" tIns="18288" rIns="36576" bIns="18288" anchor="ctr"/>'
        f'<a:lstStyle/>'
        f'<a:p><a:pPr algn="l"/>'
        f'<a:r><a:rPr sz="2000" b="0" i="1">'
        f'<a:solidFill><a:srgbClr val="011F5B"/></a:solidFill>'
        f'<a:latin typeface="Calibri"/>'
        f'</a:rPr><a:t>{EXAMPLE_TEXT}</a:t></a:r>'
        f'</a:p>'
        f'</p:txBody>'
        f'</p:sp>'
    )


def build_effect(par_ctn: int, set_ctn: int, preset_class: str, spid: int,
                 target: str, node_type: str) -> str:
    return (
        f'<p:par>'
        f'<p:cTn id="{par_ctn}" presetID="1" presetClass="{preset_class}" '
        f'presetSubtype="0" fill="hold" grpId="0" nodeType="{node_type}">'
        f'<p:stCondLst><p:cond delay="0"/></p:stCondLst>'
        f'<p:childTnLst>'
        f'<p:set>'
        f'<p:cBhvr>'
        f'<p:cTn id="{set_ctn}" dur="1" fill="hold">'
        f'<p:stCondLst><p:cond delay="0"/></p:stCondLst>'
        f'</p:cTn>'
        f'<p:tgtEl><p:spTgt spid="{spid}"/></p:tgtEl>'
        f'<p:attrNameLst><p:attrName>style.visibility</p:attrName></p:attrNameLst>'
        f'</p:cBhvr>'
        f'<p:to><p:strVal val="{target}"/></p:to>'
        f'</p:set>'
        f'</p:childTnLst>'
        f'</p:cTn>'
        f'</p:par>'
    )


def build_click_group(outer_ctn: int, inner_ctn: int, effects_xml: str) -> str:
    return (
        f'<p:par xmlns:a="{NS_A}" xmlns:p="{NS_P}">'
        f'<p:cTn id="{outer_ctn}" fill="hold">'
        f'<p:stCondLst><p:cond delay="indefinite"/></p:stCondLst>'
        f'<p:childTnLst>'
        f'<p:par>'
        f'<p:cTn id="{inner_ctn}" fill="hold">'
        f'<p:stCondLst><p:cond delay="0"/></p:stCondLst>'
        f'<p:childTnLst>'
        f'{effects_xml}'
        f'</p:childTnLst>'
        f'</p:cTn>'
        f'</p:par>'
        f'</p:childTnLst>'
        f'</p:cTn>'
        f'</p:par>'
    )


def main():
    with zipfile.ZipFile(PPTX, "r") as zin:
        slide3 = zin.read("ppt/slides/slide3.xml")
    tree = etree.fromstring(slide3)
    spTree = tree.find(".//p:cSld/p:spTree", NSMAP)

    # 1) Clean up box_root: strip any paragraphs past the first (was the
    # italic 'e.g.'). Keep the title font at the size previously tuned.
    root = find_sp_by_name(tree, "box_root")
    if root is None:
        raise SystemExit("box_root not found on slide 3")
    txBody = root.find("p:txBody", NSMAP)
    paragraphs = txBody.findall("a:p", NSMAP)
    for extra in paragraphs[1:]:
        txBody.remove(extra)

    # 2) Add example_text shape if it isn't there yet (idempotent)
    if find_sp_by_name(tree, EXAMPLE_NAME) is None:
        # Insert before box_citations to keep z-order reasonable
        citations = find_sp_by_name(tree, "box_citations")
        ex_el = etree.fromstring(build_example_shape())
        if citations is not None:
            citations.addprevious(ex_el)
        else:
            spTree.append(ex_el)

    # 3) Extend the timing sequence with 2 new click groups at the START
    # (so the example pops first). Guard: if already injected (CTN 53 exists
    # as an outer cTn), skip.
    timing = tree.find("p:timing", NSMAP)
    mainSeq = None
    for ctn in timing.findall(".//p:seq/p:cTn", NSMAP):
        if ctn.get("nodeType") == "mainSeq":
            mainSeq = ctn
            break
    if mainSeq is None:
        raise SystemExit("mainSeq not found in slide 3 timing")
    outer_childLst = mainSeq.find("p:childTnLst", NSMAP)

    already_injected = any(
        ctn.get("id") == str(CTN["a_outer"])
        for ctn in outer_childLst.findall("p:par/p:cTn", NSMAP)
    )
    if not already_injected:
        appear = build_click_group(
            CTN["a_outer"], CTN["a_inner"],
            build_effect(CTN["a_eff"], CTN["a_set"], "entr",
                          EXAMPLE_SHAPE_ID, "visible", "clickEffect"))
        disappear = build_click_group(
            CTN["b_outer"], CTN["b_inner"],
            build_effect(CTN["b_eff"], CTN["b_set"], "exit",
                          EXAMPLE_SHAPE_ID, "hidden", "clickEffect"))
        # Prepend (in reverse order so 'appear' ends up first)
        outer_childLst.insert(0, etree.fromstring(disappear))
        outer_childLst.insert(0, etree.fromstring(appear))

    # 4) Register example_text in bldLst so PPT starts it hidden
    bldLst = timing.find("p:bldLst", NSMAP)
    if bldLst is None:
        bldLst = etree.SubElement(timing, f"{{{NS_P}}}bldLst")
    already_in_bld = any(
        bp.get("spid") == str(EXAMPLE_SHAPE_ID)
        for bp in bldLst.findall("p:bldP", NSMAP)
    )
    if not already_in_bld:
        bp = etree.SubElement(bldLst, f"{{{NS_P}}}bldP")
        bp.set("spid", str(EXAMPLE_SHAPE_ID))
        bp.set("grpId", "0")
        bp.set("animBg", "1")

    # 5) Write back
    new_xml = etree.tostring(tree, xml_declaration=True, encoding="UTF-8",
                              standalone=True)
    with zipfile.ZipFile(PPTX, "r") as zin, \
         zipfile.ZipFile(TMP, "w", zipfile.ZIP_DEFLATED) as zout:
        for item in zin.infolist():
            data = zin.read(item.filename)
            if item.filename == "ppt/slides/slide3.xml":
                data = new_xml
            zout.writestr(item, data)
    os.replace(TMP, PPTX)
    print(f"OK: slide 3 example callout added (shape id={EXAMPLE_SHAPE_ID})")


if __name__ == "__main__":
    main()
