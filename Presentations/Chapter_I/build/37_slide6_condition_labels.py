"""Add swapping condition labels above the Study 1 feedback card on slide 6.

Initial state: "No Gender Feedback" (navy) visible above the card.
On the existing click (the same click that already reveals treat_occluder +
treat_item swapping the 4th bullet), the navy label hides and "Gender
Feedback" (burgundy) appears in its place.

Implementation: two stacked text-box shapes (control_label always-visible,
treat_label hidden-until-animated). Two new effects are appended to the
existing click group in <p:timing>:
  - exit effect that hides control_label
  - entrance effect that reveals treat_label
Both fire withEffect alongside the existing treat_occluder/treat_item reveal
so a single click flips all four state changes in one beat.
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
NS_R = "http://schemas.openxmlformats.org/officeDocument/2006/relationships"
NSMAP = {"a": NS_A, "p": NS_P, "r": NS_R}

# New shape IDs (slide 6 currently tops out at 17).
ID_CONTROL = 18
ID_TREAT = 19

# New cTn IDs for the extended click-group effects (existing uses 1..8).
CTN_CTL_PAR = 9
CTN_CTL_SET = 10
CTN_TRT_PAR = 11
CTN_TRT_SET = 12

# Horizontal center of the feedback card is at (1874519 + 8458200/2) ≈ 6103619.
# Label spans 4.37" wide; place centered over the card, above its top edge
# (card top = 1828800 EMU = 2.00").
LABEL_W = 4000000
LABEL_H = 560000
LABEL_X = 6103619 - LABEL_W // 2        # 4103619
LABEL_Y = 1060000                        # ≈ 1.16" — sits below title, above card


def build_label(id_: int, name: str, text: str, rgb_hex: str) -> str:
    return (
        f'<p:sp xmlns:a="{NS_A}" xmlns:p="{NS_P}">'
        f'<p:nvSpPr><p:cNvPr id="{id_}" name="{name}"/>'
        f'<p:cNvSpPr txBox="1"/><p:nvPr/></p:nvSpPr>'
        f'<p:spPr>'
        f'<a:xfrm><a:off x="{LABEL_X}" y="{LABEL_Y}"/>'
        f'<a:ext cx="{LABEL_W}" cy="{LABEL_H}"/></a:xfrm>'
        f'<a:prstGeom prst="rect"><a:avLst/></a:prstGeom>'
        f'<a:noFill/>'
        f'</p:spPr>'
        f'<p:txBody>'
        f'<a:bodyPr wrap="square" lIns="0" tIns="0" rIns="0" bIns="0" anchor="ctr"/>'
        f'<a:lstStyle/>'
        f'<a:p><a:pPr algn="ctr"/>'
        f'<a:r><a:rPr sz="2400" b="1">'
        f'<a:solidFill><a:srgbClr val="{rgb_hex}"/></a:solidFill>'
        f'<a:latin typeface="Calibri"/>'
        f'</a:rPr><a:t>{text}</a:t></a:r>'
        f'</a:p>'
        f'</p:txBody>'
        f'</p:sp>'
    )


def build_par_set(par_id: int, set_id: int, preset_class: str, spid: int,
                  target_state: str, node_type: str) -> str:
    """One <p:par> wrapping a <p:set> that toggles visibility."""
    return (
        f'<p:par xmlns:a="{NS_A}" xmlns:p="{NS_P}">'
        f'<p:cTn id="{par_id}" presetID="1" presetClass="{preset_class}" '
        f'presetSubtype="0" fill="hold" grpId="0" nodeType="{node_type}">'
        f'<p:stCondLst><p:cond delay="0"/></p:stCondLst>'
        f'<p:childTnLst>'
        f'<p:set>'
        f'<p:cBhvr>'
        f'<p:cTn id="{set_id}" dur="1" fill="hold">'
        f'<p:stCondLst><p:cond delay="0"/></p:stCondLst>'
        f'</p:cTn>'
        f'<p:tgtEl><p:spTgt spid="{spid}"/></p:tgtEl>'
        f'<p:attrNameLst><p:attrName>style.visibility</p:attrName></p:attrNameLst>'
        f'</p:cBhvr>'
        f'<p:to><p:strVal val="{target_state}"/></p:to>'
        f'</p:set>'
        f'</p:childTnLst>'
        f'</p:cTn>'
        f'</p:par>'
    )


def find_sp_by_name(tree, name):
    for sp in tree.findall(".//p:sp", NSMAP):
        cnv = sp.find("p:nvSpPr/p:cNvPr", NSMAP)
        if cnv is not None and cnv.get("name") == name:
            return sp
    return None


def main():
    with zipfile.ZipFile(PPTX, "r") as zin:
        slide6_xml = zin.read("ppt/slides/slide6.xml")

    tree = etree.fromstring(slide6_xml)
    spTree = tree.find(".//p:cSld/p:spTree", NSMAP)

    # 1) Insert the two label shapes. Put them after the title, before the
    #    feedback_card, so source order reflects visual stacking (labels first
    #    are drawn under anything that overlaps — nothing does here).
    card = find_sp_by_name(tree, "feedback_card")
    ctrl_el  = etree.fromstring(build_label(
        ID_CONTROL, "control_label", "No Gender Feedback", "011F5B"))
    treat_el = etree.fromstring(build_label(
        ID_TREAT,   "treat_label",   "Gender Feedback",    "C5093B"))
    if card is not None:
        card.addprevious(ctrl_el)
        card.addprevious(treat_el)
    else:
        spTree.append(ctrl_el)
        spTree.append(treat_el)

    # 2) Extend the existing click group. The tree currently has ONE click
    #    group with two <p:par> children (cTn id=5 clickEffect for spid=16;
    #    cTn id=7 withEffect for spid=17). Append two more <p:par> entries
    #    into the same parent so the single click toggles 4 things.
    # Locate the clickEffect par's parent childTnLst
    click_eff = None
    for ctn in tree.findall(".//p:cTn", NSMAP):
        if ctn.get("nodeType") == "clickEffect":
            click_eff = ctn
            break
    if click_eff is None:
        raise SystemExit("could not find clickEffect cTn in slide 6 timing")
    # The parent we want is the <p:childTnLst> containing our clickEffect's <p:par>
    click_par = click_eff.getparent()       # <p:par>
    parent_childLst = click_par.getparent() # <p:childTnLst> holding the group

    hide_ctrl = etree.fromstring(build_par_set(
        CTN_CTL_PAR, CTN_CTL_SET, "exit", ID_CONTROL, "hidden",  "withEffect"))
    show_treat = etree.fromstring(build_par_set(
        CTN_TRT_PAR, CTN_TRT_SET, "entr", ID_TREAT,   "visible", "withEffect"))
    parent_childLst.append(hide_ctrl)
    parent_childLst.append(show_treat)

    # 3) Register treat_label in <p:bldLst> so PPT starts it hidden. The
    #    control_label gets NO bldP entry so it stays visible at load.
    bldLst = tree.find(".//p:timing/p:bldLst", NSMAP)
    if bldLst is None:
        raise SystemExit("no <p:bldLst> on slide 6 — expected one")
    bldP = etree.SubElement(bldLst, f"{{{NS_P}}}bldP")
    bldP.set("spid", str(ID_TREAT))
    bldP.set("grpId", "0")
    bldP.set("animBg", "1")

    # 4) Write back
    new_xml = etree.tostring(tree, xml_declaration=True, encoding="UTF-8",
                              standalone=True)
    with zipfile.ZipFile(PPTX, "r") as zin, \
         zipfile.ZipFile(TMP, "w", zipfile.ZIP_DEFLATED) as zout:
        for item in zin.infolist():
            data = zin.read(item.filename)
            if item.filename == "ppt/slides/slide6.xml":
                data = new_xml
            zout.writestr(item, data)
    os.replace(TMP, PPTX)
    print(f"OK: slide 6 labels added (control id={ID_CONTROL}, treat id={ID_TREAT})")


if __name__ == "__main__":
    main()
