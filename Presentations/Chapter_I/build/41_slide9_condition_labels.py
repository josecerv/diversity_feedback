"""Add a swapping condition label to the Study 2 feedback card on slide 9,
matching the slide-6 pattern but with race-feedback copy.

Initial (before the feedback card is revealed): nothing visible.
Click that reveals feedback_card: also reveals "No Race Feedback" (navy).
Click that swaps treat_occluder/treat_item (the race-feedback swap):
  hides "No Race Feedback" and reveals "Race Feedback" (burgundy).

Implementation: inject 2 text shapes near the top of the feedback card
(above the card_header) and append the right effect parallel blocks into
the two relevant click groups in <p:timing>.
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

# Existing slide 9 ranges:
#   shape IDs go up to 141 (treat_item)
#   cTn IDs go up to 198
CONTROL_ID = 142
TREAT_ID = 143

# Position: centered above card_header (which sits at y=1,325,880) but still
# inside the feedback_card (card top = y=922,246). Horizontal center matches
# card center (card x=203,630, w=5,303,520 -> center ≈ 2,855,390).
LABEL_W = 3000000
LABEL_H =  360000
LABEL_X = 2855390 - LABEL_W // 2      # 1,355,390
LABEL_Y =  970000                      # just below card top

# Race-feedback click group markers
CARD_SPIDS = {"135", "136", "137", "138", "139"}   # feedback_card + header + 3 ctl items
SWAP_SPIDS = {"140", "141"}                         # treat_occluder + treat_item

# New cTn ids — append-only so existing IDs stay stable.
CARD_ADD_PAR = 199
CARD_ADD_SET = 200
SWAP_HIDE_PAR = 201
SWAP_HIDE_SET = 202
SWAP_SHOW_PAR = 203
SWAP_SHOW_SET = 204


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
        f'<a:r><a:rPr sz="2000" b="1">'
        f'<a:solidFill><a:srgbClr val="{rgb_hex}"/></a:solidFill>'
        f'<a:latin typeface="Calibri"/>'
        f'</a:rPr><a:t>{text}</a:t></a:r>'
        f'</a:p>'
        f'</p:txBody>'
        f'</p:sp>'
    )


def build_par_set(par_ctn: int, set_ctn: int, preset_class: str,
                  spid: int, target: str, node_type: str) -> str:
    return (
        f'<p:par xmlns:a="{NS_A}" xmlns:p="{NS_P}">'
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


def find_sp_by_name(tree, name):
    for sp in tree.findall(".//p:sp", NSMAP):
        cnv = sp.find("p:nvSpPr/p:cNvPr", NSMAP)
        if cnv is not None and cnv.get("name") == name:
            return sp
    return None


def find_click_effects_host(tree, marker_spids: set[str]):
    """Return the <p:childTnLst> that holds the <p:par> effects for the click
    group whose effects reference any of marker_spids."""
    mainSeq = None
    for ctn in tree.findall(".//p:seq/p:cTn", NSMAP):
        if ctn.get("nodeType") == "mainSeq":
            mainSeq = ctn
            break
    if mainSeq is None:
        raise SystemExit("mainSeq not found")
    outer_childLst = mainSeq.find("p:childTnLst", NSMAP)
    for click_par in outer_childLst.findall("p:par", NSMAP):
        spids_here = {tgt.get("spid")
                       for tgt in click_par.findall(".//p:spTgt", NSMAP)}
        if not (marker_spids & spids_here):
            continue
        # Walk down to find the childTnLst that contains a <p:par> with
        # <p:cTn nodeType="clickEffect"/"withEffect">.
        for inner in click_par.findall(".//p:childTnLst", NSMAP):
            for cand in inner.findall("p:par", NSMAP):
                ctn = cand.find("p:cTn", NSMAP)
                if ctn is not None and ctn.get("nodeType") in ("clickEffect",
                                                                "withEffect"):
                    return inner
    return None


def main():
    with zipfile.ZipFile(PPTX, "r") as zin:
        slide9 = zin.read("ppt/slides/slide9.xml")
    tree = etree.fromstring(slide9)
    spTree = tree.find(".//p:cSld/p:spTree", NSMAP)

    # 1) Insert label shapes (before treat_occluder so they sit below the
    # swap layer in z-order — not strictly required but tidy).
    if find_sp_by_name(tree, "control_label_race") is None:
        anchor = find_sp_by_name(tree, "treat_occluder")
        ctrl_el = etree.fromstring(build_label(
            CONTROL_ID, "control_label_race", "No Race Feedback", "011F5B"))
        treat_el = etree.fromstring(build_label(
            TREAT_ID, "treat_label_race", "Race Feedback", "C5093B"))
        if anchor is not None:
            anchor.addprevious(ctrl_el)
            anchor.addprevious(treat_el)
        else:
            spTree.append(ctrl_el)
            spTree.append(treat_el)

    # 2) Add a withEffect to the card-reveal click group that also reveals
    # control_label_race
    card_host = find_click_effects_host(tree, CARD_SPIDS)
    if card_host is None:
        raise SystemExit("card-reveal click group not found on slide 9")
    card_host.append(etree.fromstring(build_par_set(
        CARD_ADD_PAR, CARD_ADD_SET, "entr",
        CONTROL_ID, "visible", "withEffect")))

    # 3) Add two withEffects to the race-swap click group: hide control,
    # reveal treat_label
    swap_host = find_click_effects_host(tree, SWAP_SPIDS)
    if swap_host is None:
        raise SystemExit("race-swap click group not found on slide 9")
    swap_host.append(etree.fromstring(build_par_set(
        SWAP_HIDE_PAR, SWAP_HIDE_SET, "exit",
        CONTROL_ID, "hidden", "withEffect")))
    swap_host.append(etree.fromstring(build_par_set(
        SWAP_SHOW_PAR, SWAP_SHOW_SET, "entr",
        TREAT_ID, "visible", "withEffect")))

    # 4) bldP entries so PPT starts both labels hidden until their animations fire
    bldLst = tree.find(".//p:timing/p:bldLst", NSMAP)
    if bldLst is None:
        raise SystemExit("no <p:bldLst>")
    for sid in (CONTROL_ID, TREAT_ID):
        if any(bp.get("spid") == str(sid) for bp in bldLst.findall("p:bldP", NSMAP)):
            continue
        bp = etree.SubElement(bldLst, f"{{{NS_P}}}bldP")
        bp.set("spid", str(sid))
        bp.set("grpId", "0")
        bp.set("animBg", "1")

    # 5) Write back
    new_xml = etree.tostring(tree, xml_declaration=True, encoding="UTF-8",
                              standalone=True)
    with zipfile.ZipFile(PPTX, "r") as zin, \
         zipfile.ZipFile(TMP, "w", zipfile.ZIP_DEFLATED) as zout:
        for item in zin.infolist():
            data = zin.read(item.filename)
            if item.filename == "ppt/slides/slide9.xml":
                data = new_xml
            zout.writestr(item, data)
    os.replace(TMP, PPTX)
    print(f"OK: slide 9 race labels added (control id={CONTROL_ID}, treat id={TREAT_ID})")


if __name__ == "__main__":
    main()
