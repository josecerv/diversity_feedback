"""Enlarge Slide 3 (Theory & Predictions) to WBL proportions.

- Box heights 0.95" -> 1.38"; box_root -> 1.87"
- Inner font 11-14pt -> 18pt (box_root italic subtitle stays 12pt)
- box_out_pos ("Increased selection...") moves from top-right to vertical middle
  so it becomes the focal point per Jose's request
- All 9 connectors redrawn to new box edges; shape IDs preserved so existing
  <p:timing> click sequence continues to work untouched.

Surgical edit — only slide3.xml is rewritten. Jose's manual animation tweaks
in the timing block are preserved verbatim.
"""
from __future__ import annotations
import os
import sys
import zipfile
from lxml import etree

HERE = os.path.dirname(os.path.abspath(__file__))
PPTX = os.path.abspath(os.path.join(HERE, "..", "Cervantez_Chapter_I_DiversityFeedback.pptx"))
TMP = PPTX + ".tmp"

NS_A = "http://schemas.openxmlformats.org/drawingml/2006/main"
NS_P = "http://schemas.openxmlformats.org/presentationml/2006/main"
NS_R = "http://schemas.openxmlformats.org/officeDocument/2006/relationships"
NSMAP = {"a": NS_A, "p": NS_P, "r": NS_R}
qa = lambda t: f"{{{NS_A}}}{t}"
qp = lambda t: f"{{{NS_P}}}{t}"

# New box geometry (EMU). Mirrors WBL slide 9 layout — box_out_pos at y=2_997_791
# is the vertical middle of the content area.
BOXES = {
    "box_root":       dict(x= 335902, y=2526065, w=1625082, h=1707503),
    "box_tg_yes":     dict(x=2397965, y=2225351, w=1931439, h=1259633),
    "box_tg_no":      dict(x=2421291, y=4714777, w=1931439, h=1259633),
    "box_fb_low":     dict(x=4882244, y=1295104, w=2062846, h=1259633),
    "box_fb_notlow":  dict(x=4890022, y=3017570, w=2062846, h=1259633),
    "box_no_image":   dict(x=4890022, y=4714777, w=2062846, h=1259633),
    "box_motivation": dict(x=7484710, y=1315463, w=2055068, h=1259633),
    "box_out_pos":    dict(x=8733452, y=2997791, w=2995128, h=1259633),
    "box_out_null":   dict(x=7484710, y=4721294, w=2995128, h=1259633),
}

# Per-box font sizes. WBL target is 18pt, but long-worded Chapter 1 boxes
# overflow at 18pt; 16pt holds for short-text boxes, 14pt for long ones.
# Still a meaningful bump from the original 11-14pt.
FONT_BY_BOX = {
    "box_root":       (1400, 1000),  # (title, italic) — long text, keep original
    "box_tg_yes":     1600,          # "TARGET GROUP = ..."
    "box_tg_no":      1600,          # "TARGET GROUP ≠ ..."
    "box_fb_low":     1400,          # "Feedback conveys low rates..." — long
    "box_fb_notlow":  1400,          # "Feedback does NOT convey..." — long
    "box_no_image":   1600,          # "No image concerns triggered"
    "box_motivation": 1400,          # "Motivation to respond..." — medium-long
    "box_out_pos":    1600,          # "Increased selection..."
    "box_out_null":   1600,          # "No change in selection..."
}

# Arrow topology. (name, id, src_box, src_side, dst_box, dst_side, color, line_w EMU)
ARROWS = [
    ("a_root_tgYes",           62, "box_root",       "right",  "box_tg_yes",     "left", "011F5B", 19050),
    ("a_root_tgNo",            63, "box_root",       "right",  "box_tg_no",      "left", "57606C", 19050),
    ("a_tgYes_fbLow",          64, "box_tg_yes",     "right",  "box_fb_low",     "left", "011F5B", 19050),
    ("a_tgYes_fbNotLow",       65, "box_tg_yes",     "right",  "box_fb_notlow",  "left", "57606C", 19050),
    ("a_tgNo_noImage",         66, "box_tg_no",      "right",  "box_no_image",   "left", "57606C", 19050),
    ("a_fbLow_motivation",     67, "box_fb_low",     "right",  "box_motivation", "left", "011F5B", 25400),
    ("a_motivation_increased", 68, "box_motivation", "bottom", "box_out_pos",    "left", "C5093B", 25400),
    ("a_fbNotLow_noChange",    69, "box_fb_notlow",  "right",  "box_out_null",   "left", "57606C", 19050),
    ("a_noImage_noChange",     70, "box_no_image",   "right",  "box_out_null",   "left", "57606C", 19050),
]


def find_sp_by_name(tree, name):
    for sp in tree.findall(".//p:sp", NSMAP):
        cnv = sp.find("p:nvSpPr/p:cNvPr", NSMAP)
        if cnv is not None and cnv.get("name") == name:
            return sp
    return None


def set_xfrm(sp, x, y, w, h):
    spPr = sp.find("p:spPr", NSMAP)
    xfrm = spPr.find("a:xfrm", NSMAP)
    if xfrm is None:
        xfrm = etree.SubElement(spPr, qa("xfrm"))
        spPr.insert(0, xfrm)
    off = xfrm.find("a:off", NSMAP)
    if off is None:
        off = etree.SubElement(xfrm, qa("off"))
    off.set("x", str(x)); off.set("y", str(y))
    ext = xfrm.find("a:ext", NSMAP)
    if ext is None:
        ext = etree.SubElement(xfrm, qa("ext"))
    ext.set("cx", str(w)); ext.set("cy", str(h))


def set_run_size(sp, idx, size):
    rPrs = sp.findall(".//a:rPr", NSMAP)
    if idx < len(rPrs):
        rPrs[idx].set("sz", str(size))


def set_all_run_sizes(sp, size):
    for rPr in sp.findall(".//a:rPr", NSMAP):
        rPr.set("sz", str(size))


def side_point(b, side):
    if side == "right":  return (b["x"] + b["w"],       b["y"] + b["h"] // 2)
    if side == "left":   return (b["x"],                b["y"] + b["h"] // 2)
    if side == "top":    return (b["x"] + b["w"] // 2,  b["y"])
    if side == "bottom": return (b["x"] + b["w"] // 2,  b["y"] + b["h"])
    raise ValueError(side)


def build_connector(name, id_, x1, y1, x2, y2, color, line_w):
    off_x = min(x1, x2); off_y = min(y1, y2)
    cx = max(abs(x2 - x1), 1)  # avoid cx=0
    cy = max(abs(y2 - y1), 1)
    # flipV needed when the line goes bottom-left -> top-right
    # (i.e., start is lower than end while start is to the left)
    flip_v = (y2 < y1 and x2 > x1) or (y2 > y1 and x2 < x1)
    flip_h = x2 < x1
    attrs = ""
    if flip_h: attrs += ' flipH="1"'
    if flip_v: attrs += ' flipV="1"'
    return (
        f'<p:cxnSp xmlns:a="{NS_A}" xmlns:p="{NS_P}">'
        f'<p:nvCxnSpPr>'
        f'<p:cNvPr id="{id_}" name="{name}"/>'
        f'<p:cNvCxnSpPr/>'
        f'<p:nvPr/>'
        f'</p:nvCxnSpPr>'
        f'<p:spPr>'
        f'<a:xfrm{attrs}>'
        f'<a:off x="{off_x}" y="{off_y}"/>'
        f'<a:ext cx="{cx}" cy="{cy}"/>'
        f'</a:xfrm>'
        f'<a:prstGeom prst="line"><a:avLst/></a:prstGeom>'
        f'<a:ln w="{line_w}">'
        f'<a:solidFill><a:srgbClr val="{color}"/></a:solidFill>'
        f'<a:tailEnd type="triangle" w="med" len="med"/>'
        f'</a:ln>'
        f'</p:spPr>'
        f'<p:style>'
        f'<a:lnRef idx="2"><a:schemeClr val="accent1"/></a:lnRef>'
        f'<a:fillRef idx="0"><a:schemeClr val="accent1"/></a:fillRef>'
        f'<a:effectRef idx="1"><a:schemeClr val="accent1"/></a:effectRef>'
        f'<a:fontRef idx="minor"><a:schemeClr val="tx1"/></a:fontRef>'
        f'</p:style>'
        f'</p:cxnSp>'
    )


def main():
    with zipfile.ZipFile(PPTX, "r") as zin:
        slide3_xml = zin.read("ppt/slides/slide3.xml")

    tree = etree.fromstring(slide3_xml)
    spTree = tree.find(".//p:cSld/p:spTree", NSMAP)

    # 1) Resize boxes + bump fonts
    for name, geom in BOXES.items():
        sp = find_sp_by_name(tree, name)
        if sp is None:
            print(f"WARN: shape {name} not found", file=sys.stderr)
            continue
        set_xfrm(sp, geom["x"], geom["y"], geom["w"], geom["h"])
        spec = FONT_BY_BOX[name]
        if isinstance(spec, tuple):
            title_sz, italic_sz = spec
            set_run_size(sp, 0, title_sz)
            set_run_size(sp, 1, italic_sz)
        else:
            set_all_run_sizes(sp, spec)

    # 2) Delete existing a_* connectors
    to_remove = []
    for cx in spTree.findall("p:cxnSp", NSMAP):
        cnv = cx.find("p:nvCxnSpPr/p:cNvPr", NSMAP)
        if cnv is not None and (cnv.get("name") or "").startswith("a_"):
            to_remove.append(cx)
    for cx in to_remove:
        spTree.remove(cx)

    # 3) Rebuild connectors with same IDs (timing block references them)
    # Insert them before box_citations to keep z-order reasonable (citations on top).
    citations = None
    for sp in spTree.findall("p:sp", NSMAP):
        cnv = sp.find("p:nvSpPr/p:cNvPr", NSMAP)
        if cnv is not None and cnv.get("name") == "box_citations":
            citations = sp
            break

    for (name, id_, src, src_side, dst, dst_side, color, lw) in ARROWS:
        sb = BOXES[src]; db = BOXES[dst]
        x1, y1 = side_point(sb, src_side)
        x2, y2 = side_point(db, dst_side)
        el = etree.fromstring(build_connector(name, id_, x1, y1, x2, y2, color, lw))
        if citations is not None:
            citations.addprevious(el)
        else:
            spTree.append(el)

    # 4) Write back
    new_xml = etree.tostring(tree, xml_declaration=True, encoding="UTF-8", standalone=True)

    with zipfile.ZipFile(PPTX, "r") as zin, \
         zipfile.ZipFile(TMP, "w", zipfile.ZIP_DEFLATED) as zout:
        for item in zin.infolist():
            data = zin.read(item.filename)
            if item.filename == "ppt/slides/slide3.xml":
                data = new_xml
            zout.writestr(item, data)
    os.replace(TMP, PPTX)
    print(f"OK: slide 3 enlarged; {len(BOXES)} boxes resized, {len(ARROWS)} connectors redrawn")


if __name__ == "__main__":
    main()
