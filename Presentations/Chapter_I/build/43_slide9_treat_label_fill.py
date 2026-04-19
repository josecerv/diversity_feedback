"""Ensure treat_label_race (slide 9) visually hides control_label_race when
the race-swap click fires.

The existing setup stacks the two labels at the same position and uses an
exit animation on control_label_race + entrance on treat_label_race. In
PowerPoint the exit animation should hide control, but to be robust against
any rendering quirk (and to make the visual correct even in LibreOffice
exports), we give treat_label_race a solid white fill matching the card
background. Once the treat label appears (z-order is after control in
spTree), its white rectangle covers anything underneath.
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


def main():
    with zipfile.ZipFile(PPTX, "r") as zin:
        slide9 = zin.read("ppt/slides/slide9.xml")

    tree = etree.fromstring(slide9)
    target = None
    for sp in tree.findall(".//p:sp", NSMAP):
        cnv = sp.find("p:nvSpPr/p:cNvPr", NSMAP)
        if cnv is not None and cnv.get("name") == "treat_label_race":
            target = sp
            break
    if target is None:
        raise SystemExit("treat_label_race not found on slide 9")

    spPr = target.find("p:spPr", NSMAP)
    # Remove existing fill child (noFill/solidFill/etc.)
    for fill_tag in ("a:noFill", "a:solidFill", "a:gradFill", "a:pattFill",
                      "a:blipFill"):
        for el in spPr.findall(fill_tag, NSMAP):
            spPr.remove(el)
    # Insert solid white fill after <a:prstGeom>
    prstGeom = spPr.find("a:prstGeom", NSMAP)
    fill = etree.fromstring(
        f'<a:solidFill xmlns:a="{NS_A}">'
        f'<a:srgbClr val="FFFFFF"/>'
        f'</a:solidFill>'
    )
    if prstGeom is not None:
        prstGeom.addnext(fill)
    else:
        spPr.append(fill)

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
    print("OK: treat_label_race now has solid white fill (covers control underneath)")


if __name__ == "__main__":
    main()
