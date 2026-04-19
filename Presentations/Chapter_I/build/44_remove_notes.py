"""Strip every notesSlide from the deck.

Removes:
  - all ppt/notesSlides/notesSlideN.xml and their .rels
  - notesSlide relationships in each ppt/slides/_rels/slideN.xml.rels
  - Override entries for notesSlides in [Content_Types].xml

The notesMaster (ppt/notesMasters/notesMaster1.xml) is kept — it's a
shared template and removing it would force edits to presentation.xml and
a bunch of rels. Without any notesSlide referencing it, the master simply
has nothing to render, which is fine.
"""
from __future__ import annotations
import os
import re
import zipfile

HERE = os.path.dirname(os.path.abspath(__file__))
PPTX = os.path.abspath(os.path.join(HERE, "..",
                                    "Cervantez_Chapter_I_DiversityFeedback.pptx"))
TMP = PPTX + ".tmp"


def main():
    with zipfile.ZipFile(PPTX, "r") as zin:
        names = zin.namelist()

        # Which files to drop entirely
        drop = {n for n in names if n.startswith("ppt/notesSlides/")}
        print(f"dropping {len(drop)} notesSlides files")

        # Update [Content_Types].xml
        ct = zin.read("[Content_Types].xml").decode("utf-8")
        ct_new = re.sub(
            r'<Override PartName="/ppt/notesSlides/[^"]+"[^>]*?/>', "", ct)
        # Also strip any notesSlide content types for completeness
        ct_new = re.sub(
            r'<Override PartName="/ppt/notesSlides/notesSlide\d+\.xml"[^>]*?/>',
            "", ct_new)

        # Build slide rels rewrites (strip notesSlide relationships)
        slide_rels_new: dict[str, str] = {}
        for n in names:
            if re.match(r"ppt/slides/_rels/slide\d+\.xml\.rels$", n):
                rels = zin.read(n).decode("utf-8")
                cleaned = re.sub(
                    r'<Relationship [^>]*?Type="[^"]*?/notesSlide"[^>]*?/>',
                    "",
                    rels,
                )
                if cleaned != rels:
                    slide_rels_new[n] = cleaned

    with zipfile.ZipFile(PPTX, "r") as zin, \
         zipfile.ZipFile(TMP, "w", zipfile.ZIP_DEFLATED) as zout:
        for item in zin.infolist():
            name = item.filename
            if name in drop:
                continue
            if name == "[Content_Types].xml":
                zout.writestr(item, ct_new.encode("utf-8"))
            elif name in slide_rels_new:
                zout.writestr(item, slide_rels_new[name].encode("utf-8"))
            else:
                zout.writestr(item, zin.read(name))

    os.replace(TMP, PPTX)
    print(f"OK: removed notesSlides, cleaned {len(slide_rels_new)} slide rels")


if __name__ == "__main__":
    main()
