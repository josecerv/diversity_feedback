"""Replace the embedded result-figure PNGs on slides 8, 10, 13 with the
re-generated versions (centered + enlarged cell-mean labels).

Only the media blobs change — slide XML stays intact so Jose's manual edits
(slide 8 caption id=200, slide 10 mediation diagram, slide 13 wide layout)
are preserved.

Mapping:
  ppt/media/image8.png   <-  Study-3B/Figure-Study3B-wide.png   (2.26 aspect)
  ppt/media/image34.png  <-  Study-4A/Figure-Study4A.png        (1.25 aspect)
  ppt/media/image37.png  <-  Study-5/Figure-Study5-wide.png     (2.26 aspect)
"""
import os
import zipfile

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.abspath(os.path.join(HERE, "..", "..", ".."))
PPTX = os.path.join(ROOT, "Presentations", "Chapter_I",
                    "Cervantez_Chapter_I_DiversityFeedback.pptx")
TMP = PPTX + ".tmp"

REPLACEMENTS = {
    "ppt/media/image8.png":  os.path.join(ROOT, "Study-3B", "Figure-Study3B-wide.png"),
    "ppt/media/image34.png": os.path.join(ROOT, "Study-4A", "Figure-Study4A.png"),
    "ppt/media/image37.png": os.path.join(ROOT, "Study-5",  "Figure-Study5-wide.png"),
}


def main():
    # Sanity: all source files exist
    for tgt, src in REPLACEMENTS.items():
        if not os.path.exists(src):
            raise SystemExit(f"missing source: {src}")

    with zipfile.ZipFile(PPTX, "r") as zin, \
         zipfile.ZipFile(TMP,  "w", zipfile.ZIP_DEFLATED) as zout:
        for item in zin.infolist():
            if item.filename in REPLACEMENTS:
                with open(REPLACEMENTS[item.filename], "rb") as f:
                    zout.writestr(item, f.read())
            else:
                zout.writestr(item, zin.read(item.filename))

    os.replace(TMP, PPTX)
    for tgt, src in REPLACEMENTS.items():
        print(f"replaced {tgt}  <-  {os.path.relpath(src, ROOT)}")


if __name__ == "__main__":
    main()
