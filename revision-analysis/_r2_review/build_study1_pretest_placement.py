"""Build a tracked-changes .docx that applies the Study 1 pretest placement edit.

Source of truth: Study1_pretest_placement_edit.md (handoff note at repo root).

Two operations:
  (1) Remove the existing pending pretest paragraph at the top of Study 1 -> Methods.
      That paragraph is currently a Suggesting-mode insert in the Gdoc (author
      "Jose Cervantez", w:ins id=12 in the exported docx). Removing the entire
      <w:p> is equivalent to rejecting that suggestion.

  (2) Insert a fresh tracked-insert paragraph after the Study 1 -> Procedure
      paragraph that ends "...for complete study materials." The new text matches
      the handoff note: italic N, curly quotes, hyperlinked AsPredicted URL.

Inputs:
  - revision-analysis/_r2_review/manuscript_gdoc_export.docx
    (produced by fetch_manuscript_gdoc.py)

Output:
  - revision-analysis/_r2_review/Manuscript_Study1PretestPlacement_TRACKED.docx
"""
from __future__ import annotations

import datetime as _dt
import shutil
import sys
from pathlib import Path

from lxml import etree
import zipfile
import io

ROOT = Path(r"C:/Users/jcerv/Jose/diversity_feedback/revision-analysis/_r2_review")
SRC_DOCX = ROOT / "manuscript_gdoc_export.docx"
OUT_DOCX = ROOT / "Manuscript_Study1PretestPlacement_TRACKED.docx"

AUTHOR = "Claude (Study 1 pretest placement)"
DATE = "2026-05-15T00:00:00Z"

W_NS = "http://schemas.openxmlformats.org/wordprocessingml/2006/main"
W = "{%s}" % W_NS
NSMAP = {"w": W_NS}


def _q(name: str) -> str:
    return W + name


# Unique anchor text that identifies the Study 1 Procedure paragraph that ends
# with "...for complete study materials." We need to disambiguate from the
# identically-suffixed Study 3A and 3B paragraphs.
PROCEDURE_ANCHOR_SIGNATURE = (
    "After receiving descriptive feedback about the composition of their past "
    "expert selections"
)

EXISTING_PRETEST_SIGNATURE = (
    "we conducted a pre-registered pretest"
)

# Paste-ready replacement text from the handoff note.
# Split into segments so we can italicize N and embed a hyperlink for the AsPredicted URL.
NEW_PARAGRAPH_SEGMENTS = [
    ("text", "Prior to data collection, we ran a pre-registered pretest ("),
    ("hyperlink", "https://aspredicted.org/xa7u94.pdf"),
    ("text", "; "),
    ("italic", "N"),
    ("text",
     " = 300) to verify that the three comparison attributes were rated as at "
     "least as important as gender for selecting NPR experts. Each participant "
     "was randomly assigned to rate one of the four attributes on a scale from "
     "1 (“Not at all important”) to 7 (“Very important”). "
     "All three comparison attributes were rated as equally or more important "
     "than gender (full results in Appendix Section S[X]), so differences in "
     "perceived importance of the feedback target cannot account for the "
     "Study 1 effects reported below."),
]


# ---- XML builders ----------------------------------------------------------

_id_counter = [9000]


def next_id() -> int:
    _id_counter[0] += 1
    return _id_counter[0]


def make_ins(*, ins_id: int) -> etree._Element:
    ins = etree.Element(_q("ins"))
    ins.set(_q("id"), str(ins_id))
    ins.set(_q("author"), AUTHOR)
    ins.set(_q("date"), DATE)
    return ins


def make_text_run(text: str, *, italic: bool = False) -> etree._Element:
    r = etree.Element(_q("r"))
    rPr = etree.SubElement(r, _q("rPr"))
    if italic:
        etree.SubElement(rPr, _q("i"))
    etree.SubElement(rPr, _q("rtl")).set(_q("val"), "0")
    t = etree.SubElement(r, _q("t"))
    t.set("{http://www.w3.org/XML/1998/namespace}space", "preserve")
    t.text = text
    return r


def make_hyperlink_field_runs(url: str, display_text: str) -> list[etree._Element]:
    """Build the chain of runs that encode a Word field-style hyperlink.

    Mirrors the encoding used by Drive's docx export so the resulting runs
    behave like a real hyperlink (blue, underlined, clickable) once accepted.
    """
    out: list[etree._Element] = []

    # 1) field begin
    r = etree.Element(_q("r"))
    fld = etree.SubElement(r, _q("fldChar"))
    fld.set(_q("fldCharType"), "begin")
    out.append(r)

    # 2) instruction text
    r = etree.Element(_q("r"))
    instr = etree.SubElement(r, _q("instrText"))
    instr.set("{http://www.w3.org/XML/1998/namespace}space", "preserve")
    instr.text = f'HYPERLINK "{url}"'
    out.append(r)

    # 3) field separate
    r = etree.Element(_q("r"))
    fld = etree.SubElement(r, _q("fldChar"))
    fld.set(_q("fldCharType"), "separate"
    )
    out.append(r)

    # 4) the visible run (blue + underline)
    r = etree.Element(_q("r"))
    rPr = etree.SubElement(r, _q("rPr"))
    color = etree.SubElement(rPr, _q("color"))
    color.set(_q("val"), "1155cc")
    u = etree.SubElement(rPr, _q("u"))
    u.set(_q("val"), "single")
    etree.SubElement(rPr, _q("rtl")).set(_q("val"), "0")
    t = etree.SubElement(r, _q("t"))
    t.set("{http://www.w3.org/XML/1998/namespace}space", "preserve")
    t.text = display_text
    out.append(r)

    # 5) field end
    r = etree.Element(_q("r"))
    fld = etree.SubElement(r, _q("fldChar"))
    fld.set(_q("fldCharType"), "end")
    out.append(r)

    return out


def build_new_paragraph(pPr_template: etree._Element) -> etree._Element:
    """Construct the <w:p> for the new tracked-inserted paragraph.

    Clones the supplied pPr (paragraph properties) so spacing/indent match
    surrounding body paragraphs. The cloned pPr is stripped of any inherited
    Suggesting-mode revision markers so the new paragraph is properly attributed
    to the new author/date.
    """
    p = etree.Element(_q("p"))

    # Clone pPr but strip any inherited revision markers.
    pPr_clone = etree.fromstring(etree.tostring(pPr_template))
    for tag in ("ins", "del", "rPrChange", "pPrChange"):
        for el in pPr_clone.findall(f".//{_q(tag)}"):
            el.getparent().remove(el)

    # Mark the paragraph mark itself as inserted (so the new paragraph break
    # is part of the tracked change).
    rPr = pPr_clone.find(_q("rPr"))
    if rPr is None:
        rPr = etree.SubElement(pPr_clone, _q("rPr"))
    ins_marker = etree.SubElement(rPr, _q("ins"))
    ins_marker.set(_q("id"), str(next_id()))
    ins_marker.set(_q("author"), AUTHOR)
    ins_marker.set(_q("date"), DATE)
    p.append(pPr_clone)

    ins = make_ins(ins_id=next_id())
    for kind, payload in NEW_PARAGRAPH_SEGMENTS:
        if kind == "text":
            ins.append(make_text_run(payload))
        elif kind == "italic":
            ins.append(make_text_run(payload, italic=True))
        elif kind == "hyperlink":
            for run in make_hyperlink_field_runs(payload, payload):
                ins.append(run)
        else:
            raise ValueError(f"unknown segment kind: {kind}")
    p.append(ins)
    return p


# ---- Finders ---------------------------------------------------------------


def paragraph_text(p: etree._Element) -> str:
    """Concatenate the visible text of a <w:p>, ignoring field-code instructions."""
    parts = []
    for el in p.iter():
        tag = etree.QName(el).localname
        if tag == "t" or tag == "delText":
            if el.text:
                parts.append(el.text)
        elif tag == "instrText":
            # Field instruction codes are not visible.
            continue
    return "".join(parts)


def find_paragraph(body: etree._Element, signature: str) -> etree._Element:
    for p in body.iter(_q("p")):
        if signature in paragraph_text(p):
            return p
    raise RuntimeError(f"No paragraph matched signature: {signature!r}")


def find_procedure_anchor(body: etree._Element) -> etree._Element:
    """Find the Study 1 Procedure paragraph that ends with 'for complete study materials.'

    We disambiguate from Study 3A/3B by requiring the paragraph also contain the
    NPR-specific signature about expert selections and the AI / future-of-work
    segment.
    """
    candidates = []
    for p in body.iter(_q("p")):
        txt = paragraph_text(p)
        if PROCEDURE_ANCHOR_SIGNATURE in txt and "for complete study materials" in txt:
            candidates.append(p)
    if len(candidates) != 1:
        raise RuntimeError(
            f"Expected exactly 1 Study 1 procedure anchor, found {len(candidates)}"
        )
    return candidates[0]


def find_existing_pretest_paragraph(body: etree._Element) -> etree._Element:
    candidates = []
    for p in body.iter(_q("p")):
        txt = paragraph_text(p)
        if EXISTING_PRETEST_SIGNATURE in txt and "xa7u94" in txt:
            # Only target the Study 1 version (not Study 3B, which references a
            # different pretest with N = 550).
            if "300" in txt and "550" not in txt:
                candidates.append(p)
    if len(candidates) != 1:
        raise RuntimeError(
            f"Expected exactly 1 existing pretest paragraph, found {len(candidates)}"
        )
    return candidates[0]


# ---- Main ------------------------------------------------------------------


def main() -> int:
    if not SRC_DOCX.exists():
        print(f"ERROR: source docx missing: {SRC_DOCX}", file=sys.stderr)
        return 1

    print(f"Reading source: {SRC_DOCX}")
    with zipfile.ZipFile(SRC_DOCX, "r") as zin:
        names = zin.namelist()
        xml_bytes = zin.read("word/document.xml")
        members = {n: zin.read(n) for n in names}

    tree = etree.fromstring(xml_bytes)
    # body is the first <w:body> inside <w:document>
    body = tree.find(_q("body"))
    if body is None:
        print("ERROR: could not find <w:body>", file=sys.stderr)
        return 1

    # 1) Locate and remove the existing pretest paragraph.
    pretest_p = find_existing_pretest_paragraph(body)
    pretest_text = paragraph_text(pretest_p)
    print(f"Found existing pretest paragraph ({len(pretest_text)} chars). Preview:")
    print("  " + pretest_text[:160] + ("..." if len(pretest_text) > 160 else ""))
    pretest_parent = pretest_p.getparent()
    pretest_idx = list(pretest_parent).index(pretest_p)
    pretest_parent.remove(pretest_p)
    print(f"Removed existing pretest <w:p> (was at child idx {pretest_idx}).")

    # 2) Locate the procedure anchor and insert the new paragraph after it.
    anchor_p = find_procedure_anchor(body)
    anchor_text = paragraph_text(anchor_p)
    print(f"Found procedure anchor ({len(anchor_text)} chars). Last 120 chars:")
    print("  ..." + anchor_text[-120:])

    anchor_pPr = anchor_p.find(_q("pPr"))
    if anchor_pPr is None:
        # Fall back: borrow pPr from the previous paragraph if any.
        print("WARNING: anchor has no <w:pPr>; using empty template.", file=sys.stderr)
        anchor_pPr = etree.Element(_q("pPr"))

    new_p = build_new_paragraph(anchor_pPr)
    anchor_parent = anchor_p.getparent()
    anchor_idx = list(anchor_parent).index(anchor_p)
    anchor_parent.insert(anchor_idx + 1, new_p)
    new_text = paragraph_text(new_p)
    print(f"Inserted new tracked paragraph after procedure anchor ({len(new_text)} chars).")
    print("  Preview: " + new_text[:160] + ("..." if len(new_text) > 160 else ""))

    # Serialize.
    new_xml = etree.tostring(
        tree,
        xml_declaration=True,
        encoding="UTF-8",
        standalone=True,
    )

    # Repack the zip.
    if OUT_DOCX.exists():
        OUT_DOCX.unlink()
    with zipfile.ZipFile(OUT_DOCX, "w", compression=zipfile.ZIP_DEFLATED) as zout:
        for n in names:
            if n == "word/document.xml":
                zout.writestr(n, new_xml)
            else:
                zout.writestr(n, members[n])

    print(f"Wrote {OUT_DOCX} ({OUT_DOCX.stat().st_size} bytes)")

    # Quick validation: reopen and count w:ins / w:del referencing our author.
    with zipfile.ZipFile(OUT_DOCX, "r") as zin:
        out_xml = zin.read("word/document.xml").decode("utf-8")
    our_ins_count = out_xml.count(f'w:author="{AUTHOR}"')
    print(f"Validation: {our_ins_count} references to author {AUTHOR!r}")
    assert "Prior to data collection, we ran a pre-registered pretest" in out_xml, (
        "New paragraph text missing from output"
    )
    assert "Prior to this study, we conducted a pre-registered pretest" not in out_xml, (
        "Existing pretest text still present in output"
    )
    print("Validation OK.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
