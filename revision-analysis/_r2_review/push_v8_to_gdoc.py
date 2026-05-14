"""Push v8 bullets to the R2 response-letter Gdoc.

Constraints:
- PRESERVE the "Here is a summary of the most notable changes we have made
  in this revision:" line (it has a comment anchored to it).
- PRESERVE the cover-letter header above (date, recipient, Re:, Dear Dr. Wu,
  the "We thank you..." intro paragraph, and the blank line below the
  summary header).
- PRESERVE the closing ("Below we reproduce..." and "Thank you again..."),
  including the blank line above "Below we reproduce...".
- Only replace the existing 6-bullet numbered list with the 4 new v8
  bullets. Bullet 1 has internal sub-points joined by soft line breaks
  (vertical tabs) so it renders as ONE numbered item.

Approach:
1. Fetch doc as JSON.
2. Walk paragraphs to locate:
   - The first bullet ("We have modernized..." -- the current bullet 1)
   - The last bullet ("We have streamlined..." -- the current bullet 6,
     possibly with the "First, ..." / "Second, ..." sub-paragraphs between).
3. Compute the [startIndex, endIndex) span covering all current list
   paragraphs.
4. batchUpdate:
   - deleteContentRange(span)
   - insertText(new four-paragraph block) at the span start
   - createParagraphBullets(new range) with NUMBERED_DECIMAL_ALPHA_ROMAN
"""
from __future__ import annotations

import json
import pathlib

from googleapiclient.discovery import build
from google.oauth2.credentials import Credentials
from google.auth.transport.requests import Request


DOC_ID = "1YBHCMwD84xC3Qw38CPEzz1iv3JKg7Mw9jTJqyZGptkI"
TOKEN = pathlib.Path(r"C:/Users/jcerv/.config/gws/token.json")


def get_creds():
    data = json.load(open(TOKEN))
    creds = Credentials.from_authorized_user_info(
        data,
        [
            "https://www.googleapis.com/auth/documents",
            "https://www.googleapis.com/auth/drive",
        ],
    )
    if not creds.valid and creds.refresh_token:
        creds.refresh(Request())
    return creds


def paragraphs_with_text(content):
    for el in content:
        if "paragraph" not in el:
            continue
        para = el["paragraph"]
        text_parts = []
        for r in para.get("elements", []):
            if "textRun" in r:
                text_parts.append(r["textRun"].get("content", ""))
        text = "".join(text_parts).rstrip("\n")
        yield el["startIndex"], el["endIndex"], text


# ---------------------------------------------------------------------------
# v8 bullets. Within bullet 1, sub-points are separated by soft line breaks
# (vertical tab, "\v") so all of bullet 1 lives in ONE paragraph and gets
# ONE list number. Between bullets we use "\n" (paragraph break), which
# starts the next numbered item.
# ---------------------------------------------------------------------------

BULLET_1 = (
    "We have addressed your two empirical requests."
    "\v"
    "\v"
    "First, the new Study 1 importance post‑test (N = 300) confirms that "
    "the comparison attributes were not rated less important than gender, "
    "so importance differences cannot explain why only gender feedback moved "
    "selections."
    "\v"
    "\v"
    "Second, the new pooled analysis (Table R1), restricted to participants "
    "whose initial selections contained zero of the focal attribute, shows "
    "that race and gender feedback significantly raises subsequent selection "
    "of women and racial minorities, whereas comparison‑attribute feedback "
    "does not. We take these results as support for our claim that race and "
    "gender feedback appears to move selections because those dimensions "
    "carry an implicit injunctive norm against prejudiced selection, rather "
    "than simply because they start out under‑selected."
)

BULLET_2 = (
    "We have streamlined the empirical sections of the paper following your "
    "guidance on length and repetition. The revised manuscript is now [X] "
    "pages and reflects a substantial streamlining of the Methods and "
    "Results sections, particularly for later studies to avoid unnecessary "
    "repetition and redundancy."
)

BULLET_3 = (
    "In response to Reviewer 2, we have updated the Introduction’s discussion "
    "of the current landscape around DEI. We acknowledge that U.S. support "
    "for new DEI initiatives has retreated since 2024, while noting three "
    "parallel patterns that keep our central question relevant: "
    "international transparency requirements continue to expand, U.S. public "
    "and stakeholder support for corporate DEI remains broadly intact, and "
    "many firms have rebranded their diversity work rather than dismantling "
    "it. These patterns help clarify why the motivation to respond without "
    "prejudice that drives our effect can operate through external and "
    "internal pathways: external pressure from law, selection visibility, "
    "and public expectations, and evaluators’ internal desire to act "
    "consistently with egalitarian values. We therefore expect the effect "
    "to be strongest where selections are visible and fairness concerns "
    "remain salient, and weakest in fully private settings or organizations "
    "that openly reject egalitarian norms. The General Discussion now "
    "identifies organizational DEI climate as a limiting condition for "
    "future research."
)

BULLET_4 = (
    "In response to Reviewer 1, we have modernized the references that "
    "anchor our theoretical framing in both the Introduction and General "
    "Discussion, drawing on more contemporary feedback, goal‑setting, and "
    "social‑norms work alongside the foundational citations we retain."
)

# Final block: 4 paragraphs (3 paragraph breaks between them). No trailing
# newline -- the existing paragraph break after the last current bullet stays
# in place because we keep the blank paragraph below.
NEW_LIST_BLOCK = "\n".join([BULLET_1, BULLET_2, BULLET_3, BULLET_4])


# Anchors used to find the current list range.
FIRST_BULLET_PREFIX = "We have modernized the references"
LAST_BULLET_PREFIX = "We have streamlined the empirical sections"


def locate_list_range(content):
    """Return (startIndex, endIndex) covering the existing 6-bullet list."""
    start = None
    end = None
    for si, ei, text in paragraphs_with_text(content):
        if start is None and text.startswith(FIRST_BULLET_PREFIX):
            start = si
        if text.startswith(LAST_BULLET_PREFIX):
            end = ei
            break
    if start is None or end is None:
        raise RuntimeError(
            f"Could not locate list range: start={start} end={end}"
        )
    return start, end


def main():
    creds = get_creds()
    docs = build("docs", "v1", credentials=creds)

    # ----- Pass 1: delete old list and insert new list -------------------
    doc = docs.documents().get(documentId=DOC_ID).execute()
    list_start, list_end = locate_list_range(doc["body"]["content"])
    print(f"[indices] existing list range = [{list_start}, {list_end})")

    # endIndex of a paragraph points to AFTER the trailing newline. We want
    # to keep the trailing newline of the LAST current bullet so the paragraph
    # break before the next blank line is preserved. So delete [start, end-1)
    # and insert the new block at start. The new block ends with the body of
    # bullet 4 (no trailing newline); the existing terminator becomes bullet
    # 4's terminator.
    delete_end = list_end - 1
    print(f"[delete] [{list_start}, {delete_end})")
    print(f"[insert] {len(NEW_LIST_BLOCK)} chars at index {list_start}")

    pass1_requests = [
        {
            "deleteContentRange": {
                "range": {"startIndex": list_start, "endIndex": delete_end}
            }
        },
        {
            "insertText": {
                "location": {"index": list_start},
                "text": NEW_LIST_BLOCK,
            }
        },
    ]
    docs.documents().batchUpdate(
        documentId=DOC_ID, body={"requests": pass1_requests}
    ).execute()

    # ----- Pass 2: re-fetch, apply createParagraphBullets ----------------
    doc = docs.documents().get(documentId=DOC_ID).execute()
    # Find the new range: from "We have addressed..." (bullet 1) through the
    # end of "In response to Reviewer 1..." (bullet 4).
    new_first_anchor = "We have addressed your two empirical requests"
    new_last_anchor = "In response to Reviewer 1"
    new_start = None
    new_end = None
    for si, ei, text in paragraphs_with_text(doc["body"]["content"]):
        if new_start is None and text.startswith(new_first_anchor):
            new_start = si
        if text.startswith(new_last_anchor):
            new_end = ei
            break
    if new_start is None or new_end is None:
        raise RuntimeError(
            f"Could not locate new list range: start={new_start} end={new_end}"
        )
    print(f"[indices] new list range = [{new_start}, {new_end})")

    pass2_requests = [
        {
            "createParagraphBullets": {
                "range": {"startIndex": new_start, "endIndex": new_end},
                "bulletPreset": "NUMBERED_DECIMAL_ALPHA_ROMAN",
            }
        }
    ]
    docs.documents().batchUpdate(
        documentId=DOC_ID, body={"requests": pass2_requests}
    ).execute()
    print("done.")


if __name__ == "__main__":
    main()
