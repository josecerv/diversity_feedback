"""Fix the cover-letter formatting in the R2 response-letter Gdoc.

Two issues:
  1. The 6 "most notable changes" paragraphs should be a numbered list
     (1., 2., 3., ...) rather than free paragraphs.
  2. Spacing between cover-letter sections is too tight. We insert blank
     paragraphs at the section boundaries:
       - between the "We thank you..." paragraph and "Here is a summary..."
       - between "Here is a summary..." and the first numbered item
       - between the last numbered item and "Below we reproduce..."
       - between "Below we reproduce..." and "Thank you again..."
     The header block (date, recipient, Re:, salutation) stays compact, as
     in the R1 cover letter.

Two batchUpdate calls:
  - Pass 1: insert blank paragraphs (descending index order so each insert
    does not shift the indices of the earlier inserts).
  - Pass 2 (re-fetch first): apply createParagraphBullets to the now-
    repositioned numbered-list range.
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
    """Yield (startIndex, endIndex, text) for each paragraph in the body."""
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


# Anchors for the cover-letter paragraphs we care about. Each entry is
# (label, prefix-of-paragraph-text). We use prefix matching because the
# paragraphs are long.
ANCHORS = [
    ("intro", "We thank you for handling our revised manuscript"),
    ("summary_header", "Here is a summary of the most notable changes"),
    ("bullet_1", "We have modernized the references"),
    ("bullet_2", "We have clarified our theoretical account"),
    ("bullet_3", "We have situated the paper in the current sociopolitical"),
    ("bullet_4", "We have tightened the scope of our theoretical claims"),
    ("bullet_5", "We have addressed your two empirical requests"),
    ("bullet_6", "We have streamlined the empirical sections"),
    ("closing", "Below we reproduce your letter"),
    ("thank_you_again", "Thank you again for giving us the opportunity"),
]


def locate_anchors(content):
    """Return {label: (startIndex, endIndex)} for every anchor we find."""
    spans = {}
    for si, ei, text in paragraphs_with_text(content):
        for label, prefix in ANCHORS:
            if label in spans:
                continue
            if text.startswith(prefix):
                spans[label] = (si, ei)
    missing = [label for label, _ in ANCHORS if label not in spans]
    if missing:
        raise RuntimeError(f"Missing anchors: {missing}")
    return spans


def pass1_insert_blanks():
    """Insert blank paragraphs at section boundaries. Returns the new doc
    state after the batch is applied (for indices in pass 2)."""
    creds = get_creds()
    docs = build("docs", "v1", credentials=creds)
    doc = docs.documents().get(documentId=DOC_ID).execute()
    spans = locate_anchors(doc["body"]["content"])

    # endIndex of a paragraph points to the position AFTER the trailing \n.
    # Inserting "\n" at that endIndex creates one blank paragraph after the
    # current paragraph.
    insert_positions = [
        spans["intro"][1],            # between intro and "Here is a summary..."
        spans["summary_header"][1],   # between summary header and bullet 1
        spans["bullet_6"][1],         # between bullet 6 and "Below we reproduce..."
        spans["closing"][1],          # between closing and "Thank you again..."
    ]

    # Insert in descending order so each insert does not shift earlier indices.
    requests = []
    for pos in sorted(insert_positions, reverse=True):
        requests.append(
            {
                "insertText": {
                    "location": {"index": pos - 1},  # insert just before the trailing \n
                    "text": "\n",
                }
            }
        )

    print(f"[pass 1] inserting {len(requests)} blank paragraphs at positions {sorted(insert_positions, reverse=True)}")
    docs.documents().batchUpdate(documentId=DOC_ID, body={"requests": requests}).execute()


def pass2_numbered_list():
    """Re-fetch doc, then apply createParagraphBullets to the 6 summary
    bullets as a single contiguous numbered list."""
    creds = get_creds()
    docs = build("docs", "v1", credentials=creds)
    doc = docs.documents().get(documentId=DOC_ID).execute()
    spans = locate_anchors(doc["body"]["content"])

    list_start = spans["bullet_1"][0]
    list_end = spans["bullet_6"][1]
    print(f"[pass 2] numbered list range = [{list_start}, {list_end})")

    requests = [
        {
            "createParagraphBullets": {
                "range": {"startIndex": list_start, "endIndex": list_end},
                "bulletPreset": "NUMBERED_DECIMAL_ALPHA_ROMAN",
            }
        }
    ]
    docs.documents().batchUpdate(documentId=DOC_ID, body={"requests": requests}).execute()


if __name__ == "__main__":
    pass1_insert_blanks()
    pass2_numbered_list()
    print("done.")
