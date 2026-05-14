"""Publish the George response to the R2 response-letter Gdoc.

Plan:
  1. Find the paragraph "RESPONSE:" that immediately follows George's comment
     ("...so that the paper starts with a strong study.") and the FOR TEAM
     stub that follows it. The placeholder spans from the start of "RESPONSE:"
     through the end of the FOR TEAM block (last line: "Google doc"),
     ending just before Wu's comment starts ("On the request, I liked Study 4b").
  2. Delete that range.
  3. Insert "RESPONSE: <body>\n" at the deletion point.
  4. Apply italic styling to "more important" inside the inserted body.
"""

import json
import pathlib
import re
import sys
sys.stdout.reconfigure(encoding="utf-8")
from googleapiclient.discovery import build
from google.oauth2.credentials import Credentials
from google.auth.transport.requests import Request

DOC_ID = "1YBHCMwD84xC3Qw38CPEzz1iv3JKg7Mw9jTJqyZGptkI"
TOKEN  = pathlib.Path(r"C:/Users/jcerv/.config/gws/token.json")

# Body of the new response. The final paragraph terminator (\n) is appended
# when we insert, so do NOT include it here.
RESPONSE_TEXT = (
    "RESPONSE: Thank you for this suggestion. We ran the post-test you proposed "
    "and incorporated the result into the Discussion of Study 1 (p. X). We recruited "
    "N = 302 Prolific participants and randomly assigned each to rate, on a 1 to 7 "
    "importance scale, one of the four expert-attribute statements used in Study 1’s "
    "feedback manipulation, with the NPR producer framing held constant. Two of the "
    "three comparison attributes were rated significantly more important than the "
    "gender attribute (“were women”; M = 3.64, SD = 1.76, n = 76): "
    "“worked at a university” (M = 4.67, SD = 1.60; Welch’s t(148.8) = "
    "−3.76, p < .001, d = −0.61) and “were under 50 years old” "
    "(M = 4.44, SD = 1.73; t(149.0) = −2.81, p = .006, d = −0.46). The third, "
    "“based on the West Coast of the United States,” showed no significant "
    "difference in importance (M = 3.37, SD = 1.75; t(149.0) = 0.95, p = .343, "
    "d = +0.16). Differences in perceived importance therefore cannot account for why "
    "only feedback about gender moved subsequent selections in Study 1, consistent with "
    "our claim that descriptive feedback regulates behavior on dimensions carrying an "
    "implicit injunctive norm."
)

# Anchors
GEORGE_COMMENT_TAIL = "so that the paper starts with a strong study."
WU_COMMENT_HEAD     = "On the request, I liked Study 4b"


def get_creds():
    data = json.load(open(TOKEN, encoding="utf-8"))
    creds = Credentials.from_authorized_user_info(data, [
        "https://www.googleapis.com/auth/documents",
    ])
    if not creds.valid and creds.refresh_token:
        creds.refresh(Request())
    return creds


def paragraph_text(p):
    return "".join(
        e.get("textRun", {}).get("content", "") for e in p["paragraph"]["elements"]
    )


def walk_paragraphs(elements, out):
    for el in elements:
        if "paragraph" in el:
            out.append((el["startIndex"], el["endIndex"], paragraph_text(el)))
        elif "table" in el:
            for row in el["table"]["tableRows"]:
                for cell in row["tableCells"]:
                    walk_paragraphs(cell.get("content", []), out)


def find_target_span(paragraphs):
    """Return (delete_start, delete_end) covering the empty RESPONSE: paragraph
    plus the FOR TEAM stub, up to but excluding Wu's comment paragraph."""
    george_idx = next(
        i for i, (_, _, t) in enumerate(paragraphs) if GEORGE_COMMENT_TAIL in t
    )
    response_idx = george_idx + 1
    assert paragraphs[response_idx][2].strip() == "RESPONSE:", (
        f"Expected the paragraph right after George's comment to be the empty "
        f"'RESPONSE:' placeholder; got: {paragraphs[response_idx][2]!r}"
    )
    wu_idx = next(
        i for i, (_, _, t) in enumerate(paragraphs) if WU_COMMENT_HEAD in t
    )
    delete_start = paragraphs[response_idx][0]
    delete_end   = paragraphs[wu_idx][0]
    placeholder = "".join(t for _, _, t in paragraphs[response_idx:wu_idx])
    return delete_start, delete_end, placeholder


def main():
    creds = get_creds()
    docs = build("docs", "v1", credentials=creds)
    doc = docs.documents().get(documentId=DOC_ID).execute()
    body = doc["body"]["content"]

    paragraphs = []
    walk_paragraphs(body, paragraphs)

    delete_start, delete_end, placeholder = find_target_span(paragraphs)
    print(f"Will delete range [{delete_start}, {delete_end})")
    print(f"--- Placeholder to be removed ({len(placeholder)} chars) ---")
    print(placeholder)
    print("--- End placeholder ---\n")
    print(f"Will insert {len(RESPONSE_TEXT) + 1} chars (response body + trailing newline)")

    # Step 1: delete the placeholder span.
    docs.documents().batchUpdate(
        documentId=DOC_ID,
        body={
            "requests": [
                {"deleteContentRange": {
                    "range": {"startIndex": delete_start, "endIndex": delete_end}
                }}
            ]
        },
    ).execute()
    print("Step 1: placeholder deleted.")

    # Step 2: insert the new response paragraph at the same index.
    insert_idx = delete_start
    insert_text = RESPONSE_TEXT + "\n"
    docs.documents().batchUpdate(
        documentId=DOC_ID,
        body={
            "requests": [
                {"insertText": {
                    "location": {"index": insert_idx},
                    "text": insert_text,
                }},
                # Make sure the newly inserted block is plain (not italic/bold)
                {"updateTextStyle": {
                    "range": {
                        "startIndex": insert_idx,
                        "endIndex": insert_idx + len(insert_text),
                    },
                    "textStyle": {"italic": False, "bold": False},
                    "fields": "italic,bold",
                }},
            ]
        },
    ).execute()
    print("Step 2: response body inserted (and forced to plain styling).")

    # Step 3: italicize the phrase "more important".
    phrase = "more important"
    rel = RESPONSE_TEXT.index(phrase)
    italic_start = insert_idx + rel
    italic_end = italic_start + len(phrase)
    docs.documents().batchUpdate(
        documentId=DOC_ID,
        body={
            "requests": [
                {"updateTextStyle": {
                    "range": {"startIndex": italic_start, "endIndex": italic_end},
                    "textStyle": {"italic": True},
                    "fields": "italic",
                }}
            ]
        },
    ).execute()
    print(f"Step 3: italicized 'more important' at [{italic_start}, {italic_end}).")
    print("\nDone.")


if __name__ == "__main__":
    main()
