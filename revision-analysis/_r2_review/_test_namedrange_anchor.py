"""Single-comment experiment: anchor a Drive comment using a NamedRange ID.

Steps:
  1. fetch doc via Docs API
  2. find the C4 anchor text -> get its startIndex/endIndex
  3. create a NamedRange covering it via documents.batchUpdate
  4. get the namedRangeId
  5. create a Drive comment with anchor = {"r":"head","a":[{"matcher":{"id":<id>}}]}
  6. report whether the comment is anchored (no "Original content deleted").
"""
import json
import time
from google.oauth2.credentials import Credentials
from google.auth.transport.requests import Request
from googleapiclient.discovery import build

DOC_ID = "1H9jAvqG5CzQe_BksjkNmLwjLZSlAHvaEK1iyuHL23FI"
TOKEN_PATH = r"C:/Users/jcerv/.config/gws/token.json"

ANCHOR_TEXT = (
    "We further posit that such feedback can be more effective at influencing "
    "future choices than descriptive feedback about other features of "
    "selection decisions."
)

COMMENT_BODY = (
    "[TEST] Anchor-format experiment using NamedRange. If this comment shows as "
    "anchored (not 'Original content deleted') in the Gdoc UI, the format works."
)


def get_creds():
    with open(TOKEN_PATH) as f:
        tok = json.load(f)
    creds = Credentials(
        token=tok.get("token"), refresh_token=tok["refresh_token"], token_uri=tok["token_uri"],
        client_id=tok["client_id"], client_secret=tok["client_secret"], scopes=tok["scopes"],
    )
    if not creds.valid:
        creds.refresh(Request())
    return creds


def find_text_range(doc, anchor: str):
    """Return (startIndex, endIndex) of the anchor text in the doc body, by
    walking textRuns and matching against the concatenated stream."""
    runs = []  # (doc_start_index, content)
    for el in doc["body"]["content"]:
        para = el.get("paragraph")
        if not para:
            continue
        for run in para.get("elements", []):
            tr = run.get("textRun")
            if not tr:
                continue
            content = tr.get("content", "")
            doc_start = run.get("startIndex")
            if doc_start is None:
                continue
            runs.append((doc_start, content))
    full = "".join(c for _, c in runs)
    offset = full.find(anchor)
    if offset < 0:
        return None
    end_offset = offset + len(anchor)
    # Map offset -> doc index by walking the runs.
    cursor = 0
    start_doc_idx = None
    end_doc_idx = None
    for doc_start, content in runs:
        seg_end = cursor + len(content)
        if start_doc_idx is None and cursor <= offset < seg_end:
            start_doc_idx = doc_start + (offset - cursor)
        if end_doc_idx is None and cursor < end_offset <= seg_end:
            end_doc_idx = doc_start + (end_offset - cursor)
        if start_doc_idx is not None and end_doc_idx is not None:
            break
        cursor = seg_end
    return start_doc_idx, end_doc_idx


def main():
    creds = get_creds()
    docs = build("docs", "v1", credentials=creds)
    drive = build("drive", "v3", credentials=creds)

    doc = docs.documents().get(documentId=DOC_ID).execute()
    rng = find_text_range(doc, ANCHOR_TEXT)
    if rng is None or rng[0] is None or rng[1] is None:
        print("FAIL: anchor text not found in doc")
        return
    start, end = rng
    print(f"Anchor range: [{start}, {end}]")

    # Create a NamedRange.
    name = f"r2_test_anchor_{int(time.time())}"
    nr_resp = docs.documents().batchUpdate(
        documentId=DOC_ID,
        body={"requests": [{
            "createNamedRange": {
                "name": name,
                "range": {"startIndex": start, "endIndex": end},
            }
        }]},
    ).execute()
    nr_id = nr_resp["replies"][0]["createNamedRange"]["namedRangeId"]
    print(f"Created NamedRange: name={name!r} id={nr_id!r}")

    # Now create the Drive comment with matcher.id = namedRangeId.
    anchor_obj = {"r": "head", "a": [{"matcher": {"id": nr_id}}]}
    result = drive.comments().create(
        fileId=DOC_ID,
        body={
            "content": COMMENT_BODY,
            "anchor": json.dumps(anchor_obj),
            "quotedFileContent": {"value": ANCHOR_TEXT, "mimeType": "text/plain"},
        },
        fields="id,anchor,quotedFileContent",
    ).execute()
    print(f"Posted comment: id={result.get('id')}")
    print(f"  stored anchor    : {result.get('anchor')}")
    print(f"  stored quoted    : {(result.get('quotedFileContent') or {}).get('value','')[:80]!r}")
    print()
    print("=> Open the manuscript Gdoc and check whether this comment shows as")
    print("   ANCHORED (visible highlight on the anchored sentence) or as")
    print("   'Original content deleted'.")


if __name__ == "__main__":
    main()
