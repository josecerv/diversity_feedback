"""Test 2: pass anchor as a PLAIN kix.xxxx string (matching the format Sophia
Pink's UI-created comments are stored as), not wrapped in the {"r":"head",...}
JSON envelope.

If this binds in the UI, we know the JSON wrapping was the problem.
"""
import json
import time
from google.oauth2.credentials import Credentials
from google.auth.transport.requests import Request
from googleapiclient.discovery import build

DOC_ID = "1H9jAvqG5CzQe_BksjkNmLwjLZSlAHvaEK1iyuHL23FI"
TOKEN_PATH = r"C:/Users/jcerv/.config/gws/token.json"

ANCHOR_TEXT = (
    "(Allcott, 2011; Bhanot, 2021; Cialdini et al., 2006; Tiefenbeck et al., 2018)"
)
COMMENT_BODY = "[TEST 2] Anchor as plain kix.xxxx string (not JSON-wrapped)."


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


def find_text_range(doc, anchor):
    runs = []
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
        print("FAIL: anchor text not found")
        return
    start, end = rng
    print(f"Anchor range: [{start}, {end}]")

    name = f"r2_test_plain_{int(time.time())}"
    nr = docs.documents().batchUpdate(
        documentId=DOC_ID,
        body={"requests": [{
            "createNamedRange": {"name": name, "range": {"startIndex": start, "endIndex": end}}
        }]},
    ).execute()
    nr_id = nr["replies"][0]["createNamedRange"]["namedRangeId"]
    print(f"NamedRange id: {nr_id}")

    # Try the plain-string form.
    result = drive.comments().create(
        fileId=DOC_ID,
        body={
            "content": COMMENT_BODY,
            "anchor": nr_id,  # plain string, NOT JSON-wrapped
            "quotedFileContent": {"value": ANCHOR_TEXT, "mimeType": "text/plain"},
        },
        fields="id,anchor,quotedFileContent",
    ).execute()
    print(f"Posted: id={result.get('id')}")
    print(f"  stored anchor: {result.get('anchor')!r}")


if __name__ == "__main__":
    main()
