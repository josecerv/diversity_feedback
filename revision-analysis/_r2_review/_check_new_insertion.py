"""Confirm the new pretest paragraph is in the Gdoc as a tracked suggestion in the right place."""
import json
from googleapiclient.discovery import build
from google.oauth2.credentials import Credentials
from google.auth.transport.requests import Request

with open(r"C:/Users/jcerv/.config/gws/token.json") as f:
    data = json.load(f)
creds = Credentials.from_authorized_user_info(
    data,
    ["https://www.googleapis.com/auth/documents", "https://www.googleapis.com/auth/drive"],
)
if not creds.valid and creds.refresh_token:
    creds.refresh(Request())

docs = build("docs", "v1", credentials=creds)
doc = docs.documents().get(
    documentId="1H9jAvqG5CzQe_BksjkNmLwjLZSlAHvaEK1iyuHL23FI",
    suggestionsViewMode="SUGGESTIONS_INLINE",
).execute()


def walk(els, prev_anchor_idx=None):
    for el in els:
        if "paragraph" in el:
            txt = ""
            ins_ids = set()
            italic_segments = []
            for r in el["paragraph"].get("elements", []):
                if "textRun" in r:
                    tr = r["textRun"]
                    txt += tr.get("content", "")
                    for sid in tr.get("suggestedInsertionIds", []):
                        ins_ids.add(sid)
                    style = tr.get("textStyle", {})
                    if style.get("italic"):
                        italic_segments.append(tr.get("content", "").strip())
            if "Prior to data collection" in txt:
                print(f"Found new paragraph at start={el['startIndex']} end={el['endIndex']}")
                print(f"  Suggestion ids in this paragraph: {sorted(ins_ids)}")
                print(f"  Italic segments: {italic_segments!r}")
                print(f"  Text (first 240 chars): {txt[:240]!r}")
                print(f"  Text (chars 240-480): {txt[240:480]!r}")
                print(f"  Text (chars 480-800): {txt[480:800]!r}")
                print(f"  Total length: {len(txt)} chars")
        elif "table" in el:
            for row in el["table"]["tableRows"]:
                for cell in row["tableCells"]:
                    walk(cell.get("content", []))


walk(doc["body"]["content"])
