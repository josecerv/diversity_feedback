"""Verify the pretest suggestion was rejected and locate the new anchor index."""
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

target = "suggest.6o0uig9rs2pc"
found = False


def has_suggestion(els):
    global found
    for el in els:
        if "paragraph" in el:
            for r in el["paragraph"].get("elements", []):
                if "textRun" in r:
                    if target in r["textRun"].get("suggestedInsertionIds", []):
                        found = True
                        return
        elif "table" in el:
            for row in el["table"]["tableRows"]:
                for cell in row["tableCells"]:
                    has_suggestion(cell.get("content", []))


has_suggestion(doc["body"]["content"])
print(f"Suggestion {target} still present? {found}")


def find_anchor(els):
    for el in els:
        if "paragraph" in el:
            txt = ""
            for r in el["paragraph"].get("elements", []):
                if "textRun" in r:
                    txt += r["textRun"].get("content", "")
            if (
                "After receiving descriptive feedback about the composition of their past expert"
                in txt
                and "complete study materials" in txt
            ):
                print(f"Anchor paragraph startIndex={el['startIndex']} endIndex={el['endIndex']}")
                for r in el["paragraph"].get("elements", []):
                    if "textRun" in r and "complete study materials" in r["textRun"].get("content", ""):
                        print(f"  Last run startIndex={r['startIndex']} endIndex={r['endIndex']}")
                        print(f"  Tail: {r['textRun']['content'][-80:]!r}")
        elif "table" in el:
            for row in el["table"]["tableRows"]:
                for cell in row["tableCells"]:
                    find_anchor(cell.get("content", []))


find_anchor(doc["body"]["content"])
