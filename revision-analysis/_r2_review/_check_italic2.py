"""Check the suggestedTextStyleChanges in detail for the N run."""
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


def walk(els):
    for el in els:
        if "paragraph" in el:
            txt = ""
            for r in el["paragraph"].get("elements", []):
                if "textRun" in r:
                    txt += r["textRun"].get("content", "")
            if "Prior to data collection" in txt:
                for i, r in enumerate(el["paragraph"]["elements"]):
                    if "textRun" in r:
                        tr = r["textRun"]
                        c = tr.get("content", "")
                        sug_style = tr.get("suggestedTextStyleChanges", {})
                        if sug_style:
                            print(f"run {i}: content={c[:60]!r}")
                            print(f"  sug_style: {json.dumps(sug_style, indent=2)}")
                return
        elif "table" in el:
            for row in el["table"]["tableRows"]:
                for cell in row["tableCells"]:
                    walk(cell.get("content", []))


walk(doc["body"]["content"])
