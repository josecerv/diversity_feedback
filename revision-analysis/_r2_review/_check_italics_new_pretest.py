"""Verify italics on N and the three p's in the new Study 1 pretest paragraph."""
import json
from pathlib import Path

from googleapiclient.discovery import build
from google.oauth2.credentials import Credentials
from google.auth.transport.requests import Request

DOC_ID = "1H9jAvqG5CzQe_BksjkNmLwjLZSlAHvaEK1iyuHL23FI"
TOKEN = Path(r"C:/Users/jcerv/.config/gws/token.json")

data = json.load(open(TOKEN))
creds = Credentials.from_authorized_user_info(
    data,
    ["https://www.googleapis.com/auth/documents", "https://www.googleapis.com/auth/drive"],
)
if not creds.valid and creds.refresh_token:
    creds.refresh(Request())

docs = build("docs", "v1", credentials=creds)
doc = docs.documents().get(
    documentId=DOC_ID,
    suggestionsViewMode="SUGGESTIONS_INLINE",
).execute()


def walk(els):
    for el in els:
        if "paragraph" in el:
            txt = ""
            for r in el["paragraph"].get("elements", []):
                if "textRun" in r:
                    txt += r["textRun"].get("content", "")
            if "To verify that the three comparison attributes" in txt:
                for i, r in enumerate(el["paragraph"]["elements"]):
                    if "textRun" in r:
                        tr = r["textRun"]
                        c = tr.get("content", "")
                        style = tr.get("textStyle", {})
                        sug_style = tr.get("suggestedTextStyleChanges", {})
                        # Resolve italic state from suggested style
                        resolved_italic = style.get("italic")
                        for sid, sc in sug_style.items():
                            if "italic" in sc.get("textStyle", {}):
                                resolved_italic = sc["textStyle"]["italic"]
                        if c.strip() in ("N", "p"):
                            print(f"  run {i}: {c!r}  italic={resolved_italic!r}")
                        elif len(c) <= 4:
                            print(f"  run {i}: {c!r}  italic={resolved_italic!r}  (short)")
                return
        elif "table" in el:
            for row in el["table"]["tableRows"]:
                for cell in row["tableCells"]:
                    walk(cell.get("content", []))


walk(doc["body"]["content"])
