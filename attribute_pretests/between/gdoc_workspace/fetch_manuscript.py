"""Fetch the canonical manuscript Gdoc text for voice-reference."""
import json, pathlib
from googleapiclient.discovery import build
from google.oauth2.credentials import Credentials
from google.auth.transport.requests import Request

DOC_ID = "1H9jAvqG5CzQe_BksjkNmLwjLZSlAHvaEK1iyuHL23FI"
TOKEN  = pathlib.Path(r"C:/Users/jcerv/.config/gws/token.json")
OUT    = pathlib.Path(__file__).parent / "manuscript_text.txt"

def get_creds():
    data = json.load(open(TOKEN))
    creds = Credentials.from_authorized_user_info(data, [
        "https://www.googleapis.com/auth/documents",
        "https://www.googleapis.com/auth/drive",
    ])
    if not creds.valid and creds.refresh_token:
        creds.refresh(Request())
    return creds

def walk(elements, lines):
    for el in elements:
        if "paragraph" in el:
            text = ""
            for r in el["paragraph"]["elements"]:
                if "textRun" in r:
                    text += r["textRun"].get("content", "")
            lines.append(text.rstrip("\n"))
        elif "table" in el:
            for row in el["table"]["tableRows"]:
                cells = []
                for cell in row["tableCells"]:
                    sub = []
                    walk(cell.get("content", []), sub)
                    cells.append(" / ".join(sub))
                lines.append(" | ".join(cells))

creds = get_creds()
docs = build("docs", "v1", credentials=creds)
doc = docs.documents().get(documentId=DOC_ID).execute()
lines = []
walk(doc["body"]["content"], lines)
OUT.write_text("\n".join(lines), encoding="utf-8")
print(f"Wrote {OUT} ({sum(len(l) for l in lines)} chars)")
