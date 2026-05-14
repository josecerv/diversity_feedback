"""Read the R2 response-letter Gdoc to a plain-text dump for inspection."""
import json, pathlib
from googleapiclient.discovery import build
from google.oauth2.credentials import Credentials
from google.auth.transport.requests import Request

DOC_ID = "1YBHCMwD84xC3Qw38CPEzz1iv3JKg7Mw9jTJqyZGptkI"
TOKEN  = pathlib.Path(r"C:/Users/jcerv/.config/gws/token.json")
OUT    = pathlib.Path(r"C:/Users/jcerv/Jose/diversity_feedback/revision-analysis/_r2_review/r2_gdoc_text.txt")


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
        elif "sectionBreak" in el:
            lines.append("---")


def main():
    creds = get_creds()
    docs = build("docs", "v1", credentials=creds)
    doc = docs.documents().get(documentId=DOC_ID).execute()
    body = doc["body"]["content"]
    lines = []
    walk(body, lines)
    text = "\n".join(lines)
    OUT.write_text(text, encoding="utf-8")
    print(f"Wrote {OUT} ({len(text)} chars)")


if __name__ == "__main__":
    main()
