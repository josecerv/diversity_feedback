"""Fetch the canonical manuscript Gdoc as both plain text and a .docx export.

The local Manuscript_DoesFeedbackEnhanceDiversity.docx is stale; the canonical source
is the Google Doc whose ID is hard-coded below. Always re-run this before any build
that consumes the manuscript.
"""
import io
import json
import pathlib

from googleapiclient.discovery import build
from googleapiclient.http import MediaIoBaseDownload
from google.oauth2.credentials import Credentials
from google.auth.transport.requests import Request


DOC_ID = "1H9jAvqG5CzQe_BksjkNmLwjLZSlAHvaEK1iyuHL23FI"
TOKEN = pathlib.Path(r"C:/Users/jcerv/.config/gws/token.json")
OUT_DIR = pathlib.Path(
    r"C:/Users/jcerv/Jose/diversity_feedback/revision-analysis/_r2_review"
)
TEXT_OUT = OUT_DIR / "manuscript_gdoc_text.txt"
DOCX_OUT = OUT_DIR / "manuscript_gdoc_export.docx"

DOCX_MIME = (
    "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
)


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
    TEXT_OUT.write_text(text, encoding="utf-8")
    print(f"Wrote {TEXT_OUT} ({len(text)} chars)")

    drive = build("drive", "v3", credentials=creds)
    request = drive.files().export_media(fileId=DOC_ID, mimeType=DOCX_MIME)
    buf = io.BytesIO()
    downloader = MediaIoBaseDownload(buf, request)
    done = False
    while not done:
        status, done = downloader.next_chunk()
    DOCX_OUT.write_bytes(buf.getvalue())
    print(f"Wrote {DOCX_OUT} ({DOCX_OUT.stat().st_size} bytes)")


if __name__ == "__main__":
    main()
