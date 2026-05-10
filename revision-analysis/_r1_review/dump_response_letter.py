"""Dump the response-letter Gdoc + its Drive comments for review.

Outputs:
  - response_letter.json        full structured doc
  - response_letter.txt         plain-text rendering with [TABLE]/[SECTION] markers
  - response_letter_comments.json  full Drive comments (with replies)
"""
from __future__ import annotations
import json, sys, io
from pathlib import Path

sys.stdout = io.TextIOWrapper(sys.stdout.buffer, encoding="utf-8")

from google.oauth2.credentials import Credentials
from google.auth.transport.requests import Request
from googleapiclient.discovery import build

DOC_ID = "1YBHCMwD84xC3Qw38CPEzz1iv3JKg7Mw9jTJqyZGptkI"
TOKEN_PATH = r"C:/Users/jcerv/.config/gws/token.json"
OUT_DIR = Path(__file__).resolve().parent
JSON_OUT = OUT_DIR / "response_letter.json"
TEXT_OUT = OUT_DIR / "response_letter.txt"
COMMENTS_OUT = OUT_DIR / "response_letter_comments.json"

with open(TOKEN_PATH) as f:
    tok = json.load(f)
creds = Credentials(
    token=tok.get("token"),
    refresh_token=tok["refresh_token"],
    token_uri=tok["token_uri"],
    client_id=tok["client_id"],
    client_secret=tok["client_secret"],
    scopes=tok["scopes"],
)
if not creds.valid:
    creds.refresh(Request())

docs = build("docs", "v1", credentials=creds)
drive = build("drive", "v3", credentials=creds)

# 1. Doc body
doc = docs.documents().get(
    documentId=DOC_ID,
    suggestionsViewMode="DEFAULT_FOR_CURRENT_ACCESS",
).execute()
JSON_OUT.write_text(json.dumps(doc, ensure_ascii=False, indent=2), encoding="utf-8")
print(f"Wrote {JSON_OUT} ({JSON_OUT.stat().st_size:,} bytes)")
print(f"Title: {doc.get('title')}")
print(f"Body content elements: {len(doc['body']['content'])}")

# 2. Plain text rendering
def walk(elements, out, depth=0):
    for el in elements:
        if "paragraph" in el:
            buf = []
            for run in el["paragraph"].get("elements", []):
                tr = run.get("textRun")
                if tr:
                    buf.append(tr.get("content", ""))
            out.append("".join(buf))
        elif "table" in el:
            out.append("[TABLE START]\n")
            for row in el["table"]["tableRows"]:
                row_cells = []
                for cell in row["tableCells"]:
                    cell_buf = []
                    walk(cell.get("content", []), cell_buf, depth + 1)
                    row_cells.append("".join(cell_buf).strip())
                out.append(" | ".join(row_cells) + "\n")
            out.append("[TABLE END]\n")
        elif "sectionBreak" in el:
            out.append("\n[SECTION BREAK]\n")
        elif "tableOfContents" in el:
            out.append("[TOC]\n")

text_out = []
walk(doc["body"]["content"], text_out)
TEXT_OUT.write_text("".join(text_out), encoding="utf-8")
print(f"Wrote {TEXT_OUT} ({TEXT_OUT.stat().st_size:,} bytes)")

# 3. Comments via Drive API
all_comments = []
page_token = None
while True:
    resp = drive.comments().list(
        fileId=DOC_ID,
        fields=("nextPageToken,comments(id,author,createdTime,modifiedTime,resolved,"
                "anchor,quotedFileContent,content,htmlContent,replies(id,author,"
                "createdTime,content,htmlContent))"),
        includeDeleted=False,
        pageSize=100,
        pageToken=page_token,
    ).execute()
    all_comments.extend(resp.get("comments", []))
    page_token = resp.get("nextPageToken")
    if not page_token:
        break

COMMENTS_OUT.write_text(json.dumps(all_comments, ensure_ascii=False, indent=2), encoding="utf-8")
print(f"Wrote {COMMENTS_OUT} ({COMMENTS_OUT.stat().st_size:,} bytes; {len(all_comments)} comments)")

# Quick comment summary
print("\n--- Comment authors / quoted anchors ---")
for c in all_comments:
    author = c.get("author", {}).get("displayName", "?")
    quote = (c.get("quotedFileContent") or {}).get("value", "")[:80].replace("\n", " ")
    body = (c.get("content") or "")[:120].replace("\n", " ")
    resolved = c.get("resolved")
    print(f"[{author}] resolved={resolved}  anchor={quote!r}")
    print(f"    body: {body!r}")
    for r in c.get("replies") or []:
        rauthor = r.get("author", {}).get("displayName", "?")
        rbody = (r.get("content") or "")[:120].replace("\n", " ")
        print(f"    reply [{rauthor}]: {rbody!r}")
