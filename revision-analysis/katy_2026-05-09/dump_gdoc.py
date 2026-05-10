"""Pull the current state of the manuscript Gdoc and dump:
- the full structured JSON
- a plain-text rendering with paragraph breaks (for diff-friendly review)

This is the "current Gdoc state" we will triangulate against Katy's PDF.
"""
from __future__ import annotations
import json, sys, io
from pathlib import Path

sys.stdout = io.TextIOWrapper(sys.stdout.buffer, encoding="utf-8")

from google.oauth2.credentials import Credentials
from google.auth.transport.requests import Request
from googleapiclient.discovery import build

DOC_ID = "1H9jAvqG5CzQe_BksjkNmLwjLZSlAHvaEK1iyuHL23FI"
TOKEN_PATH = r"C:/Users/jcerv/.config/gws/token.json"
OUT_DIR = Path(r"C:/Users/jcerv/Jose/diversity_feedback/revision-analysis/katy_2026-05-09")
JSON_OUT = OUT_DIR / "gdoc_current.json"
TEXT_OUT = OUT_DIR / "gdoc_current.txt"

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

# Pull suggestions inline so we can see Sophia/Jose unsynced edits + Katy's
# preview-mode "accepted" view as separate streams.
doc = docs.documents().get(
    documentId=DOC_ID,
    suggestionsViewMode="DEFAULT_FOR_CURRENT_ACCESS",
).execute()

JSON_OUT.write_text(json.dumps(doc, ensure_ascii=False, indent=2), encoding="utf-8")
print(f"Wrote {JSON_OUT} ({JSON_OUT.stat().st_size:,} bytes)")

# Plain-text rendering (no suggestions resolved — raw text runs)
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

# Quick metadata
print()
print(f"Title: {doc.get('title')}")
print(f"Body content elements: {len(doc['body']['content'])}")
suggested_count = doc.get("suggestionsViewMode")
print(f"suggestionsViewMode: {suggested_count}")

# Count tables
tables = [e for e in doc["body"]["content"] if "table" in e]
print(f"Tables: {len(tables)}")
