"""Overwrite the existing prereg Gdoc with the current local markdown.

Target doc: https://docs.google.com/document/d/1MjUzC3wKq0VEx6qCk1P-jLO779wyIpLl81y7B3FLXDE/edit
Reuses the strip_markdown + bolding logic from create_prereg_gdoc.py but replaces
content in place (no new doc).
"""

from __future__ import annotations

import json
import re
import sys
from pathlib import Path

from google.oauth2.credentials import Credentials
from google.auth.transport.requests import Request
from googleapiclient.discovery import build

TOKEN_PATH = Path(r"C:/Users/jcerv/.config/gws/token.json")
DRAFT_PATH = Path(__file__).parent / "Prereg_NPR_AttributeImportance_draft.md"
DOC_ID = "1MjUzC3wKq0VEx6qCk1P-jLO779wyIpLl81y7B3FLXDE"


def get_creds() -> Credentials:
    tok = json.loads(TOKEN_PATH.read_text())
    creds = Credentials(
        token=tok.get("token"),
        refresh_token=tok["refresh_token"],
        token_uri=tok.get("token_uri", "https://oauth2.googleapis.com/token"),
        client_id=tok["client_id"],
        client_secret=tok["client_secret"],
        scopes=tok["scopes"],
    )
    if not creds.valid:
        creds.refresh(Request())
    return creds


def strip_markdown(md: str) -> str:
    out_lines: list[str] = []
    for raw in md.splitlines():
        line = raw.rstrip()
        if line.strip() == "---":
            continue
        if line.startswith("#"):
            line = line.lstrip("#").strip()
        line = line.replace("**", "")
        out_lines.append(line)
    collapsed: list[str] = []
    blank_run = 0
    for ln in out_lines:
        if ln.strip() == "":
            blank_run += 1
            if blank_run <= 2:
                collapsed.append(ln)
        else:
            blank_run = 0
            collapsed.append(ln)
    return "\n".join(collapsed).strip() + "\n"


QUESTION_HEADER_RE = re.compile(r"^[1-9]\)\s.+$", re.MULTILINE)


def find_bold_ranges(text: str) -> list[tuple[int, int]]:
    ranges: list[tuple[int, int]] = []
    first_line_end = text.find("\n")
    if first_line_end > 0:
        ranges.append((1, 1 + first_line_end))
    for m in QUESTION_HEADER_RE.finditer(text):
        ranges.append((1 + m.start(), 1 + m.end()))
    return ranges


def main() -> int:
    md = DRAFT_PATH.read_text(encoding="utf-8")
    body_text = strip_markdown(md)

    creds = get_creds()
    docs = build("docs", "v1", credentials=creds)

    doc = docs.documents().get(documentId=DOC_ID).execute()
    body = doc.get("body", {}).get("content", [])
    # End index of the doc body is the endIndex of the last structural element.
    # We delete from index 1 to (end - 1) to clear everything but the trailing
    # newline the API insists on keeping.
    end_index = body[-1]["endIndex"] if body else 1
    requests: list[dict] = []
    if end_index > 2:
        requests.append(
            {
                "deleteContentRange": {
                    "range": {"startIndex": 1, "endIndex": end_index - 1}
                }
            }
        )
    requests.append({"insertText": {"location": {"index": 1}, "text": body_text}})

    # Reset any leftover styling on the inserted range, then re-bold headings.
    text_len = len(body_text)
    requests.append(
        {
            "updateTextStyle": {
                "range": {"startIndex": 1, "endIndex": 1 + text_len},
                "textStyle": {"bold": False},
                "fields": "bold",
            }
        }
    )
    for start, end in find_bold_ranges(body_text):
        requests.append(
            {
                "updateTextStyle": {
                    "range": {"startIndex": start, "endIndex": end},
                    "textStyle": {"bold": True},
                    "fields": "bold",
                }
            }
        )

    docs.documents().batchUpdate(documentId=DOC_ID, body={"requests": requests}).execute()
    print(f"https://docs.google.com/document/d/{DOC_ID}/edit")
    return 0


if __name__ == "__main__":
    sys.exit(main())
