"""Create a Google Doc from the local prereg draft markdown.

Reads Prereg_NPR_AttributeImportance_draft.md, creates a fresh Gdoc titled
"Prereg - NPR Attribute Importance Post-test (draft)", inserts the content,
and prints the share URL. Auths via the long-lived token at
C:/Users/jcerv/.config/gws/token.json (scopes: documents + drive).
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
DOC_TITLE = "Prereg - NPR Attribute Importance Post-test (draft)"


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
    """Convert the draft markdown to plain text suitable for a Gdoc body.

    Keeps structure (blank lines, question numbers, bullets) but drops
    markdown markers like leading '#', '---', and bold '**'.
    """
    out_lines: list[str] = []
    for raw in md.splitlines():
        line = raw.rstrip()
        # drop horizontal rules
        if line.strip() == "---":
            continue
        # heading lines: drop leading '#' markers, keep the text
        if line.startswith("#"):
            line = line.lstrip("#").strip()
        # drop emphasis asterisks (we'll re-apply bold via API to headings)
        line = line.replace("**", "")
        out_lines.append(line)
    # collapse runs of 3+ blank lines down to 2
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


# Regex for the 9 question headers like "1) Have any data..." at start of line
QUESTION_HEADER_RE = re.compile(r"^[1-9]\)\s.+$", re.MULTILINE)


def find_bold_ranges(text: str) -> list[tuple[int, int]]:
    """Return Gdoc (startIndex, endIndex) pairs (1-based) for headings to bold.

    Bolds:
      - The very first line (the doc title)
      - Each numbered question header line ("1) ...", "2) ...", ...)
    """
    # Google Docs body content starts at index 1 (index 0 is the doc segment).
    ranges: list[tuple[int, int]] = []
    # first non-empty line == doc title
    first_line_end = text.find("\n")
    if first_line_end > 0:
        ranges.append((1, 1 + first_line_end))
    for m in QUESTION_HEADER_RE.finditer(text):
        # Gdoc index is 1 + character offset (since index 1 = first char)
        start = 1 + m.start()
        end = 1 + m.end()
        ranges.append((start, end))
    return ranges


def main() -> int:
    if not DRAFT_PATH.exists():
        print(f"ERROR: draft not found at {DRAFT_PATH}", file=sys.stderr)
        return 1

    md = DRAFT_PATH.read_text(encoding="utf-8")
    body_text = strip_markdown(md)

    creds = get_creds()
    docs = build("docs", "v1", credentials=creds)
    drive = build("drive", "v3", credentials=creds)

    created = docs.documents().create(body={"title": DOC_TITLE}).execute()
    doc_id = created["documentId"]

    bold_ranges = find_bold_ranges(body_text)

    requests = [
        {"insertText": {"location": {"index": 1}, "text": body_text}},
    ]
    for start, end in bold_ranges:
        requests.append(
            {
                "updateTextStyle": {
                    "range": {"startIndex": start, "endIndex": end},
                    "textStyle": {"bold": True},
                    "fields": "bold",
                }
            }
        )

    docs.documents().batchUpdate(documentId=doc_id, body={"requests": requests}).execute()

    # Make the doc accessible to anyone with the link (so JC sees it without re-share)
    try:
        drive.permissions().create(
            fileId=doc_id,
            body={"type": "anyone", "role": "writer"},
            supportsAllDrives=True,
        ).execute()
    except Exception as e:  # noqa: BLE001
        print(f"WARN: could not set anyone-link permission: {e}", file=sys.stderr)

    url = f"https://docs.google.com/document/d/{doc_id}/edit"
    print(url)
    print(f"docId: {doc_id}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
