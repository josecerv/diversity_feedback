"""Pull the current prereg Gdoc body to plain text and overwrite the local md.

Treats the Gdoc as the source of truth. Run before making any edit so the
local markdown reflects whatever JC last typed in the doc.
"""

from __future__ import annotations

import json
import sys
from pathlib import Path

from google.oauth2.credentials import Credentials
from google.auth.transport.requests import Request
from googleapiclient.discovery import build

TOKEN_PATH = Path(r"C:/Users/jcerv/.config/gws/token.json")
LOCAL_PATH = Path(__file__).parent / "Prereg_NPR_AttributeImportance_draft.md"
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


def doc_to_text(doc: dict) -> str:
    out: list[str] = []
    for el in doc.get("body", {}).get("content", []):
        para = el.get("paragraph")
        if not para:
            continue
        line_parts: list[str] = []
        for run in para.get("elements", []):
            tr = run.get("textRun")
            if tr and "content" in tr:
                line_parts.append(tr["content"])
        out.append("".join(line_parts))
    return "".join(out)


def main() -> int:
    creds = get_creds()
    docs = build("docs", "v1", credentials=creds)
    doc = docs.documents().get(documentId=DOC_ID).execute()
    text = doc_to_text(doc)
    LOCAL_PATH.write_text(text, encoding="utf-8")
    print(f"wrote {len(text)} chars to {LOCAL_PATH.name}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
