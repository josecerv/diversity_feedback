"""Italicize the three `p` characters in the new zero-baseline sentences we just
added to the Study 4 Discussion. Per JC's instruction.

Strategy:
  - Find the paragraph that contains "Building on this, we conducted a pooled".
  - Locate the three `p` characters that immediately precede ' < .001', ' = .19',
    and ' = .016'.
  - For each, batchUpdate.updateTextStyle to set italic=true on the single
    character.
"""
import json
import re
from pathlib import Path

from googleapiclient.discovery import build
from google.oauth2.credentials import Credentials
from google.auth.transport.requests import Request

DOC_ID = "1H9jAvqG5CzQe_BksjkNmLwjLZSlAHvaEK1iyuHL23FI"
TOKEN = Path(r"C:/Users/jcerv/.config/gws/token.json")


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


def main():
    creds = get_creds()
    docs = build("docs", "v1", credentials=creds)
    doc = docs.documents().get(
        documentId=DOC_ID,
        suggestionsViewMode="SUGGESTIONS_INLINE",
    ).execute()

    # Walk paragraphs and reconstruct text with character indices.
    # The Docs API gives us each run's startIndex; characters within a run
    # are at startIndex + offset.
    targets = [
        ("p < .001", "in (p < .001, N = 864)"),
        ("p = .19", "in (p = .19, N = 414)"),
        ("p = .016", "in (p = .016)"),
    ]

    target_indices = []

    def visit_para(para_el):
        # Reconstruct paragraph text with absolute indices.
        for r in para_el.get("elements", []):
            if "textRun" not in r:
                continue
            tr = r["textRun"]
            content = tr.get("content", "")
            start = r.get("startIndex", 0)
            # Skip runs that are NOT part of our suggestion
            ins_ids = tr.get("suggestedInsertionIds", [])
            if "suggest.uyglz6sdx856" not in ins_ids:
                continue
            for pattern_p, _label in targets:
                for m in re.finditer(re.escape(pattern_p), content):
                    p_index = start + m.start()
                    # We want to italicize just the single 'p' character at p_index
                    target_indices.append((pattern_p, p_index))

    def walk(els):
        for el in els:
            if "paragraph" in el:
                visit_para(el["paragraph"])
            elif "table" in el:
                for row in el["table"]["tableRows"]:
                    for cell in row["tableCells"]:
                        walk(cell.get("content", []))

    walk(doc["body"]["content"])

    print(f"Found {len(target_indices)} target positions:")
    for pat, idx in target_indices:
        print(f"  {pat!r} at index {idx}")

    if not target_indices:
        print("No targets found; aborting.")
        return 1

    # Build batchUpdate requests. Each updateTextStyle applies italic=true to a
    # single character at target_indices[i].
    requests_body = []
    for pat, idx in target_indices:
        requests_body.append({
            "updateTextStyle": {
                "range": {"startIndex": idx, "endIndex": idx + 1},
                "textStyle": {"italic": True},
                "fields": "italic",
            }
        })

    res = docs.documents().batchUpdate(
        documentId=DOC_ID,
        body={"requests": requests_body},
    ).execute()
    print(json.dumps(res, indent=2))


if __name__ == "__main__":
    import sys
    sys.exit(main() or 0)
