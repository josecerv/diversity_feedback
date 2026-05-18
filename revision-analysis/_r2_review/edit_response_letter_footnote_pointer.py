"""Edit 3: Update response letter pointer to reflect new manuscript location
(footnote in General Discussion paragraph, anchored on the Technologist
differential-effect sentence)."""
import json
from pathlib import Path

from googleapiclient.discovery import build
from google.oauth2.credentials import Credentials
from google.auth.transport.requests import Request


RESPONSE_ID = "1YBHCMwD84xC3Qw38CPEzz1iv3JKg7Mw9jTJqyZGptkI"
TOKEN = Path(r"C:/Users/jcerv/.config/gws/token.json")

OLD = (
    "We have added a short paragraph to the preamble of Study 5 (p. X) "
    "summarizing these analyses, where it sits alongside the manuscript’s "
    "existing re-analysis of Studies 2, 3A, 3B, 4A, and 4B examining whether "
    "feedback effects are larger when initial selections contain fewer women "
    "or racial minorities. Interested readers can find the detailed results in "
    "a new appendix section (see Appendix Section X)."
)
NEW = (
    "We have added a footnote to the General Discussion (p. X), anchored on "
    "the sentence where we first state the differential effect of race or "
    "gender feedback versus comparison-attribute feedback. The footnote "
    "summarizes the cross-study rule-out and points readers to Appendix "
    "Section S[X] for the full per-cell analysis."
)


def main():
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

    docs = build("docs", "v1", credentials=creds)

    pre = docs.documents().get(documentId=RESPONSE_ID).execute()

    def collect(els, out):
        for el in els:
            if "paragraph" in el:
                for r in el["paragraph"].get("elements", []):
                    if "textRun" in r:
                        out.append(r["textRun"].get("content", ""))
            elif "table" in el:
                for row in el["table"]["tableRows"]:
                    for cell in row["tableCells"]:
                        collect(cell.get("content", []), out)

    chunks = []
    collect(pre["body"]["content"], chunks)
    full = "".join(chunks)
    occ = full.count(OLD)
    print(f"OLD occurrences: {occ}")
    if occ != 1:
        # Diagnose
        import re
        for m in re.finditer(r"We have added.{0,260}", full):
            print(f"  found at offset {m.start()}: {m.group()[:300]!r}")
        return 1

    res = docs.documents().batchUpdate(
        documentId=RESPONSE_ID,
        body={
            "requests": [
                {
                    "replaceAllText": {
                        "containsText": {"text": OLD, "matchCase": True},
                        "replaceText": NEW,
                    }
                }
            ]
        },
    ).execute()
    print(json.dumps(res, indent=2))


if __name__ == "__main__":
    import sys
    sys.exit(main() or 0)
