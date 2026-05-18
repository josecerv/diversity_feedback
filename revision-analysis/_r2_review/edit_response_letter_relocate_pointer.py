"""Edit 3: Direct API replace in the response letter Gdoc to redirect the
manuscript-location pointer from "Discussion section of Study 4" to the Study 5
preamble (alongside the existing mini-meta re-analysis).

JC has already revised the response letter; we inherit that state and only
update the single pointer sentence at the end of paragraph 42.
"""
import json
from pathlib import Path

from googleapiclient.discovery import build
from google.oauth2.credentials import Credentials
from google.auth.transport.requests import Request


RESPONSE_ID = "1YBHCMwD84xC3Qw38CPEzz1iv3JKg7Mw9jTJqyZGptkI"
TOKEN = Path(r"C:/Users/jcerv/.config/gws/token.json")

OLD = (
    "We have added to the Discussion section of Study 4 (p. X) summarizing "
    "these analyses and pointing interested readers to a new appendix section "
    "describing the results in more detail (see Appendix Section X)."
)
NEW = (
    "We have added a short paragraph to the preamble of Study 5 (p. X) "
    "summarizing these analyses, where it sits alongside the manuscript’s "
    "existing re-analysis of Studies 2, 3A, 3B, 4A, and 4B examining whether "
    "feedback effects are larger when initial selections contain fewer women "
    "or racial minorities. Interested readers can find the detailed results in "
    "a new appendix section (see Appendix Section X)."
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

    # Verify the OLD text is present (single occurrence) before mutating.
    pre = docs.documents().get(documentId=RESPONSE_ID).execute()

    def collect_text(els, out):
        for el in els:
            if "paragraph" in el:
                for r in el["paragraph"].get("elements", []):
                    if "textRun" in r:
                        out.append(r["textRun"].get("content", ""))
            elif "table" in el:
                for row in el["table"]["tableRows"]:
                    for cell in row["tableCells"]:
                        collect_text(cell.get("content", []), out)

    chunks = []
    collect_text(pre["body"]["content"], chunks)
    full = "".join(chunks)
    occurrences = full.count(OLD)
    print(f"OLD-string occurrences in current doc: {occurrences}")
    if occurrences != 1:
        print("Aborting: expected exactly 1 occurrence.")
        if occurrences == 0:
            # Try to find similar text to diagnose
            import re
            for m in re.finditer(r"We have added.{0,200}Appendix Section", full):
                print(f"  similar at offset {m.start()}: {m.group()[:300]}")
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
