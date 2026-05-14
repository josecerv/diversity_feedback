"""Push two edits to the R2 response-letter Gdoc:

1. Cover-letter Point 3: replace the `[Insert ...]` placeholder with a clear
   2-sentence summary of the new zero-baseline analysis (no table, no
   specific stats -- those live in the body).

2. DE-response body (line 44 area): drop Study 4A from the pooled-analysis
   sentence and add a one-line rationale matching Katy's anchored comment
   ("you can't include Study 4A without a single 'comparison attribute'
   datapoint").

Stale numerical estimates (21.28pp, 6.83pp, CI [+2.70, +26.21]) are NOT
touched in this pass per Jose's note that the data is stale and may be
re-run with the refreshed sample.

Uses replaceAllText so we don't have to compute indices. Both target strings
are unique in the doc (verified before writing).
"""
from __future__ import annotations

import json
import pathlib

from googleapiclient.discovery import build
from google.oauth2.credentials import Credentials
from google.auth.transport.requests import Request


DOC_ID = "1YBHCMwD84xC3Qw38CPEzz1iv3JKg7Mw9jTJqyZGptkI"
TOKEN = pathlib.Path(r"C:/Users/jcerv/.config/gws/token.json")


# --- Edit 1: cover-letter Point 3 -----------------------------------------
POINT3_OLD = "[Insert new description of analyses in R1 here]"
POINT3_NEW = (
    "To address your concern that participants might already use comparison "
    "attributes “appropriately” in their initial selections "
    "(potentially explaining why feedback about race and gender appears "
    "uniquely effective in our studies), we conducted a new pooled analysis "
    "restricted to participants whose initial selections contained zero of a "
    "given attribute, a conservative signal of underrepresentation that "
    "should provoke a reaction to feedback regardless of which attribute is "
    "involved. Within this sample, feedback about race or gender "
    "significantly increased the probability of subsequently selecting a "
    "woman or racial minority, whereas feedback about a comparison "
    "attribute (e.g., year of release, protagonist’s profession) did "
    "not significantly change subsequent selections, and a Wald test "
    "confirms this difference is significant."
)


# --- Edit 2: body text dropping Study 4A ----------------------------------
BODY_OLD = (
    "we pooled data from Studies 2, 3A, 3B, 4A, and 4B (clustering standard "
    "errors by participant)."
)
BODY_NEW = (
    "we pooled data from Studies 2, 3A, 3B, and 4B (clustering standard "
    "errors by participant). We exclude Study 4A from this analysis because "
    "no participants in that study made initial selections containing zero "
    "of any of the comparison attributes, meaning there is no comparable "
    "“zero-baseline” cell to contrast against the race/gender "
    "effect."
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


def doc_text(doc):
    parts = []
    for el in doc["body"]["content"]:
        if "paragraph" not in el:
            continue
        for r in el["paragraph"].get("elements", []):
            if "textRun" in r:
                parts.append(r["textRun"].get("content", ""))
    return "".join(parts)


def main():
    creds = get_creds()
    docs = build("docs", "v1", credentials=creds)

    # Verify both target strings exist and are unique before sending updates.
    doc = docs.documents().get(documentId=DOC_ID).execute()
    text = doc_text(doc)
    for label, needle in [
        ("POINT3_OLD", POINT3_OLD),
        ("BODY_OLD", BODY_OLD),
    ]:
        count = text.count(needle)
        if count == 0:
            raise RuntimeError(
                f"{label} not found in doc. Refetch and inspect the placeholder text."
            )
        if count > 1:
            raise RuntimeError(
                f"{label} appears {count} times; aborting to avoid mass replace."
            )
        print(f"[verify] {label}: found exactly once.")

    requests = [
        {
            "replaceAllText": {
                "containsText": {"text": POINT3_OLD, "matchCase": True},
                "replaceText": POINT3_NEW,
            }
        },
        {
            "replaceAllText": {
                "containsText": {"text": BODY_OLD, "matchCase": True},
                "replaceText": BODY_NEW,
            }
        },
    ]
    resp = docs.documents().batchUpdate(
        documentId=DOC_ID, body={"requests": requests}
    ).execute()

    for i, reply in enumerate(resp.get("replies", []), 1):
        n = reply.get("replaceAllText", {}).get("occurrencesChanged", 0)
        print(f"[replace #{i}] occurrencesChanged = {n}")
    print("done.")


if __name__ == "__main__":
    main()
