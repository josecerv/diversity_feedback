"""Replace the zero-baseline footnote (kix.sbd52qc7gtyl) in the manuscript Gdoc.

The current footnote uses jargony "comparison-attribute feedback" language. JC
selected Option A from claude_final_audit reconciliation. This script does a
surgical replace via the Docs API.
"""
import json
import pathlib
import sys
import io

sys.stdout = io.TextIOWrapper(sys.stdout.buffer, encoding="utf-8")

from googleapiclient.discovery import build
from google.oauth2.credentials import Credentials
from google.auth.transport.requests import Request

DOC_ID = "1H9jAvqG5CzQe_BksjkNmLwjLZSlAHvaEK1iyuHL23FI"
FOOTNOTE_ID = "kix.sbd52qc7gtyl"
TOKEN = pathlib.Path(r"C:/Users/jcerv/.config/gws/token.json")

NEW_FOOTNOTE_TEXT = (
    "One concern is that feedback about other attributes shows smaller effects "
    "simply because participants are already using those attributes "
    "appropriately in their initial selections, leaving little room for feedback "
    "to move them further. To test this, we pooled Studies 2, 3A, 3B, and 4B "
    "and looked only at participants whose initial selections had included zero "
    "options with a given attribute (e.g., a participant in Study 3A whose "
    "seven initial films included no films released after 2010, or a "
    "participant in Study 4B whose six initial books included no women "
    "authors). Selecting zero of an attribute is a clear signal of "
    "underrepresentation on that dimension, and if feedback's effect simply "
    "reflected a reaction to underrepresentation, it should move these "
    "participants regardless of which attribute is involved. Even within this "
    "subset, however, feedback that initial selections included no women or "
    "racial minorities increased the likelihood of subsequently selecting one "
    "by 22.50 percentage points (women: +27.59 pp; racial minorities: +9.77 pp; "
    "p < .001, N = 864), whereas feedback that initial selections included none "
    "of any other attribute (e.g., budget, release year, profession) shifted "
    "subsequent selections by only 7.25 percentage points (p = .19, N = 414). "
    "A Wald test confirms the gap is significant (p = .016, 95% CI [+2.87, "
    "+27.63]). Per-cell results appear in Appendix Section S[X]."
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


def gather_footnote_runs(footnote):
    """Return a flat list of (startIndex, endIndex, text) runs for the footnote
    (skipping any runs that are part of a tracked deletion suggestion)."""
    runs = []
    for el in footnote.get("content", []):
        if "paragraph" not in el:
            continue
        for r in el["paragraph"].get("elements", []):
            if "textRun" not in r:
                continue
            tr = r["textRun"]
            # Skip text that is only in the doc because it's a pending deletion
            # (we want to replace based on currently-visible text, but operate
            # on real indices in the segment).
            runs.append(
                {
                    "start": r.get("startIndex", 0),
                    "end": r.get("endIndex", 0),
                    "text": tr.get("content", ""),
                    "is_deletion": bool(tr.get("suggestedDeletionIds")),
                    "is_insertion_suggestion": bool(
                        tr.get("suggestedInsertionIds")
                    ),
                }
            )
    return runs


def main():
    creds = get_creds()
    docs = build("docs", "v1", credentials=creds)

    # Fetch with suggestions inline so we see all current content.
    doc = docs.documents().get(
        documentId=DOC_ID,
        suggestionsViewMode="SUGGESTIONS_INLINE",
    ).execute()

    footnote = doc.get("footnotes", {}).get(FOOTNOTE_ID)
    if not footnote:
        print(f"ERROR: footnote {FOOTNOTE_ID} not found in doc.")
        sys.exit(1)

    runs = gather_footnote_runs(footnote)
    print(f"Footnote {FOOTNOTE_ID}: {len(runs)} text run(s)")
    for run in runs:
        flags = ""
        if run["is_deletion"]:
            flags += " [DEL]"
        if run["is_insertion_suggestion"]:
            flags += " [INS-SUGGESTION]"
        print(
            f"  [{run['start']:>6}, {run['end']:>6}){flags}  "
            f"{run['text'][:120]!r}"
        )

    if any(r["is_deletion"] or r["is_insertion_suggestion"] for r in runs):
        print()
        print(
            "WARNING: Footnote has pending suggestions; replacement will operate "
            "on accepted text only. Confirm before proceeding."
        )

    # Compute total range covered (smallest start, largest end).
    # For a clean replace: delete from min_start to max_end, insert new text at min_start.
    # We need to operate within the footnote's segment, not the body.
    starts = [r["start"] for r in runs]
    ends = [r["end"] for r in runs]
    min_start = min(starts)
    max_end = max(ends)
    # The trailing newline that separates paragraphs (or terminates the segment)
    # should be preserved. Cut deletion 1 char short of max_end so we keep the
    # trailing newline that Docs requires at end of a paragraph.
    delete_end = max_end - 1  # preserve final newline

    print()
    print(f"Plan: delete footnote segment [{min_start}, {delete_end}); "
          f"insert new text at {min_start} (within footnote segment).")

    location_kwargs = {
        "index": min_start,
        "segmentId": FOOTNOTE_ID,
    }
    range_kwargs = {
        "startIndex": min_start,
        "endIndex": delete_end,
        "segmentId": FOOTNOTE_ID,
    }

    requests = [
        # Delete first (with higher endIndex), then insert, to keep indices stable.
        {"deleteContentRange": {"range": range_kwargs}},
        {"insertText": {"location": location_kwargs, "text": NEW_FOOTNOTE_TEXT}},
    ]

    print()
    print("Applying batchUpdate...")
    result = docs.documents().batchUpdate(
        documentId=DOC_ID,
        body={"requests": requests},
    ).execute()
    print(f"  Got {len(result.get('replies', []))} replies; no errors raised.")

    # Verify
    doc2 = docs.documents().get(
        documentId=DOC_ID,
        suggestionsViewMode="SUGGESTIONS_INLINE",
    ).execute()
    fn2 = doc2.get("footnotes", {}).get(FOOTNOTE_ID, {})
    body = []
    for el in fn2.get("content", []):
        if "paragraph" in el:
            for r in el["paragraph"].get("elements", []):
                if "textRun" in r:
                    body.append(r["textRun"].get("content", ""))
    new_text = "".join(body).strip()
    print()
    print("--- Footnote text after edit ---")
    print(new_text[:1500])

    if (
        new_text.startswith("One concern is that feedback about other attributes")
        and "signal of underrepresentation" in new_text
        and "regardless of which attribute is involved" in new_text
    ):
        print()
        print("OK: footnote opens as expected and includes the underrepresentation framing.")
    else:
        print()
        print("WARN: footnote does not start as expected; inspect manually.")


if __name__ == "__main__":
    main()
