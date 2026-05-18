"""Replace the RESPONSE paragraph for George's NPR-attribute post-test request
with the updated wording (pre-registered N=300 data, n-from-parens stripped,
aspredicted URL inline)."""
from __future__ import annotations

import json
import pathlib

from googleapiclient.discovery import build
from google.oauth2.credentials import Credentials
from google.auth.transport.requests import Request


DOC_ID = "1YBHCMwD84xC3Qw38CPEzz1iv3JKg7Mw9jTJqyZGptkI"
TOKEN = pathlib.Path(r"C:/Users/jcerv/.config/gws/token.json")


# EXACT current paragraph text in the doc, copied from the most recent
# fetch (curly quotes, U+2212 minus sign in the t-values).
OLD = (
    "RESPONSE: Thank you for this suggestion. We ran the post-test you proposed and "
    "incorporated the result into the Methods section of Study 1 (p. X). Specifically, "
    "in new pre-registered study ([link to AsPredicted]), we recruited 300 Prolific "
    "participants to rate the importance of the different attributes of experts "
    "selected to appear on NPR that we used in Study 1.  This study offers participants "
    "feedback about the gender, age, location and employment status of a panel of 10 "
    "NPR experts who they selected to feature in upcoming NPR stories about the future "
    "of work.  Our new study randomly assigned each participant to rate the importance "
    "of one of these four attributes of NPR experts (specifically: how many were women, "
    "how many worked at a university, how many were under 50 years old, or how many "
    "were based on the West Coast of the United States) on a scale from 1 = "
    "“Not at all important” to 7 = “Very important”. Two of the "
    "three non-gender attributes were rated as significantly more important than "
    "gender (“were women”: Mean Importance = 3.64, SD = 1.76, n = 76; "
    "“worked at a university”: Mean Importance = 4.67, SD = 1.60; "
    "t(148.8) = −3.76, p < .001, d = −0.61;  “were under 50 years old"
    "”: Mean Importance = 4.44, SD = 1.73; t(149.0) = −2.81, p = .006, "
    "d = −0.46). The third, “based on the West Coast of the United States,"
    "” did not differ significantly from gender in importance (Mean Importance = "
    "3.37, SD = 1.75; t(149.0) = 0.95, p = .343, d = +0.16). These results suggest "
    "that differences in the perceived importance of the type of feedback participants "
    "received about their selections in Study 1 cannot account for why only feedback "
    "about gender moved subsequent selections."
)

NEW = (
    "RESPONSE: Thank you for this suggestion. We ran the post-test you proposed and "
    "have incorporated the results into the Methods section of Study 1 (p. X). In a "
    "new pre-registered study (https://aspredicted.org/xa7u94.pdf), we recruited 300 "
    "Prolific participants and randomly assigned each to rate the importance of one "
    "of the four attributes featured in Study 1 (how many of the selected experts "
    "were women, worked at a university, were under 50 years old, or were based on "
    "the West Coast of the United States) for selecting NPR experts on a scale from "
    "1 = “Not at all important” to 7 = “Very important.” "
    "“Worked at a university” was rated as significantly more important "
    "than gender (“were women”: M = 3.87, SD = 1.92; "
    "“worked at a university”: M = 4.50, SD = 1.48; "
    "t(139.0) = 2.26, p = .026, d = 0.37). “Were under 50 years old” did "
    "not differ significantly from gender in rated importance (M = 4.05, SD = 1.74; "
    "t(146.6) = 0.62, p = .534, d = 0.10). “Were based on the West Coast of "
    "the United States” was rated as less important than gender (M = 3.11, "
    "SD = 1.81; t(146.7) = 2.48, p = .014, d = 0.41). Two of the three comparison "
    "attributes were thus rated as at least as important as gender. These results "
    "suggest that differences in the perceived importance of the type of feedback "
    "participants receive about their selections in Study 1 are unlikely to account "
    "for why only feedback about gender moved subsequent selections."
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


def main():
    creds = get_creds()
    docs = build("docs", "v1", credentials=creds)

    # Sanity check: the OLD text must appear exactly once in the doc.
    doc = docs.documents().get(documentId=DOC_ID).execute()
    full_text = ""
    def walk(elements):
        nonlocal full_text
        for el in elements:
            if "paragraph" in el:
                for r in el["paragraph"]["elements"]:
                    if "textRun" in r:
                        full_text += r["textRun"].get("content", "")
            elif "table" in el:
                for row in el["table"]["tableRows"]:
                    for cell in row["tableCells"]:
                        walk(cell.get("content", []))
    walk(doc["body"]["content"])

    occurrences = full_text.count(OLD)
    print(f"OLD text occurrences in doc: {occurrences}")
    if occurrences != 1:
        print("ABORT: OLD text must appear exactly once. Inspect text alignment.")
        # show a short diff hint
        head = OLD[:80]
        idx = full_text.find(head)
        if idx >= 0:
            print("Doc has matching prefix at idx", idx)
            print("Doc text starting there:")
            print(full_text[idx:idx+len(OLD)+50])
        else:
            print("Doc does not contain OLD prefix at all.")
        return 1

    # Single replaceAllText call (matchCase=True, single occurrence).
    req = {
        "replaceAllText": {
            "containsText": {"text": OLD, "matchCase": True},
            "replaceText": NEW,
        }
    }
    result = docs.documents().batchUpdate(
        documentId=DOC_ID, body={"requests": [req]}
    ).execute()
    print("batchUpdate result:")
    print(json.dumps(result, indent=2))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
