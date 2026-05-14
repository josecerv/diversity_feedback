"""Fix-up pass:
- D1: panel of 6? + selection-context placeholder (anchor was wrong case).
- Clean pre-existing double period after the scale-anchor sentence.
"""
from __future__ import annotations

import io
import json
import pathlib
import sys

sys.stdout = io.TextIOWrapper(sys.stdout.buffer, encoding="utf-8")
sys.stderr = io.TextIOWrapper(sys.stderr.buffer, encoding="utf-8")

from googleapiclient.discovery import build
from google.oauth2.credentials import Credentials
from google.auth.transport.requests import Request


DOC_ID = "1YBHCMwD84xC3Qw38CPEzz1iv3JKg7Mw9jTJqyZGptkI"
TOKEN = pathlib.Path(r"C:/Users/jcerv/.config/gws/token.json")


EDITS = [
    ("D1 panel of 6? + selection context",
     "our Study 1 offers participants feedback about the gender, age, location and employment status of a panel of 6? NPR experts who they selected for [what was the selection about again? Remind reader!]",
     "our Study 1 offers participants feedback about the gender, age, location and employment status of a panel of 10 NPR experts who they selected to feature in upcoming NPR stories about the future of work"),

    ("Cleanup: double period after scale anchor",
     "to 7 = “Very important”. .",
     "to 7 = “Very important”."),
]


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
    doc = docs.documents().get(documentId=DOC_ID).execute()
    text = doc_text(doc)

    requests = []
    for label, old, new in EDITS:
        n = text.count(old)
        if n == 0:
            print(f"[SKIP] {label}: anchor not found")
            continue
        if n > 1:
            print(f"[SKIP] {label}: anchor appears {n} times")
            continue
        print(f"[OK]   {label}")
        requests.append({
            "replaceAllText": {
                "containsText": {"text": old, "matchCase": True},
                "replaceText": new,
            }
        })

    if not requests:
        return

    resp = docs.documents().batchUpdate(
        documentId=DOC_ID, body={"requests": requests}
    ).execute()
    for i, reply in enumerate(resp.get("replies", []), 1):
        n = reply.get("replaceAllText", {}).get("occurrencesChanged", 0)
        print(f"[replace #{i}] occurrencesChanged = {n}")
    print("done.")


if __name__ == "__main__":
    main()
