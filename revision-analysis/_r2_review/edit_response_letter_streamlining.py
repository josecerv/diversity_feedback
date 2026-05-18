"""Direct edit to the response letter Gdoc: soften 'each Results section' to
'the later Results sections.' Per JC: no tracked changes needed on this doc.

Source of truth path (per reference memory): doc id below, single tab,
token at C:/Users/jcerv/.config/gws/token.json.
"""
import json
from pathlib import Path

from googleapiclient.discovery import build
from google.oauth2.credentials import Credentials
from google.auth.transport.requests import Request


DOC_ID = "1YBHCMwD84xC3Qw38CPEzz1iv3JKg7Mw9jTJqyZGptkI"
TOKEN = Path(r"C:/Users/jcerv/.config/gws/token.json")

OLD = "with explicit references from each Results section"
NEW = "with explicit references from the later Results sections"


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
    res = docs.documents().batchUpdate(
        documentId=DOC_ID,
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
    main()
