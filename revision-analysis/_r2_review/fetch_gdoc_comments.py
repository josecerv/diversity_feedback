"""Fetch open comments on the R2 response-letter Gdoc.

Comments in Gdocs live in Drive (not in the document body), so we use the
Drive v3 comments.list endpoint. The Drive scope is already granted in the
existing token at C:/Users/jcerv/.config/gws/token.json.
"""
import io
import json
import pathlib
import sys

# Force UTF-8 stdout so non-Latin chars in comments don't crash on Windows.
sys.stdout = io.TextIOWrapper(sys.stdout.buffer, encoding="utf-8")

from googleapiclient.discovery import build
from google.oauth2.credentials import Credentials
from google.auth.transport.requests import Request


DOC_ID = "1YBHCMwD84xC3Qw38CPEzz1iv3JKg7Mw9jTJqyZGptkI"
TOKEN = pathlib.Path(r"C:/Users/jcerv/.config/gws/token.json")


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
    drive = build("drive", "v3", credentials=creds)

    # Fields: ask for everything useful. quotedFileContent gives the text the
    # comment is anchored to, which is essential for understanding what each
    # comment is about.
    fields = "*"

    page_token = None
    all_comments = []
    while True:
        resp = (
            drive.comments()
            .list(
                fileId=DOC_ID,
                fields=fields,
                pageToken=page_token,
                pageSize=100,
                includeDeleted=False,
            )
            .execute()
        )
        all_comments.extend(resp.get("comments", []))
        page_token = resp.get("nextPageToken")
        if not page_token:
            break

    if "--json" in sys.argv:
        print(json.dumps(all_comments, indent=2, ensure_ascii=False))
        return

    # Human-friendly rendering: one comment per block, with anchor text and
    # any replies. Filter to unresolved comments only unless --all is passed.
    show_resolved = "--all" in sys.argv
    for i, c in enumerate(all_comments, 1):
        if c.get("resolved") and not show_resolved:
            continue
        print(f"=== Comment {i} ===")
        author = (c.get("author") or {}).get("displayName", "?")
        print(f"Author : {author}")
        print(f"Created: {c.get('createdTime')}")
        print(f"Resolved: {c.get('resolved')}")
        quoted = (c.get("quotedFileContent") or {}).get("value", "")
        if quoted:
            preview = quoted.strip().replace("\n", " ")
            if len(preview) > 240:
                preview = preview[:240] + "..."
            print(f"Anchor : {preview!r}")
        print(f"Body   : {c.get('content', '').strip()}")
        for r in c.get("replies", []):
            r_author = (r.get("author") or {}).get("displayName", "?")
            r_body = r.get("content", "").strip()
            r_resolved = r.get("resolved")
            tag = " [resolves]" if r_resolved else ""
            print(f"  -> reply by {r_author}{tag}: {r_body}")
        print()


if __name__ == "__main__":
    main()
