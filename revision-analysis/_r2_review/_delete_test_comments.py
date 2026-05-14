"""Delete the two [TEST] comments I posted to verify the anchor format."""
import json
from google.oauth2.credentials import Credentials
from google.auth.transport.requests import Request
from googleapiclient.discovery import build

DOC_ID = "1H9jAvqG5CzQe_BksjkNmLwjLZSlAHvaEK1iyuHL23FI"
TOKEN_PATH = r"C:/Users/jcerv/.config/gws/token.json"

with open(TOKEN_PATH) as f:
    tok = json.load(f)
creds = Credentials(
    token=tok.get("token"), refresh_token=tok["refresh_token"], token_uri=tok["token_uri"],
    client_id=tok["client_id"], client_secret=tok["client_secret"], scopes=tok["scopes"],
)
if not creds.valid:
    creds.refresh(Request())
drive = build("drive", "v3", credentials=creds)

deleted = 0
page_token = None
while True:
    resp = drive.comments().list(
        fileId=DOC_ID,
        fields="nextPageToken,comments(id,content)",
        includeDeleted=False,
        pageSize=100,
        pageToken=page_token,
    ).execute()
    for c in resp.get("comments", []):
        body = c.get("content") or ""
        if body.startswith("[TEST]") or body.startswith("[TEST 2]"):
            drive.comments().delete(fileId=DOC_ID, commentId=c["id"]).execute()
            deleted += 1
            print(f"  deleted {c['id']} -- {body[:60]!r}")
    page_token = resp.get("nextPageToken")
    if not page_token:
        break

print(f"\nDeleted {deleted} test comments.")
