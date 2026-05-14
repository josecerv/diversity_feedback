import json
from google.oauth2.credentials import Credentials
from google.auth.transport.requests import Request
from googleapiclient.discovery import build

DOC_ID = "1H9jAvqG5CzQe_BksjkNmLwjLZSlAHvaEK1iyuHL23FI"
TOKEN_PATH = r"C:/Users/jcerv/.config/gws/token.json"

with open(TOKEN_PATH) as f:
    tok = json.load(f)
creds = Credentials(
    token=tok.get("token"),
    refresh_token=tok["refresh_token"],
    token_uri=tok["token_uri"],
    client_id=tok["client_id"],
    client_secret=tok["client_secret"],
    scopes=tok["scopes"],
)
if not creds.valid:
    creds.refresh(Request())

drive = build("drive", "v3", credentials=creds)

all_comments = []
page_token = None
while True:
    resp = drive.comments().list(
        fileId=DOC_ID,
        fields="nextPageToken,comments(id,author,createdTime,resolved,deleted,anchor,quotedFileContent,content)",
        includeDeleted=False,
        pageSize=100,
        pageToken=page_token,
    ).execute()
    all_comments.extend(resp.get("comments", []))
    page_token = resp.get("nextPageToken")
    if not page_token:
        break

print(f"Active comments on manuscript Gdoc: {len(all_comments)}")
for i, c in enumerate(all_comments):
    author = c.get("author", {}).get("displayName", "?")
    quote = (c.get("quotedFileContent") or {}).get("value", "")[:70].replace("\n", " ")
    body = (c.get("content") or "")[:80].replace("\n", " ")
    is_v3 = "[v3-plan " in (c.get("content") or "")
    tag = "  V3-PLAN" if is_v3 else ""
    print(f"  [{i}] {author}  {tag}")
    print(f"      anchor : {quote!r}")
    print(f"      body   : {body!r}")
