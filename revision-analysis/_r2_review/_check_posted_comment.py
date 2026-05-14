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

c = drive.comments().get(
    fileId=DOC_ID,
    commentId="AAAB6cVwyqo",
    fields="id,author,createdTime,resolved,anchor,quotedFileContent,content",
).execute()
print("anchor :", c.get("anchor"))
print("quoted :", (c.get("quotedFileContent") or {}).get("value", "")[:120])
print("body   :", c.get("content", "")[:200])
