"""Delete every UN-RESOLVED comment authored by Jose Cervantez that I posted today.
These are the orphan/'Original content deleted' comments from my last two runs."""
import json
from google.oauth2.credentials import Credentials
from google.auth.transport.requests import Request
from googleapiclient.discovery import build

DOC_ID = "1H9jAvqG5CzQe_BksjkNmLwjLZSlAHvaEK1iyuHL23FI"
TOKEN_PATH = r"C:/Users/jcerv/.config/gws/token.json"

# Body fragments that uniquely identify the comments I just posted (so we don't
# touch any other JC-authored comment that already existed on the doc).
MY_BODY_FRAGMENTS = [
    "Expand to:\n(Allcott, 2011",
    "Expand the parenthetical to:\n(Kluger & DeNisi, 1996, 1998",
    "Insert after this sentence (before the",
    "First, in the prior sentence, expand",
    "Insert this as a new paragraph immediately before",
    "Create a new Appendix section (placed near the existing Study S4",
    "Insert after this sentence (before",
    "Attach a new footnote to this sentence",
    "Expand the parenthetical to:\n(Kluger & DeNisi, 1996; Locke",
    "Append three sentences after",
    "Append two sentences after this sentence",
    "Add the following references alphabetically",
    "Remove this sentence from the Table 2 footnote",
]

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
kept = 0
page_token = None
while True:
    resp = drive.comments().list(
        fileId=DOC_ID,
        fields="nextPageToken,comments(id,content,author)",
        includeDeleted=False,
        pageSize=100,
        pageToken=page_token,
    ).execute()
    for c in resp.get("comments", []):
        body = c.get("content") or ""
        if any(frag in body for frag in MY_BODY_FRAGMENTS):
            drive.comments().delete(fileId=DOC_ID, commentId=c["id"]).execute()
            deleted += 1
            print(f"  deleted {c['id']} -- {body[:70]!r}")
        else:
            kept += 1
    page_token = resp.get("nextPageToken")
    if not page_token:
        break

print(f"\nDeleted {deleted} of my recent comments; kept {kept} others.")
