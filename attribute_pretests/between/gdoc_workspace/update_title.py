"""Swap Section 2 title to JC's chosen version."""
import json, pathlib
from googleapiclient.discovery import build
from google.oauth2.credentials import Credentials
from google.auth.transport.requests import Request

DOC_ID = "1svt9mnSQhI8ieCagC2ezGA_ni1SDpV-JTP-_698bUVg"
TOKEN  = pathlib.Path(r"C:/Users/jcerv/.config/gws/token.json")

OLD_TITLE = "Ruling Out an Alternative Explanation: Initial Selection Calibration"
NEW_TITLE = "Cross-Study Pooled Analysis of Participants Whose Initial Selections Contain Zero of the Focal Attribute"


def get_docs():
    data = json.load(open(TOKEN))
    creds = Credentials.from_authorized_user_info(data, [
        "https://www.googleapis.com/auth/documents",
        "https://www.googleapis.com/auth/drive",
    ])
    if not creds.valid and creds.refresh_token:
        creds.refresh(Request())
    return build("docs", "v1", credentials=creds)


def main():
    docs = get_docs()
    doc = docs.documents().get(documentId=DOC_ID).execute()
    target = None
    for el in doc["body"]["content"]:
        if "paragraph" not in el:
            continue
        text = "".join(r["textRun"].get("content", "")
                       for r in el["paragraph"]["elements"] if "textRun" in r)
        if text.startswith(OLD_TITLE):
            target = el
            break
    if target is None:
        raise SystemExit("Could not find the old Section 2 title.")
    body_start = target["startIndex"]
    body_end   = target["endIndex"] - 1   # keep trailing \n
    print(f"Replacing title at [{body_start}, {body_end})")

    reqs = [
        {"deleteContentRange": {"range": {"startIndex": body_start, "endIndex": body_end}}},
        {"insertText": {"location": {"index": body_start}, "text": NEW_TITLE}},
        {"updateTextStyle": {
            "range": {"startIndex": body_start, "endIndex": body_start + len(NEW_TITLE)},
            "textStyle": {"bold": True, "italic": False,
                          "weightedFontFamily": {"fontFamily": "Times New Roman"}},
            "fields": "bold,italic,weightedFontFamily",
        }},
    ]
    docs.documents().batchUpdate(documentId=DOC_ID, body={"requests": reqs}).execute()
    print("Done.")


if __name__ == "__main__":
    main()
