"""Revert Discussion to the 2-sentence 'rules out / empirical pattern' version
— no underpowered hedge.
"""
import json, pathlib
from googleapiclient.discovery import build
from google.oauth2.credentials import Credentials
from google.auth.transport.requests import Request

DOC_ID = "1svt9mnSQhI8ieCagC2ezGA_ni1SDpV-JTP-_698bUVg"
TOKEN  = pathlib.Path(r"C:/Users/jcerv/.config/gws/token.json")

DISCUSSION_NEW = (
    "This analysis rules out an alternative explanation arguing that race "
    "or gender feedback shifts subsequent selections more than feedback "
    "about other attributes only because participants already use those "
    "other attributes appropriately in their initial selections. Within "
    "the subset of participants whose initial selections contained zero of "
    "a given attribute (a clear signal of underrepresentation on that "
    "dimension), feedback about the race or gender of past selectees still "
    "significantly increased subsequent selections of women or racial "
    "minorities, whereas feedback about other attributes did not."
)


def get_docs():
    data = json.load(open(TOKEN))
    creds = Credentials.from_authorized_user_info(data, [
        "https://www.googleapis.com/auth/documents",
        "https://www.googleapis.com/auth/drive",
    ])
    if not creds.valid and creds.refresh_token:
        creds.refresh(Request())
    return build("docs", "v1", credentials=creds)


def find_para(doc, prefix):
    for el in doc["body"]["content"]:
        if "paragraph" not in el:
            continue
        text = "".join(r["textRun"].get("content", "")
                       for r in el["paragraph"]["elements"] if "textRun" in r)
        if text.startswith(prefix):
            return el, text
    raise SystemExit(f"Could not find paragraph starting with: {prefix!r}")


def main():
    docs = get_docs()
    doc = docs.documents().get(documentId=DOC_ID).execute()
    el, _ = find_para(doc, "This analysis provides suggestive evidence")
    body_start = el["startIndex"]
    body_end   = el["endIndex"] - 1
    print(f"revert discussion at [{body_start}, {body_end})")
    reqs = [
        {"deleteContentRange": {"range":
            {"startIndex": body_start, "endIndex": body_end}}},
        {"insertText": {"location": {"index": body_start}, "text": DISCUSSION_NEW}},
        {"updateTextStyle": {
            "range": {"startIndex": body_start,
                      "endIndex": body_start + len(DISCUSSION_NEW)},
            "textStyle": {"bold": False, "italic": False,
                          "weightedFontFamily": {"fontFamily": "Times New Roman"}},
            "fields": "bold,italic,weightedFontFamily",
        }},
    ]
    docs.documents().batchUpdate(documentId=DOC_ID, body={"requests": reqs}).execute()
    print("Done.")


if __name__ == "__main__":
    main()
