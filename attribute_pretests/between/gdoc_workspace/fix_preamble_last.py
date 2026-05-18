"""Replace the preamble's last sentence with the response-letter phrasing:
'We propose this is a conservative signal of underrepresentation that should
provoke a reaction to feedback regardless of which attribute is involved.'
"""
import json, pathlib, re
from googleapiclient.discovery import build
from google.oauth2.credentials import Credentials
from google.auth.transport.requests import Request

DOC_ID = "1svt9mnSQhI8ieCagC2ezGA_ni1SDpV-JTP-_698bUVg"
TOKEN  = pathlib.Path(r"C:/Users/jcerv/.config/gws/token.json")

PREAMBLE_NEW = (
    "One alternative explanation for the differential effects of descriptive "
    "feedback observed across attributes is that feedback about other "
    "attributes shows smaller effects simply because participants already use "
    "those attributes appropriately in their initial selections, leaving "
    "little room for feedback to move them further. To test this possibility, "
    "we pooled participant-level data from Studies 2, 3A, 3B, and 4B and "
    "restricted the sample to participants whose initial selections contained "
    "zero films or books with a given attribute (e.g., a Study 3A participant "
    "whose seven initial films contained no films released after 2010, or a "
    "Study 4B participant whose six initial books contained no books by women "
    "authors). We propose this is a conservative signal of underrepresentation "
    "that should provoke a reaction to feedback regardless of which attribute "
    "is involved."
)

ITALIC_RE = [
    re.compile(r"(?<![A-Za-z])M(?=\s*=)"),
    re.compile(r"(?<![A-Za-z])SD(?=\s*=)"),
    re.compile(r"(?<![A-Za-z])t(?=\()"),
    re.compile(r"(?<![A-Za-z])p(?=\s*[<=>.])"),
    re.compile(r"(?<![A-Za-z])d(?=\s*=)"),
    re.compile(r"(?<![A-Za-z])N(?=\s*=)"),
    re.compile(r"Δ(?=\s*=)"),
]


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
    el, _ = find_para(doc, "One alternative explanation for the differential")
    body_start = el["startIndex"]
    body_end   = el["endIndex"] - 1
    print(f"replace preamble at [{body_start}, {body_end})")
    reqs = [
        {"deleteContentRange": {"range":
            {"startIndex": body_start, "endIndex": body_end}}},
        {"insertText": {"location": {"index": body_start}, "text": PREAMBLE_NEW}},
        {"updateTextStyle": {
            "range": {"startIndex": body_start, "endIndex": body_start + len(PREAMBLE_NEW)},
            "textStyle": {"bold": False, "italic": False,
                          "weightedFontFamily": {"fontFamily": "Times New Roman"}},
            "fields": "bold,italic,weightedFontFamily",
        }},
    ]
    for rx in ITALIC_RE:
        for m in rx.finditer(PREAMBLE_NEW):
            reqs.append({"updateTextStyle": {
                "range": {"startIndex": body_start + m.start(),
                          "endIndex":   body_start + m.end()},
                "textStyle": {"italic": True},
                "fields": "italic",
            }})
    docs.documents().batchUpdate(documentId=DOC_ID, body={"requests": reqs}).execute()
    print("Done.")


if __name__ == "__main__":
    main()
