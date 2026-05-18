"""Rewrite the Section 2 Results paragraph in plainer language + re-italicize stats."""
import json, pathlib, re
from googleapiclient.discovery import build
from google.oauth2.credentials import Credentials
from google.auth.transport.requests import Request

DOC_ID = "1svt9mnSQhI8ieCagC2ezGA_ni1SDpV-JTP-_698bUVg"
TOKEN  = pathlib.Path(r"C:/Users/jcerv/.config/gws/token.json")

OLD_PREFIX = "Within the zero-initial subset, random assignment"
NEW_TEXT = (
    "Among participants who initially selected zero options with the focal "
    "attribute, random assignment to receive descriptive feedback about race "
    "or gender significantly increased the likelihood that the next selection "
    "had that attribute by 22.50 percentage points on average (women cells: "
    "+27.59 pp; racial-minority cell: +9.77 pp; p < .001, N = 864 "
    "participant-cell observations across Studies 2, 3A, 3B, and 4B). Among "
    "participants who initially selected zero options with a non-race/gender "
    "focal attribute, descriptive feedback about that attribute did not "
    "significantly shift subsequent selections (pooled estimate: +7.25 pp, "
    "p = .19, N = 414 participant-cell observations). A Wald test comparing "
    "the pooled race/gender estimate to the pooled non-race/gender estimate "
    "rejects equality (Δ = +15.25 pp, p = .016, 95% CI [+2.87, +27.63]). "
    "Per-cell estimates appear in Table B1."
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


def main():
    docs = get_docs()
    doc = docs.documents().get(documentId=DOC_ID).execute()
    target = None
    for el in doc["body"]["content"]:
        if "paragraph" not in el:
            continue
        text = "".join(r["textRun"].get("content", "")
                       for r in el["paragraph"]["elements"] if "textRun" in r)
        if text.startswith(OLD_PREFIX):
            target = el
            break
    if target is None:
        raise SystemExit("Could not find the Section 2 results paragraph.")

    body_start = target["startIndex"]
    body_end   = target["endIndex"] - 1   # keep trailing \n
    print(f"Replacing results body at [{body_start}, {body_end})")

    # Stage 1: delete + insert.
    docs.documents().batchUpdate(documentId=DOC_ID, body={"requests": [
        {"deleteContentRange": {"range": {"startIndex": body_start, "endIndex": body_end}}},
        {"insertText": {"location": {"index": body_start}, "text": NEW_TEXT}},
    ]}).execute()

    # Stage 2: reset to normal style then italicize stats.
    new_end = body_start + len(NEW_TEXT)
    style_reqs = [
        {"updateTextStyle": {
            "range": {"startIndex": body_start, "endIndex": new_end},
            "textStyle": {"bold": False, "italic": False,
                          "weightedFontFamily": {"fontFamily": "Times New Roman"}},
            "fields": "bold,italic,weightedFontFamily",
        }}
    ]
    for rx in ITALIC_RE:
        for m in rx.finditer(NEW_TEXT):
            s = body_start + m.start()
            e = body_start + m.end()
            style_reqs.append({
                "updateTextStyle": {
                    "range": {"startIndex": s, "endIndex": e},
                    "textStyle": {"italic": True},
                    "fields": "italic",
                }
            })

    docs.documents().batchUpdate(documentId=DOC_ID, body={"requests": style_reqs}).execute()
    print(f"Applied {len(style_reqs)} style requests.")


if __name__ == "__main__":
    main()
