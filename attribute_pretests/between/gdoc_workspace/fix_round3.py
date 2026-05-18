"""Round 3 fixes:
  - Preamble: 2-sentence structure ('this analysis rules out X because Y'),
    drop 'restricted our attention' and 'one's'.
  - Results body: use concrete language from the response letter
    ('zero women or racial minorities initially' etc.).
  - Rename Table B1 -> Table 1 (caption + in-text references).
  - Methods stays (JC said it's fine).
"""
import json, pathlib, re
from googleapiclient.discovery import build
from google.oauth2.credentials import Credentials
from google.auth.transport.requests import Request

DOC_ID = "1svt9mnSQhI8ieCagC2ezGA_ni1SDpV-JTP-_698bUVg"
TOKEN  = pathlib.Path(r"C:/Users/jcerv/.config/gws/token.json")

PREAMBLE_NEW = (
    "To address the concern that comparison attributes show smaller "
    "feedback effects simply because participants already use them "
    "appropriately in their initial selections, we pooled participant-level "
    "data from Studies 2, 3A, 3B, and 4B and restricted the sample to "
    "participants whose initial selections contained zero films or books "
    "with a given attribute (e.g., a Study 3A participant whose seven "
    "initial films contained no films released after 2010, or a Study 4B "
    "participant whose six initial books contained no books by women "
    "authors). This analysis rules out that alternative explanation, "
    "because initial selections containing zero of an attribute are a clear "
    "signal of underrepresentation on that dimension, so if feedback simply "
    "reacts to underrepresentation, it should move participants in this "
    "restricted sample regardless of which attribute is involved."
)

METHODS_P2_NEW = (
    "For each combination of a study and a focal attribute (e.g., Study 3A "
    "and films released after 2010, or Study 4B and books by women authors), "
    "we restricted the sample to participants whose initial selections "
    "contained zero options with that attribute. Within each of these "
    "study-by-attribute cells, we ran an ordinary least squares (OLS) "
    "regression with robust standard errors to estimate the effect of random "
    "assignment to receive descriptive feedback about that focal attribute "
    "on the likelihood that the participant’s subsequent selection had "
    "that attribute. The resulting per-cell estimates appear in Table 1."
)

RESULTS_NEW = (
    "Among participants who selected zero women or racial minorities "
    "initially, receiving feedback that they had selected no women or "
    "racial minorities significantly increased the likelihood of selecting "
    "a woman or racial minority as their final selection by 22.50 "
    "percentage points on average (women cells: +27.59 pp; racial-minority "
    "cell: +9.77 pp; p < .001, N = 864 participant-cell observations across "
    "Studies 2, 3A, 3B, and 4B). Among participants who initially selected "
    "zero options with any of the other attributes under study (e.g., a "
    "high budget, a recent release year, over 500 pages), feedback that "
    "none of their initial selectees had those attributes did not "
    "significantly shift subsequent selections (pooled estimate: +7.25 pp, "
    "p = .19, N = 414 participant-cell observations). A Wald test confirms "
    "that the gap between the two pooled estimates is statistically "
    "significant (Δ = +15.25 pp, p = .016, 95% CI [+2.87, +27.63]). "
    "Per-cell estimates appear in Table 1."
)

CAPTION_NEW = (
    "Table 1. Ordinary Least Squares (OLS) Regressions Predicting Whether "
    "a Participant’s Subsequent Selection Exhibited the Focal Attribute, "
    "Restricted to Participants Whose Initial Selections Contained No "
    "Options Exhibiting That Attribute"
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


def replace_paragraph(docs, prefix, new_text, prefix_len=0, want_bold=False):
    doc = docs.documents().get(documentId=DOC_ID).execute()
    el, _ = find_para(doc, prefix)
    body_start = el["startIndex"] + prefix_len
    body_end   = el["endIndex"] - 1
    print(f"  replace {prefix[:40]!r} at [{body_start}, {body_end})")
    reqs = []
    if body_end > body_start:
        reqs.append({"deleteContentRange": {"range":
            {"startIndex": body_start, "endIndex": body_end}}})
    reqs.append({"insertText": {"location": {"index": body_start}, "text": new_text}})
    new_end = body_start + len(new_text)
    reqs.append({"updateTextStyle": {
        "range": {"startIndex": body_start, "endIndex": new_end},
        "textStyle": {"bold": want_bold, "italic": False,
                      "weightedFontFamily": {"fontFamily": "Times New Roman"}},
        "fields": "bold,italic,weightedFontFamily",
    }})
    for rx in ITALIC_RE:
        for m in rx.finditer(new_text):
            reqs.append({"updateTextStyle": {
                "range": {"startIndex": body_start + m.start(),
                          "endIndex":   body_start + m.end()},
                "textStyle": {"italic": True},
                "fields": "italic",
            }})
    docs.documents().batchUpdate(documentId=DOC_ID, body={"requests": reqs}).execute()


def main():
    docs = get_docs()
    print("Preamble (concise 2-sentence rewrite)...")
    replace_paragraph(docs, "One alternative explanation for the differential", PREAMBLE_NEW, 0)
    print("Methods P2 (Table B1 -> Table 1)...")
    replace_paragraph(docs, "For each combination of a study and a focal attribute", METHODS_P2_NEW, 0)
    print("Results body (concrete language)...")
    replace_paragraph(docs, "Among participants who initially selected zero", RESULTS_NEW, 0)
    print("Caption (Table B1 -> Table 1)...")
    replace_paragraph(docs, "Table B1.", CAPTION_NEW, 0, want_bold=True)
    print("Done.")


if __name__ == "__main__":
    main()
