"""Round 4 fixes (per JC):
  - Preamble: revert to the underrepresentation-signal framing (the
    'rules out X' line was supposed to go in the Discussion, not here).
    Drop 'restricted our attention' and 'one's'.
  - Methods P1: drastically simplify the Study 4A exclusion.
  - Notes: shorten the Study 4A line to match the simpler P1.
  - Discussion: rewrite as 2 sentences using
    'This analysis rules out an alternative explanation arguing X. Y.'
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
    "authors). Learning that your initial selections contained zero of a "
    "given attribute is itself a signal of underrepresentation on that "
    "dimension, so if descriptive feedback simply reflects a reaction to "
    "underrepresentation, it should move participants regardless of which "
    "attribute is involved."
)

METHODS_P1_NEW = (
    "We pooled participant-level data from Studies 2, 3A, 3B, and 4B. We "
    "excluded Study 4A because its non-race comparison attributes did not "
    "yield enough zero-initial participants to estimate per-cell effects."
)

NOTES_NEW = (
    "Each row reports the estimated effect of random assignment to receive "
    "descriptive feedback about the focal attribute on the likelihood that "
    "the participant’s subsequent selection exhibited that attribute, "
    "estimated from an ordinary least squares (OLS) regression. Estimates "
    "and standard errors are reported in percentage points (pp); standard "
    "errors are estimated robustly using HC3. Within each study, rows are "
    "ordered with the focal race/gender attribute appearing last and shown "
    "in bold. The sample is restricted within each cell to participants "
    "whose initial selections contained no options exhibiting the focal "
    "attribute. Cells with fewer than approximately 20 zero-initial "
    "participants are omitted. Study 4A is excluded because its non-race "
    "comparison attributes did not yield enough zero-initial participants "
    "to estimate per-cell effects (see Methods). The pooled estimate for "
    "race/gender attributes (+22.50 pp; women cells: +27.59 pp; "
    "racial-minority cell: +9.77 pp; N = 864) and the pooled estimate for "
    "other attributes (+7.25 pp; N = 414) come from a single OLS regression "
    "on the pooled participant-cell observations with study-by-attribute "
    "fixed effects and standard errors clustered by participant; the final "
    "row reports a Wald test of the difference between the two pooled "
    "coefficients in that model. + p < .10, * p < .05, ** p < .01, "
    "*** p < .001."
)

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
    print("Preamble (restore underrepresentation-signal framing)...")
    replace_paragraph(docs, "To address the concern that comparison attributes", PREAMBLE_NEW, 0)
    print("Methods P1 (simplified Study 4A exclusion)...")
    replace_paragraph(docs, "We pooled participant-level data from Studies 2", METHODS_P1_NEW, 0)
    print("Notes (shortened Study 4A line)...")
    replace_paragraph(docs, "Notes.", NOTES_NEW, prefix_len=len("Notes. "))
    print("Discussion (2 sentences: rules out X. empirical pattern.)...")
    replace_paragraph(docs, "This rule-out matters because the most plausible", DISCUSSION_NEW, 0)
    print("Done.")


if __name__ == "__main__":
    main()
