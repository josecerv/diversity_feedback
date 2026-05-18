"""Section 2 Methods + Notes rewrite — second pass.

Drops jargon JC doesn't use ("HC3-robust standard errors" in body text;
"stacked all participant-cell observations", "long-format OLS"). Restores
"Our primary predictor was X" / "Our primary predictors were: (1)..."
framing per the manuscript voice (Studies 1-5 + appendix). The pool
paragraph mirrors the response-letter language about clustering rationale.
"""
import json, pathlib, re
from googleapiclient.discovery import build
from google.oauth2.credentials import Credentials
from google.auth.transport.requests import Request

DOC_ID = "1svt9mnSQhI8ieCagC2ezGA_ni1SDpV-JTP-_698bUVg"
TOKEN  = pathlib.Path(r"C:/Users/jcerv/.config/gws/token.json")

# --- new prose ---

METHODS_P2_NEW = (
    "For each combination of a study and a focal attribute (e.g., Study 3A "
    "and films released after 2010, or Study 4B and books by women authors), "
    "we restricted the sample to participants whose initial selections "
    "contained zero options with that attribute. Within each of these "
    "study-by-attribute cells, we ran an ordinary least squares (OLS) "
    "regression with robust standard errors to predict whether the "
    "participant’s subsequent selection had that attribute. Our primary "
    "predictor was an indicator for random assignment to receive descriptive "
    "feedback about that focal attribute. The resulting per-cell estimates "
    "appear in Table B1."
)

METHODS_P3_NEW = (
    "To estimate a single pooled effect of feedback for race or gender focal "
    "attributes and a single pooled effect for other focal attributes, we "
    "pooled all participant-cell observations across these studies and ran "
    "an OLS regression predicting whether the participant’s subsequent "
    "selection had the focal attribute. Our primary predictors were: (1) an "
    "indicator for random assignment to receive descriptive feedback about "
    "the focal attribute, (2) an indicator for whether the focal attribute "
    "was race or gender, and (3) the interaction between (1) and (2). We "
    "included study-by-attribute fixed effects and clustered standard errors "
    "by participant, because each respondent contributes multiple rows to "
    "this pooled dataset (one row for the race or gender feedback effect "
    "they were assigned to, plus one row for each comparison attribute "
    "featured in their study). We then used a Wald test to compare the "
    "pooled race or gender feedback effect against the pooled other-attribute "
    "feedback effect."
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
    "comparison attributes were sufficiently common in the candidate pool "
    "that very few participants made initial selections containing zero "
    "options with any of them (see Methods). The pooled estimate for "
    "race/gender attributes (+22.50 pp; women cells: +27.59 pp; "
    "racial-minority cell: +9.77 pp; N = 864) and the pooled estimate for "
    "other attributes (+7.25 pp; N = 414) come from a single OLS regression "
    "on the pooled participant-cell observations with study-by-attribute "
    "fixed effects and standard errors clustered by participant; the final "
    "row reports a Wald test of the difference between the two pooled "
    "coefficients in that model. + p < .10, * p < .05, ** p < .01, "
    "*** p < .001."
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


def replace_paragraph(docs, prefix, new_text, prefix_len=0):
    doc = docs.documents().get(documentId=DOC_ID).execute()
    el, text = find_para(doc, prefix)
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
        "textStyle": {"bold": False, "italic": False,
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
    print("Methods P2 (per-cell, restore 'Our primary predictor was...')...")
    replace_paragraph(docs, "For each combination of a study and a focal attribute", METHODS_P2_NEW, 0)
    print("Methods P3 (pool, restore 'Our primary predictors were: (1)...(2)...(3)')...")
    replace_paragraph(docs, "To obtain a single pooled estimate of feedback", METHODS_P3_NEW, 0)
    print("Notes body...")
    replace_paragraph(docs, "Notes.", NOTES_NEW, prefix_len=len("Notes. "))
    print("Done.")


if __name__ == "__main__":
    main()
