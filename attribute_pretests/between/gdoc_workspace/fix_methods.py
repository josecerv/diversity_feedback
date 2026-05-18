"""Section 2 fixes:
  - Methods P1: drop em-dash, tighten Study 4A exclusion.
  - Methods P2: drop 'Following our pre-registration', make estimation concrete.
  - Methods P3: drop 'mini meta-analysis' / random-effects framing; describe the
    FE pool (study x attribute fixed effects, participant-clustered SEs) as in
    the response letter.
  - Results body: drop 'rejects equality' wording for the Wald test.
  - Notes: align with FE-pool description.

Stats are re-italicized after each replacement.
"""
import json, pathlib, re
from googleapiclient.discovery import build
from google.oauth2.credentials import Credentials
from google.auth.transport.requests import Request

DOC_ID = "1svt9mnSQhI8ieCagC2ezGA_ni1SDpV-JTP-_698bUVg"
TOKEN  = pathlib.Path(r"C:/Users/jcerv/.config/gws/token.json")

# --- new prose ---

METHODS_P1_NEW = (
    "We pooled participant-level data from Studies 2, 3A, 3B, and 4B. We "
    "excluded Study 4A because its non-race comparison attributes were "
    "sufficiently common in the candidate pool that very few participants "
    "made initial selections containing zero options with any of them; this "
    "left fewer than approximately 20 zero-initial participants per "
    "comparison cell, too few to support a stable estimate."
)

METHODS_P2_NEW = (
    "For each combination of a study and a focal attribute (e.g., Study 3A "
    "and films released after 2010, or Study 4B and books by women authors), "
    "we restricted the sample to participants whose initial selections "
    "contained zero options with that attribute. Within each of these "
    "study-by-attribute cells, we estimated an ordinary least squares (OLS) "
    "regression with HC3-robust standard errors to test whether random "
    "assignment to receive descriptive feedback about that focal attribute "
    "increased the rate at which participants subsequently selected an "
    "option with that attribute; the resulting per-cell estimates appear in "
    "Table B1."
)

METHODS_P3_NEW = (
    "To obtain a single pooled estimate of feedback’s effect for "
    "race/gender focal attributes and a single pooled estimate for "
    "non-race/gender focal attributes, we stacked all participant-cell "
    "observations and ran a long-format OLS regression with study × "
    "attribute fixed effects and standard errors clustered at the "
    "participant level. We then used a Wald test to compare the pooled "
    "race/gender coefficient against the pooled non-race/gender coefficient."
)

RESULTS_NEW = (
    "Among participants who initially selected zero options with the focal "
    "attribute, random assignment to receive descriptive feedback about race "
    "or gender significantly increased the likelihood that the next selection "
    "had that attribute by 22.50 percentage points on average (women cells: "
    "+27.59 pp; racial-minority cell: +9.77 pp; p < .001, N = 864 "
    "participant-cell observations across Studies 2, 3A, 3B, and 4B). Among "
    "participants who initially selected zero options with a non-race/gender "
    "focal attribute, descriptive feedback about that attribute did not "
    "significantly shift subsequent selections (pooled estimate: +7.25 pp, "
    "p = .19, N = 414 participant-cell observations). A Wald test confirms "
    "that the gap between the two pooled estimates is statistically "
    "significant (Δ = +15.25 pp, p = .016, 95% CI [+2.87, +27.63]). "
    "Per-cell estimates appear in Table B1."
)

NOTES_NEW = (
    "Each row reports the estimated effect of random assignment to receive "
    "descriptive feedback about the focal attribute on the likelihood that "
    "the participant’s subsequent selection exhibited that attribute, "
    "estimated from an ordinary least squares (OLS) regression with "
    "HC3-robust standard errors. Estimates are reported in percentage points "
    "(pp). Within each study, rows are ordered with the focal race/gender "
    "attribute appearing last and shown in bold. The sample is restricted "
    "within each cell to participants whose initial selections contained no "
    "options exhibiting the focal attribute. Cells with fewer than "
    "approximately 20 zero-initial participants are omitted. Study 4A is "
    "excluded because its non-race comparison attributes were sufficiently "
    "common in the candidate pool that very few participants made initial "
    "selections containing zero options with any of them (see Methods). "
    "The pooled estimate for race/gender attributes (+22.50 pp; women cells: "
    "+27.59 pp; racial-minority cell: +9.77 pp; N = 864) and the pooled "
    "estimate for non-race/gender attributes (+7.25 pp; N = 414) come from a "
    "single OLS model on the stacked participant-cell observations with "
    "study × attribute fixed effects and standard errors clustered at the "
    "participant level; the final row reports a Wald test of the difference "
    "between the two pooled coefficients in that model. + p < .10, "
    "* p < .05, ** p < .01, *** p < .001."
)

# --- helpers ---

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
    """Delete the body of the paragraph after prefix_len and insert new_text."""
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

    # JC already deleted the original per-cell paragraph and updated P1, so
    # we replace the surviving 'To summarize...' paragraph with the combined
    # P2 (per-cell estimation) + P3 (FE pool) content.
    print("Methods P1 (idempotent re-run)...")
    replace_paragraph(docs, "We pooled participant-level data", METHODS_P1_NEW, 0)
    print("Methods P2 + P3 (combined replace into 2 paragraphs)...")
    replace_paragraph(docs, "To summarize the overall pattern across cells",
                      METHODS_P2_NEW + "\n" + METHODS_P3_NEW, 0)
    print("Results body...")
    replace_paragraph(docs, "Among participants who initially selected zero", RESULTS_NEW, 0)
    print("Notes body...")
    replace_paragraph(docs, "Notes.", NOTES_NEW, prefix_len=len("Notes. "))
    print("Done.")


if __name__ == "__main__":
    main()
