"""Push approved changes A-F to the R2 response-letter Gdoc.

A. Mechanical fixes (typo, missing period, en-dashes, callback, term).
B. Tone smoothing (lines 66, 70).
C. Line 44 placeholder fills (Study X / Z / 0 Y's / XX / X,Y,Z / missing comma).
D. NPR pretest paragraph fills on line 41 (panel of 10, selection context, scale anchors).
E. Numerical refresh on line 44 (22.50pp / 7.25pp / CI [+2.87, +27.63]).
F. Add 11 missing reference entries (web/news + missing academic) to the References list.

Uses replaceAllText (unique anchors) in one batchUpdate. Verifies every anchor is
unique before sending so a malformed match can't quietly mass-replace.
"""
from __future__ import annotations

import io
import json
import pathlib
import sys

sys.stdout = io.TextIOWrapper(sys.stdout.buffer, encoding="utf-8")
sys.stderr = io.TextIOWrapper(sys.stderr.buffer, encoding="utf-8")

from googleapiclient.discovery import build
from google.oauth2.credentials import Credentials
from google.auth.transport.requests import Request


DOC_ID = "1YBHCMwD84xC3Qw38CPEzz1iv3JKg7Mw9jTJqyZGptkI"
TOKEN = pathlib.Path(r"C:/Users/jcerv/.config/gws/token.json")


# Curly quotes / apostrophes per JC's style: U+2019 (’), U+201C (“), U+201D (”).
RSQ = "’"
LDQ = "“"
RDQ = "”"


# ===========================================================================
# A. Mechanical fixes
# ===========================================================================
EDITS: list[tuple[str, str, str]] = [
    # (label, target, replacement)
    ("A1 typo: handling our → handling of our",
     "for your thoughtful handling our revised manuscript",
     "for your thoughtful handling of our revised manuscript"),

    ("A2 missing period at end of opening paragraph",
     "we hope you will agree that in responding to each directive, we have further strengthened our paper",
     "we hope you will agree that in responding to each directive, we have further strengthened our paper."),

    ("A3 en-dash → commas (One pathway)",
     f"One pathway{chr(0x2013)}the external motivation to control prejudice{chr(0x2013)}should be driven",
     "One pathway, the external motivation to control prejudice, should be driven"),

    ("A4 en-dash → commas (The second pathway)",
     f"The second pathway{chr(0x2013)}internal motivation to control prejudice{chr(0x2013)}by contrast",
     "The second pathway, internal motivation to control prejudice, by contrast"),

    ("A5 implicit norm → implicit injunctive norm",
     "The implicit norm against prejudicial behavior is largely sustained",
     "The implicit injunctive norm against prejudicial behavior is largely sustained"),

    ("A6 remove callback on line 79",
     "As we note in the General Discussion, motivation to respond without prejudice is a key mechanism isolated in this paper",
     "Motivation to respond without prejudice is a key mechanism isolated in this paper"),

    # =======================================================================
    # B. Tone smoothing
    # =======================================================================
    ("B1 tone line 66 (figure 2 / reputation management)",
     "We are happy that you liked figure 2 and that our discussion surrounding reputation management resonated with you. We truly appreciate your thoughtful feedback throughout the review process.",
     "We appreciate your comments on Figure 2 and on our discussion of reputation management, and we are grateful for your thoughtful feedback throughout the review process."),

    ("B2 tone line 70 (kind words about improvement)",
     "Thank you for your kind words about how much you feel our manuscript has improved. We are grateful for your feedback, which we feel has helped us improve the paper immensely.",
     "Thank you for noting the improvements to the manuscript. We are grateful for your feedback, which has strengthened the paper."),

    # =======================================================================
    # C. Line 44 placeholder fills
    # =======================================================================
    ("C1+C2 Study X / Study Z fills (one combined anchor for the parenthetical)",
     f"(e.g., a set of films in Study X that included 0 Y{RSQ}s or a set of books in Study Z that included 0 Y{RSQ}s)",
     f"(e.g., a set of films in Study 3A that included 0 films released after 2010, or a set of books in Study 4B that included 0 books with women protagonists)"),

    ("C3 zero movies, books or XX",
     "zero movies, books or XX with any of the other attributes under study",
     "zero movies, books, or authors with any of the other attributes under study"),

    ("C4 X, Y or Z → concrete examples",
     "(e.g., X, Y or Z)",
     f"(e.g., budget, year of release, or protagonist{RSQ}s profession)"),

    ("C5 missing comma + XX fix",
     "selecting an additional movie book, or XX with the relevant attribute",
     "selecting an additional movie, book, or author with the relevant attribute"),

    # =======================================================================
    # D. NPR pretest paragraph fills on line 41
    # =======================================================================
    ("D1 panel of 6? → 10",
     "Our Study 1 offers participants feedback about the gender, age, location and employment status of a panel of 6? NPR experts who they selected for [what was the selection about again? Remind reader!]",
     "Our Study 1 offers participants feedback about the gender, age, location and employment status of a panel of 10 NPR experts who they selected to feature in upcoming NPR stories about the future of work"),

    ("D3 scale anchors XX → Not at all important / Very important",
     f"on a scale from 1 = XX to 7 = XX",
     f"on a scale from 1 = {LDQ}Not at all important{RDQ} to 7 = {LDQ}Very important{RDQ}"),

    # =======================================================================
    # E. Numerical refresh (no-Study-4A run)
    # =======================================================================
    ("E1 target estimate refresh",
     "by 21.28 percentage points (p < .001, N = 1,222).",
     "by 22.50 percentage points (p < .001, N = 864)."),

    ("E2 comparison estimate refresh",
     "(estimated effect =+6.83 percentage point; p = .21, N = 429)",
     "(estimated effect = +7.25 percentage points; p = .19, N = 414)"),

    ("E3 contrast CI refresh",
     "(p = .016, 95% CI [+2.70, +26.21])",
     "(p = .016, 95% CI [+2.87, +27.63])"),
]


# ===========================================================================
# F. Reference-list insertions
#
# Strategy: for each insertion point, anchor on the FIRST existing entry that
# should come AFTER the new ones. Replace that anchor with [new entries]\n[anchor].
# Each \n becomes a paragraph break in Gdocs.
# ===========================================================================

REF_BENTLEY = (
    "Bentley University & Gallup. (2025). Bentley-Gallup Business in Society "
    "Survey: 2025 report. Gallup. "
    "https://www.gallup.com/file/analytics/696014/Gallup-Bentley-University_Business-In-Society%20Survey_2025%20Report.pdf"
)
REF_CBS = (
    "CBS News. (2025, January 24). Costco shareholders reject an anti-DEI "
    "measure, after Walmart and others end diversity programs. "
    "https://www.cbsnews.com/news/costco-dei-policy-board-statement-shareholder-meeting-vote/"
)
REF_DIR_2022 = (
    "Directive (EU) 2022/2381 of the European Parliament and of the Council "
    "of 23 November 2022 on improving the gender balance among directors of "
    "listed companies and related measures. Official Journal of the European "
    "Union, L 315, 44-59. http://data.europa.eu/eli/dir/2022/2381/oj"
)
REF_DIR_2023 = (
    "Directive (EU) 2023/970 of the European Parliament and of the Council "
    "of 10 May 2023 to strengthen the application of the principle of equal "
    "pay for equal work or work of equal value between men and women through "
    "pay transparency and enforcement mechanisms. Official Journal of the "
    "European Union, L 132, 21-44. http://data.europa.eu/eli/dir/2023/970/oj"
)
REF_DUNGAN = (
    f"Dungan, R. (2025, March 31). Semantics: Has DEI actually gone to ground, "
    f"or has it just been rebranded? HR Grapevine. "
    f"https://www.hrgrapevine.com/us/content/article/2025-03-31-has-dei-really-gone-to-ground-or-has-it-just-assumed-a-new-identity"
)
REF_ELIAS = (
    "Elias, J., & Palmer, A. (2025, March 30). In Trump era, companies are "
    "rebranding DEI efforts, not giving up. CNBC. "
    "https://www.cnbc.com/2025/03/30/in-trump-era-companies-are-rebranding-dei-efforts-not-giving-up.html"
)
REF_MKT_2025 = (
    "Marketplace. (2025, May 22). Costco doubles down on DEI and benefits. "
    "Marketplace. "
    "https://www.marketplace.org/story/2025/05/22/costco-doubles-down-on-dei-and-benefits"
)
REF_MKT_2026 = (
    f"Marketplace. (2026). [Reporting on firm-level DEI rebranding toward "
    f"{LDQ}belonging,{RDQ} {LDQ}respect,{RDQ} {LDQ}fairness,{RDQ} and "
    f"{LDQ}inclusive experiences{RDQ} framings, including Nationwide and "
    f"UPS]. Marketplace. [URL pending verification]"
)
REF_MIGNANO = (
    f"Mignano, V. (2024). Gender Pay Gap: The Protection of the Right to "
    f"Equal Pay under the Pay Transparency Directive. Zeitschrift für "
    f"Europarechtliche Studien, 27(3), 371-401. "
    f"https://doi.org/10.5771/1435-439X-2024-3-371"
)
REF_PARADIGM = (
    "Paradigm. (2025). [Fortune 100 DEI/diversity language analysis report]. "
    "Paradigm. [URL pending verification]"
)
REF_RESSIA = (
    "Ressia, S. (2024). Addressing gender inequality issues in Australia: An "
    "annual review of developments. Journal of Industrial Relations, 66(5), "
    "742-758. https://doi.org/10.1177/00221856241295490"
)

# Anchor-based insertions. Each tuple: (label, anchor, new_entries_before_anchor)
# New entries are joined with single \n (paragraph break, matching existing format).
REF_INSERTIONS: list[tuple[str, str, list[str]]] = [
    ("F1 Bentley + CBS before Chen",
     "Chen, X., Latham, G. P., & Piccolo, R. F. (2020).",
     [REF_BENTLEY, REF_CBS]),

    ("F2 Directives + Dungan + Elias before Henry",
     "Henry, M. L., Ferraro, P. J., & Kontoleon, A. (2019).",
     [REF_DIR_2022, REF_DIR_2023, REF_DUNGAN, REF_ELIAS]),

    ("F3 Marketplace 2025 + 2026 before Mertens",
     "Mertens, S. N., & Schultz, P. W. (2021).",
     [REF_MKT_2025, REF_MKT_2026]),

    ("F4 Mignano + Paradigm + Ressia before Schultz",
     "Schultz, P. W., Nolan, J. M., & Cialdini, R. B. (2018).",
     [REF_MIGNANO, REF_PARADIGM, REF_RESSIA]),
]


def get_creds():
    data = json.load(open(TOKEN))
    creds = Credentials.from_authorized_user_info(
        data,
        [
            "https://www.googleapis.com/auth/documents",
            "https://www.googleapis.com/auth/drive",
        ],
    )
    if not creds.valid and creds.refresh_token:
        creds.refresh(Request())
    return creds


def doc_text(doc):
    parts = []
    for el in doc["body"]["content"]:
        if "paragraph" not in el:
            continue
        for r in el["paragraph"].get("elements", []):
            if "textRun" in r:
                parts.append(r["textRun"].get("content", ""))
    return "".join(parts)


def main():
    creds = get_creds()
    docs = build("docs", "v1", credentials=creds)

    doc = docs.documents().get(documentId=DOC_ID).execute()
    text = doc_text(doc)

    requests = []
    skipped = []

    # Verify A-E edits.
    for label, old, new in EDITS:
        n = text.count(old)
        if n == 0:
            skipped.append((label, "anchor missing"))
            print(f"[SKIP] {label}: anchor not found")
            continue
        if n > 1:
            skipped.append((label, f"anchor appears {n} times"))
            print(f"[SKIP] {label}: anchor appears {n} times (would mass-replace)")
            continue
        print(f"[OK]   {label}")
        requests.append({
            "replaceAllText": {
                "containsText": {"text": old, "matchCase": True},
                "replaceText": new,
            }
        })

    # F. Reference insertions.
    for label, anchor, new_entries in REF_INSERTIONS:
        n = text.count(anchor)
        if n == 0:
            skipped.append((label, "anchor missing"))
            print(f"[SKIP] {label}: anchor not found")
            continue
        if n > 1:
            skipped.append((label, f"anchor appears {n} times"))
            print(f"[SKIP] {label}: anchor appears {n} times")
            continue
        print(f"[OK]   {label}")
        # Each new entry becomes its own paragraph (\n in replaceText = paragraph break).
        replacement = "\n".join(new_entries) + "\n" + anchor
        requests.append({
            "replaceAllText": {
                "containsText": {"text": anchor, "matchCase": True},
                "replaceText": replacement,
            }
        })

    if not requests:
        print("\nNothing to do. All anchors missing or duplicated.")
        return

    print(f"\nSending {len(requests)} requests in one batchUpdate...")
    resp = docs.documents().batchUpdate(
        documentId=DOC_ID, body={"requests": requests}
    ).execute()

    total = 0
    for i, reply in enumerate(resp.get("replies", []), 1):
        n = reply.get("replaceAllText", {}).get("occurrencesChanged", 0)
        total += n
    print(f"Total occurrencesChanged: {total}")
    if skipped:
        print(f"\nSkipped {len(skipped)}:")
        for lbl, reason in skipped:
            print(f"  - {lbl}: {reason}")
    print("done.")


if __name__ == "__main__":
    main()
