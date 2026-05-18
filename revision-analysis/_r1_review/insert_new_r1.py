"""Replace the old R1 response block with the new zero-benchmark version.

Stages (each is a separate batchUpdate so we can stop and inspect between):

  A. Delete old block [12298, 17009] -- response paragraphs, Table R1 caption,
     18x6 table, Notes paragraph. Deletes issued in DESCENDING order so each
     uses original indices.
  B. Insert the new prose (4 paragraphs) + Table R1 caption.
  C. Insert empty 3-col x 8-row table at end of caption.
  D. Populate cells (re-fetch first to find the new cell indices).
  E. Insert Notes paragraph after the table.

Bold / italic / footnote-marker styling is *not* applied -- per JC, "I'll make
my edits myself." Goal here is to land the prose + table structure cleanly so
he can polish.

Pre-run: confirm DOC_ID and TOKEN_PATH; run `python dump_response_letter.py`
to refresh response_letter.json/.txt for verification.
"""
from __future__ import annotations

import io
import json
import sys
from pathlib import Path

sys.stdout = io.TextIOWrapper(sys.stdout.buffer, encoding="utf-8")

from google.oauth2.credentials import Credentials
from google.auth.transport.requests import Request
from googleapiclient.discovery import build

DOC_ID = "1YBHCMwD84xC3Qw38CPEzz1iv3JKg7Mw9jTJqyZGptkI"
TOKEN_PATH = r"C:/Users/jcerv/.config/gws/token.json"

with open(TOKEN_PATH) as f:
    tok = json.load(f)
creds = Credentials(
    token=tok.get("token"),
    refresh_token=tok["refresh_token"],
    token_uri=tok["token_uri"],
    client_id=tok["client_id"],
    client_secret=tok["client_secret"],
    scopes=tok["scopes"],
)
if not creds.valid:
    creds.refresh(Request())

docs = build("docs", "v1", credentials=creds)


def execute(requests, label):
    print(f"\n=== {label} ({len(requests)} request(s)) ===")
    result = docs.documents().batchUpdate(
        documentId=DOC_ID, body={"requests": requests}
    ).execute()
    print("OK")
    return result


def fetch_doc():
    return docs.documents().get(
        documentId=DOC_ID,
        suggestionsViewMode="DEFAULT_FOR_CURRENT_ACCESS",
    ).execute()


# ----------------------------------------------------------------------------
# Curly punctuation per JC preference (no em/en dashes; curly quotes)
# ----------------------------------------------------------------------------
LSQUO, RSQUO = "‘", "’"   # ' '
LDQUO, RDQUO = "“", "”"   # " "
MINUS        = "−"             # minus sign for negative numbers

NEW_PROSE_PARAS = [
    # ¶1 -- opener planting the manuscript addition
    f"RESPONSE: Thank you for raising this alternative. We agree it is worth "
    f"testing directly, and we report the follow-up analysis below (Table R1). "
    f"We have also added a footnote to the Discussion of Study 4B (p. X) that "
    f"summarizes this evidence and makes the underlying theoretical claim more "
    f"explicit in the manuscript.",
    # ¶2 -- theory in one paragraph, ties to existing manuscript framing
    f"In our studies we assume that participants do not enter the task with a "
    f"normative or {LDQUO}appropriate{RDQUO} level ex ante for any attribute in "
    f"the absence of explicit goals or prescriptions; we anticipate that the "
    f"activation of a representational concern occurs only after participants "
    f"receive descriptive feedback, and only for race and gender. This is "
    f"consistent with the theoretical framing already in the paper: descriptive "
    f"feedback shifts behavior on a given dimension only when that dimension "
    f"carries an implicit injunctive norm against unequal treatment, which we "
    f"have argued holds for race and gender but not for the comparison "
    f"attributes we test (see Introduction, p. X, and General Discussion, p. X).",
    # ¶3 -- empirical move, results in running prose, Wald test phrasing
    f"That said, we can examine directly what happens when feedback flags zero "
    f"representation of the focal attribute. Among participants whose initial "
    f"selection contained zero of an attribute, the inference that the "
    f"attribute is {LDQUO}missing{RDQUO} from the initial set could itself "
    f"produce some directional motivation to select one of those candidates "
    f"next, independent of any injunctive norm; if our race/gender feedback "
    f"effects were driven by this inference rather than by an underlying norm, "
    f"we would expect comparable feedback effects on the comparison attributes "
    f"among the subset of participants who picked zero of them initially. We "
    f"do not observe that. Pooling participants across Studies 2, 3A, 3B, 4A, "
    f"and 4B in a study-by-attribute fixed-effects regression with "
    f"participant-clustered standard errors, restricted to the "
    f"participant-attribute observations in which the initial selection "
    f"contained zero of the focal attribute, race/gender feedback increases "
    f"the probability of selecting a candidate with the focal attribute by "
    f"+21.28 percentage points (p < .0001, N = 1,222 across all five studies), "
    f"while comparison-attribute feedback increases that probability by only "
    f"+6.83 percentage points, which is not statistically distinguishable from "
    f"zero (p = .21, N = 429 across six attribute-by-study cells; the "
    f"remaining nine cells are omitted because the comparison attribute is "
    f"common in its candidate pool and almost no participant picked zero of it "
    f"initially). A Wald test of the difference between the two pooled "
    f"coefficients yields +14.46 percentage points (p = .016, 95% CI [+2.70, "
    f"+26.21]).",
    # ¶4 -- walk reader through the table columns + caveat
    f"Table R1 reports each per-cell estimate together with the pooled "
    f"coefficients and the Wald-test difference. The first column lists each "
    f"study and its total sample size; the second column reports the race or "
    f"gender (target) feedback effect among participants whose initial "
    f"selection contained zero of the target attribute; the third column "
    f"reports the analogous effect for each estimable comparison attribute in "
    f"that study. The bottom two rows give the pooled fixed-effects "
    f"coefficients for target and comparison feedback, respectively, and the "
    f"final row reports the Wald-test difference between them. We acknowledge "
    f"that the comparison side is estimable for only six of the fifteen "
    f"comparison attribute-by-study cells, and we therefore present this as a "
    f"directional, lower-powered test. The new survey described below "
    f"complements this behavioral evidence by asking participants directly "
    f"about the appropriateness of different attribute mixes.",
]

TABLE_CAPTION = (
    f"Table R1. Effect of Feedback on the Final Selection, Among Participants "
    f"Whose Initial Selection Contained Zero of the Focal Attribute."
)

# Table cells -- 8 rows x 3 cols
TABLE_CELLS = [
    # Header
    ["Study (Total N)",
     "Target Feedback Effect when Initial = 0",
     "Comparison Feedback Effects when Initial = 0"],
    # Study 2
    ["Study 2 (N = 302)",
     "Women: +32.66** (N = 63)",
     f"Featured Entertainer: +21.30+ (N = 66); Over 500 Pages: {MINUS}1.79 (N = 144)"],
    # Study 3A
    ["Study 3A (N = 1,000)",
     "Racial Minority: +9.77+ (N = 247)",
     "High Budget: +33.33 (N = 40)"],
    # Study 3B
    ["Study 3B (N = 1,000)",
     "Women: +31.57*** (N = 336)",
     f"High Budget: {MINUS}9.52 (N = 61); Political Leader: +9.77 (N = 52)"],
    # Study 4A
    ["Study 4A (N = 1,000)",
     "Racial Minority: +18.37*** (N = 358)",
     f"{MINUS}"],
    # Study 4B
    ["Study 4B (N = 1,000)",
     "Women: +20.01** (N = 218)",
     "Sold 30M+ Copies: +27.54 (N = 29)"],
    # Pooled
    ["Pooled (study-by-attribute FE; participant-clustered SE)",
     "+21.28*** (N = 1,222) [+16.15, +26.42]",
     f"+6.83 (N = 429) [{MINUS}3.81, +17.46]"],
    # Wald contrast (put value in col 2; col 3 empty -- JC can merge cells manually)
    [f"Target {MINUS} Comparison difference (Wald test)",
     "+14.46* (N = 1,651) [+2.70, +26.21]",
     ""],
]

TABLE_NOTES = (
    f"Notes. This table reports the OLS treatment effect of receiving feedback "
    f"about the focal attribute on the rate of selecting a candidate with that "
    f"attribute on the participant{RSQUO}s final selection, restricted to "
    f"participants whose initial selection contained zero of the focal "
    f"attribute. Per-cell estimates use HC3 robust standard errors. The pooled "
    f"rows come from a single OLS model on the stacked participant-attribute "
    f"observations with study-by-attribute fixed effects and standard errors "
    f"clustered by participant; the bottom row is a Wald test of the "
    f"difference between the pooled target and pooled comparison coefficients "
    f"in that model. Target cells are the race or gender feedback condition "
    f"for each study, depending on the study{RSQUO}s manipulation. Of the "
    f"fifteen comparison attribute-by-study cells, six are reported here; the "
    f"remaining nine are omitted because the comparison attribute is common in "
    f"its candidate pool, leaving fewer than twenty participants with zero of "
    f"the attribute in their initial selection. Studies 1 and 5 are excluded "
    f"because their initial portfolios are not participant-generated in a way "
    f"that defines a per-participant zero-initial cell. Confidence intervals "
    f"are 95%. +p < .10; *p < .05; **p < .01; ***p < .001."
)


# ============================================================================
# Stage A -- delete old block
#
# Indices are looked up live from the current document so we don't trip over
# upstream edits that have shifted the doc.  The doc API doesn't allow
# deleteContentRange to "exactly match" a table's range -- but it does allow
# a single range that spans from outside the table through the table and into
# the next paragraph (both endpoints in non-table content). We use one
# spanning delete from the start of the RESPONSE opener through the start of
# the empty paragraph immediately after the Notes line.
# ============================================================================
def find_block_indices():
    doc = fetch_doc()
    elements = doc["body"]["content"]
    resp_start = None
    notes_end = None
    next_para_start = None
    for i, el in enumerate(elements):
        if "paragraph" not in el:
            continue
        txt = "".join(r.get("textRun", {}).get("content", "")
                      for r in el["paragraph"].get("elements", []))
        if resp_start is None and txt.startswith(
                "RESPONSE: Thank you for raising this alternative"):
            resp_start = el["startIndex"]
        if resp_start is not None and txt.startswith("Notes: This table reports"):
            notes_end = el["endIndex"]
            # next para after Notes
            if i + 1 < len(elements):
                next_para_start = elements[i + 1].get("startIndex")
            break
    if resp_start is None or notes_end is None:
        raise RuntimeError("Could not locate RESPONSE opener or Notes paragraph.")
    return resp_start, notes_end, next_para_start

ORIG_START, NOTES_END, NEXT_PARA_START = find_block_indices()
print(f"Block to delete: RESPONSE opener at {ORIG_START}, "
      f"Notes ends at {NOTES_END}, next para starts at {NEXT_PARA_START}")

# Spanning delete: from RESPONSE opener through end of Notes (which is also
# the start of the next paragraph). Endpoints are both in non-table content.
stage_a = [
    {"deleteContentRange": {"range": {"startIndex": ORIG_START,
                                      "endIndex":   NOTES_END}}}
]
execute(stage_a, "Stage A: spanning delete of old block")


# ============================================================================
# Stage B -- insert new prose paragraphs + Table R1 caption at ORIG_START
# Each paragraph is its own insertText with a trailing newline. We insert
# in DESCENDING-position order at the SAME anchor, which means each new
# insertText goes BEFORE the previously inserted text (i.e., we insert them
# in REVERSE order to end up with correct paragraph order).
#
# Simpler: build a single text blob with \n separators and insert at once.
# Docs API treats each \n as a paragraph break.
# ============================================================================
prose_blob = "\n".join(NEW_PROSE_PARAS) + "\n" + TABLE_CAPTION + "\n"

stage_b = [
    {"insertText": {"location": {"index": ORIG_START}, "text": prose_blob}}
]
execute(stage_b, "Stage B: insert new prose + Table R1 caption")


# ============================================================================
# Stage C -- insert empty 3x8 table at the end of the caption paragraph.
# After Stage B, the caption ends at: ORIG_START + len(prose_blob).
# But because the blob ended with a "\n", the insertion point for the table
# is at: ORIG_START + len(prose_blob)  -- that index sits at the start of
# what was originally the empty paragraph after the old Notes line.
# We want the table to appear after the caption, which is what this gives us.
# ============================================================================
table_anchor = ORIG_START + len(prose_blob)
stage_c = [
    {"insertTable": {
        "location": {"index": table_anchor},
        "rows": len(TABLE_CELLS),
        "columns": 3,
    }}
]
execute(stage_c, "Stage C: insert empty 3-col x 8-row table")


# ============================================================================
# Stage D -- populate cells.
# After insertTable, the doc indices have shifted. Re-fetch to find the new
# table and walk its cell paragraph indices.
# ============================================================================
doc = fetch_doc()
table_el = None
for el in doc["body"]["content"]:
    if "table" in el and el.get("startIndex", 0) >= table_anchor - 5:
        table_el = el
        break
if table_el is None:
    raise RuntimeError("Could not find the newly inserted table.")

# Walk cells -- for each, the cell's first paragraph startIndex is where
# we insertText. Collect all (row, col, insert_index) tuples.
cell_inserts = []  # list of (insert_index, text)
for r_idx, row in enumerate(table_el["table"]["tableRows"]):
    for c_idx, cell in enumerate(row["tableCells"]):
        text = TABLE_CELLS[r_idx][c_idx]
        if not text:
            continue
        # First paragraph in the cell
        first_para = cell["content"][0]
        insert_at = first_para["startIndex"]
        cell_inserts.append((insert_at, text))

# Insert in DESCENDING index order so each request uses pre-insert indices.
cell_inserts.sort(key=lambda x: -x[0])
stage_d = [
    {"insertText": {"location": {"index": idx}, "text": text}}
    for idx, text in cell_inserts
]
execute(stage_d, "Stage D: populate table cells")


# ============================================================================
# Stage E -- append Notes paragraph after the table.
# Re-fetch to find the index right after the table.
# ============================================================================
doc = fetch_doc()
table_end = None
for el in doc["body"]["content"]:
    if "table" in el and el.get("startIndex", 0) >= table_anchor - 5:
        table_end = el["endIndex"]
        break
if table_end is None:
    raise RuntimeError("Could not find the table to anchor the Notes paragraph.")

# Notes paragraph -- insert AT table_end. Docs inserts after the table's
# trailing paragraph break. Prepend a newline so Notes starts its own paragraph.
stage_e = [
    {"insertText": {"location": {"index": table_end}, "text": TABLE_NOTES + "\n"}}
]
execute(stage_e, "Stage E: insert Notes paragraph after table")

print("\nAll stages complete.")
print("Open the Gdoc to verify; revert via version history if anything is off.")
