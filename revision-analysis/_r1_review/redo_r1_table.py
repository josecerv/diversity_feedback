"""Redo Table R1 with the old-R1-style layout:

  Col 1: Study (Total N)                                   <- merged for multi-attribute studies
  Col 2: Race/Gender Feedback Effect when Initial = 0      <- merged with study
  Col 3: Comparison Attribute                              <- stacked per study
  Col 4: Comparison Feedback Effect when Initial = 0       <- stacked per study

Rows (1 header + 9 data):
  0  Header
  1  Study 2 / Women: +32.66** (N=63)   / Featured Entertainer / +21.30+ (N=66)
  2                                     / Over 500 Pages       / -1.79 (N=144)
  3  Study 3A / Racial Minority: +9.77+ (N=247) / High Budget  / +33.33 (N=40)
  4  Study 3B / Women: +31.57*** (N=336)        / High Budget  / -9.52 (N=61)
  5                                             / Political Leader / +9.77 (N=52)
  6  Study 4A / Racial Minority: +18.37*** (N=358) / -- / --
  7  Study 4B / Women: +20.01** (N=218)            / Sold 30M+ Copies / +27.54 (N=29)
  8  Pooled (FE) / +21.28*** (N=1,222) [...]      / -- / +6.83 (N=429) [...]
  9  Target - Comparison difference (Wald test)  / value spans cols 2-4

Vertical merges in cols 0 & 1 for Studies 2 and 3B (which have 2 comparison
attributes each). Horizontal merge of cols 1-3 in the Wald row.

We also rewrite paragraph 4 of the prose (the table-column description) to
match the new 4-column structure.

Stages:
  A. Delete from start of prose paragraph 4 through end of Notes (spanning)
  B. Insert new paragraph 4 + new caption
  C. Insert empty 10x4 table
  D. Populate cells (skip cells that will be hidden by a merge)
  E. Merge cells
  F. Insert Notes paragraph
"""
from __future__ import annotations

import io
import json
import sys

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


# Curly punctuation
LSQUO, RSQUO = "‘", "’"
LDQUO, RDQUO = "“", "”"
MINUS = "−"   # U+2212
EMDASH = "—"  # used in cells as a placeholder for "not estimable"

# ---------------------------------------------------------------------------
# New paragraph-4 prose (table column description) -- replaces the old version
# in the doc so the prose matches the new 4-column layout.
# ---------------------------------------------------------------------------
NEW_P4 = (
    f"Table R1 reports each per-cell estimate together with the pooled "
    f"coefficients and the Wald-test difference. The first column lists each "
    f"study and its total sample size; the second column reports the race or "
    f"gender feedback effect among participants whose initial selection "
    f"contained zero of that attribute; the third column lists each estimable "
    f"comparison attribute in the study; and the fourth column reports the "
    f"feedback effect for each comparison attribute, again among participants "
    f"whose initial selection contained zero of that attribute. The "
    f"penultimate row gives the pooled fixed-effects coefficients for race "
    f"and gender feedback and for comparison feedback, and the final row "
    f"reports the Wald-test difference between them. We acknowledge that the "
    f"comparison side is estimable for only six of the fifteen comparison "
    f"attribute-by-study cells, and we therefore present this as a "
    f"directional, lower-powered test. The new survey described below "
    f"complements this behavioral evidence by asking participants directly "
    f"about the appropriateness of different attribute mixes."
)

TABLE_CAPTION = (
    f"Table R1. Effect of Feedback on the Final Selection, Among Participants "
    f"Whose Initial Selection Contained Zero of the Focal Attribute."
)

# ---------------------------------------------------------------------------
# 10 rows x 4 cols. Cells that will be hidden by a merge are left as "".
# We populate ONLY the leftmost cell of each merge group.
# ---------------------------------------------------------------------------
CELLS = [
    # Row 0 -- header
    ["Study (Total N)",
     "Race/Gender Feedback Effect when Initial = 0",
     "Comparison Attribute",
     "Comparison Feedback Effect when Initial = 0"],
    # Row 1 -- Study 2 (1st comparison row)
    ["Study 2 (N = 302)",
     "Women: +32.66** (N = 63)",
     "Featured Entertainer",
     "+21.30+ (N = 66)"],
    # Row 2 -- Study 2 (2nd comparison row); col 0 & 1 hidden by merge
    ["", "", "Over 500 Pages", f"{MINUS}1.79 (N = 144)"],
    # Row 3 -- Study 3A
    ["Study 3A (N = 1,000)",
     "Racial Minority: +9.77+ (N = 247)",
     "High Budget",
     "+33.33 (N = 40)"],
    # Row 4 -- Study 3B (1st comparison row)
    ["Study 3B (N = 1,000)",
     "Women: +31.57*** (N = 336)",
     "High Budget",
     f"{MINUS}9.52 (N = 61)"],
    # Row 5 -- Study 3B (2nd comparison row)
    ["", "", "Political Leader", "+9.77 (N = 52)"],
    # Row 6 -- Study 4A (no estimable comparison)
    ["Study 4A (N = 1,000)",
     "Racial Minority: +18.37*** (N = 358)",
     f"{EMDASH}",
     f"{EMDASH}"],
    # Row 7 -- Study 4B
    ["Study 4B (N = 1,000)",
     "Women: +20.01** (N = 218)",
     "Sold 30M+ Copies",
     "+27.54 (N = 29)"],
    # Row 8 -- Pooled
    ["Pooled (study-by-attribute FE; participant-clustered SE)",
     "+21.28*** (N = 1,222) [+16.15, +26.42]",
     f"{EMDASH}",
     f"+6.83 (N = 429) [{MINUS}3.81, +17.46]"],
    # Row 9 -- Wald row: value spans cols 1-3 after merge
    [f"Target {MINUS} Comparison difference (Wald test)",
     "+14.46* (N = 1,651) [+2.70, +26.21]",
     "",
     ""],
]

# Vertical merges: (rowIndex, columnIndex, rowSpan, columnSpan)
VERTICAL_MERGES = [
    (1, 0, 2, 1),  # Study 2 spans rows 1-2 col 0
    (1, 1, 2, 1),  # Women effect spans rows 1-2 col 1
    (4, 0, 2, 1),  # Study 3B spans rows 4-5 col 0
    (4, 1, 2, 1),  # Women effect spans rows 4-5 col 1
]
HORIZONTAL_MERGES = [
    (9, 1, 1, 3),  # Wald row: cols 1-3 merge into one cell
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
    f"difference between the pooled race/gender and pooled comparison "
    f"coefficients in that model. Of the fifteen comparison attribute-by-study "
    f"cells, six are reported here; the remaining nine are omitted because the "
    f"comparison attribute is common in its candidate pool, leaving fewer than "
    f"twenty participants with zero of the attribute in their initial "
    f"selection. Studies 1 and 5 are excluded because their initial portfolios "
    f"are not participant-generated in a way that defines a per-participant "
    f"zero-initial cell. Confidence intervals are 95%. +p < .10; *p < .05; "
    f"**p < .01; ***p < .001."
)


# ============================================================================
# Stage A -- find current paragraph 4, caption, table, Notes and span-delete
# ============================================================================
def find_block():
    doc = fetch_doc()
    elements = doc["body"]["content"]
    p4_start = notes_end = None
    for el in elements:
        if "paragraph" not in el:
            continue
        txt = "".join(r.get("textRun", {}).get("content", "")
                      for r in el["paragraph"].get("elements", []))
        if p4_start is None and txt.startswith("Table R1 reports each per-cell estimate"):
            p4_start = el["startIndex"]
        if p4_start is not None and txt.startswith("Notes."):
            notes_end = el["endIndex"]
            break
    if p4_start is None or notes_end is None:
        raise RuntimeError("Could not locate paragraph 4 or Notes paragraph.")
    return p4_start, notes_end


p4_start, notes_end = find_block()
print(f"Spanning delete: [{p4_start}, {notes_end}]")

execute(
    [{"deleteContentRange": {"range": {"startIndex": p4_start,
                                       "endIndex": notes_end}}}],
    "Stage A: spanning delete (p4 + caption + table + notes)",
)


# ============================================================================
# Stage B -- insert new paragraph 4 + Table R1 caption
# ============================================================================
prose_blob = NEW_P4 + "\n" + TABLE_CAPTION + "\n"
execute(
    [{"insertText": {"location": {"index": p4_start}, "text": prose_blob}}],
    "Stage B: insert new paragraph 4 + caption",
)


# ============================================================================
# Stage C -- insert empty 10x4 table at end of caption
# ============================================================================
table_anchor = p4_start + len(prose_blob)
execute(
    [{"insertTable": {
        "location": {"index": table_anchor},
        "rows": len(CELLS),
        "columns": 4,
    }}],
    "Stage C: insert empty 10x4 table",
)


# ============================================================================
# Stage D -- populate cells (skip empty placeholder cells)
# ============================================================================
doc = fetch_doc()
table_el = None
table_start_index = None
for el in doc["body"]["content"]:
    if "table" in el and el.get("startIndex", 0) >= table_anchor - 5:
        table_el = el
        table_start_index = el["startIndex"]
        break
if table_el is None:
    raise RuntimeError("Could not find inserted table.")

print(f"Table inserted at index {table_start_index}")

cell_inserts = []
for r_idx, row in enumerate(table_el["table"]["tableRows"]):
    for c_idx, cell in enumerate(row["tableCells"]):
        text = CELLS[r_idx][c_idx]
        if not text:
            continue
        first_para = cell["content"][0]
        insert_at = first_para["startIndex"]
        cell_inserts.append((insert_at, text))

# Sort descending so each insert uses pre-insert indices
cell_inserts.sort(key=lambda x: -x[0])
stage_d = [
    {"insertText": {"location": {"index": idx}, "text": text}}
    for idx, text in cell_inserts
]
execute(stage_d, f"Stage D: populate {len(stage_d)} cells")


# ============================================================================
# Stage E -- merge cells (vertical merges + Wald-row horizontal merge)
# Use the *original* table_start_index recorded just after the table insert.
# rowIndex/columnIndex always refer to the original (pre-merge) cell grid.
# ============================================================================
merge_requests = []
for r, c, rs, cs in VERTICAL_MERGES + HORIZONTAL_MERGES:
    merge_requests.append({
        "mergeTableCells": {
            "tableRange": {
                "tableCellLocation": {
                    "tableStartLocation": {"index": table_start_index},
                    "rowIndex": r,
                    "columnIndex": c,
                },
                "rowSpan": rs,
                "columnSpan": cs,
            }
        }
    })
execute(merge_requests, f"Stage E: merge {len(merge_requests)} cell groups")


# ============================================================================
# Stage F -- insert Notes paragraph after table
# Re-fetch to find post-merge table end.
# ============================================================================
doc = fetch_doc()
table_end = None
for el in doc["body"]["content"]:
    if "table" in el and el.get("startIndex", 0) >= table_start_index - 1:
        table_end = el["endIndex"]
        break
if table_end is None:
    raise RuntimeError("Could not find post-merge table end.")

execute(
    [{"insertText": {"location": {"index": table_end}, "text": TABLE_NOTES + "\n"}}],
    "Stage F: insert Notes paragraph",
)

print("\nDone. Refresh the Gdoc to verify.")
