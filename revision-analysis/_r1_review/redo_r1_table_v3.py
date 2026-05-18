"""Table R1 v3 -- old-R1-style 3-column layout, all attributes in one column.

Layout (15 rows x 3 cols; header row 0; data rows 1..14):

  Col 0: Study (Total N)         -- merged vertically across attribute rows
  Col 1: Attribute               -- race/gender attribute listed first per study,
                                    then comparison attributes stacked below
  Col 2: Effect when Initial = 0 -- the per-cell coefficient, stars, N

Footer (rows 12-14):
  Pooled (across studies) [merged col 0] | Race/Gender Feedback (FE...) | +21.28*** ...
                                         | Comparison Feedback (same)   | +6.83  ...
                                         | Race/Gender - Comparison (Wald) | +14.46* ...

Stages
  A. Spanning delete of current prose-p4 + caption + table + Notes
  B. Insert rewritten paragraph 4 + caption
  C. Insert empty 15x3 table
  D. Populate cells (skip cells that will be hidden by a merge)
  E. Vertical merges in col 0 for Studies 2, 3A, 3B, 4B, and Pooled footer
  F. Insert Notes paragraph
  G. Polish pass: bold caption prefix, bold header row, bold Pooled label,
     bold pooled effect values, italic "Notes." prefix, italic each "p" in
     the Notes paragraph.
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


# ---------------------------------------------------------------------------
# Curly punctuation
# ---------------------------------------------------------------------------
LSQUO, RSQUO = "‘", "’"
LDQUO, RDQUO = "“", "”"
MINUS = "−"   # U+2212
EMDASH = "—"

# ---------------------------------------------------------------------------
# Replacement paragraph 4 (column description), matching new 3-col structure
# ---------------------------------------------------------------------------
NEW_P4 = (
    f"Table R1 reports each per-cell estimate together with the pooled "
    f"coefficients and the Wald-test difference. The first column lists each "
    f"study and its total sample size; the second column lists the focal "
    f"attribute, the race or gender attribute for the study followed by each "
    f"estimable comparison attribute; and the third column reports the "
    f"feedback effect for that attribute, restricted to participants whose "
    f"initial selection contained zero of it. The bottom three rows report "
    f"the pooled race/gender feedback effect, the pooled comparison feedback "
    f"effect, and the Wald-test difference between them. We acknowledge that "
    f"the comparison side is estimable for only six of the fifteen comparison "
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
# 15 rows x 3 cols. Cells hidden by a merge are "".
# ---------------------------------------------------------------------------
CELLS = [
    # 0 -- header
    ["Study (Total N)", "Attribute", "Effect when Initial = 0"],
    # 1..3 -- Study 2
    ["Study 2 (N = 302)",    "Women",                  "+32.66** (N = 63)"],
    ["",                     "Featured Entertainer",   "+21.30+ (N = 66)"],
    ["",                     "Over 500 Pages",         f"{MINUS}1.79 (N = 144)"],
    # 4..5 -- Study 3A
    ["Study 3A (N = 1,000)", "Racial Minority",        "+9.77+ (N = 247)"],
    ["",                     "High Budget",            "+33.33 (N = 40)"],
    # 6..8 -- Study 3B
    ["Study 3B (N = 1,000)", "Women",                  "+31.57*** (N = 336)"],
    ["",                     "High Budget",            f"{MINUS}9.52 (N = 61)"],
    ["",                     "Political Leader",       "+9.77 (N = 52)"],
    # 9 -- Study 4A (no estimable comparison)
    ["Study 4A (N = 1,000)", "Racial Minority",        "+18.37*** (N = 358)"],
    # 10..11 -- Study 4B
    ["Study 4B (N = 1,000)", "Women",                  "+20.01** (N = 218)"],
    ["",                     "Sold 30M+ Copies",       "+27.54 (N = 29)"],
    # 12..14 -- Pooled footer
    ["Pooled (across studies)",
     "Race/Gender Feedback (FE, participant-clustered SE)",
     "+21.28*** (N = 1,222) [+16.15, +26.42]"],
    ["",
     "Comparison Feedback (same model)",
     f"+6.83 (N = 429) [{MINUS}3.81, +17.46]"],
    ["",
     f"Race/Gender {MINUS} Comparison difference (Wald test)",
     "+14.46* (N = 1,651) [+2.70, +26.21]"],
]

# Vertical merges: (rowIndex, columnIndex, rowSpan, columnSpan)
VERTICAL_MERGES = [
    (1, 0, 3, 1),    # Study 2 spans rows 1-3
    (4, 0, 2, 1),    # Study 3A spans rows 4-5
    (6, 0, 3, 1),    # Study 3B spans rows 6-8
    (10, 0, 2, 1),   # Study 4B spans rows 10-11
    (12, 0, 3, 1),   # Pooled footer spans rows 12-14
]

TABLE_NOTES = (
    f"Notes. This table reports the OLS treatment effect of receiving feedback "
    f"about the focal attribute on the rate of selecting a candidate with that "
    f"attribute on the participant{RSQUO}s final selection, restricted to "
    f"participants whose initial selection contained zero of the focal "
    f"attribute. Per-cell estimates use HC3 robust standard errors. The pooled "
    f"rows come from a single OLS model on the stacked participant-attribute "
    f"observations with study-by-attribute fixed effects and standard errors "
    f"clustered by participant; the final row is a Wald test of the "
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
# Stage A -- find current paragraph 4 + Notes, span-delete
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
    "Stage A: span-delete p4 + caption + table + Notes",
)


# ============================================================================
# Stage B -- insert new paragraph 4 + caption
# ============================================================================
prose_blob = NEW_P4 + "\n" + TABLE_CAPTION + "\n"
execute(
    [{"insertText": {"location": {"index": p4_start}, "text": prose_blob}}],
    "Stage B: insert paragraph 4 + caption",
)


# ============================================================================
# Stage C -- insert empty 15x3 table
# ============================================================================
table_anchor = p4_start + len(prose_blob)
execute(
    [{"insertTable": {
        "location": {"index": table_anchor},
        "rows": len(CELLS),
        "columns": 3,
    }}],
    "Stage C: insert empty 15x3 table",
)


# ============================================================================
# Stage D -- populate cells
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

cell_inserts.sort(key=lambda x: -x[0])
execute(
    [{"insertText": {"location": {"index": idx}, "text": text}}
     for idx, text in cell_inserts],
    f"Stage D: populate {len(cell_inserts)} cells",
)


# ============================================================================
# Stage E -- vertical merges
# ============================================================================
execute(
    [{"mergeTableCells": {
        "tableRange": {
            "tableCellLocation": {
                "tableStartLocation": {"index": table_start_index},
                "rowIndex": r,
                "columnIndex": c,
            },
            "rowSpan": rs,
            "columnSpan": cs,
        }}}
     for r, c, rs, cs in VERTICAL_MERGES],
    f"Stage E: {len(VERTICAL_MERGES)} vertical merges",
)


# ============================================================================
# Stage F -- insert Notes paragraph after the table
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


# ============================================================================
# Stage G -- polish pass: bold + italic where it matters.
# Re-fetch the doc and locate by character offsets / text matches.
# ============================================================================
def find_paragraph_starting_with(prefix, after_index=0):
    d = fetch_doc()
    for el in d["body"]["content"]:
        if "paragraph" not in el:
            continue
        if el.get("startIndex", 0) < after_index:
            continue
        runs = el["paragraph"].get("elements", [])
        txt = "".join(r.get("textRun", {}).get("content", "") for r in runs)
        if txt.startswith(prefix):
            return el, txt
    return None, None


def find_substring_index_in_paragraph(para_el, substring):
    """Return absolute doc index where `substring` starts inside this paragraph."""
    cursor = para_el["startIndex"]
    for run in para_el["paragraph"].get("elements", []):
        tr = run.get("textRun")
        if not tr:
            continue
        content = tr.get("content", "")
        pos = content.find(substring)
        if pos != -1:
            return run["startIndex"] + pos
        cursor = run["endIndex"]
    return None


def style_text_request(start, length, style_dict, fields):
    return {
        "updateTextStyle": {
            "range": {"startIndex": start, "endIndex": start + length},
            "textStyle": style_dict,
            "fields": fields,
        }
    }


polish_requests = []

# 1. Bold "Table R1." prefix in the caption paragraph
caption_para, caption_text = find_paragraph_starting_with("Table R1. Effect of Feedback", p4_start)
if caption_para:
    cap_start = caption_para["startIndex"]
    polish_requests.append(style_text_request(cap_start, len("Table R1."),
                                              {"bold": True}, "bold"))

# 2. Bold the table header row (row 0, all 3 cells)
doc = fetch_doc()
new_table = None
for el in doc["body"]["content"]:
    if "table" in el and el.get("startIndex", 0) >= table_start_index - 1:
        new_table = el
        break
if new_table is not None:
    header_row = new_table["table"]["tableRows"][0]
    for cell in header_row["tableCells"]:
        para = cell["content"][0]
        runs = para["paragraph"].get("elements", [])
        for run in runs:
            tr = run.get("textRun")
            if not tr:
                continue
            content = tr.get("content", "")
            length = len(content.rstrip("\n"))
            if length > 0:
                polish_requests.append(style_text_request(
                    run["startIndex"], length, {"bold": True}, "bold"))

# 3. Bold "Pooled (across studies)" label cell (row 12, col 0 -- merged)
if new_table is not None:
    pooled_row = new_table["table"]["tableRows"][12]
    pooled_cell = pooled_row["tableCells"][0]
    para = pooled_cell["content"][0]
    for run in para["paragraph"].get("elements", []):
        tr = run.get("textRun")
        if not tr:
            continue
        content = tr.get("content", "")
        length = len(content.rstrip("\n"))
        if length > 0:
            polish_requests.append(style_text_request(
                run["startIndex"], length, {"bold": True}, "bold"))

# 4. Bold the effect-value cells in the pooled footer (rows 12, 13, 14 col 2)
if new_table is not None:
    for r in (12, 13, 14):
        cell = new_table["table"]["tableRows"][r]["tableCells"][2]
        para = cell["content"][0]
        for run in para["paragraph"].get("elements", []):
            tr = run.get("textRun")
            if not tr:
                continue
            content = tr.get("content", "")
            length = len(content.rstrip("\n"))
            if length > 0:
                polish_requests.append(style_text_request(
                    run["startIndex"], length, {"bold": True}, "bold"))

# 5. Italicize "Notes." prefix in the Notes paragraph
notes_para, _ = find_paragraph_starting_with("Notes. This table reports",
                                              table_start_index)
if notes_para is not None:
    notes_start = notes_para["startIndex"]
    polish_requests.append(style_text_request(notes_start, len("Notes."),
                                              {"italic": True}, "italic"))
    # 6. Italicize each "p" before " <" or " =" in the Notes (significance stars)
    # The Notes ends with "+p < .10; *p < .05; **p < .01; ***p < .001."
    # We italicize each "p" that immediately follows + or *.
    paragraph_text = ""
    para_first_run_start = None
    for run in notes_para["paragraph"].get("elements", []):
        tr = run.get("textRun")
        if not tr:
            continue
        if para_first_run_start is None:
            para_first_run_start = run["startIndex"]
        paragraph_text += tr.get("content", "")
    # Find each "p" position in the paragraph string
    # We italicize p preceded by + or * and followed by " "
    for i, ch in enumerate(paragraph_text):
        if ch != "p":
            continue
        if i == 0:
            continue
        prev = paragraph_text[i - 1]
        nxt = paragraph_text[i + 1] if i + 1 < len(paragraph_text) else ""
        if prev in ("+", "*") and nxt == " ":
            abs_idx = para_first_run_start + i
            polish_requests.append(style_text_request(abs_idx, 1,
                                                     {"italic": True}, "italic"))

if polish_requests:
    execute(polish_requests, f"Stage G: polish ({len(polish_requests)} style ops)")
else:
    print("Stage G: no polish requests generated.")

print("\nDone.")
