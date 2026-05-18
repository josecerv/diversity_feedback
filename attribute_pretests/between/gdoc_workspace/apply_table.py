"""Phase 2: replace the per-cell results table.

Steps:
  1. Locate and delete the existing 12x7 table.
  2. Insert a fresh 12x6 table at that position.
  3. Re-fetch, then populate the cells with new content (descending order).
  4. Bold the header row and bold each focal (race/gender) row.

Following the data layout in new_text.TABLE_ROWS:
  - 11 data rows (10 study-attribute rows + 1 Wald summary)
  - 1 header row
  - 6 columns (dropped the old "Type" column)
"""
import json, pathlib, sys, time
from googleapiclient.discovery import build
from google.oauth2.credentials import Credentials
from google.auth.transport.requests import Request

sys.path.insert(0, str(pathlib.Path(__file__).parent))
from new_text import TABLE_HEADER, TABLE_ROWS

DOC_ID = "1svt9mnSQhI8ieCagC2ezGA_ni1SDpV-JTP-_698bUVg"
TOKEN  = pathlib.Path(r"C:/Users/jcerv/.config/gws/token.json")


def get_docs():
    data = json.load(open(TOKEN))
    creds = Credentials.from_authorized_user_info(data, [
        "https://www.googleapis.com/auth/documents",
        "https://www.googleapis.com/auth/drive",
    ])
    if not creds.valid and creds.refresh_token:
        creds.refresh(Request())
    return build("docs", "v1", credentials=creds)


def find_table(doc):
    """Return the first 'table' element in the body content list, with its index/range."""
    for i, el in enumerate(doc["body"]["content"]):
        if "table" in el:
            return i, el
    raise SystemExit("No table found.")


def collect_cell_indices(table_el):
    """Return list of dicts {row, col, start_index, end_index, first_para_start} for each cell."""
    cells = []
    for ri, row in enumerate(table_el["table"]["tableRows"]):
        for ci, cell in enumerate(row["tableCells"]):
            # First paragraph inside the cell holds an empty \n by default.
            first_para_start = None
            for sub in cell.get("content", []):
                if "paragraph" in sub:
                    first_para_start = sub.get("startIndex")
                    break
            cells.append({
                "row": ri, "col": ci,
                "start_index": cell.get("startIndex"),
                "end_index":   cell.get("endIndex"),
                "para_start":  first_para_start,
            })
    return cells


def main():
    docs = get_docs()

    # === STEP 1: delete existing table ===
    doc = docs.documents().get(documentId=DOC_ID).execute()
    _, tbl = find_table(doc)
    tbl_start = tbl["startIndex"]
    tbl_end   = tbl["endIndex"]
    print(f"Deleting old table at [{tbl_start}, {tbl_end})")

    docs.documents().batchUpdate(documentId=DOC_ID, body={"requests": [
        {"deleteContentRange": {"range": {"startIndex": tbl_start, "endIndex": tbl_end}}}
    ]}).execute()

    # === STEP 2: insert empty table at the same position ===
    # After deletion, the previous content has shifted; we need to find where to insert.
    # The table caption paragraph ends with \n right before the table position.
    # Insert at the position previously occupied by the table's startIndex (now shifted).
    # Re-fetch to find the right insertion point.
    doc = docs.documents().get(documentId=DOC_ID).execute()
    # Find the table caption paragraph "Table B1. ..." and insert at its endIndex.
    insert_at = None
    for el in doc["body"]["content"]:
        if "paragraph" in el:
            text = ""
            for r in el["paragraph"]["elements"]:
                if "textRun" in r:
                    text += r["textRun"].get("content", "")
            if text.startswith("Table B1."):
                insert_at = el["endIndex"]  # right after the caption's trailing \n
                # But endIndex points just past the \n; we want to insert AT the \n position
                # Actually, the table should be inserted in place of where the old table was —
                # which is right after the caption's \n. The Docs API inserts a new table at
                # the segment index, creating a new paragraph before/after as needed.
                # We'll use endIndex - 1 to insert right *before* the trailing position to
                # avoid double-newlines. Actually safer: just use endIndex.
                # Per Docs API, InsertTableRequest expects a Location whose index is the
                # position where the table should be inserted.
                break
    if insert_at is None:
        raise SystemExit("Could not locate caption paragraph to anchor new table.")
    print(f"Inserting new table at index {insert_at}")

    n_rows = len(TABLE_ROWS) + 1  # +1 for header
    n_cols = len(TABLE_HEADER)

    docs.documents().batchUpdate(documentId=DOC_ID, body={"requests": [
        {"insertTable": {
            "rows": n_rows,
            "columns": n_cols,
            "location": {"index": insert_at},
        }}
    ]}).execute()

    # The API auto-inserts a newline before the new table; remove it if it
    # creates an empty paragraph between the caption and the table.
    doc = docs.documents().get(documentId=DOC_ID).execute()
    _, tbl = find_table(doc)
    tbl_start = tbl["startIndex"]
    blank_para = None
    for el in doc["body"]["content"]:
        if "paragraph" in el and el.get("endIndex") == tbl_start:
            text = ""
            for r in el["paragraph"]["elements"]:
                if "textRun" in r:
                    text += r["textRun"].get("content", "")
            if text == "\n":
                blank_para = el
            break
    if blank_para is not None:
        s, e = blank_para["startIndex"], blank_para["endIndex"]
        print(f"  removing empty paragraph at [{s}, {e})")
        docs.documents().batchUpdate(documentId=DOC_ID, body={"requests": [
            {"deleteContentRange": {"range": {"startIndex": s, "endIndex": e}}}
        ]}).execute()

    # === STEP 3: populate cells ===
    # Re-fetch to get exact cell indices.
    doc = docs.documents().get(documentId=DOC_ID).execute()
    _, tbl = find_table(doc)
    cells = collect_cell_indices(tbl)
    print(f"New table has {len(cells)} cells across {n_rows} rows × {n_cols} cols.")

    # Map (row, col) -> text.
    cell_text = {}
    for ci, h in enumerate(TABLE_HEADER):
        cell_text[(0, ci)] = h
    for ri, (_, row_cells) in enumerate(TABLE_ROWS, start=1):
        for ci, val in enumerate(row_cells):
            cell_text[(ri, ci)] = val

    # Insert in descending order of insertion index so we don't disturb later (lower) indices.
    inserts = []
    for c in cells:
        text = cell_text.get((c["row"], c["col"]), "")
        if not text:
            continue
        # Each empty cell already contains a trailing \n; insert text at para_start.
        inserts.append((c["para_start"], text))
    inserts.sort(key=lambda x: -x[0])

    reqs = [{"insertText": {"location": {"index": idx}, "text": txt}} for idx, txt in inserts]
    docs.documents().batchUpdate(documentId=DOC_ID, body={"requests": reqs}).execute()
    print(f"Populated {len(reqs)} cells.")

    # === STEP 4: bold header row and bold focal rows ===
    doc = docs.documents().get(documentId=DOC_ID).execute()
    _, tbl = find_table(doc)

    # Compute which rows are focal (race/gender).
    focal_rows = set()
    focal_rows.add(0)  # header always bold
    for ri, (is_focal, _) in enumerate(TABLE_ROWS, start=1):
        if is_focal:
            focal_rows.add(ri)

    bold_reqs = []
    for ri, row in enumerate(tbl["table"]["tableRows"]):
        if ri not in focal_rows:
            continue
        for cell in row["tableCells"]:
            for sub in cell.get("content", []):
                if "paragraph" in sub:
                    s = sub.get("startIndex")
                    e = sub.get("endIndex")
                    # Bold the paragraph text excluding the trailing \n.
                    if e and s is not None and e > s + 1:
                        bold_reqs.append({
                            "updateTextStyle": {
                                "range": {"startIndex": s, "endIndex": e - 1},
                                "textStyle": {"bold": True,
                                              "weightedFontFamily": {"fontFamily": "Times New Roman"}},
                                "fields": "bold,weightedFontFamily",
                            }
                        })

    print(f"Bolding {len(bold_reqs)} cell paragraphs (header + focal rows).")
    if bold_reqs:
        docs.documents().batchUpdate(documentId=DOC_ID, body={"requests": bold_reqs}).execute()
    print("Done.")


if __name__ == "__main__":
    main()
