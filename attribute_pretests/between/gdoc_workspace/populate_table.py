"""Populate the empty table that's already in the doc + bold header/focal rows.

This is split from apply_table.py because the cleanup deletion of the
auto-inserted blank paragraph before the new table is rejected by the API
(the table apparently requires a preceding paragraph). We just leave the
extra blank for now; the user can remove it manually if desired.
"""
import json, pathlib, sys
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
    for el in doc["body"]["content"]:
        if "table" in el:
            return el
    raise SystemExit("No table found.")


def main():
    docs = get_docs()

    # --- Populate cells ---
    doc = docs.documents().get(documentId=DOC_ID).execute()
    tbl = find_table(doc)
    cells = []
    for ri, row in enumerate(tbl["table"]["tableRows"]):
        for ci, cell in enumerate(row["tableCells"]):
            para_start = None
            for sub in cell.get("content", []):
                if "paragraph" in sub:
                    para_start = sub.get("startIndex")
                    break
            cells.append((ri, ci, para_start))

    # Build (row, col) -> text map.
    cell_text = {(0, ci): h for ci, h in enumerate(TABLE_HEADER)}
    for ri, (_, row_cells) in enumerate(TABLE_ROWS, start=1):
        for ci, val in enumerate(row_cells):
            cell_text[(ri, ci)] = val

    # Sort cells by descending para_start so inserts don't shift later positions.
    inserts = []
    for ri, ci, ps in cells:
        text = cell_text.get((ri, ci), "")
        if not text:
            continue
        inserts.append((ps, text))
    inserts.sort(key=lambda x: -x[0])
    reqs = [{"insertText": {"location": {"index": idx}, "text": txt}} for idx, txt in inserts]
    print(f"Populating {len(reqs)} cells...")
    docs.documents().batchUpdate(documentId=DOC_ID, body={"requests": reqs}).execute()

    # --- Bold header row + focal rows ---
    doc = docs.documents().get(documentId=DOC_ID).execute()
    tbl = find_table(doc)

    focal_rows = {0}
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
                    if e and s is not None and e > s + 1:
                        bold_reqs.append({
                            "updateTextStyle": {
                                "range": {"startIndex": s, "endIndex": e - 1},
                                "textStyle": {"bold": True,
                                              "weightedFontFamily": {"fontFamily": "Times New Roman"}},
                                "fields": "bold,weightedFontFamily",
                            }
                        })
    print(f"Bolding {len(bold_reqs)} cell paragraphs (header + focal rows)...")
    if bold_reqs:
        docs.documents().batchUpdate(documentId=DOC_ID, body={"requests": bold_reqs}).execute()

    print("Done.")


if __name__ == "__main__":
    main()
