"""Clear stray italic across all table cells (and the blank paragraph before the table)."""
import json, pathlib
from googleapiclient.discovery import build
from google.oauth2.credentials import Credentials
from google.auth.transport.requests import Request

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


def main():
    docs = get_docs()
    doc = docs.documents().get(documentId=DOC_ID).execute()

    reqs = []
    # Find the table and the blank paragraph that precedes it.
    for el in doc["body"]["content"]:
        if "table" in el:
            tbl_start = el["startIndex"]
            for cell_el in el["table"]["tableRows"]:
                for cell in cell_el["tableCells"]:
                    for sub in cell.get("content", []):
                        if "paragraph" in sub:
                            s = sub.get("startIndex")
                            e = sub.get("endIndex")
                            if s is not None and e is not None and e > s:
                                reqs.append({
                                    "updateTextStyle": {
                                        "range": {"startIndex": s, "endIndex": e - 1 if e - 1 > s else e},
                                        "textStyle": {"italic": False},
                                        "fields": "italic",
                                    }
                                })
        elif "paragraph" in el:
            text = "".join(r["textRun"].get("content", "")
                           for r in el["paragraph"]["elements"] if "textRun" in r)
            # Blank paragraph right before the table — clear italic on its \n.
            if text == "\n":
                s, e = el["startIndex"], el["endIndex"]
                reqs.append({
                    "updateTextStyle": {
                        "range": {"startIndex": s, "endIndex": e},
                        "textStyle": {"italic": False},
                        "fields": "italic",
                    }
                })

    print(f"Clearing italic on {len(reqs)} ranges...")
    docs.documents().batchUpdate(documentId=DOC_ID, body={"requests": reqs}).execute()
    print("Done.")


if __name__ == "__main__":
    main()
