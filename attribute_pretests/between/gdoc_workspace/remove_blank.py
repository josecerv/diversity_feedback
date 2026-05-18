"""Try to remove the stray blank paragraph between caption and table.

Approach: delete the caption's trailing \n so the empty paragraph's \n becomes
the caption's terminator.
"""
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

    # Find the caption paragraph and the table that follows.
    caption_end = None
    found_table = False
    for el in doc["body"]["content"]:
        if "paragraph" in el:
            text = "".join(r["textRun"].get("content", "")
                           for r in el["paragraph"]["elements"] if "textRun" in r)
            if text.startswith("Table B1."):
                caption_end = el["endIndex"]  # one past the caption's \n
        elif "table" in el:
            if caption_end is not None:
                found_table = True
                tbl_start = el["startIndex"]
                # If there's exactly one \n between caption and table, no fix needed.
                # If two (caption's \n + empty para's \n), delete the caption's \n.
                gap = tbl_start - caption_end
                print(f"Caption ends at {caption_end}, table starts at {tbl_start}, gap={gap}")
                if gap >= 1:
                    # Delete [caption_end - 1, caption_end): the caption's terminating \n.
                    delete_at = caption_end - 1
                    print(f"Attempt: delete caption \\n at [{delete_at}, {caption_end})")
                    try:
                        docs.documents().batchUpdate(documentId=DOC_ID, body={"requests": [
                            {"deleteContentRange": {"range":
                                {"startIndex": delete_at, "endIndex": caption_end}}}
                        ]}).execute()
                        print("  OK")
                    except Exception as exc:
                        print(f"  rejected: {exc}")
            break

    if not found_table:
        print("No table found after caption.")


if __name__ == "__main__":
    main()
