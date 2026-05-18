"""Fetch the NPR attribute-importance pretest Gdoc to a plain-text dump + raw JSON."""
import json, pathlib
from googleapiclient.discovery import build
from google.oauth2.credentials import Credentials
from google.auth.transport.requests import Request

DOC_ID = "1svt9mnSQhI8ieCagC2ezGA_ni1SDpV-JTP-_698bUVg"
TOKEN  = pathlib.Path(r"C:/Users/jcerv/.config/gws/token.json")
HERE   = pathlib.Path(__file__).parent
OUT_TXT  = HERE / "doc_text.txt"
OUT_JSON = HERE / "doc_raw.json"


def get_creds():
    data = json.load(open(TOKEN))
    creds = Credentials.from_authorized_user_info(data, [
        "https://www.googleapis.com/auth/documents",
        "https://www.googleapis.com/auth/drive",
    ])
    if not creds.valid and creds.refresh_token:
        creds.refresh(Request())
    return creds


def walk(elements, lines, depth=0):
    for el in elements:
        if "paragraph" in el:
            text = ""
            for r in el["paragraph"]["elements"]:
                if "textRun" in r:
                    text += r["textRun"].get("content", "")
            style = el["paragraph"].get("paragraphStyle", {}).get("namedStyleType", "")
            prefix = f"[{style}] " if style and style != "NORMAL_TEXT" else ""
            lines.append(prefix + text.rstrip("\n"))
        elif "table" in el:
            lines.append(f"<<<TABLE start (rows={len(el['table']['tableRows'])})>>>")
            for ri, row in enumerate(el["table"]["tableRows"]):
                cells = []
                for cell in row["tableCells"]:
                    sub = []
                    walk(cell.get("content", []), sub, depth+1)
                    cells.append(" / ".join(s for s in sub if s.strip()))
                lines.append(f"  R{ri}: " + " | ".join(cells))
            lines.append("<<<TABLE end>>>")
        elif "sectionBreak" in el:
            lines.append("---SECTION---")


def main():
    creds = get_creds()
    docs = build("docs", "v1", credentials=creds)
    doc = docs.documents().get(documentId=DOC_ID).execute()
    OUT_JSON.write_text(json.dumps(doc, indent=2), encoding="utf-8")
    body = doc["body"]["content"]
    lines = []
    walk(body, lines)
    text = "\n".join(lines)
    OUT_TXT.write_text(text, encoding="utf-8")
    print(f"Wrote {OUT_TXT} ({len(text)} chars) and {OUT_JSON}")


if __name__ == "__main__":
    main()
