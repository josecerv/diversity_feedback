"""Dump every footnote in the manuscript Gdoc with its text."""
import json
import pathlib
import sys
import io

sys.stdout = io.TextIOWrapper(sys.stdout.buffer, encoding="utf-8")

from googleapiclient.discovery import build
from google.oauth2.credentials import Credentials
from google.auth.transport.requests import Request

DOC_ID = "1H9jAvqG5CzQe_BksjkNmLwjLZSlAHvaEK1iyuHL23FI"
TOKEN = pathlib.Path(r"C:/Users/jcerv/.config/gws/token.json")


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


def para_text(p):
    parts = []
    for r in p.get("elements", []):
        if "textRun" in r:
            parts.append(r["textRun"].get("content", ""))
        elif "footnoteReference" in r:
            fid = r["footnoteReference"]["footnoteId"]
            parts.append(f"[FN→{fid}]")
    return "".join(parts)


def main():
    creds = get_creds()
    docs = build("docs", "v1", credentials=creds)
    doc = docs.documents().get(
        documentId=DOC_ID,
        suggestionsViewMode="SUGGESTIONS_INLINE",
    ).execute()

    footnotes = doc.get("footnotes", {})
    print(f"Total footnotes in doc: {len(footnotes)}")
    print("=" * 60)
    for fid, fn in footnotes.items():
        body = []
        for el in fn.get("content", []):
            if "paragraph" in el:
                body.append(para_text(el["paragraph"]))
        text = "".join(body).strip()
        print(f"\n--- Footnote {fid} ---")
        print(text[:1200])
        print()

    # Also find where footnote references occur in the main body
    print("=" * 60)
    print("Footnote references in body:")
    body_content = doc["body"]["content"]
    for el in body_content:
        if "paragraph" in el:
            for r in el["paragraph"].get("elements", []):
                if "footnoteReference" in r:
                    fid = r["footnoteReference"]["footnoteId"]
                    idx = r.get("startIndex", "?")
                    # Get surrounding paragraph text
                    ptext = "".join(
                        x["textRun"]["content"]
                        for x in el["paragraph"].get("elements", [])
                        if "textRun" in x
                    )
                    print(f"  fn={fid} at body idx {idx} | anchor para: {ptext[:200]!r}")


if __name__ == "__main__":
    main()
