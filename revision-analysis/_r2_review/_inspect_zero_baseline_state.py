"""Inspect the current state of the manuscript paragraphs around the zero-baseline
insertion and the orphaned fragment, plus the current response-letter paragraph 34
text. Reports back enough info to plan the Chrome moves precisely.
"""
import json
from pathlib import Path

from googleapiclient.discovery import build
from google.oauth2.credentials import Credentials
from google.auth.transport.requests import Request


MANUSCRIPT_ID = "1H9jAvqG5CzQe_BksjkNmLwjLZSlAHvaEK1iyuHL23FI"
RESPONSE_ID = "1YBHCMwD84xC3Qw38CPEzz1iv3JKg7Mw9jTJqyZGptkI"
TOKEN = Path(r"C:/Users/jcerv/.config/gws/token.json")


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


def walk_paragraphs(els, out):
    for el in els:
        if "paragraph" in el:
            out.append(el)
        elif "table" in el:
            for row in el["table"]["tableRows"]:
                for cell in row["tableCells"]:
                    walk_paragraphs(cell.get("content", []), out)


def para_text(p):
    s = ""
    for r in p["paragraph"].get("elements", []):
        if "textRun" in r:
            s += r["textRun"].get("content", "")
    return s


def para_runs(p):
    rows = []
    for r in p["paragraph"].get("elements", []):
        if "textRun" not in r:
            continue
        tr = r["textRun"]
        rows.append({
            "start": r.get("startIndex"),
            "end": r.get("endIndex"),
            "content": tr.get("content", ""),
            "ins_ids": tr.get("suggestedInsertionIds", []),
            "del_ids": list(tr.get("suggestedDeletionIds", [])),
        })
    return rows


def main():
    creds = get_creds()
    docs = build("docs", "v1", credentials=creds)

    # Manuscript: find zero-baseline insertion + Study 5 preamble mini-meta paragraph
    m = docs.documents().get(
        documentId=MANUSCRIPT_ID,
        suggestionsViewMode="SUGGESTIONS_INLINE",
    ).execute()

    paras = []
    walk_paragraphs(m["body"]["content"], paras)

    print("=" * 80)
    print("MANUSCRIPT INSPECTION")
    print("=" * 80)

    for i, p in enumerate(paras):
        txt = para_text(p)
        if "Building on this, we conducted a pooled" in txt or "pooled analysis across Studies 2" in txt:
            print(f"\n--- Para {i} contains zero-baseline block ---")
            print(f"Length: {len(txt)} chars")
            print(f"Text:\n{txt[:3000]}")
            print("\nRuns with suggestion IDs:")
            for r in para_runs(p):
                if r["ins_ids"] or r["del_ids"]:
                    print(f"  [{r['start']}-{r['end']}] ins={r['ins_ids']} del={r['del_ids']} content={r['content'][:80]!r}")
        if "Finally, it" in txt and "Kirgios" in txt:
            print(f"\n--- Para {i} contains 'Finally, it' fragment ---")
            print(f"Text (last 1000 chars):\n{txt[-1000:]}")
        if "Before testing this prediction experimentally" in txt:
            print(f"\n--- Para {i} = mini-meta setup ---")
            print(f"Text (first 300):\n{txt[:300]}")
            print(f"endIndex of last run: {p['paragraph']['elements'][-1].get('endIndex')}")
        if "mini meta-analysis" in txt or "mini-meta" in txt or "pooled interaction" in txt:
            print(f"\n--- Para {i} = mini-meta RESULTS paragraph ---")
            print(f"Length: {len(txt)} chars")
            print(f"Text (first 200):\n{txt[:200]}")
            print(f"Text (last 400):\n{txt[-400:]}")
            print(f"endIndex of last run: {p['paragraph']['elements'][-1].get('endIndex')}")
        if "However, because the proportion of women" in txt:
            print(f"\n--- Para {i} = paragraph immediately after mini-meta ---")
            print(f"Text (first 200):\n{txt[:200]}")
            start = p['paragraph']['elements'][0].get('startIndex')
            print(f"startIndex: {start}")

    # Response letter: dump paragraph 34 region
    r = docs.documents().get(
        documentId=RESPONSE_ID,
        suggestionsViewMode="SUGGESTIONS_INLINE",
    ).execute()
    rparas = []
    walk_paragraphs(r["body"]["content"], rparas)

    print("\n" + "=" * 80)
    print("RESPONSE LETTER INSPECTION (paragraph containing Table R1 setup)")
    print("=" * 80)
    for i, p in enumerate(rparas):
        txt = para_text(p)
        if ("Thank you for raising this alternative" in txt) or ("Table R1" in txt and "follow-up" in txt) or ("footnote to the Discussion of Study 4B" in txt) or ("each Results section" in txt) or ("the later Results sections" in txt):
            print(f"\n--- Resp para {i} ---")
            print(f"Length: {len(txt)} chars")
            print(f"Text:\n{txt}")


if __name__ == "__main__":
    main()
