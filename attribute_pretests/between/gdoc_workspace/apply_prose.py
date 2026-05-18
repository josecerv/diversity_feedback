"""Phase 1: replace prose paragraphs in the Gdoc.

Strategy:
- Re-fetch the doc and re-locate paragraphs by leading text (so we pick up
  any of JC's recent edits and use fresh indices).
- For each paragraph, delete the body range and re-insert new text.
- Sort operations by *descending* startIndex so earlier ops don't shift
  later ops' positions within a single batchUpdate.
- After inserts, apply textStyle: reset italic/bold to a known state, then
  italicize stats inside results/notes/preamble.

Does NOT touch the table or its caption (those are Phase 2).
"""
import json, pathlib, re, sys
from googleapiclient.discovery import build
from google.oauth2.credentials import Credentials
from google.auth.transport.requests import Request

sys.path.insert(0, str(pathlib.Path(__file__).parent))
from new_text import (
    SEC1_PROCEDURE_BODY, SEC1_RESULTS_BODY,
    SEC2_TITLE, SEC2_PREAMBLE,
    SEC2_METHODS_P1, SEC2_METHODS_P2, SEC2_METHODS_P3,
    SEC2_RESULTS, SEC2_TABLE_CAPTION, SEC2_NOTES_BODY,
)

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


def collect_paragraphs(doc):
    """Return list of dicts {start, end, text, runs:[(s,e,style,content)]} for all body paragraphs (no table cells)."""
    out = []
    for el in doc["body"]["content"]:
        if "paragraph" not in el:
            continue
        runs = []
        text = ""
        for r in el["paragraph"]["elements"]:
            if "textRun" in r:
                tr = r["textRun"]
                runs.append((r.get("startIndex"), r.get("endIndex"),
                             tr.get("textStyle", {}), tr.get("content", "")))
                text += tr.get("content", "")
        out.append({"start": el.get("startIndex"), "end": el.get("endIndex"),
                    "text": text, "runs": runs})
    return out


def find_para(paras, prefix=None, exact=None):
    """Find the paragraph whose text starts with `prefix` or equals `exact`."""
    for p in paras:
        if exact is not None and p["text"].rstrip("\n") == exact:
            return p
        if prefix is not None and p["text"].startswith(prefix):
            return p
    raise SystemExit(f"Could not find paragraph: prefix={prefix!r} exact={exact!r}")


# Stat tokens to italicize wherever they appear as standalone stat symbols.
ITALIC_RE = [
    re.compile(r"(?<![A-Za-z])M(?=\s*=)"),
    re.compile(r"(?<![A-Za-z])SD(?=\s*=)"),
    re.compile(r"(?<![A-Za-z])t(?=\()"),
    re.compile(r"(?<![A-Za-z])p(?=\s*[<=>.])"),
    re.compile(r"(?<![A-Za-z])d(?=\s*=)"),
    re.compile(r"(?<![A-Za-z])N(?=\s*=)"),
    re.compile(r"Δ(?=\s*=)"),
]


def italic_ranges(text, base_index):
    out = []
    for rx in ITALIC_RE:
        for m in rx.finditer(text):
            out.append((base_index + m.start(), base_index + m.end()))
    return out


def main():
    docs = get_docs()

    # === STAGE 1: text replacements ===
    doc = docs.documents().get(documentId=DOC_ID).execute()
    paras = collect_paragraphs(doc)

    # Locate the paragraphs we need to edit.
    p_proc    = find_para(paras, prefix="Procedure. ")
    p_res1    = find_para(paras, prefix="Following our pre-registration, we conducted three t-tests")
    p_title   = find_para(paras, exact="Pooled Zero-Baseline Analysis")
    p_pream   = find_para(paras, prefix="A reader might worry")
    p_methP1  = find_para(paras, prefix="We pooled participant-level data")
    p_methP2  = find_para(paras, prefix="Within each (study, attribute) cell")
    p_methP3  = find_para(paras, prefix="We then pooled cell-level estimates")
    p_results = find_para(paras, prefix="Within the zero-initial subset")
    p_caption = find_para(paras, prefix="Table B1.")
    p_notes   = find_para(paras, prefix="Notes.")

    # For each, compute (delete_range, insert_index, new_text, post_style_callable).
    # "post_style_callable" produces a list of updateTextStyle requests using the
    # new absolute index (computed after this stage from a re-fetch).
    # The italic prefix is preserved by leaving the leading 'Procedure. ' (11ch)
    # and 'Notes. ' (7ch) untouched.

    PROC_PREFIX_LEN  = len("Procedure. ")   # 11
    NOTES_PREFIX_LEN = len("Notes. ")        # 7

    edits = []

    def add(para, prefix_len, new_text, label):
        body_start = para["start"] + prefix_len
        body_end   = para["end"] - 1   # leave trailing \n alone
        edits.append({
            "label": label,
            "delete_start": body_start,
            "delete_end":   body_end,
            "insert_at":    body_start,
            "text":         new_text,
        })

    add(p_proc,    PROC_PREFIX_LEN, SEC1_PROCEDURE_BODY, "sec1.procedure")
    add(p_res1,    0,                SEC1_RESULTS_BODY,   "sec1.results")
    add(p_title,   0,                SEC2_TITLE,          "sec2.title")
    add(p_pream,   0,                SEC2_PREAMBLE,       "sec2.preamble")
    add(p_methP1,  0,                SEC2_METHODS_P1,     "sec2.methods.p1")
    add(p_methP2,  0,                SEC2_METHODS_P2,     "sec2.methods.p2")
    add(p_methP3,  0,                SEC2_METHODS_P3,     "sec2.methods.p3")
    add(p_results, 0,                SEC2_RESULTS,        "sec2.results")
    add(p_caption, 0,                SEC2_TABLE_CAPTION,  "sec2.caption")
    add(p_notes,   NOTES_PREFIX_LEN, SEC2_NOTES_BODY,     "sec2.notes")

    # Sort descending by delete_start so earlier ops don't shift later ones.
    edits.sort(key=lambda e: e["delete_start"], reverse=True)

    requests = []
    for e in edits:
        # Delete the body range (only if non-empty).
        if e["delete_end"] > e["delete_start"]:
            requests.append({
                "deleteContentRange": {
                    "range": {"startIndex": e["delete_start"], "endIndex": e["delete_end"]}
                }
            })
        requests.append({
            "insertText": {
                "location": {"index": e["insert_at"]},
                "text": e["text"],
            }
        })

    print(f"Stage 1: {len(requests)} requests for {len(edits)} paragraph edits...")
    result = docs.documents().batchUpdate(documentId=DOC_ID, body={"requests": requests}).execute()
    print(f"  OK: {len(result.get('replies', []))} replies")

    # === STAGE 2: style fixes on the new text ===
    doc = docs.documents().get(documentId=DOC_ID).execute()
    paras = collect_paragraphs(doc)

    style_reqs = []

    def reset_normal(para_text_prefix, prefix_len=0, want_bold=False):
        """Find the paragraph, then reset italic=False/bold=want_bold on body range."""
        p = find_para(paras, prefix=para_text_prefix)
        body_start = p["start"] + prefix_len
        body_end   = p["end"] - 1
        if body_end <= body_start:
            return None
        style_reqs.append({
            "updateTextStyle": {
                "range": {"startIndex": body_start, "endIndex": body_end},
                "textStyle": {
                    "italic": False,
                    "bold": want_bold,
                    "weightedFontFamily": {"fontFamily": "Times New Roman"},
                },
                "fields": "italic,bold,weightedFontFamily",
            }
        })
        return (p, body_start)

    # Reset everything to default (not italic, not bold) except title + caption (bold).
    # Procedure body: was inheriting italic from "Procedure. " — clear it.
    reset_normal("Procedure. ", PROC_PREFIX_LEN, want_bold=False)
    reset_normal("Following our pre-registration, we conducted three t-tests", 0, want_bold=False)
    reset_normal(SEC2_TITLE,    0, want_bold=True)    # Title bold
    reset_normal(SEC2_PREAMBLE[:60], 0, want_bold=False)
    reset_normal(SEC2_METHODS_P1[:60], 0, want_bold=False)
    reset_normal(SEC2_METHODS_P2[:60], 0, want_bold=False)
    reset_normal(SEC2_METHODS_P3[:60], 0, want_bold=False)
    reset_normal(SEC2_RESULTS[:60], 0, want_bold=False)
    reset_normal("Table B1.", 0, want_bold=True)       # Caption bold
    reset_normal("Notes.", NOTES_PREFIX_LEN, want_bold=False)

    # Now italicize stats in: SEC1_RESULTS, SEC2_RESULTS, SEC2_NOTES.
    def italicize_in(para_text_prefix, body_text, prefix_len=0):
        p = find_para(paras, prefix=para_text_prefix)
        body_start = p["start"] + prefix_len
        for s, e in italic_ranges(body_text, body_start):
            style_reqs.append({
                "updateTextStyle": {
                    "range": {"startIndex": s, "endIndex": e},
                    "textStyle": {"italic": True},
                    "fields": "italic",
                }
            })

    italicize_in("Following our pre-registration, we conducted three t-tests", SEC1_RESULTS_BODY, 0)
    italicize_in(SEC2_RESULTS[:60], SEC2_RESULTS, 0)
    italicize_in("Notes.", SEC2_NOTES_BODY, NOTES_PREFIX_LEN)

    print(f"Stage 2: {len(style_reqs)} style requests...")
    if style_reqs:
        result = docs.documents().batchUpdate(documentId=DOC_ID, body={"requests": style_reqs}).execute()
        print(f"  OK: {len(result.get('replies', []))} replies")


if __name__ == "__main__":
    main()
