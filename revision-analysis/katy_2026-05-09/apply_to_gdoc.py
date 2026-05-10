"""Apply operations.json to the live manuscript Gdoc.

Pipeline:
  1. Re-fetch current Gdoc state (so we don't apply against stale JSON).
  2. Phase A: 50 replaceAllText + 16 indexDelete/indexReplace via batchUpdate.
     Index-based ops sorted DESCENDING by startIndex so earlier indices
     stay valid through the batch. Submitted in chunks of <= 50 requests.
  3. Phase B: createFootnote + replaceAllText. Footnotes use anchor-search
     to resolve insertion index from a fresh post-Phase-A fetch.
  4. Phase C: Drive comments API per anchor.
  5. Re-fetch post-state and write gdoc_after.json + .txt.

Safety:
  - Defaults to --dry-run; refuses --apply without --eyeballed
  - Aborts on any API error
  - Logs every request to apply_log.txt with timestamp
"""
from __future__ import annotations
import argparse, json, sys, io, time
from pathlib import Path

sys.stdout = io.TextIOWrapper(sys.stdout.buffer, encoding="utf-8")

from google.oauth2.credentials import Credentials
from google.auth.transport.requests import Request
from googleapiclient.discovery import build
from googleapiclient.errors import HttpError

DOC_ID = "1H9jAvqG5CzQe_BksjkNmLwjLZSlAHvaEK1iyuHL23FI"
TOKEN_PATH = r"C:/Users/jcerv/.config/gws/token.json"
ROOT = Path(__file__).parent
LOG = ROOT / "apply_log.txt"

def log(msg):
    line = f"[{time.strftime('%Y-%m-%d %H:%M:%S')}] {msg}"
    print(line)
    with open(LOG, "a", encoding="utf-8") as f:
        f.write(line + "\n")

def load_creds():
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
    return creds

def find_text_index(doc, target_text):
    """Walk the doc body and return the (start, end) index of the FIRST
    occurrence of target_text. Returns None if not found.

    Used to resolve anchor positions for createFootnote (which needs a
    numeric index, not a search string).
    """
    def walk(content):
        for elem in content:
            if "paragraph" in elem:
                full = ""
                run_starts = []
                for run in elem["paragraph"].get("elements", []):
                    tr = run.get("textRun")
                    if tr:
                        run_starts.append((len(full), run.get("startIndex"), tr.get("content", "")))
                        full += tr.get("content", "")
                if target_text in full:
                    pos = full.index(target_text)
                    end_pos = pos + len(target_text)
                    # Find the run that contains position 'end_pos - 1'
                    for offset_in_full, run_start, txt in run_starts:
                        offset_end = offset_in_full + len(txt)
                        if offset_in_full <= end_pos <= offset_end:
                            api_index = run_start + (end_pos - offset_in_full)
                            return api_index
            elif "table" in elem:
                for row in elem["table"]["tableRows"]:
                    for cell in row["tableCells"]:
                        r = walk(cell.get("content", []))
                        if r is not None:
                            return r
        return None
    return walk(doc["body"]["content"])

def chunk(seq, n):
    for i in range(0, len(seq), n):
        yield seq[i:i+n]

# ---------------------------------------------------------------------------
# Run-level Phase A apply
# ---------------------------------------------------------------------------
# Per Codex round 4, per-half-op deletes were unsafe because multiple
# suggestion IDs can tag the SAME textRun, so independent half-op deletes
# would double-delete overlapping ranges. The fix: walk every textRun
# once, decide a single net action (keep / remove) per run from the
# disposition map, then emit one delete (and optional reinsert+style
# restore) per run. Runs by definition do not overlap with each other.
# ---------------------------------------------------------------------------

# textStyle fields that updateTextStyle accepts. We pass the source
# textStyle verbatim and let unset fields fall back to defaults — which
# is correct, since an unset bold/italic/etc. should reset to default.
_STYLE_FIELDS = (
    "bold,italic,underline,strikethrough,smallCaps,backgroundColor,"
    "foregroundColor,fontSize,weightedFontFamily,baselineOffset,link"
)

def _normalize_del_ids(sd):
    if not sd:
        return []
    if isinstance(sd, dict):
        return list(sd.keys())
    return list(sd)

def _walk_runs(content, out):
    for elem in content:
        if "paragraph" in elem:
            for run in elem["paragraph"].get("elements", []):
                tr = run.get("textRun")
                if tr is not None:
                    out.append((run, tr))
        elif "table" in elem:
            for row in elem["table"]["tableRows"]:
                for cell in row["tableCells"]:
                    _walk_runs(cell.get("content", []), out)

def _decide_run_action(ins_ids, del_ids, dispositions):
    """Return ('keep', None) | ('keep', 'commit') | ('remove', None).

    'keep'+None  → run is unflagged; do nothing.
    'keep'+commit → run kept as committed text; need delete + reinsert
                    (+ style restore) to strip suggestion flag.
    'remove'     → run gone; just delete.

    Conflict policy (a run flagged by multiple sids):
      - any insertion-sid REJECT  → remove (the proposed insert is dropped)
      - any deletion-sid  ACCEPT  → remove (the proposed delete fires)
      - else                       → keep+commit
    """
    if not ins_ids and not del_ids:
        return ("keep", None)
    for sid in ins_ids:
        if dispositions.get(sid, {}).get("disposition") == "reject":
            return ("remove", None)
    for sid in del_ids:
        if dispositions.get(sid, {}).get("disposition") == "accept":
            return ("remove", None)
    return ("keep", "commit")

def build_phase_a_requests(doc, dispositions):
    """Walk doc runs and return (requests_in_apply_order, summary).

    requests_in_apply_order is the exact request list `batchUpdate` will
    receive, sorted descending by primary index with deletes before
    inserts before style-restores at the same index. summary is a
    diagnostic dict for logs and the replay simulator.
    """
    runs = []
    _walk_runs(doc["body"]["content"], runs)

    actions = []  # list of dicts; one per flagged run
    keep_committed = remove_count = unchanged = 0
    unknown_sids = set()
    for run, tr in runs:
        ins_ids = list(tr.get("suggestedInsertionIds") or [])
        del_ids = _normalize_del_ids(tr.get("suggestedDeletionIds"))
        if not (ins_ids or del_ids):
            unchanged += 1
            continue
        for sid in ins_ids + del_ids:
            if sid not in dispositions:
                unknown_sids.add(sid)
        decision, mode = _decide_run_action(ins_ids, del_ids, dispositions)
        if decision == "keep":
            keep_committed += 1
        else:
            remove_count += 1
        actions.append({
            "start": run["startIndex"],
            "end": run["endIndex"],
            "decision": decision,        # "keep" or "remove"
            "mode": mode,                # "commit" or None
            "text": tr.get("content", ""),
            "textStyle": tr.get("textStyle") or {},
            "ins_ids": ins_ids,
            "del_ids": del_ids,
        })

    if unknown_sids:
        raise RuntimeError(
            f"dispositions.json missing entries for: {sorted(unknown_sids)}; "
            f"rebuild dispositions with build_operations_v3.py"
        )

    # Build raw requests with priority tags.
    #   priority 0 = delete,  1 = insert,  2 = updateTextStyle
    raw = []
    for a in actions:
        raw.append((a["start"], 0, {
            "deleteContentRange": {
                "range": {"startIndex": a["start"], "endIndex": a["end"]}
            }
        }))
        if a["decision"] == "keep" and a["mode"] == "commit":
            raw.append((a["start"], 1, {
                "insertText": {"location": {"index": a["start"]}, "text": a["text"]}
            }))
            if a["textStyle"]:
                raw.append((a["start"], 2, {
                    "updateTextStyle": {
                        "range": {
                            "startIndex": a["start"],
                            "endIndex": a["start"] + len(a["text"]),
                        },
                        "textStyle": a["textStyle"],
                        "fields": _STYLE_FIELDS,
                    }
                }))

    # Sort descending primary index. At same index: delete then insert
    # then style restore. (Run starts are unique across non-overlapping
    # textRuns, so secondary keys mostly tiebreak the 3-op packs.)
    raw.sort(key=lambda t: (-t[0], t[1]))
    requests = [t[2] for t in raw]
    summary = {
        "runs_total": len(runs),
        "runs_unchanged": unchanged,
        "runs_kept_committed": keep_committed,
        "runs_removed": remove_count,
        "request_total": len(requests),
        "deletes": sum(1 for r in requests if "deleteContentRange" in r),
        "inserts": sum(1 for r in requests if "insertText" in r),
        "style_restores": sum(1 for r in requests if "updateTextStyle" in r),
        "actions": actions,
    }
    return requests, summary

def run_phase_a(docs, _legacy_ops_unused):
    """Run-level Phase A apply. _legacy_ops_unused is kept positional for
    backwards compatibility with the main() call site; the apply uses
    dispositions.json + a fresh fetch of the live doc.
    """
    log("Phase A: fetching fresh doc state for run-level apply...")
    doc = docs.documents().get(documentId=DOC_ID).execute()
    dispositions = json.loads((ROOT / "dispositions.json").read_text(encoding="utf-8"))

    requests, summary = build_phase_a_requests(doc, dispositions)
    log(f"  runs total: {summary['runs_total']} "
        f"(unchanged {summary['runs_unchanged']}, "
        f"kept-committed {summary['runs_kept_committed']}, "
        f"removed {summary['runs_removed']})")
    log(f"  requests: {summary['request_total']} "
        f"(deletes {summary['deletes']}, inserts {summary['inserts']}, "
        f"style restores {summary['style_restores']})")

    if not requests:
        log("  no flagged runs; nothing to do")
        return

    submitted = 0
    for i, batch in enumerate(chunk(requests, 50), 1):
        for attempt in range(3):
            try:
                docs.documents().batchUpdate(
                    documentId=DOC_ID, body={"requests": batch}
                ).execute()
                submitted += len(batch)
                log(f"  chunk {i}: {len(batch)} requests OK")
                break
            except HttpError as e:
                if e.resp.status in (500, 503) and attempt < 2:
                    log(f"  chunk {i} attempt {attempt+1}: {e.resp.status}, retrying in 5s...")
                    time.sleep(5)
                    continue
                log(f"  chunk {i} FAILED status={e.resp.status}: {e.content[:300]}")
                raise
    log(f"  Phase A done: {submitted}/{len(requests)} requests submitted")

def run_phase_b(docs, ops):
    log(f"Phase B: {len(ops)} operations")
    replace_ops = [o for o in ops if o["type"] == "replaceAllText"]
    footnote_ops = [o for o in ops if o["type"] == "createFootnote"]

    # All replaceAllText in one batch
    if replace_ops:
        ra_requests = [
            {"replaceAllText": {
                "containsText": {"text": o["search"], "matchCase": True},
                "replaceText": o["replace"],
            }}
            for o in replace_ops
        ]
        log(f"  submitting {len(ra_requests)} replaceAllText requests...")
        try:
            resp = docs.documents().batchUpdate(
                documentId=DOC_ID, body={"requests": ra_requests}
            ).execute()
            replies = resp.get("replies", [])
            total_matches = sum(r.get("replaceAllText", {}).get("occurrencesChanged", 0)
                                for r in replies)
            log(f"    OK: {total_matches} total replacements (expected {len(replace_ops)})")
            if total_matches < len(replace_ops):
                log(f"    WARN: some Phase B replaceAllText didn't match")
        except HttpError as e:
            log(f"    ERROR: {e}")
            raise

    # For footnotes, fetch fresh state to resolve indices
    if footnote_ops:
        log(f"  fetching post-Phase-A doc to resolve footnote anchors...")
        doc = docs.documents().get(documentId=DOC_ID).execute()
        for op in footnote_ops:
            anchor = op["anchor_search"]
            idx = find_text_index(doc, anchor)
            if idx is None:
                log(f"    SKIP {op['id']}: anchor not found")
                continue
            log(f"    {op['id']}: anchor at index {idx}")
            try:
                resp = docs.documents().batchUpdate(
                    documentId=DOC_ID,
                    body={"requests": [{
                        "createFootnote": {
                            "location": {"index": idx},
                        }
                    }]},
                ).execute()
                # Get the new footnote ID from the reply, then insert text into it.
                fn_id = resp["replies"][0]["createFootnote"]["footnoteId"]
                log(f"      created footnote {fn_id}; inserting text...")
                # createFootnote produces a footnote segment with an empty
                # paragraph; the first valid insertion index inside that
                # segment is 1 (index 0 is reserved for the segment-start
                # marker). The handoff's old code looked up
                # doc2["footnotes"][fn_id]["content"][0]["startIndex"]
                # which raised KeyError because the structural element's
                # startIndex was sometimes nested deeper. Hard-coding
                # index=1 with segmentId is the documented-and-stable form.
                docs.documents().batchUpdate(
                    documentId=DOC_ID,
                    body={"requests": [{
                        "insertText": {
                            "location": {"index": 1, "segmentId": fn_id},
                            "text": op["footnote_text"],
                        }
                    }]},
                ).execute()
                log(f"      OK")
                # Refresh doc for next anchor lookup (indices have shifted)
                doc = docs.documents().get(documentId=DOC_ID).execute()
            except HttpError as e:
                log(f"      ERROR: {e}")
                raise

def run_phase_c(drive, ops):
    log(f"Phase C: {len(ops)} comments")
    for op in ops:
        try:
            # Drive comments API expects an anchor JSON, but for plain text
            # documents we can just create unanchored comments with content
            # describing the anchor. Anchored comments require the doc be
            # opened in editor mode (anchor IDs are doc-internal).
            # Workaround: prepend the anchor text in the comment body.
            body = f"[anchor: {op['anchor_search'][:60]}]\n\n{op['comment_body']}"
            drive.comments().create(
                fileId=DOC_ID,
                body={"content": body},
                fields="id",
            ).execute()
            log(f"  posted {op['id']}")
        except HttpError as e:
            log(f"  ERROR posting {op['id']}: {e}")
            raise

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--apply", action="store_true",
                    help="Actually call the API; default is dry-run.")
    ap.add_argument("--eyeballed", action="store_true",
                    help="Required with --apply; affirms you reviewed dryrun_apply.txt")
    ap.add_argument("--phase", choices=["A", "B", "C", "all"], default="all",
                    help="Which phase to run (default all)")
    args = ap.parse_args()

    if args.apply and not args.eyeballed:
        sys.exit("Refusing --apply without --eyeballed")

    OPS = json.loads((ROOT / "operations.json").read_text(encoding="utf-8"))
    log(f"=== Run started: phase={args.phase}, apply={args.apply} ===")
    log(f"DOC_ID={DOC_ID}")
    log(f"Phase A: {len(OPS['phaseA'])} ops")
    log(f"Phase B: {len(OPS['phaseB'])} ops")
    log(f"Phase C: {len(OPS['phaseC'])} comments")

    if not args.apply:
        log("DRY RUN — no API calls made. See dryrun_apply.txt for full preview.")
        return

    creds = load_creds()
    docs = build("docs", "v1", credentials=creds)
    drive = build("drive", "v3", credentials=creds)
    if args.phase in ("A", "all"):
        run_phase_a(docs, OPS["phaseA"])
    if args.phase in ("B", "all"):
        run_phase_b(docs, OPS["phaseB"])
    if args.phase in ("C", "all"):
        run_phase_c(drive, OPS["phaseC"])
    log("=== Run complete ===")

if __name__ == "__main__":
    main()
