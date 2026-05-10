"""Validate operations.json against the live Gdoc JSON.

Phase A validation (per-half index ops):
  - Each op has spans with API indices that must point at runs in the
    live JSON whose textRun.content matches the span's text.
  - The set of (suggestionId, kind) pairs we cover must equal the set
    of (sid, kind) pairs present in the JSON. Missing pairs would mean
    the doc has suggestions we won't touch; extra pairs would mean we
    have ops referencing dead suggestions.

Phase B validation:
  - search strings appear exactly once in the post-Phase-A simulated text
  - createFootnote anchors appear exactly once in the simulated text

Phase C validation:
  - anchor strings appear in the simulated text
"""
from __future__ import annotations
import json, sys, io
from collections import defaultdict
from pathlib import Path

sys.stdout = io.TextIOWrapper(sys.stdout.buffer, encoding="utf-8")

ROOT = Path(__file__).parent
OPS = json.loads((ROOT / "operations.json").read_text(encoding="utf-8"))
DOC = json.loads((ROOT / "gdoc_current.json").read_text(encoding="utf-8"))

def short(t, n=80):
    t = t.replace("\n", " ↵ ")
    return t[:n] + ("…" if len(t) > n else "")

# Build a (start, end) -> {text, ins_ids, del_ids} map for every textRun
runs_by_start = {}
def walk(content):
    for elem in content:
        if "paragraph" in elem:
            for run in elem["paragraph"].get("elements", []):
                tr = run.get("textRun")
                if not tr: continue
                runs_by_start[run["startIndex"]] = {
                    "endIndex": run["endIndex"],
                    "text": tr.get("content", ""),
                    "ins_ids": set(tr.get("suggestedInsertionIds") or []),
                    "del_ids": set((tr.get("suggestedDeletionIds") or {}).keys()
                                   if isinstance(tr.get("suggestedDeletionIds"), dict)
                                   else (tr.get("suggestedDeletionIds") or [])),
                }
        elif "table" in elem:
            for row in elem["table"]["tableRows"]:
                for cell in row["tableCells"]:
                    walk(cell.get("content", []))
walk(DOC["body"]["content"])

# All (sid, kind) pairs in the live JSON
live_pairs = set()
for r in runs_by_start.values():
    for sid in r["ins_ids"]:
        live_pairs.add((sid, "insertion"))
    for sid in r["del_ids"]:
        live_pairs.add((sid, "deletion"))

print(f"=== Phase A validation: per-span index/text matches against live JSON ===")
phase_a_ok = 0
phase_a_fail = []
op_pairs = set()
for op in OPS["phaseA"]:
    sid, kind = op["suggestion_id"], op["kind"]
    op_pairs.add((sid, kind))
    bad_spans = []
    for sp in op["spans"]:
        run = runs_by_start.get(sp["startIndex"])
        if run is None:
            bad_spans.append(("no run at index", sp))
            continue
        if run["endIndex"] != sp["endIndex"]:
            bad_spans.append(("end mismatch", sp, run["endIndex"]))
            continue
        if run["text"] != sp["text"]:
            bad_spans.append(("text mismatch", sp, run["text"][:40]))
            continue
        # confirm sid+kind tag is on the run
        if kind == "insertion" and sid not in run["ins_ids"]:
            bad_spans.append(("not flagged as insertion", sp, list(run["ins_ids"])))
            continue
        if kind == "deletion" and sid not in run["del_ids"]:
            bad_spans.append(("not flagged as deletion", sp, list(run["del_ids"])))
            continue
    if bad_spans:
        phase_a_fail.append((op, bad_spans))
    else:
        phase_a_ok += 1
print(f"  {phase_a_ok}/{len(OPS['phaseA'])} half-ops have all spans valid against live JSON")
if phase_a_fail:
    print(f"  {len(phase_a_fail)} FAILED:")
    for op, bad in phase_a_fail[:5]:
        print(f"    [{op['suggestion_id']} {op['kind']}] {len(bad)} bad spans")
        for b in bad[:3]:
            print(f"      {b}")

missing = live_pairs - op_pairs
extra = op_pairs - live_pairs
print(f"  live (sid, kind) pairs: {len(live_pairs)};  op pairs: {len(op_pairs)}")
print(f"  missing from ops: {len(missing)};  extra in ops: {len(extra)}")
if missing:
    for p in list(missing)[:5]:
        print(f"    MISSING from ops: {p}")
if extra:
    for p in list(extra)[:5]:
        print(f"    EXTRA in ops (no run in live JSON): {p}")

print()
print("=== Phase B validation: search/anchor strings must appear exactly once in POST-A snapshot ===")
POST_A = ROOT / "gdoc_simulated_post_a.txt"
if POST_A.exists():
    SIM = POST_A.read_text(encoding="utf-8")
    phase_b_ok = 0
    phase_b_fail = []
    for op in OPS["phaseB"]:
        target = op.get("search") if op["type"] == "replaceAllText" else op.get("anchor_search")
        if not target:
            continue
        cnt = SIM.count(target)
        if cnt == 1:
            phase_b_ok += 1
        else:
            phase_b_fail.append((op, cnt))
    print(f"  {phase_b_ok}/{len(OPS['phaseB'])} anchors unique in post-A snapshot")
    if phase_b_fail:
        for op, cnt in phase_b_fail:
            target = op.get("search") if op["type"] == "replaceAllText" else op.get("anchor_search")
            print(f"    [{op['id']}] count={cnt} target={short(target)!r}")
else:
    print("  (gdoc_simulated_post_a.txt not yet generated; run simulate_v2.py first)")

print()
print("=== Phase C validation: anchors must appear EXACTLY ONCE in simulated post-apply ===")
POSTAPPLY = ROOT / "gdoc_simulated_postapply.txt"
if POSTAPPLY.exists():
    SIM = POSTAPPLY.read_text(encoding="utf-8")
    phase_c_ok = 0
    phase_c_fail = []
    for op in OPS["phaseC"]:
        cnt = SIM.count(op["anchor_search"])
        if cnt == 1:
            phase_c_ok += 1
        else:
            phase_c_fail.append((op, cnt))
    print(f"  {phase_c_ok}/{len(OPS['phaseC'])} anchors unique in post-apply")
    if phase_c_fail:
        for op, cnt in phase_c_fail:
            status = "MISSING" if cnt == 0 else f"AMBIGUOUS ({cnt} occurrences)"
            print(f"    [{op['id']}] {status}  anchor={short(op['anchor_search'])!r}")
