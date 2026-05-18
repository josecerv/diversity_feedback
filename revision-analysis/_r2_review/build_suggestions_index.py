"""Dump every pending suggestion in the manuscript Gdoc with surrounding context.

Output: revision-analysis/_r2_review/manuscript_pending_suggestions.json
  [
    {
      "suggestion_id": "...",
      "kind": "insertion" | "deletion" | "style",
      "author": "...",            (may be empty - Gdocs doesn't always expose)
      "paragraph_start": 1234,
      "paragraph_end": 1500,
      "section_hint": "Study 1 -> Methods" | "General Discussion" | ...,
      "inserted_text": "...",     (full text of the insertion, or null)
      "deleted_text": "...",      (full text being deleted, or null)
      "context_before": "...",    (50 words of the paragraph before)
      "context_after": "..."      (50 words of the paragraph after)
    },
    ...
  ]

Also produces a markdown summary at manuscript_pending_suggestions.md for quick reading.
"""
from __future__ import annotations

import json
import pathlib
import re

from googleapiclient.discovery import build
from google.oauth2.credentials import Credentials
from google.auth.transport.requests import Request

DOC_ID = "1H9jAvqG5CzQe_BksjkNmLwjLZSlAHvaEK1iyuHL23FI"
TOKEN = pathlib.Path(r"C:/Users/jcerv/.config/gws/token.json")
OUT_DIR = pathlib.Path(
    r"C:/Users/jcerv/Jose/diversity_feedback/revision-analysis/_r2_review"
)
JSON_OUT = OUT_DIR / "manuscript_pending_suggestions.json"
MD_OUT = OUT_DIR / "manuscript_pending_suggestions.md"


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


# Section detection: look for known headings as anchors.
SECTION_HEADINGS = [
    "Abstract",
    "Descriptive Feedback about the Composition",
    "Summary of the Current Research",
    "Overview of Studies",
    "Study 1",
    "Study 2",
    "Study 3A",
    "Study 3B",
    "Study 4A",
    "Study 4B",
    "Study 5",
    "General Discussion",
    "References",
    "Appendix",
]


def paragraph_text(para_el):
    parts = []
    for r in para_el.get("elements", []):
        if "textRun" in r:
            parts.append(r["textRun"].get("content", ""))
    return "".join(parts)


def gather_paragraphs(body_content):
    """Flatten to ordered list of (start, end, text, raw_para)."""
    out = []

    def walk(els):
        for el in els:
            if "paragraph" in el:
                out.append(
                    (
                        el.get("startIndex", 0),
                        el.get("endIndex", 0),
                        paragraph_text(el["paragraph"]),
                        el["paragraph"],
                    )
                )
            elif "table" in el:
                for row in el["table"]["tableRows"]:
                    for cell in row["tableCells"]:
                        walk(cell.get("content", []))

    walk(body_content)
    return out


def section_for_index(idx, section_anchors):
    label = "Front-matter"
    for anchor_idx, anchor_label in section_anchors:
        if idx >= anchor_idx:
            label = anchor_label
    return label


def extract_inserted_text(para_el, suggestion_id):
    parts = []
    for r in para_el.get("elements", []):
        if "textRun" in r:
            tr = r["textRun"]
            if suggestion_id in tr.get("suggestedInsertionIds", []):
                parts.append(tr.get("content", ""))
    return "".join(parts)


def find_subsection(paragraphs, para_idx):
    """Walk backward from this paragraph to find a recent heading line.

    Returns the most specific section breadcrumb (e.g., "Study 1 -> Methods").
    """
    breadcrumbs = []
    # Walk backward
    seen_subheadings = []
    for i in range(para_idx, -1, -1):
        txt = paragraphs[i][2].strip()
        if not txt:
            continue
        # Match major section heading
        for h in SECTION_HEADINGS:
            if txt == h or txt.startswith(h + " "):
                if h not in seen_subheadings:
                    seen_subheadings.append(h)
                break
        # Sub-headings within studies (Methods, Results, Procedure, Discussion, Participants)
        for sub in ("Methods", "Results", "Procedure", "Discussion", "Participants"):
            if txt == sub or txt.startswith(sub + ".") or txt.startswith(sub + ":"):
                if sub not in seen_subheadings:
                    seen_subheadings.append(sub)
                break
        # Stop once we have a major heading
        if seen_subheadings and any(s in SECTION_HEADINGS for s in seen_subheadings):
            break
    if not seen_subheadings:
        return "Front-matter"
    # Order: major heading first, then subheading
    major = next((s for s in seen_subheadings if s in SECTION_HEADINGS), None)
    sub = next((s for s in seen_subheadings if s not in SECTION_HEADINGS), None)
    if major and sub:
        return f"{major} -> {sub}"
    return major or sub or "Front-matter"


def main():
    creds = get_creds()
    docs = build("docs", "v1", credentials=creds)
    doc = docs.documents().get(
        documentId=DOC_ID,
        suggestionsViewMode="SUGGESTIONS_INLINE",
    ).execute()

    paragraphs = gather_paragraphs(doc["body"]["content"])

    # Find section anchor positions: paragraphs whose stripped text equals a known heading.
    section_anchors = []
    for (start, end, txt, _para) in paragraphs:
        t = txt.strip()
        if not t:
            continue
        for h in SECTION_HEADINGS:
            if t == h or (t.startswith(h) and len(t) <= len(h) + 5):
                section_anchors.append((start, h))
                break

    suggestions: dict[str, dict] = {}

    for i, (p_start, p_end, p_text, p_raw) in enumerate(paragraphs):
        # Collect insertion ids in this paragraph
        ins_ids_in_para = set()
        del_ids_in_para = set()
        style_ids_in_para = set()
        for r in p_raw.get("elements", []):
            if "textRun" in r:
                tr = r["textRun"]
                for sid in tr.get("suggestedInsertionIds", []):
                    ins_ids_in_para.add(sid)
                for sid in tr.get("suggestedDeletionIds", []):
                    del_ids_in_para.add(sid)
                for sid in tr.get("suggestedTextStyleChanges", {}).keys():
                    style_ids_in_para.add(sid)

        for sid in ins_ids_in_para:
            inserted = extract_inserted_text(p_raw, sid)
            if not inserted:
                continue
            entry = suggestions.setdefault(
                sid,
                {
                    "suggestion_id": sid,
                    "kind": "insertion",
                    "paragraph_start": p_start,
                    "paragraph_end": p_end,
                    "section_hint": find_subsection(paragraphs, i),
                    "inserted_text": "",
                    "deleted_text": None,
                    "anchor_paragraph_text": p_text.strip(),
                    "context_before": (
                        paragraphs[i - 1][2].strip() if i > 0 else ""
                    )[-400:],
                    "context_after": (
                        paragraphs[i + 1][2].strip() if i + 1 < len(paragraphs) else ""
                    )[:400],
                },
            )
            entry["inserted_text"] += inserted

        for sid in del_ids_in_para:
            # Collect the runs being deleted (content stays in doc until accepted)
            del_parts = []
            for r in p_raw.get("elements", []):
                if "textRun" in r:
                    tr = r["textRun"]
                    if sid in tr.get("suggestedDeletionIds", []):
                        del_parts.append(tr.get("content", ""))
            deleted = "".join(del_parts)
            if not deleted.strip():
                continue
            entry = suggestions.setdefault(
                sid,
                {
                    "suggestion_id": sid,
                    "kind": "deletion",
                    "paragraph_start": p_start,
                    "paragraph_end": p_end,
                    "section_hint": find_subsection(paragraphs, i),
                    "inserted_text": None,
                    "deleted_text": "",
                    "anchor_paragraph_text": p_text.strip(),
                    "context_before": (
                        paragraphs[i - 1][2].strip() if i > 0 else ""
                    )[-400:],
                    "context_after": (
                        paragraphs[i + 1][2].strip() if i + 1 < len(paragraphs) else ""
                    )[:400],
                },
            )
            entry["deleted_text"] = (entry.get("deleted_text") or "") + deleted

        for sid in style_ids_in_para:
            if sid not in suggestions:
                # Style-only suggestion
                suggestions[sid] = {
                    "suggestion_id": sid,
                    "kind": "style",
                    "paragraph_start": p_start,
                    "paragraph_end": p_end,
                    "section_hint": find_subsection(paragraphs, i),
                    "inserted_text": None,
                    "deleted_text": None,
                    "anchor_paragraph_text": p_text.strip(),
                    "context_before": "",
                    "context_after": "",
                }

    ordered = sorted(suggestions.values(), key=lambda d: d["paragraph_start"])

    # Try to merge consecutive insertions in the same paragraph that share a suggestion id
    # (already merged above by setdefault).

    JSON_OUT.write_text(json.dumps(ordered, indent=2, ensure_ascii=False), encoding="utf-8")
    print(f"Wrote {JSON_OUT} ({len(ordered)} suggestions)")

    # Markdown summary
    lines = [f"# Manuscript pending suggestions ({len(ordered)})", ""]
    for s in ordered:
        lines.append(
            f"## [{s['paragraph_start']}] {s['section_hint']} — {s['kind']} `{s['suggestion_id']}`"
        )
        if s["inserted_text"]:
            lines.append(f"**Inserted text:**\n\n> {s['inserted_text'].replace(chr(10), ' ')}")
        if s["deleted_text"]:
            lines.append(f"**Deleted text:**\n\n> {s['deleted_text'].replace(chr(10), ' ')}")
        lines.append("")
        lines.append(
            f"_Anchor paragraph:_ {s['anchor_paragraph_text'][:300].replace(chr(10), ' ')}…"
        )
        lines.append("")
    MD_OUT.write_text("\n".join(lines), encoding="utf-8")
    print(f"Wrote {MD_OUT}")


if __name__ == "__main__":
    main()
