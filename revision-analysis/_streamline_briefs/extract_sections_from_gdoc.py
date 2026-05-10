"""Pull per-study Methods+Results text from the live manuscript Gdoc.

Used by the second streamlining pass on Studies 3A, 4 (4A+4B), and 5.
The first pass relied on stale local excerpts; this script always fetches
the current state from the live doc so the audit and re-draft work from
ground truth.

Outputs:
- _v2_2026-05-10/study_1_full.txt
- _v2_2026-05-10/study_2_full.txt
- _v2_2026-05-10/study_3a_methods_results.txt
- _v2_2026-05-10/study_3b_methods_results.txt
- _v2_2026-05-10/study_3_discussion.txt
- _v2_2026-05-10/study_4_full.txt
- _v2_2026-05-10/study_5_full.txt
- _v2_2026-05-10/table_2_consolidation.txt    (the new cross-study table)
- _v2_2026-05-10/_pending_suggestions.txt     (any suggestions still pending in audited sections)

Section boundaries are heading-text driven so the extractor survives
re-numbering (e.g., "5.1 Study 4A" vs "Study 4A").
"""

from __future__ import annotations
import io
import json
import sys
from datetime import date
from pathlib import Path

sys.stdout = io.TextIOWrapper(sys.stdout.buffer, encoding="utf-8")

from google.oauth2.credentials import Credentials
from google.auth.transport.requests import Request
from googleapiclient.discovery import build

DOC_ID = "1H9jAvqG5CzQe_BksjkNmLwjLZSlAHvaEK1iyuHL23FI"
TOKEN_PATH = r"C:/Users/jcerv/.config/gws/token.json"
HERE = Path(__file__).resolve().parent
OUT_DIR = HERE / "_source_excerpts" / "_v2_2026-05-10"
OUT_DIR.mkdir(parents=True, exist_ok=True)


def _creds():
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


def fetch_doc():
    docs = build("docs", "v1", credentials=_creds())
    return docs.documents().get(
        documentId=DOC_ID,
        suggestionsViewMode="DEFAULT_FOR_CURRENT_ACCESS",
    ).execute()


def heading_text(el):
    """Return (level, text) for a paragraph-style heading or (None, None)."""
    if "paragraph" not in el:
        return None, None
    style = el["paragraph"].get("paragraphStyle", {}).get("namedStyleType", "")
    if not ("HEADING" in style or "TITLE" in style):
        return None, None
    text = "".join(
        run.get("textRun", {}).get("content", "")
        for run in el["paragraph"].get("elements", [])
    ).strip()
    return style, text


def paragraph_text(el):
    """Return the stripped text of any paragraph element, or '' for non-paragraphs."""
    if "paragraph" not in el:
        return ""
    return "".join(
        run.get("textRun", {}).get("content", "")
        for run in el["paragraph"].get("elements", [])
    ).strip()


def render_paragraph(el, pending_warn):
    """Render a paragraph element to plain text. Records pending suggestions."""
    if "paragraph" not in el:
        return ""
    out = []
    for run in el["paragraph"].get("elements", []):
        tr = run.get("textRun")
        if not tr:
            continue
        text = tr.get("content", "")
        ins = tr.get("suggestedInsertionIds")
        dele = tr.get("suggestedDeletionIds")
        if dele:
            pending_warn.append(("DELETION", text.strip()[:120]))
            continue  # skip suggested deletions (they don't appear in the rendered doc)
        if ins:
            pending_warn.append(("INSERTION", text.strip()[:120]))
        out.append(text)
    return "".join(out)


def render_table(el, pending_warn):
    if "table" not in el:
        return ""
    rows = []
    for row in el["table"].get("tableRows", []):
        cells = []
        for cell in row.get("tableCells", []):
            cell_text = []
            for inner in cell.get("content", []):
                cell_text.append(render_paragraph(inner, pending_warn))
            cells.append("".join(cell_text).strip())
        rows.append(" | ".join(cells))
    return "[TABLE]\n" + "\n".join(rows) + "\n[/TABLE]\n"


def render_element(el, pending_warn):
    if "paragraph" in el:
        return render_paragraph(el, pending_warn)
    if "table" in el:
        return render_table(el, pending_warn)
    if "sectionBreak" in el:
        return "\n[SECTION BREAK]\n"
    return ""


def find_heading_index(content, target_text, *, level_filter=None, start_at=0):
    """Find the first content index whose heading text equals target_text.

    Match is case-insensitive and ignores extra whitespace. ``level_filter``
    may be a string like ``"HEADING_1"`` to restrict to that level.
    """
    norm_target = target_text.strip().lower()
    for i in range(start_at, len(content)):
        style, text = heading_text(content[i])
        if not style:
            continue
        if level_filter and style != level_filter:
            continue
        if text.strip().lower() == norm_target:
            return i
    return -1


def find_heading_endswith(content, suffix, *, start_at=0):
    """Find first heading whose text ends with the given suffix (case-insensitive)."""
    s = suffix.strip().lower()
    for i in range(start_at, len(content)):
        style, text = heading_text(content[i])
        if not style:
            continue
        if text.strip().lower().endswith(s):
            return i
    return -1


def find_section_boundary(content, target_text, *, start_at=0):
    """Find the first paragraph whose stripped text equals ``target_text``.

    Prefers heading-styled paragraphs; falls back to NORMAL_TEXT paragraphs if
    no heading match is found. This is needed because some section labels
    (notably "Study 4") live as NORMAL_TEXT paragraphs in the live doc.
    """
    norm = target_text.strip().lower()
    # First pass: heading-styled match
    for i in range(start_at, len(content)):
        style, text = heading_text(content[i])
        if style and text.strip().lower() == norm:
            return i
    # Fallback: any paragraph with exact stripped text
    for i in range(start_at, len(content)):
        if paragraph_text(content[i]).lower() == norm:
            return i
    return -1


def render_range(content, start, end, pending_warn):
    """Concatenate rendered text for content[start:end]."""
    return "".join(render_element(content[i], pending_warn) for i in range(start, end))


def extract_table_2_consolidation(content):
    """Find and render the new cross-study Table 2 by header signature.

    The new Table 2 has first-row cells beginning with 'Study (N)' and
    'Identity-Target Group'. The OTHER Table 2 (Study 1 OLS regressions)
    starts with 'Independent Regression Equations'.
    """
    pending_warn = []
    for i, el in enumerate(content):
        if "table" not in el:
            continue
        first_row_cells = []
        for cell in el["table"]["tableRows"][0].get("tableCells", []):
            cb = []
            for inner in cell.get("content", []):
                cb.append(render_paragraph(inner, pending_warn))
            first_row_cells.append("".join(cb).strip())
        if any("Study (N)" in c for c in first_row_cells) and any(
            "Identity-Target" in c for c in first_row_cells
        ):
            # also pull the caption from the preceding heading or paragraph
            caption = ""
            for j in range(max(0, i - 5), i):
                if "paragraph" in content[j]:
                    txt = render_paragraph(content[j], pending_warn)
                    if "Table" in txt:
                        caption = txt.strip()
                        break
            return i, caption, render_table(el, pending_warn)
    return -1, "", ""


def main():
    print("Fetching live Gdoc...")
    doc = fetch_doc()
    content = doc["body"]["content"]
    print(f"  body has {len(content)} elements")

    pending_warn = []

    # Index every heading once for visibility
    print("\nHeading map:")
    for i, el in enumerate(content):
        _, text = heading_text(el)
        if text:
            print(f"  [{i:3d}] {text!r}")

    # --- Section boundaries -----------------------------------------------
    h_study_1 = find_heading_index(content, "Study 1", level_filter="HEADING_1")
    h_study_2 = find_heading_index(content, "Study 2", level_filter="HEADING_1")
    h_study_3 = find_heading_index(content, "Study 3", level_filter="HEADING_1")
    h_study_3a = find_heading_index(content, "Study 3A")
    h_study_3b = find_heading_index(content, "Study 3B")
    # the shared Study-3 Discussion comes after 3B Results, before Study 4
    # Find the FIRST Discussion heading at-or-after 3B
    h_study_3_disc = -1
    for i in range(h_study_3b + 1, len(content)):
        style, text = heading_text(content[i])
        if style and text and text.strip().lower() == "discussion":
            h_study_3_disc = i
            break
    # Study 4 lead-in lives as a NORMAL_TEXT paragraph "Study 4" (NOT a heading)
    # in the live doc; heading-only search misses it. Use the broader matcher.
    h_study_4_leadin = find_section_boundary(content, "Study 4", start_at=h_study_3b)
    h_study_4a = find_heading_endswith(content, "Study 4A")
    h_study_4b = find_heading_endswith(content, "Study 4B")
    h_study_5 = find_heading_endswith(content, "Study 5")
    h_benchmark = find_heading_index(content, "Benchmarking Study: Descriptive Feedback vs. Quotas to Promote Diversity")

    print("\nSection anchors:")
    for label, idx in [
        ("Study 1", h_study_1),
        ("Study 2", h_study_2),
        ("Study 3", h_study_3),
        ("Study 3A", h_study_3a),
        ("Study 3B", h_study_3b),
        ("Study 3 Discussion", h_study_3_disc),
        ("Study 4 lead-in", h_study_4_leadin),
        ("Study 4A", h_study_4a),
        ("Study 4B", h_study_4b),
        ("Study 5", h_study_5),
        ("Benchmarking", h_benchmark),
    ]:
        print(f"  {label:24s} -> [{idx}]")

    if min([h_study_1, h_study_2, h_study_3a, h_study_3b, h_study_4a, h_study_4b, h_study_5]) < 0:
        raise SystemExit("Missing one or more required section anchors. Inspect heading map.")

    # The Study 3 Discussion ends just before the Study 4 lead-in (if found)
    # or just before the Study 4A heading. Same for Study 4 source: it begins
    # at the Study 4 lead-in (when present) so the mechanism + comparison-
    # attributes framing is preserved.
    study_3_disc_end = h_study_4_leadin if h_study_4_leadin > 0 else h_study_4a
    study_4_start = h_study_4_leadin if h_study_4_leadin > 0 else h_study_4a

    # --- Render each section ---------------------------------------------
    sections = {
        "study_1_full.txt": (h_study_1, h_study_2),
        "study_2_full.txt": (h_study_2, h_study_3),
        "study_3a_methods_results.txt": (h_study_3a, h_study_3b),
        "study_3b_methods_results.txt": (h_study_3b, h_study_3_disc if h_study_3_disc > 0 else study_4_start),
        "study_3_discussion.txt": (h_study_3_disc, study_3_disc_end) if h_study_3_disc > 0 else None,
        "study_4_full.txt": (study_4_start, h_study_5),
        "study_5_full.txt": (h_study_5, h_benchmark if h_benchmark > 0 else len(content)),
    }

    for filename, span in sections.items():
        if span is None:
            print(f"\n  SKIP {filename} (anchors not found)")
            continue
        start, end = span
        local_warn: list = []
        text = render_range(content, start, end, local_warn)
        out = OUT_DIR / filename
        out.write_text(text, encoding="utf-8")
        # Word count for sanity
        wc = len(text.split())
        print(f"\n  wrote {filename}: {wc} words; {len(text)} chars")
        if local_warn:
            print(f"    {len(local_warn)} pending suggestion fragments in this range")
            pending_warn.extend((filename, *p) for p in local_warn)

    # --- Extract the new Table 2 -----------------------------------------
    idx, caption, body = extract_table_2_consolidation(content)
    table2_path = OUT_DIR / "table_2_consolidation.txt"
    if idx < 0:
        table2_path.write_text("(consolidation table not found)\n", encoding="utf-8")
        print("\n  WARNING: cross-study Table 2 not located")
    else:
        table2_path.write_text(f"{caption}\n\n{body}", encoding="utf-8")
        print(f"\n  Table 2 (consolidation) at content[{idx}]")
        print(f"    caption: {caption[:140]}")
        print(f"    wrote {table2_path.name}")

    # --- Pending-suggestion log -------------------------------------------
    if pending_warn:
        log = OUT_DIR / "_pending_suggestions.txt"
        with log.open("w", encoding="utf-8") as f:
            for entry in pending_warn:
                f.write(repr(entry) + "\n")
        print(f"\n  {len(pending_warn)} pending suggestion fragments logged to {log.name}")
    else:
        print("\n  No pending suggestions in extracted ranges")

    print(f"\nDone. Output dir: {OUT_DIR}")
    print(f"Extraction date: {date.today().isoformat()}")


if __name__ == "__main__":
    main()
