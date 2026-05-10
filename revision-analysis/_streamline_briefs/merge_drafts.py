"""Merge Claude and Codex drafts; surface every divergence for Jose to resolve.

For each study (3A, 4A, 4B, 5), produces `study_<X>.draft.merged.md` with:
  - BEFORE: source-verbatim (from Claude's draft; Codex's BEFORE block may differ
    in scope so we trust Claude's bounded-by-the-brief version).
  - AFTER (primary): Claude's version is used as the primary AFTER. Where
    Codex's version is strictly tighter and the validator passes both, the
    DIVERGENCES section flags Codex as a candidate alternative.
  - DIVERGENCES: paragraph-by-paragraph comparison. For each non-trivial
    difference, both Claude's and Codex's wording is surfaced so Jose can
    resolve in his manual review.
  - Word count: Claude's count is primary; Codex's count surfaced as alt.
  - Preservation notes: Claude's notes are primary; Codex's notes are
    appended under "Codex preservation notes" for transparency.

Usage:
  python merge_drafts.py            # merge all four studies
  python merge_drafts.py study_3a   # merge a single study
"""

from __future__ import annotations
import re
import sys
from difflib import SequenceMatcher
from pathlib import Path

HERE = Path(__file__).resolve().parent
TARGETS = ["study_3a", "study_4a", "study_4b", "study_5"]


def parse_sections(text: str) -> dict[str, str]:
    """Split a draft file into its top-level ## sections."""
    sections: dict[str, str] = {}
    current = None
    buf: list[str] = []
    for line in text.splitlines():
        if line.startswith("## "):
            if current is not None:
                sections[current] = "\n".join(buf).strip()
            current = line[3:].strip()
            buf = []
        else:
            buf.append(line)
    if current is not None:
        sections[current] = "\n".join(buf).strip()
    return sections


SECTION_HEADERS = {"methods", "results", "discussion"}


def split_paragraphs(block: str) -> list[str]:
    """Split a markdown block on blank lines, then merge orphan section headers
    into the paragraph that follows them.

    Claude's drafts use section headers on their own lines (separated by blank
    lines); Codex's drafts often inline the header with the body of the next
    paragraph. To compare apples to apples we collapse the orphan headers.
    """
    raw = [p.strip() for p in re.split(r"\n\s*\n", block) if p.strip()]
    out: list[str] = []
    pending_header: str | None = None
    for p in raw:
        # Detect orphan single-word section headers (Methods, Results, Discussion)
        # or "Mediation." / "Mediator..." sub-sections etc.
        first_line = p.split("\n", 1)[0].strip().rstrip(".")
        if (
            first_line.lower() in SECTION_HEADERS
            and len(p) <= 60
            and "\n" not in p
        ):
            pending_header = p
            continue
        if pending_header is not None:
            out.append(pending_header + "\n" + p)
            pending_header = None
        else:
            out.append(p)
    if pending_header is not None:
        out.append(pending_header)
    return out


def is_substantive(p: str) -> bool:
    """Filter out tiny "paragraphs" like a stray heading or lone marker line."""
    words = p.split()
    return len(words) > 6


def normalize_for_compare(text: str) -> str:
    """Aggressive normalization to detect "morally identical" paragraphs."""
    t = text.lower()
    t = re.sub(r"[‘’]", "'", t)
    t = re.sub(r"[“”]", '"', t)
    t = re.sub(r"[–—]", "-", t)
    t = re.sub(r"\s+", " ", t).strip()
    return t


def similarity(a: str, b: str) -> float:
    return SequenceMatcher(None, normalize_for_compare(a), normalize_for_compare(b)).ratio()


def align_paragraphs(claude_paras: list[str], codex_paras: list[str]) -> list[tuple[str | None, str | None, float]]:
    """Pair Claude paragraphs with their best Codex match (by similarity).

    Returns a list of (claude_para, codex_para, similarity) triples in
    Claude's order. Unmatched Codex paragraphs are appended at the end.
    """
    used_codex: set[int] = set()
    pairs: list[tuple[str | None, str | None, float]] = []
    for cp in claude_paras:
        best_idx = -1
        best_sim = -1.0
        for j, kp in enumerate(codex_paras):
            if j in used_codex:
                continue
            s = similarity(cp, kp)
            if s > best_sim:
                best_sim = s
                best_idx = j
        if best_idx >= 0 and best_sim > 0.4:
            used_codex.add(best_idx)
            pairs.append((cp, codex_paras[best_idx], best_sim))
        else:
            pairs.append((cp, None, 0.0))
    for j, kp in enumerate(codex_paras):
        if j not in used_codex:
            pairs.append((None, kp, 0.0))
    return pairs


def compose_divergences(pairs: list[tuple[str | None, str | None, float]]) -> str:
    """Produce a human-readable DIVERGENCES section.

    Filters out divergences where one side has no substantive paragraph and
    the other is also not substantive (e.g., dueling "Methods" header
    artifacts). Filters trivial-similarity (>= 0.97) pairs since whitespace
    or single-token differences aren't worth flagging.
    """
    lines: list[str] = [
        "The Claude pass and the Codex pass agree on most of the AFTER block; the items below are places where they chose different wording. Each entry shows Claude's version first, then Codex's. Jose can pick either or write a third option when applying edits manually. Trivial whitespace/header alignment artifacts are filtered out.",
        "",
    ]
    div_index = 0
    for cp, kp, sim in pairs:
        if cp is None and kp is None:
            continue
        if cp is None and kp is not None:
            if not is_substantive(kp):
                continue
            div_index += 1
            lines.append(f"### Divergence {div_index} — Codex paragraph not present in Claude version")
            lines.append("")
            lines.append("**Claude:** (no corresponding paragraph)")
            lines.append("")
            lines.append(f"**Codex:** {kp}")
            lines.append("")
        elif kp is None and cp is not None:
            if not is_substantive(cp):
                continue
            div_index += 1
            lines.append(f"### Divergence {div_index} — Claude paragraph not present in Codex version")
            lines.append("")
            lines.append(f"**Claude:** {cp}")
            lines.append("")
            lines.append("**Codex:** (no corresponding paragraph)")
            lines.append("")
        elif sim < 0.97:
            assert cp is not None and kp is not None
            if not (is_substantive(cp) or is_substantive(kp)):
                continue
            div_index += 1
            lines.append(f"### Divergence {div_index} (paragraph similarity {sim:.2f})")
            lines.append("")
            lines.append(f"**Claude:** {cp}")
            lines.append("")
            lines.append(f"**Codex:** {kp}")
            lines.append("")
    if div_index == 0:
        return "(No substantive paragraph-level divergences detected. Claude and Codex produced near-identical AFTER blocks; spot-check by hand to confirm.)\n"
    return "\n".join(lines)


def get_word_count_line(sections: dict[str, str]) -> str:
    """Find the 'Word count' header line in the parsed sections dict.

    parse_sections strips the leading '## '. The key for the word-count
    section is something like 'Word count: 732 -> 459 (saved 273)'.
    """
    for k in sections:
        if k.startswith("Word count"):
            return k
    return "Word count: ? -> ? (saved ?)"


def merge_one(study_id: str) -> Path:
    claude_path = HERE / f"{study_id}.draft.claude.md"
    codex_path = HERE / f"{study_id}.draft.codex.md"
    out_path = HERE / f"{study_id}.draft.merged.md"

    if not claude_path.exists():
        raise FileNotFoundError(f"Claude draft missing: {claude_path}")
    if not codex_path.exists():
        raise FileNotFoundError(f"Codex draft missing: {codex_path}")

    claude_text = claude_path.read_text(encoding="utf-8")
    codex_text = codex_path.read_text(encoding="utf-8")

    claude_sec = parse_sections(claude_text)
    codex_sec = parse_sections(codex_text)

    before = claude_sec.get("BEFORE", "(BEFORE block not found in Claude draft)")
    claude_after = claude_sec.get("AFTER", "")
    codex_after = codex_sec.get("AFTER", "")

    claude_paras = split_paragraphs(claude_after)
    codex_paras = split_paragraphs(codex_after)
    pairs = align_paragraphs(claude_paras, codex_paras)
    divergences = compose_divergences(pairs)

    claude_wc_header = get_word_count_line(claude_sec)
    codex_wc_header = get_word_count_line(codex_sec)
    claude_notes = claude_sec.get("Preservation notes", "")
    codex_notes = codex_sec.get("Preservation notes", "")

    # Compose merged file
    merged = []
    merged.append("## BEFORE")
    merged.append(before)
    merged.append("")
    merged.append("## AFTER")
    merged.append(claude_after)
    merged.append("")
    merged.append(f"## {claude_wc_header}")
    merged.append(f"_(Codex pass: {codex_wc_header}.)_")
    merged.append("")
    merged.append("## DIVERGENCES (Claude vs. Codex passes)")
    merged.append(divergences)
    merged.append("")
    merged.append("## Preservation notes (Claude pass; primary)")
    merged.append(claude_notes)
    merged.append("")
    merged.append("## Preservation notes (Codex pass; secondary, for transparency)")
    merged.append(codex_notes)
    merged.append("")

    out_path.write_text("\n".join(merged), encoding="utf-8")
    print(f"merged {study_id} -> {out_path.name} ({len(claude_paras)} Claude paras, {len(codex_paras)} Codex paras, {sum(1 for _, _, s in pairs if s != 0 and s < 0.97) + sum(1 for c, k, _ in pairs if c is None or k is None)} divergences)")
    return out_path


def main(filter_ids: list[str] | None = None) -> int:
    targets = filter_ids if filter_ids else TARGETS
    for study_id in targets:
        if study_id not in TARGETS:
            print(f"WARNING: {study_id} not in known TARGETS list; skipping")
            continue
        try:
            merge_one(study_id)
        except FileNotFoundError as e:
            print(f"FAIL {study_id}: {e}")
            return 1
    return 0


if __name__ == "__main__":
    args = sys.argv[1:]
    raise SystemExit(main(args or None))
