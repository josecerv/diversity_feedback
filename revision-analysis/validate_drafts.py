"""Validate the streamlined drafts for replication-grade detail integrity.

Updated 2026-05-10:
- Accepts Wald result either as a verbose `(1)... (2)... (3)...` enumeration
  OR as a single sentence + Table 2 pointer (the new 2026-05-10 amendment).
  Strict verbose enforcement remains for Study 1 if validated.
- Iterates all known draft variants per study: `.draft.md` (legacy),
  `.draft.claude.md`, `.draft.codex.md`, `.draft.merged.md`.
- Adds compliance checks for the strict-consistency rules surfaced in
  RULES_DISCOVERED.md: Materials Section closing, "where their prior
  selections were highlighted" procedural detail, em/en-dash sanity check.
- CLI: `python validate_drafts.py [study_id ...]` to filter; default = all.

Per-section AFTER block must preserve:
  - Demographics line (gender splits, Mage)
  - Balance-check sentence (with balance / balance across / numeric p next to %)
  - Primary effect statistics (B, 95% CI, t, p)
  - Wald result (verbose enumeration OR Table 2 pointer; per the 2026-05-10
    amendment in TEMPLATE_MOVES)
  - Mediation alpha values for Studies 4A / 4B
  - Fairness mediator + 2x2 mention for Study 5
  - Reversal interpretation for Study 5 Discussion
  - "where their prior selections were highlighted" procedural detail (Studies
    3A, 4A, 4B, 5)
  - "See Materials Section: Study X in the Appendix" closing (Studies 1, 3A,
    3B, 4A, 4B, 5)
  - No em/en dashes in newly authored AFTER prose (warning only — source
    quotes in BEFORE may legitimately have them)
"""

from __future__ import annotations
import re
import sys
from pathlib import Path

HERE = Path(__file__).resolve().parent
BRIEFS_DIR = HERE / "_streamline_briefs"

# Pattern for the new Table 2 reference rule.
WALD_TABLE2_POINTER = re.compile(
    r"see\s+Table\s+2[^.]*?(cross[\s-]?study|consolidation|estimates|Wald)",
    re.IGNORECASE,
)

CHECKS = {
    "study_3a": [
        ("demographics line", r"(\d+\.\d+%\s+(men|women)|Mage\s*=\s*\d)"),
        ("balance check sentence", r"balance\s+(across|with)|with\s+balance"),
        ("primary effect B/CI", r"\bB\s*=\s*[-\d\.]+"),
        ("primary effect 95% CI", r"95%\s*CI\s*="),
        ("Wald result (verbose OR Table 2 pointer)", "WALD_OR_TABLE2"),
        ("DV named (cell mean phrase)", r"chose\s+a\s+\w+|featuring\s+a\s+racial[\s-]?minority"),
        ("Materials Section closing", r"See\s+Materials\s+Section:\s+Study\s+3A\s+in\s+the\s+Appendix"),
        ("highlighted prior selections", r"prior\s+selections\s+were\s+highlighted"),
    ],
    "study_3_discussion": [
        ("demand-effect framing", r"demand[\s-]?effect"),
        ("S2A/S2B Appendix pointer", r"Appendix\s+Table[s]?\s+S1[78]|Appendix\s+Tables\s+S\d+|interaction\s+p\s*[>≥]"),
    ],
    "study_4a": [
        ("demographics line", r"(\d+\.\d+%\s+(men|women)|Mage\s*=\s*\d)"),
        ("balance check sentence", r"balance\s+(across|with)|with\s+balance|p\s*=\s*\.\d{3}"),
        ("primary effect B/CI", r"\bB\s*=\s*[-\d\.]+"),
        ("Wald result (verbose OR Table 2 pointer)", "WALD_OR_TABLE2"),
        ("Cronbach alpha values", r"(α|alpha|Cronbach)\s*=?\s*0?\.[789]"),
        ("Sobel test", r"Sobel"),
        ("ACME or indirect effect", r"ACME|indirect\s+effect|average\s+causal\s+mediation"),
        ("multiple mediation", r"multiple\s+mediation|simultaneous"),
        ("Materials Section closing", r"See\s+Materials\s+Section:\s+Study\s+4A\s+in\s+the\s+Appendix"),
        ("highlighted prior selections", r"prior\s+selections\s+were\s+highlighted"),
    ],
    "study_4b": [
        ("demographics line", r"(\d+\.\d+%\s+(men|women)|Mage\s*=\s*\d)"),
        ("balance check sentence", r"balance\s+(across|with)|with\s+balance|p\s*=\s*\.\d{3}"),
        ("primary effect B/CI", r"\bB\s*=\s*[-\d\.]+"),
        ("Wald result (verbose OR Table 2 pointer)", "WALD_OR_TABLE2"),
        ("Cronbach alpha values", r"(α|alpha|Cronbach)\s*=?\s*0?\.[789]"),
        ("Sobel test", r"Sobel"),
        ("ACME or indirect effect", r"ACME|indirect\s+effect|average\s+causal\s+mediation"),
        ("multiple mediation", r"multiple\s+mediation|simultaneous"),
        ("pretest with appendix pointer", r"pretest|pre-test"),
        ("21 attributes mention OR appendix pointer", r"21\s+(candidate|attributes)|Appendix\s+Section"),
        ("Materials Section closing", r"See\s+Materials\s+Section:\s+Study\s+4B\s+in\s+the\s+Appendix"),
        ("highlighted prior selections", r"prior\s+selections\s+were\s+highlighted"),
    ],
    "study_4_discussion": [
        ("internal MRWP mediates", r"internal\s+(motivation|MRWP)"),
        ("external MRWP non-mediator", r"external\s+(motivation|MRWP)"),
    ],
    "study_5": [
        ("demographics line (Prolific)", r"(\d+\.\d+%\s+(men|women|non[\s-]?binary)|Mage\s*=\s*\d)"),
        ("2x2 design mention", r"2\s*[x×X]\s*2|2\s*\([^)]+\)\s*[x×X]\s*2|two[\s-]?by[\s-]?two"),
        ("pooled interaction analysis", r"pool|meta[\s-]?analysis|random[\s-]?effects"),
        ("18M/6W or 18W/6M conditions", r"18\s*(men|M|women|W)|6\s*(men|M|women|W)"),
        ("fairness concerns mediator", r"fairness"),
        ("primary effect B/CI", r"\bB\s*=\s*[-\d\.]+"),
        ("balance check sentence (multi-cell)", r"balance\s+(across|with)|with\s+balance|t\(\d"),
        ("Wald z-test (study-specific)", r"Wald\s+(Test|z[-\s]?test)|z\s*=\s*\d"),
        ("Materials Section closing", r"See\s+Materials\s+Section:\s+Study\s+5\s+in\s+the\s+Appendix"),
        ("highlighted prior selections", r"prior\s+selections\s+were\s+highlighted"),
    ],
    "study_5_discussion": [
        ("reversal interpretation", r"revers|opposite|reduce[ds]?\s+selection"),
        ("fairness explanation", r"fairness"),
    ],
}

DRAFT_VARIANTS = [".draft.merged.md", ".draft.claude.md", ".draft.codex.md", ".draft.md"]


def parse_after(text: str) -> tuple[str, str, int, int]:
    """Return (after_block, notes_block, wc_before, wc_after)."""
    after_lines: list[str] = []
    notes_lines: list[str] = []
    cursor = None
    wc_before = wc_after = 0
    for line in text.splitlines():
        s = line.strip()
        if s.startswith("## AFTER"):
            cursor = "after"; continue
        if s.startswith("## Word count"):
            cursor = None
            m = re.search(r"([\d,]+)\s*->\s*([\d,]+)", s)
            if m:
                wc_before = int(m.group(1).replace(",", ""))
                wc_after = int(m.group(2).replace(",", ""))
            continue
        if s.startswith("## Preservation notes"):
            cursor = "notes"; continue
        if s.startswith("## "):
            cursor = None; continue
        if cursor == "after":
            after_lines.append(line)
        elif cursor == "notes":
            notes_lines.append(line)
    return "\n".join(after_lines).strip(), "\n".join(notes_lines).strip(), wc_before, wc_after


def has_verbose_wald(after: str) -> bool:
    """True if the AFTER block has a verbose Wald F-test enumeration.

    Heuristic: at least three matches of `F(1, ...)` patterns.
    """
    return len(re.findall(r"F\(\s*1", after)) >= 3


def has_table2_pointer(after: str) -> bool:
    return bool(WALD_TABLE2_POINTER.search(after))


def evaluate_check(label: str, pattern: str | None, after: str) -> tuple[bool, str]:
    """Return (ok, note) for a single check entry."""
    if pattern == "WALD_OR_TABLE2":
        verbose = has_verbose_wald(after)
        pointer = has_table2_pointer(after)
        if verbose and pointer:
            return True, "verbose+pointer (both)"
        if verbose:
            return True, "verbose enumeration"
        if pointer:
            return True, "Table 2 pointer (compressed)"
        return False, "no verbose Wald and no Table 2 pointer"
    assert pattern is not None
    hits = re.findall(pattern, after, flags=re.IGNORECASE)
    if not hits:
        return False, "no match"
    return True, f"{len(hits)} match{'es' if len(hits) != 1 else ''}"


def em_dash_warnings(after: str) -> list[str]:
    """Find em/en dashes in newly authored AFTER prose; return contextual warnings.

    Note: the AFTER block may legitimately quote source phrases that contain
    en-dashes (e.g., "from the original set–where prior selections were
    highlighted–to include in the display"). Warnings only — not failures.
    """
    warnings: list[str] = []
    for m in re.finditer(r"[–—]", after):
        start = max(0, m.start() - 30)
        end = min(len(after), m.end() + 30)
        ctx = after[start:end].replace("\n", " ")
        warnings.append(f"  ... {ctx} ...")
    return warnings


def find_drafts(study_id: str) -> list[Path]:
    """Return all draft variants found for a study, ordered by preference."""
    out: list[Path] = []
    for suffix in DRAFT_VARIANTS:
        p = BRIEFS_DIR / f"{study_id}{suffix}"
        if p.exists():
            out.append(p)
    return out


def validate_one(study_id: str, draft_path: Path, checks: list) -> int:
    """Validate a single draft. Return number of failed checks."""
    text = draft_path.read_text(encoding="utf-8")
    after, _notes, wb, wa = parse_after(text)
    saved = wb - wa if wb else 0
    print(f"\n[ {study_id} | {draft_path.name} ]  {wb} -> {wa}  (saved {saved})")
    failures = 0
    for label, pattern in checks:
        ok, note = evaluate_check(label, pattern, after)
        mark = "OK  " if ok else "FAIL"
        if not ok:
            failures += 1
        print(f"  [{mark}] {label} ({note})")
    em_warnings = em_dash_warnings(after)
    if em_warnings:
        print(f"  [WARN] {len(em_warnings)} em/en dash(es) in AFTER prose:")
        for w in em_warnings[:3]:
            print(w)
        if len(em_warnings) > 3:
            print(f"    ... and {len(em_warnings) - 3} more")
    return failures


def main(filter_ids: list[str] | None = None) -> int:
    total_before = total_after = 0
    failures = 0
    drafts_validated = 0
    print("=" * 80)
    studies = list(CHECKS.keys()) if not filter_ids else [s for s in CHECKS if s in filter_ids]
    for study_id in studies:
        drafts = find_drafts(study_id)
        if not drafts:
            print(f"\n[ {study_id} ] NO DRAFTS FOUND (looked for {DRAFT_VARIANTS})")
            continue
        for draft in drafts:
            failures += validate_one(study_id, draft, CHECKS[study_id])
            drafts_validated += 1
            text = draft.read_text(encoding="utf-8")
            _after, _notes, wb, wa = parse_after(text)
            total_before += wb; total_after += wa
    print("\n" + "=" * 80)
    print(f"DRAFTS VALIDATED: {drafts_validated}")
    print(f"GRAND TOTAL words: {total_before} -> {total_after} (saved {total_before - total_after})")
    print(f"FAILURES: {failures}")
    print("=" * 80)
    return failures


if __name__ == "__main__":
    args = sys.argv[1:]
    raise SystemExit(0 if main(args or None) == 0 else 1)
