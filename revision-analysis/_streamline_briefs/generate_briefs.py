"""Generate the per-study Codex briefs from TEMPLATE_MOVES.md + RULES_DISCOVERED.md + source excerpts.

Run from repo root or revision-analysis/_streamline_briefs/. Writes one brief
file per target into the same directory.

Updated 2026-05-10:
- Source excerpts now pulled from `_v2_2026-05-10/` (refreshed from live Gdoc)
- TEMPLATE_MOVES.md amended for the new Table 2 reference rule
- RULES_DISCOVERED.md inlined into briefs as a second authoritative ruleset
- Each brief now references the prior post-Katy draft as a calibration anchor
- Each brief notes the optional emphasis (consistency vs. holistic flow) for parallel passes
"""

from pathlib import Path

HERE = Path(__file__).resolve().parent
TEMPLATE = (HERE / "TEMPLATE_MOVES.md").read_text(encoding="utf-8")
RULES = (HERE / "RULES_DISCOVERED.md").read_text(encoding="utf-8")
SRC = HERE / "_source_excerpts" / "_v2_2026-05-10"
TABLE_2 = (SRC / "table_2_consolidation.txt").read_text(encoding="utf-8").strip()


def src(name: str) -> str:
    return (SRC / f"{name}.txt").read_text(encoding="utf-8").strip()


def prior_draft(study_id: str) -> str:
    """Return the prior post-Katy draft text or a stub if missing."""
    p = HERE / f"{study_id}.draft.md"
    if p.exists():
        return p.read_text(encoding="utf-8").strip()
    return "(no prior draft on file)"


PREAMBLE_BASE = """\
You are streamlining a section of the manuscript "Does Feedback Enhance Diversity" (R2 revision under Wu).

Background. Studies 1, 2, and 3B Methods+Results have already been streamlined and accepted in the live Google Doc as of 2026-05-09 after a back-and-forth with the advisor (Katherine Milkman, "Katy") and co-author (Sophia Pink). Sophia's earlier May-7 pass was over-aggressive; Katy reverted several cuts on 2026-05-09. A first pass on Studies 3A, 4 (4A+4B), and 5 was attempted afterward, and Katy reverted parts of that pass too. This is the SECOND pass on Studies 3A, 4A, 4B, and 5 — calibrated to (a) the rules Katy enforced, (b) implicit rules surfaced by line-by-line audit of the accepted post-Katy text (see `RULES_DISCOVERED.md`), and (c) a NEW cross-study Table 2 that consolidates focal-vs-comparison Wald-test results, allowing Studies 3A/4A/4B to point at Table 2 instead of repeating the verbose `(1)... (2)... (3)...` enumeration.

Your job. Apply both TEMPLATE_MOVES.md and RULES_DISCOVERED.md to the verbatim SOURCE text and produce a faithful streamlined version. Honor every "DON'T" rule. Preserve replication-grade detail (demographics, balance check, mediation detail where present). For Studies 3A, 4A, 4B: replace the verbose primary Wald F-test enumeration with a single sentence + Table 2 pointer per the 2026-05-10 amendment in TEMPLATE_MOVES.md. For Study 5 only, keep its 2x2 Wald *z*-test in full. Cut redundancy. Avoid em/en dashes in newly authored prose (use commas, parentheses, or sentence breaks); preserve dashes only when transcribing source verbatim in BEFORE.

Calibration. The first pass was reverted as too aggressive on certain axes. Cut more on different axes (per § VI of RULES_DISCOVERED.md, "Candidate cuts the first pass missed"). Don't repeat the cuts the first pass made that triggered Katy's reverts.

Required output. Exactly four top-level markdown sections, in this order, and nothing else:

## BEFORE
[Paste the SOURCE text verbatim. Do not modify a single character.]

## AFTER
[Your streamlined version following TEMPLATE_MOVES + RULES_DISCOVERED + the study-specific preservation rules below.]

## Word count: <before-word-count> -> <after-word-count> (saved <delta>)

## Preservation notes
- One bullet per non-trivial decision: what you cut, what you kept, what you compressed.
- Cite Katy's red lines explicitly when applicable: "kept demographics per Katy red line #1", etc.
- For each cut from § VI of RULES_DISCOVERED.md you accepted or rejected, state which and why.
- Note any place where you departed from the template and why.
- Note any DIVERGENCE you'd like Jose to resolve manually (single sentence per).

Do not write anything outside the four sections above. Do not write any files. Do not run any tools. Output only the final assistant message.
"""

EMPHASIS_CONSISTENCY = """\
EMPHASIS for this pass: CONSISTENCY + FIRST-MENTION DISCIPLINE.
Your priority is uniform application of every strict-consistency rule in RULES_DISCOVERED.md (§ I), the cross-study first-mention map (§ III), and Katy's explicit red lines. Where the source departs from the canonical pattern, normalize toward it. Prefer keeping the prior post-Katy draft's wording when it already complies, only changing what's needed to apply the new Table 2 rule and any implicit rules the prior draft missed.
"""

EMPHASIS_FLOW = """\
EMPHASIS for this pass: HOLISTIC READER FLOW + ADDITIONAL CUTS.
Your priority is reader experience across the manuscript as a whole and finding cuts the first pass missed without re-triggering Katy's reverts. Read § VI of RULES_DISCOVERED.md ("Candidate cuts the first pass missed") and accept or reject each candidate cut explicitly. Look for sentences that repeat content the reader has just absorbed in a prior study. Tighten transitions and verb choices where the prior draft is wordy. Do not violate any rule in § I-V or any Katy red line.
"""


def make_brief(
    study_id: str,
    study_name: str,
    source_name: str,
    study_rules: list[str],
    *,
    emphasis: str = "consistency",
) -> str:
    rules_md = "\n".join(f"- {r}" for r in study_rules)
    body = src(source_name)
    prior = prior_draft(study_id)
    emphasis_block = EMPHASIS_CONSISTENCY if emphasis == "consistency" else EMPHASIS_FLOW
    return f"""# Brief: streamline {study_name}

{PREAMBLE_BASE}
{emphasis_block}

---

# TEMPLATE_MOVES.md (the accepted post-Katy moves; amended 2026-05-10 for Table 2)

{TEMPLATE}

---

# RULES_DISCOVERED.md (line-by-line audit of post-Katy text; new 2026-05-10)

{RULES}

---

# STUDY-SPECIFIC PRESERVATION RULES for {study_name}

{rules_md}

---

# CROSS-STUDY TABLE 2 (consolidation; reference target for Studies 3A, 4A, 4B)

{TABLE_2}

---

# PRIOR POST-KATY DRAFT for {study_name} (calibration anchor; do NOT copy verbatim — use only as a comparison reference)

{prior}

---

# SOURCE (verbatim from live Gdoc, refreshed 2026-05-10; do not modify)

{body}
"""


BRIEFS = [
    {
        "study_id": "study_3a",
        "study_name": "Study 3A Methods + Results",
        "source": "study_3a_methods_results",
        "rules": [
            "Race-feedback paradigm (NOT gender). Focal DV is racial-minority protagonist selection. Keep racial-minority terminology distinct.",
            "Stimulus set: 7 initial films, 4 of 25 candidate films feature racial-minority protagonists (16% baseline).",
            "**NEW RULE (2026-05-10):** Replace the verbose primary Wald F-test enumeration with a single sentence + Table 2 pointer per the 2026-05-10 amendment in TEMPLATE_MOVES.md. Pattern: 'The effect of assignment to the race feedback condition on subsequent selection of a racial-minority protagonist was significantly larger than the effects of assignment to receive descriptive feedback about each of the three comparison attributes (all p's <= .X; see Table 2 for cross-study estimates and Wald tests).' Do NOT keep the verbose `(1)... (2)... (3)...` form.",
            "Demographics line per RULES_DISCOVERED § I.3 (semicolon-delimited, gender-then-Mage-then-race). Match the order in the source (men first, women second).",
            "Balance-check sentence per RULES_DISCOVERED § I.8 — Study 3A initial selections are clean-balanced, so use Study 3B's compressed form: 'Initial selections featured racial-minority protagonists X.X% of the time, with balance across conditions (p = X.XXX; see Appendix Table SX).' Do NOT include cell means, t, or 95% CI in main text.",
            "No separate Discussion for 3A: the Study 3 Discussion is shared with 3B at the end of Study 3 and will be streamlined separately.",
            "Preserve verbatim: 'from the original set, where their prior selections were highlighted' (Katy red line #4).",
            "Preserve verbatim: 'See Materials Section: Study 3A in the Appendix for complete study materials.' (Methods closing line).",
            "Avoid em/en dashes in newly authored AFTER prose; commas/parentheses/sentence breaks instead. The BEFORE block must be source-verbatim.",
        ],
    },
    {
        "study_id": "study_3_discussion",
        "study_name": "Study 3 Discussion (covers 3A + 3B + supplementary S2A/S2B demand-effect ruling-out)",
        "source": "study_3_discussion",
        "rules": [
            "This is a Discussion (not Methods/Results). The four required output sections still apply, but the AFTER content is a discussion paragraph cluster.",
            "Preserve the demand-effect framing: this is the contribution of Studies 3A/3B + S2A/S2B together. Do not weaken it.",
            "Compress the S2A/S2B stat enumeration (Studies S2A and S2B detailed in pages 7-11 in the Appendix). Replace the verbose B/p enumeration ('B = 0.030, p = .471... B = 0.053, p = .176... interaction B = -0.005, p = .941... interaction B = 0.019, p = .748') with a summary: 'all main-effect p's > .15 and all interaction p's > .15 (see Appendix Tables S17-S22)'.",
            "Cut redundant restatements of what 3A/3B found in Results.",
            "Keep the alternative-explanation framing (demand effect, Orne 1962, Zizzo 2010 citations). These are essential.",
            "Aim for ~40-50% reduction. Currently ~600+ words; target ~300-350 words.",
        ],
    },
    {
        "study_id": "study_4a",
        "study_name": "Study 4A Methods + Results + Mediation",
        "source": "study_4_full",
        "rules": [
            "ONLY streamline the Study 4A portion of the source. The source contains the full Study 4 (lead-in + 4A + 4B + Discussion). In your AFTER, output ONLY the Study 4A section (Methods + Results + Mediation). Do NOT include the Study 4 lead-in, Study 4B, or the Study 4 Discussion (those are separate briefs).",
            "Stimulus set: 25 authors, 8 racial-minority authors (32% baseline). Books-by-author paradigm.",
            "PRESERVE THE MEDIATION SECTION IN DETAIL. This is the contribution of Studies 4A/4B. Specifically: keep Sobel test results, ACME (average causal mediation effect) percentages, multiple-mediation model specification, multicollinearity addressing.",
            "Mediator measures (internal MRWP, external MRWP): keep Cronbach's alpha values (alpha = 0.93 for internal, alpha = 0.90 for external). Wording can be condensed but the alpha values stay.",
            "**NEW RULE (2026-05-10):** Replace the verbose primary Wald F-test enumeration with a single sentence + Table 2 pointer per TEMPLATE_MOVES § Wald F-test enumeration (Studies 3A, 4A, 4B branch). Pattern: 'The effect of assignment to the race feedback condition on subsequent selection of a racial-minority author was significantly larger than the effects of assignment to receive descriptive feedback about each of the three comparison attributes (all p's <= .X; see Table 2 for cross-study estimates and Wald tests).'",
            "Balance-check sentence per RULES_DISCOVERED § I.8 — Study 4A balance is clean (p = .114), so use compressed form: 'Initial selections featured racial-minority authors X.X% of the time, with balance across conditions (p = .114; see Appendix Table SX).' Do NOT include cell means, t, or 95% CI in main text.",
            "Demographics line per RULES_DISCOVERED § I.3.",
            "Preserve verbatim: 'from the original set, where their prior selections were highlighted'.",
            "Preserve verbatim: 'See Materials Section: Study 4A in the Appendix for complete study materials.'",
            "Avoid em/en dashes in newly authored AFTER prose.",
            "In your BEFORE section, include the Study 4A subsection of the source verbatim (from the '5.1 Study 4A' header to just before '5.2 Study 4B'). Do not include 4B or Discussion in BEFORE either.",
        ],
    },
    {
        "study_id": "study_4b",
        "study_name": "Study 4B Methods + Results + Mediation + Pretest",
        "source": "study_4_full",
        "rules": [
            "ONLY streamline the Study 4B portion of the source. In your AFTER, output ONLY Study 4B (Methods + Pretest + Procedure + Results + Mediation). Do NOT include Study 4A or the Study 4 Discussion.",
            "Pretest (N=550 rating 21 attributes for importance to gender): condense to 1-2 sentences with appendix pointer: 'Prior to this study, we conducted a pretest (N = 550) rating 21 candidate attributes on importance for gender (see Appendix Section SX for full details). The three comparison attributes used did not differ significantly from gender on rated importance.' Do NOT keep the full pretest procedure inline.",
            "Stimulus set: 25 authors, 19 male / 6 female (24% women baseline). Books-by-author paradigm, gender feedback.",
            "PRESERVE MEDIATION as in Study 4A: Sobel, ACME, multiple-mediation, multicollinearity, alpha values for internal/external MRWP.",
            "**NEW RULE (2026-05-10):** Replace the verbose primary Wald F-test enumeration with a single sentence + Table 2 pointer per TEMPLATE_MOVES. Pattern: 'The effect of assignment to the gender feedback condition on subsequent selection of a woman author was significantly larger than the effects of assignment to receive descriptive feedback about each of the three comparison attributes (all p's < .X; see Table 2 for cross-study estimates and Wald tests).'",
            "Political ideology + party affiliation moderation: KEEP inline in Results per RULES_DISCOVERED § V.5. The interactions are pre-registered and part of the Study 4B contribution. Format: report each non-significant interaction on one line with B, 95% CI, t, p (per the prior post-Katy draft of 4B at line 32).",
            "Balance check + demographics per RULES_DISCOVERED § I.3 and § I.8.",
            "Preserve 'See Materials Section: Study 4B in the Appendix for complete study materials.' Methods closing.",
            "Avoid em/en dashes in newly authored AFTER prose.",
            "In your BEFORE section, include the Study 4B subsection of the source verbatim (from '5.2 Study 4B' through the end of Study 4B Mediation, just before the Study 4 Discussion). Do not include 4A, the Study 4 lead-in, or Discussion in BEFORE.",
        ],
    },
    {
        "study_id": "study_4_discussion",
        "study_name": "Study 4 Discussion (covers 4A + 4B)",
        "source": "study_4_full",
        "rules": [
            "ONLY streamline the Discussion portion of Study 4. In your AFTER, output ONLY the Study 4 Discussion. The source contains the full Study 4 including 4A and 4B; locate the 'Discussion' header that follows Study 4B's Mediation paragraph and use only that section.",
            "Preserve the conclusion that internal MRWP mediates the descriptive-feedback effect, external MRWP does not (in multiple mediation). This is the theoretical contribution of Study 4.",
            "Cut redundant restatements of what 4A/4B found in Results.",
            "Check redundancy with the General Discussion (which appears separately in the manuscript) and cut any duplication.",
            "In your BEFORE section, include only the Study 4 Discussion text verbatim (from the 'Discussion' header at the end of Study 4 to the end of the source).",
        ],
    },
    {
        "study_id": "study_5",
        "study_name": "Study 5 Methods + Results",
        "source": "study_5_full",
        "rules": [
            "ONLY streamline Study 5 Methods + Results (NOT the Discussion). In your AFTER, output Methods + Results only. The source contains the full Study 5 including its Discussion; locate the 'Discussion' header and stop just before it.",
            "RETAIN the pooled interaction analysis (Table 4 mini meta-analysis, random-effects model) preceding the experimental design. This is the theoretical motivation for the 2x2 design and must survive. Compress wording but keep the result.",
            "2x2 design: (gender feedback vs. no gender feedback) x (women underrepresented [18M/6W] vs. women overrepresented [18W/6M]). Preserve the design description.",
            "Stimulus set: 25 business leaders, randomized to 18M/6W or 18W/6M (18 men + 6 women, or 18 women + 6 men). Panelist-selection paradigm.",
            "Recruitment: Prolific (NOT MTurk), N=1,200. Demographics format per RULES_DISCOVERED § I.3: 39.3% men, 59.0% women, 1.6% non-binary, Mage = 42.7, race breakdown.",
            "Fairness-concerns mediator (3 new items on distributive justice) is DISTINCT from the internal/external MRWP measures used in 4A/4B. Preserve this distinction. Keep alpha for the fairness-concerns scale.",
            "Separate regressions by underrepresented vs. overrepresented subset (the structural finding is the reversal: when women overrepresented, descriptive feedback DECREASES selection of women). Preserve the separate-regressions structure.",
            "**STUDY 5 IS NOT IN TABLE 2.** Keep Study 5's own analysis intact. Specifically: KEEP the unequal-magnitude Wald *z*-test (z = 2.50, p = .012) in full because it is the structural finding of Study 5. For the cross-attribute comparison Wald F-tests (gender feedback vs. tech-industry/founders), check the source: if all are null, compress to 'all p's...' form; if any are significant, keep them in the verbose form (Study 5 does not have a Table 2 row to point at).",
            "Balance check per RULES_DISCOVERED § I.8 multi-cell form: report cell means + t + p + 95% CI for both 18M/6W and 18W/6M conditions because the 2x2 design requires it.",
            "Preserve 'See Materials Section: Study 5 in the Appendix for complete study materials.' Methods closing.",
            "Avoid em/en dashes in newly authored AFTER prose.",
            "In your BEFORE section, include the Study 5 Methods + Results text verbatim (from the 'Study 5' header through the 2x2 results, stopping just before the 'Discussion' header).",
        ],
    },
    {
        "study_id": "study_5_discussion",
        "study_name": "Study 5 Discussion",
        "source": "study_5_full",
        "rules": [
            "ONLY streamline Study 5's own Discussion section. The source contains the full Study 5; locate the 'Discussion' header and use only that section through the end.",
            "Preserve the reversal interpretation: when women are OVERrepresented in past selections, descriptive feedback REDUCES subsequent selection of women. This is the key finding of Study 5.",
            "Preserve the fairness-concerns explanation for the reversal. This is the theoretical contribution.",
            "Cut redundant restatements of the 2x2 results already reported in Results.",
            "Check redundancy with the General Discussion and cut overlap.",
            "In your BEFORE section, include only the Study 5 Discussion text verbatim.",
        ],
    },
]


def main():
    """Write a consistency-emphasis brief and a flow-emphasis brief per study.

    The .brief.md (consistency emphasis) is for the Claude pass and as a
    canonical reference. The .brief.codex.md (flow emphasis) is for the
    Codex parallel pass.
    """
    out = []
    for spec in BRIEFS:
        for emphasis, suffix in [("consistency", ".brief.md"), ("flow", ".brief.codex.md")]:
            path = HERE / f"{spec['study_id']}{suffix}"
            content = make_brief(
                spec["study_id"],
                spec["study_name"],
                spec["source"],
                spec["rules"],
                emphasis=emphasis,
            )
            path.write_text(content, encoding="utf-8")
            out.append((spec["study_id"], emphasis, len(content), path))
            print(f"wrote {path.name}: {len(content)} chars ({emphasis} emphasis)")
    return out


if __name__ == "__main__":
    main()
