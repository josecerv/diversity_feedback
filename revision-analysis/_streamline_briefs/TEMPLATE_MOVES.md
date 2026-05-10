# Post-Katy streamlining template (extracted from live Gdoc Studies 1, 2, 3B as of 2026-05-09; amended 2026-05-10)

This is the **accepted** streamlining pattern after Katy's 2026-05-09 audit reverted Sophia's over-aggressive May-7 cuts. The May-8 `Streamlining_Demo_Study3A.docx` is OUTDATED and must NOT be used as a template. Use only the moves below.

## 2026-05-10 amendment (this pass)

Two changes since the 2026-05-09 baseline:

1. **New cross-study Table 2** (`Table 2. Effects of Random Assignment to Receive Descriptive Feedback About the Identity-Target Composition of Initial Selections Versus Descriptive Feedback About Other Initial-Selection Attributes on Subsequent Selections, Across Studies`) consolidates the focal-vs-comparison Wald-test results for Studies 1, 2, 3A, 3B, 4A, and 4B. With this table in place, the verbose `(1)... (2)... (3)...` Wald F-test enumeration for Studies 3A, 4A, and 4B can collapse to a single sentence + Table 2 pointer. **Study 1 keeps the verbose enumeration in full** because it is the place of first mention for the system-of-equations machinery. Study 5 is NOT in Table 2 and follows its own rule (see § Wald F-test enumeration below).

2. **`RULES_DISCOVERED.md`** in this directory captures the line-by-line audit of the post-Katy text and codifies implicit rules this template did not previously call out (demographics-line format, balance-check forms by sample-balance state, cross-study reference style, no em/en dashes in newly authored prose, etc.). **Drafters MUST read `RULES_DISCOVERED.md` in addition to this template before drafting any AFTER block.**

## Section structure (per study)

```
Study X[A/B]
[1-2 sentence lead-in: who participants imagined being, what task, what manipulation tested]
Methods
Participants. As pre-registered (https://osf.io/...), we recruited N participants on [platform] to complete a [Y]-minute survey for $[Z] (gender %; Mage = X; race breakdown).
Procedure. [One dense paragraph describing scenario + display details + procedural detail.]
[Conditions paragraph: condition 1 vs condition 2 with parallel structure. Preserve key procedural details like "from the original set–where their prior selections were highlighted–to include in the display."]
[For Study 2 only: "The IRB approved a waiver of informed consent, so participants were not told that they were participating in a research study."]
See Materials Section: Study X in the Appendix for complete study materials.
------
Insert Table X about here
------
Results
[Optional balance-check sentence: "Initial selections featured ... X%, with balance across conditions (p = ..., see Appendix Table/Section X)." For Study 2 the imbalance is in main text since it was non-trivial.]
Following our pre-registration, we ran an OLS regression... [primary effect with B, 95% CI, t, p, including condition cell means].
[Robustness reference: "robust to controlling for participant demographics (Appendix Table SX)" or similar.]
[Verbose Wald F-test enumeration for the primary contrast that tests whether assignment to the focal feedback condition has a larger effect on focal-attribute selection than assignment to other feedback conditions has on their respective attribute selections. KEEP all F-stats. KEEP the parallel "(1)... (2)... (3)..." structure. KEEP the closing "(in fact, all other types of feedback had insignificant effects on subsequent ... selections)" if present.]
[For null secondary contrasts: collapsed "all p's ≥ X; see Table X Models Y-Z" form is allowed.]
```

## Move-by-move (what to do, what NOT to do)

### Lead-in sentence
- **DO** keep a 1-2 sentence lead-in establishing the paradigm (NPR, university campus, biopics, library, panel) and what manipulation is being tested.
- **DON'T** repeat the full theoretical motivation (that lives in the General Introduction).

### Participants
- **DO** strip MTurk/Prolific/Cloudresearch boilerplate to a single line: `(N=X) (gender split %; Mage = Y; race breakdown)`.
- **DO** keep gender, age, race demographics consistent across studies (Katy's screenshot 7 red line).
- **DON'T** delete the demographic line entirely. Reviewers need it.
- **DON'T** repeat the inclusion criteria (90% approval, 1000+ prior completions). These are in footnote B1 on Study 1's first appearance — every later study just gets the recruitment platform + survey length + pay.

### Procedure
- **DO** condense the 2-3 paragraph original into ONE dense paragraph.
- **DO** preserve any procedural display detail Katy flagged as critical:
    - "from the original set–where their prior selections were highlighted–to include in the display." (screenshot 4 revert)
    - For Study 2 only: "The IRB approved a waiver of informed consent, so participants were not told that they were participating in a research study." (screenshot 3 revert)
- **DON'T** elide the brief description of stimuli (e.g., "each with a cover thumbnail and a brief description (protagonist's name, role, publication year, page count)").

### Conditions paragraph
- **DO** collapse the 3-paragraph (no-feedback / feedback / post-feedback) original into ONE paragraph with parallel structure.
- **DO** name the focal feedback condition variable (gender, race, etc.) explicitly each time.

### Closing line of Methods
- **DO** keep "See Materials Section: Study X in the Appendix for complete study materials."

### Results: opening line
- **DO** report the balance check in main text with a numerical p-value AND an appendix pointer: "Initial selections featured women X% of the time, with balance across conditions (p = .813; see Appendix Table/Section X)." If imbalanced, Katy still wants the imbalance noted with cell means.
- **DON'T** drop the balance check entirely (Katy screenshot 9 red line).

### Results: primary effect
- **DO** report the OLS regression results with: cell means for both conditions ("46.2% women experts selected vs 33.7%"), B, 95% CI, t, p.
- **DO** reference the figure and table.
- **DO** add a robustness one-liner: "robust to controlling for participant demographics (Appendix Table SX)" or "These results were robust to several alternative specifications, including controlling for participant gender, race, and age, and analyzing our data with logistic regression (see Appendix Table SX for robustness checks)."
- **DON'T** replace the cell-mean DV phrasing ("chose a woman protagonist") with bare "%" (Katy screenshot 5 red line).

### Results: Wald F-test enumeration (PRIMARY contrast) — UPDATED 2026-05-10 for Table 2
The rule depends on which study you are drafting:

- **Study 1 (place of first mention).** Keep the FULL parallel construction with all F-stats. This is the Zellner-systems-of-equations result and Study 1 is where the analysis machinery is first introduced; readers learn it here.
  - Pattern: "the effect of assignment to the [X] feedback condition on [DV] was larger than the effect of assignment to receive descriptive feedback about the percentage of initial [items] with (1) [attribute A] (F(1, ...) = ..., p = ...), (2) [attribute B] (F(1, ...) = ..., p = ...), or (3) [attribute C] (F(1, ...) = ..., p = ...) on subsequent selections of [final item] with, respectively, (1) [A], (2) [B], or (3) [C]"
  - End with: "(in fact, all other types of feedback had insignificant effects on subsequent ... selections)" if present in original.

- **Studies 3A, 4A, 4B.** Replace the verbose enumeration with a single sentence + the new Table 2 pointer. These three studies appear as rows in the cross-study consolidation table, so repeating the enumeration is redundant.
  - Recommended pattern (single sentence): "The effect of assignment to the [focal] feedback condition on subsequent selection of [DV] was significantly larger than the effects of assignment to receive descriptive feedback about each of the three comparison attributes (all p's ≤ .X; see Table 2 for cross-study estimates and Wald tests)."
  - When one comparison attribute has a sizable opposite-direction effect that warrants surfacing, keep that one inline and cite Table 2 for the rest.
  - **DO NOT** drop the focal claim entirely. The single sentence still has to assert the discriminant validity result.
  - **DO NOT** delete the Table reference (e.g., `see Table SX, Models 5-8`) for the per-study OLS regression appendix. Keep it.

- **Study 5.** Study 5 is NOT in Table 2 (its analytical structure is different — 2x2 reversal). Keep Study 5's own Wald *z*-test for the unequal-magnitude finding across the underrepresented and overrepresented conditions in full; that is the structural finding of Study 5.

- **Study 2 (null secondaries form).** "all p's ≥ X; see Table 3 Models 2-4". Already short — Study 2's secondary contrasts are all null so the verbose enumeration is unnecessary.

- **DON'T** collapse Study 1 to "the effect was larger than the effect of the other three descriptive statistics shown" (Sophia's cut, Katy reverted via dispositions screenshot 10).
- **DON'T** add a Table 2 pointer for Study 1 — Table 2 is already referenced for Study 1's own regression results in the live doc, and the system-of-equations description belongs inline.

### Results: secondary null contrasts
- **DO** collapse to "all p's ≥ X; see Table X Models Y-Z" (Study 2 pattern).
- **DON'T** keep the verbose enumeration when the test is a null finding for non-focal attribute self-selection.

### Mediation (Studies 4A, 4B only)
- **DO** preserve the Sobel test, ACME indirect-effect estimates, multiple-mediation specification, multicollinearity addressing in their original detail. This is the contribution of these studies.
- **DO** keep Cronbach's α values for internal MRWP and external MRWP scales.
- **DON'T** condense mediation; it's the theoretical mechanism.

### Pretest (Study 4B only)
- **DO** condense to 1-2 sentences with appendix pointer: "Prior to this study, we conducted a pretest (N=550) rating 21 candidate attributes on importance for gender (see Appendix Section X for full details). The three attributes used as comparison conditions did not differ significantly from gender on rated importance."
- **DON'T** keep the full N=550, 21-attribute rating procedure inline; appendix carries the details.

### Discussion (cross-study)
- **DO** preserve the contribution-distinguishing claim (e.g., "demand effect ruled out", "internal-MRWP mediates", "reversal under overrepresentation explained by fairness concerns").
- **DO** condense restatement of findings already reported in Results.
- **DO** if applicable, compress S2A/S2B / supplemental experiment stat enumerations to "all interaction p's > .15; see Appendix Tables S17–S22".
- **DON'T** add new findings or new framing.

## Footnotes (already added by 2026-05-09 apply, do not duplicate)

- **B1**: screening criteria (90% approval, 1000+ prior completions, U.S.-based) — anchored to Study 1 Methods. Every later study's Participants line is shorter because of this.
- **B2**: stimulus sampling + stimulus-set interaction stats — anchored to Study 1 Results.
- **B5**: Zellner Wald-tests appendix pointer — anchored to Study 1 Results paragraph.

## Reference: live Gdoc post-Katy text (refreshed 2026-05-10)

- Study 1 (post-Katy template): `_streamline_briefs/_source_excerpts/_v2_2026-05-10/study_1_full.txt`
- Study 2 (post-Katy template): `_streamline_briefs/_source_excerpts/_v2_2026-05-10/study_2_full.txt`
- Study 3B Methods+Results (post-Katy template): `_streamline_briefs/_source_excerpts/_v2_2026-05-10/study_3b_methods_results.txt`
- Cross-study Table 2 (NEW): `_streamline_briefs/_source_excerpts/_v2_2026-05-10/table_2_consolidation.txt`
- Pre-2026-05-10 archive: `_streamline_briefs/_source_excerpts/_archived_pre-2026-05-10/`

## Reference: Katy's red lines (verbatim)

`revision-analysis/katy_2026-05-09/katy_comments.md` — re-read before drafting any AFTER block.

## Reference: line-by-line rule audit of the post-Katy text (NEW 2026-05-10)

`revision-analysis/_streamline_briefs/RULES_DISCOVERED.md` — implicit rules extracted from the post-Katy live Gdoc that this template did not previously call out. **Drafters must consult this file** for: demographics-line canonical format (semicolon-delimited, gender→Mage→race), balance-check forms by sample-balance state (Study 3B compressed form for clean balance, Study 2 form for imbalance), cross-study reference style, voice/tense rules, the no-em/en-dash rule for newly authored prose, candidate cross-study cuts the first pass missed, and the first-mention vs. repetition map.
