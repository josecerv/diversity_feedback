# Brief: streamline Study 4 Discussion (covers 4A + 4B)

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

EMPHASIS for this pass: CONSISTENCY + FIRST-MENTION DISCIPLINE.
Your priority is uniform application of every strict-consistency rule in RULES_DISCOVERED.md (§ I), the cross-study first-mention map (§ III), and Katy's explicit red lines. Where the source departs from the canonical pattern, normalize toward it. Prefer keeping the prior post-Katy draft's wording when it already complies, only changing what's needed to apply the new Table 2 rule and any implicit rules the prior draft missed.


---

# TEMPLATE_MOVES.md (the accepted post-Katy moves; amended 2026-05-10 for Table 2)

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


---

# RULES_DISCOVERED.md (line-by-line audit of post-Katy text; new 2026-05-10)

# Rules discovered from the post-Katy live Gdoc (2026-05-10 audit)

This document is the line-by-line audit of the *currently accepted* Methods+Results writeups for Studies 1, 2, and 3B, together with cross-checks against `katy_2026-05-09/katy_comments.md` and a diff against the prior `study_*.draft.md` files. It exists to falsify and extend `TEMPLATE_MOVES.md`: every implicit pattern Katy enforced should appear here before we redraft Studies 3A, 4A, 4B, and 5.

Sources audited:
- `_source_excerpts/_v2_2026-05-10/study_1_full.txt` (783 words; pulled from live doc)
- `_source_excerpts/_v2_2026-05-10/study_2_full.txt` (656 words)
- `_source_excerpts/_v2_2026-05-10/study_3b_methods_results.txt` (571 words)
- `_source_excerpts/_v2_2026-05-10/table_2_consolidation.txt` (the new cross-study table; covers Studies 1 to 4B)
- `katy_2026-05-09/katy_comments.md` (Katy's 10 red-line screenshots)

---

## I. Strict-consistency rules (apply uniformly across studies)

### I.1 Lead-in sentence(s)
- 1 to 2 sentences right after the `Study X` heading.
- Establishes the paradigm (NPR producers, biographies on campus, Facebook biopic ad, library reading list, panel selection) and what manipulation is being tested.
- Pattern in Study 1: "In Study X, participants imagined [paradigm]. We tested whether [feedback condition]…"
- Pattern in Study 3B: "In this study, we sought to replicate our key findings from Study 3A about [variation]…"
- **DO NOT** restate full theoretical motivation (lives in the Introduction).
- **DO** name the focal feedback dimension (gender, race) explicitly.

### I.2 Methods header structure
- `Methods` is the section header (HEADING_2).
- `Participants.` and `Procedure.` are bold paragraph leads (NOT separate headings).
- For Studies with measure-specific paragraphs (4A, 4B, 5), the bold leads also include `Internal motivation…`, `External motivation…`, `Political ideology.`, `Political party affiliation.`, etc.

### I.3 Participants line — canonical structure
Single sentence, semicolon-delimited fields, in this order:

```
Participants. As pre-registered (https://OSF/AsPredicted/...), we recruited N participants on/through PLATFORM to complete a [Y]-minute survey for $[Z] (gender breakdown; Mage = X.XX; race breakdown).
```

Evidence (Study 1): `As pre-registered (https://osf.io/vazsm/), we recruited 1,000 participants through Prolific to complete a 3-minute survey for $0.75 (55.4% women; 43.7% men; 0.9% non-binary; Mage = 43.80; 72.3% White, 13.2% Black, 6.5% Hispanic, 7.2% Asian).`

Evidence (Study 3B): `As pre-registered (https://osf.io/uyn8g/), we recruited 1,000 participants on Cloudresearch to complete a 3-minute survey for $0.75 (51.0% men; 48.3% women; Mage = 43.60; 77.9% White, 9.7% Black, 5.5% Hispanic, 6.4% Asian).`

Sub-rules:
- **Gender order is whichever group is larger in the actual sample**, not a fixed convention. Study 1 women first (55.4% > 43.7%); Study 3B men first (51.0% > 48.3%). Match each study's source.
- **Include `non-binary` (or whatever the sample produced) when present.** Study 1 has 0.9% non-binary; Studies 2 and 3B do not.
- **Use semicolons between gender-breakdown fields, between gender and Mage, and between Mage and race breakdown.** Use commas only inside the race breakdown.
- **Mage formatting:** `Mage = XX.XX` (two decimals). The M is italicized and `age` is subscripted in the Word render; in markdown source we just write `Mage`.
- Eligibility criteria (90% approval, 1,000+ prior completions, U.S.-based) are NOT in the participants line — they live in footnote B1 anchored at Study 1 (per Katy red line #1).
- Study 2 is the one outlier on this rule: it uses TWO sentences (`This study was pre-registered on AsPredicted (...). We recruited 302 participants...`) because of the field-setting context (recruiting at a campus location, dates of data collection, IRB consent waiver in the same paragraph). Studies 1, 3A, 3B, 4A, 4B, 5 use the single-sentence canonical form.

### I.4 Procedure paragraph — content checklist
ONE dense paragraph (the Procedure may extend to a Conditions paragraph plus optional measure-specific paragraphs in Studies 4A, 4B, 5). Required content:
- Scenario setup naming the paradigm (NPR producer, candy-bar voter, Facebook biopic ad, library reading list, conference panel).
- Stimulus display details: total stimulus count (e.g., 25), per-stimulus brief-description fields with at least one example each, baseline percentage of the focal group (e.g., `4 of 25 films (16%) featured a racial-minority protagonist`).
- The selection task (initial portfolio size, final selection mechanism, any incentive language).
- The "from the original set, where their prior selections were highlighted" procedural detail (Studies 2, 3A, 3B, 4A, 4B, 5; not Study 1). Katy red line #4 explicitly required this.
- For Study 2 only: the IRB consent waiver sentence ("The IRB approved a waiver of informed consent, so participants were not told that they were participating in a research study."). Katy red line #3.

### I.5 Conditions paragraph — parallel structure
Single paragraph collapsing the no-feedback / feedback / post-feedback flow:
- Names the focal feedback condition explicitly (gender, race) every time.
- "In the no [focal] feedback condition, participants received [3-attribute list with parallel structure]."
- "In the [focal] feedback condition, participants always received feedback about [focal attribute] alongside [two/one randomly drawn] piece(s) of feedback from the no-[focal]-feedback list."
- After-feedback action: "After receiving feedback, participants selected a Nth and final [item] from the original set, where their prior selections were highlighted."
- DO NOT collapse the parallel structure into a single bare clause (Sophia's May-7 collapse was reverted; see Katy red line #4).

### I.6 Methods closing line
`See Materials Section: Study X in the Appendix for complete study materials.` Present in Studies 1, 3A, 3B, 4A, 4B; NOT present in Study 2 (likely because Study 2's Procedure ends with the voting-mechanic sentence and no separate Materials Section is referenced inline).

### I.7 Insert Table marker
A typesetter marker between Methods and Results, formatted as:
```
------------------------------------------------------------------
Insert Table X about here 
------------------------------------------------------------------
```
Studies 1 and 2 have it. Study 3B does not (in the post-Katy text). Study 5 has multiple (`Insert Table 4 about here`, `Insert Table 5 and Figure 4 about here`). Pattern: include where there's a focal main-text Table.

### I.8 Balance-check sentence (Results opening)
Format depends on whether the initial selections were balanced across conditions:
- **Balanced (Study 3B form):** `Initial selections featured [DV] X.X% of the time, with balance across conditions (p = X.XXX; see Appendix Table SX).` Compressed; no cell means or t-test in main text; appendix carries the full stats. Per Katy red lines #8 and #9.
- **Imbalanced or marginal (Study 2 form):** `Initial selections featured [DV] X.X% of the time, with a slight imbalance across conditions (Y.Y% control vs. Z.Z% treatment, p = X.XXX).` Cell means in main text; no appendix pointer because the imbalance is non-trivial and the reader needs the numbers immediately.
- **No initial selection (Study 1 form):** No balance-check sentence — Study 1 is hypothetical with no pre-random-assignment selection stage. Skip the sentence entirely.
- **Multi-cell design (Study 5 form):** report balance per condition cell, e.g., `Initial six-person panels were 60.7% women in the women overrepresented condition, with balance across feedback conditions (59.8% vs. 61.6%; t(...) = ..., p = ..., 95% CI = [...])`. Repeat for the other condition.

### I.9 Primary effect — cell-mean DV phrasing
For the primary regression result, the DV must be named at every percentage report. Per Katy red line #5:
- DO: `41.60% chose a racial minority author` / `46.2% women experts selected`.
- DON'T: `41.60%` (bare), `41.60% chose a woman` if the manuscript uses "racial minority author" elsewhere (mismatched DV).

Standard pattern: `Participants were significantly more likely to choose a [DV] in the [focal] feedback condition (X.XX% chose a [DV]) than in the no [focal] feedback condition (Y.YY% chose a [DV]; B = ..., 95% CI = [..., ...], t(...) = ..., p = ...; see Figure X and Table SX, Model Y).`

### I.10 Robustness one-liner
Pattern: `These results were robust to controlling for participant demographics (Appendix Table SX).`
Or the longer form: `These results were robust to several alternative specifications, including controlling for participant gender, race, and age, and analyzing our data with logistic regression (see Appendix Table SX for robustness checks).`

Studies 1 and 3B have it. Study 2 does not (likely small N + field constraints). Studies 3A, 4A, 4B, 5 should include the short form unless the source omits it.

### I.11 DV phrasing — preserve verb object always
Katy red line #5 (Screenshot 5). Whenever a percentage is reported, keep the verb object: `X.X% chose a woman`, `X.X% chose a racial minority author`, `X.X% chose a biopic with a woman protagonist`. NEVER drop the object even when the DV is named elsewhere in the same paragraph.

### I.12 Wald F-test enumeration — UPDATED for Table 2
This is the rule that changes most for the new pass.

**Study 1 (place of first mention):** keep the FULL system-of-equations description and the verbose `(1)... (2)... (3)...` enumeration. Study 1 is where readers learn the analysis machinery.

**Studies 3A, 4A, 4B:** replace the verbose enumeration with a single sentence + Table 2 pointer. The new Table 2 (cross-study consolidation) reports each study's focal effect, comparison-attribute effects, and Wald p-values in one row. Pattern:
> The effect of assignment to the [focal] feedback condition on subsequent selection of [DV] was significantly larger than the effects of assignment to receive descriptive feedback about each of three comparison attributes (all p's < .X; see Table 2 for cross-study estimates and Wald tests).

Or, when one comparison attribute is more interesting (e.g., its own effect was sizable but in the wrong direction): keep that one inline and cite Table 2 for the rest.

**Study 5:** Study 5 is NOT in Table 2. Keep its idiosyncratic Wald *z*-test for the underrepresented-vs-overrepresented magnitude difference (this is the structural finding of Study 5). The earlier Studies 3A/4A/4B-style cross-attribute Wald F-tests are reported in the source — keep them or compress per the same rule, since Study 5 isn't in Table 2.

**Study 2 (null secondaries form):** `all p's >= X; see Table 3 Models 2-4`. Already short. Per Katy red line #6 the verbose enumeration is unnecessary when all secondary contrasts are null.

### I.13 Mediation paragraphs (Studies 4A and 4B only)
Mediation is the contribution of these studies. Preserve in full:
- 10,000-sample bootstrapped mediation model + Sobel test
- 95% bias-corrected CI for indirect effect
- Imai et al. (2010) ACME percentages for both internal and external MRWP
- Multiple-mediation model, with the multicollinearity addressed via the inter-mediator correlation (`r = ..., p < .001`)
- Cronbach's α for internal MRWP (typically 0.93) and external MRWP (typically 0.89-0.90)
- Final conclusion: which mediator survives in the multiple-mediation specification

DON'T condense any of this. Both 4A and 4B carry the full mediation result.

### I.14 Pretest paragraph (Study 4B only)
Condense to 1-2 sentences with appendix pointer:
> Prior to this study, we conducted a pretest (N = 550) rating 21 candidate attributes on importance for gender (see Appendix Section SX for full details). The three comparison attributes used did not differ significantly from gender on rated importance.

DON'T inline the full N=550, 21-attribute, between-subjects rating procedure.

### I.15 Decimal and notation conventions
- Sample sizes: comma-separated thousands, e.g., `1,000 participants` (Study 1) but bare `302 participants` (Study 2).
- Percentages: 1-2 decimals where the source has them (`46.1%`, `25.2%`). Don't add decimals not in source.
- t/F/z: as the source reports them.
- p-values: `.010`, `.314`, `< .001` (strict less-than). Three decimals when meaningful; `< .001` when extremely small.
- B and 95% CI: `B = 0.125, 95% CI = [0.093, 0.157]`. Square brackets, not parentheses.
- Italicization (in the rendered docx/Gdoc, not in source): M, p, t, F, z, B all italicized; α stays Greek.

---

## II. Study-dependent variations with reasons

| Pattern | Study 1 | Study 2 | Study 3B | Reason |
| --- | --- | --- | --- | --- |
| Pre-reg sentence form | Single sentence inline link | Two sentences with separate IRB clause | Single sentence inline link | Study 2 has IRB consent waiver to fit in the participants line |
| `See Materials Section…` closing | Yes | No | Yes | Study 2 ends Procedure with the voting-mechanic sentence rather than a materials pointer |
| Robustness one-liner | Yes (short form) | No | Yes (long form) | Field study + small N for Study 2 |
| Balance-check sentence | No | Yes (imbalanced form) | Yes (balanced form) | Study 1 has no pre-random-assignment selection stage |
| Verbose Wald enumeration | Yes (full) | No (collapsed null form) | Yes (full) | Study 2 secondaries are all null; Studies 1, 3A, 3B, 4A, 4B secondaries are all significant |
| `Insert Table X` marker | Yes (Table 2) | Yes (Table 3) | No | Study 3B's regression results are appendix-only (Table S5), no main-text Table for it |
| Discussion section | Yes (short) | Yes (short) | No (shared Study 3 Discussion at end) | Studies 3A and 3B share a single Discussion |

---

## III. First-mention vs. repetition map

This is the cross-study scaffolding. When a detail is first introduced, it appears in full at that location; later studies abbreviate or reference back.

| Detail | First introduced | Later studies do | Why |
| --- | --- | --- | --- |
| Eligibility criteria (90% approval, 1,000+ prior completions, U.S.-based) | Study 1 footnote B1 | Omit entirely (just platform + survey duration + pay) | Katy red line #1 |
| System-of-equations / Zellner construction | Study 1 Results (full description) | Mention "Wald tests" without re-explaining | Study 1 carries the methodological scaffold |
| Cross-study Wald F-test results table | Table 2 (consolidation, NEW) | Reference: "see Table 2 for cross-study estimates and Wald tests" | The new consolidation table absorbs per-study repetition |
| Stimulus display field convention (name, role, year, page count, etc.) | Study 1 (NPR experts) | Each later study describes its own stimulus fields explicitly because the field set changes per paradigm | Stimulus details ARE study-specific |
| "From the original set, where their prior selections were highlighted" | Study 2 (first appearance with this paradigm) | Studies 3A, 3B, 4A, 4B, 5 repeat verbatim | Procedural detail Katy explicitly requires (red line #4) |
| IRB consent waiver | Study 2 only | N/A | Field-study-only feature |
| Internal/external MRWP scale items + Cronbach's α | Study 4A | Study 4B says "the same items as in Study 4A, adapted to focus on gender prejudice"; Study 5 says "as in Study 4B"; Cronbach's α reported each time even when wording is referenced back | Mechanism is the contribution of these studies; α values are study-specific |
| Pretest of 21 attributes for importance rating | Study 4B only | N/A | Pretest exists only for Study 4B |
| Fairness-concerns mediator (3 distributive-justice items) | Study 5 only | N/A | Fairness mediator is the Study 5 contribution distinct from MRWP |
| 2x2 design description (women under- vs. overrepresented x feedback) | Study 5 only | N/A | Reversal design unique to Study 5 |
| Demographic-line format | Study 1 sets the format | Match exactly (semicolon delimiters, gender-Mage-race order); include non-binary if present in the study's sample | Katy red line #7 |
| `See Materials Section: Study X in the Appendix for complete study materials.` closing | Study 1 | Studies 3A, 3B, 4A, 4B (verbatim with study label substitution) | Convention; Study 2 is the exception |
| Pre-registration link | Study 1 | Each later study has its own (different OSF/AsPredicted ID) | Study-specific |

---

## IV. Implicit calibrations Katy enforced (not in TEMPLATE_MOVES.md before)

These are patterns visible in the post-Katy text that the prior `TEMPLATE_MOVES.md` did not call out explicitly. Each is now a live rule.

### IV.1 Voice and tense
- "We" voice throughout for the authorial team.
- "Participants" for subjects.
- Past tense for procedure narration ("we recruited", "participants were asked").
- Present tense only for static facts in the Discussion ("Study 1 mirrors a real organizational practice").

### IV.2 Cross-study reference style
- Refer to other studies by their label ("In Study 4A", "as in Study 4B", "replicating Studies 2, 3B, and 4A").
- Do NOT use phrases like "the prior study" without qualification when there is more than one prior candidate.

### IV.3 Replication-grade detail must appear somewhere
Per Katy red line #1 and the cross-cutting principles: full demographic numbers, eligibility thresholds, baseline-balance stats, and full Wald F-test reporting must appear EITHER in main text OR in an appendix (footnote / table / section). Compression in main text is acceptable only when the appendix carries the full numbers.

### IV.4 "Original wording often beats rewrite"
Per Katy red line #6 (Screenshot 6) and #10 (Screenshot 10). When a streamlining attempt produces a sentence that is not meaningfully shorter or clearer than the original, leave the original. Do not rewrite for rewriting's sake.

### IV.5 Compress nulls to "all p's…" form; keep significant tests verbose (or table-pointer)
Verbose enumeration is for results readers care about. Null secondary contrasts should compress to `all p's >= X; see Table SX Models Y-Z` (Study 2 pattern). Significant primary contrasts that demonstrate the discriminant validity of focal feedback (the Zellner Wald tests) get either verbose enumeration (Study 1) or a Table 2 pointer (Studies 3A, 4A, 4B post-2026-05-10 amendment).

### IV.6 Don't rename DVs for compression
Katy red line #2 (Screenshot 2): "Without my additions, it's totally unclear what DV you're even measuring in the comparisons so really doesn't work." Whenever a comparison test is reported, the DV must be named — at minimum the focal selection variable ("a woman", "a racial minority author", "a biopic with a woman protagonist").

### IV.7 No em-dashes or en-dashes in newly authored prose
Per the standing user feedback (`feedback_no_em_dashes.md`): when authoring NEW prose for the manuscript, avoid em (—) and en (–) dashes. Use commas, parentheses, or sentence breaks instead. Use curly apostrophes/quotes (U+2019 for ', U+201C/U+201D for "). When transcribing existing post-Katy text verbatim (e.g., in a BEFORE block), preserve the source's punctuation.

---

## V. Diff against prior `study_*.draft.md` (violations to fix in this pass)

These are issues in the prior post-Katy drafts that the new pass must address.

### V.1 Study 3A draft (`study_3a.draft.md`)
- **Wald enumeration is verbose** — under the new Table 2 reference rule, this should compress to "see Table 2 for cross-study estimates and Wald tests" plus a one-sentence claim of the focal-vs-comparison contrast. Per rule I.12.
- **Robustness one-liner is missing.** The source does not provide a robustness result, so the prior draft skipped it; per the canonical rule (I.10) we should still include a short pointer if Appendix Table SX contains robustness checks. To be confirmed by checking Appendix.
- **Lead-in is paradigm-only**, doesn't preview the manipulation. Compare Study 1 ("We tested the effect of providing participants with descriptive feedback about the gender composition of their previously selected expert sources on their subsequent hypothetical selections of experts for future NPR stories.") The prior 3A draft says only "We tested whether feedback about the racial diversity of their initial film selections increased the likelihood that their final selection featured a racial-minority protagonist." That's good; preserve it.

### V.2 Study 4A draft (`study_4a.draft.md`)
- **Wald enumeration verbose** — compress to Table 2 pointer per I.12.
- **Lead-in starts with "Study 4A tested whether feedback about the race of past selectees increased…"** — direct, but the Study-1 / Study-3B pattern starts with "In Study X" or "In this study". Aligning to that pattern is optional polish; not strictly required.
- **No `See Materials Section: Study 4A in the Appendix for complete study materials` closing** — actually present (line 29 of the prior draft). OK.

### V.3 Study 4B draft (`study_4b.draft.md`)
- **Wald enumeration verbose** — compress per I.12.
- **Pretest is properly condensed to 2 sentences** — good per I.14.
- **Political ideology and party affiliation results are kept inline** — these are part of the Study 4B contribution (they tested moderation by ideology). Keep, per the source.
- **"In the Appendix" missing** in the closing line — actually present (line 27 of prior draft). OK.

### V.4 Study 5 draft (`study_5.draft.md`)
- **Wald F-test for cross-attribute comparisons NOT IN Table 2** — Study 5 isn't in Table 2 because it has its own analytical structure (2x2 reversal). Decision: keep the cross-attribute Wald enumeration if it exists in the source AND it's significant; OR compress to "all p's…" form if all are null. Need to check the source.
- **2x2 reversal Wald *z*-test preserved** — required (I.12 Study 5 carve-out). Good.
- **Fairness-concerns mediator preserved** — required. Good.

### V.5 Pre-registered moderation analyses (Study 4B political ideology)
The prior 4B draft at line 32 includes the political-ideology and party-affiliation interactions as part of the primary effect paragraph. This is per the source. Keep.

---

## VI. Candidate cuts the first pass missed

Cuts that emerge from holistic top-to-bottom reading rather than per-study local view:

### VI.1 Repeated framing of "as in our prior studies" — collapse to one phrase
Each study tends to have a sentence saying "we replicated the findings from our previous studies (see Table SX for complete results)" which can usually be cut entirely, with the cross-study comparison routed to Table 2.

### VI.2 Repeated procedural description of the random-draw mechanism
"alongside two randomly drawn pieces of feedback from the no-[focal]-feedback list" appears in every study from 2 onward. Cannot be cut entirely (it's procedural), but can be compressed to "alongside two randomly drawn pieces from the no-[focal]-feedback list" without "of feedback" (already short enough).

### VI.3 "We followed the same procedure described in Study 4A, …"
Study 4B's Procedure already references Study 4A. Could be tightened to a single sentence: "Procedure. We followed the Study 4A procedure with two changes: (1) the focal feedback was about gender rather than race; (2) the comparison attributes were derived from a pretest (N = 550) confirming all three were rated at least as important as gender for selecting authors (see Appendix Section SX)." Saves about 80 words.

### VI.4 Mediator-paragraph boilerplate
The phrasing "we tested each proposed mediator independently using a 10,000-sample bootstrapped mediation model and a Sobel test" appears in 4A and 4B. Could be condensed in 4B by referring back: "Following the same mediation procedure as Study 4A," then proceeding directly to the indirect-effect estimates.

### VI.5 "Initial selections featured X X% of the time, with balance across conditions…"
Study 3B's compressed balance-check (p + appendix pointer only) is shorter than the prior 3A/4A/4B drafts' balance-check (cell means + t + p + CI). For studies that are cleanly balanced, adopt Study 3B's compressed form to save ~25 words per study.

### VI.6 Result-section opening boilerplate
"Following our pre-registration, we ran an OLS regression with robust standard errors to predict whether the seventh and final author a participant selected was a racial minority…" — appears with study-specific substitutions in every study. Could be shortened to "Following our pre-registration, we ran an OLS regression predicting [DV]; the primary predictor was the [focal] feedback condition." Saves ~15 words per study.

### VI.7 Discussion paragraph's restatement of Results
Both Study 1 and Study 2 Discussion paragraphs restate the primary finding (which has just appeared in Results). The Discussion can lead with the contribution claim and skip the restatement. Study 3 Discussion already does this well.

---

## VII. Open questions for Jose

These are genuine ambiguities the audit surfaced — the drafters can flag them as DIVERGENCES rather than guess.

1. **Should Study 4B's political ideology / party affiliation interaction tests stay in the streamlined Results, or compress to "moderation by political ideology and party affiliation was non-significant (all p's ≥ .39; see Table SX)"?** The prior 4B draft kept them inline. The cross-study consistency argument favors compression; the "this was a pre-registered analysis" argument favors keeping them.

2. **Should the Insert-Table-X markers be retained in the AFTER blocks?** They're typesetter cues; for a streamlining demo they're noise. Recommend: keep them where the source has them.

3. **Should Study 3B add a `Insert Table SX about here` marker for Table S5?** Currently absent. Probably optional given the table is appendix-only.

4. **Table 2 numbering collision.** The current manuscript has both an old Table 2 (Study 1 OLS regressions) and the new consolidation Table 2. Streamlined references to "Table 2" will be ambiguous until the manuscript-wide renumber. Recommend: in the streamlined drafts, write "Table 2 (cross-study consolidation)" the first time it's referenced from outside Study 1 to disambiguate.


---

# STUDY-SPECIFIC PRESERVATION RULES for Study 4 Discussion (covers 4A + 4B)

- ONLY streamline the Discussion portion of Study 4. In your AFTER, output ONLY the Study 4 Discussion. The source contains the full Study 4 including 4A and 4B; locate the 'Discussion' header that follows Study 4B's Mediation paragraph and use only that section.
- Preserve the conclusion that internal MRWP mediates the descriptive-feedback effect, external MRWP does not (in multiple mediation). This is the theoretical contribution of Study 4.
- Cut redundant restatements of what 4A/4B found in Results.
- Check redundancy with the General Discussion (which appears separately in the manuscript) and cut any duplication.
- In your BEFORE section, include only the Study 4 Discussion text verbatim (from the 'Discussion' header at the end of Study 4 to the end of the source).

---

# CROSS-STUDY TABLE 2 (consolidation; reference target for Studies 3A, 4A, 4B)

Table 2. Effects of Random Assignment to Receive Descriptive Feedback About the Identity-Target Composition of Initial Selections Versus Descriptive Feedback About Other Initial-Selection Attributes on Subsequent Selections, Across Studies

[TABLE]
Study (N) | Identity-Target Group | Effect of Identity-Target Feedback on Final Selection(s) | Comparison Attributes Provided in Descriptive Feedback | Effect of Comparison-Attribute Feedback on Whether Final Selection(s) Featured the Comparison Attribute | p-value of Wald Test Between the effect of Identity-Target Feedback and Comparison-Attribute Feedback
Study 1: Online Experiment (N = 1,000) | % of Past NPR Experts Who Were Women | +12.51*** | Were Under 50 Years Old | +2.57 | .010*
 |  |  | Were Based on the West Coast of the U.S. | +4.37* | .004**
 |  |  | Worked at a University | +6.41*** | < .001***
Study 2: University Campus Field Experiment (N = 302) | % of Initial Biographies That Featured a Woman Protagonist | +13.20* | Were Over 500 Pages Long | +1.00 | .129
 |  |  | Were Written in the Past 25 Years | +5.00 | .278
 |  |  | Featured an Entertainer | +6.00 | .450
Study 3A: Incentive Compatible Online Experiment (N = 1,000) | % of Initial Films That Featured a Racial Minority Protagonist | +9.38*** | Were Made on a Budget Above $40 Million | −0.23 | .006*
 |  |  | Were Released After 2010 | +6.54 | < .001***
 |  |  | Were Over Two Hours Long | −6.72+ | .009**
Study 3B: Incentive Compatible Online Experiment (N = 1,000) | % of Initial Films That Featured a Woman Protagonist | +20.56*** | Were Made on a Budget Above $40 Million | +1.98 | < .001***
 |  |  | Were Released After 2010 | −0.29 | < .001***
 |  |  | Featured a Political Leader as Protagonist | −5.08 | < .001***
Study 4A: Online Experiment (N = 1,000) | % of Initial Authors Who Were Racial Minorities | +13.40*** | Wrote Poetry | +6.38+ | .013*
 |  |  | Authored More Than 10 Books | +0.68 | .019*
 |  |  | Were Born in the 1800s | +7.44+ | .022*
Study 4B: Online Experiment (N = 1,000) | % of Initial Authors Who Were Women | +11.94*** | Wrote Books, Poems, or Essays Spanning Multiple Genres | −8.22+ | < .001***
 |  |  | Wrote at Least One Book with 30 Million or More Copies Sold | +1.21 | < .001***
 |  |  | Wrote at Least One Book That Remained in Continuous Print for Over 50 Years | +2.06 | .003**
[/TABLE]

---

# PRIOR POST-KATY DRAFT for Study 4 Discussion (covers 4A + 4B) (calibration anchor; do NOT copy verbatim — use only as a comparison reference)

## BEFORE
Discussion
In Study 4, we again replicated the effect of descriptive feedback about the race and gender of past selectees increasing the subsequent selection of racial minorities and women. In addition, we found evidence that both internal and external motivation to respond without prejudice mediated this effect for subsequent selections of racial minorities (Study 4A) and women (Study 4B), respectively. When combining the highly correlated measures of internal and external motivation to respond without prejudice in a multiple mediation model, our findings suggest that internal motivation is the dominant mediator in our studies. However, external motivation to respond without prejudice could likely play a more significant role in settings where choices are made publicly. 
In addition, Study 4B established that the effect of gender feedback was not moderated by political ideology nor party affiliation. Finally, it ruled out the possibility that our effects were driven by perceptions of which attributes were considered most important.

## AFTER
Discussion
Study 4 shows that descriptive feedback about the race or gender of past selectees increases subsequent selections of underrepresented candidates partly by activating internal motivation to respond without prejudice. In Studies 4A and 4B, internal and external motivation mediated the effect in separate models, but multiple mediation models showed that internal motivation was the dominant mediator; external motivation did not mediate the effect in the expected positive direction (null in 4A; negative in 4B). External motivation may matter more when choices are public. Study 4B also ruled out two alternative explanations: the gender-feedback effect was not moderated by political ideology or party affiliation, and it persisted when comparison attributes were pretested to be at least as important as gender.

## Word count: 154 -> 120 (saved 34)

## Preservation notes
- Kept the Study 4 contribution-distinguishing claim: internal MRWP mediates the descriptive-feedback effect in multiple mediation, whereas external MRWP does not mediate it in the expected positive direction.
- Compressed the Results recap into one mechanism-focused opening sentence to avoid redundant restatement.
- Kept the public-choice caveat for external MRWP because it qualifies the mechanism claim.
- Kept Study 4B’s alternative-explanation results: no moderation by political ideology or party affiliation, and comparison attributes pretested as at least as important as gender.
- No specific Katy red line applied to this Discussion section; departed from the Methods/Results-heavy template only because the study-specific rule asks for the Study 4 Discussion alone.

---

# SOURCE (verbatim from live Gdoc, refreshed 2026-05-10; do not modify)

Study 4
In Study 4, we tested the mechanism we theorize underlies the relationship between descriptive feedback and subsequent selection decisions: motivation to respond without prejudice. 
We also attempted to rule out the possibility that the “comparison” attributes summarized in descriptive feedback (alongside feedback about the race or gender of past selectees) in our prior studies may have been perceived as less important to participants than the race and gender of candidates. In Study 4B, we selected comparison attributes after pre-testing them for importance, and we ensured that all were rated at least as important as gender or race. See Appendix Study S4 for a complete description of our pre-testing procedure and results.
We examined the effect of feedback about the race of past selectees in Study 4A and feedback about the gender of past selectees in Study 4B. 
5.1 Study 4A

Methods
Participants. As pre-registered (https://aspredicted.org/XGG_21R), we recruited 1,000 participants on Amazon’s MTurk. Participants were based in the U.S., had a minimum 90% approval rate and at least 1,000 prior completions, and were paid $0.75 to complete a 3-minute survey (47.6% men, 51.5% women; Mage = 42.53; 75.75% White, 9.21% Black, 4.75% Hispanic, 8.83% Asian). 
Procedure. Participants were asked to imagine they had been tasked with creating a summer fiction reading list for their local library. A list of 25 (real) authors was presented to each participant. Each author was displayed with a small photograph and the following information: the author’s name (e.g., James Baldwin, Isabel Allende), their most notable work (e.g., Go Tell It on the Mountain, The House of the Spirits), as well as a count of their total published works (e.g., authored 6 books and several short stories and poems; authored 6 books). Eight of the 25 authors on the list (32%) were racial minorities.
Participants were randomly assigned to one of two conditions: (1) the no race feedback condition or (2) the race feedback condition. In the no race feedback condition, participants received summary statistics about the percentage of authors they had selected who (1) were born in the 1800s, (2) wrote more than 10 books, and/or (3) wrote poetry. In the race feedback condition, participants always received feedback about the percentage of authors they had selected who were racial minorities, alongside one or two randomly drawn pieces of feedback from the list above.
After receiving feedback, participants selected a seventh and final author to recommend for their local library’s summer fiction reading list from the original author candidate set, where their prior selections were highlighted. Finally, participants were asked questions to assess their internal and external motivation to respond without prejudice. See Materials Section: Study 4A for full study materials. 
Internal motivation to respond without prejudice. To assess the extent to which participants considered internal motives (e.g., image concerns) when deciding which final author to select, we adapted four items from Plant and Devine’s (1998) internal motivation to respond without prejudice scale (e.g., “Given my personal values and beliefs, an important factor in my selection of the seventh author to add to my list was my desire to promote the success of racial minorities.”; Cronbach’s α = 0.93). Each item was collected on a scale from 1 (Strongly disagree) to 7 (Strongly agree). As pre-registered, we collapsed the items into a single scale by z-scoring and then averaging them.
External motivation to respond without prejudice. To assess the extent to which participants considered external motives (e.g., reputational concerns) when deciding on which final author to select, we adapted three items from Plant and Devine’s external motivation to respond without prejudice scale (e.g., “When making my decision about who to select as my seventh author, I considered the social pressures I face to appear to promote racial minorities where they are typically underrepresented.”; Cronbach’s α = 0.90). Each item was collected on a scale from 1 (Strongly disagree) to 7 (Strongly agree). As pre-registered, we collapsed the items on a single scale by z-scoring and then averaging them. 
Results
Prior to random assignment, participants in both conditions first selected six authors. Of these initial selections, 21.8% were racial minority authors (balance was successfully achieved on this attribute: 20.7% in the no race feedback condition and 22.9% in the race feedback condition, t(993.88) = -1.58, p = .114, 95% CI = [-0.049, 0.005]). Following our pre-registration, we ran an ordinary least squares regression with robust standard errors to predict whether the seventh and final author a participant selected was a racial minority, and our primary predictor was an indicator for assignment to the race feedback condition. We again replicated the findings from our previous studies (see Table S7 for complete results). As predicted, we found that participants were significantly more likely to choose a racial minority author as their seventh and final author in the race feedback condition (41.60% chose a racial minority author) than in the no race feedback condition (28.20% chose a racial minority author; B = 0.134, 95% CI = [0.075, 0.193], t(998) = 4.48, p < .001; see Table S7, Model 1). In addition, as predicted, we again found that the effect of providing feedback about the race of initial selectees is significantly larger than the effects of providing descriptive feedback about other attributes of initial selectees (poets: F(1, 3984) = 6.19, p = .013; wrote > 10 books: F(1, 3984) = 5.50, p = .019; born in 1800s: F(1, 3984) = 5.26, p = .022; see Table S7, Models 5-8).
Mediation. Next, following our pre-registration, we tested each proposed mediator independently using a 10,000-sample bootstrapped mediation model and a Sobel test. We found that the 95% bias-corrected CI for the size of the indirect effect of internal motivation to respond without prejudice on choosing a racial minority as the final author excludes zero (indirect effect = 0.06, 95% CI = [0.034, 0.09], suggesting that participants’ internal motivation to respond without prejudice helped account for the effect of assignment to the race feedback condition on the likelihood of selecting a racial minority as the final author. Using Imai et al.’s (2010) average causal mediation effect approach, we found that 44.7% of the effect under study occurred through mediation by the participant’s internal motivation to respond without prejudice. In addition, we found that the 95% bias-corrected CI for the size of the indirect effect of external motivation to respond without prejudice excludes zero (indirect effect = 0.019, 95% CI = [0.005, 0.03]), suggesting that participants’ external motivation to respond without prejudice also helped account for the effect of assignment to the race feedback condition (vs. no race feedback) on a participant’s likelihood of selecting a racial minority as their final author. Here, Imai et al.’s average causal mediation effect approach suggested that 14.1% of the effect under study occurs through mediation by the participant’s external motivation to respond without prejudice. 
Given these results, we next tested whether both internal and external motivation to respond without prejudice would continue to mediate in a multiple mediation model, following our pre-registration. This multiple mediation model allows us to address potential multicollinearity between these correlated measures (r = 0.72, p < 0.001). The results suggested that, once we account for correlation between the two constructs, internal motivation to respond without prejudice (indirect effect = 0.08, 95% CI = [0.042, 0.11]), significantly mediates our effect of interest while external motivation to respond without prejudice (indirect effect = -0.014, 95% CI = [-0.03, 0.00]) does not.  
5.2 Study 4B
In this study, we replicate our key findings from Study 4A, but this time we provide feedback about the gender (rather than race) of the initial selectees. 
To ensure that the feedback offered in this study was on dimensions of selected authors that were perceived as equally important by participants, we first conducted a pretest with an independent sample of participants (N = 550). Participants in this pretest were asked to imagine creating a winter reading list for their local library and to rate the importance of considering various author attributes when making their selections on a scale from 1 (Not at all important) to 7 (Very important). We tested a set of 21 potential attributes in a between subjects design (so each participant rated one attribute). From this pretest, we selected the three attributes that were rated as more important than whether authors were women to include in our stimuli (M = 3.80, SD = 1.55): whether authors wrote works spanning multiple genres (M = 4.32, SD = 1.70, p = .265, d = -0.32), whether authors had at least one book with 30 million or more copies sold (M = 4.00, SD = 1.83, p = .679, d = -0.12), and whether authors had works that remained in continuous print for over 50 years (M = 4.20, SD = 1.53, p = .363, d = -0.26). These three attributes were then used as the comparison attributes. 
Given that liberals and conservatives differ not only in their attitudes toward diversity but also in their understanding of what diversity means (Howard, Cervone, & Motyl, 2022), and that recent research suggests diversity initiatives may produce divergent reactions across the political spectrum (Hachem & Dover, 2024), we also examine whether participants’ political ideology moderates the effect of providing descriptive feedback about the gender of past selectees.
Methods	Participants. As pre-registered (https://osf.io/de3bu/), we recruited 1,000 participants through Prolific Academic. Participants were based in the U.S., had a minimum 90% approval rate and at least 1,000 prior completions, and were paid $0.75 to complete a 3-minute survey (48.0% men, 50.7% women, 1.1% non-binary; Mage = 43.0; 75.9% White, 10.8% Black, 4.8% Hispanic, 8.0% Asian).
Procedure. Participants were asked to imagine they had been tasked with creating a winter fiction reading list for their local library. As in Study 4A, a list of 25 (real) authors was presented to each participant. This time, each author was displayed with a photograph and the following information: the author’s name (e.g., Kurt Vonnegut), their most notable work (e.g., Slaughterhouse-Five), their most notable work’s estimate of all-time sales and the length of time their most notable work has been in print (e.g., 800K copies sold, in print 56 years), number of works authored (e.g., 14 books, 3 essays), and genre(s) (e.g., Science Fiction, Satire). Six of the authors on this list (24%) were women.
We followed the same procedure described in Study 4A, inviting participants to select a portfolio of six authors whose work would be featured on a reading list at their local library, except instead of studying the effect of feedback about the race of selected authors, we studied the effect of feedback about their gender. Specifically, participants were randomly assigned to one of two conditions: participants in the no gender feedback condition received summary statistics about the percentage of authors they had selected who (1) wrote works spanning multiple genres, (2) had at least one book with 30 million or more copies sold, and/or (3) had works that remained in continuous print for over 50 years. In the gender feedback condition, participants always received feedback about the percentage of authors they had selected who were women, alongside two randomly drawn pieces of feedback from the list of items in the no gender feedback condition. After receiving feedback, participants selected a seventh and final author to recommend for their local library’s winter fiction reading list from the original candidate set, where their prior selections were highlighted. 
After making all decisions, participants were asked questions to assess their internal (Cronbach’s α = 0.93) and external (Cronbach’s α = 0.89) motivation to respond without prejudice, which were identical to the questions in Study 4A except for word changes made to focus on avoiding gender prejudice rather than racial prejudice. See Materials Section: Study 4B for full study materials. 
Political ideology. To measure participants’ political orientation, we used the 7-point political ideology scale from the American National Election Studies (ANES, 2022). Participants responded to the prompt “When it comes to politics, do you usually think of yourself as...?” on a scale ranging from 1 (Extremely liberal) to 7 (Extremely conservative), with 4 representing Moderate; middle of the road. This measure was centered at 4 (moderate) for analyses examining moderation effects.
Political party affiliation. To assess participants’ political party identification, we adapted the party identification item from the American National Election Studies (ANES, 2022). Participants were asked “Generally speaking, do you usually think of yourself as a Republican, a Democrat, or an Independent?” and selected one of three options: Democrat, Republican, or Independent. For regression analyses, party affiliation was dummy coded with Republican as the reference category.
Results 
Prior to random assignment, participants in both conditions first selected six authors. Of these initial selections, 25.3% were women authors (balance was successfully achieved on this attribute: 25.6% in the no gender feedback condition and 25.1% in the gender feedback condition, t(997.51) = 0.41, p = .679, 95% CI = [-0.020, 0.031]).
Following our pre-registration, we ran an ordinary least squares regression with robust standard errors to predict whether the seventh and final author a participant selected was a woman, and our primary predictor was an indicator for assignment to the gender feedback condition. First, we replicated the primary findings from our previous studies (see Table S9 for complete regression results). Again, confirming our prediction, we found that participants were significantly more likely to choose a woman author as their seventh and final selection in the gender feedback condition (41.48% chose a woman author) than in the no gender feedback condition (29.54% chose a woman author; B = 0.119, 95% CI = [0.060, 0.178], t(998) = 3.97, p < .001; Table S9, Model 1).
In addition, as theorized, we found that the effect of providing descriptive feedback about the gender of initial selectees is significantly larger than the effect of providing descriptive feedback about whether participants’ initial selectees wrote works spanning multiple genres (F(1, 1992) = 37.92, p < .001), wrote at least one book with 30 million or more copies sold (F(1, 1992) = 11.48, p < .001), or wrote any works that remained in continuous print for over 50 years (F(1, 1992) = 9.01, p = .003; see Table S9, Models 5-8).
Mediation. Next, following our pre-registration, we tested each proposed mediator independently using a 10,000-sample bootstrapped mediation model and a Sobel test (Imai et al., 2010). First, we found that the 95% bias-corrected CI for the indirect effect of internal motivation to respond without prejudice excludes zero (indirect effect = 0.064, 95% CI = [0.037, 0.094]), suggesting that participants’ internal motivation to respond without prejudice partially accounts for the effect of assignment to the gender feedback condition on the likelihood of selecting a woman as the final author. Using Imai et al.’s (2010) average causal mediation effect approach, we found that 53.9% of the effect under study occurs through mediation by a participant’s internal motivation to respond without prejudice. Next, we found that the 95% bias-corrected CI for the indirect effect of external motivation to respond without prejudice also excluded zero (indirect effect = 0.030, 95% CI = [0.014, 0.048]), suggesting that participants’ external motivation to respond without prejudice also accounted for the effect of assignment to the gender feedback condition on their likelihood of selecting a woman as their final author. Here, Imai et al.’s average causal mediation effect approach suggested that 24.8% of the effect under study occurs through mediation by a participant’s external motivation to respond without prejudice.
Following our pre-registration, we next tested whether both internal and external motivation to respond without prejudice would continue to mediate the effect under study in a multiple mediation model (these items were highly correlated: r = 0.78, p < .001). The results confirmed that internal motivation to respond without prejudice (indirect effect = 0.091, 95% CI = [0.052, 0.131]) significantly mediated the effect. When both mediators were included simultaneously, external motivation to respond without prejudice showed a significant negative indirect effect (indirect effect = -0.028, 95% CI = [-0.046, -0.012]).
To test whether political ideology or party affiliation moderated our effects, we added pre-registered interaction terms to our primary model. We found no significant interaction between assignment to the gender feedback condition and political ideology (B = -0.013, 95% CI = [-0.044, 0.018], t(996) = -0.80, p = .424), nor did we find significant interactions between assignment to the gender feedback condition and party affiliation (Democrat interaction: B = 0.019, 95% CI = [-0.125, 0.164], t(994) = 0.26, p = .792; Independent interaction: B = 0.067, 95% CI = [-0.085, 0.220], t(994) = 0.87, p = .387), 
Discussion
In Study 4, we again replicated the effect of descriptive feedback about the race and gender of past selectees increasing the subsequent selection of racial minorities and women. In addition, we found evidence that both internal and external motivation to respond without prejudice mediated this effect for subsequent selections of racial minorities (Study 4A) and women (Study 4B), respectively. When combining the highly correlated measures of internal and external motivation to respond without prejudice in a multiple mediation model, our findings suggest that internal motivation is the dominant mediator in our studies. However, external motivation to respond without prejudice could likely play a more significant role in settings where choices are made publicly. 
In addition, Study 4B established that the effect of gender feedback was not moderated by political ideology nor party affiliation. Finally, it ruled out the possibility that our effects were driven by perceptions of which attributes were considered most important.
