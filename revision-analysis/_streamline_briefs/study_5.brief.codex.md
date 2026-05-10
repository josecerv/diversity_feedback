# Brief: streamline Study 5 Methods + Results

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

EMPHASIS for this pass: HOLISTIC READER FLOW + ADDITIONAL CUTS.
Your priority is reader experience across the manuscript as a whole and finding cuts the first pass missed without re-triggering Katy's reverts. Read § VI of RULES_DISCOVERED.md ("Candidate cuts the first pass missed") and accept or reject each candidate cut explicitly. Look for sentences that repeat content the reader has just absorbed in a prior study. Tighten transitions and verb choices where the prior draft is wordy. Do not violate any rule in § I-V or any Katy red line.


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

# STUDY-SPECIFIC PRESERVATION RULES for Study 5 Methods + Results

- ONLY streamline Study 5 Methods + Results (NOT the Discussion). In your AFTER, output Methods + Results only. The source contains the full Study 5 including its Discussion; locate the 'Discussion' header and stop just before it.
- RETAIN the pooled interaction analysis (Table 4 mini meta-analysis, random-effects model) preceding the experimental design. This is the theoretical motivation for the 2x2 design and must survive. Compress wording but keep the result.
- 2x2 design: (gender feedback vs. no gender feedback) x (women underrepresented [18M/6W] vs. women overrepresented [18W/6M]). Preserve the design description.
- Stimulus set: 25 business leaders, randomized to 18M/6W or 18W/6M (18 men + 6 women, or 18 women + 6 men). Panelist-selection paradigm.
- Recruitment: Prolific (NOT MTurk), N=1,200. Demographics format per RULES_DISCOVERED § I.3: 39.3% men, 59.0% women, 1.6% non-binary, Mage = 42.7, race breakdown.
- Fairness-concerns mediator (3 new items on distributive justice) is DISTINCT from the internal/external MRWP measures used in 4A/4B. Preserve this distinction. Keep alpha for the fairness-concerns scale.
- Separate regressions by underrepresented vs. overrepresented subset (the structural finding is the reversal: when women overrepresented, descriptive feedback DECREASES selection of women). Preserve the separate-regressions structure.
- **STUDY 5 IS NOT IN TABLE 2.** Keep Study 5's own analysis intact. Specifically: KEEP the unequal-magnitude Wald *z*-test (z = 2.50, p = .012) in full because it is the structural finding of Study 5. For the cross-attribute comparison Wald F-tests (gender feedback vs. tech-industry/founders), check the source: if all are null, compress to 'all p's...' form; if any are significant, keep them in the verbose form (Study 5 does not have a Table 2 row to point at).
- Balance check per RULES_DISCOVERED § I.8 multi-cell form: report cell means + t + p + 95% CI for both 18M/6W and 18W/6M conditions because the 2x2 design requires it.
- Preserve 'See Materials Section: Study 5 in the Appendix for complete study materials.' Methods closing.
- Avoid em/en dashes in newly authored AFTER prose.
- In your BEFORE section, include the Study 5 Methods + Results text verbatim (from the 'Study 5' header through the 2x2 results, stopping just before the 'Discussion' header).

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

# PRIOR POST-KATY DRAFT for Study 5 Methods + Results (calibration anchor; do NOT copy verbatim — use only as a comparison reference)

## BEFORE
Study 5
In all of our studies so far, we examined situations where women or racial minorities were underrepresented in candidate pools, which means that mechanically they would also likely be underrepresented in many respondents’ initial set of selectees. As a result, the descriptive feedback we delivered typically highlighted that historically underrepresented groups had been picked at low rates (in fact, across all studies thus far, 83% of participants who received feedback about the gender or race of past selectees learned that the candidates they had initially selected were less than half women or racial minorities, respectively). In Study 5, we tested how descriptive feedback affects decision makers when it suggests that historically underrepresented groups have recently been picked at high rates. This would not, in theory, activate a motivation to respond without prejudice, since the initial feedback would indicate that historically underrepresented groups were being well-represented. Under such circumstances, we would no longer predict a positive effect of descriptive feedback about past decisions on the subsequent selection of candidates from a historically underrepresented population.
Before testing this prediction experimentally, we examined whether the size of the effect we study differs based on the proportion of women and racial minorities initially selected by re-analyzing data from Studies 2, 3A, 3B, 4A, and 4B. Specifically, we tested for an interaction between (1) the proportion of women or racial minorities a participant initially selected (before random assignment to condition) for inclusion in their portfolio and (2) random assignment to receive descriptive feedback about the fraction of women or racial minorities in their original portfolio of selectees. Based on our theorizing that feedback is unlikely to evoke a motivation to respond without prejudice when initial selections of women or racial minorities are high, we would predict a negative interaction between these predictors.
------------------------------------------------------------------
Insert Table 4 about here 
------------------------------------------------------------------ 
In Table 4, Models 1-5, we present five regression models re-analyzing data from Studies 2, 3A, 3B, 4A, and 4B, in which we added two new predictors: (1) the proportion of racial minorities or women (depending on the study) in the initially selected candidate set and (2) an interaction between this predictor and our primary predictor: assignment to the race feedback or gender feedback condition (depending on the study). In Model 1, which analyzes data from Study 2 (gender), we found the predicted significant negative interaction between the proportion of women selected initially and the impact of assignment to the gender feedback condition on subsequent selections of women (B = -0.107, 95% CI = [-0.198, -0.015], t(298) = -2.30, p = .022). In Model 2, which analyzes data from Study 3A (race), we found only directional but insignificant evidence of the predicted negative interaction between the proportion of racial minorities selected initially and the impact of assignment to the race feedback condition on subsequent selections of racial minorities (B = -0.019, 95% CI = [-0.070, 0.032], t(996) = -0.73, p = .465). In Model 3, which analyzes data from Study 3B (gender), we found the predicted significant negative interaction between the proportion of women selected initially and the impact of assignment to the gender feedback condition on subsequent selections of women (B = -0.087, 95% CI = [-0.141, -0.034], t(996) = -3.20, p = .001). In Model 4, which analyzed data from Study 4A (race), we found marginally significant evidence of the predicted negative interaction (B = -0.044, 95% CI = [-0.088, 0.001], t(996) = -1.92, p = .055). And in Model 5, which analyzed data from Study 4B (gender), we found a significant negative interaction (B = -0.046, 95% CI = [-0.090, -0.002], t(996) = -2.06, p = .040), as predicted. 	To quantify the overall pattern of interactions across these five studies, we conducted a mini meta-analysis combining the interaction coefficients from Models 1-5 (Goh et al., 2016). Specifically, we pooled the five coefficients representing the interaction between assignment to the gender (Study 2, 3B, and 4B) or race (Study 3A and 4A) feedback condition and the proportion of women or racial minorities initially selected. Using a random-effects model, which weighs each study’s contribution by its standard error, we found a significant negative pooled interaction effect (B = -0.052, 95% CI = [-0.075, -0.028], z = -4.36, p < .001). This suggests that the effect of descriptive feedback on female and racial minority selections is reliably larger when participants have selected a lower initial proportion of women or racial minorities and receive feedback to this effect.
However, because the proportion of women or racial minorities in participants’ initial selections was not randomly assigned (although selections were made before random assignment) and was somewhat restricted in range, we conducted an experiment where we used our standard experimental setup from Studies 2-4 but exogenously varied whether women made up the minority versus the majority of candidates available for selection. We then examined how exogenously manipulating whether people likely select very few versus very many female candidates related to the effect of descriptive feedback about the gender of their past selectees on the gender of their subsequent selectees.
------------------------------------------------------------------
Insert Table 5 and Figure 4 about here 
------------------------------------------------------------------ 
Methods

Participants. As pre-registered (https://aspredicted.org/xt2ct6), we recruited 1,200 participants through Prolific Academic. Participants were based in the U.S., had a minimum 90% approval rate and at least 1,000 prior survey completions, and were paid $0.75 to complete a 3-minute survey (39.3% men, 59.0% women, 1.6% non-binary/other; Mage = 42.7; 70.0% White, 11.2% Black, 9.3% Hispanic, 8.4% Asian).
Procedure. Participants were asked to imagine that they were planning a major international business conference and had been tasked with selecting a panel of business leaders to provide insights into the global business landscape during the event. We then presented them with a list of 25 real business leaders in randomized order. We included each leader’s name (e.g., Jeff Bezos, Mary Barra), title (e.g., Founder, CEO), and company affiliation (e.g., Amazon, General Motors). Participants were instructed to select a portfolio of six leaders for their panel. After creating their six-person panel, they were presented with summary statistics describing the composition of their selected portfolio of panelists.
Participants were randomly assigned to one of four conditions in a 2 (gender feedback vs. no gender feedback) X 2 (women underrepresented vs. women overrepresented) design. Participants randomly assigned to the women underrepresented conditions made selections from a panelist candidate list including 18 men and 6 women (women comprised 25% of the initial candidate set). In the women overrepresented conditions, participants made selections from a panelist candidate list including 18 women and 6 men (women comprised 75% of the initial candidate set). Participants in the no gender feedback conditions were randomly assigned to receive separate summary statistics about both the percentage of panelists they initially selected who were (1) in the tech industry and (2) company founders. In the gender feedback conditions, participants received feedback about the percentage of panelists they had selected who were women as well as the two summary statistics about past selectees that were provided to participants in the no gender feedback condition. We ensured balance in the fraction of men and women candidates who were in the tech industry and company founders across the women underrepresented and women overrepresented conditions. After receiving feedback on their initial selections, participants were asked to select a seventh and final panelist from the original set, where their prior selections were highlighted.
In addition, we collected survey responses to questions designed to capture potential mechanisms that might motivate final panelist selections. First, as in Study 4B, we assessed internal motivation to respond without prejudice using four items adapted from Plant and Devine’s (1998) scale (Cronbach’s α = 0.93) as well as external motivation to respond without prejudice using three items from the same scale (Cronbach's α = 0.91). In addition, we measured potential concerns about the fairness of the gender balance in the set, which we speculated could lead feedback to reduce selections of women in cases where it highlighted the underrepresentation of men.  We measured this type of fairness concern using three items adapted from research on distributive justice (Adams, 1965; Colquitt, 2001): (1) “When deciding who to add as my seventh panelist, I prioritized achieving a degree of balance in the representation of men and women,” (2) “An important factor when deciding who to add as my seventh panelist was avoiding extreme over- or under-representation of men or women,” and (3) “I aimed to ensure that neither men nor women were left out when making my final decision” (Cronbach’s α = 0.90). All items were collected on scales from 1 (Strongly disagree) to 7 (Strongly agree) and collapsed into scales by z-scoring and then averaging across items. See Materials Section: Study 5 in the Appendix for complete study materials.
Results
Participants in all conditions first selected six panelists. In the women overrepresented condition, 60.7% of these initial selectees were women (balance was successfully achieved on this attribute: 59.8% of initial selectees were women in the no gender feedback condition and 61.6% were women in the gender feedback condition, t(599.9) = -1.11, p = .268, 95% CI = [-0.049, 0.014]). In the women underrepresented condition, 21.0% of initial selectees were women (balance was successfully achieved on this attribute: 20.5% of initial selectees were women in the no gender feedback condition and 21.4% were women in the gender feedback condition, t(596) = -0.49, p = .624, 95% CI = [-0.045, 0.027]).
For simplicity, we began by estimating separate regressions (pre-registered as secondary analyses) predicting whether the seventh and final panelist a participant selected was a woman in which the primary predictor was an indicator for assignment to the gender feedback condition, among (1) the subset of participants assigned to the women underrepresented condition (see Table 5, Models 1-2) and (2) the subset of participants assigned to the women overrepresented condition (see Table 5, Models 3-4). Replicating our prior studies’ findings, Figure 4 and Table 5 (Model 1) show that when participants were randomly assigned to the women underrepresented condition (and selected panelists from a candidate pool in which women leaders were underrepresented), they were significantly more likely to choose a woman as their seventh and final panelist when assigned to the gender feedback condition (57.5% chose a woman panelist) than to the no gender feedback condition (33.3% chose a woman panelist; B_gender feedback = 0.241, 95% CI [0.164, 0.319], t(596) = 6.09, p < .001). However, as shown in Figure 4 and Table 5 (Model 3), when participants were randomly assigned to the women overrepresented condition, the effect of feedback reversed in direction: participants in the gender feedback condition selected a woman as their seventh and final panelist 66.1% of the time, whereas participants in the no gender feedback condition selected a woman as their final panelist 76.7% of the time (B_gender feedback = -0.106, 95% CI [-0.178, -0.034], t(600) = -2.90, p = .004). Both simple effects were robust to controlling for participant gender, race, and age (see Table 5, Models 2 and 4).
Our primary pre-registered analysis pooled data across all conditions in a single OLS regression with robust standard errors to predict whether the seventh and final panelist a participant selected was a woman. Our primary predictors were: (1) an indicator for assignment to the gender feedback condition, (2) an indicator for assignment to the women overrepresented condition (with women underrepresented as the reference category), and (3) an interaction between the prior two indicators. As shown in Table 5, Model 5, the coefficient on the gender feedback indicator in this regression was positive and significant (B_gender feedback = 0.241, SE = 0.040, p < .001), indicating that when women were underrepresented in the candidate pool, receiving gender feedback increased the likelihood of selecting a woman as the final panelist by 24.1 percentage points. The coefficient on the women overrepresented indicator was also positive and significant (B_women overrepresented = 0.434, SE = 0.037, p < .001), indicating that in the absence of gender feedback, participants were 43.4 percentage points more likely to select a woman when women were overrepresented in the candidate pool compared to when women were underrepresented. Most notably, the interaction between the prior two indicators was negative and significant (B_gender feedback x women overrepresented = -0.348, SE = 0.054, p < .001), indicating that the effect of gender feedback was 34.8 percentage points lower when women were overrepresented than when they were underrepresented. In other words, gender feedback decreased the selection of women by 10.6 percentage points when women were overrepresented (vs. increasing selection of women by 24.1 percentage points when women were underrepresented). These results were robust to controlling for participant gender, race, and age (see Table 5, Model 6).
Finally, we confirmed that not only did the effect of the gender feedback treatment flip across conditions, it was also unequal in magnitude across the women overrepresented and women underrepresented conditions (Wald Test: z = 2.50, p = .012). Specifically, the magnitude of the effect was 56% smaller in magnitude in the women overrepresented condition than in the women underrepresented condition.
Exploratory Mediation Analyses. In analyses that were not pre-registered, we found that in the women underrepresented condition there were three significant mediators of the effect of descriptive feedback on the increased selection of women: internal motivation to respond without prejudice (indirect effect = 0.103, 95% CI = [0.070, 0.139], p < .001; replicating the results of Study 4), external motivation to respond without prejudice (indirect effect = 0.046, 95% CI = [0.025, 0.072], p < .001; replicating the results of Study 4), and fairness concerns (indirect effect = 0.101, 95% CI = [0.067, 0.137], p < .001). Fairness concerns were highly correlated with both measures of motivation to respond without prejudice (r_internal,fairness = .86; r_external,fairness = .68).	In the women overrepresented condition, however, there was only one significant mediator of the effect of descriptive feedback on the increased selection of men: fairness concerns (indirect effect = -0.012, 95% CI [-0.026, -0.002], p = .013) mediated this effect. Neither internal motivation to respond without prejudice (indirect effect = -0.010, 95% CI [-0.024, 0.001], p = .072) nor external motivation to respond without prejudice (indirect effect = -0.003, 95% CI [-0.011, 0.003], p = .409) significantly mediated the decreased selection of women induced by descriptive feedback in the women overrepresented condition (though internal motivation to respond without prejudice was a marginally significant mediator). This is consistent with our theory that these mechanisms are only triggered when feedback suggests that a historically underrepresented group (e.g., women) has plausibly faced prejudice.

## AFTER
Study 5
In prior studies, most participants who received race or gender feedback learned that their initial selections were less than half women or racial minorities. Study 5 tested whether descriptive gender feedback still increases women's selection when women are likely to be overrepresented rather than underrepresented among initial selectees.

Before the experiment, we re-analyzed Studies 2, 3A, 3B, 4A, and 4B to test whether the effect of race/gender feedback was moderated by the initial proportion of women/racial minorities selected. Table 4, Models 1-5, added that initial proportion and its interaction with assignment to the race/gender feedback condition; interactions were negative in all five studies and significant or marginally significant in Studies 2, 3B, 4A, and 4B (Bs = -0.107, -0.019, -0.087, -0.044, and -0.046; ps = .022, .465, .001, .055, and .040). A random-effects mini meta-analysis pooling the five interaction coefficients found a significant negative interaction (B = -0.052, 95% CI = [-0.075, -0.028], z = -4.36, p < .001), suggesting feedback's effect is larger when participants initially select fewer women or racial minorities and receive feedback to this effect.

------------------------------------------------------------------
Insert Table 4 about here
------------------------------------------------------------------

Because participants' initial selections in those studies were not randomly assigned, we next experimentally manipulated whether women were underrepresented versus overrepresented in the candidate pool, using the same panelist-selection paradigm to test whether gender feedback has different effects when women are likely to be selected at low versus high rates.

------------------------------------------------------------------
Insert Table 5 and Figure 4 about here
------------------------------------------------------------------

Methods

Participants. As pre-registered (https://aspredicted.org/xt2ct6), we recruited 1,200 participants through Prolific Academic to complete a 3-minute survey for $0.75 (39.3% men, 59.0% women, 1.6% non-binary/other; Mage = 42.7; 70.0% White, 11.2% Black, 9.3% Hispanic, 8.4% Asian).
Procedure. Participants imagined planning a major international business conference and selected six business leaders for a panel from a randomized list of 25 real leaders, each shown with name (e.g., Jeff Bezos, Mary Barra), title (e.g., Founder, CEO), and company affiliation (e.g., Amazon, General Motors). After creating their six-person panel, they received summary statistics and then selected a seventh and final panelist from the original set, where their prior selections were highlighted.
Participants were randomly assigned to one of four conditions in a 2 (gender feedback vs. no gender feedback) X 2 (women underrepresented vs. women overrepresented) design. In the women underrepresented conditions, the candidate list included 18 men and 6 women (25% women); in the women overrepresented conditions, it included 18 women and 6 men (75% women). Participants in the no gender feedback conditions received summary statistics about the percentage of their initially selected panelists who were (1) in the tech industry and (2) company founders; participants in the gender feedback conditions also received feedback about the percentage of their initially selected panelists who were women. We balanced the fraction of men and women candidates who were in the tech industry and company founders across representation conditions.
We also measured potential mechanisms. As in Study 4B, we assessed internal motivation to respond without prejudice with four Plant and Devine (1998) items (Cronbach's α = 0.93) and external motivation to respond without prejudice with three items from the same scale (Cronbach's α = 0.91). Distinct from these MRWP measures, we assessed fairness concerns about gender balance using three distributive-justice items: prioritizing gender balance, avoiding extreme over- or under-representation of men or women, and ensuring that neither men nor women were left out (Cronbach's α = 0.90). All items used 1 (Strongly disagree) to 7 (Strongly agree) scales and were z-scored and averaged. See Materials Section: Study 5 in the Appendix for complete study materials.

Results
Initial six-person panels were 60.7% women in the women overrepresented condition, with balance across feedback conditions (59.8% in no gender feedback vs. 61.6% in gender feedback; t(599.9) = -1.11, p = .268, 95% CI = [-0.049, 0.014]), and 21.0% women in the women underrepresented condition, also balanced across feedback conditions (20.5% vs. 21.4%; t(596) = -0.49, p = .624, 95% CI = [-0.045, 0.027]).
Following our pre-registration, we first estimated separate OLS regressions (pre-registered as secondary analyses) by representation condition predicting whether the seventh and final panelist selected was a woman. Replicating prior findings, Figure 4 and Table 5 (Model 1) show that participants in the women underrepresented condition were more likely to choose a woman as their seventh and final panelist when assigned to the gender feedback condition (57.5% chose a woman panelist) than to the no gender feedback condition (33.3% chose a woman panelist; B_gender feedback = 0.241, 95% CI [0.164, 0.319], t(596) = 6.09, p < .001). In the women overrepresented condition, the effect reversed: participants in the gender feedback condition selected a woman as their seventh and final panelist 66.1% of the time, versus 76.7% in the no gender feedback condition (B_gender feedback = -0.106, 95% CI [-0.178, -0.034], t(600) = -2.90, p = .004; Figure 4; Table 5, Model 3). Both simple effects were robust to controlling for participant gender, race, and age (Table 5, Models 2 and 4).
Our primary pre-registered analysis pooled all conditions in a single OLS regression with robust standard errors predicting whether the seventh and final panelist selected was a woman from indicators for gender feedback, women overrepresented condition, and their interaction. As shown in Table 5 (Model 5), the gender feedback coefficient was positive and significant (B_gender feedback = 0.241, SE = 0.040, p < .001), indicating that when women were underrepresented, gender feedback increased selection of a woman final panelist by 24.1 percentage points; the women overrepresented coefficient was also positive and significant (B_women overrepresented = 0.434, SE = 0.037, p < .001), indicating that without gender feedback, participants were 43.4 percentage points more likely to select a woman when women were overrepresented than underrepresented. Most importantly, the interaction was negative and significant (B_gender feedback x women overrepresented = -0.348, SE = 0.054, p < .001): gender feedback's effect was 34.8 percentage points lower when women were overrepresented, decreasing women's selection by 10.6 percentage points instead of increasing it by 24.1 percentage points. These results were robust to controlling for participant gender, race, and age (Table 5, Model 6).
Finally, the effect of gender feedback both reversed across conditions and was unequal in magnitude across the women underrepresented and women overrepresented conditions (Wald Test: z = 2.50, p = .012); the effect was 56% smaller in magnitude when women were overrepresented.
Exploratory Mediation Analyses. In non-pre-registered analyses, three mediators significantly explained the effect of descriptive feedback on increased selection of women in the women underrepresented condition: internal motivation to respond without prejudice (indirect effect = 0.103, 95% CI = [0.070, 0.139], p < .001), external motivation to respond without prejudice (indirect effect = 0.046, 95% CI = [0.025, 0.072], p < .001), and fairness concerns (indirect effect = 0.101, 95% CI = [0.067, 0.137], p < .001). Fairness concerns were highly correlated with both measures of motivation to respond without prejudice (r_internal,fairness = .86; r_external,fairness = .68). In the women overrepresented condition, only fairness concerns significantly mediated the effect of descriptive feedback on increased selection of men (indirect effect = -0.012, 95% CI [-0.026, -0.002], p = .013). Neither internal motivation to respond without prejudice (indirect effect = -0.010, 95% CI [-0.024, 0.001], p = .072) nor external motivation to respond without prejudice (indirect effect = -0.003, 95% CI [-0.011, 0.003], p = .409) significantly mediated the decreased selection of women. These exploratory results preserve the distinction between MRWP and fairness concerns: MRWP is triggered when feedback suggests that a historically underrepresented group has plausibly faced prejudice, whereas fairness concerns explain the reversal when feedback highlights men's underrepresentation.

## Word count: 2415 -> 1265 (saved 1150)

## Preservation notes
- Kept the pooled interaction analysis and random-effects mini meta-analysis because Study 5 uses it to motivate the 2x2 design; compressed model-by-model prose while retaining all interaction coefficients, p-values, and the pooled result.
- Kept demographics per Katy red line #1 and stripped later-study screening boilerplate per the accepted template.
- Kept the 25-leader stimulus description and the “original set, where their prior selections were highlighted” detail per Katy red line #4.
- Kept both balance checks with cell means, p-values, and confidence intervals per Katy red line #9.
- Kept cell-mean DV phrasing for final woman-panelist selection and retained Figure 4/Table 5 pointers per Katy red line #5.
- Preserved the separate-regressions structure for underrepresentation versus overrepresentation because the reversal is the Study 5 structural finding.
- Departed from the verbose Wald F-test template because Study 5 reports a Wald z-test rather than the multi-feedback-condition F-test enumeration; retained the z-test and magnitude comparison in full.
- Preserved the fairness-concerns mediator as distinct from internal/external MRWP, including α values, indirect effects, correlations, and non-pre-registered status.
- Stopped before Discussion and omitted the benchmarking study, as requested.

---

# SOURCE (verbatim from live Gdoc, refreshed 2026-05-10; do not modify)

Study 5
In all of our studies so far, we examined situations where women or racial minorities were underrepresented in candidate pools, which means that mechanically they would also likely be underrepresented in many respondents’ initial set of selectees. As a result, the descriptive feedback we delivered typically highlighted that historically underrepresented groups had been picked at low rates (in fact, across all studies thus far, 83% of participants who received feedback about the gender or race of past selectees learned that the candidates they had initially selected were less than half women or racial minorities, respectively). In Study 5, we tested how descriptive feedback affects decision makers when it suggests that historically underrepresented groups have recently been picked at high rates. This would not, in theory, activate a motivation to respond without prejudice, since the initial feedback would indicate that historically underrepresented groups were being well-represented. Under such circumstances, we would no longer predict a positive effect of descriptive feedback about past decisions on the subsequent selection of candidates from a historically underrepresented population.
Before testing this prediction experimentally, we examined whether the size of the effect we study differs based on the proportion of women and racial minorities initially selected by re-analyzing data from Studies 2, 3A, 3B, 4A, and 4B. Specifically, we tested for an interaction between (1) the proportion of women or racial minorities a participant initially selected (before random assignment to condition) for inclusion in their portfolio and (2) random assignment to receive descriptive feedback about the fraction of women or racial minorities in their original portfolio of selectees. Based on our theorizing that feedback is unlikely to evoke a motivation to respond without prejudice when initial selections of women or racial minorities are high, we would predict a negative interaction between these predictors.
------------------------------------------------------------------
Insert Table 4 about here 
------------------------------------------------------------------ 
In Table 4, Models 1-5, we present five regression models re-analyzing data from Studies 2, 3A, 3B, 4A, and 4B, in which we added two new predictors: (1) the proportion of racial minorities or women (depending on the study) in the initially selected candidate set and (2) an interaction between this predictor and our primary predictor: assignment to the race feedback or gender feedback condition (depending on the study). In Model 1, which analyzes data from Study 2 (gender), we found the predicted significant negative interaction between the proportion of women selected initially and the impact of assignment to the gender feedback condition on subsequent selections of women (B = -0.107, 95% CI = [-0.198, -0.015], t(298) = -2.30, p = .022). In Model 2, which analyzes data from Study 3A (race), we found only directional but insignificant evidence of the predicted negative interaction between the proportion of racial minorities selected initially and the impact of assignment to the race feedback condition on subsequent selections of racial minorities (B = -0.019, 95% CI = [-0.070, 0.032], t(996) = -0.73, p = .465). In Model 3, which analyzes data from Study 3B (gender), we found the predicted significant negative interaction between the proportion of women selected initially and the impact of assignment to the gender feedback condition on subsequent selections of women (B = -0.087, 95% CI = [-0.141, -0.034], t(996) = -3.20, p = .001). In Model 4, which analyzed data from Study 4A (race), we found marginally significant evidence of the predicted negative interaction (B = -0.044, 95% CI = [-0.088, 0.001], t(996) = -1.92, p = .055). And in Model 5, which analyzed data from Study 4B (gender), we found a significant negative interaction (B = -0.046, 95% CI = [-0.090, -0.002], t(996) = -2.06, p = .040), as predicted. 	To quantify the overall pattern of interactions across these five studies, we conducted a mini meta-analysis combining the interaction coefficients from Models 1-5 (Goh et al., 2016). Specifically, we pooled the five coefficients representing the interaction between assignment to the gender (Study 2, 3B, and 4B) or race (Study 3A and 4A) feedback condition and the proportion of women or racial minorities initially selected. Using a random-effects model, which weighs each study’s contribution by its standard error, we found a significant negative pooled interaction effect (B = -0.052, 95% CI = [-0.075, -0.028], z = -4.36, p < .001). This suggests that the effect of descriptive feedback on female and racial minority selections is reliably larger when participants have selected a lower initial proportion of women or racial minorities and receive feedback to this effect.
However, because the proportion of women or racial minorities in participants’ initial selections was not randomly assigned (although selections were made before random assignment) and was somewhat restricted in range, we conducted an experiment where we used our standard experimental setup from Studies 2-4 but exogenously varied whether women made up the minority versus the majority of candidates available for selection. We then examined how exogenously manipulating whether people likely select very few versus very many female candidates related to the effect of descriptive feedback about the gender of their past selectees on the gender of their subsequent selectees.
------------------------------------------------------------------
Insert Table 5 and Figure 4 about here 
------------------------------------------------------------------ 
Methods

Participants. As pre-registered (https://aspredicted.org/xt2ct6), we recruited 1,200 participants through Prolific Academic. Participants were based in the U.S., had a minimum 90% approval rate and at least 1,000 prior survey completions, and were paid $0.75 to complete a 3-minute survey (39.3% men, 59.0% women, 1.6% non-binary/other; Mage = 42.7; 70.0% White, 11.2% Black, 9.3% Hispanic, 8.4% Asian).
Procedure. Participants were asked to imagine that they were planning a major international business conference and had been tasked with selecting a panel of business leaders to provide insights into the global business landscape during the event. We then presented them with a list of 25 real business leaders in randomized order. We included each leader’s name (e.g., Jeff Bezos, Mary Barra), title (e.g., Founder, CEO), and company affiliation (e.g., Amazon, General Motors). Participants were instructed to select a portfolio of six leaders for their panel. After creating their six-person panel, they were presented with summary statistics describing the composition of their selected portfolio of panelists.
Participants were randomly assigned to one of four conditions in a 2 (gender feedback vs. no gender feedback) X 2 (women underrepresented vs. women overrepresented) design. Participants randomly assigned to the women underrepresented conditions made selections from a panelist candidate list including 18 men and 6 women (women comprised 25% of the initial candidate set). In the women overrepresented conditions, participants made selections from a panelist candidate list including 18 women and 6 men (women comprised 75% of the initial candidate set). Participants in the no gender feedback conditions were randomly assigned to receive separate summary statistics about both the percentage of panelists they initially selected who were (1) in the tech industry and (2) company founders. In the gender feedback conditions, participants received feedback about the percentage of panelists they had selected who were women as well as the two summary statistics about past selectees that were provided to participants in the no gender feedback condition. We ensured balance in the fraction of men and women candidates who were in the tech industry and company founders across the women underrepresented and women overrepresented conditions. After receiving feedback on their initial selections, participants were asked to select a seventh and final panelist from the original set, where their prior selections were highlighted.
In addition, we collected survey responses to questions designed to capture potential mechanisms that might motivate final panelist selections. First, as in Study 4B, we assessed internal motivation to respond without prejudice using four items adapted from Plant and Devine’s (1998) scale (Cronbach’s α = 0.93) as well as external motivation to respond without prejudice using three items from the same scale (Cronbach's α = 0.91). In addition, we measured potential concerns about the fairness of the gender balance in the set, which we speculated could lead feedback to reduce selections of women in cases where it highlighted the underrepresentation of men.  We measured this type of fairness concern using three items adapted from research on distributive justice (Adams, 1965; Colquitt, 2001): (1) “When deciding who to add as my seventh panelist, I prioritized achieving a degree of balance in the representation of men and women,” (2) “An important factor when deciding who to add as my seventh panelist was avoiding extreme over- or under-representation of men or women,” and (3) “I aimed to ensure that neither men nor women were left out when making my final decision” (Cronbach’s α = 0.90). All items were collected on scales from 1 (Strongly disagree) to 7 (Strongly agree) and collapsed into scales by z-scoring and then averaging across items. See Materials Section: Study 5 in the Appendix for complete study materials.
Results
Participants in all conditions first selected six panelists. In the women overrepresented condition, 60.7% of these initial selectees were women (balance was successfully achieved on this attribute: 59.8% of initial selectees were women in the no gender feedback condition and 61.6% were women in the gender feedback condition, t(599.9) = -1.11, p = .268, 95% CI = [-0.049, 0.014]). In the women underrepresented condition, 21.0% of initial selectees were women (balance was successfully achieved on this attribute: 20.5% of initial selectees were women in the no gender feedback condition and 21.4% were women in the gender feedback condition, t(596) = -0.49, p = .624, 95% CI = [-0.045, 0.027]).
For simplicity, we began by estimating separate regressions (pre-registered as secondary analyses) predicting whether the seventh and final panelist a participant selected was a woman in which the primary predictor was an indicator for assignment to the gender feedback condition, among (1) the subset of participants assigned to the women underrepresented condition (see Table 5, Models 1-2) and (2) the subset of participants assigned to the women overrepresented condition (see Table 5, Models 3-4). Replicating our prior studies’ findings, Figure 4 and Table 5 (Model 1) show that when participants were randomly assigned to the women underrepresented condition (and selected panelists from a candidate pool in which women leaders were underrepresented), they were significantly more likely to choose a woman as their seventh and final panelist when assigned to the gender feedback condition (57.5% chose a woman panelist) than to the no gender feedback condition (33.3% chose a woman panelist; B_gender feedback = 0.241, 95% CI [0.164, 0.319], t(596) = 6.09, p < .001). However, as shown in Figure 4 and Table 5 (Model 3), when participants were randomly assigned to the women overrepresented condition, the effect of feedback reversed in direction: participants in the gender feedback condition selected a woman as their seventh and final panelist 66.1% of the time, whereas participants in the no gender feedback condition selected a woman as their final panelist 76.7% of the time (B_gender feedback = -0.106, 95% CI [-0.178, -0.034], t(600) = -2.90, p = .004). Both simple effects were robust to controlling for participant gender, race, and age (see Table 5, Models 2 and 4).
Our primary pre-registered analysis pooled data across all conditions in a single OLS regression with robust standard errors to predict whether the seventh and final panelist a participant selected was a woman. Our primary predictors were: (1) an indicator for assignment to the gender feedback condition, (2) an indicator for assignment to the women overrepresented condition (with women underrepresented as the reference category), and (3) an interaction between the prior two indicators. As shown in Table 5, Model 5, the coefficient on the gender feedback indicator in this regression was positive and significant (B_gender feedback = 0.241, SE = 0.040, p < .001), indicating that when women were underrepresented in the candidate pool, receiving gender feedback increased the likelihood of selecting a woman as the final panelist by 24.1 percentage points. The coefficient on the women overrepresented indicator was also positive and significant (B_women overrepresented = 0.434, SE = 0.037, p < .001), indicating that in the absence of gender feedback, participants were 43.4 percentage points more likely to select a woman when women were overrepresented in the candidate pool compared to when women were underrepresented. Most notably, the interaction between the prior two indicators was negative and significant (B_gender feedback x women overrepresented = -0.348, SE = 0.054, p < .001), indicating that the effect of gender feedback was 34.8 percentage points lower when women were overrepresented than when they were underrepresented. In other words, gender feedback decreased the selection of women by 10.6 percentage points when women were overrepresented (vs. increasing selection of women by 24.1 percentage points when women were underrepresented). These results were robust to controlling for participant gender, race, and age (see Table 5, Model 6).
Finally, we confirmed that not only did the effect of the gender feedback treatment flip across conditions, it was also unequal in magnitude across the women overrepresented and women underrepresented conditions (Wald Test: z = 2.50, p = .012). Specifically, the magnitude of the effect was 56% smaller in magnitude in the women overrepresented condition than in the women underrepresented condition.
Exploratory Mediation Analyses. In analyses that were not pre-registered, we found that in the women underrepresented condition there were three significant mediators of the effect of descriptive feedback on the increased selection of women: internal motivation to respond without prejudice (indirect effect = 0.103, 95% CI = [0.070, 0.139], p < .001; replicating the results of Study 4), external motivation to respond without prejudice (indirect effect = 0.046, 95% CI = [0.025, 0.072], p < .001; replicating the results of Study 4), and fairness concerns (indirect effect = 0.101, 95% CI = [0.067, 0.137], p < .001). Fairness concerns were highly correlated with both measures of motivation to respond without prejudice (r_internal,fairness = .86; r_external,fairness = .68).	In the women overrepresented condition, however, there was only one significant mediator of the effect of descriptive feedback on the increased selection of men: fairness concerns (indirect effect = -0.012, 95% CI [-0.026, -0.002], p = .013) mediated this effect. Neither internal motivation to respond without prejudice (indirect effect = -0.010, 95% CI [-0.024, 0.001], p = .072) nor external motivation to respond without prejudice (indirect effect = -0.003, 95% CI [-0.011, 0.003], p = .409) significantly mediated the decreased selection of women induced by descriptive feedback in the women overrepresented condition (though internal motivation to respond without prejudice was a marginally significant mediator). This is consistent with our theory that these mechanisms are only triggered when feedback suggests that a historically underrepresented group (e.g., women) has plausibly faced prejudice.
Discussion
	This study shows that when women are underrepresented in a candidate pool, descriptive feedback about the prior fraction of women selected increases the likelihood of subsequently selecting a woman, replicating our findings from Studies 3A, 3B, 4A, and 4B. However, when women are overrepresented, feedback produces a statistically smaller effect in the opposite direction, decreasing the selection of women. 
	Exploratory mediation analyses suggest that different mechanisms are key drivers of the effects of descriptive feedback on selection decisions after people learn that women were initially underrepresented versus overrepresented in their original pool of picks. Consistent with Study 4 and our theorizing, when women are underrepresented, feedback triggers a motivation to respond without prejudice, which prompts decision makers to select more women. However, when women are overrepresented, general concerns about treating men and women fairly emerge as the main mediator (notably, these concerns arise alongside a motivation to respond without prejudice when women are overrepresented). These fairness concerns drive people to select candidates who reduce gender imbalance.
