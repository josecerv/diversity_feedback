# Brief: streamline Study 3 Discussion (covers 3A + 3B + supplementary S2A/S2B demand-effect ruling-out)

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

# STUDY-SPECIFIC PRESERVATION RULES for Study 3 Discussion (covers 3A + 3B + supplementary S2A/S2B demand-effect ruling-out)

- This is a Discussion (not Methods/Results). The four required output sections still apply, but the AFTER content is a discussion paragraph cluster.
- Preserve the demand-effect framing: this is the contribution of Studies 3A/3B + S2A/S2B together. Do not weaken it.
- Compress the S2A/S2B stat enumeration (Studies S2A and S2B detailed in pages 7-11 in the Appendix). Replace the verbose B/p enumeration ('B = 0.030, p = .471... B = 0.053, p = .176... interaction B = -0.005, p = .941... interaction B = 0.019, p = .748') with a summary: 'all main-effect p's > .15 and all interaction p's > .15 (see Appendix Tables S17-S22)'.
- Cut redundant restatements of what 3A/3B found in Results.
- Keep the alternative-explanation framing (demand effect, Orne 1962, Zizzo 2010 citations). These are essential.
- Aim for ~40-50% reduction. Currently ~600+ words; target ~300-350 words.

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

# PRIOR POST-KATY DRAFT for Study 3 Discussion (covers 3A + 3B + supplementary S2A/S2B demand-effect ruling-out) (calibration anchor; do NOT copy verbatim — use only as a comparison reference)

## BEFORE

Discussion
Across two experiments involving incentivized decisions, we found that providing descriptive feedback about the race (Study 3A) or gender (Study 3B) of protagonists featured in past films selected for a marketing campaign significantly increased the likelihood that participants subsequently selected films featuring women or racial minorities, respectively. One possible alternative explanation for our findings is that providing participants with feedback about the race or gender of past selections signaled that we value racial or gender diversity. For example, when participants first selected films, they might have chosen based on personal preferences. But when they received descriptive feedback summarizing the attributes of their selected biopics, evaluators may have inferred that protagonist race (in Study 3A) or gender (in Study 3B) were of particular significance to the experimenter and attended to these attributes more in subsequent decisions as a result of a “demand effect” (Orne, 1962; Zizzo, 2010). If this alternative explanation were driving our effects, however, we would also expect participants to respond to the “demand effect” inherent in receiving feedback about other attributes of their selected films (e.g., feedback about a film’s budget or year of release), but we found no such patterns. 
However, to further rule out a “demand effect” explanation for our findings, we conducted two supplemental experiments (Studies S2A and S2B, detailed in pages 7-11 in the Appendix). In both studies, we employed a 2x2 design, crossing our original gender feedback manipulation with the addition of a values-based injunctive message (i.e., “The organization planning this event cares deeply about presenting a diversity of speakers in terms of [their experience in CEO roles, their experience as company founders, their background in the technology industry] and their gender.”) to the participant emphasizing the importance of making diverse selections. This message was designed to parallel real-world organizational communications, which typically signal that diversity is valued rather than issue direct commands about whom to select. In Study S2A, participants made hypothetical selections of corporate speakers to join a high-profile panel, while in Study S2B, participants made incentivized selections of podcasts hosted by prominent men and women to include in a Facebook advertisement. Across both studies, we replicated our finding from Study 3B: descriptive feedback about the gender of past selectees increased the likelihood that participants subsequently selected a woman. However, explicitly highlighting the importance of making diverse selections (via an explicit injunctive norm message) did not significantly increase the selection of women candidates (in S2A: the message produced a regression-estimated 3% increase in the selection of women panelists, B = 0.030, p = .471; in S2B: the message produced a regression-estimated 5% increase in the selection of podcasts with women hosts, B = 0.053, p = .176). Moreover, in neither study did the effect of the explicit injunctive norm message highlighting the importance of diverse selections significantly alter the effect of providing descriptive feedback about the gender of past selectees (Study S2A interaction: B = -0.005, p = .941; Study S2B interaction: B = 0.019, p = .748; see Appendix Tables S17-S22 for full results). Together, these findings suggest our results in Study 3 are not the product of an experimenter demand effect.

## AFTER

Discussion
Across two incentivized experiments, descriptive feedback about the race (Study 3A) or gender (Study 3B) of protagonists featured in previously selected films increased subsequent selections of films featuring racial minorities or women, respectively. One possible alternative explanation is that the feedback itself signaled that we valued racial or gender diversity: after initially choosing films based on personal preferences, participants may have inferred from race or gender feedback that these attributes mattered to the experimenter and attended to them more because of a “demand effect” (Orne, 1962; Zizzo, 2010). Yet if demand drove the effects, participants should also have responded to feedback about other film attributes (e.g., budget or release year), and we found no such pattern.

We further tested this explanation in two supplemental experiments (Studies S2A and S2B, detailed on pages 7-11 of the Appendix) using a 2x2 design that crossed the original gender-feedback manipulation with a values-based injunctive message emphasizing the importance of diverse selections. The message stated that the organization cared deeply about presenting diversity in terms of another speaker attribute and gender, paralleling real-world organizational communications that signal diversity is valued without directly instructing participants whom to select. Study S2A involved hypothetical selections of corporate speakers for a high-profile panel; Study S2B involved incentivized selections of podcasts hosted by prominent men and women for a Facebook advertisement. In both studies, descriptive feedback about the gender of past selectees again increased subsequent selection of women. By contrast, the injunctive message did not significantly increase the selection of women, and it did not significantly alter the effect of gender feedback: all main-effect p's > .15 and all interaction p's > .15 (see Appendix Tables S17-S22). That pattern is difficult to square with a demand-effect account. Together, these findings suggest that Study 3's effects are not the product of experimenter demand.

## Word count: 515 -> 301 (saved 214)

## Preservation notes

- Kept the demand-effect alternative-explanation framing, including Orne (1962) and Zizzo (2010), because this is the contribution-distinguishing claim for Studies 3A/3B plus S2A/S2B.
- Compressed the redundant recap of Studies 3A/3B findings while preserving the race/gender mapping and incentivized-decision context.
- Kept the S2A/S2B design, injunctive-message purpose, and task distinction; compressed the quoted message and removed the bracketed attribute list.
- Replaced the verbose S2A/S2B B/p enumeration with “all main-effect p's > .15 and all interaction p's > .15 (see Appendix Tables S17-S22)” per the study-specific rule.
- Departed from the Methods/Results template because this is a Discussion; Katy red-line items on demographics, balance checks, and verbose primary Wald F-tests were not applicable here.

---

# SOURCE (verbatim from live Gdoc, refreshed 2026-05-10; do not modify)

Discussion
Across two experiments involving incentivized decisions, we found that providing descriptive feedback about the race (Study 3A) or gender (Study 3B) of protagonists featured in past films selected for a marketing campaign significantly increased the likelihood that participants subsequently selected films featuring women or racial minorities, respectively. One possible alternative explanation for our findings is that providing participants with feedback about the race or gender of past selections signaled that we value racial or gender diversity. For example, when participants first selected films, they might have chosen based on personal preferences. But when they received descriptive feedback summarizing the attributes of their selected biopics, evaluators may have inferred that protagonist race (in Study 3A) or gender (in Study 3B) were of particular significance to the experimenter and attended to these attributes more in subsequent decisions as a result of a “demand effect” (Orne, 1962; Zizzo, 2010). If this alternative explanation were driving our effects, however, we would also expect participants to respond to the “demand effect” inherent in receiving feedback about other attributes of their selected films (e.g., feedback about a film’s budget or year of release), but we found no such patterns. 
However, to further rule out a “demand effect” explanation for our findings, we conducted two supplemental experiments (Studies S2A and S2B, detailed in pages 7-11 in the Appendix). In both studies, we employed a 2x2 design, crossing our original gender feedback manipulation with the addition of a values-based injunctive message (i.e., “The organization planning this event cares deeply about presenting a diversity of speakers in terms of [their experience in CEO roles, their experience as company founders, their background in the technology industry] and their gender.”) to the participant emphasizing the importance of making diverse selections. This message was designed to parallel real-world organizational communications, which typically signal that diversity is valued rather than issue direct commands about whom to select. In Study S2A, participants made hypothetical selections of corporate speakers to join a high-profile panel, while in Study S2B, participants made incentivized selections of podcasts hosted by prominent men and women to include in a Facebook advertisement. Across both studies, we replicated our finding from Study 3B: descriptive feedback about the gender of past selectees increased the likelihood that participants subsequently selected a woman. However, explicitly highlighting the importance of making diverse selections (via an explicit injunctive norm message) did not significantly increase the selection of women candidates (in S2A: the message produced a regression-estimated 3% increase in the selection of women panelists, B = 0.030, p = .471; in S2B: the message produced a regression-estimated 5% increase in the selection of podcasts with women hosts, B = 0.053, p = .176). Moreover, in neither study did the effect of the explicit injunctive norm message highlighting the importance of diverse selections significantly alter the effect of providing descriptive feedback about the gender of past selectees (Study S2A interaction: B = -0.005, p = .941; Study S2B interaction: B = 0.019, p = .748; see Appendix Tables S17-S22 for full results). Together, these findings suggest our results in Study 3 are not the product of an experimenter demand effect.
