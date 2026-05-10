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
