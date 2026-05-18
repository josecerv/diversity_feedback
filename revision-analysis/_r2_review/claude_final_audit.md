# Claude pre-coauthor audit: R2 response letter vs manuscript

Both files pulled fresh from Gdocs at 2026-05-15:
- Manuscript: `manuscript_gdoc_text.txt` (Gdoc `1H9jAvqG5CzQe...`)
- Response letter: `response_letter_gdoc_text.txt` (Gdoc `1YBHCMwD84xC3Qw38CPEzz1iv3JKg7Mw9jTJqyZGptkI`)
- Pending suggestions snapshot: `manuscript_pending_suggestions.md` (20 pending)

Line numbers below refer to those txt exports.

---

## 1. Critical — must fix before sending to coauthors

### 1A. Doomed cross-study "Table 2" is still in the manuscript
**Manuscript lines 385-406.** A second "Table 2" (with the title "Effects of Random Assignment to Receive Descriptive Feedback About the Identity-Target Composition of Initial Selections Versus Descriptive Feedback About Other Initial-Selection Attributes on Subsequent Selections, Across Studies") sits after Table 5 in the post-references appendix. This is the unifying Wald-test summary you said was decided against. It also creates a duplicate-numbering conflict with the legitimate Table 2 at lines 302-322 (the Study 1 OLS table).

**Fix:** Delete lines 385-406 in their entirety, including the section break at line 385 and the notes paragraph that lists Study 1 and Study 5 exclusions (the pending deletion `suggest.th0c0nlaxdr` on the notes is moot once the whole table is gone). After deletion, only one Table 2 remains (the Study 1 table).

### 1B. Zero-baseline pooled analysis is promised three times but missing from the manuscript
The response letter commits to it in three places:
- **Line 16** (cover letter): "We describe these new analyses in more detail below and in our revised manuscript on page X."
- **Line 43** (DE response): "We now both discuss it (see p. X) and a follow-up analysis…"
- **Line 44** (DE response, detailed): "We have added a footnote to the General Discussion (p. X), anchored on the sentence where we first state the differential effect of race or gender feedback versus comparison-attribute feedback. The footnote summarizes the cross-study rule-out and points readers to Appendix Section S[X] for the full per-cell analysis."

The specific numbers in the letter (22.50 pp, p < .001, N = 864 race/gender; 7.25 pp, p = .19, N = 414 other attributes; Wald p = .016, 95% CI [+2.87, +27.63]) appear nowhere in `manuscript_gdoc_text.txt`. No General Discussion footnote. No Appendix Section S[X] reference in body.

**Fix:** Either (a) write the General Discussion footnote and add the Appendix section, or (b) edit the response letter to remove these promises and rely on what's actually in the manuscript. The letter currently advertises content that doesn't exist.

### 1C. DEI rebranding/sentiment paragraph promised but missing from manuscript
**Response letter lines 71-74** describe a substantial new manuscript paragraph covering:
- Bentley/Gallup 2025 survey ("69% of U.S. adults believe businesses should promote DEI")
- Failed anti-DEI shareholder proposals at Costco/Apple/Levi's/John Deere/Goldman Sachs
- Corporate rebranding examples (Constellation, Kohl's, Nationwide, UPS)
- Gravity Research stats (22% decline in Fortune 100 "DEI" references; 59% YoY increase in "belonging")

None of this prose appears in the manuscript body. The manuscript only contains the **international regulatory expansion** sentences (Ressia 2024, Mignano 2024, EU directives) plus the new "Although the corporate posture toward DEI has fluctuated…" insertion at line 15. That insertion does NOT cite Bentley/Gallup, Costco, the rebranding examples, or the Gravity Research statistics.

**Fix:** Either add the prose to the Introduction (probably as a continuation of the new line-15 insertion) or trim the response-letter paragraph to match what the manuscript actually says.

---

## 2. Medium — orphan references and pending-suggestion state

### 2A. References cited in response letter but missing from manuscript body
After R1, the manuscript references list contains several new entries that the response letter explicitly cites but the manuscript prose never does:

| Reference (manuscript line) | Used in response letter? | Used in manuscript body? |
| --- | --- | --- |
| Bentley/Gallup 2025 (l. 196) | Yes (l. 74) | **No** |
| CBS News 2025 (l. 202) | Yes (l. 74) | **No** |
| Dungan 2025 (l. 218) | Yes (l. 74) | **No** |
| Elias & Palmer 2025 / CNBC (l. 219) | Yes (l. 74) | **No** |
| Marketplace 2025 (l. 257) | Yes (l. 74) | **No** |

These are orphans relative to the manuscript. APA convention says references should only list cited works. **Either** add the rebranding/sentiment prose (fixing 1C in the process) **or** remove these refs from the manuscript reference list.

### 2B. Three pending deletions that become orphans once accepted
- Heaton 2025 (manuscript line 233) — pending deletion `suggest.pb8xijyvy0xs`
- Kidwai 2025 (line 241) — pending deletion `suggest.44ipp4mnnp8i`
- Minkin 2024 (line 260) — pending deletion `suggest.w4qe8souamau`

These were cited in the old "And despite recent rollbacks…" sentence that suggestion `suggest.nkzob64uaq9w` replaces. Once Wu accepts, both the body cite and the reference entry need to drop. The pending deletions in the references list are queued correctly; just verify all three are accepted as a set (don't accept the body insertion without accepting the reference deletions).

### 2C. The duplicated General-Discussion sentence is fine as a pending suggestion, but reads broken until accepted
**Manuscript line 171.** With suggestions inline, the paragraph currently reads "…how their selections will be judged by others.**the feedback itself elicits reputational or self-image concerns**…" (the bolded portion is `suggest.wk7zr07vhkc`'s pending deletion that the new sentence replaces). This will resolve cleanly once Wu accepts the suggestion, but coauthors reading the live Gdoc will see both blocks running together. Same pattern at line 15 (Intro paragraph 15) with the "And despite recent rollbacks…" deletion overlapping with the new "Although the corporate posture…" insertion. Worth noting in the coauthor email so nobody panics.

---

## 3. Cross-document inconsistencies in the response letter

### 3A. Study-list shorthand is inconsistent
- Cover letter, **line 16**: "we conducted a new pooled analysis of Studies 2, 3, and 4B"
- DE response, **line 44**: "we pooled data from Studies 2, 3A, 3B, and 4B"

Both refer to the same analysis. The cover-letter shorthand "Studies 2, 3, and 4B" is loose (3A vs 3B?) and inconsistent with the detailed treatment. Recommend aligning the cover-letter sentence to "Studies 2, 3A, 3B, and 4B."

### 3B. Typo in response letter line 17
"…particularly in regard **tothe** current landscape…" — missing space.

### 3C. Response-letter references with [URL pending verification]
- **Line 99**: Marketplace. (2026). … [URL pending verification]
- **Line 102**: Paradigm. (2025). … [URL pending verification]

Fill these in or replace before sending.

### 3D. Per-study Wald F-stats are still in body for Studies 1, 3B, 4A, 4B
Response letter line 39 promises: "we have relegated the reporting of the regression specifications behind the secondary feedback-about-race/gender-versus-other-attribute comparisons to a dedicated section of the Appendix, with explicit references from the later Results sections."

What the manuscript actually does:
- Study 1 (l. 49): full Zellner-system spec + F-stats remain in body, referencing legitimate Table 2 Models 5-8 (in main paper).
- Study 2 (l. 68): brief mention only, references Table 3 Models 2-4 (in main paper). ✓ tightened.
- Study 3A (l. 84): brief mention, "see Table S3 Models 5-8" in Appendix. ✓ relegated.
- Study 3B (l. 92): full F-stats in body, "see Table S5 Models 5-8" in Appendix.
- Study 4A (l. 111): full F-stats in body, "see Table S7 Models 5-8" in Appendix.
- Study 4B (l. 127): full F-stats in body, "see Table S9 Models 5-8" in Appendix.

The "dedicated section of the Appendix" doesn't exist as a single consolidated section — instead there are five per-study Appendix tables. And Study 1's body still carries the full Zellner specification (~6 sentences) plus all three F-stats. Either the letter overstates the relegation, or Studies 1/3B/4A/4B Results sections need further trimming.

---

## 4. Placeholders that still need filling before submission

These are expected (you fill page numbers last), but listing for completeness:

**Manuscript:**
- Line 43 (Study 1 pretest): "Appendix Section S[X]"
- Line 92 (Study 3B): "see Appendix Table/Section X"

**Response letter:**
- Cover letter "[X] pages shorter" (line 14)
- ~14 instances of "(p. X)" / "(pp. X-X)" / "Page X" / "Pages X-X" scattered through the letter

**Manuscript study list issue I forgot above:**
- Manuscript line 99 (Study 4 intro) reads: "In Study 4B, we selected comparison attributes after pre-testing them for importance, and we ensured that all were rated at least as important as gender or race." This was accurate before R2, but now Study 1 also has a pretest of comparison-attribute importance, in which 2 of 3 attributes were rated at least as important as gender (one — "West Coast" — was rated significantly less important). Either soften line 99 ("In Study 4B specifically, we ensured that all comparison attributes were rated at least as important…") or note in line 99 that Study 1 also has a pretest now. Currently the line implies importance-pretesting started at Study 4B, which is no longer the timeline.

---

## 5. Style / convention sweep on NEW R2 insertions

Cross-checked every NEW insertion against the no-em-dashes / no-callbacks / use-implicit-not-latent rules:

- ✅ **No em/en dashes** in any of the 20 pending insertions.
- ✅ **No "latent"** anywhere in either document.
- ✅ **No manuscript callbacks** ("as we discussed earlier", "as noted in the Introduction", etc.) in the new insertions. Wording stays local.
- ✅ **"Implicit injunctive norm" language** consistently used.

Existing manuscript prose still has em dashes (e.g., line 8 "however —the type NPR…—", line 16, line 17), but those predate R2.

---

## 6. Promise-by-promise verification table (response-letter → manuscript)

| Letter | Promise | Manuscript location | Status |
|---|---|---|---|
| Cover l.14 | Manuscript [X] pages shorter; Methods/Results tightened | n/a (length claim) | Placeholder |
| Cover l.15 | New N=300 pre-registered Study 1 pretest | l. 43 (Study 1 Procedure) | ✅ Present, numbers match |
| Cover l.16 | New pooled zero-baseline analysis of Studies 2/3/4B; describe in detail at "page X" | nowhere | ❌ **Missing — see 1B** |
| Cover l.17 | Intro + Discussion updated re: DEI landscape | l. 15 intro insertion + l. 172/176 discussion | ⚠️ Partial — rebranding/sentiment paragraph missing (see 1C) |
| Cover l.18 | Modernized references in Intro + General Discussion | l. 10, 11, 15, 170 | ✅ All 8 new cites present in body + refs |
| DE l.34 | Reviewer 1 updates; expanded intro theorizing pp. X-X | l. 14-17 | ✅ Theorizing present |
| DE l.36 | Reviewer 3 (=Reviewer 2) Intro pp. X + Discussion pp. X — "timestamped" trends avoided | l. 15 + l. 172, 176 | ⚠️ Partial (see 1C) |
| DE l.37 | Scope-conditions narrative; future DEI-climate research direction | l. 176 (DEI climate scope condition insertion) | ✅ Present |
| DE l.39 | Regression specs behind secondary comparisons relegated to dedicated Appendix section | Appendix tables S3/S5/S7/S9 + main-paper Tables 2-3 | ⚠️ Partial (see 3D) |
| DE l.41 | New Study 1 pretest in Methods (p. X) | l. 43 | ✅ Numbers match letter exactly |
| DE l.43 | "We now both discuss it (see p. X) and a follow-up analysis" | nowhere | ❌ **Missing — see 1B** |
| DE l.44 | Footnote in General Discussion + Appendix Section S[X] for zero-baseline cross-study rule-out | nowhere | ❌ **Missing — see 1B** |
| R1 l.59 | Modernized Locke & Latham 2019, Schultz, Nolan, Cialdini 2018 alongside classics | l. 10, 11, 170 | ✅ Present |
| R1 l.61 | Refresh feedback + goals citations | l. 10, 11 | ✅ Present (Andor, Henry, Mertens, Schultz, Itzchakov, Chen, Devine & Ash all cited) |
| R1 l.62 | Expanded theorizing on entrenched injunctive norm; new cites Plant & Devine 1998; Crandall & Eshleman 2003; Álvarez-Benjumea 2023 | l. 14-17 (esp. 15-17), all 3 cites present | ✅ Present |
| R2 l.71 | Intro (pp. X) + Discussion updated to avoid "timestamped" feel | l. 15 + l. 172, 176 | ⚠️ Partial (1C) |
| R2 l.72 | International regulatory expansion: Ressia 2024, Mignano 2024, EU directive 2022/2381 | l. 15 (new insertion) | ✅ Present |
| R2 l.74 | Bentley/Gallup 2025, anti-DEI shareholder proposals failing, rebranding examples, Gravity Research stats | nowhere | ❌ **Missing — see 1C** |
| R2 l.77 | Kirgios et al. 2022 cross-partisan converging evidence in Study 4B Discussion | l. 133 | ✅ Present |
| R2 l.78-80 | General Discussion sharpens scope conditions; two pathways (internal/external) | l. 171-172 + 176 (DEI climate scope) | ✅ Present |
| R2 l.80 | Fairness norms future-direction sentence | l. 177 (end) | ✅ Present (`suggest.sp7gnhhipi1d`) |

---

## Summary

Three issues are critical: the doomed cross-study Wald Table 2 at lines 385-406 needs to be deleted; the zero-baseline pooled analysis promised in three places in the response letter has no home in the manuscript; and the response letter advertises a rebranding/sentiment narrative (Bentley/Gallup, Costco/Apple, rebranding examples, Gravity Research) that doesn't exist in the manuscript prose, leaving five orphan references in the manuscript bibliography.

Everything else is style and placeholder cleanup — the modernized references, expanded theorizing, two-pathways framing, Kirgios insertion, DEI-climate scope condition, fairness-norms future direction, and Study 1 pretest are all present and match the letter's specifics.

Recommend resolving #1A (delete doomed Table 2), then deciding #1B and #1C jointly: either write the missing manuscript prose (~one new Intro paragraph + a General Discussion footnote + Appendix Section), or trim the response letter to match what's actually been written.
