# Reconciled pre-coauthor audit — 2026-05-15

Two independent passes over fresh Gdoc pulls:
- Claude: `claude_final_audit.md`
- Codex (gpt-5.5, xhigh): `codex_final_audit.md`

Both agree on the four critical issues. Codex also caught several typos in tables and Results sections that I missed. Everything below is verified against the live manuscript text.

Manuscript and response letter line numbers refer to:
- `manuscript_gdoc_text.txt` (Gdoc `1H9jAvqG5CzQe…`)
- `response_letter_gdoc_text.txt` (Gdoc `1YBHCMwD84xC3Qw38CPEzz1iv3JKg7Mw9jTJqyZGptkI`)

---

## CRITICAL — must fix before coauthors see this

### C1. Doomed cross-study Wald "Table 2" is still in the manuscript
**Manuscript lines 385-406.** A second "Table 2" sits after Table 5 in the post-references area: *"Effects of Random Assignment to Receive Descriptive Feedback About the Identity-Target Composition…"*. This is the unifying summary you decided against, and it creates a Table-2 numbering conflict with the legitimate Study 1 Table 2 (lines 302-322).

**Fix:** Delete lines 385-406 entirely (including the section break and the "Study 1 is excluded…" notes line). The pending deletion `suggest.th0c0nlaxdr` on the notes is moot once the whole table is gone.

### C2. Zero-baseline pooled analysis: promised three times, present zero times
- Cover letter, **l. 16**: "We describe these new analyses in more detail below and in our revised manuscript on page X."
- DE response, **l. 43**: "We now both discuss it (see p. X) and a follow-up analysis…"
- DE response, **l. 44**: "We have added a footnote to the General Discussion (p. X)…points readers to Appendix Section S[X] for the full per-cell analysis."

Neither the General Discussion footnote nor the Appendix section exists in the manuscript. Codex confirms: zero hits in the manuscript for `22.50`, `7.25`, `N = 864`, `N = 414`, `+2.87`, `+27.63`, or "zero representation." Likely anchor for the footnote is **l. 168** in the General Discussion (the differential-effects sentence).

**Fix:** Either write the footnote + Appendix section, or strip the three promises from the response letter and rely on what the manuscript actually contains. The letter currently advertises content that doesn't exist.

### C3. DEI rebranding / sentiment narrative is promised but absent from manuscript
**Response letter lines 71-74** describes a substantial new manuscript paragraph covering:
- Bentley/Gallup 2025: 69% favorability
- Failed anti-DEI shareholder proposals at Costco, Apple, Levi's, John Deere, Goldman Sachs
- Corporate rebranding examples (Constellation, Kohl's, Nationwide, UPS)
- Gravity Research statistics (22% decline; 59% YoY increase in "belonging")

The manuscript has only the **international regulatory expansion** sentences (Ressia, Mignano, EU directives) plus the new "Although the corporate posture toward DEI has fluctuated…" insertion at **l. 15**. The Bentley/Gallup, Costco/Apple, rebranding, and Gravity Research content appears nowhere in the manuscript body.

**Consequence:** Five new references in the manuscript bibliography are orphaned (cited only in the letter):
- Bentley/Gallup 2025 (l. 196)
- CBS News 2025 (l. 202)
- Dungan 2025 / HRGrapevine (l. 218)
- Elias & Palmer 2025 / CNBC (l. 219)
- Marketplace 2025 (l. 257)

**Fix:** Either add the rebranding paragraph to the Introduction (probably extending the l. 15 insertion) or trim the response-letter description and remove the five orphan references.

### C4. Pending suggestions render duplicated text in inline view (cosmetic until accepted)
While suggestions are still pending, the live Gdoc shows new + old prose running together. After Wu accepts, they resolve. But coauthors reading the live Gdoc right now see:

- **l. 15** (Intro p. 15): the new "Although the corporate posture…" insertion immediately followed by the old "And despite recent rollbacks…" deletion. Two near-identical claims back-to-back.
- **l. 171** (General Discussion, Theoretical Contributions): "…how their selections will be judged by others.**the feedback itself elicits**…" — missing space + duplicated framing because `suggest.wk7zr07vhkc`'s deletion is still pending.
- **l. 133** (Study 4B Discussion): the new Kirgios sentence is followed by the deleted-but-still-visible "Finally, it ruled out the possibility…" sentence (`suggest.hheakjbtoec`).

**Fix:** Accept all 20 pending suggestions before sharing with coauthors (or warn them in the cover email that the doc shows tracked changes inline). My recommendation: accept them now — the suggestions are all your own edits and have already been reviewed.

---

## HIGH — table/figure inconsistencies Codex caught that I missed

### H1. Table 4 title has Studies labeled wrong for racial minorities
**Manuscript l. 342.** Title reads:
> "…or Racial Minorities (in Studies **3B and 4B**)"

Should be "Studies **3A and 4A**." The 3B/4A swap appears once more in the same sentence chain. Studies 3B and 4B are the GENDER studies; Studies 3A and 4A are the RACE studies.

### H2. Table 4 notes are internally inconsistent
**Manuscript l. 366.** Two problems:
1. Opening clause says "four ordinary least squares (OLS) regressions" but the table reports five models (Model 1–Model 5).
2. The first sentence pairs model numbers with outcomes incorrectly:
   - "featured a woman (Model 1)" ✓
   - "a woman protagonist (Model 2)" ✗ — Model 2 is Study 3A racial minority film
   - "racial minority protagonist (Model 3)" ✗ — Model 3 is Study 3B woman protagonist film
   - "woman author (Model 4)" ✗ — Model 4 is Study 4A racial minority author
   - "racial minority author (Model 5)" ✗ — Model 5 is Study 4B woman author

The detailed per-model descriptions later in the same paragraph have the correct pairings, so this is just the first summary sentence and the model count.

**Fix:** Rewrite the opening sentence of the notes to:
> "This table reports the results of five ordinary least squares (OLS) regressions that predict whether the final biography (Model 1, Study 2), film (Models 2-3, Studies 3A-3B), or book (Models 4-5, Studies 4A-4B) a participant selected featured a woman, a racial minority protagonist, a woman protagonist, a racial minority author, or a woman author, respectively."

---

## MEDIUM — copy-edit catches in Results / Procedure prose

### M1. Study 4B Results, l. 126
Two grammar issues in one sentence:
> "we ran **an OLS regressions**" → "we ran **an OLS regression**" (or "ran OLS regressions")
> "as the primary predictor **First**, we replicated…" → missing period: "as the primary predictor. First, we replicated…"

### M2. Study 4B Results, l. 130
Sentence ends with a trailing comma:
> "…**p = .387),**" → should be `p = .387).`

### M3. Study 5 Procedure, l. 148
Two formatting issues in adjacent sentences:
> "Following a similar procedure to Studies 2–4 participants imagined" → missing comma: "Studies 2–4**,** participants imagined"
> "global business landscape.We included" → missing space: "landscape.**We**" → "landscape. We"

### M4. Study 3B Results, l. 92
Unresolved placeholder still in body:
> "(p = 0.813; see **Appendix Table/Section X**)"

### M5. Study 1 Methods, l. 43
Unresolved placeholder in the new pretest insertion:
> "(full results in **Appendix Section S[X]**)"

---

## MEDIUM — response letter cleanups

### M6. Response letter line 17 typo
> "particularly in regard **tothe** current landscape" — missing space.

### M7. Study-list shorthand inconsistency
- Cover letter, **l. 16**: "Studies 2, 3, and 4B"
- DE response, **l. 44**: "Studies 2, 3A, 3B, and 4B"

Recommend aligning the cover letter shorthand to the detailed wording.

### M8. Response letter reference vs prose mismatch
- **Prose, l. 74**: "Gravity Research reports a 22% decline…"
- **Reference list, l. 102**: "Paradigm. (2025). [Fortune 100 DEI/diversity language analysis report]…"

Citation says Paradigm; prose attributes it to Gravity Research. Pick one. (If Gravity Research is the right org, the placeholder reference still needs filling.)

### M9. Response letter placeholders
- **l. 99**: "Marketplace. (2026). … [URL pending verification]"
- **l. 102**: "Paradigm. (2025). … [URL pending verification]"

Fill or remove before sending.

### M10. Andor / Chen author lists differ between docs
- Manuscript refs: Andor, M. A., Gerster, A., Peters, J., **& Schmidt, C. M.** (2020) (l. 189); Chen, X., Latham, G. P., Piccolo, R. F., **& Itzchakov, G.** (2020) (l. 205)
- Response letter refs: Andor, M. A., Gerster, A., & Peters, J. (2020) (l. 85); Chen, X., Latham, G. P., & Piccolo, R. F. (2020) (l. 88)

Verify which has the full author list and reconcile.

---

## LOW — known placeholders + style notes

### L1. Page-number placeholders
Expected, fill last:
- Response letter "[X] pages shorter" (l. 14)
- ~14 `(p. X)` / `(pp. X-X)` / `Page X` references throughout the letter

### L2. Study 4 intro line 99 implies pretests started at Study 4B
> "In Study 4B, we selected comparison attributes after pre-testing them for importance, and we ensured that all were rated at least as important as gender or race."

Now that Study 1 has its own pretest (where 2 of 3 attributes were rated at least as important as gender, but West Coast was rated significantly less important), this line reads as if pretesting started at Study 4B. Consider softening to "Study 4B took the additional step of selecting comparison attributes after pre-testing them…" or similar.

### L3. Partial fulfillment of "relegate regression specs to Appendix" promise
Response letter l. 39 promises "a dedicated section of the Appendix." What actually exists: five per-study Appendix tables (S3, S5, S7, S9) plus main-paper Tables 2-3, and Studies 1, 3B, 4A, 4B still print full F-stats in body. Either narrow the letter's promise ("we have moved the full per-study Wald specifications into the Appendix tables and pared back the body description") or do another tightening pass on Studies 1, 3B, 4A, 4B Results.

### L4. Style sweep passes on NEW insertions
- ✅ No em/en dashes in any of the 20 pending suggestions
- ✅ No "latent" anywhere
- ✅ No callbacks ("as discussed earlier", etc.) in new insertions
- ✅ "Implicit injunctive norm" terminology consistently used
- ✅ All modernized references (Locke & Latham 2019, Itzchakov & Latham 2018, Chen et al. 2020, Schultz et al. 2018, Devine & Ash 2022, Andor et al. 2020, Henry et al. 2019, Mertens & Schultz 2021) are present in body + refs
- ✅ Two-pathways framing present (l. 171-172)
- ✅ DEI-climate scope condition present (l. 176)
- ✅ Kirgios et al. cross-partisan sentence present (l. 133)
- ✅ Fairness-norms future direction present (l. 177)
- ✅ Study 1 pretest numbers match response letter exactly (l. 43 vs l. 41)
- ✅ International regulatory expansion present (l. 15)
- ✅ Expanded implicit-injunctive-norm theorizing present (l. 14-17)

---

## Recommended action sequence

1. **Accept all 20 pending suggestions** in the manuscript Gdoc (resolves C4 — eliminates the duplicated/leftover text at lines 15, 133, 171, plus the three reference-list deletions for Heaton/Kidwai/Minkin).
2. **Delete lines 385-406** of the manuscript (the doomed cross-study Table 2). After this, only one Table 2 exists.
3. **Decide on C2 (zero-baseline) and C3 (DEI rebranding):** either write the missing manuscript prose, or trim the response letter to match what's actually written. These two decisions also resolve the reference-orphan issue in C3.
4. **Apply table-notes fixes** (H1, H2 on Table 4).
5. **Apply Results/Procedure copy-edits** (M1-M5).
6. **Apply response-letter cleanups** (M6-M10).
7. Fill remaining page-number placeholders (L1).
8. Optional: tweak Study 4 intro l. 99 (L2) and reconsider the Appendix-relegation framing in the response letter (L3).

After steps 1-3, the manuscript should be ready for coauthor circulation.
