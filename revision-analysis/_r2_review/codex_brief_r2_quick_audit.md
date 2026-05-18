# Codex brief: quick 2-pass audit (sub-10 min)

I (Claude) just landed a batch of edits on the manuscript Gdoc and response letter for the R2 submission of "Does Feedback Enhance Diversity in Selection Decisions?" (Management Science, Wu DE). Two high-stakes things need an independent recheck. Be terse. Just bullet/table format. Total output <600 words.

## Pass A — NPR pretest stat reconciliation (CRITICAL)

I swapped pretest stats in the manuscript + response letter to the values below, computed from the canonical Qualtrics export. Confirm they match the data with the pre-registered exclusion criteria applied (Finished + Status==IP Address + ec_2 attention-check pass == "one one" after stripping outer quotes + has attribute assignment).

**Data**: `C:/Users/jcerv/Jose/diversity_feedback/Survey1_NPR_AttributeImportance_canonical_SV_aeIi4xABcJiz1vU.csv`

**Values I quoted**:
- women: N=75, M=4.28, SD=2.33
- under-50: N=75, M=4.49, SD=2.17; vs women t(147.2)=+0.58, p=.563, d=+0.09
- West Coast: N=74, M=3.35, SD=2.17; vs women t(146.5)=-2.51, p=.013, d=-0.41
- university: N=74, M=5.04, SD=1.93; vs women t(142.6)=+2.17, p=.032, d=+0.36

Quick inline Python (Welch's t with two-sided p, Cohen's d pooled (n1+n2-2 denominator)) is sufficient. Don't write a full re-analysis script — just verify and report a diff table if numbers don't match.

## Pass B — Edit-vs-comment diff verification

For each of the **14 Katy open comments** in `C:/Users/jcerv/AppData/Local/Temp/manuscript_comments_fresh.json` (filter author==Katherine Milkman, resolved!=true), check whether my edits actually address her ask. My summary of what I did:

1. **C[a] line 13 — "internal/external motives CITES"** → filled with `Plant & Devine, 1998; Crandall & Eshleman, 2003` (internal) and `Plant & Devine, 2009; Lerner & Tetlock, 1999` (external)
2. **C[c] line 17 — "Anti-discrimination law... 60-70% DEI support"** → added `(Bentley University & Gallup, 2025; Levi & Fried, 2024)` at end of paragraph
3. **C[d] line 193 — "org climate paragraph citations"** → added `(Apfelbaum et al., 2008; Plant & Devine, 2009)`
4. **C[b] line 15 — "adjust timelessness paragraph"** → DEFERRED (needs editorial paragraph adjustment, not just a cite)
5. **C-188 GD `(CITES!)`** → filled with `(Bentley University & Gallup, 2025; Levi & Fried, 2024)`
6. **C-187 GD two `(CITE)` markers** → filled with `(Plant & Devine, 1998; Crandall & Eshleman, 2003)` internal and `(Plant & Devine, 2009; Lerner & Tetlock, 1999)` external
7. **C-9, C-30 demographics consistency** → swept 6 demographic strings to women-first; semicolon-separated
8. **C-10 payment + duration** → verified all 7 Participants blocks state both; no edit needed
9. **C-11, C-17 "Appendix Table/Section X"** → fixed line 205 to `see Appendix Table S5` (Study 3B); footnote 17 to `See Appendix Section B`
10. **C-29 IRB waiver wording** → already in place; no edit needed
11. **C-31, C-32, C-33 Study 1/2 prose** → already restored in current text; no edit needed
12. **C-2 clustering of SE explanation** → will live in new Appendix Section B (drafted in separate Gdoc `1svt9mnSQhI8ieCagC2ezGA_ni1SDpV-JTP-_698bUVg`)
13. **C-34 italicize "no gender feedback"** → DEFERRED for JC manual pass (range-targeted text-style work)

Confirm by reading current manuscript text: `C:/Users/jcerv/AppData/Local/Temp/manuscript_fresh.txt` (147K chars, includes all my edits).

For each Katy open comment, classify: **ADDRESSED | PARTIAL | DEFERRED | NOT-ADDRESSED**. One-line rationale. Table form.

## Output

Write to `C:/Users/jcerv/Jose/diversity_feedback/revision-analysis/_r2_review/codex_r2_final_audit.md` with H2 sections matching Pass A and Pass B. **Do NOT modify any other files.** Stop as soon as the audit is written.
