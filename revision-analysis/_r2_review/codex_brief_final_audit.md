# Codex brief: final pre-submission audit of R2 manuscript vs response letter

## Context

JC is preparing to send the R2 manuscript + response letter to coauthors for review. Before that, every promise in the response letter must be reflected in the manuscript, and the manuscript must be internally consistent (no leftover scaffolding from earlier drafts).

**Both files were just freshly pulled from Google Docs:**
- `revision-analysis/_r2_review/manuscript_gdoc_text.txt` — canonical manuscript (Gdoc `1H9jAvqG5CzQe_BksjkNmLwjLZSlAHvaEK1iyuHL23FI`)
- `revision-analysis/_r2_review/response_letter_gdoc_text.txt` — canonical response letter (Gdoc `1YBHCMwD84xC3Qw38CPEzz1iv3JKg7Mw9jTJqyZGptkI`)
- `revision-analysis/_r2_review/manuscript_pending_suggestions.md` — pending tracked-changes in the manuscript (Wu has not yet accepted; rendered inline in the .txt above)

You are the second pair of eyes. Claude is running its own pass in parallel; please be independent — don't summarize the brief back, find issues yourself.

## What I want you to do

Read BOTH files in full. Then for EVERY promise the response letter makes (whether in the cover-letter summary, DE response, R1 response, or R2 response), verify:

1. **Does the manuscript actually contain the corresponding edit?** Many promises reference "(p. X)" placeholders — that's fine, but the prose itself must exist somewhere identifiable.
2. **If the prose exists, does it read smoothly in context?** (Not just inserted as a block — does it transition cleanly in/out of adjacent sentences?)
3. **Are there leftover artifacts from earlier drafts** that should have been removed but weren't? JC specifically mentioned that an early draft had a unifying cross-study "Table 2" for the Wald tests that was decided AGAINST — verify whether such a table still exists and flag it as critical.

## Specific items to verify (non-exhaustive — find others too)

The response letter makes these concrete promises. For each, locate the corresponding manuscript prose and verify it matches:

1. **New Study 1 pretest** (response letter line 41): "incorporated the results into the Methods section of Study 1 (p. X)" — N=300, pre-reg URL `xa7u94`, all means/SDs/ts/dfs/ps/ds match between letter and manuscript Methods.
2. **Zero-baseline pooled analysis** (response letter line 16 cover, line 44 detailed): promises a **footnote in General Discussion** anchored on the differential-effects sentence, pointing to **Appendix Section S[X]** for the full per-cell zero-baseline analysis. The detailed numbers (22.50pp, p<.001, N=864; 7.25pp, p=.19, N=414; Wald p=.016, 95% CI [+2.87, +27.63]) are in the letter. Where are they in the manuscript?
3. **Implicit injunctive norm expansion** (line 34, 62): "we have expanded our Introduction's theorizing (pp. X-X) to clarify why descriptive feedback should shift selection decisions... we argue that there is a strong, implicit injunctive norm in this setting against prejudiced selection." New citations: Plant & Devine 1998; Crandall & Eshleman 2003; Álvarez-Benjumea 2023. Verify the introduction prose actually contains this expanded theorizing and the new cites.
4. **Modernized references** (line 18, 34, 59, 61): Locke & Latham 2019; Itzchakov & Latham 2018; Chen, Latham, & Piccolo 2020; Schultz, Nolan, & Cialdini 2018; Devine & Ash 2022; Andor, Gerster, & Peters 2020; Henry, Ferraro, & Kontoleon 2019; Mertens & Schultz 2021. Verify each is both (a) cited in the manuscript body and (b) present in the manuscript references list, with no orphans either direction.
5. **DEI landscape narrative** (line 17, 71-74): the response letter describes extensive manuscript prose about Bentley/Gallup 2025 (69% favorability), failed anti-DEI shareholder proposals at Costco/Apple/Levi's/John Deere/Goldman Sachs, corporate rebranding (Constellation, Kohl's, Nationwide, UPS), Gravity Research 22%/59% statistics. Find this prose in the manuscript — is it actually there, or just in the response letter?
6. **International regulatory expansion** (line 72): Australia Workplace Gender Equality Act 2023 (Ressia 2024); EU Pay Transparency Directive 2023/970 (Mignano 2024); EU Women on Boards Directive 2022/2381. Verify in manuscript body + references.
7. **Two-pathways (internal/external motivation) framing in General Discussion** (line 79): verify the framing now explicitly distinguishes internal vs external pathways, with the external pathway sustained by anti-discrimination law/visibility/reputation, and internal pathway by personal egalitarian self-image.
8. **DEI climate scope condition** in Future Directions (line 37, line 79): verify the new "Organizational DEI climate is a parallel scope condition..." prose is present and reads coherently.
9. **Kirgios et al. (2022) insertion in Study 4B Discussion** (line 77): verify the political-ideology null-interaction is now followed by the Kirgios et al. cross-partisan converging-evidence sentence.
10. **Fairness norms future-direction insertion** (Future Directions, end): verify the "Study 5 also suggests that fairness norms may not be confined to the overrepresentation case..." sentence is there.
11. **Appendix relegation of secondary-comparison regressions** (line 39): DE asked us to relegate the regression specifications behind the feedback-about-race/gender-versus-other-attribute comparisons to a dedicated Appendix section, with explicit references from the later Results sections. Verify (a) the relegation actually happened (Study 2-4B results sections should NOT repeat full Wald regression specifications in main text — they should reference an Appendix table) and (b) the Appendix references are present in the body.
12. **Page-shorter claim** (line 14): cover letter says manuscript is "[X] pages shorter" — placeholder, fine, but flag if you see length-claim text elsewhere that's inconsistent.

## Leftover-artifacts hunt (critical)

JC explicitly flagged this: an early draft had a unifying cross-study Wald-test table that was decided against. Verify:

- There must be NO table titled something like "Effects of Random Assignment to Receive Descriptive Feedback About the Identity-Target Composition... Across Studies" anywhere in the manuscript. If you find it, flag it as **CRITICAL — must be removed**.
- There must be NO numbering conflict. Confirm only one Table 2 exists (the legitimate Study 1 OLS table).
- Search for "identity-target", "Identity-Target", "per-cell", "Identity-Target Group" — these phrases appeared in the doomed unifying table. Any hits should be flagged.
- Search for table notes that read "Study 1 is excluded because..." / "Study 5 is excluded because its design varies..." — these were notes on the doomed table.

## Other things to check

- **Em dashes (—) and en dashes (–) in NEW R2 insertions only.** Existing manuscript prose has dashes that JC tolerates; we only care about NEW prose introduced for R2. Cross-reference `manuscript_pending_suggestions.md` to know which prose is new.
- **"latent" should never appear** — paper's theory uses "implicit injunctive norm".
- **Manuscript callbacks** ("as we discussed earlier", "as noted in the Introduction", "see above") — JC dislikes these; flag any in new insertions.
- **Orphan references**: any reference in the manuscript's reference list that is NEVER cited in the body? Or any in-body cite missing from the list?
- **Sample sizes / numbers**: any numeric claim in the response letter that doesn't match the manuscript text or appendix tables?
- **Voice / tone drift**: the new insertions should sound like the rest of the manuscript. Flag passages that read as if written by a different author.
- **Study list discrepancy**: the response letter cover (line 16) says "Studies 2, 3, and 4B" for the pooled zero-baseline analysis; the detailed response (line 44) says "Studies 2, 3A, 3B, and 4B." Same set? Worth a sentence.

## Output format

Write your report to `revision-analysis/_r2_review/codex_final_audit.md` with EXACTLY these section headers (the wrapper validates them):

```
## 1. Critical issues (must fix before sending to coauthors)
## 2. Promise-by-promise verification
## 3. Leftover-artifact findings
## 4. Reference-list audit
## 5. Style / voice drift in new prose
## 6. Other inconsistencies
## Summary
```

Under each section, cite line numbers from the relevant file (e.g., `manuscript_gdoc_text.txt:386`) and quote the offending text briefly. If a section has no issues, write `No issues found.` and move on. Don't pad. Severity calls (critical / medium / low) are welcome — JC will use them to triage.

**Do NOT edit any file.** This is a read-only audit. Findings only.
