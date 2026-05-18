# Codex brief: R2 final audit (4-pass triangulation)

You are auditing the final R2 submission of "Does Feedback Enhance Diversity in Selection Decisions?" (Management Science, Wu DE). I (Claude) just finished a pass of edits and need a fresh, skeptical second-opinion across four dimensions. Be terse, concrete, and surface only real issues. Skip approvals — assume edits are fine unless you see a problem.

## Inputs you have

1. **Manuscript Gdoc** (live): `1H9jAvqG5CzQe_BksjkNmLwjLZSlAHvaEK1iyuHL23FI`
   - Cached text export: `C:/Users/jcerv/AppData/Local/Temp/manuscript_fresh.txt` (~147K chars, **fresh after my edits**)
2. **Response letter Gdoc** (live): `1YBHCMwD84xC3Qw38CPEzz1iv3JKg7Mw9jTJqyZGptkI`
   - Cached: `C:/Users/jcerv/AppData/Local/Temp/response_letter_fresh.txt`
3. **Open comments** (28 total, 14 from Katy):
   - `C:/Users/jcerv/AppData/Local/Temp/manuscript_comments_fresh.json` (pre-edit snapshot)
   - To re-fetch live: `python C:/Users/jcerv/Jose/diversity_feedback/revision-analysis/_r2_review/_list_manuscript_comments.py`
4. **New appendix Gdoc** (just created): `1svt9mnSQhI8ieCagC2ezGA_ni1SDpV-JTP-_698bUVg`
   - Source docx: `C:/Users/jcerv/AppData/Local/Temp/AppendixAdditions_R2.docx`
5. **NPR pretest canonical data + results**:
   - Raw: `C:/Users/jcerv/Jose/diversity_feedback/Survey1_NPR_AttributeImportance_canonical_SV_aeIi4xABcJiz1vU.csv`
   - Computed stats: `C:/Users/jcerv/Jose/diversity_feedback/Survey1_NPR_AttributeImportance_canonical_results.csv`
   - Descriptives: `C:/Users/jcerv/Jose/diversity_feedback/Survey1_NPR_AttributeImportance_canonical_descriptives.csv`
6. **Zero-baseline analysis outputs**: `C:/Users/jcerv/Jose/diversity_feedback/revision-analysis/_r1_review/de_zero_benchmark_results.csv`

## What I just did (so you can audit it)

1. **Pre-test stat reconciliation**: Fetched canonical Qualtrics data for `SV_aeIi4xABcJiz1vU` (N=306 raw → 298 after pre-registered attention-check), ran the 3 pre-registered Welch t-tests, and swapped the OLD values in both manuscript + response letter via Gdoc API. New canonical values:
   - women: M=4.28, SD=2.33, N=75
   - under-50: M=4.49, SD=2.17, N=75; t(147.2)=0.58, p=.563, d=0.09
   - West Coast: M=3.35, SD=2.17, N=74; t(146.5)=−2.51, p=.013, d=−0.41
   - university: M=5.04, SD=1.93, N=74; t(142.6)=2.17, p=.032, d=0.36
2. **Citation placeholder fills** (5 Katy comments, 7 substring swaps in manuscript):
   - C[a] line 13 internal motives: `Plant & Devine, 1998; Crandall & Eshleman, 2003`
   - C[a] line 13 external motives: `Plant & Devine, 2009; Lerner & Tetlock, 1999`
   - C[c] line 17 anti-disc law + DEI support: `(Bentley University & Gallup, 2025; Levi & Fried, 2024)`
   - C[d] line 193 org climate: `(Apfelbaum et al., 2008; Plant & Devine, 2009)`
   - C-187 GD internal motivation: `(Plant & Devine, 1998; Crandall & Eshleman, 2003)`
   - C-187 GD external motivation: `(Plant & Devine, 2009; Lerner & Tetlock, 1999)`
   - C-188 GD `(CITES!)` ebbing/flowing: `(Bentley University & Gallup, 2025; Levi & Fried, 2024)`
3. **Cross-references fixed**:
   - line 205 (Study 3B Results): `see Appendix Table/Section X` → `see Appendix Table S5`
   - line 255 (Study 4B): `see Appendix Section S4` → `see Appendix Study S3`
   - footnote 17: `See Appendix X` → `See Appendix Section B`
4. **Demographics consistency sweep** (6 strings standardized to women-first; semicolons):
   - Studies 2, 3A, 3B, 4B, + two other studies with non-binary
5. **Two new appendix sections drafted** in a separate Gdoc (Appendix Section A: NPR Pretest; Appendix Section B: Zero-Baseline). Tables A1 + B1 embedded inline.

## What I deliberately DID NOT do
- Did not auto-resolve any Katy comments (per JC: leave open for his review).
- Did not address Sophia's comments or any already-resolved Katy comments (out of scope).
- Did not adjust the C[b] "timelessness" paragraph at line 15 (needs JC editorial decision).
- Did not italicize condition names like "no gender feedback" (C-34) — range-targeted text-style work; flagged for JC manual pass.
- Did not merge the new appendix sections into `Appendix_Does FeedbackEnhanceDiversity.docx` (separate future task per JC).

## Four audit passes — produce concise, separate sections

### Pass 1 — NPR pretest stat reconciliation (verify only)
Re-run `r2_npr_pretest_analysis.py`-style logic against the canonical CSV. Confirm the 4 per-arm means/SDs and 3 t-test outputs I quoted above match the data, with the **pre-registered exclusion criteria** applied (Finished + IP Address + ec_2 attention-check pass + has condition assignment). Flag any divergence with magnitude.

### Pass 2 — Citation audit
For each of the 7 citation insertions, confirm:
- (a) the cite is plausible support for the claim it anchors;
- (b) the cite is already in the manuscript's References section (none should be net-new given my plan);
- (c) ordering and punctuation match other in-line citations in the manuscript.
Flag any cite Katy would push back on.

### Pass 3 — Parallel appendix-section review
Read the new Appendix Gdoc (`1svt9mnSQhI8ieCagC2ezGA_ni1SDpV-JTP-_698bUVg`) end-to-end. Verify:
- Section A: methods correctly describe what we did; results match my canonical stat numbers; Table A1 values match the canonical CSV; the directional narrative ("two of three rated as at least as important; one rated less important") is consistent with the numbers.
- Section B: methods specification (LPM, HC3, clustering, pooling) is internally consistent and matches what manuscript footnote 17 implies; pooled headline numbers (+22.50 pp; +27.59 women; +9.77 minority; +7.25 comparison; Wald Δ=+15.25 pp, p=.016) reproduce from `de_zero_benchmark_results.csv`; Table B1 per-cell numbers match.
Flag any internal inconsistency, voice clash with the existing appendix, or wrong table reference.

### Pass 4 — Edit-vs-comment diff verification
For each of the **14 Katy open comments** in `manuscript_comments_fresh.json`, classify my edits as: ADDRESSED | PARTIAL | NOT-ADDRESSED | DEFERRED. Produce a compact table. Be honest — if my edit doesn't fully answer Katy's ask, say so.

## Output

Write to `C:/Users/jcerv/Jose/diversity_feedback/revision-analysis/_r2_review/codex_r2_final_audit.md` with H2 sections matching the 4 passes. **Do not modify any other files.** Keep total under 1500 words; bullet/table format wherever possible.
