# Codex Brief: Relocate the zero-baseline pooled analysis (round 2)

## Background

The previous Codex pass and Claude pass both recommended putting the zero-baseline pooled analysis in the Study 5 preamble immediately after the mini-meta. That edit landed. JC now reviewed the doc and is pushing back: it doesn't motivate well there, because the Study 5 preamble's narrative arc is "does the effect size depend on baseline proportion → so we manipulate baseline experimentally in Study 5." The mini-meta tests *that* (continuous baseline × feedback). The zero-baseline analysis tests something different: *whether the race/gender vs. comparison-attribute differential is real, or whether comparison attributes are just used appropriately at baseline*. JC says: "The motivation of this test was whether people are selecting people at the right portfolio."

JC wants you to read the full manuscript in context and re-evaluate the placement. He suspects the current location is wrong but doesn't have a fix in mind.

## What the analysis actually does

DE Wu's R1 request had two prongs:
1. **Importance**: Are comparison attributes seen as equally important as gender/race? *(He singled out Study 1's NPR comparison attributes: "under 50 years old," "based on the West Coast," "worked at a university" — his intuition was these would seem less important than gender.)*
2. **Appropriate initial use**: "I'm still left wondering whether your findings are showing what you say or it is just that people use these other measures appropriately in their initial selections."

Our manuscript engages these two prongs in two different ways:
- **Importance rule-out**: handled by importance pretests. Study 1 now has a new N=300 pretest (recently inserted). Study 4B already has an N=550 importance pretest (manuscript line 115, Appendix Section S4). Studies 3A/3B do NOT have explicit pretests.
- **Appropriate-initial-use rule-out**: the zero-baseline pooled analysis. Among participants whose initial selections contained zero of an attribute (definitely *not* using it "appropriately" initially), race/gender feedback shifts +22.50 percentage points (*p* < .001, *N* = 864), comparison-attribute feedback shifts only +7.25 percentage points (*p* = .19, *N* = 414), Wald difference *p* = .016. Pools Studies 2, 3A, 3B, 4B (4A excluded because no one began with zero of any comparison attribute).

These two rule-outs are conceptually paired. Both target the same skepticism about the differential-effect claim.

## Critical manuscript context

**Study 4 preamble (manuscript line 99) — the most important sentence in this whole analysis:**
> "We also attempted to rule out the possibility that the 'comparison' attributes summarized in descriptive feedback (alongside feedback about the race or gender of past selectees) in our prior studies may have been perceived as less important to participants than the race and gender of candidates. In Study 4B, we selected comparison attributes after pre-testing them for importance, and we ensured that all were rated at least as important as gender or race. See Appendix Study S4 for a complete description of our pre-testing procedure and results."

This is where Study 4 explicitly addresses the comparison-attribute robustness concern. It currently only addresses the importance prong. It does NOT address the appropriate-initial-use prong.

**Study 4B preamble (line 115)** describes the importance pretest in more detail.

**Table 2** is the consolidated cross-study attribute-comparison table, first referenced from Study 1 Results (manuscript line 49). It contains the F-test models showing race/gender feedback effects > comparison-attribute feedback effects.

**Per the response letter (JC's revised wording)**, the regression specifications behind the cross-study attribute comparisons are now relegated to a dedicated Appendix section, with references from later Results sections.

## Where the zero-baseline analysis currently lives (state to undo)

In the Study 5 preamble, as a new paragraph immediately after the mini-meta results paragraph and before the bridge sentence "However, because the proportion of women or racial minorities in participants' initial selections was not randomly assigned…". The mini-meta tests continuous baseline×feedback for the Study 5 setup; the zero-baseline analysis sits awkwardly in between because it's about a different question.

## Constraints (non-negotiable)

- No em or en dashes.
- Italic *p* and *N* in stats.
- "implicit injunctive norm" terminology; never "latent."
- Curly quotes only.
- Timeless DEI framing (no date-stamping).
- Use JC's voice: direct, "we argue/agree/expect," "for instance," "however," "by contrast." Avoid "marketing posture," "underneath that rhetoric," "robustness to its swings."

## Candidate placements to evaluate

**A. Extend the Study 4 preamble.** Add a sentence (or sentences) to the existing rule-out paragraph (line 99) that introduces the second rule-out: an appropriate-initial-use analysis pooling across studies, with full results in Appendix Section S[X]. This keeps the importance and appropriateness rule-outs as siblings in one paragraph, exactly where the manuscript already engages the comparison-attribute robustness concern. Could be one main-text sentence plus full reporting in Appendix.

**B. Appendix-only, with a brief footnote at Study 4 preamble line 99 (or at a cross-study F-test reference in Table 2 region).** Pointer says something like: "We also test whether the differential effect persists among participants whose initial portfolios contained zero of the comparison attribute; see Appendix Section S[X]." Cleanest main text; full analysis in Appendix.

**C. Inside the General Discussion's Theoretical Contributions paragraph.** Add a sentence supporting the norm-activation claim: "The differential responsiveness to race/gender vs. comparison-attribute feedback persists even when initial portfolios contain zero of the focal attribute, ruling out the alternative that comparison attributes are simply selected appropriately at baseline (see Appendix Section S[X])." Anchored to the theoretical claim.

**D. Inside the General Discussion's Future Directions / Limitations.** Frame as a rule-out of an alternative explanation. Weaker because it pushes the answer to the end of the paper.

**E. Keep at Study 5 preamble (current state) but reframe so it doesn't sit between mini-meta and Study 5 bridge.** This is the option to argue against — JC has rejected it.

**F. Footnote inserted at the F-test paragraph of Study 4B Results** (closest spot in main text to where the race/gender vs. comparison-attribute differential is first formally tested via F-test). Footnote text refers reader to Appendix Section S[X]. Local, surgical, low main-text cost.

**G. New dedicated subsection titled "Ruling Out Alternative Explanations" or similar**, placed between Study 5 preamble's bridge and Study 5 Methods, OR between Study 4 Discussion and Study 5 preamble. Riskier — adds a new heading.

## Your task

Use `gpt-5.5` with `model_reasoning_effort='xhigh'`.

1. Read the full manuscript text dump at `revision-analysis/_r2_review/manuscript_gdoc_text.txt` (420 lines).
2. Read the response letter dump at `revision-analysis/_r2_review/response_letter_gdoc_text.txt` (if present; otherwise `revision-analysis/_r1_review/response_letter.txt` lines 14–60).
3. **Evaluate each placement option A through G** with strengths and weaknesses, considering:
   - Where the DE's two-pronged R1 concern is engaged in the manuscript.
   - Where the cross-study attribute differential is formally established.
   - Where readers would *expect* a rule-out of the appropriate-initial-use alternative to live.
   - Page-budget pressure (JC has been streamlining; less main text is better unless conceptually necessary).
   - Coherence with the importance-pretest rule-outs (Study 1, Study 4B) which are conceptual siblings.
4. **Recommend ONE** option with full reasoning.
5. **Provide exact prose** for:
   - The main-text insertion at the recommended location (if any), in JC's voice.
   - The undo plan for the current Study 5 preamble insertion (which paragraph to remove).
   - An updated response-letter pointer sentence reflecting the new placement (current pointer says "we have added a short paragraph to the preamble of Study 5"; this needs to point to wherever you recommend instead).

## Invocation

```
rm -f "revision-analysis/_r2_review/codex_zero_baseline_relocation.md"
codex exec --model gpt-5.5 -c model_reasoning_effort='"xhigh"' \
  --skip-git-repo-check --dangerously-bypass-approvals-and-sandbox \
  -C "C:\Users\jcerv\Jose\diversity_feedback" \
  "[your prompt that references this brief]" 2>&1 \
  | tee "revision-analysis/_r2_review/codex_zero_baseline_relocation.session.log"
```

Hard timeout: 25 minutes.

## Required output structure

Write to `revision-analysis/_r2_review/codex_zero_baseline_relocation.md`:

```
# Codex Recommendation: Zero-Baseline Relocation (round 2)
## Option evaluation (A–G)
[each with strengths/weaknesses]
## Recommendation
[chosen option + reasoning]
## Exact prose
### Main-text insertion at the recommended location
### Undo plan for current Study 5 preamble paragraph
### Updated response-letter pointer
## Constraints verification
[em/en dashes: 0; "latent": 0; etc.]
```

## Post-run validation (report back to me)

- File exists
- All five required sections present
- Em/en dash hits (line numbers)
- "latent" hits
- Recommended option (A–G)
- Two-sentence summary of reasoning
