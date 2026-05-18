# Codex Brief: Where should the zero-baseline pooled analysis live in the manuscript?

## Stakes

JC is reading the manuscript and the current Study 4 Discussion paragraph 2 reads as "doing too much." He thinks we may have inserted the zero-baseline pooled analysis "out of convenience" rather than where it's theoretically motivated. Co-authors will see this. We need a defensible placement recommendation.

**Constraints (non-negotiable):**
- DEI/backlash content must be **timeless**, not date-stamped to 2024.
- No em/en dashes. Use commas, parens, semicolons, or split sentences.
- Italicize *p* in statistics; use "implicit injunctive norm" (not "latent").
- Curly quotes (U+2019/U+201C/U+201D), not straight.
- You are working in `gpt-5.5-pro` with **high reasoning effort**. If you find yourself using `gpt-5.4` or anything else, stop and switch.

## The analysis in question (zero-baseline pooled)

DE Wu raised an alternative explanation in R1: maybe people just use the other (comparison) attributes appropriately in their initial selections, and the race/gender feedback effects are nothing special — people just bump the attribute the experimenter highlighted.

Our R1 response: among the subset of participants whose initial portfolios contained ZERO of a given attribute (a clear underrepresentation signal), does race/gender feedback move people more than comparison-attribute feedback? Yes:

- Race/gender feedback: +22.50 pp (*p* < .001, N = 864) when baseline = 0
- Comparison-attribute feedback: +7.25 pp (*p* = .19, N = 414) when baseline = 0
- Wald difference: *p* = .016
- Studies pooled: 2, 3A, 3B, 4B. Study 4A excluded because no participant began with zero of any comparison attribute.
- Full reporting in Appendix Section S[X] (not yet built).

## Where it currently lives in the manuscript

End of Study 4 Discussion paragraph 2 (manuscript paragraph 133). Full current text:

> In addition, Study 4B established that the effect of gender feedback was not moderated by political ideology nor party affiliation. Although we are cautious about over-interpreting a null interaction, this pattern is suggestive that descriptive feedback operates across partisan lines, an interpretation that converges with field-experiment evidence that women and racial minorities benefited from explicitly naming their demographic identity when contacting Republican and Democratic city councilors alike, an effect mediated by motivation to respond without prejudice (Kirgios et al., 2022). **Building on this, we conducted a pooled analysis across Studies 2, 3A, 3B, and 4B (excluding Study 4A, where no participant began with zero of any comparison attribute), restricting our sample to participants whose initial selections contained zero candidates with a given attribute. Selecting zero of an attribute is a clear signal of underrepresentation that should provoke a response to feedback regardless of which attribute is summarized. Race and gender feedback raised the probability of selecting a racial minority or woman, respectively, by 22.50 percentage points (*p* < .001, N = 864), whereas feedback about other attributes did not significantly shift subsequent selections among participants whose initial portfolios contained zero of that attribute (effect = +7.25 percentage points, *p* = .19, N = 414). A Wald test confirms the difference between the two pooled coefficients is significant (*p* = .016). Full results appear in Appendix Section S[X].** Finally, it(Kirgios et al., 2022). Finally, it ruled out the possibility that our effects were driven by perceptions of which attributes were considered most important.

**Problems with this placement:**
1. "Building on this" doesn't follow from the Kirgios convergence point — the zero-baseline analysis is unrelated to political moderation.
2. The analysis is across Studies 2, 3A, 3B, 4B (not Study 4A), so it doesn't conceptually belong inside Study 4's Discussion.
3. There is an orphaned broken fragment ("Finally, it(Kirgios et al., 2022). Finally, it ruled out...") from a prior edit. This must be cleaned up regardless of placement.
4. Para 132 already establishes the Study 4–specific mediation findings. Para 133 should be about Study 4–specific moderation findings (political ideology). Cross-study analyses don't fit.

## The candidate alternative home: Study 5 preamble

Study 5's preamble (paragraphs 134–140) ALREADY contains a cross-study re-analysis that asks the same conceptual question. Current Study 5 preamble structure:

**Paragraph 134** (motivation for Study 5):
> "In all of our studies so far, we examined situations where women or racial minorities were underrepresented in candidate pools, which means that mechanically they would also likely be underrepresented in many respondents' initial set of selectees. As a result, the descriptive feedback we delivered typically highlighted that historically underrepresented groups had been picked at low rates… In Study 5, we tested how descriptive feedback affects decision makers when it suggests that historically underrepresented groups have recently been picked at high rates. This would not, in theory, activate a motivation to respond without prejudice, since the initial feedback would indicate that historically underrepresented groups were being well-represented."

**Paragraph 136** (the mini-meta setup):
> "Before testing this prediction experimentally, we examined whether the size of the effect we study differs based on the proportion of women and racial minorities initially selected by re-analyzing data from Studies 2, 3A, 3B, 4A, and 4B. Specifically, we tested for an interaction between (1) the proportion of women or racial minorities a participant initially selected and (2) random assignment to receive descriptive feedback. Based on our theorizing that feedback is unlikely to evoke a motivation to respond without prejudice when initial selections of women or racial minorities are high, we would predict a negative interaction between these predictors."

**Paragraph 140** (the mini-meta results):
> Reports five regression models for Studies 2, 3A, 3B, 4A, 4B with the proportion × feedback interaction. Mini-meta pooled coefficient: B = -0.052, 95% CI = [-0.075, -0.028], *z* = -4.36, *p* < .001. "This suggests that the effect of descriptive feedback on female and racial minority selections is reliably larger when participants have selected a lower initial proportion of women or racial minorities."

**Why this is the natural home:**
- Same theoretical claim being tested: feedback works through *perceived underrepresentation*, not through a generic "fill the gap" response.
- Same studies pooled (2, 3A, 3B, 4A, 4B).
- Same "we re-analyzed existing data before running the experiment" structural move.
- The mini-meta tests the **continuous** form of the question (proportion × feedback). The zero-baseline analysis tests the **discrete extreme** (baseline = 0) AND adds the **comparison-attribute control** (race/gender feedback vs. other-attribute feedback under the same zero baseline). These are complementary, not redundant.
- Justifies the analysis instead of feeling like it dropped from the sky.
- Naturally bridges into Study 5 (which manipulates baseline experimentally).

## Response letter (currently)

The response letter says (paragraph 34):
> "We have also added a footnote to the Discussion of Study 4B (p. X) that summarizes this evidence and makes the underlying theoretical claim more explicit in the manuscript."

This is currently inaccurate (we made it main text, not a footnote, in Study 4 Discussion). If we move it to Study 5 preamble, the response letter needs updating to match. The DE's underlying question can still be addressed via the analysis — we just point them to where it now lives.

## Your task

Use **`gpt-5.5-pro`** with **high reasoning effort**.

1. **Read the manuscript text dump** at `revision-analysis/_r2_review/manuscript_gdoc_text.txt`. Lines 131–141 hold Study 4 Discussion through Study 5 preamble. Lines 17–60 of `revision-analysis/_r1_review/response_letter.txt` hold the DE's request and our R1 response (Table R1).

2. **Stress-test the placement options.** Generate at least four candidate placements and argue against each:
   - (A) Keep at end of Study 4 Discussion but split into its own paragraph, clean up the broken fragment.
   - (B) Move to Study 5 preamble as a follow-up analysis to the mini-meta.
   - (C) Move to Study 4B preamble (before Study 4B results) framed as a check on the alternative explanation before presenting Study 4B's primary results.
   - (D) Move into the Appendix only, with a brief mention in the manuscript (one sentence at Study 5 preamble pointing to the appendix).
   - (E) Any other placement you think dominates.

   Apply the lens: where is the analysis most clearly motivated theoretically, where does it fit the surrounding prose flow, and where does it best serve the DE's R1 question without feeling like a convenience tack-on?

3. **Recommend ONE option with full justification.**

4. **Provide exact prose** for:
   - Cleaned Study 4 Discussion paragraph 2 (must remove the orphaned broken fragment regardless of placement decision).
   - Wherever the zero-baseline analysis ends up (with surrounding bridge sentences so it flows).
   - Updated response letter paragraph 34 to reflect the new manuscript location.

5. **Flag a separate consideration JC raised:** Study 4B uses different stimuli (book-author attributes) than Study 1. Should Study 4B's preamble add a brief callback to the Study 1 importance pretest logic, acknowledging that Study 4B's comparison attributes (multi-genre work, large book sales, durable canonical presence) are selection-relevant by design but were not separately pretested for importance? Or is the F-test comparison already reported in Study 4B's Results sufficient? Make a recommendation.

## Output format

Write to `revision-analysis/_r2_review/codex_zero_baseline_placement.md`.

Structure:
```
# Codex Recommendation: Zero-Baseline Placement
## Option evaluation
[A through E, each with argued strengths/weaknesses]
## Recommendation
[which option, why it dominates]
## Exact prose
### Cleaned Study 4 Discussion paragraph 2
[verbatim]
### Insertion at chosen location
[verbatim, with surrounding bridge sentences if needed]
### Updated response letter paragraph 34
[verbatim]
## Study 4B importance-callback question
[recommendation + rationale]
```

## Safeguards (per JC's standing instruction)

- Pre-delete the deliverable file if it exists from a prior run.
- Tee your session log to `revision-analysis/_r2_review/codex_zero_baseline_placement.session.log`.
- Hard timeout: 25 minutes max.
- Post-run validation: confirm the deliverable file exists, contains all five required sections, and does not contain em dashes (`—` or `–`) or the literal word "latent."
