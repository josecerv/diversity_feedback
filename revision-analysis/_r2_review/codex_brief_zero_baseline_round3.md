# Codex Brief: Zero-Baseline Placement (Round 3) — Full Manuscript Context

## Why we are here

We have iterated on placement of the zero-baseline pooled analysis three times and JC keeps pushing back. Each prior recommendation was inadequate for a specific reason, and we need to do this right.

**Round 1 (both Claude and Codex):** Study 5 preamble after the mini-meta. *Rejected because*: Study 5 preamble's narrative arc is "does effect depend on baseline proportion → motivates Study 5's experimental manipulation of baseline." The zero-baseline analysis answers a different question (whether the race/gender vs. comparison-attribute differential is real). Different questions forced into the same paragraph create the jarring effect JC felt.

**Round 2 (both Claude and Codex):** Study 4 preamble extension. *Rejected because*: Study 4 preamble would have to pool Studies 2, 3A, 3B, 4B (which includes 4B's experimental data) before Study 4B's results have been presented to the reader. Temporal violation.

**Round 2.5 (Claude):** Dedicated third paragraph at end of Study 4 Discussion. *Rejected because*: even after Study 4B's data is on the table, "could people just be using comparison attributes appropriately initially?" is not a question a reader is spontaneously asking. It feels artificially injected. The reader has to be *primed* with that alternative for the rule-out to register as motivated.

## What JC is now asking for

JC's exact words: *"I still don't feel like it is a natural question that someone would be asking after reading study four. I still think maybe there's a way we can include it in the preamble. Maybe initially I need you to look at it holistically again and ask Codex to read all of study four and five and then the response letter again and see where a better position is. Give Codex everything."*

JC's intuition points back to the preamble. Look for a way to make that work, possibly by separating the *framing* (which goes in main text preamble) from the *data* (which lives in the Appendix). The preamble of Study 4 is already where the manuscript engages the comparison-attribute robustness concern (line 99). If we can extend that framing without putting Study 4B experimental stats in the pre-Study-4B preamble, we solve both problems at once.

## Read this context dump first

Full Study 4 + Study 5 + response letter context is at:
`C:\Users\jcerv\Jose\diversity_feedback\revision-analysis\_r2_review\codex_full_context.txt`

You MUST read it in full before recommending placement. It includes the exact text of:
- Study 4 section (lines 96–133 of manuscript dump): preamble, Study 4A Methods/Results, Study 4B preamble (including the existing N=550 importance pretest reference), Study 4B Methods, Study 4B Results (including the F-test paragraph comparing gender feedback to each comparison attribute), Study 4 Discussion (currently 2 paragraphs after my edits: replication+mediation, political-ideology+Kirgios)
- Study 5 section (lines 134–160 of manuscript dump): preamble setup, mini-meta setup and results, the **current** zero-baseline paragraph (which JC wants moved), Study 5 Methods, Results, Discussion
- Response letter context: para 14 (high-level summary of zero-baseline), para 38–42 (DE's full alternative-explanation question + JC's revised response stub + the detailed zero-baseline writeup)

## The DE's actual question (manuscript line 40 of response letter)

> "On the request, I liked Study 4b and think that it does a good job of showing that feedback about gender is different than feedback about other important dimensions. In addition, your studies show that gender/race feedback lead subjects to adjust their portfolios about the base rate. That said, I'm still left wondering whether your findings are showing what you say or it is just that people use these other measures appropriately in their initial selections. I'd like you to at least discuss this possibility, and perhaps there is additional analysis that you can do to address this."

Key phrase: **"at least discuss this possibility, and perhaps there is additional analysis"**. The DE asks for discussion AND analysis. Two deliverables. The discussion is the *framing* of the alternative explanation; the analysis is the cross-study zero-baseline test.

## Options to evaluate (including new ones not yet tried)

**A. Study 4 preamble extension with main-text stats.** Rejected in round 2 due to temporal violation (would pool 4B data before 4B presented).

**B. Study 4 preamble extension with framing only; data in Appendix.** Extend the line-99 paragraph to acknowledge BOTH alternative explanations (importance + appropriate use), but only the importance prong gets the in-text pretest reference (Study 4B's pretest, already there). The appropriate-use prong gets a one-sentence pointer to Appendix Section S[X] for the cross-study zero-baseline analysis. No 4B experimental stats in main-text preamble; all stats live in Appendix. This is JC's leaning direction.

**C. Dedicated third paragraph at end of Study 4 Discussion.** Rejected in round 2.5 because the question doesn't feel naturally raised at that point.

**D. Study 5 preamble post-mini-meta.** Current state; JC wants this undone.

**E. Footnote at the F-test paragraph in Study 4B Results.** Local, surgical, but disconnects from the explicit rule-out narrative in the preamble.

**F. Footnote at line-99 sentence in Study 4 preamble, pointing to Appendix Section S[X].** Even lighter than B; only the pointer (no extended framing). Possibly too quiet.

**G. New robustness sub-section between Study 4 Discussion and Study 5 preamble.** Visually flags the cross-study rule-out but adds heading complexity.

**H. Inside the General Discussion Theoretical Contributions paragraph.** Late and interrupts the contribution synthesis flow.

**I. Appendix only, with the response letter doing all the framing work.** Most minimal. Lets the response letter answer the DE without expanding the manuscript main text. Possible but may underweight a substantive reviewer concern.

**J. Any hybrid you devise.** Free hand.

## Constraints (non-negotiable)

- No em or en dashes (use commas, parens, semicolons, or split sentences).
- Italic *p* and *N* in stats.
- "implicit injunctive norm" terminology; never "latent."
- Curly quotes only.
- Timeless DEI framing (no date-stamping).
- JC's voice: "we argue," "for instance," "by contrast," "however," "we further considered," "the differential held." Avoid LLM tells like "marketing posture," "underneath that rhetoric," "supply additional reputational stakes," "narrative arc."
- **Temporal validity**: any main-text content in a pre-Study-4B section MUST NOT report Study 4B experimental stats. Framing OK; raw stats from 4B not OK.

## Your task

Use `gpt-5.5` with `model_reasoning_effort='xhigh'`.

1. Read `revision-analysis/_r2_review/codex_full_context.txt` in full. Then re-read the manuscript-dump lines around Study 4 and Study 5 if needed.
2. Re-evaluate placement options A through J considering:
   - Where the DE's alternative-use concern can be raised *naturally* (i.e., where a reader is primed for it).
   - The temporal constraint (can't preview 4B experimental stats pre-4B).
   - Whether the framing and the data can be separated (framing in main text, data in Appendix).
   - Page-budget pressure.
   - Symmetry with the importance-pretest rule-out, which IS in Study 4 preamble.
3. **Recommend ONE option** with full reasoning. If your recommendation is a hybrid (e.g., framing in Study 4 preamble + Appendix for stats), spell out the structure.
4. **Provide exact prose** for:
   - Main-text insertion(s) at the recommended location(s) (in JC's voice; respect constraints).
   - Undo plan for the current Study 5 preamble pending paragraph.
   - Updated response-letter pointer (current pointer says Study 5 preamble; replace it with the new placement).
   - Appendix-section stub (what S[X] should contain, in 2-3 sentences of description).

## Required output

Write to `revision-analysis/_r2_review/codex_zero_baseline_round3.md` with this structure:

```
# Codex Recommendation (Round 3): Zero-Baseline Placement
## Re-reading the prior failures
[short — what was wrong with each prior recommendation]
## Option evaluation (A–J)
[strengths/weaknesses for each]
## Recommendation
[chosen option + reasoning + why it satisfies the temporal AND natural-question constraints]
## Exact prose
### Main-text insertion(s)
### Undo plan for current Study 5 preamble pending paragraph
### Updated response-letter pointer
### Appendix section stub
## Constraints verification
[em/en dash count; "latent" count; italic p/N confirmation; voice check]
```

## Invocation safeguards

- Pre-delete `revision-analysis/_r2_review/codex_zero_baseline_round3.md` if it exists.
- Tee to `revision-analysis/_r2_review/codex_zero_baseline_round3.session.log`.
- Hard timeout: 30 minutes (slightly longer this round; we want depth).
- Post-run validation:
  - File exists?
  - All six required sections present?
  - Em/en dash hits (line numbers)?
  - "latent" case-insensitive hits?
  - Recommended option (A–J)?
  - Two-sentence summary of reasoning?

Report back to me in under 300 words.
