# Claude Round 3: Zero-Baseline Placement — Holistic Re-think

## What I missed in rounds 1 & 2

**Round 1 (Study 5 preamble):** Mistook surface similarity to the mini-meta (both "baseline" cross-study analyses) for actual conceptual fit. They test different questions.

**Round 2 (Study 4 preamble with stats):** Tried to put main-text stats in a pre-Study-4B section, which temporally previews 4B's experimental data.

**Round 2.5 (Study 4 Discussion 3rd paragraph):** Missed that the DE's appropriate-use concern isn't a question a reader spontaneously asks. It has to be *primed*, then *answered*. By the end of Study 4, the F-test evidence is already in; the concern feels artificially inserted because no one was carrying it as an open question.

## The actual constraint set

A correct placement must satisfy ALL of:
1. **Temporal validity**: no Study 4B experimental stats in main text before Study 4B's Results.
2. **Natural-question priming**: the alternative explanation has to be raised somewhere the reader is *waiting* for it, not after the data has already settled the question.
3. **Conceptual home**: the manuscript already engages this kind of concern in *one specific place* — manuscript line 99 (Study 4 preamble). That paragraph is structured as "we attempted to rule out the possibility that X… here's how Study 4 addresses X." The zero-baseline analysis addresses a *sibling* possibility (appropriate initial use, where the existing paragraph addresses the importance possibility).
4. **JC's voice + constraints**: no em dashes, italic *p* and *N* in any stats reported, "we considered/addressed/argued" patterns, curly quotes, timeless DEI framing.

## The unlock: separate framing from data

Line 99 itself doesn't put pretest stats in main text. It says: *"we selected comparison attributes after pre-testing them for importance, and we ensured that all were rated at least as important as gender or race. See Appendix Study S4 for a complete description of our pre-testing procedure and results."* No N=550, no t-stats, no p-values. The framing lives in main text; the data lives in Appendix.

We can do the **same thing** for the appropriate-use concern. Frame the alternative, name the rule-out approach, point to Appendix. Zero 4B experimental stats in main text. Zero temporal violation.

## Recommendation: Option B (Study 4 preamble extension, framing only)

Add a new paragraph immediately after the existing line 99 paragraph (which addresses the importance prong) and before "We examined the effect of feedback about the race of past selectees in Study 4A…" The new paragraph names the appropriate-use concern, says we addressed it via a cross-study analysis, and points to Appendix Section S[X]. No stats in main text. Symmetric structure with the importance rule-out above it.

**Why this satisfies the four constraints:**
- *Temporal*: no 4B experimental stats appear in main text pre-4B. Only framing.
- *Natural question*: the reader has just been primed (one paragraph earlier) with the importance concern. Adding the appropriate-use concern as a sibling on the next paragraph is exactly when a curious reader would be open to "what other alternatives did you rule out?"
- *Conceptual home*: extends the existing rule-out neighborhood. The Study 4 section becomes "two concerns about the comparison-attribute differential, both addressed."
- *Voice + constraints*: matches line 99's structure; no main-text stats means no italic-formatting risk; "we also considered/addressed" patterns are JC's.

## Exact prose

### New paragraph for Study 4 preamble (insert after line 99 paragraph, before "We examined the effect…")

> Relatedly, we also considered the possibility that participants may have already been using the comparison attributes appropriately in their initial selections, leaving little room for feedback about those attributes to shift behavior. To address this, we conducted a cross-study analysis examining the subset of participants across our studies whose initial selections contained zero of a given attribute, where appropriate prior use cannot account for any non-response to feedback. The differential between race or gender feedback and comparison-attribute feedback persisted in this subset. See Appendix Section S[X] for full details.

Four short sentences. Sentence 1 names the concern, in language echoing the DE's R1 wording ("appropriately" matches the DE's word choice). Sentence 2 describes the rule-out approach without 4B stats. Sentence 3 states the directional finding (parallel to line 99's directional "rated at least as important as gender or race"). Sentence 4 points to Appendix.

### Undo plan for current Study 5 preamble paragraph

Reject the pending suggestion in Study 5 preamble (the paragraph beginning *"A complementary analysis examines the clearest case of underrepresentation…"*) via Chrome.

### Updated response-letter pointer

Replace current pointer in response letter para 42:

**OLD** (current state after my prior edit): *"We have added a short paragraph to the preamble of Study 5 (p. X) summarizing these analyses, where it sits alongside the manuscript's existing re-analysis of Studies 2, 3A, 3B, 4A, and 4B examining whether feedback effects are larger when initial selections contain fewer women or racial minorities. Interested readers can find the detailed results in a new appendix section (see Appendix Section X)."*

**NEW**: *"We have added a brief paragraph to the Study 4 preamble (p. X) addressing this concern alongside the importance rule-out described in the same section. The full cross-study analysis is reported in Appendix Section S[X]."*

### Appendix Section S[X] stub (what should be there)

Two-paragraph Appendix entry:
- *Para 1 — Motivation*: Restates the DE's appropriate-use concern. Briefly frames why a zero-baseline subset is the appropriate test.
- *Para 2 — Analysis and results*: Pooled OLS with study-by-attribute fixed effects and participant-clustered standard errors. Restricts to participant-attribute observations where the initial selection contained zero of the focal attribute. Reports race/gender pooled coefficient (+22.50pp, *p* < .001, *N* = 864), comparison-attribute pooled coefficient (+7.25pp, *p* = .19, *N* = 414), and the Wald test of the difference (*p* = .016, 95% CI [+2.70, +26.21]). Includes Table R1 from the response letter (per-cell estimates with notes about which cells are omitted and why).

## What I'm explicitly NOT recommending

- **Stats in main-text preamble**: violates temporal constraint.
- **Dedicated Study 4 Discussion paragraph**: JC says question isn't naturally raised after Study 4.
- **General Discussion paragraph**: too late; reader has already finished the empirics.
- **Footnote-only**: too quiet for a substantive DE concern.
- **New robustness sub-section with heading**: adds structural complexity for one cross-study check.

## Constraints check

- No em or en dashes in proposed prose.
- No stats in main text → no italic *p*/*N* formatting risk in the new paragraph.
- Voice: "we also considered," "to address this," "the differential persisted," "see Appendix… for full details" — all JC patterns.
- "implicit injunctive norm" not invoked (not needed for this paragraph); "latent" not used.
- Curly quotes (auto-curl in Gdocs typing).
- Timeless framing (no date-stamping).
