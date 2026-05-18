# Claude Recommendation: Zero-Baseline Placement

## Option evaluation

### Option A — Keep in Study 4 Discussion, split into own paragraph, clean fragment
**For:** Minimal restructuring; preserves the proximity to Study 4B's F-test attribute comparison in paragraph 127.
**Against:** The analysis pools Studies 2, 3A, 3B, 4B (not Study 4A). Putting a cross-study re-analysis inside an individual study's Discussion misleads the reader about scope. Even with a paragraph break, this would be the only cross-study re-analysis tucked inside any single-study discussion in the paper, breaking the manuscript's structural pattern. Does not motivate Study 5 (which already motivates itself with the mini-meta). Leaves the analysis feeling like a tack-on, exactly the problem JC flagged.
**Verdict:** Acceptable fallback. Does not solve the structural problem.

### Option B — Move to Study 5 preamble as a follow-up to the mini-meta
**For:** The Study 5 preamble already contains a sister analysis (the mini-meta) testing the same theoretical claim — that descriptive feedback operates through perceived underrepresentation — using nearly the same studies. The mini-meta tests the continuous form (proportion × feedback) and pools across 2, 3A, 3B, 4A, 4B. The zero-baseline analysis tests the discrete extreme (initial = 0) and adds the comparison-attribute contrast, pooling across 2, 3A, 3B, 4B. They are conceptually complementary, not redundant. Co-locating them gives the reader one coherent pre-Study-5 evidentiary unit that grounds the experimental manipulation. Clears Study 4 Discussion of cross-study material that does not belong there.
**Against:** Adds a paragraph to a preamble that is already setup-heavy. Reader sees more re-analysis before reaching the new Study 5 experimental data. The DE's R1 question response is now physically distant from the original Studies 2-4 results sections — but this is mitigated by the fact that the analysis pools those same studies and explicitly references them.
**Verdict:** Dominant choice. The narrative and conceptual fit is exact.

### Option C — Move to Study 4B preamble (before Study 4B results)
**For:** Pre-empts the DE's alternative explanation before showing Study 4B's primary findings.
**Against:** Study 4B preamble is methods/setup material — a cross-study re-analysis does not belong there structurally. Awkward to tell the reader "before I show you Study 4B's results, here is a pooled re-analysis that includes Study 4B and four earlier studies." Disrupts methods → results flow. Reject.

### Option D — Appendix only, one-sentence manuscript pointer
**For:** Cleanest manuscript surface.
**Against:** DE Wu invested in this alternative explanation specifically. A one-sentence pointer is not engagement; it reads as evasion. Burying the analysis weakens the manuscript's response to a substantive theoretical challenge. Reject.

### Option E — Footnote at Study 4B Discussion (what response letter currently promises)
**For:** Aligns with what we already told the DE.
**Against:** A footnote for a multi-coefficient cross-study pooled analysis with a Wald test is awkward in a Management Science paper. The analysis is too substantive for footnote-only treatment. Still does not fit Study 4B Discussion thematically. We can update the response letter to match a better placement, so the "alignment with response letter" argument carries no weight.

## Recommendation

**Option B.** The zero-baseline analysis and the existing mini-meta are sister analyses testing the same theoretical claim across nearly the same set of studies, using complementary statistical cuts (continuous proportion vs. discrete extreme; without vs. with comparison-attribute control). They must be co-located. The Study 5 preamble is the natural home because its established purpose is to ground the experimental Study 5 manipulation in re-analyses of prior-study data. The current Study 4 Discussion placement violates structural symmetry (no other individual study's Discussion contains cross-study re-analysis) and feels unmotivated because the analysis genuinely is unmotivated by its surrounding prose. Move it where the question it answers is already being asked.

## Exact prose

### Cleaned Study 4 Discussion paragraph 2 (paragraph 133)

> In addition, Study 4B established that the effect of gender feedback was not moderated by political ideology nor party affiliation. Although we are cautious about over-interpreting a null interaction, this pattern is suggestive that descriptive feedback operates across partisan lines, an interpretation that converges with field-experiment evidence that women and racial minorities benefited from explicitly naming their demographic identity when contacting Republican and Democratic city councilors alike, an effect mediated by motivation to respond without prejudice (Kirgios et al., 2022).

*(Removes both the zero-baseline insertion and the orphaned "Finally, it…" fragment. Paragraph ends with the Kirgios citation.)*

### Insertion in Study 5 preamble (new paragraph immediately after paragraph 140, the mini-meta results)

> A complementary analysis isolates the most extreme case of underrepresentation: participants whose initial selections contained zero candidates with a given focal attribute. Selecting zero of an attribute is a clear signal of underrepresentation that should provoke a response to feedback regardless of whether that attribute is race, gender, or one of the comparison attributes we test. This setup also lets us compare the response to race or gender feedback against the response to feedback about comparison attributes, holding the underrepresentation signal constant. Pooling participants across Studies 2, 3A, 3B, and 4B (Study 4A is excluded because no participant began with zero of any comparison attribute), race or gender feedback raised the probability of selecting a racial minority or woman, respectively, by 22.50 percentage points among participants whose initial selection contained zero of the focal attribute (*p* < .001, N = 864). Feedback about the comparison attributes in those same studies did not significantly shift subsequent selections among participants whose initial portfolios contained zero of the comparison attribute (effect = +7.25 percentage points, *p* = .19, N = 414). A Wald test confirms the difference between the two pooled coefficients is significant (*p* = .016). Full results appear in Appendix Section S[X]. This pattern aligns with our theorizing that descriptive feedback operates through an implicit injunctive norm against the underrepresentation of historically marginalized groups, rather than through a general response to any noticed gap in an initial portfolio.

### Updated response letter paragraph 34

> RESPONSE: Thank you for raising this alternative. We agree it is worth testing directly, and we report a follow-up analysis below (Table R1). We have also incorporated this analysis into the Study 5 preamble (p. X), where it sits alongside an existing mini-meta of the proportion of women or racial minorities initially selected and provides a complementary cross-study test of whether perceived underrepresentation specifically drives the effects of race or gender feedback. The full per-cell estimates appear in Appendix Section S[X].

## Study 4B importance-callback question

JC raised the worry that Study 4B's comparison attributes (multi-genre work, large book sales, durable canonical presence) were not separately pretested for importance the way Study 1's NPR-expert comparison attributes will be. Should Study 4B's preamble add a callback?

**My recommendation:** No standalone pretest is needed, and the existing F-test comparisons in paragraph 127 already empirically establish that the gender-feedback effect is significantly larger than the feedback effect for each comparison attribute. That is the relevant empirical defense. However, a one-sentence note in Study 4B's preamble or Methods acknowledging that the comparison attributes were selected as plausibly important evaluative criteria for book authors (thematic range, market reach, lasting cultural significance) would frame the design choice without overclaiming a separate pretest. If JC wants to add this, it should be one short clause in Study 4B's preamble, not a structural addition.

**Why this is lighter than Study 1:** Study 1 needed a separate importance pretest because the comparison attributes for NPR experts ("under 50 years old," "based on the West Coast," "worked at a university") were narrow and could plausibly read as less important than gender. Study 4B's comparison attributes are intrinsically core to evaluating book authors, so the design itself carries more face validity for importance, and the F-test comparisons in 4B's Results provide direct empirical confirmation.

## Knock-on edits required if Option B is chosen

1. Manuscript: Delete the zero-baseline block + the orphaned fragment from Study 4 Discussion paragraph 2 (paragraph 133 in the current text dump).
2. Manuscript: Insert the new paragraph after paragraph 140 in the Study 5 preamble.
3. Response letter (Gdoc 1YBHCMwD84xC3Qw38CPEzz1iv3JKg7Mw9jTJqyZGptkI): Replace paragraph 34 RESPONSE text.
4. Verify italics on *p* and N stay intact when the insertion lands in its new location (we already debugged this once for the current placement).
5. No change needed to the existing mini-meta paragraphs in the Study 5 preamble. They stand on their own.

## Constraints checked

- No em or en dashes in any prose above.
- "implicit injunctive norm" used; "latent" not used.
- Italic *p* and N in stats.
- Curly quotes only.
- DEI content not date-stamped to 2024.
