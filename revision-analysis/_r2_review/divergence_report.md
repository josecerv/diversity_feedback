# Claude vs. Codex divergence report (R2 manuscript drafts)

Generated 2026-05-12 after both passes completed. Both passes were independent and
worked from the same canonical sources (`manuscript_gdoc_text.txt`,
`response_letter_gdoc_text.txt`).

| Draft | Status |
|---|---|
| 1 (Intro citation refresh) | DIVERGES on Schultz sentence handling |
| 2 (Intro implicit-norm framing) | DIVERGES on opening clause |
| 3 (GD citation refresh) | AGREE (identical citation block) |
| 4 (GD two-pathway) | DIVERGES on Kirgios placement + paragraph structure |
| 5 (political-ideology null) | DIVERGES on 5A wording length |
| 6 (Study 1 NPR pretest) | DIVERGES on opening clause |
| 7 (References) | AGREE (same 9 entries, alphabetical) |

Agreements: numbers, citations, factual claims. No factual disagreements.

---

## Draft 1 — Intro Schultz sentence

**Claude (in .docx):** appends a new sentence AFTER the existing Schultz et al. (2007)
example, leaving the original sentence intact:

> A growing applied literature has since shown that descriptive-feedback-plus-norm
> designs of this kind change household behavior across countries and settings
> (Schultz, Nolan, & Cialdini, 2018; Andor, Gerster, & Peters, 2020; Henry, Ferraro,
> & Kontoleon, 2019; Mertens & Schultz, 2021).

**Codex:** REWRITES the Schultz sentence as a freestanding takeaway:

> Taken together, the original Schultz et al. (2007) findings and later reprise by
> Schultz, Nolan, and Cialdini (2018) underscore that descriptive feedback is most
> effective when it is accompanied by normative information about what behavior is
> desirable, a conclusion echoed in more recent applied feedback interventions in
> energy and household conservation domains (Andor, Gerster, & Peters, 2020; Henry,
> Ferraro, & Kontoleon, 2019; Mertens & Schultz, 2021).

**Trade-off:** Claude preserves the existing manuscript prose (lower-risk, less
review burden). Codex synthesizes the two Schultz papers into a single point (cleaner
theoretical framing but rewrites text JC already approved in prior rounds).

---

## Draft 2 — Intro implicit-norm framing, opening clause

**Claude:** *"Consistent with the broader logic of the feedback literature, feedback
regulates behavior only when there is a standard against which past performance can be
evaluated. The classical literature establishes that pairing feedback with an explicit
goal supplies that standard. But the same logic implies that descriptive feedback
alone can be effective in domains where a strong implicit standard is already
entrenched..."*

**Codex:** *"This logic implies a boundary condition on when descriptive feedback
alone should motivate change. The classical feedback literature establishes that
pairing feedback with an explicit goal can work because it supplies a standard
against which past performance can be evaluated. However, an explicit goal should be
less necessary when a strong implicit standard is already entrenched..."*

**Trade-off:** Claude follows response-letter line 63's opening verbatim ("Consistent
with the broader logic..."). Codex is more compact and frames the same content as a
"boundary condition," which is a sharper rhetorical move but departs from the
letter's wording.

---

## Draft 4 — GD two-pathway paragraph

**Claude:** integrates Kirgios et al. (2022) AS A SENTENCE INSIDE the two-pathway
paragraph, after the internal-motivation definition. Closing line ties both pathways
to partisan generalization.

**Codex:** keeps the two-pathway paragraph free of Kirgios; the political-ideology
null is the closing sentence, but Kirgios is placed instead in Draft 5A. Codex's
structure: define external → define internal → null is consistent with both.

**Trade-off:** Codex's split (definition paragraph in 4, citation in 5) reads
cleaner; Claude's integration ties the empirical evidence to the theoretical
distinction more tightly. Either works.

---

## Draft 5A — Study 4 Discussion hedge

**Claude:** *"Although we are cautious about over-interpreting a null interaction,
the pattern is suggestive that descriptive feedback operates across partisan lines,
consistent with the broader claim that the effect is sustained by injunctive norms
shared across the political spectrum rather than by any single political community's
stated DEI commitments. This interpretation also converges with a recent field
experiment in which..."*

**Codex:** *"This null moderation pattern is suggestive that descriptive feedback
operates across partisan lines, consistent with field-experiment evidence that women
and racial minorities benefited similarly from explicitly naming their demographic
identity when contacting Republican and Democratic city councilors, an effect
mediated by motivation to control prejudice (Kirgios et al., 2022)."*

**Trade-off:** Claude is more cautious (explicit "we are cautious about
over-interpreting") and longer. Codex is direct, single-sentence, and integrates the
Kirgios citation more tightly. The response letter's wording at line 76 is closer to
Codex's structure (one sentence about the null + immediate Kirgios reference).

---

## Draft 6 — Study 1 NPR pretest paragraph opening

**Claude:** *"To address a concern that participants might have been less swayed by
descriptive feedback about the three comparison attributes simply because they viewed
those attributes as less important to NPR's mission than gender, we ran a between-
subjects pretest with N = 300 Prolific participants..."*

**Codex:** *"To rule out the possibility that Study 1's pattern was driven by
participants viewing gender as more important than the comparison attributes, we
conducted a separate pretest. We recruited N = 300 Prolific participants in a
between-subjects design..."*

**Trade-off:** Claude's opening frames the alternative as a reviewer concern;
Codex's opening states the empirical possibility being ruled out (which is the same
content but slightly more neutral, less defensive in tone). Both use the same
numbers. Codex matches the response letter's "rule out" framing.
