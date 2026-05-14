# R2 Manuscript Changes — Independent Codex Pass v3 (Enhancement-Based)

You are producing an independent SECOND draft of the R2 manuscript-change
proposals. Claude already produced v2 at
`revision-analysis/_r2_review/r2_manuscript_changes.md` after the user gave
strong feedback on v1. Your job is to do your own pass with the same framing
constraints so the user can diff your version against Claude's and converge on
the best one.

## CRITICAL framing constraint

The user explicitly rejected v1 because it proposed several "new paragraphs"
at points where the manuscript already discusses the concept. His feedback:
"As I'm reading this, I'm finding out that you have not read the manuscript
properly. We need to enhance and augment all of the places we have already
discussed these items. None of these things are necessarily new; they're just
maybe not enhanced or thought out deeply. Scratch all this. Read the intro
first, read the discussion, read every single piece of the manuscript, and
then work backwards, saying, 'Oh, what can we enhance or what can we augment?'
That's really what we should be doing."

Your DEFAULT for every Change must be:
1. READ the manuscript end-to-end first (Introduction through Conclusion, plus
   every Study Discussion).
2. For each response-letter commitment, FIND the existing paragraph(s) that
   already discuss the concept. Quote the relevant excerpts.
3. Propose a MINIMAL enhancement to that paragraph (one to four sentences
   added, or a citation block expanded), in JC's existing voice. Do NOT
   propose a "new paragraph" unless you have verified the concept genuinely
   is not discussed AND the response letter explicitly commits to substantive
   new content.
4. Show the new material in [square brackets] inside the existing paragraph
   so the augmentation is visually distinct from the existing prose.

Only Change 8 (the DEI socio-political update) is expected to be a genuine
multi-paragraph addition, because Reviewer 2 explicitly said the manuscript's
one-sentence rollback nod was "not sufficient" and the response letter
commits to ~3 paragraphs of new content.

## Sources (already on disk)

- Manuscript text: `revision-analysis/_r2_review/manuscript_gdoc_text.txt`
- Response letter: `revision-analysis/_r2_review/response_letter_gdoc_text.txt`
- Claude's v2 draft for reference: `revision-analysis/_r2_review/r2_manuscript_changes.md`
- Citation audit: `revision-analysis/_r2_review/citation_audit.md`

Read the manuscript text in full BEFORE drafting any Change. The
Introduction (paragraphs 7-29), every Study Discussion (paragraphs 50, 69,
93, 130-132, 156-158), the General Discussion (paragraphs 166-172), and
Future Directions and Limitations (paragraphs 173-178) all matter.

## The 12 Changes to draft (same as v2)

For each Change you must produce:
- **Anchor.** The existing manuscript paragraph (line number + brief excerpt).
- **Response letter commitment.** A verbatim block quote from
  `response_letter_gdoc_text.txt` with the line number, showing exactly what
  we promised.
- **What the manuscript already says.** One short paragraph summarizing
  where the concept already lives in the manuscript with line numbers and
  brief excerpts. If you cannot find existing discussion, say so and justify
  why a new paragraph is necessary.
- **CURRENT.** Block quote of the existing paragraph (verbatim).
- **PROPOSED enhancement.** The SAME paragraph with new material in
  [square brackets]. The unbracketed text must be unchanged from the
  manuscript.
- **Notes.** Any divergence from Claude's v2 take, voice/style notes, or
  open questions for the user.

Changes 1-12 below are anchored to the same response-letter commitments as
v2. You do not need to redo Change 13 (References); that converged in v2
and Claude already incorporated the citation-audit fixes.

1. **Intro citation refresh A** (FIT/Kluger parenthetical, paragraph 11):
   add Locke & Latham 2019; Itzchakov & Latham 2018; Chen et al. 2020 to the
   existing parenthetical. Pure citation-block expansion. Response letter
   line 35 and 82.

2. **Intro citation refresh B** (paragraph 10 social-norms parenthetical
   plus Schultz 2007 inline): add Schultz et al. 2018, Andor et al. 2020,
   Henry et al. 2019, Mertens & Schultz 2021 to the existing parenthetical;
   pair Schultz et al. (2007) with the 2018 reprise inline. JC explicitly
   rejected v1's "feedback designs" theoretical sentence as too long-winded;
   keep this as a citation-only refresh. Response letter line 80 and 82.

3. **Intro implicit-norm framing** (enhance paragraph 11 closing): add
   1-2 sentences making explicit the "standard-already-entrenched is a clean
   test case" logic and citing Plant & Devine 1998, Crandall & Eshleman 2003,
   Álvarez-Benjumea 2023. Builds on paragraph 11's existing claim that
   descriptive feedback works when it suggests behavior violates implicit
   injunctive norms. Response letter line 83.

4. **Study 1 Discussion brief importance-pretest mention** (enhance paragraph
   50): ONE additional sentence borrowing the brief framing already used in
   Study 4 (paragraphs 98 and 132). Point to a new Appendix section for the
   full N = 300 pretest numbers. JC explicitly said: "don't make it super
   [long]." Response letter line 42.

5. **GD foundational-citation refresh** (paragraph 170 citation block): add
   Locke & Latham 2019 and Schultz et al. 2018 to the existing
   `(Kluger & DeNisi, 1996; Schultz et al., 2007)` parenthetical. Pure
   citation-block expansion. Response letter line 35.

6. **GD two-pathway enhancement** (enhance paragraph 170): add 2-3 sentences
   sharpening the external/internal motivation distinction and the
   political-ideology bridge, plus the Levi & Fried 2024 citation for the
   "DEI rhetoric as downstream signal" claim. Builds on paragraph 170's
   existing "internal and external motivation to respond without prejudice"
   sentence and paragraph 176's existing public/private discussion. Response
   letter line 98.

7. **Study 4B Discussion Kirgios sentence** (enhance paragraph 132): one
   sentence between the existing two, interpreting the political-ideology
   null and citing Kirgios et al. 2022 as corroborating field evidence.
   Response letter line 96.

8. **Intro DEI socio-political landscape** (replace paragraph 15's closing
   "And despite…" sentence and insert 3 new paragraphs between 15 and 16).
   This IS the legitimately multi-paragraph change. Response letter line
   92-93 supplies the canonical content. R2 explicitly called the existing
   one-sentence nod "not sufficient."

9. **Intro no-appropriate-level clarification** (enhance paragraph 14): 2
   short sentences inserted between the existing "We further posit..."
   sentence and the existing "Why?" rhetorical question. Makes explicit that
   the asymmetric prediction does not assume an "appropriate" baseline for
   any attribute. Response letter line 45.

10. **Study 4 Discussion footnote** (new footnote on paragraph 132's last
    sentence). Local-context restatement of Change 9. Response letter line
    45. NOT the deferred Table R1 footnote.

11. **Future Directions scope condition** (enhance paragraph 174): append
    2-4 sentences identifying organizational DEI climate as a parallel
    scope condition. Builds on paragraph 174's existing attribute-visibility
    scope discussion. Response letter lines 38 and 93.

12. **Future Directions fairness-norms mention** (enhance paragraph 175): 1-2
    sentences appended to the existing fairness-threshold sentence
    generalizing the fairness-norms mechanism to the underrepresentation
    case as a future-research direction. Response letter line 99.

## Voice constraints (NON-NEGOTIABLE)

- NO em dashes (U+2014) or en dashes (U+2013) in DRAFTED prose. Curly quotes
  / non-breaking hyphens / en-dashes inside verbatim response-letter quotes
  are fine (preserve those exactly).
- Use curly apostrophes (U+2019) and curly quotes (U+201C / U+201D) when you
  write new prose, not straight ones.
- Theory term is "implicit injunctive norm" — NEVER write "latent" in
  drafted prose. (It is fine to mention "the disallowed theory term" in a
  Self-Audit line.)
- Match JC's scholarly register exactly. Read paragraphs 11, 14, 17, 170,
  174, 175 to calibrate. Do NOT invent new theoretical language such as
  "feedback designs that pair information about past behavior with a clear
  normative standard"; if you can use the manuscript's existing phrasing,
  do so.

## Required output structure

Single markdown document at `revision-analysis/_r2_review/codex_v3_pass.md`
with the following section headings in this exact order (the wrapper
validates their presence):

```
## Change 1
## Change 2
## Change 3
## Change 4
## Change 5
## Change 6
## Change 7
## Change 8
## Change 9
## Change 10
## Change 11
## Change 12
## Self-Audit
```

Under "## Self-Audit", confirm:
- No em or en dashes in drafted prose.
- The disallowed theory term does not appear in drafted prose.
- Every Change either points to an existing paragraph that already discusses
  the concept (and proposes enhancement) OR explicitly justifies why a new
  paragraph is necessary.
- Out-of-scope item (Study 4B Table R1 / zero-initial pooled analysis
  footnote) is NOT drafted.
- All 12 Changes drafted.
