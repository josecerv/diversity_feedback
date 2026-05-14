# R2 Manuscript-Insert Drafts — Second Pass (Independent Parallel)

You are producing an independent SECOND-pass set of prose drafts for the R2 minor
revision of a Management Science manuscript on whether descriptive feedback enhances
diversity in selection decisions. Claude is producing its own drafts in parallel; the
user will diff your prose against Claude's afterward. Your job is to write the best
prose you can without seeing Claude's drafts.

This pass differs from the first pass (`codex_brief_r2.md`) in two ways:
1. The R2 response letter has been finalized with substantially more detail since
   the first pass. Re-examine items 1-7 against the finalized response letter and
   refine where the now-final wording demands it.
2. The previously out-of-scope DEI socio-political update is now IN SCOPE, along
   with several smaller items the first pass did not draft.

## Sources (already on disk)

- Manuscript text: `revision-analysis/_r2_review/manuscript_gdoc_text.txt`
- Response letter (canonical promises): `revision-analysis/_r2_review/response_letter_gdoc_text.txt`
- First-pass output: `revision-analysis/_r2_review/codex_first_pass.md`
- First-pass divergence report: `revision-analysis/_r2_review/divergence_report.md`

The local `Manuscript_DoesFeedbackEnhanceDiversity.docx` and
`revision-analysis/_r1_review/response_letter.txt` are STALE; do not read them.

## What to draft (twelve inserts; out-of-scope items are explicit below)

For each item, the response letter is the canonical statement of intent. Match its
citations and theoretical commitments; write fresh prose in JC's manuscript voice
(read manuscript paragraphs 13-17 for Introduction register, 167-178 for General
Discussion register).

For items 1-7, the first-pass drafts in `codex_first_pass.md` are your starting point
but not your ceiling. Re-read the response letter passage cited in each item and
revise where the now-final letter demands sharper, more accurate, or more conservative
prose. For items 8-12, write from scratch.

### Item 1 — Introduction, feedback/goals citation refresh A

Anchor: manuscript line 11 (`"According to Feedback Intervention Theory, such feedback
is often ineffective because it lacks a benchmark against which people can evaluate
their past behavior and consider making a change (Kluger & DeNisi, 1996, 1998)."`).

Append three citations to the existing parenthetical: Locke & Latham, 2019; Itzchakov
& Latham, 2018; Chen, Latham, & Piccolo, 2020. Do not change the surrounding prose
unless the response letter (line 35, 82) demands it.

### Item 2 — Introduction, Schultz reprise + applied feedback citations

Anchor: manuscript line 10. The existing sentence reads `"Similarly, Schultz et al.
(2007) found that when households received feedback about their energy usage
alongside an explicit injunctive norm communicating that conservation was desirable as
well as a social comparison to the energy use of their neighbors, energy consumption
dropped by 5.7%."`

Update to incorporate Schultz, Nolan, & Cialdini (2018) alongside the classic 2007
paper, and add the applied trio (Andor et al. 2020; Henry et al. 2019; Mertens &
Schultz 2021).

The first-pass divergence (Claude appended, Codex rewrote) was unresolved. Default
**append**: keep the existing Schultz et al. (2007) sentence intact and add one
follow-on sentence that introduces the 2018 reprise + applied citations. This
minimizes review burden and preserves text JC already approved in R1.

### Item 3 — Introduction, expanded implicit-injunctive-norm framing

Insert a NEW paragraph immediately before manuscript line 17 (`"We propose that when
descriptive feedback reveals that an evaluator's past selections included few women or
racial minorities..."`).

The paragraph should articulate why descriptive feedback alone can suffice in domains
where a strong implicit standard is already entrenched, naming the diversity-related
selection context as the clean test case. Cite Plant & Devine (1998), Crandall &
Eshleman (2003), and Álvarez-Benjumea (2023). The canonical wording target is response
letter line 83 (Reviewer 1 reply).

### Item 4 — Study 1 Discussion, NPR-attribute-importance pretest

Anchor: insert a NEW paragraph AFTER manuscript line 50 (`"Study 1 mirrored a real
organizational practice currently deployed by NPR..."`) and BEFORE the `"Insert Figure
3 about here"` placeholder at line 52.

USE THESE EXACT NUMBERS from the response letter (letter line 42):

- Recruited N = 300 Prolific participants, between-subjects, each rated ONE expert
  attribute on a 1-7 importance scale with the NPR producer framing held constant.
- Whether a guest was a woman: M = 3.64, SD = 1.76, n = 76.
- Worked at a university: M = 4.67, SD = 1.60; t(148.8) = -3.76, p < .001, d = -0.61.
- Were under 50 years old: M = 4.44, SD = 1.73; t(149.0) = -2.81, p = .006, d = -0.46.
- Based on the West Coast of the United States: M = 3.37, SD = 1.75; t(149.0) = 0.95,
  p = .343, d = +0.16.
- Frame as a conservative test: comparison attributes were rated as equally or MORE
  important than gender, so differences in perceived importance cannot explain why
  only gender feedback moved subsequent selections.

DO NOT use numbers from any earlier analysis log — only the response-letter values.

### Item 5 — General Discussion, foundational-citation refresh

Anchor: manuscript line 170 (`"Our work makes two primary theoretical contributions...
According to Feedback Intervention Theory, feedback is most effective when paired with
explicit goals, standards, or explicit injunctive norms that signal a clear gap
between current performance and a desired state (Kluger & DeNisi, 1996; Schultz et
al., 2007)."`).

Replace `(Kluger & DeNisi, 1996; Schultz et al., 2007)` with `(Kluger & DeNisi, 1996;
Locke & Latham, 2019; Schultz et al., 2007; Schultz, Nolan, & Cialdini, 2018)`.

### Item 6 — General Discussion, sharpened two-pathway framing

Insert a NEW paragraph in the Theoretical and Practical Contributions section (around
line 170), after the sentence ending `"an implicit injunctive norm to avoid prejudice
has already been internalized by the decision-maker."` and before the existing closing
sentence about `"focus attention on past behavior"`.

The new paragraph must explicitly distinguish two pathways:
- **External motivation to control prejudice**: driven by observability, legal
  accountability, customer/employee scrutiny, reputational concerns. Firms' DEI
  rhetoric is a downstream signal of these forces, not the source of the implicit
  injunctive norm itself. Cite Levi & Fried (2024).
- **Internal motivation to control prejudice**: personally held egalitarian beliefs,
  self-image as non-prejudiced; the pathway most directly supported by Study 4's
  private-feedback design.

Target wording: response letter line 98. Use the exact phrasings `"external motivation
to control prejudice"` and `"internal motivation to control prejudice"`, not
`"external pathway"` or `"internal pathway"`.

### Item 7 — Study 4 Discussion, political-ideology null + Kirgios sentence

Anchor: manuscript line 132 (`"In addition, Study 4B established that the effect of
gender feedback was not moderated by political ideology nor party affiliation.
Finally, it ruled out the possibility that our effects were driven by perceptions of
which attributes were considered most important."`).

Append a sentence (or two) interpreting the null as suggestive that descriptive
feedback operates across partisan lines, citing Kirgios et al. (2022) as corroborating
field-experiment evidence (response letter line 96 spells out the city-counselor
result mediated by motivation to control prejudice).

Plus: at the END of the new two-pathway paragraph from Item 6, add a one-sentence
bridge tying the Study 4B political-ideology null to both pathways generalizing across
the political spectrum.

### Item 8 — Introduction, current DEI socio-political landscape (NEW)

Anchor: manuscript line 15, replacing or substantially expanding the single existing
sentence `"And despite recent rollbacks of diversity, equity, and inclusion (DEI)
policies at some organizations in the United States (Heaton, 2025), recent surveys
indicate that the majority of Americans still hold favorable views about DEI (Minkin,
2024; Kidwai, 2025)."`.

The canonical replacement paragraph is response letter line 93. The new prose should:
1. Acknowledge fluctuating U.S. corporate posture toward DEI over the past five years
   (rapid adoption, then post-2024 scaling back).
2. Document three countervailing patterns that keep the paper's central question
   relevant:
   a. **International transparency**: Australia's Workplace Gender Equality Amendment
      (Closing the Gender Pay Gap) Act 2023 (Ressia, 2024); EU Pay Transparency
      Directive 2023/970 with member-state transposition by June 2026 (Mignano, 2024);
      EU Women on Boards Directive 2022/2381 (40% representation target).
   b. **U.S. public and stakeholder support**: 2025 Bentley/Gallup survey showing
      69% of U.S. adults believe businesses should promote DEI (Bentley/Gallup, 2025);
      anti-DEI shareholder proposals failing at Costco, Apple, Levi's, John Deere,
      Goldman Sachs, and others (CBS, 2025; Marketplace, 2025).
   c. **Firm-level rebranding**: Constellation Brands "Inclusive Culture Team"; Kohl's
      "Chief Inclusion and Belonging Officer"; Nationwide and UPS shifting toward
      "belonging," "respect," "fairness," "inclusive experiences" framings (CNBC,
      2025; HRGrapevine, 2025; Marketplace, 2026); Paradigm reports 22% decrease in
      Fortune 100 references to "DEI/diversity" between 2023 and 2024 alongside 59%
      increase in references to "belonging" (Paradigm, 2025).
3. End by reinforcing the paper's continued relevance: the contribution is not that
   feedback is a tool firms can pull off the shelf in any climate, but that
   descriptive feedback about past selection patterns can change subsequent decisions
   *when those patterns raise concerns about appearing prejudiced or failing to live
   up to one's own egalitarian standards*.

Length target: roughly 2-3 paragraphs (the existing single sentence is being expanded
into a fuller treatment). Match the manuscript's Introduction register, not the
response letter's defensive tone.

### Item 9 — Introduction, no-appropriate-level clarification (NEW)

Insert a few sentences in the Introduction (the precise location is up to you, but
the natural home is the theorizing paragraph at line 14, or immediately after the
"We propose…" paragraph at line 17). The sentences should make explicit:

1. Descriptive feedback shifts subsequent selections only on dimensions carrying an
   implicit injunctive norm that the feedback calls into question.
2. We make **no** assumption that participants' initial selections reflect an
   "appropriate" or normative benchmark for the comparison attributes (or for any
   attribute).
3. The contrast: gender and race carry such an injunctive norm; the comparison
   attributes in our studies (university, age, West Coast, genre, etc.) do not.

Canonical wording: response letter line 45.

### Item 10 — Study 4 Discussion, no-appropriate-level footnote (NEW; NOT Table R1)

Insert a footnote attached to manuscript line 132 (or wherever the no-appropriate-
level claim most naturally lands in Study 4 Discussion). Restate the no-assumption
claim in the local context of Study 4: we do not assume that participants' initial
selections reflect an appropriate level for the comparison attributes, and the effect
of gender or racial feedback in our paradigm is interpreted as evidence that those
dimensions carry an implicit injunctive norm, not that participants under-selected
them relative to some "correct" base rate.

This footnote is the companion to Item 9. It is NOT the Table R1 / zero-initial pooled
analysis footnote, which is out of scope for this pass.

### Item 11 — General Discussion, organizational DEI climate as scope condition (NEW)

Insert in Future Directions and Limitations (around line 175) a passage that
identifies organizational DEI climate as a scope condition for descriptive feedback:
the effect should be most likely to matter when past selection patterns raise active
concerns about prejudice, and less likely to matter in fully private settings or in
organizations that openly reject egalitarian norms.

This is one of the cleanest places to tie the Introduction's DEI-landscape paragraph
(Item 8) to the paper's theoretical claims. The Theoretical and Practical
Contributions section (line 170) is an alternative home; if you prefer it, justify
why in your "Why this wording" note.

Canonical wording: response letter lines 38 and 93 (final paragraph), plus the
two-pathway logic from Item 6.

### Item 12 — General Discussion, fairness-norms additional-mechanism mention (NEW)

Insert in Future Directions and Limitations (near line 175 or where naturally
appropriate) a brief acknowledgment that fairness norms and broader concerns about
appropriate representation may also drive descriptive-feedback effects on subsequent
selections. Study 5's mechanism analysis already supports this in the
overrepresentation case (fairness concerns drove participants to select men when women
were overrepresented); the new passage should generalize the point without
overstating it.

Canonical wording: response letter line 99.

### Item 13 — References (alphabetical insertion)

List the full APA-style entries for:

From items 1-7:
- Andor, M. A. (2020)
- Chen, X., Latham, & Piccolo (2020)
- Devine, P. G., & Ash, T. L. (2022)
- Henry, M. L. (2019)
- Itzchakov, G., & Latham, G. P. (2018)
- Kirgios, E. L. (2022) — already in the manuscript references (line 229), do NOT duplicate
- Levi, A., & Fried, Y. (2024)
- Locke, E. A., & Latham, G. P. (2019)
- Mertens, S. N., & Schultz, P. W. (2021)
- Schultz, P. W., Nolan, J. M., & Cialdini, R. B. (2018)

From item 8 (new DEI-landscape sources — format to match the manuscript's existing
Heaton 2025 / Minkin 2024 / Kidwai 2025 trade-press style at lines 222, 228, 240):
- Bentley/Gallup (2025) — survey report
- Ressia (2024) — academic chapter or law-firm note on the Australian WGEA amendment
- Mignano (2024) — law-firm or academic note on EU Pay Transparency Directive 2023/970
- EU Women on Boards Directive 2022/2381 — official EU citation
- CBS (2025) — anti-DEI shareholder proposals reporting
- Marketplace (2025; 2026) — anti-DEI shareholder proposals + rebranding reporting
- CNBC (2025) — DEI rebranding reporting
- HRGrapevine (2025) — DEI rebranding reporting
- Paradigm (2025) — Fortune 100 DEI/belonging analysis

For sources where the exact APA-format citation requires fact-finding you cannot do,
write the entry as `[FORMAT TBD: source X with information Y]` and flag it for JC.

Also flag where Devine & Ash (2022) should be cited in the manuscript body
(suggestion: Introduction normative-context paragraph at line 15, near the "Ellis,
2025" citation, as a multidisciplinary review of organizational diversity training).

## Out of scope — DO NOT draft

- **Study 4B Discussion footnote summarizing Table R1 / zero-initial pooled analysis.**
  JC is not yet settled on the level of analysis. Do not draft this.

That is the only out-of-scope item. The first pass also excluded the DEI socio-
political update — that item is now IN scope as Item 8.

## Voice constraints (NON-NEGOTIABLE)

- NO em dashes (U+2014) or en dashes (U+2013) anywhere. Use commas, parens, or full
  sentences.
- Use curly apostrophes (U+2019) and curly quotes (U+201C / U+201D), not straight
  ones.
- Theory term is "implicit injunctive norm" — NEVER write "latent" anywhere.
- Match JC's scholarly register: precise, conservative, no hedging clichés.

## Required output structure

Single markdown document at `revision-analysis/_r2_review/codex_second_pass.md` with
the following section headings, in this exact order (the wrapper script verifies their
presence):

```
## Item 1
## Item 2
## Item 3
## Item 4
## Item 5
## Item 6
## Item 7
## Item 8
## Item 9
## Item 10
## Item 11
## Item 12
## Item 13
## Self-Audit
```

Under each Item heading, give:
- A one-line locator (which anchor paragraph, with manuscript line number).
- The full proposed insert text or replacement text, in block-quote format so it reads
  as the actual manuscript-ready prose (no commentary inline).
- A 1-3 sentence "Why this wording" note after the block quote.
- For items where you are extending or modifying existing manuscript prose (Items 1,
  2, 5, 7), include both the CURRENT text and the PROPOSED text in separate block
  quotes so JC can diff visually.

Under "## Self-Audit", confirm:
- No em or en dashes anywhere in the deliverable.
- "latent" appears nowhere.
- All seven canonical NPR-pretest numbers from Item 4 match the spec above.
- Out-of-scope item (Table R1 footnote) was not drafted.
- All 13 items have a draft (Item 13 is the references list).
