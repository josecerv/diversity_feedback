# Triangulated R2 fidelity audit (Claude × Codex)

Two independent stringent-reviewer passes were run on:
- `response_letter_gdoc_text.txt` (35,877 chars, current Gdoc)
- `manuscript_gdoc_text.txt` (142,586 chars, suggestion-inline view)
- `manuscript_pending_suggestions.json` (20 pending suggestions)

Source files:
- `claude_fidelity_audit.md` — Claude's pass (deeper cross-check between
  pending suggestions and the accepted prose surrounding them).
- `codex_fidelity_audit.md` — Codex's pass (strict matching of letter
  promises to pending suggestions only).

The two passes converge on the **headline gaps** but diverge on a few
borderline calls. Below is the triangulated finding set, sorted by what
JC's co-authors will actually want to see fixed before submission.

---

## Things both Claude and Codex flagged (CONFIRMED gaps)

### A. Zero-baseline pooled analysis is missing entirely (HIGH)

- Letter promises detailed stats for Studies 2, 3A, 3B, 4B pooled
  analysis restricted to participants with zero initial selections of a
  given attribute: race/gender effect = +22.50pp (p<.001, N=864); other
  attributes = +7.25pp (p=.19, N=414); Wald p=.016, CI [+2.87, +27.63].
- No matching insertion in pending tracked changes. No new Appendix
  section. No mention in Study 4 Discussion or General Discussion.
- Both audits flagged this as the single biggest fidelity failure.

**Fix:** Author the Study 4 Discussion sentences and the Appendix
section. The `de_zero_benchmark_*.R/csv/log` artifacts in
`revision-analysis/_r1_review/` already contain the underlying analysis
— promote them to a real manuscript-grade appendix section and add 2–3
sentences to the Study 4 Discussion. Suggested wording is in
`claude_fidelity_audit.md` Finding 1.

### B. Study 1 pretest stats are thinner in the manuscript than in the letter (MEDIUM)

- Letter (DE response §41) gives full results: Mean Importance per
  attribute, SDs, t-tests, p-values, effect sizes (e.g., "worked at a
  university: Mean Importance = 4.67, SD = 1.60; t(148.8) = −3.76,
  p < .001, d = −0.61").
- Manuscript pretest insertion (`suggest.tapkt1bva35d`, applied this
  session) only contains the high-level claim ("All three comparison
  attributes were rated as equally or more important than gender") and
  defers details to "Appendix Section S[X]" — a placeholder.
- **Codex caught this; Claude initially marked it ALIGNED.** On
  re-review, Codex is right: the letter promises specific numbers, the
  manuscript currently doesn't carry them, and "Appendix Section S[X]"
  is unresolved.

**Fix:** Two options.
- (i) Add the means / t-tests / effect sizes to the Appendix in a real
  numbered section (e.g., "Appendix Section S2: Study 1 Pretest of
  Attribute Importance") and replace `S[X]` in the Methods insertion
  with the real section number.
- (ii) Trim the response letter's detail to match the manuscript
  (less preferred — the editor explicitly asked for the post-test, so
  giving the stats in the letter and pointing to the appendix is the
  cleaner path).

### C. Soft timeless violation in General Discussion (LOW-MEDIUM)

- `suggest.yoyrvafrumd9` includes "Although many U.S. firms have
  **recently** pulled back from their public DEI rhetoric…"
- Both audits flagged this. Easy fix: drop "recently" and adjust to
  "When U.S. firms pull back from their public DEI rhetoric, such
  rhetoric is best understood as one downstream signal…"

### D. Streamlining claim is partly out of step with the manuscript (MEDIUM)

- Letter says "the Methods and Results sections of each study have been
  shortened" (cover §14) and "we have relegated the reporting of the
  regression specifications behind the secondary feedback-about-
  race/gender-versus-other-attribute comparisons to a dedicated section
  of the Appendix, with explicit references from **each Results
  section**" (DE §39).
- Studies 3A, 3B, 4A, 4B Results are clearly streamlined (one-paragraph
  reports with a "see Table S#, Models 5–8" reference).
- Study 1 Results (manuscript paragraph 49) still carries the full
  inline Zellner-systems description. The letter's claim of "each
  Results section" does not match.
- **Codex flagged this as MISSING (no pending suggestion shows the
  streamlining); Claude flagged it as PARTIAL (work is in accepted
  text, but Study 1 is the holdout).** On reflection: the streamlining
  of Studies 3A onwards IS visible in the accepted body, so the
  promise is partly fulfilled — but the letter overclaims to "each
  Results section." Triangulated verdict: **PARTIAL.**

**Fix:** Either (i) streamline Study 1 Results paragraph 49 to match
the Studies 3A onwards pattern, or (ii) soften the letter from "each
Results section" to "the later Results sections." (i) is cleaner.

### E. Two-pathway insertion is missing the "internal does not vary with climate" claim (MEDIUM)

- Letter (R2 §77) promises: "This second pathway should not vary with
  organizational climate, and it is the pathway most directly supported
  by Study 4 (a study in which feedback was delivered privately and
  internal motivation to respond without prejudice mediated subsequent
  selections)."
- Manuscript insertion `suggest.yoyrvafrumd9` introduces the
  internal/external distinction but does NOT include the explicit claim
  that internal motivation is invariant to organizational climate.
- The Study 4 / private / internal link is partly in paragraph 177 of
  the manuscript ("In Study 4, we found that the primary driver of the
  effect of feedback was to trigger internal motivation to respond
  without prejudice, which may be because the delivery of feedback was
  private") but the *climate-invariance* sentence is missing.
- **Codex caught this; Claude initially marked it ALIGNED.** Codex is
  right.

**Fix:** Add one sentence to `suggest.yoyrvafrumd9`, e.g., after the
existing "Internal motivation to respond without prejudice, by
contrast, reflects evaluators' own egalitarian commitments…":

> "Because internal motivation is grounded in evaluators' own values
> rather than organizational signals, this pathway should be largely
> invariant to fluctuations in an organization's DEI climate, and it is
> the pathway most directly supported by Study 4, where feedback was
> delivered privately and internal motivation mediated subsequent
> selections."

This is also the natural bridge to the "Organizational DEI climate is a
parallel scope condition" sentence (`suggest.26qurvd2dwno`) in Future
Directions a few paragraphs later.

---

## Divergences between Claude and Codex (CALL on each)

### Divergence 1: Year-specific regulatory citations in the Intro

- **Codex flagged TIMELESS-VIOLATION** for the new Intro insertion
  naming "Australia's Workplace Gender Equality Amendment (Closing the
  Gender Pay Gap) Act 2023", "EU Pay Transparency Directive 2023/970",
  "EU Women on Boards Directive 2022/2381", and "by June 2026"
  transposition.
- **Claude judged these acceptable** because the years are *names of
  laws* and *inherent features of directives* (a directive's
  transposition deadline is a structural fact, not a time-stamp on
  prose). Removing the years would force vaguer citations
  ("Australia's gender-pay law") that hurt scholarly precision.
- **Triangulated call: Claude's view stands.** The reviewer's
  concern was that *narrative* prose would feel pre-2024-stamped; the
  fix the manuscript made is to anchor the discussion in **durable
  regulatory frameworks** that exist regardless of U.S. political
  weather. That is exactly what an evergreen pivot looks like. Year-
  embedded statute names are conventional in legal/policy citations
  (cf. "Civil Rights Act of 1964" already in the paper) and reading
  them as time-stamps is over-reading.
- **Caveat to send to co-authors:** if anyone disagrees, the easy
  rewrite is to drop "by June 2026" and the directive numbers (keep the
  Australian Act name as a single anchor), but Claude's view is that the
  current text is fine.

### Divergence 2: Streamlining promise (P1)

- Codex flagged as MISSING; Claude flagged as PARTIAL. Triangulated
  above to **PARTIAL** — see Finding D.

### Divergence 3: Letter examples that don't appear in the manuscript

- Codex noted that the letter §72 names specific companies (Costco,
  Apple, Levi's, Constellation Brands, Kohl's, Nationwide, UPS) and
  cites Bentley/Gallup, Paradigm, CNBC, HRGrapevine — none of which
  appear in manuscript insertions. Codex coded this as part of the
  TIMELESS-VIOLATION.
- **Claude's view: this is fine.** Response letters can — and should —
  carry more detail than the evergreen manuscript. The reviewer asked
  the *manuscript* to read as timeless; the letter is JC explaining to
  the reviewer the empirical basis for the rewrite. Mismatch between
  the letter's specifics and the manuscript's generality is the *point*
  of the rewrite.
- **Triangulated call: Claude's view stands.** Co-authors will likely
  agree — letters routinely contain more receipts than the manuscript
  itself.

### Divergence 4: Table 2 note deletion (`suggest.th0c0nlaxdr`)

- Codex flagged as OVER-REACH (no clear letter promise).
- Claude flagged as ALIGNED with the §39 streamlining direction
  (regression-specs relegation).
- **Triangulated call: this is borderline; flag to JC for verification.**
  The deletion removes the note "Study 1 is excluded because participants
  did not generate their own initial selections… and Study 5 is excluded
  because its design varies the initial candidate pool's composition…".
  If that explanatory note has been moved elsewhere in the table or
  appendix, the deletion is fine. If it hasn't been moved, readers will
  wonder why Studies 1 and 5 are absent from Table 2 (or whatever the
  containing table is). JC: please verify the rationale stays visible.

---

## Things Codex raised that Claude did not (worth a glance)

- Codex's flag P10 on the "incorporated the result into the Methods
  section of Study 1" — same issue as Finding B above (pretest stats
  thinner in manuscript than letter). Already triangulated.
- Codex's flag P11 on the zero-baseline follow-up — same as Finding A.

## Things Claude raised that Codex did not (worth a glance)

- **Placeholder hygiene** (`[X] pages`, `pp. X-X`, `Appendix Section
  S[X]`, `[link to AsPredicted]`, `Marketplace. (2026). [URL pending
  verification]`, `Paradigm. (2025). [URL pending verification]`).
  These are not fidelity violations but co-authors will notice them
  immediately and you don't want the response-letter draft circulated
  with placeholders. See `claude_fidelity_audit.md` Finding 4 for the
  full list.

---

## Action list for JC, in priority order

1. **Add the zero-baseline pooled analysis** to (a) Study 4 Discussion
   (2-3 sentences) and (b) a new Appendix Section. Use the
   `de_zero_benchmark_*.R/csv/log` artifacts as the data backbone.
   (Triangulated Finding A.)
2. **Add the Study 1 pretest descriptive stats** to a real Appendix
   section and replace "Appendix Section S[X]" in the Methods
   insertion with the resolved number. (Triangulated Finding B.)
3. **Drop "recently" from `suggest.yoyrvafrumd9`** so the GD pathway
   paragraph reads as evergreen. (Triangulated Finding C.)
4. **Streamline Study 1 Results paragraph 49** to match the Studies
   3A onwards pattern, OR soften the letter's "each Results section"
   to "the later Results sections." (Triangulated Finding D.)
5. **Add the climate-invariance + Study 4 sentence** to
   `suggest.yoyrvafrumd9` per the suggested wording above.
   (Triangulated Finding E.)
6. **Resolve the placeholder set** (`[X]`, `pp. X`, `S[X]`, etc.)
   throughout the letter before circulation. (Claude Finding 4.)
7. **Verify the Table 2 note deletion** still has a home elsewhere in
   the doc; if not, restore the note. (Divergence 4.)

---

## Confidence statement (for co-author cover note)

Two independent stringent-reviewer passes (Claude Opus 4.7 with full
access to the Gdocs + structured suggestion index; Codex GPT-5.5 Pro
with the same inputs but no shared notes) were run on the response
letter and manuscript. Both passes converged on the same headline gap
(missing zero-baseline pooled analysis) and the same soft timelessness
issue (the "recently pulled back" sentence). They diverged on whether
year-embedded statute names count as time-stamps (Claude argued they
don't, citing the Civil Rights Act of 1964 precedent already in the
paper) and on whether the streamlining claim is missing vs. partial
(triangulated to partial — the work is in the accepted body for
Studies 3A onwards, but Study 1 is the holdout). The set of pending
tracked changes is otherwise tightly scoped — no over-reaches detected
outside the Table 2 note flag, which JC should verify.
