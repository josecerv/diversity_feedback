# Codex brief: response-letter manuscript-fidelity audit (R2)

## Context for you (Codex)

JC has spent the last 24h iterating on the R2 response letter at
`response_letter_gdoc_text.txt` (synced from Gdoc
`1YBHCMwD84xC3Qw38CPEzz1iv3JKg7Mw9jTJqyZGptkI`). Several wording and DEI-rhetoric
changes were made and approved yesterday, but new decisions have surfaced today
that mean parts of the response letter no longer match the actual state of the
work or the upcoming plan:

1. JC is **adding a pre-registration** for the Study 1 attribute-importance
   post-test (`Prereg_NPR_AttributeImportance_draft.md`) and **waiting to
   collect that pre-registered data**. The existing pretest data
   (`Survey1_NPR_Attribute_Test_replication.csv`,
   `r2_pretest_results_option1.csv`) is exploratory/pilot, not pre-registered.
2. JC is contemplating **changing Study 1 methods** in a way that has not yet
   been specified. The current response letter commits to the existing Study 1
   methods (with a Discussion-section addition pointing to the post-test).
3. The R2 manuscript changes proposed in `r2_manuscript_changes.md` are still
   PROPOSED -- none of Changes 1-13 are in the manuscript Gdoc yet (see
   `manuscript_gdoc_text.txt`).

Claude is running a separate internal-consistency pass on the same letter.
Your job is the **manuscript-fidelity pass**: every factual claim, every
"we have done X" sentence in the response letter, every page reference,
every numerical result -- does it match what is actually in the manuscript
Gdoc right now?

## Inputs

- `response_letter_gdoc_text.txt` -- the current response letter (this is
  what you are auditing)
- `manuscript_gdoc_text.txt` -- the current state of the manuscript
- `Prereg_NPR_AttributeImportance_draft.md` -- the upcoming pre-registration
  (note: "No, no data have been collected for this study yet")
- `r2_pretest_results_option1.csv` -- analysis output of the existing
  exploratory pretest (M = 3.64, etc. -- the numbers the response letter
  reports come from here)
- `r2_manuscript_changes.md` -- the still-pending manuscript-side proposals
- `r2_gdoc_text.txt` -- earlier snapshot for diffing if useful

## What I want from you

Write a single Markdown file at
`revision-analysis/_r2_review/codex_response_letter_audit.md` with these
sections, in this order:

### 1. Overstatement audit
For each sentence in the response letter that asserts an action has been
taken ("we have done X", "we ran X", "we added X to p. X"), check whether
the manuscript Gdoc actually reflects that action. List every overstatement
in a table:
- response_letter_line | claim made | actual manuscript state | severity (high/med/low)

### 2. Numerical / claim audit
For every numerical claim, statistical result, or specific factual assertion
in the letter, verify it against the underlying source:
- The pretest M / SD / t / df / p / d values (line 41, line 44): do they
  match `r2_pretest_results_option1.csv`?
- The Study 1 / Study 4A / Study 4B / Table 2 references and claims: do they
  match `manuscript_gdoc_text.txt`?
- The cover-letter summary on line 15 says the comparison attributes were
  "rated as just as important as gender" but the detailed report on line 41
  says "Two of the three non-gender attributes were rated as significantly
  more important than gender." Which is correct given the CSV? Flag.
- The cover-letter summary on line 16 (and line 44's elaboration) says the
  zero-restriction pooled analysis EXCLUDED Study 4A. But the existing
  Table 2 footnote on manuscript line 383 says "Study 1 is excluded ... and
  Study 5 is excluded" -- and does NOT explain a Study 4A exclusion. Is the
  Table 2 footnote stale, or is the response letter wrong about which
  studies are excluded? Recommend an exact fix.

### 3. Mechanism terminology audit
The manuscript uses "motivation to respond without prejudice" consistently
(Plant & Devine, 1998 IMS/EMS scale terminology). The response letter mixes
"motivation to respond without prejudice" and "motivation to control
prejudice" -- sometimes within the same response (see line 76 + line 78).
List every occurrence of "control prejudice" in the response letter and
recommend the unified wording for each.

### 4. Reference / placeholder audit
List every:
- `[X]` placeholder (e.g., "[X] pages shorter")
- `p. X` / `pp. X-X` placeholder
- `[link to AsPredicted]` or similar
- `[URL pending verification]`
- `[FORMAT TBD]` reference entry in r2_manuscript_changes.md Change 13
- Reference cited in body text but not in the response-letter References
  section (lines 82-103), or vice versa
- Reference cited in the response letter that is NOT yet in the manuscript's
  reference section (manuscript_gdoc_text.txt around lines 210-260)

### 5. Tone / wording flags
- Line 73 calls the paper's central question "timeless." Reviewer 2's worry
  was that the paper feels "timestamped." Flag whether "timeless" overshoots
  in the opposite direction.
- Line 73 says firm DEI commitments are "only one small reason." Flag the
  word "small."
- Any em or en dashes drafted in JC's voice (JC's house style is no em/en
  dashes). DO NOT flag em/en dashes inside reviewer-quote blocks; only flag
  them in our response text.

### 6. New plan ramifications
JC has signaled two new directions:
- Adding a pre-registration and waiting on data (existing letter language
  treats the post-test as already complete with the stated numbers)
- Changing Study 1 methods (not yet specified)

For each, list which paragraphs of the response letter (and which sections
of the manuscript) will need to be rewritten depending on JC's final
decision, and which sentences could be drafted in two versions (current
state vs. post-pre-registered-data state) so JC can pick one when the data
arrive.

## Style and runtime rules

- The response-letter Gdoc may contain en-dashes / curly quotes from the
  Gdoc; do not flag those as errors. Only flag em/en dashes that originate
  in newly drafted JC-voice prose.
- Avoid the word "latent." The paper's theory uses "implicit injunctive
  norm."
- Keep your output to a single Markdown file. No code changes. No prose
  rewrites of the response letter -- just the audit.
- Confirm at the top of your output that the auditor read every line of
  `response_letter_gdoc_text.txt` (1-103) and every paragraph of
  `manuscript_gdoc_text.txt` referenced in the letter.
