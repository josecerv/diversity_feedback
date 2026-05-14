# Consistency Review of R2 Response Letter (READ-ONLY)

Write your findings to `revision-analysis/_r2_review/codex_consistency_pass.md` using `apply_patch`. Do NOT edit any other file.

## Context

The latest plain-text export of the response-letter Gdoc is at:
`revision-analysis/_r2_review/response_letter_gdoc_text.txt` (freshly pulled). Read it first.

Two edits were just made:
1. **Cover-letter Point 3 (around line 16)**: replaced placeholder `[Insert new description of analyses in R1 here]` with a 2-sentence summary of the zero-baseline pooled analysis.
2. **DE-response body (around line 44)**: dropped Study 4A from the pooled-analysis sentence (`"Studies 2, 3A, 3B, 4A, and 4B"` → `"Studies 2, 3A, 3B, and 4B"`) and added a one-line rationale ("no participants in 4A made initial selections containing zero of any of the comparison attributes...").

**Open issue you must flag, not fix:** the body still has OLD numerical estimates from a run that INCLUDED Study 4A: `21.28pp / N=1,222`, `6.83pp / N=429`, `CI [+2.70, +26.21]`. The refreshed no-4A numbers are `22.50pp / N=864`, `7.25pp / N=414`, `CI [+2.87, +27.63]`. The user is deciding whether to plug these in; flag the inconsistency, do not edit.

**Do NOT flag:** the many `(p. X)` and `(pp. X-X)` page-number placeholders — those are intentional and filled in last.

## What to check

For each numbered section, cite line numbers in `response_letter_gdoc_text.txt` and quote the offending text briefly. If a section has no issues, write `No issues found.` and move on. Don't pad.

### 1. Cover-letter Point 3 vs body description
Compare line ~16 (cover letter Point 3) with line ~44 (body description of the same analysis). Consistent in framing, scope, and stated conclusion? Flag mismatches.

### 2. Study 4A residual references
Search the whole letter for any lingering reference to Study 4A in the pooled / zero-baseline analysis context. Other 4A mentions (e.g., elsewhere in the letter unrelated to the pooled analysis) are fine — only flag if 4A is referenced inside the pooled-analysis discussion.

### 3. Numerical-estimate consistency
Explicitly state that keeping with-4A numbers (`21.28pp` / `N=1,222`; `6.83pp` / `N=429`; `CI [+2.70, +26.21]`) alongside the updated `"Studies 2, 3A, 3B, and 4B"` language is internally inconsistent and should be reconciled. Cite the line(s) where the stale numbers appear.

### 4. Other placeholders in line ~44
Identify `Study X`, `Study Z`, `XX`, `X, Y or Z`, and `movie book` (missing comma). Propose concrete fills using examples consistent with Studies 3A (films: budget, year of release, protagonist's profession) and 4B (books: protagonist gender). Do NOT propose fills involving any Study 4A attributes.

### 5. Style consistency
Flag violations of these conventions:
- No em-dashes (—) or en-dashes (–). Use commas, parentheses, or hyphens.
- Curly apostrophes (U+2019) and curly quotes (U+201C / U+201D), not straight.
- Use "implicit injunctive norm" — never "latent" anywhere in the letter.
- No callbacks like "as we discuss earlier" / "as noted in the Introduction".

### 6. Tone / voice consistency
Across the Cover-letter, DE response, R1 response, and R2 response sections.

### 7. Other internal inconsistencies
Especially: the R2 paragraph (around line 73) cites news/web sources by year — Ressia 2024, Mignano 2024, Bentley/Gallup 2025, CBS 2025, Marketplace 2025/2026, CNBC 2025, HRGrapevine 2025, Paradigm 2025 — but the references list (around lines 80-90) is academic-only. Flag the gap so the user can decide whether to add a separate web-sources list or convert to footnotes.

## Output format

Write the report to `revision-analysis/_r2_review/codex_consistency_pass.md` with EXACTLY these section headers (the wrapper validates their presence):

```
## 1. Cover-letter Point 3 vs body
## 2. Study 4A residual references
## 3. Numerical-estimate consistency
## 4. Other placeholders in line 44
## 5. Style consistency
## 6. Tone/voice consistency
## 7. Other internal inconsistencies
## Summary
```

The `## Summary` section is a 1-paragraph overall verdict on whether the letter is internally consistent and ready to submit pending the open items.

## Hard constraints

- READ-ONLY review. Edit ONLY the deliverable file.
- Be concise — bullets and short quotes, not prose paragraphs.
- Use line numbers from `response_letter_gdoc_text.txt`.
- Do NOT recompute statistics or modify any number.
- Do NOT touch the Gdoc.
