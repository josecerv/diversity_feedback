# Citation verification audit (Codex pass)

You are running an independent verification pass on the 11 new academic citations
introduced in `revision-analysis/_r2_review/r2_manuscript_changes.md`. Claude is
running the same audit in parallel using the scite MCP. Your job is to cross-check
using web search and any other tools you have. Do NOT trust the existing reference
list at face value; verify against the original sources.

## Sources to verify

For each citation, web-search the title, DOI, and author list to confirm:
1. The paper actually exists and is published.
2. Authors, year, journal, volume, issue, pages, and DOI all match what we wrote.
3. The abstract or full text actually supports the way we are citing the paper in
   the manuscript change file (the claim we are using it to support).
4. The paper is NOT retracted, withdrawn, or under correction.

The 11 citations and their claimed roles in the manuscript:

1. **Locke, E. A., & Latham, G. P. (2019).** "The development of goal setting
   theory: A half century retrospective." *Motivation Science*, 5(2), 93-105.
   https://doi.org/10.1037/mot0000127
   Claim being supported: feedback regulates behavior change when paired with
   explicit goals (used as contemporary update to Kluger & DeNisi 1996 in both
   Introduction and General Discussion).

2. **Itzchakov, G., & Latham, G. P. (2018).** "The moderating effect of performance
   feedback and the mediating effect of self-set goals on the primed
   goal-performance relationship." *Applied Psychology*, 69(2), 379-414.
   https://doi.org/10.1111/apps.12176
   Claim: feedback paired with goals motivates behavior change. (NOTE: confirm
   whether this is *Applied Psychology* or *Applied Psychology: An International
   Review*; confirm volume 69(2) vs other possibilities.)

3. **Chen, X., Latham, G. P., & Piccolo, R. F. (2020).** "An enumerative review and
   a meta-analysis of primed goal effects on organizational behavior." *Applied
   Psychology*, 70(1), 216-253. https://doi.org/10.1111/apps.12239
   Claim: feedback paired with goals motivates behavior change. (NOTE: 2020 paper
   in a 2021 volume issue is plausible but worth flagging.)

4. **Schultz, P. W., Nolan, J. M., & Cialdini, R. B. (2018).** "The constructive,
   destructive, and reconstructive power of social norms: Reprise." *Perspectives
   on Psychological Science*, 13(2), 249-254.
   https://doi.org/10.1177/1745691617693325
   Claim: descriptive feedback paired with normative information produces reliable
   behavior change in conservation domains. (Reprise of the 2007 paper.)

5. **Andor, M. A., Gerster, A., & Peters, J. (2020).** "Social norms and energy
   conservation beyond the US." *Journal of Environmental Economics and
   Management*, 103, 102351. https://doi.org/10.1016/j.jeem.2020.102351
   Claim: applied feedback intervention in energy conservation, generalizes the
   Opower / Schultz finding beyond the US.

6. **Henry, M. L., Ferraro, P. J., & Kontoleon, A. (2019).** "The behavioural
   effect of electronic home energy reports: Evidence from a randomised field
   trial in the United States." *Energy Policy*, 132, 1256-1261.
   https://doi.org/10.1016/j.enpol.2019.06.039
   Claim: applied feedback intervention in household energy.

7. **Mertens, S. N., & Schultz, P. W. (2021).** "Referent group specificity:
   Optimizing normative feedback to increase residential recycling." *Journal of
   Environmental Psychology*, 73, 101541.
   https://doi.org/10.1016/j.jenvp.2020.101541
   Claim: applied feedback intervention in household recycling.

8. **Devine, P. G., & Ash, T. L. (2022).** "Diversity training goals, limitations,
   and promise: A review of the multidisciplinary literature." *Annual Review of
   Psychology*, 73(1), 403-429.
   https://doi.org/10.1146/annurev-psych-060221-122215
   Claim: multidisciplinary review of organizational diversity training, cited
   near the Ellis (2025) sentence in the Introduction's normative-context
   paragraph.

9. **Levi, A., & Fried, Y. (2024).** "Diversity, equity, and inclusion programs'
   emphasis on symbolism: Causes and consequences." *Journal of Organizational
   Behavior*, 46(1), 172-187. https://doi.org/10.1002/job.2834
   Claim: firms' public DEI rhetoric is a downstream signal of broader legal,
   employee, customer, and reputational pressures, not the source of the implicit
   injunctive norm itself. (NOTE: published 2024 in a 2025 volume issue is
   plausible; verify.)

10. **Ressia, S. (2024).** "Addressing gender inequality issues in Australia: An
    annual review of developments." *Journal of Industrial Relations*, 66(5),
    742-758. https://doi.org/10.1177/00221856241295490
    Claim: documents the Australian Workplace Gender Equality Amendment (Closing
    the Gender Pay Gap) Act 2023.

11. **Mignano, V. (2024).** "Gender Pay Gap: The Protection of the Right to Equal
    Pay under the Pay Transparency Directive." *Zeitschrift für Europarechtliche
    Studien*, 27(3), 371-401. https://doi.org/10.5771/1435-439X-2024-3-371
    Claim: documents the EU Pay Transparency Directive 2023/970 and member-state
    transposition deadline of June 2026.

## What to output

Write your audit report to `revision-analysis/_r2_review/codex_citation_audit.md`.

For each citation use this structure:

```
## Citation N: <author short form> (<year>)
- **Existence**: PASS / FAIL / UNCERTAIN
- **Metadata accuracy** (authors, year, journal, volume, issue, pages, DOI): PASS / FAIL / UNCERTAIN with specifics
- **Claim alignment** (does the abstract or full text actually support the use we made of it?): PASS / FAIL / UNCERTAIN with one-sentence justification quoting the abstract or a passage
- **Retraction / correction check**: NONE FOUND / FLAGGED (give details)
- **Recommendation**: KEEP AS IS / KEEP WITH FIX (specify) / DROP AND REPLACE (suggest replacement) / NEEDS HUMAN REVIEW
```

End with a `## Summary` section listing any citation flagged for fix, drop, or
human review.

## Voice constraints (same as other deliverables)

- NO em or en dashes. Use commas, parens, or full sentences.
- Curly apostrophes (U+2019) and curly quotes (U+201C/U+201D).
- Match the manuscript's scholarly register.

## Required output structure

The deliverable file must contain:

```
## Citation 1
## Citation 2
## Citation 3
## Citation 4
## Citation 5
## Citation 6
## Citation 7
## Citation 8
## Citation 9
## Citation 10
## Citation 11
## Summary
```

(14 section headings; the wrapper validates their presence.)
