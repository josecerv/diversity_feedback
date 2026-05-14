# Codex response-letter manuscript-fidelity audit

Confirmed: I read every line of `response_letter_gdoc_text.txt` (1-103) and checked every paragraph of `manuscript_gdoc_text.txt` referenced or implicated by the response letter, including the Introduction (lines 7-17), Study 1 (lines 35-50), Studies 4A/4B and Study 4 Discussion (lines 97-132), Study 5 and General Discussion (lines 133-180), manuscript References (lines 181-263), Table 1 (lines 266-278), Table 2 and table notes (lines 281-299 and 363-383), and figure notes (lines 386-397).

## 1. Overstatement audit

| response_letter_line | claim made | actual manuscript state | severity |
|---|---|---|---|
| 14 | The empirical sections have been streamlined, the manuscript is `[X]` pages shorter, and Methods/Results sections have been shortened. | The current manuscript export does not establish a page-count reduction, and the page count is still a placeholder. Main-text Results sections still contain detailed regression reporting and secondary comparison tests, e.g., Study 1 line 48, Study 3B line 91, Study 4A line 110, and Study 4B line 126. | High |
| 15 | A new pre-registered Study 1 attribute-importance study was run with N = 300. | `Prereg_NPR_AttributeImportance_draft.md` says no data have been collected yet. The existing numbers come from the exploratory/pilot CSV, not a completed pre-registered study. The current manuscript has no Study 1 post-test text. | High |
| 15 | The Study 1 comparison attributes were "rated as just as important as gender." | The exploratory CSV shows two non-gender attributes were rated significantly more important than gender, while West Coast was not significantly different by Welch test. The planned non-inferiority test in the preregistration has not been run on pre-registered data. | High |
| 16 | A new zero-baseline pooled analysis was conducted and is part of the revision. | The current manuscript does not contain this analysis, its estimates, a discussion paragraph, or an appendix section. The only related manuscript analysis is the continuous initial-proportion interaction analysis in Study 5 setup (lines 135-139). | High |
| 17 | The Introduction and Discussion have been updated to discuss the current DEI landscape in the U.S. and abroad. | The current Introduction has only the older one-sentence rollback/favorable-views note (line 15). The General Discussion does not contain the claimed updated DEI landscape discussion. | High |
| 18 | The theoretical framing references have been modernized in the Introduction and General Discussion. | The current manuscript still cites the older feedback/social-norms anchors in the relevant places, e.g., lines 9-11 and line 170. The new references listed in the response letter are not in the manuscript reference section. | High |
| 34 | The Introduction and General Discussion now draw on Locke & Latham (2019), Itzchakov & Latham (2018), Chen et al. (2020), Schultz et al. (2018), Devine & Ash (2022), Andor et al. (2020), Henry et al. (2019), and Mertens & Schultz (2021). | None of these references appear in the current manuscript body or reference section. | High |
| 34 | The Introduction's theorizing has been expanded on pp. X-X to explain the implicit injunctive norm logic. | The current Introduction already has older implicit-injunctive-norm framing (lines 11, 14, 17), but it does not contain the new "standard already entrenched" expansion or the new citation block. | Medium |
| 36 | The timestamped-DEI issue has been addressed in the Introduction and General Discussion. | The current manuscript has no detailed international/global DEI update and no General Discussion update on this point. | High |
| 37 | The revised manuscript clarifies DEI pullbacks as one source of pressure, adds other reasons race/gender remain consequential, and identifies organizational DEI climate as a future-research moderator. | These points are proposed in `r2_manuscript_changes.md` but are not in `manuscript_gdoc_text.txt`. The current Future Directions section (lines 173-178) does not mention organizational DEI climate. | High |
| 39 | Regression specifications behind secondary race/gender-vs-other-attribute comparisons have been relegated to a dedicated Appendix section with explicit Results-section references. | The current manuscript still reports several secondary comparison tests in the main text, and no new dedicated appendix section appears in the manuscript export. | High |
| 41 | The Study 1 post-test result has been incorporated into the Study 1 Methods section. | Study 1 Methods and Discussion (lines 38-50) contain no post-test. `r2_manuscript_changes.md` Change 4 proposes a brief Study 1 Discussion sentence, not a Methods-section insertion. | High |
| 41 | The new study was pre-registered and recruited 300 Prolific participants. | Contradicted by the preregistration draft, which states no data have been collected. The exploratory raw CSV contains 302 observations, not 300 completed observations. | High |
| 43 | The manuscript now discusses the "appropriate initial selections" possibility and includes a follow-up analysis addressing it. | The current manuscript has the older Study 4B importance-pretest discussion (lines 98, 132) and the continuous initial-proportion analysis (lines 135-139), but no zero-baseline follow-up analysis or new local discussion of the "appropriate" benchmark issue. | High |
| 44 | The manuscript has added several Study 4 Discussion sentences plus a new appendix section describing the zero-baseline pooled analysis. | Not present. `r2_manuscript_changes.md` line 11 explicitly says the zero-initial pooled analysis footnote was out of scope/deferred. | High |
| 60 | The manuscript now cites Locke & Latham (2019) and Schultz et al. (2018) alongside the classic sources. | The current manuscript still cites Locke & Latham (2002) and Schultz et al. (2007), but not the 2019/2018 additions. | High |
| 62 | The Introduction's feedback/goals and applied social-norms citations have been refreshed. | The named new citations are absent from the current Introduction and References. | High |
| 63 | The theoretical framing has been expanded on pp. X-X with the standard-against-which-performance-is-evaluated logic. | The current manuscript contains related but older language; it does not contain the specific expansion claimed here. | Medium |
| 72-73 | The manuscript now contains the detailed DEI landscape update, including international regulation, U.S. public sentiment, shareholder proposals, rebranding, and Paradigm figures. | None of this detailed material is in the current manuscript. The only current DEI-landscape text is manuscript line 15. | High |
| 77-78 | The revised General Discussion now sharpens the theoretical contribution and explicitly distinguishes two pathways. | Current GD line 170 mentions internal and external motivation to respond without prejudice, but it does not include the claimed two-pathway explanation or organizational-climate scope discussion. | Medium |
| 79 | The manuscript now notes future research on fairness norms and other plausible mechanisms. | Current Study 5 and Future Directions text discusses fairness concerns (lines 155, 158, 175), but the proposed broader future-research note is not present. | Medium |

## 2. Numerical / claim audit

| response_letter_line | numerical or factual claim | source check | audit finding |
|---|---|---|---|
| 41 | Gender importance: M = 3.64, SD = 1.76, n = 76. | `r2_pretest_results_option1.csv`: M = 3.6447, SD = 1.7565, n = 76. | Matches after rounding. Not in current manuscript. |
| 41 | Worked at a university: M = 4.67, SD = 1.60; t(148.8) = -3.76, p < .001, d = -0.61. | CSV: M = 4.6711, SD = 1.6032, Welch t = -3.7622, df = 148.7661, p = .00024, d = -0.6103. | Matches after rounding. Not in current manuscript. |
| 41 | Under 50: M = 4.44, SD = 1.73; t(149.0) = -2.81, p = .006, d = -0.46. | CSV: M = 4.44, SD = 1.7261, Welch t = -2.8060, df = 148.9974, p = .00569, d = -0.4567. | Matches after rounding. Not in current manuscript. |
| 41 | West Coast: M = 3.37, SD = 1.75; t(149.0) = 0.95, p = .343, d = +0.16. | CSV: M = 3.3733, SD = 1.7458, Welch t = 0.9522, df = 148.9922, p = .3425, d = +0.1550. | Matches after rounding. Not in current manuscript. |
| 41 | Choice of Study 1 pretest output. | The reported values match `r2_pretest_results_option1.csv`, not `r2_pretest_results_option2.csv` (where gender M = 2.29 and all three comparison attributes are significantly higher). | The letter is using Option 1. That is internally clear from the numbers, but it should be intentional because Option 2 would support a different textual summary. |
| 15, 41 | N = 300 for the Study 1 post-test. | `Survey1_NPR_Attribute_Test_replication.csv` has 302 observations: 76 women, 75 under 50, 75 West Coast, 76 university. The preregistration plans to recruit 300 but says no data are collected yet. | The completed exploratory sample is N = 302, while the future preregistered target is N = 300. Do not report N = 300 as completed data unless this is describing the planned preregistered sample. |
| 15 vs. 41 | Cover-letter summary says comparison attributes were "just as important as gender"; detailed response says two were significantly more important and one did not differ. | The CSV supports the detailed line 41 description by Welch tests. It does not support a blanket equivalence claim. West Coast did not differ significantly from gender, but the preregistered non-inferiority logic has not been satisfied by completed preregistered data. | Line 15 is the inaccurate summary. Use "equally or more important" only if the final preregistered data support that claim; otherwise describe the two-more-important/one-not-significantly-different pattern. |
| 41 | The post-test was "pre-registered." | `Prereg_NPR_AttributeImportance_draft.md` line 3 says no data have been collected. | Contradicted as a completed-data claim. The existing statistics are exploratory/pilot. |
| 41 | Study 1 involved feedback about a portfolio of 10 NPR experts selected for future-of-work stories. | Manuscript lines 37-42 support the Study 1 setup: imagined NPR producers, future-of-work stories, portfolio of ten experts, and feedback about gender, age, West Coast, and university affiliation. | Supported, apart from the post-test insertion claim. |
| 44 | The zero-baseline pooled analysis uses Studies 2, 3A, 3B, and 4B and excludes 4A because no comparison-attribute zero cells exist. | Current manuscript does not report this analysis or this exclusion rationale. `r2_manuscript_changes.md` line 11 says this item was deferred. | Not supported by current manuscript. |
| 44 | Zero-baseline effect estimates: +22.50 percentage points, p < .001, N = 864; comparison effect +7.25 points, p = .19, N = 414; Wald p = .016, 95% CI [+2.87, +27.63]. | These values do not appear in `manuscript_gdoc_text.txt`. They also are not drafted into `r2_manuscript_changes.md`. | The numbers may be intended updated no-4A values, but the current manuscript does not contain them. Do not say "we have added" until the manuscript and appendix actually include them. |
| 44 | "Compared to the U.S. population." | The current manuscript describes low representation and "less than half women or racial minorities" (line 134), but it does not say the feedback compared selections to the U.S. population. | Unsupported by current manuscript. |
| 44 | "A set of books in Study 4B that included 0 books with women protagonists." | Study 4B is about authors and books by women authors, not books with women protagonists (manuscript lines 114, 119, 125-126; Table 2 line 380). | Contradicted. This should refer to women authors, not women protagonists. |
| 76 | Study 4 political ideology did not moderate the effect. | Manuscript line 129 reports no significant interaction with political ideology or party affiliation. | Supported. |
| 76 | Kirgios et al. (2022) provides parallel evidence involving Republicans and Democrats and motivation to control/respond without prejudice. | The manuscript currently cites Kirgios et al. only generally in line 16 and includes the full reference at line 229. It does not contain the Republican/Democrat bridge or this mechanism comparison. | Fine for response-letter explanation if cited, but not yet a manuscript addition. Also terminology should be unified to "motivation to respond without prejudice." |
| 98, 114, 383 manuscript context | Study 4B comparison attributes were pre-tested and rated at least as important as gender/race. | Manuscript lines 98, 114, and 383 support this for Study 4B. | Supported for Study 4B only. It does not support the new Study 1 post-test claim. |
| 383 manuscript Table 2 note vs. line 44 response | Response says the zero-restriction pooled analysis excludes Study 4A. Table 2 note says Study 1 and Study 5 are excluded, with no Study 4A rationale. | The current Table 2 note is not a zero-baseline-analysis note and does not match the line 44 response. | The response is not supported by the current manuscript. If the zero-baseline analysis is added, use a separate note: "The zero-baseline pooled analysis includes Studies 2, 3A, 3B, and 4B. Study 1 is excluded because participants did not generate their own initial selections; Study 5 is excluded because the design varies candidate-pool composition; Study 4A is excluded because no participants made initial selections containing zero of any comparison attribute, leaving no comparable zero-baseline comparison cell." |

## 3. Mechanism terminology audit

The manuscript consistently uses "motivation to respond without prejudice." The response letter should use that wording throughout.

| response_letter_line | occurrence | recommended unified wording |
|---|---|---|
| 76 | "the underlying mechanism identified was a motivation to control prejudice" | "the underlying mechanism identified was motivation to respond without prejudice" |
| 76 | "political ideology does not seem to moderate motivation to control prejudice" | "political ideology does not seem to moderate motivation to respond without prejudice" |
| 78 | "the external motivation to control prejudice" | "external motivation to respond without prejudice" |
| 78 | "internal motivation to control prejudice" | "internal motivation to respond without prejudice" |

Related note: `r2_manuscript_changes.md` line 183 also uses "motivation to control prejudice" in a proposed Study 4B sentence. If that sentence is migrated into the response letter or manuscript, update it too.

## 4. Reference / placeholder audit

### Response-letter placeholders

| line | placeholder/reference issue |
|---|---|
| 2 | `May X, 2026` date placeholder. |
| 14 | `[X] pages shorter` placeholder. |
| 34 | `pp. X-X` placeholder. |
| 36 | `pp. X` for Introduction and `pp. X` for General Discussion. |
| 39 | `[X] pages` placeholder. |
| 41 | `p. X` placeholder for Study 1 insertion. |
| 41 | `[link to AsPredicted]` placeholder. |
| 43 | `p. X` placeholder. |
| 44 | `p. X` placeholder for Study 4 Discussion. |
| 63 | `pp. X-X` placeholder. |
| 72 | `pp. X` placeholder. |
| 77 | `pp. X` placeholder. |
| 98 | Bracketed reference description and `[URL pending verification]` for Marketplace (2026). |
| 101 | Bracketed reference description and `[URL pending verification]` for Paradigm (2025). |

### Proposed-manuscript placeholders in `r2_manuscript_changes.md`

| line | placeholder/reference issue |
|---|---|
| 75 | Response-letter proposed text still has `pp. X-X`. |
| 99 | Response-letter proposed text still has `p. X`. |
| 109 | Proposed Study 1 sentence has `Appendix Section S[X]`. |
| 193 | Proposed response text has `pp. X`. |
| 226 | Proposed response text has two `p. X` placeholders. |
| 250 | Proposed response text has two `p. X` placeholders. |
| 259 | Proposed footnote has `Introduction (p. X)`. |
| 263 | Note repeats `Introduction (p. X)`. |
| 367 | `[FORMAT TBD: Marketplace (2026) ...]` reference entry. |
| 369 | `[FORMAT TBD: Paradigm (2025) ...]` reference entry. |

Additional manuscript placeholder observed: manuscript line 91 still says `Appendix Table/Section X`.

### Response-letter body citations missing from response-letter References

| body line | cited in body | status |
|---|---|---|
| 63 | Plant & Devine (1998) | Missing from response-letter References, though present in manuscript References. |
| 63 | Crandall & Eshleman (2003) | Missing from response-letter References, though present in manuscript References. |
| 63 | Alvarez-Benjumea (2023) | Missing from response-letter References, though present in manuscript References. |
| 76 | Kirgios et al. (2022) | Missing from response-letter References, though present in manuscript References. |

Citation-key consistency issues in the response body: line 73 cites `(CBS, 2025)`, `(CNBC, 2025)`, and `(HRGrapevine, 2025)`, while the References entries are `CBS News (2025)`, `Elias & Palmer (2025)`, and `Dungan (2025)`. Line 73 also uses `Bentley/Gallup (2025)` while the entry is `Bentley University & Gallup (2025)`.

### Response-letter References not cited in response body

| reference line | reference | status |
|---|---|---|
| 95 | Levi & Fried (2024) | Listed in response-letter References but not cited in the response-letter body. It appears in `r2_manuscript_changes.md` Change 6, not in the current response text. |

### Response-letter References not yet in manuscript References

The following response-letter references are not in `manuscript_gdoc_text.txt` lines 181-263 and must be added to the manuscript References if the corresponding body citations are added to the manuscript:

Andor et al. (2020); Bentley University & Gallup (2025); CBS News (2025); Chen et al. (2020); Devine & Ash (2022); Directive (EU) 2022/2381; Directive (EU) 2023/970; Dungan (2025); Elias & Palmer (2025); Henry et al. (2019); Itzchakov & Latham (2018); Levi & Fried (2024); Locke & Latham (2019); Marketplace (2025); Marketplace (2026); Mertens & Schultz (2021); Mignano (2024); Paradigm (2025); Ressia (2024); Schultz et al. (2018).

## 5. Tone / wording flags

| response_letter_line | wording | flag |
|---|---|---|
| 73 | "the paper's central question remains timeless" | Yes. Reviewer 2 worried the paper felt timestamped. "Timeless" overshoots in the opposite direction and may sound dismissive of the reviewer's point. |
| 73 | "organizational commitments are only one small reason" | Yes. "Small" minimizes the force of organizational DEI commitments too much, especially because the paper motivates descriptive feedback partly through organizational practices. |
| 36 | "trends.." | Minor cleanup: double period. |
| JC-response text overall | Em/en dashes | I found no em or en dashes in JC-voice response text that need flagging. The dash marks found by search are in the editor/reviewer quoted text (lines 31, 40, 56, 71) or are ordinary hyphens in response prose. |

## 6. New plan ramifications

### Adding the pre-registration and waiting on data

Response-letter paragraphs that need alternate versions:

| response-letter location | why it needs revision |
|---|---|
| Line 15 cover-letter summary | It currently says the new pre-registered study was run and completed. A current-state version should say only that the study has been pre-registered and data collection is pending. A post-data version can report the final N, AsPredicted link, and results. |
| Line 41 detailed DE response | Almost the entire paragraph depends on whether preregistered data have been collected. The existing exploratory statistics should either be removed, labeled as exploratory/pilot, or replaced with final preregistered results once available. |
| Lines 98 and 101 references | Marketplace/Paradigm placeholders are unrelated to the preregistration, but the reference section will also need the AsPredicted link and any appendix cross-reference filled if the completed study is reported. |

Manuscript sections affected:

| manuscript section | required decision |
|---|---|
| Study 1 Methods/Results/Discussion, lines 35-50 | Decide whether the post-test belongs in Methods, Discussion, or Appendix. Current `r2_manuscript_changes.md` proposes a brief Discussion sentence, not a Methods insertion. |
| Study 1 table/figure notes, lines 281-299 and 392-394 | If the post-test is used to interpret Study 1, add an appendix/table-note cross-reference as needed. |
| Manuscript References, lines 181-263 | Add any preregistration or method references needed for the final post-test report. |
| Appendix | Add the full post-test procedure/results only after the preregistered data exist, or label current material as exploratory/pilot. |

Sentences that should be drafted in two versions:

| current-state version needed | post-preregistered-data version needed |
|---|---|
| Line 15: study has been pre-registered; no completed results yet. | Line 15: completed preregistered post-test with final N and final conclusion. |
| Line 41: planned preregistered design and rationale, without completed statistics. | Line 41: completed design, AsPredicted link, final M/SD/test results, and exact manuscript location. |
| Study 1 manuscript insertion | Current: no manuscript claim, or a note that a preregistered post-test is planned if appropriate. Post-data: concise Discussion or Appendix cross-reference reporting final results. |

### Changing Study 1 methods

Response-letter paragraphs that would need rewriting:

| response-letter location | dependency |
|---|---|
| Line 15 | Any change to Study 1 methods changes the cover-letter description of the post-test and possibly whether the first study remains as currently framed. |
| Line 41 | The paragraph describes the current Study 1 setup and the post-test as addressing it. If Study 1 is rerun or redesigned, the paragraph must describe the new methods and avoid relying on old Study 1 results. |
| Lines 14 and 39 | Streamlining/page-count claims may change if Study 1 methods/results are rewritten or expanded. |
| Lines 16, 43, 44 | The zero-baseline analysis may need different framing if Study 1 changes affect the paper's first-study rationale or the scope of comparison-attribute claims. |

Manuscript sections affected:

| manuscript section | likely required changes |
|---|---|
| Abstract line 4 | Total N, study count, and summary claims may change if Study 1 is rerun or replaced. |
| Overview/Summary lines 19, 28-33 | Any Study 1 design change affects the summary of the research package. |
| Study 1 lines 35-50 | Methods, results, and discussion would need full revision. |
| Table 1 line 269 | Study 1 design and key findings would need revision. |
| Table 2 lines 281-299 and 363-367 | Study 1 coefficients, table notes, and figure/table cross-references may need revision. |
| Figure 3 notes lines 392-394 | Any Study 1 outcome or feedback-target change affects the figure note. |
| References and Appendix | Update preregistration links, materials, analysis code, and any post-test appendix section. |

Sentences that should be drafted in two versions:

| retain-current-Study-1 version | changed-Study-1 version |
|---|---|
| "We retained the Study 1 paradigm and added a preregistered attribute-importance post-test..." | "We revised/reran Study 1 so that the comparison attributes directly address the editor's concern..." |
| "The post-test helps rule out perceived importance as an alternative explanation for the existing Study 1 pattern." | "The revised Study 1 methods were designed to remove the perceived-importance concern directly, with the post-test serving as a validation check if still needed." |
| "Study 1 Methods/Discussion now include a cross-reference to the post-test." | "Study 1 Methods, Results, tables, figures, preregistration, and appendix materials have been updated to reflect the revised design." |
