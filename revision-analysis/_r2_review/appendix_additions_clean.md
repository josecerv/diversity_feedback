# Appendix Additions — clean .md for paste

Two new sections to add to `Appendix_Does FeedbackEnhanceDiversity.docx`. Source-of-truth values: canonical Qualtrics survey `SV_aeIi4xABcJiz1vU` (N = 298 after pre-registered attention-check exclusion) for Section A, and `de_zero_benchmark_results.csv` plus manuscript footnote 17 for Section B.

---

## Appendix Section A: NPR Attribute-Importance Pretest

In Study 1, we sought to rule out the possibility that the unique effect of descriptive feedback about the gender of past selectees on subsequent expert selections stemmed from participants perceiving gender as a more important attribute than the three comparison attributes featured in Study 1. To address this concern, we conducted a separate pre-registered, between-subjects pretest measuring the importance participants assigned to each of the four NPR expert attributes featured in Study 1.

### Methods

*Participants.* As pre-registered (https://aspredicted.org/xa7u94.pdf), we recruited 300 participants on Prolific (U.S.-based, English-speaking, with at least a 90% approval rate and at least 1,000 prior survey completions) to complete a brief survey for a standard Prolific payment. After applying the pre-registered attention-check exclusion, N = 298 participants remained in the analyzable sample.

*Procedure.* Participants were told to imagine they worked as producers at NPR and were responsible for identifying and booking expert sources to interview for various news segments and stories. Each participant was then randomly assigned with equal probability to one of four between-subjects conditions defined by which expert attribute they were asked to rate for importance: (1) the percentage of experts who were women, (2) the percentage of experts who were under 50 years old, (3) the percentage of experts who were based on the West Coast of the United States, or (4) the percentage of experts who worked at a university. Participants in each condition rated their assigned attribute on a single 7-point importance scale (1 = “Not at all important”, 7 = “Very important”). See Materials Section in the Appendix for the verbatim prompt.

### Results

Following our pre-registration, we conducted three two-sided Welch t-tests comparing the importance rating in the Gender condition (reference) against each of the three comparison conditions. Table A1 reports per-condition descriptive statistics and the pairwise tests against the Gender condition. Participants rated the importance of considering whether experts “worked at a university” as significantly higher than the importance of considering whether experts “were women” (M = 5.04, SD = 1.93 vs. M = 4.28, SD = 2.33; t(142.6) = 2.17, p = .032, d = 0.36). The importance of considering whether experts “were under 50 years old” did not differ significantly from the importance of considering whether experts “were women” (M = 4.49, SD = 2.17 vs. M = 4.28, SD = 2.33; t(147.2) = 0.58, p = .563, d = 0.09). Participants rated the importance of considering whether experts “were based on the West Coast of the United States” as significantly lower than the importance of considering whether experts “were women” (M = 3.35, SD = 2.17 vs. M = 4.28, SD = 2.33; t(146.5) = 2.51, p = .013, d = 0.41). Together, these results suggest that gender is not viewed as a uniquely important feature of NPR experts; two of the three comparison attributes (university affiliation and age) were rated as at least as important as gender, and one (geographic location) was rated as significantly less important. Differences in perceived attribute importance therefore cannot account for the unique impact of gender feedback observed in Study 1.

**Table A1.** Importance Ratings of the Four NPR Expert Attributes Featured in Study 1, with Pairwise Welch t-Tests vs. the Gender Condition (Reference)

| Attribute | N | M | SD | t (vs. gender) | df | p | Cohen’s d |
|---|---:|---:|---:|---:|---:|---:|---:|
| Were women (reference) | 75 | 4.28 | 2.33 | — | — | — | — |
| Worked at a university | 74 | 5.04 | 1.93 | 2.17 | 142.6 | .032* | 0.36 |
| Were under 50 years old | 75 | 4.49 | 2.17 | 0.58 | 147.2 | .563 | 0.09 |
| Were based on the West Coast of the United States | 74 | 3.35 | 2.17 | 2.51 | 146.5 | .013* | 0.41 |

*Notes.* Importance was measured on a single 7-point scale (1 = “Not at all important”, 7 = “Very important”). N = 298 (after pre-registered attention-check exclusion). t-statistics, df, and p-values are from two-sided Welch t-tests of importance rating in each comparison condition against the Gender (reference) condition. Reported t and d values are absolute; verbal direction (more / less important than gender) is given inline above. Cohen’s d is computed with the pooled standard deviation. + p < .10, * p < .05, ** p < .01, *** p < .001.

*Reproducibility.* The raw response data and analysis script that produced the values reported in Table A1 are archived alongside the manuscript materials (`Survey1_NPR_AttributeImportance_canonical_SV_aeIi4xABcJiz1vU.csv`; `r2_npr_pretest_analysis.py`).

---

## Appendix Section B: Pooled Zero-Baseline Analysis

A reader might worry that descriptive feedback about race or gender has larger effects on subsequent selections than descriptive feedback about other attributes simply because participants already use those non-race/gender attributes “appropriately” in their initial selections, leaving little room for feedback to move them further. To address this concern, we conducted a cross-study pooled analysis restricted to participants whose initial selection sets contained zero options exhibiting a given focal attribute. We treat this zero-initial subset as a common floor across attributes: if descriptive feedback influences subsequent selections simply by signaling under-representation on any dimension, then within this subset we should observe a comparable feedback effect whether the focal attribute is race, gender, or any other feature about which we provided feedback.

### Methods

We pooled participant-level data from Studies 2, 3A, 3B, and 4B. Study 4A is excluded because every comparison attribute featured in that study had near-saturated initial selection rates (over 90% of participants selected at least one author with each comparison attribute in their initial portfolio), leaving fewer than approximately 20 zero-initial participants per comparison cell — insufficient for a stable estimate.

Within each (study, attribute) cell, we restricted the sample to participants whose initial selection portfolio contained zero options exhibiting that attribute (e.g., a Study 3A participant whose seven initial films contained zero films released after 2010, or a Study 4B participant whose six initial books contained zero books with women authors). We then estimated a linear probability model regressing the binary final-selection outcome (whether the participant’s next selection exhibited the focal attribute) on a dummy indicator for assignment to the feedback condition relevant to that attribute. Standard errors are heteroskedasticity-robust (HC3). Where a participant contributed observations to multiple cells (because a participant in a given study could be in the zero-initial subset for multiple focal attributes), we clustered standard errors at the participant level in pooled regressions.

We then pooled cell-level estimates using inverse-variance weighting, separately for “target” cells (those in which the focal attribute was race or gender) and “comparison” cells (those in which the focal attribute was a non-race/gender feature, e.g., budget, year of release, page count, or protagonist profession). We compare the pooled target estimate against the pooled comparison estimate using a Wald test of equality.

### Results

Within the zero-initial subset, random assignment to receive descriptive feedback about race or gender significantly increased the likelihood of subsequently selecting an option exhibiting the focal attribute, by 22.50 percentage points on average (women cells: +27.59 pp; racial-minority cell: +9.77 pp; p < .001, N = 864 zero-initial participant-cell observations across Studies 2, 3A, 3B, and 4B). By contrast, within the zero-initial subset, random assignment to receive descriptive feedback about non-race/gender attributes did not significantly shift subsequent selections (pooled estimate: +7.25 pp, p = .19, N = 414 zero-initial participant-cell observations). A Wald test comparing the pooled target and pooled comparison estimates rejects equality (Δ = +15.25 pp, p = .016, 95% CI [+2.87, +27.63]). Per-cell estimates appear in Table B1.

**Table B1.** Per-Cell Linear Probability Model Estimates from the Cross-Study Pooled Zero-Baseline Analysis

| Study | Focal Attribute | Type | N (zero-init) | Effect (pp) | SE (pp) | 95% CI |
|---|---|---|---:|---:|---:|---|
| Study 2 | Female protagonist | Target | 63 | +32.66** | 11.03 | [+10.60, +54.72] |
| Study 3A | Racial-minority protagonist | Target | 247 | +9.77 | 5.98 | [−2.00, +21.54] |
| Study 3B | Woman protagonist | Target | 336 | +31.57*** | 4.91 | [+21.93, +41.22] |
| Study 4B | Woman author | Target | 218 | +20.01** | 6.43 | [+7.35, +32.68] |
| Study 2 | Featured an entertainer | Comparison | 66 | +21.30+ | 10.76 | [−0.21, +42.80] |
| Study 2 | Over 500 pages | Comparison | 144 | −1.79 | 9.25 | [−20.08, +16.50] |
| Study 3A | High budget | Comparison | 40 | +33.33 | 20.28 | [−7.73, +74.39] |
| Study 3B | High budget | Comparison | 61 | −9.52 | 16.51 | [−42.55, +23.51] |
| Study 3B | Political-leader protagonist | Comparison | 52 | +9.77 | 16.50 | [−23.36, +42.91] |
| Study 4B | Sold 30M+ copies | Comparison | 29 | +27.54 | 25.43 | [−24.64, +79.71] |
| **Pooled target vs. comparison contrast (Wald)** | **Δ = +15.25 pp** |  |  | **p = .016** |  | **[+2.87, +27.63]** |

*Notes.* Each row reports the estimated effect of random assignment to receive descriptive feedback about the focal attribute on the likelihood that the participant’s final selection exhibited that attribute, estimated from a linear probability model with HC3-robust standard errors. Estimates are reported in percentage points (pp). “Target” attributes are race or gender; “Comparison” attributes are non-race/gender features (e.g., budget, release year, page count, protagonist profession). The sample is restricted within each cell to participants whose initial selections contained zero options exhibiting the focal attribute. Cells with fewer than approximately 20 zero-initial participants are omitted. Study 4A is excluded because its comparison attributes had near-saturated initial selection rates (see Methods). The pooled target estimate (+22.50 pp; women cells: +27.59 pp; racial-minority cell: +9.77 pp; N = 864) and pooled comparison estimate (+7.25 pp; N = 414) are inverse-variance weighted across the cells above; the Wald test compares their equality. + p < .10, * p < .05, ** p < .01, *** p < .001.

### Discussion

This rule-out matters because the most plausible alternative explanation for the differential effect of race/gender feedback compared with feedback about other attributes is that participants already use those other attributes appropriately when making their initial selections, leaving little room for feedback to shift them further. The zero-baseline restriction forces a common floor across focal attributes: every participant in the zero-initial subset has just learned that their portfolio contained none of the focal attribute, a strong signal of under-representation on that dimension. Within this subset, feedback about the race or gender of past selectees still produces a sizable shift in subsequent selections, whereas feedback about other attributes does not. This differential cannot be attributed to baseline calibration of comparison attributes.

*Reproducibility.* The pooled estimates and per-cell coefficients reported here can be regenerated from `revision-analysis/_r1_review/de_zero_benchmark_long.csv` using the script `de_zero_benchmark_analysis.R`.
