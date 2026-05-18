**Subject:** Quick check on zero-benchmark version of Table R1

Hi Katy,

To address the AE's concern that comparison attributes might just reflect "appropriate use" in participants' initial selections, I reran Table R1 using zero as the benchmark instead of the candidate-pool base rate: among participants whose initial selection contained zero of attribute X, does receiving X-feedback shift their final pick? I pooled across Studies 2, 3A, 3B, 4A, and 4B with a study × attribute fixed-effects regression and participant-clustered standard errors.

**Target (race/gender) feedback at initial = 0:**
- Estimate: +21.28 pp
- p < .0001
- N = 1,222 across 5 study cells

**Comparison-attribute feedback at initial = 0:**
- Estimate: +6.83 pp
- p = .21
- N = 429 across 6 study cells (the other 9 comparison attributes have too few zero-initial participants to estimate, since most are majority-prevalence in their candidate pool)

**Target vs. Comparison contrast** (Wald test on the feedback × is_target interaction in the stacked initial-zero model, participant-clustered SEs):
- Estimate: +14.46 pp
- p = .016
- 95% CI [+2.70, +26.21]

So when feedback flags zero representation, race/gender feedback moves people about 21 pp because they have a normative target above zero, while the same zero framing does not reliably move them for the comparison attributes, and the differential is significant. Two questions for you: is this the right operationalization for the AE's comment, and does the comparison side covering only 6 of 15 attributes feel sufficient, or do we need to do more?

Thanks,
Jose
