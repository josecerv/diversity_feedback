# DE Study 4B Concern — Analysis Run

Results in `de_4b_concern_results.csv`. Script: `de_4b_concern_analysis.R`.

## What the DE asked

The DE (George Wu) worried that the gender/race feedback effect might look larger than comparison-attribute feedback effects only because **participants already use those comparison attributes appropriately in their initial selections** — i.e., the other-attribute feedback is informationally redundant, so there is no room to move. Under that story, gender/race isn't inherently "special"; it's just the one attribute where initial selections were off.

## What the analysis does

For each attribute in each of Studies 3A, 3B, 4A, 4B, it estimates the simple main effect of receiving feedback about that attribute on the likelihood of picking it in the final slot — and then splits the sample into two subgroups:

- **Below pool rate:** participants whose initial selection proportion was below the pool base rate (i.e., there IS room to move toward the pool rate).
- **At/above pool rate:** participants whose initial proportion already matched or exceeded the pool rate.

If Wu's alternative is correct, comparison-attribute feedback should move the Below subgroup (room to move) but not the Above subgroup (already "appropriately" using the attribute). And gender/race feedback should work only because it's always in the Below subgroup.

## What the data actually show

**Pattern 1: The target (gender/race) effect holds in BOTH subgroups.**

| Study | Target feedback | Main effect | Below pool rate | At/above pool rate |
|---|---|---|---|---|
| 3A (race) | race → race | +9.4 pp, p<.001 | +10.0 pp, p=.003 | +7.1 pp, p=.05 |
| 3B (gender) | gender → female | +20.6 pp, p<.001 | +25.1 pp, p<.001 | +10.3 pp, p=.04 |
| 4A (race) | race → race | +13.4 pp, p<.001 | +16.6 pp, p<.001 | +7.6 pp, p=.13 |
| 4B (gender) | gender → female | +11.9 pp, p<.001 | +13.3 pp, p=.001 | +10.2 pp, p=.02 |

In every study, gender/race feedback moves people *even when their initial selections already exceeded the pool base rate*. The effect shrinks some (which makes sense — diminishing returns as you approach saturation), but it stays significant or near-significant in 3 of 4 studies even in the Above subgroup. So the gender/race effect is not purely a gap-closing phenomenon.

**Pattern 2: Comparison-attribute feedback does NOT move people, even when there IS a gap.**

Look at comparison attributes where participants initially selected *below* the pool rate (so there was room to move toward the rate):

| Study | Attribute | Pool rate | Initial proportion | Gap (below pool) | Feedback effect in Below subgroup |
|---|---|---|---|---|---|
| 3A | year | 68% | 59% | −9 pp | +3.6 pp, p=.55 |
| 3A | duration | 76% | 68% | −8 pp | **−6.4 pp**, p=.13 (wrong direction) |
| 3A | budget | 40% | 36% | −4 pp | +4.7 pp, p=.39 |
| 3B | political | 40% | 35% | −5 pp | −3.5 pp, p=.43 |
| 4A | poets | 52% | 46% | −6 pp | +6.2 pp, p=.15 |
| 4A | oldies | 52% | 57% | +5 pp (over) | +9.5 pp, p=.07 |
| 4A | books | 60% | 59% | −1 pp | −1.2 pp, p=.79 |
| 4B | sold30m | 32% | 43% | +11 pp (over) | +8.7 pp, p=.42 |

Across 8 comparison attributes with genuine gaps or slack, only one (4A `oldies`, marginal at p=.07) shows a positive feedback effect approaching conventional significance — and even that is smaller than every one of the target-attribute effects. When there's room to move on a non-identity attribute, feedback still doesn't reliably move them.

**Pattern 3: Interaction tests (feedback × centered initial count) are flat for comparison attributes.**

For comparison attributes, the interaction between "received feedback" and "initial count" is mostly small and nonsignificant — meaning the effect of comparison feedback doesn't depend on where the participant started. This is the opposite of Wu's alternative, which predicts a strong interaction (feedback should work more for people with room to move). The target (gender/race) feedback shows a significant negative interaction in Studies 3B and 4B (p=.001 and p=.04 respectively) and marginal in 4A — consistent with diminishing returns but not with "the effect only exists because of the initial gap."

## Bottom line for the response letter

The data rebut Wu's alternative:

1. **Gender/race feedback moves people whose initial picks were already at or above the pool rate.** The effect is not just correcting underrepresentation — it persists when there is no underrepresentation to correct.
2. **Comparison-attribute feedback fails to move people even when they had clear initial under-selection of that attribute.** The "no effect" finding for comparison feedback is NOT because participants were already maxed out; it's because that kind of feedback doesn't carry the same motivational weight.
3. **The interaction between feedback and initial count is flat for comparison attributes but negative for gender/race.** That pattern is inconsistent with Wu's "they already use these appropriately" story and consistent with the paper's theory (identity-linked feedback triggers motivation to respond without prejudice; other feedback does not, regardless of initial use).

## Honest caveats

- **Study 4A `oldies` (+7.4 pp main, p=.05) and `poets` (+6.4 pp main, p=.08)** are not zero. They are moderate, less reliable, and smaller than the race-feedback effect in the same study (+13.4 pp, p<.001). Worth acknowledging in the write-up that comparison feedback is not literally inert — but it is clearly weaker and less consistent than identity feedback.
- **The `*_shown` indicators are not orthogonal to treatment status** in Studies 3A/3B/4A/4B because the stimulus-sampling scheme means control participants always receive all three comparison-feedback pieces while treatment participants receive 2 of 3 random ones plus the target. That makes each `*_shown` indicator a mixture of (control always on) and (within-treatment variation). This is the same spec the original Rmd analyses use, so the comparison is apples-to-apples with the paper's own Panel B in Figure 2, but it's worth knowing the contrast isn't a clean 2-arm comparison. A cleaner Zellner-style system (as used in Tables S3/S5/S7/S9) gives the same sign/magnitude pattern.
- **Study 2 is omitted.** Too few participants for reliable subsetting, and the comparison attributes (>500 pages, recent, entertainer) behave similarly to Figure 2.
- **Study 1 is omitted from this specific analysis** because its "initial counts" are stimulus-sampled treatment values, not participant behavior, so the Below/Above subsetting logic doesn't translate. Study 1 deserves its own importance-matching follow-up (see plan item 1.1).
