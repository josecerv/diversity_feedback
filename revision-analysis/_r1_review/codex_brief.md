# Brief: independent read on Katy's critique of Table R1 base-rate analysis

You are reviewing a methodological disagreement between a PhD student (Jose) and his advisor (Katy / Katherine Milkman). The setting is a manuscript revision (R2) for Management Science. The Associate Editor (George Wu) raised an alternative explanation in R1; Jose ran a follow-up analysis to address it; Katy is unconvinced and left five comments on the response letter.

The question we want your judgment on:
> Did Jose get this analysis wrong, or is there a reasonable defense? Lay out the strongest case for each side. If a defense exists, what is the tightest version of it? If the analysis really is broken, what would replace it?

We want an independent, opinionated read — not a hedge. You can disagree with Jose, with Katy, or with both. But be specific about WHERE the analysis is sound and WHERE it isn't, and don't just rephrase the disagreement.

Output format requirements (mandatory — the wrapper validates these markers):

## Verdict
A 2-4 sentence bottom line: in your view, is the analysis defensible, partially defensible, or broken? Pick a side and justify it.

## What the analysis actually shows
Walk through what the analysis logically licenses (and does NOT license) as a claim against the AE's alternative explanation. Be precise about the inferential chain.

## Where Katy is right
Specific points where her critique lands. Quote the comment text and explain why she's correct.

## Where Jose has a defense
Specific points where the analysis or framing is defensible. Be concrete about what claim is supported and under what assumptions.

## Recommended path forward
Concrete edits to (a) the Table R1 prose, (b) the table itself, and/or (c) the underlying R analysis, that resolve Katy's valid critiques while preserving Jose's defensible claims. If the analysis cannot be salvaged, say so and propose a replacement.

---

# CONTEXT 1: The AE's actual prompt (verbatim from the response letter)

> "I liked Study 4b and think that it does a good job of showing that feedback about gender is different than feedback about other important dimensions. In addition, your studies show that gender/race feedback lead subjects to adjust their portfolios about the base rate. That said, I'm still left wondering whether your findings are showing what you say or it is just that people use these other measures appropriately in their initial selections. I'd like you to at least discuss this possibility, and perhaps there is additional analysis that you can do to address this."

# CONTEXT 2: Jose's response prose (verbatim)

> RESPONSE: Thank you for raising this alternative. We agree it is worth testing directly, and we have run a follow-up analysis (presented in Table R1 below) to evaluate it. We now report these analyses in our revised manuscript in [WHERE?]
>
> To operationalize "appropriate use," we use each attribute's base rate in the candidate pool. In each study, participants choose from a fixed pool with a known proportion of candidates having a given attribute (for example, 32% of the authors in Study 4A are racial minorities). The base rate is a neutral benchmark for proportional selection. Arguably, a participant whose initial selection rate is below the base rate has under-weighted the attribute, leaving feedback room to increase that attribute's selection. Conversely, if a participant selects at or above the base rate, then there is no proportional gap left for feedback to close. For each of the five studies in which participants generated their own initial portfolios (Studies 2, 3A, 3B, 4A, 4B), we split the sample into Below-Pool and At/Above-Pool subgroups by attribute and re-estimated the feedback effect within each subgroup.
>
> As shown in Table R1, we show that even when participants' initial selections are below the base rate, there is no subsequent increase in that attribute's selections. More specifically, across the fifteen Below-Pool comparison-attribute coefficients, thirteen are statistically null, one is marginally positive (+9.53 percentage points, p = .07), and one is marginally negative (-16.47 percentage points, p = .07). A random-effects mini meta-analysis (Goh et al., 2016) pooling the fifteen estimates yields +0.82 percentage points (95% CI [-2.48, 4.13], z = 0.49, p = .63): comparison feedback does not move participants even when we would expect the largest effect of feedback.
>
> That said, we acknowledge that "appropriate use" can also be read more broadly as preference-consistent selection (e.g., under-selecting long-duration films because participants dislike them), which the present subgroup test cannot rule out.

# CONTEXT 3: Table R1 (verbatim)

Table R1. Treatment Effects of Comparison-Attribute Feedback on Subsequent Selections, by Initial-Selection Subgroup.

```
Study / (N) | Comparison Attribute | Base Rate in Candidate Pool | Main Effect (Overall) | Below-Pool Subgroup | At/Above-Pool Subgroup
Study 2 (N=302)  | Featured an Entertainer        | 44.0% | +5.58 (N=302)  | +11.87 (N=233) | -9.49  (N=69)
                 | Over 500 Pages                  | 28.0% | +1.29 (N=302)  | +5.06  (N=257) | -19.44 (N=45)
                 | Written in Past 25 Years        | 76.0% | +4.57 (N=302)  | -2.85  (N=197) | +15.59 (N=105)
Study 3A (N=1k)  | High Budget                     | 40.0% | -0.23 (N=1000) | +4.70  (N=516) | -5.66  (N=484)
                 | Long Duration                   | 76.0% | -6.72+ (N=1000)| -6.44  (N=727) | -8.08  (N=273)
                 | Recent Release                  | 68.0% | +6.54 (N=1000) | +3.59  (N=602) | +7.97  (N=398)
Study 3B (N=1k)  | High Budget                     | 32.0% | +1.98 (N=1000) | -5.14  (N=308) | +5.23  (N=692)
                 | Political Leader                | 40.0% | -5.08 (N=1000) | -3.52  (N=647) | -7.69  (N=353)
                 | Recent Release                  | 52.0% | -0.29 (N=1000) | -16.47+ (N=402)| +7.58  (N=598)
Study 4A (N=1k)  | Wrote Books in 500-Most-Common  | 60.0% | +0.68 (N=1000) | -1.24  (N=490) | +3.62  (N=510)
                 | Wrote Classic Works (50+)       | 52.0% | +7.44+ (N=1000)| +9.53+ (N=508) | +5.62  (N=492)
                 | Wrote Poetry                    | 52.0% | +6.38+ (N=1000)| +6.18  (N=754) | +6.25  (N=246)
Study 4B (N=1k)  | In Continuous Print 50+ Years   | 68.0% | +2.06 (N=1000) | +1.59  (N=290) | +2.40  (N=710)
                 | Sold 30M+ Copies                | 32.0% | +1.21 (N=1000) | +8.73  (N=154) | -0.12  (N=846)
                 | Spanning Multiple Genres        | 44.0% | -8.22+ (N=1000)| -3.02  (N=431) | -12.52* (N=569)
Pooled (k = 15)  | Random-effects meta-analysis    |       | +0.78 [-1.87, 3.43] | +0.82 [-2.48, 4.13] | +0.45 [-3.14, 4.04]
```

Notes (verbatim from response letter): "Subgroup columns split each sample by whether the participant's initial selections of the attribute were below the candidate-pool base rate ('Below-Pool') or at or above the candidate-pool base rate ('At/Above-Pool'). The bottom row reports the pooled effects across studies, computed via random-effects meta-analysis weighting each study by its standard error. Studies 1 and 5 are excluded because their initial portfolios are not participant-generated in a way that defines per-participant Below-Pool or At/Above-Pool subgroups."

# CONTEXT 4: Katy's five comments on this analysis (verbatim)

**Comment A** — anchored to "run a follow-up analysis":
> "I don't think the follow up analyses make the point you think they are making. Let's talk about what analyses would. And then we should plan to put them in the paper, not just the response letter."

**Comment B** — anchored to "To operationalize 'appropriate use,' we use each attribute's base rate in the candidate pool":
> "But you don't really do this because then 'above' that level would also be inappropriate? The categories are arbitrary so it's not clear which direction is 'appropriate'. I don't think this analysis makes sense? Let's discuss (and in general, it would be great to talk about things like this before you do a bunch of time-consuming analyses!)"

**Comment C** — anchored to "At/Above-Pool Subgroup":
> "I am not following this category!"

**Comment D** — anchored to "or above" (in "if a participant selects at or above the base rate, then there is no proportional gap left for feedback to close"):
> "This isn't true. If they selected ABOVE the base rate, the feedback should move the gap in the reverse direction?"

**Comment E** — anchored to discussion of dichotomous category survey design:
> "I think the thing is that if we run #2, we probably have to redo the NPR study anyway so it better matches it. The real issue is: what are dichotomous predictors that will work? OR we could do something a bit different on NPR. We could give categories of feedback like:
> GENDER:
>   Male: X%
>   Female: Y%
>   Non-binary: Z%
> AGE:
>   20s: X%
>   30s: Y%
>   40s: Z%
> ...
> And then with that new design, I think we could get people to say the categories are equally important. It will not be clear to 'compare' the effectiveness of the feedback in quite the same way. But we could save that for later studies and just use Study 1 to establish a big shift in gender of selectees without big shifts in the avg. age/location/job titles of selectees or something?"

# CONTEXT 5: How the analysis was actually computed

The R script `de_meta_by_attribute_type.R` does the following:
1. Takes per-study results from `de_4b_concern_results.csv` covering Studies 2, 3A, 3B, 4A, 4B (the five studies where participants generated their own initial portfolios).
2. For each attribute (target = gender/race; comparison = three other binary attributes per study), it has: main effect, below-pool subgroup effect, above-pool subgroup effect (each in pp), each with a derived SE from |z| = qnorm(1 - p/2).
3. Pools across studies using random-effects REML meta-analysis (`metafor::rma`).
4. Computes critical-cell pools and contrasts:
   - Target effect among At/Above-Pool participants (k=5)
   - Comparison effect among Below-Pool participants (k=15)
   - Contrast between the two

The Below-Pool subgroup is defined PER PARTICIPANT PER ATTRIBUTE: a participant is "Below-Pool" for attribute X in study Y if the proportion of their initial portfolio that has attribute X is strictly below X's pool base rate. Otherwise (equal or above) they are "At/Above-Pool."

# CONTEXT 6: What's at stake

If the analysis is defensible, Jose wants to keep it (with edits to address Katy's clarity comments) as the core response to Wu's alternative-explanation request, and move it into the manuscript proper.

If it's broken, Jose needs to either (a) replace it with an analysis Katy will sign off on, or (b) drop the analytic response and address Wu's concern via prose / via the planned new pretest study (Option 1 / Option 2 from comment E).

# What we want from you

Be honest. Don't just hedge. Read the AE's prompt, the analysis, the table, and Katy's five comments. Then output the five required sections. The reader (Jose) is going to make a judgment call based on your verdict — so don't soften.
