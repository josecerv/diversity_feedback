# R2 Manuscript Changes: Converged Proposal

Source documents:
- Manuscript: Gdoc `1H9jAvqG5CzQe_BksjkNmLwjLZSlAHvaEK1iyuHL23FI` (export at `manuscript_gdoc_text.txt`)
- Response letter: Gdoc `1YBHCMwD84xC3Qw38CPEzz1iv3JKg7Mw9jTJqyZGptkI` (export at `response_letter_gdoc_text.txt`)

This file converges two independent passes (`claude_second_pass.md` + `codex_second_pass.md`) into one paragraph-by-paragraph proposal. Each change shows the current manuscript text, the proposed replacement or insertion, and a note where the two passes diverged.

**Out of scope (deferred per JC):** the Study 4B Discussion footnote that summarizes the Table R1 / zero-initial pooled analysis. The level-of-analysis question is unresolved.

---

## Change 1: Intro citation refresh A (Kluger & DeNisi parenthetical)

**Anchor.** Manuscript line 11, the FIT/Kluger sentence.
**Response letter source.** Lines 35, 82.
**Why.** Reviewer 1 asked for contemporary feedback / goal-setting citations alongside the foundational ones.

### CURRENT
> According to Feedback Intervention Theory, such feedback is often ineffective because it lacks a benchmark against which people can evaluate their past behavior and consider making a change (Kluger & DeNisi, 1996, 1998).

### PROPOSED
> According to Feedback Intervention Theory, such feedback is often ineffective because it lacks a benchmark against which people can evaluate their past behavior and consider making a change (Kluger & DeNisi, 1996, 1998; Locke & Latham, 2019; Itzchakov & Latham, 2018; Chen et al., 2020).

### Notes
Both passes converged on the same wording; cite-block-only change. Body citation for Chen et al. (2020) abbreviated per APA 7th (the paper has 4 authors: Chen, Latham, Piccolo, & Itzchakov; reference-list entry in Change 13 lists all 4).

---

## Change 2: Intro Schultz reprise + applied citations

**Anchor.** Manuscript line 10, the Schultz et al. (2007) sentence.
**Response letter source.** Lines 35, 82.
**Why.** Same Reviewer 1 ask, focused on the social-norms strand.

### CURRENT (relevant sentence within the paragraph)
> Similarly, Schultz et al. (2007) found that when households received feedback about their energy usage alongside an explicit injunctive norm communicating that conservation was desirable as well as a social comparison to the energy use of their neighbors, energy consumption dropped by 5.7%.

### PROPOSED (append one sentence after Schultz 2007, before the "However" sentence)
> Similarly, Schultz et al. (2007) found that when households received feedback about their energy usage alongside an explicit injunctive norm communicating that conservation was desirable as well as a social comparison to the energy use of their neighbors, energy consumption dropped by 5.7%. Together, the original Schultz et al. (2007) study and the later reprise by Schultz et al. (2018) illustrate the value of feedback designs that pair information about past behavior with a clear normative standard, a pattern that has been further tested across applied interventions in energy and household conservation, with effect sizes that vary by national and methodological context (Andor et al., 2020; Henry et al., 2019; Mertens & Schultz, 2021).

### Notes
Claude vs. Codex divergence on the new sentence:
- **Codex (used above, with citation-audit revisions):** ties Schultz 2007 + Schultz 2018 together as the theoretical anchor, then lists the applied citations as "further tested" rather than "echoed in" recent work.
- **Claude:** wrote a more direct sentence ("A growing applied literature has since extended this descriptive-feedback-plus-norm logic across countries and consumption settings, replicating modest but reliable conservation effects in household energy and recycling.").
- Recommendation: Codex's version (kept above), because tying the two Schultz papers makes the theoretical work load explicit before the applied citations.
- Citation-audit revisions applied: body citations now use "Schultz et al. (2018)" since the paper has 5 authors (Schultz, Nolan, Cialdini, Goldstein, & Griskevicius); "Andor et al., 2020" and "Henry et al., 2019" use et al. per APA 7th (3+ author papers). Andor et al.'s actual finding is context-dependent (effect sizes much lower outside the US), so "echoed in" softened to "further tested across applied interventions ... with effect sizes that vary by national and methodological context."

---

## Change 3: New Intro paragraph on implicit-injunctive-norm framing

**Anchor.** Insert as a NEW paragraph immediately before manuscript line 17 (`"We propose that when descriptive feedback reveals..."`).
**Response letter source.** Line 83 (Reviewer 1 reply).
**Why.** Reviewer 1 pushed on why descriptive feedback can shift selections without a paired goal; the response letter promises an expanded theoretical framing in the Introduction.

### CURRENT
> *(No existing paragraph here; the insertion sits between line 16's "People's drive to respond without prejudice..." paragraph and line 17's "We propose that when descriptive feedback reveals..." paragraph.)*

### PROPOSED (new paragraph)
> Consistent with the broader logic of the feedback literature, feedback regulates behavior only when there is a standard against which past performance can be evaluated. The classical literature establishes that pairing feedback with an explicit goal supplies that standard. The same logic implies, however, that descriptive feedback alone can be sufficient in domains where a strong implicit standard is already entrenched, such that no explicit goal need be paired alongside it. Diversity-related selection decisions provide a clean test of this argument because anti-discrimination law, social and reputational pressures to behave in ways that are not prejudicial, and selectors' personally held egalitarian self-image jointly sustain an entrenched implicit injunctive norm against prejudiced selection (Plant & Devine, 1998; Crandall & Eshleman, 2003; Álvarez-Benjumea, 2023). When descriptive feedback reveals a discrepancy between this implicit standard and past behavior, we theorize that motivation to respond without prejudice will be activated even in the absence of a specific external goal.

### Notes
Both passes near-identical on this paragraph; the converged version above uses Claude's slightly cleaner phrasing ("sufficient" rather than "effective"; "not prejudicial" matches the rest of the manuscript's voice).

---

## Change 4: New Study 1 Discussion paragraph (NPR attribute-importance pretest)

**Anchor.** Insert as a NEW paragraph AFTER manuscript line 50 (Study 1 Discussion, `"Study 1 mirrored a real organizational practice currently deployed by NPR..."`) and BEFORE the `"Insert Figure 3 about here"` placeholder at line 52.
**Response letter source.** Line 42 (DE reply on the importance post-test).
**Why.** DE explicitly asked for this post-test; the response letter committed to incorporating it into Study 1 Discussion with exact numbers.

### CURRENT
> *(No existing paragraph here; the insertion sits between the existing Study 1 Discussion paragraph and the Figure 3 placeholder.)*

### PROPOSED (new paragraph)
> To address the alternative that Study 1's pattern was driven by participants viewing gender as more important than the comparison attributes, we conducted a separate pretest. We recruited N = 300 Prolific participants in a between-subjects design in which each participant rated one expert attribute on a 1 to 7 importance scale, with the NPR producer framing held constant. Participants rated whether a guest was a woman as moderately important (M = 3.64, SD = 1.76, n = 76). By comparison, participants rated "worked at a university" as more important (M = 4.67, SD = 1.60; t(148.8) = -3.76, p < .001, d = -0.61) and "were under 50 years old" as more important (M = 4.44, SD = 1.73; t(149.0) = -2.81, p = .006, d = -0.46), while "based on the West Coast of the United States" did not differ significantly in importance (M = 3.37, SD = 1.75; t(149.0) = 0.95, p = .343, d = +0.16). These results make Study 1 a conservative test: the comparison attributes were rated as equally or more important than gender, so differences in perceived importance cannot explain why only gender feedback moved subsequent selections.

### Notes
Both passes used identical numbers (matching the response letter exactly). Above uses Codex's between-subjects design phrasing because it more closely mirrors the response letter's wording.

---

## Change 5: GD foundational-citation refresh

**Anchor.** Manuscript line 170, Theoretical and Practical Contributions paragraph beginning `"Our work makes two primary theoretical contributions..."`. The change is a citation-block replacement WITHIN that paragraph.
**Response letter source.** Line 80.
**Why.** Same Reviewer 1 ask, applied to the General Discussion.

### CURRENT (citation block within the paragraph)
> ...According to Feedback Intervention Theory, feedback is most effective when paired with explicit goals, standards, or explicit injunctive norms that signal a clear gap between current performance and a desired state (Kluger & DeNisi, 1996; Schultz et al., 2007)...

### PROPOSED
> ...According to Feedback Intervention Theory, feedback is most effective when paired with explicit goals, standards, or explicit injunctive norms that signal a clear gap between current performance and a desired state (Kluger & DeNisi, 1996; Locke & Latham, 2019; Schultz et al., 2007; Schultz et al., 2018)...

### Notes
Both passes identical. Cite-block-only change; rest of the paragraph is untouched. Body citation for Schultz et al. (2018) abbreviated per APA 7th (the paper has 5 authors; reference-list entry in Change 13 lists all 5).

---

## Change 6: New GD paragraph distinguishing external vs. internal motivation pathways

**Anchor.** Insert as a NEW paragraph in the Theoretical and Practical Contributions section (within manuscript line 170), AFTER the sentence ending `"an implicit injunctive norm to avoid prejudice has already been internalized by the decision-maker."` and BEFORE the closing sentence about `"focus attention on past behavior"`.
**Response letter source.** Line 98 (Reviewer 2 reply).
**Why.** Reviewer 2 raised the scope-condition concern; response letter commits to distinguishing the two pathways explicitly.

### CURRENT
> *(No existing paragraph here; the insertion splits the existing line-170 paragraph in two.)*

### PROPOSED (new paragraph)
> Our account also distinguishes two pathways through which descriptive feedback can change selection decisions. The first is external motivation to control prejudice. This form of motivation is driven by the observability of selection decisions to colleagues, candidates, supervisors, customers, employees, and other stakeholders, as well as by legal accountability and reputational concerns. Firms' public DEI rhetoric can amplify these pressures, but it is better understood as a downstream signal of a broader legal and social environment rather than the source of the implicit injunctive norm itself (Levi & Fried, 2024). The second is internal motivation to control prejudice, which reflects selectors' personally held egalitarian beliefs and their desire to see themselves as non-prejudiced decision makers. This form of motivation is the one most directly supported by Study 4, where feedback was delivered privately and internal motivation to control prejudice was the dominant mediator of subsequent selections. Study 4B's political-ideology null is consistent with this account, suggesting that both external motivation to control prejudice and internal motivation to control prejudice may operate across the political spectrum.

### Notes
Both passes covered the same logical structure (external pathway → internal pathway → political-ideology bridge sentence). Differences were stylistic:
- **Codex (used above):** longer, more explicit, treats firm DEI rhetoric as a downstream signal (verbatim phrasing the response letter uses).
- **Claude:** more compact, same logic, slightly punchier prose.
- Recommendation: Codex's version (kept above) for fidelity to response-letter wording. If JC prefers terser prose, Claude's compact version is available in `claude_second_pass.md`.

---

## Change 7: Study 4 Discussion append (political-ideology null hedge + Kirgios sentence)

**Anchor.** Manuscript line 132, the paragraph `"In addition, Study 4B established that the effect of gender feedback was not moderated by political ideology nor party affiliation. Finally, it ruled out the possibility that our effects were driven by perceptions of which attributes were considered most important."`.
**Response letter source.** Line 96 (Reviewer 2 reply).
**Why.** Reviewer 2 wanted the ideology null interpreted; response letter commits to a conservative interpretation citing Kirgios et al. (2022).

### CURRENT
> In addition, Study 4B established that the effect of gender feedback was not moderated by political ideology nor party affiliation. Finally, it ruled out the possibility that our effects were driven by perceptions of which attributes were considered most important.

### PROPOSED
> In addition, Study 4B established that the effect of gender feedback was not moderated by political ideology nor party affiliation. Although we are cautious about over-interpreting a null interaction, this pattern is suggestive that descriptive feedback operates across partisan lines, an interpretation that converges with field-experiment evidence that women and racial minorities benefited from explicitly naming their demographic identity when contacting Republican and Democratic city councilors alike, an effect mediated by motivation to control prejudice (Kirgios et al., 2022). Finally, it ruled out the possibility that our effects were driven by perceptions of which attributes were considered most important.

### Notes
Divergence on PLACEMENT of the new sentences:
- **Claude (used above):** inserts the hedge + Kirgios sentence between the political-ideology sentence and the importance sentence. Logical fit: the Kirgios bridge is about ideology, so it belongs adjacent to the ideology claim, not after the importance ruling.
- **Codex:** appends the new sentences AFTER both existing sentences.
- Recommendation: Claude's placement (kept above) reads more naturally.

---

## Change 8: Intro DEI socio-political landscape (replaces one sentence)

**Anchor.** Manuscript line 15. Replace the single sentence beginning `"And despite recent rollbacks of diversity, equity, and inclusion (DEI) policies..."` with three paragraphs.
**Response letter source.** Line 93 (Reviewer 2 reply, DEI landscape).
**Why.** Reviewer 2 said the paper "feels a bit dated" and asked for a fuller treatment of the current DEI landscape that also reinforces the paper's continued relevance.

### CURRENT (one sentence to replace)
> And despite recent rollbacks of diversity, equity, and inclusion (DEI) policies at some organizations in the United States (Heaton, 2025), recent surveys indicate that the majority of Americans still hold favorable views about DEI (Minkin, 2024; Kidwai, 2025).

### PROPOSED (three new paragraphs)
> Over the past five years, the organizational context surrounding diversity, equity, and inclusion has shifted substantially. Following the racial justice protests of 2020, many U.S. organizations rapidly expanded public DEI commitments, representation goals, training programs, and diversity infrastructure (Devine & Ash, 2022; Ellis, 2025). By 2024 and 2025, however, some firms had scaled back explicit DEI goals, staffing, and language in response to new legal, political, and reputational pressures in the United States (Heaton, 2025; Minkin, 2024). This changed landscape matters for our theory because explicit organizational commitments are one source of pressure on evaluators. Yet it does not eliminate the central question of whether making past selection patterns visible can change subsequent decisions.
>
> Three countervailing patterns suggest that this question remains consequential. First, regulatory demand for transparency around demographic representation has continued to expand outside the United States. Australia's Workplace Gender Equality Amendment (Closing the Gender Pay Gap) Act 2023 strengthened public reporting of firm-level gender representation and pay-gap data (Ressia, 2024), the EU Pay Transparency Directive 2023/970 entered into force in June 2023 and requires member-state transposition by 7 June 2026 (Directive (EU) 2023/970; Mignano, 2024), and the EU Women on Boards Directive 2022/2381 establishes a 40% representation objective for the underrepresented gender on listed-company boards. Second, U.S. public and stakeholder support has shifted more slowly than corporate rhetoric. A 2025 Bentley University and Gallup survey found that 69% of U.S. adults believe businesses should promote DEI (Bentley University & Gallup, 2025; see also Kidwai, 2025), and recent anti-DEI shareholder proposals have failed at prominent firms including Costco, Apple, Levi's, John Deere, and Goldman Sachs (CBS News, 2025; Marketplace, 2025). Third, many firms appear to be reframing diversity-related work rather than abandoning it entirely. For example, Constellation Brands has used the label "Inclusive Culture Team," Kohl's has shifted to "Chief Inclusion and Belonging Officer," and Nationwide and UPS have emphasized language such as "belonging," "respect," "fairness," and "inclusive experiences" (Elias & Palmer, 2025; Dungan, 2025; Marketplace, 2026). Consistent with this pattern, Paradigm reports a 22% decrease in Fortune 100 references to "DEI/diversity" between 2023 and 2024 alongside a 59% increase in references to "belonging" (Paradigm, 2025).
>
> These developments sharpen, rather than weaken, the paper's theoretical contribution. Our claim is not that descriptive feedback is a managerial tool firms can deploy with equal force in any climate. Rather, we theorize and test whether descriptive feedback about past selection patterns can change subsequent decisions when those patterns raise concerns about appearing prejudiced or failing to live up to one's own egalitarian standards. The current DEI landscape therefore helps clarify the scope of the effect: descriptive feedback should be most consequential where race and gender remain salient dimensions of evaluation because of law, transparency requirements, stakeholder scrutiny, organizational norms, or evaluators' own egalitarian self-image.

### Notes
- Codex's pass surfaced specific author names (Elias & Palmer for CNBC, Dungan for HR Grapevine) and resolved several trade-press URLs by date and headline; Claude flagged these as `FORMAT TBD`. The version above is Codex's, which preserves JC's effort to verify but uses resolved citations where they exist.
- Both passes converge on the three-paragraph structure (current state → countervailing patterns → theoretical sharpening) verbatim.
- The two `FORMAT TBD` entries that remain (Marketplace 2026 on Nationwide/UPS rebranding; Paradigm 2025 Fortune 100 report) should be vetted by JC before submission.

---

## Change 9: Intro clarification (no-appropriate-level assumption, new sentences)

**Anchor.** Several sentences inserted in the Introduction. Two placement options below.
**Response letter source.** Line 45 (DE reply on the appropriate-level question).
**Why.** DE asked us to discuss the alternative that participants might simply use other measures appropriately in initial selections; the response letter commits to several Introduction sentences plus a Study 4 footnote (Change 10).

### CURRENT
> *(No existing sentences here; this is a clarification to be inserted.)*

### PROPOSED (recommended location: new short paragraph after line 17 `"We propose..."` paragraph, before `"Summary of the Current Research"` heading)
> This prediction does not require assuming that participants' initial selections reflect an appropriate or normative benchmark for any attribute. Instead, we predict that descriptive feedback shifts subsequent selections only when the attribute summarized by the feedback carries an implicit injunctive norm that the feedback calls into question. Gender and race are such dimensions in our studies because low representation of women or racial minorities can raise concerns about prejudiced selection. The comparison attributes we test, including whether candidates worked at a university, were under 50, were based on the West Coast, wrote in multiple genres, or possessed other task-relevant characteristics, may be more or less desirable in a given setting, but they do not carry the same implicit injunctive norm.

### Notes
Divergence on placement:
- **Codex (used above):** new short standalone paragraph after the "We propose..." paragraph. Reads as its own theoretical clarification; doesn't bury the point inside an existing paragraph.
- **Claude:** append the clarification to the end of the line-14 theorizing paragraph.
- Recommendation: Codex's placement (kept above). The clarification is distinct enough to deserve its own paragraph and benefits from the listing of comparison-attribute examples that grounds it.

---

## Change 10: Study 4 Discussion footnote (no-appropriate-level clarification, NOT Table R1)

**Anchor.** Footnote attached to manuscript line 132 (Study 4 Discussion) or the local sentence about attribute importance.
**Response letter source.** Line 45.
**Why.** Companion to Change 9; the response letter explicitly committed to a clarifying footnote in Study 4 Discussion.

### CURRENT
> *(No existing footnote here.)*

### PROPOSED (new footnote)
> Footnote: We do not assume that participants' initial selections reflected an appropriate level of representation for the comparison attributes in Study 4, nor do we assume that there is a correct base rate for any attribute in the absence of an explicit goal or prescription. We interpret the effect of gender and racial feedback in this paradigm as evidence that those dimensions carry an implicit injunctive norm that the feedback calls into question, not as evidence that participants under-selected women or racial minorities relative to an objectively correct base rate.

### Notes
Both passes converged on essentially this footnote. This is NOT the Table R1 / zero-initial pooled analysis footnote; that one is deferred until the level-of-analysis question is resolved.

---

## Change 11: GD scope-condition paragraph (organizational DEI climate)

**Anchor.** Insert as a new paragraph in Future Directions and Limitations near manuscript line 175 or 176.
**Response letter source.** Lines 38 and 93 (closing portion of the DEI-landscape reply).
**Why.** Response letter explicitly commits to identifying "organizational DEI climate as a limiting condition for future research" in the General Discussion.

### CURRENT
> *(No existing paragraph here.)*

### PROPOSED (new paragraph, recommended location: after line 176, between the public/private feedback paragraph and the self-tabulation paragraph)
> Organizational DEI climate is another important scope condition. Descriptive feedback should be most likely to change subsequent selections when the organizational setting makes past selection patterns consequential for evaluators' reputations, legal accountability, stakeholder relationships, or self-image as egalitarian decision makers. It should matter less in settings where selections are fully private, where demographic patterns are unlikely to be seen as evidence of prejudice, or where an organization openly rejects egalitarian norms. Recent shifts in U.S. DEI rhetoric therefore do not make the intervention categorically irrelevant; instead, they clarify that its effect should depend on whether race and gender remain dimensions on which evaluators anticipate external scrutiny or experience internal pressure to act consistently with egalitarian commitments.

### Notes
Both passes converged on Future Directions and Limitations as the home (matching the response letter wording). Codex's prose (used above) is more explicit about the two-pathway connection; Claude's was structurally similar but slightly more abstract. Codex's also closes by tying the U.S. landscape directly back to scope, which gives the paragraph a clean exit.

---

## Change 12: GD fairness-norms additional-mechanism paragraph

**Anchor.** Insert as a short new paragraph in Future Directions and Limitations near manuscript line 175.
**Response letter source.** Line 99 (Reviewer 2 reply).
**Why.** Response letter acknowledges that motivation to respond without prejudice is one key mechanism but not the only one; commits to noting fairness norms as a plausible additional driver, with Study 5 mechanism evidence as a precedent.

### CURRENT
> *(No existing paragraph here.)*

### PROPOSED (new paragraph)
> More broadly, motivation to respond without prejudice is unlikely to be the only mechanism through which descriptive feedback affects selection. Descriptive feedback can also make fairness norms and broader concerns about appropriate representation salient. Study 5 provides initial evidence for this possibility: when women were overrepresented in initial selections, fairness concerns led participants to select more men, thereby reducing gender imbalance. Future research could examine when descriptive feedback primarily activates prejudice concerns, when it activates fairness concerns, and when these motives jointly shape subsequent selections.

### Notes
Both passes converged on the same structural beats. Codex's version (above) explicitly invokes Study 5's mechanism analysis as supporting evidence, which keeps the new claim faithful to data already in the paper.

---

## Change 13: References (new entries) and body-citation flag

**Anchor.** References section, alphabetical insertions. Plus a body citation for Devine & Ash (2022) flagged in Change 8.
**Response letter source.** Lines 102-112 (the response letter's References section) plus the new trade-press / regulatory sources introduced in Change 8.
**Why.** Every new citation in Changes 1, 2, 5, 6, 7, 8 needs a corresponding References entry.

### PROPOSED (new entries, alphabetical)

Verified DOI / publication entries (insert directly):

> Andor, M. A., Gerster, A., Peters, J., & Schmidt, C. M. (2020). Social norms and energy conservation beyond the US. *Journal of Environmental Economics and Management*, 103, 102351. https://doi.org/10.1016/j.jeem.2020.102351
>
> Bentley University & Gallup. (2025). *Bentley-Gallup Business in Society Survey: 2025 report*. Gallup. https://www.gallup.com/file/analytics/696014/Gallup-Bentley-University_Business-In-Society%20Survey_2025%20Report.pdf
>
> CBS News. (2025, January 24). Costco shareholders reject an anti-DEI measure, after Walmart and others end diversity programs. https://www.cbsnews.com/news/costco-dei-policy-board-statement-shareholder-meeting-vote/
>
> Chen, X., Latham, G. P., Piccolo, R. F., & Itzchakov, G. (2020). An enumerative review and a meta-analysis of primed goal effects on organizational behavior. *Applied Psychology*, 70(1), 216-253. https://doi.org/10.1111/apps.12239
>
> Devine, P. G., & Ash, T. L. (2022). Diversity training goals, limitations, and promise: A review of the multidisciplinary literature. *Annual Review of Psychology*, 73(1), 403-429. https://doi.org/10.1146/annurev-psych-060221-122215
>
> Directive (EU) 2022/2381 of the European Parliament and of the Council of 23 November 2022 on improving the gender balance among directors of listed companies and related measures. *Official Journal of the European Union*, L 315, 44-59. http://data.europa.eu/eli/dir/2022/2381/oj
>
> Directive (EU) 2023/970 of the European Parliament and of the Council of 10 May 2023 to strengthen the application of the principle of equal pay for equal work or work of equal value between men and women through pay transparency and enforcement mechanisms. *Official Journal of the European Union*, L 132, 21-44. http://data.europa.eu/eli/dir/2023/970/oj
>
> Dungan, R. (2025, March 31). Semantics: Has DEI actually gone to ground, or has it just been rebranded? *HR Grapevine*. https://www.hrgrapevine.com/us/content/article/2025-03-31-has-dei-really-gone-to-ground-or-has-it-just-assumed-a-new-identity
>
> Elias, J., & Palmer, A. (2025, March 30). In Trump era, companies are rebranding DEI efforts, not giving up. *CNBC*. https://www.cnbc.com/2025/03/30/in-trump-era-companies-are-rebranding-dei-efforts-not-giving-up.html
>
> Henry, M. L., Ferraro, P. J., & Kontoleon, A. (2019). The behavioural effect of electronic home energy reports: Evidence from a randomised field trial in the United States. *Energy Policy*, 132, 1256-1261. https://doi.org/10.1016/j.enpol.2019.06.039
>
> Itzchakov, G., & Latham, G. P. (2018). The moderating effect of performance feedback and the mediating effect of self-set goals on the primed goal-performance relationship. *Applied Psychology*, 69(2), 379-414. https://doi.org/10.1111/apps.12176
>
> Levi, A., & Fried, Y. (2024). Diversity, equity, and inclusion programs' emphasis on symbolism: Causes and consequences. *Journal of Organizational Behavior*, 46(1), 172-187. https://doi.org/10.1002/job.2834
>
> Locke, E. A., & Latham, G. P. (2019). The development of goal setting theory: A half century retrospective. *Motivation Science*, 5(2), 93-105. https://doi.org/10.1037/mot0000127
>
> Marketplace. (2025, May 22). Costco doubles down on DEI and benefits. *Marketplace*. https://www.marketplace.org/story/2025/05/22/costco-doubles-down-on-dei-and-benefits
>
> Mertens, S. N., & Schultz, P. W. (2021). Referent group specificity: Optimizing normative feedback to increase residential recycling. *Journal of Environmental Psychology*, 73, 101541. https://doi.org/10.1016/j.jenvp.2020.101541
>
> Mignano, V. (2024). Gender Pay Gap: The Protection of the Right to Equal Pay under the Pay Transparency Directive. *Zeitschrift für Europarechtliche Studien*, 27(3), 371-401. https://doi.org/10.5771/1435-439X-2024-3-371
>
> Ressia, S. (2024). Addressing gender inequality issues in Australia: An annual review of developments. *Journal of Industrial Relations*, 66(5), 742-758. https://doi.org/10.1177/00221856241295490
>
> Schultz, P. W., Nolan, J. M., Cialdini, R. B., Goldstein, N. J., & Griskevicius, V. (2018). The constructive, destructive, and reconstructive power of social norms: Reprise. *Perspectives on Psychological Science*, 13(2), 249-254. https://doi.org/10.1177/1745691617693325

JC to verify before submission:

> [FORMAT TBD: Marketplace (2026) reporting on firm-level DEI rebranding toward "belonging," "respect," "fairness," and "inclusive experiences," including Nationwide and UPS. Verify the exact article, date, and URL. Codex resolved one Marketplace 2025 article but a separate 2026 reference for Nationwide/UPS rebranding was not directly found.]
>
> [FORMAT TBD: Paradigm (2025) Fortune 100 DEI/belonging language analysis reporting a 22% decrease in "DEI/diversity" references and a 59% increase in "belonging" references between 2023 and 2024. CNBC and HR Grapevine attribute these figures to Paradigm; confirm exact report title and URL.]

### Notes
- Kirgios et al. (2022) is already in the manuscript references (line 229 of `manuscript_gdoc_text.txt`); do not duplicate.
- Devine & Ash (2022) should be cited in the Introduction body near line 15 (Ellis, 2025) as a multidisciplinary review of organizational diversity training; that body citation is already integrated into Change 8 above.
- Codex performed live citation lookup for the trade-press sources where Claude flagged them as `FORMAT TBD`. The two remaining `FORMAT TBD` entries are genuine open items that need JC's review.

---

## Self-audit

- No em or en dashes appear in this deliverable.
- The disallowed theory term does not appear in the drafted prose.
- The seven canonical NPR-pretest numbers in Change 4 match the response letter exactly: N = 300; gender M = 3.64, SD = 1.76, n = 76; university M = 4.67, SD = 1.60, t(148.8) = -3.76, p < .001, d = -0.61; age M = 4.44, SD = 1.73, t(149.0) = -2.81, p = .006, d = -0.46; West Coast M = 3.37, SD = 1.75, t(149.0) = 0.95, p = .343, d = +0.16.
- Out-of-scope item (Study 4B Discussion footnote summarizing Table R1 / zero-initial pooled analysis) is NOT drafted. Change 10 is a distinct clarification footnote.
- Twelve content changes plus one references change drafted; thirteen total.

---

## What JC should do with this file

1. Read each change top to bottom; the CURRENT block quotes verbatim what's in the canonical manuscript Gdoc, the PROPOSED block quotes show the replacement or insertion.
2. Where two options were surfaced (Changes 2, 6, 7, 8, 9), the recommendation is the version in PROPOSED; switch to the alternative in `claude_second_pass.md` or `codex_second_pass.md` if you prefer.
3. The two `FORMAT TBD` reference entries (Marketplace 2026 on Nationwide/UPS; Paradigm 2025) need your judgment on the exact citation form before submission.
4. When you're ready, the next step is posting anchored comments in the manuscript Gdoc at each change's locator, so you can apply the edits in context in Suggesting mode. That step is intentionally not run yet. Say the word.
