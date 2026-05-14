"""Post anchored Drive comments to the manuscript Gdoc for each v3 change.

Each comment quotes a short, unique anchor text from the current manuscript and
contains ONLY the proposed prose / change content (no [v3-plan] tag, no
EXPAND/INSERT scaffolding -- JC asked for the actual content only, 2026-05-14).

Modes:
  --dry-run        list planned comments + verify each anchor matches exactly
                   once in the current manuscript export (no API calls)
  --cleanup        delete any existing comments whose body still contains the
                   old "[v3-plan " tag (one-time cleanup)
  --post           post all comments
  --post --replace cleanup + post in one run

Anchor format for Google Docs (Drive API v3):
  {"r":"head","a":[{"txt":{"v":"<exact text>"}}]}
"""
from __future__ import annotations

import argparse
import json
import sys
from pathlib import Path

from google.oauth2.credentials import Credentials
from google.auth.transport.requests import Request
from googleapiclient.discovery import build
from googleapiclient.errors import HttpError

DOC_ID = "1H9jAvqG5CzQe_BksjkNmLwjLZSlAHvaEK1iyuHL23FI"
TOKEN_PATH = r"C:/Users/jcerv/.config/gws/token.json"
MANUSCRIPT_TXT = Path(__file__).resolve().parent / "manuscript_gdoc_text.txt"


def get_creds() -> Credentials:
    with open(TOKEN_PATH) as f:
        tok = json.load(f)
    creds = Credentials(
        token=tok.get("token"),
        refresh_token=tok["refresh_token"],
        token_uri=tok["token_uri"],
        client_id=tok["client_id"],
        client_secret=tok["client_secret"],
        scopes=tok["scopes"],
    )
    if not creds.valid:
        creds.refresh(Request())
    return creds


# Each entry:
#   tag      short identifier (internal only; not put into the comment body)
#   anchor   EXACT substring from the current manuscript_gdoc_text.txt
#            (must appear exactly once)
#   body     the comment text JC will see in the Gdoc -- proposed prose only

CHANGES = [
    {
        "tag": "C2",
        "anchor": "(Allcott, 2011; Bhanot, 2021; Cialdini et al., 2006; Tiefenbeck et al., 2018)",
        "body": (
            "Expand to:\n"
            "(Allcott, 2011; Bhanot, 2021; Cialdini et al., 2006; Tiefenbeck et al., 2018; "
            "Schultz et al., 2018; Andor et al., 2020; Henry et al., 2019; Mertens & Schultz, 2021)\n\n"
            "Then a few sentences later, pair the Schultz et al. (2007) mention with the 2018 reprise:\n"
            "Similarly, Schultz et al. (2007, with later reprise in Schultz et al., 2018) found that..."
        ),
    },
    {
        "tag": "C1+C3",
        "anchor": "such feedback is often ineffective because it lacks a benchmark against which people can evaluate their past behavior and consider making a change (Kluger & DeNisi, 1996, 1998).",
        "body": (
            "Expand the parenthetical to:\n"
            "(Kluger & DeNisi, 1996, 1998; Locke & Latham, 2019; Itzchakov & Latham, 2018; "
            "Chen et al., 2020)\n\n"
            "Then append at the end of the paragraph:\n"
            "We argue that such norms are sustained by both internal factors, such as a person's "
            "egalitarian personal standards of conduct, and external factors, such as anti-discrimination "
            "law and other social and reputational pressures "
            "(Plant & Devine, 1998; Crandall & Eshleman, 2003; Álvarez-Benjumea, 2023)."
        ),
    },
    {
        "tag": "C4",
        "anchor": "We further posit that such feedback can be more effective at influencing future choices than descriptive feedback about other features of selection decisions.",
        "body": (
            "Insert after this sentence (before the “Why?”):\n\n"
            "This prediction does not assume that participants' initial selections reflect a normatively "
            "“appropriate” or correct level for any attribute. Instead, we predict that descriptive "
            "feedback shifts subsequent selections on dimensions that carry an implicit injunctive norm "
            "the feedback calls into question, which we argue is true of race and gender but not of the "
            "comparison attributes we test."
        ),
    },
    {
        "tag": "C5",
        "anchor": "And despite recent rollbacks of diversity, equity, and inclusion (DEI) policies at some organizations in the United States (Heaton, 2025), recent surveys indicate that the majority of Americans still hold favorable views about DEI (Minkin, 2024; Kidwai, 2025).",
        "body": (
            "First, in the prior sentence, expand “(Ellis, 2025)” to “(Ellis, 2025; Devine & Ash, 2022).”\n\n"
            "Then replace this anchored sentence with three new paragraphs:\n\n"
            "The corporate posture toward DEI in the United States has fluctuated substantially over the "
            "past five years. Many U.S. organizations rapidly expanded public DEI commitments, "
            "representation goals, training programs, and diversity infrastructure following the racial "
            "justice protests of 2020 (Devine & Ash, 2022; Ellis, 2025). By 2024 and 2025, however, some "
            "firms had scaled back explicit DEI goals, staffing, and language in response to new legal, "
            "political, and reputational pressures (Heaton, 2025; Minkin, 2024).\n\n"
            "Three countervailing patterns, however, keep the question of how to support diverse "
            "selection decisions central rather than peripheral. First, regulatory demand for "
            "transparency around demographic representation continues to expand internationally. "
            "Australia's Workplace Gender Equality Amendment (Closing the Gender Pay Gap) Act 2023 "
            "mandates public reporting of firm-level gender representation and pay-gap data "
            "(Ressia, 2024); the EU Pay Transparency Directive 2023/970 entered into force in June 2023 "
            "and requires member-state transposition by 7 June 2026 (Directive (EU) 2023/970; "
            "Mignano, 2024); and the EU Women on Boards Directive 2022/2381 establishes a 40% "
            "representation objective for the underrepresented gender on listed-company boards. Second, "
            "within the United States, public and stakeholder sentiment has shifted more slowly than "
            "corporate rhetoric. A 2025 Bentley University and Gallup survey found that 69% of U.S. "
            "adults believe businesses should promote DEI (Bentley University & Gallup, 2025; see also "
            "Kidwai, 2025), and recent anti-DEI shareholder proposals have failed at prominent firms "
            "including Costco, Apple, Levi's, John Deere, and Goldman Sachs (CBS News, 2025; "
            "Marketplace, 2025). Third, many firms appear to be reframing diversity-related work rather "
            "than abandoning it entirely. For example, Constellation Brands has used the label "
            "“Inclusive Culture Team,” Kohl's has shifted to “Chief Inclusion and Belonging Officer,” "
            "and Nationwide and UPS have emphasized language such as “belonging,” “respect,” "
            "“fairness,” and “inclusive experiences” (Elias & Palmer, 2025; Dungan, 2025; "
            "Marketplace, 2026). Consistent with this pattern, Paradigm reports a 22% decrease in "
            "Fortune 100 references to “DEI/diversity” between 2023 and 2024 alongside a 59% increase "
            "in references to “belonging” (Paradigm, 2025).\n\n"
            "These developments sharpen, rather than weaken, the paper's central question. Our claim is "
            "not that descriptive feedback is a managerial tool firms can deploy with equal force in any "
            "climate. Rather, we test whether descriptive feedback about past selection patterns can "
            "change subsequent decisions when those patterns raise concerns about appearing prejudiced "
            "or about failing to live up to one's own egalitarian standards. The current DEI landscape "
            "therefore helps clarify the scope of the effect: descriptive feedback should be most "
            "consequential where race and gender remain salient dimensions of evaluation because of "
            "law, transparency requirements, stakeholder scrutiny, organizational norms, or evaluators' "
            "own egalitarian self-image."
        ),
    },
    {
        "tag": "C6",
        "anchor": "Participants. As pre-registered (https://osf.io/vazsm/), we recruited 1,000 participants through Prolific to complete a 3-minute survey for $0.75",
        "body": (
            "Insert this as a new paragraph immediately before “Participants.” (parallels how Study 4B "
            "describes its pretest in Methods). All numbers as X placeholders until pre-registered data arrive:\n\n"
            "Attribute-importance pretest. Prior to this study, we conducted a pre-registered pretest "
            "([link to AsPredicted]; N = X) rating the four expert attributes used in our gender feedback "
            "manipulation – the percentage of past NPR experts who were (1) women, (2) under 50 years old, "
            "(3) based on the West Coast of the United States, or (4) worked at a university – on their "
            "importance for selecting NPR experts on a 1 (Not at all important) to 7 (Very important) "
            "scale. Participants were randomly assigned to rate one of the four attributes (n = X per "
            "cell). The pretest was designed to address the concern that any observed difference between "
            "the effect of gender feedback and the effect of comparison-attribute feedback on subsequent "
            "selections might be driven by differences in perceived importance of the attribute "
            "summarized in the feedback. As reported in Appendix Section S[X], all three comparison "
            "attributes were rated as equally or more important than gender, suggesting that the "
            "perceived importance of the feedback target cannot account for our Study 1 effects."
        ),
    },
    {
        "tag": "C7",
        "anchor": "See Materials Section: Study 1 in the Appendix for complete study materials.",
        "body": (
            "Create a new Appendix section (placed near the existing Study S4 pretest appendix). All "
            "numbers as X placeholders until pre-registered data arrive:\n\n"
            "Appendix Section S[X]: Study 1 Attribute-Importance Pretest\n\n"
            "Prior to running Study 1, we pre-registered ([link to AsPredicted]) an attribute-importance "
            "pretest to address the concern that any observed asymmetry between feedback about gender "
            "and feedback about the three comparison attributes used in Study 1 might be driven by "
            "participants viewing gender as more important than the comparison attributes.\n\n"
            "Participants. We recruited N = X US-based, English-speaking participants from Prolific "
            "(90% approval rate, ≥1,000 completions; demographic breakdown to be reported when data are "
            "collected). Participants were excluded only if they exited the survey before being assigned "
            "to an experimental condition.\n\n"
            "Procedure. Participants imagined working as NPR producers selecting expert sources for "
            "stories about the future of work in America, mirroring the Study 1 cover story. They were "
            "randomly assigned with equal probability to rate the importance of one of four expert "
            "attributes: the percentage of experts who (1) were women, (2) were under 50 years old, "
            "(3) were based on the West Coast of the United States, or (4) worked at a university. The "
            "single dependent measure was a 7-point importance rating (1 = “Not at all important,” "
            "7 = “Very important”).\n\n"
            "Analyses and results. Following our pre-registration, we conducted three two-sided Welch "
            "t-tests comparing the Gender condition against each comparison condition (under 50; West "
            "Coast; university). To directly support the “equally important” portion of the prediction, "
            "we also conducted three one-sided non-inferiority tests using the two one-sided tests "
            "(TOST) procedure (Schuirmann, 1987; Lakens, 2017; Lakens, Scheel, & Isager, 2018) with a "
            "pre-specified smallest-effect-size-of-interest (SESOI) of d = 0.4. As shown in Appendix "
            "Table S[X], [X result summary]. These results suggest that the perceived importance of the "
            "comparison attributes cannot account for the observed asymmetry between feedback about "
            "gender and feedback about comparison attributes on subsequent selections in Study 1."
        ),
    },
    {
        "tag": "C8",
        "anchor": "In addition, Study 4B established that the effect of gender feedback was not moderated by political ideology nor party affiliation.",
        "body": (
            "Insert after this sentence (before “Finally, it ruled out...”):\n\n"
            "Although we are cautious about over-interpreting a null interaction, this pattern is "
            "suggestive that descriptive feedback operates across partisan lines, an interpretation "
            "that converges with field-experiment evidence that women and racial minorities benefited "
            "from explicitly naming their demographic identity when contacting Republican and "
            "Democratic city councilors alike, an effect mediated by motivation to respond without "
            "prejudice (Kirgios et al., 2022)."
        ),
    },
    {
        "tag": "C9",
        "anchor": "Finally, it ruled out the possibility that our effects were driven by perceptions of which attributes were considered most important.",
        "body": (
            "Attach a new footnote to this sentence:\n\n"
            "We do not assume that participants' initial selections reflect a normatively appropriate "
            "level of representation for the comparison attributes in our paradigm, nor do we assume "
            "that there is a correct base rate for any attribute in the absence of an explicit goal or "
            "prescription. We interpret the effect of gender and racial feedback as evidence that those "
            "dimensions carry an implicit injunctive norm that the feedback calls into question, not as "
            "evidence that participants under-selected women or racial minorities relative to an "
            "objectively correct base rate. See the Introduction (p. X) for the parallel theoretical "
            "statement."
        ),
    },
    {
        "tag": "C10+C11",
        "anchor": "(Kluger & DeNisi, 1996; Schultz et al., 2007)",
        "body": (
            "Expand the parenthetical to:\n"
            "(Kluger & DeNisi, 1996; Locke & Latham, 2019; Schultz et al., 2007; Schultz et al., 2018)\n\n"
            "Then insert three sentences later in the same paragraph, after the sentence ending "
            "“...internal and external motivation to respond without prejudice.” and before “In the "
            "context of diversity, an explicit goal, target, social norm or injunctive norm does not "
            "need to be coupled...”:\n\n"
            "External motivation to respond without prejudice operates when selection decisions are "
            "observable to colleagues, candidates, supervisors, customers, employees, or stakeholders, "
            "and is sustained primarily by anti-discrimination law and broader reputational pressures. "
            "Although many U.S. firms have recently pulled back from their public DEI rhetoric, such "
            "rhetoric is best understood as one downstream signal of these broader pressures rather "
            "than as their source (Levi & Fried, 2024), so the external pathway should remain operative "
            "even as organizational DEI commitments ebb and flow. Internal motivation to respond "
            "without prejudice, by contrast, reflects evaluators' own egalitarian commitments and "
            "desire to view themselves as non-prejudiced decision makers."
        ),
    },
    {
        "tag": "C12",
        "anchor": "However, our theorizing suggests that descriptive feedback should influence behavior in any domain where the attributes it summarizes relate to similar implicit injunctive norms. Future research on this would be valuable.",
        "body": (
            "Append three sentences after “Future research on this would be valuable.”:\n\n"
            "Organizational DEI climate is a parallel scope condition. Descriptive feedback should "
            "matter most in organizations where past selection patterns raise active concerns about "
            "prejudice, because decisions are visible, egalitarian values are endorsed, or employees "
            "and stakeholders continue to treat race and gender representation as consequential. It "
            "should be weaker, and may not arise, in settings where both external accountability and "
            "internal egalitarian concerns are muted or openly rejected. Future work should directly "
            "test how descriptive feedback effects vary across organizations that differ in DEI climate."
        ),
    },
    {
        "tag": "C13",
        "anchor": "future research could explore the precise threshold at which feedback shifts from triggering motivation to respond without prejudice to triggering fairness concerns, though this threshold is likely context dependent.",
        "body": (
            "Append two sentences after this sentence:\n\n"
            "Study 5 also suggests that fairness norms may not be confined to the overrepresentation "
            "case, since fairness concerns significantly mediated responses when women were "
            "underrepresented as well. Future research could examine when concerns about appearing "
            "prejudiced and broader concerns about appropriate representation work together, and when "
            "the relative weight of these motives depends on feedback framing or organizational climate."
        ),
    },
    {
        "tag": "C14",
        "anchor": "Imai, K., Keele, L., & Tingley, D. (2010). A general approach to causal mediation analysis. Psychological Methods, 15(4), 309",
        "body": (
            "Add the following references alphabetically. All DOIs verified except the two FORMAT TBD "
            "entries at the bottom.\n\n"
            "Andor, M. A., Gerster, A., Peters, J., & Schmidt, C. M. (2020). Social norms and energy "
            "conservation beyond the US. Journal of Environmental Economics and Management, 103, "
            "102351. https://doi.org/10.1016/j.jeem.2020.102351\n\n"
            "Bentley University & Gallup. (2025). Bentley-Gallup Business in Society Survey: 2025 "
            "report. Gallup. https://www.gallup.com/file/analytics/696014/"
            "Gallup-Bentley-University_Business-In-Society%20Survey_2025%20Report.pdf\n\n"
            "CBS News. (2025, January 24). Costco shareholders reject an anti-DEI measure, after "
            "Walmart and others end diversity programs. "
            "https://www.cbsnews.com/news/costco-dei-policy-board-statement-shareholder-meeting-vote/\n\n"
            "Chen, X., Latham, G. P., Piccolo, R. F., & Itzchakov, G. (2020). An enumerative review and "
            "a meta-analysis of primed goal effects on organizational behavior. Applied Psychology, "
            "70(1), 216-253. https://doi.org/10.1111/apps.12239\n\n"
            "Devine, P. G., & Ash, T. L. (2022). Diversity training goals, limitations, and promise: A "
            "review of the multidisciplinary literature. Annual Review of Psychology, 73(1), 403-429. "
            "https://doi.org/10.1146/annurev-psych-060221-122215\n\n"
            "Directive (EU) 2022/2381 of the European Parliament and of the Council of 23 November "
            "2022 on improving the gender balance among directors of listed companies and related "
            "measures. Official Journal of the European Union, L 315, 44-59. "
            "http://data.europa.eu/eli/dir/2022/2381/oj\n\n"
            "Directive (EU) 2023/970 of the European Parliament and of the Council of 10 May 2023 to "
            "strengthen the application of the principle of equal pay for equal work or work of equal "
            "value between men and women through pay transparency and enforcement mechanisms. Official "
            "Journal of the European Union, L 132, 21-44. http://data.europa.eu/eli/dir/2023/970/oj\n\n"
            "Dungan, R. (2025, March 31). Semantics: Has DEI actually gone to ground, or has it just "
            "been rebranded? HR Grapevine. https://www.hrgrapevine.com/us/content/article/"
            "2025-03-31-has-dei-really-gone-to-ground-or-has-it-just-assumed-a-new-identity\n\n"
            "Elias, J., & Palmer, A. (2025, March 30). In Trump era, companies are rebranding DEI "
            "efforts, not giving up. CNBC. "
            "https://www.cnbc.com/2025/03/30/in-trump-era-companies-are-rebranding-dei-efforts-not-giving-up.html\n\n"
            "Henry, M. L., Ferraro, P. J., & Kontoleon, A. (2019). The behavioural effect of electronic "
            "home energy reports: Evidence from a randomised field trial in the United States. Energy "
            "Policy, 132, 1256-1261. https://doi.org/10.1016/j.enpol.2019.06.039\n\n"
            "Itzchakov, G., & Latham, G. P. (2018). The moderating effect of performance feedback and "
            "the mediating effect of self-set goals on the primed goal-performance relationship. "
            "Applied Psychology, 69(2), 379-414. https://doi.org/10.1111/apps.12176\n\n"
            "Lakens, D. (2017). Equivalence tests: A practical primer for t tests, correlations, and "
            "meta-analyses. Social Psychological and Personality Science, 8(4), 355-362. "
            "https://doi.org/10.1177/1948550617697177\n\n"
            "Lakens, D., Scheel, A. M., & Isager, P. M. (2018). Equivalence testing for psychological "
            "research: A tutorial. Advances in Methods and Practices in Psychological Science, 1(2), "
            "259-269. https://doi.org/10.1177/2515245918770963\n\n"
            "Levi, A., & Fried, Y. (2024). Diversity, equity, and inclusion programs' emphasis on "
            "symbolism: Causes and consequences. Journal of Organizational Behavior, 46(1), 172-187. "
            "https://doi.org/10.1002/job.2834\n\n"
            "Locke, E. A., & Latham, G. P. (2019). The development of goal setting theory: A half "
            "century retrospective. Motivation Science, 5(2), 93-105. "
            "https://doi.org/10.1037/mot0000127\n\n"
            "Marketplace. (2025, May 22). Costco doubles down on DEI and benefits. Marketplace. "
            "https://www.marketplace.org/story/2025/05/22/costco-doubles-down-on-dei-and-benefits\n\n"
            "Mertens, S. N., & Schultz, P. W. (2021). Referent group specificity: Optimizing normative "
            "feedback to increase residential recycling. Journal of Environmental Psychology, 73, "
            "101541. https://doi.org/10.1016/j.jenvp.2020.101541\n\n"
            "Mignano, V. (2024). Gender Pay Gap: The Protection of the Right to Equal Pay under the "
            "Pay Transparency Directive. Zeitschrift für Europarechtliche Studien, 27(3), 371-401. "
            "https://doi.org/10.5771/1435-439X-2024-3-371\n\n"
            "Ressia, S. (2024). Addressing gender inequality issues in Australia: An annual review of "
            "developments. Journal of Industrial Relations, 66(5), 742-758. "
            "https://doi.org/10.1177/00221856241295490\n\n"
            "Schuirmann, D. J. (1987). A comparison of the two one-sided tests procedure and the power "
            "approach for assessing the equivalence of average bioavailability. Journal of "
            "Pharmacokinetics and Biopharmaceutics, 15(6), 657-680. https://doi.org/10.1007/BF01068419\n\n"
            "Schultz, P. W., Nolan, J. M., Cialdini, R. B., Goldstein, N. J., & Griskevicius, V. "
            "(2018). The constructive, destructive, and reconstructive power of social norms: Reprise. "
            "Perspectives on Psychological Science, 13(2), 249-254. "
            "https://doi.org/10.1177/1745691617693325\n\n"
            "FORMAT TBD (JC to verify before submission):\n"
            "Marketplace (2026) reporting on firm-level DEI rebranding (Nationwide/UPS).\n"
            "Paradigm (2025) Fortune 100 DEI/belonging language analysis."
        ),
    },
    {
        "tag": "C15",
        "anchor": "Study 1 is excluded because participants did not generate their own initial selections (the initial portfolio was stimulus-sampled), and Study 5 is excluded because its design varies the initial candidate pool's composition rather than holding it constant.",
        "body": (
            "Remove this sentence from the Table 2 footnote. Table 2 itself includes Study 1; the "
            "exclusion language belongs with the new zero-baseline pooled analysis, not Table 2.\n\n"
            "Then create a new Appendix section for the pooled analysis that lists all three exclusions "
            "together:\n\n"
            "Appendix Section S[Y]: Zero-Initial Pooled Analysis\n\n"
            "Across studies, some participants made initial selections that contained zero people, "
            "books, or films with a given attribute (e.g., a set of films in Study 3A that included 0 "
            "films released after 2010, or a set of books in Study 4B with 0 books by women authors). "
            "Assuming that selecting 0 candidates with a given attribute would always signal "
            "underrepresentation of that attribute in the initial selections, we test the hypothesis "
            "that descriptive feedback shifts subsequent selections to make up for underrepresentation, "
            "regardless of whether the attribute is race/gender or a non-demographic comparison "
            "attribute. To examine this hypothesis, we pooled data from Studies 2, 3A, 3B, and 4B, "
            "clustering standard errors by participant. Study 1 is excluded because participants did "
            "not generate their own initial selections (the initial portfolio was stimulus-sampled); "
            "Study 4A is excluded because no participants in that study made initial selections "
            "containing zero of any comparison attribute, leaving no comparable zero-baseline cell; and "
            "Study 5 is excluded because its design varies the initial candidate pool's composition "
            "rather than holding it constant.\n\n"
            "Among participants who initially selected zero women or racial minorities, race and "
            "gender feedback significantly increased the probability of selecting a woman or racial "
            "minority as the final selectee, respectively, by 22.50 percentage points (p < .001, "
            "N = 864). Among participants who initially selected zero candidates with any of the "
            "comparison attributes under study (e.g., budget, year of release, protagonist's "
            "profession, multi-genre authorship), feedback informing them that 0% of their selectees "
            "had those other attributes did not significantly increase their probability of "
            "subsequently selecting a candidate with the relevant attribute (estimated effect = +7.25 "
            "percentage points; p = .19, N = 414). A Wald test of the difference between the two "
            "pooled coefficients confirms that the pooled effect of feedback on subsequent choices is "
            "significantly larger when feedback is about the race or gender of initial selections than "
            "when it is about other features of initial selections (p = .016, 95% CI [+2.87, +27.63])."
        ),
    },
]


def verify_anchors(text: str, changes) -> list[tuple[str, int]]:
    results = []
    for c in changes:
        count = text.count(c["anchor"])
        results.append((c["tag"], count))
    return results


def list_all_comments(drive, file_id: str) -> list[dict]:
    out = []
    page_token = None
    while True:
        resp = drive.comments().list(
            fileId=file_id,
            fields="nextPageToken,comments(id,author,content,resolved,quotedFileContent)",
            includeDeleted=False,
            pageSize=100,
            pageToken=page_token,
        ).execute()
        out.extend(resp.get("comments", []))
        page_token = resp.get("nextPageToken")
        if not page_token:
            break
    return out


def cleanup_v3_plan_comments(drive, file_id: str) -> int:
    """Delete any comment whose body still contains the legacy '[v3-plan ' tag."""
    deleted = 0
    for c in list_all_comments(drive, file_id):
        body = c.get("content") or ""
        if "[v3-plan " in body:
            drive.comments().delete(fileId=file_id, commentId=c["id"]).execute()
            deleted += 1
    return deleted


def post_comment(drive, file_id: str, anchor_text: str, body: str) -> dict:
    anchor_obj = {"r": "head", "a": [{"txt": {"v": anchor_text}}]}
    return drive.comments().create(
        fileId=file_id,
        body={
            "content": body,
            "anchor": json.dumps(anchor_obj),
            "quotedFileContent": {
                "value": anchor_text,
                "mimeType": "text/plain",
            },
        },
        fields="id,anchor,quotedFileContent,content,resolved",
    ).execute()


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--dry-run", action="store_true", help="verify only, no API calls")
    parser.add_argument("--cleanup", action="store_true", help="delete existing [v3-plan ...] comments")
    parser.add_argument("--post", action="store_true", help="post all comments")
    parser.add_argument("--replace", action="store_true", help="with --post: cleanup first")
    args = parser.parse_args()

    if not (args.dry_run or args.cleanup or args.post):
        parser.error("must pass --dry-run, --cleanup, or --post")

    manuscript_text = MANUSCRIPT_TXT.read_text(encoding="utf-8")
    counts = verify_anchors(manuscript_text, CHANGES)

    print("=== Anchor verification ===")
    bad = []
    for tag, count in counts:
        status = "OK" if count == 1 else f"BAD ({count} matches)"
        print(f"  {tag:8s}  {status}")
        if count != 1:
            bad.append(tag)
    if bad:
        print(f"\nERROR: {len(bad)} anchor(s) did not match exactly once. Fix CHANGES before posting.")
        sys.exit(1)
    print("All anchors verified.\n")

    if args.dry_run:
        print("=== Planned comments (dry-run) ===")
        for c in CHANGES:
            print(f"--- {c['tag']} ---")
            print(f"anchor: {c['anchor'][:90]}{'...' if len(c['anchor']) > 90 else ''}")
            print(f"body  : {c['body'][:160].replace(chr(10), ' / ')}...")
            print()
        print(f"Total: {len(CHANGES)} comments would be posted.")
        return

    creds = get_creds()
    drive = build("drive", "v3", credentials=creds)

    if args.cleanup or (args.post and args.replace):
        n = cleanup_v3_plan_comments(drive, DOC_ID)
        print(f"Deleted {n} legacy [v3-plan ...] comments.")
        if args.cleanup and not args.post:
            return

    if args.post:
        print(f"About to post {len(CHANGES)} comment(s).")
        posted = []
        for c in CHANGES:
            try:
                result = post_comment(drive, DOC_ID, c["anchor"], c["body"])
                posted.append((c["tag"], result.get("id")))
                anchored = bool(result.get("anchor") and result.get("quotedFileContent"))
                tag = "anchored" if anchored else "UNANCHORED"
                print(f"  [{c['tag']}] {tag}  id={result.get('id')}")
            except HttpError as e:
                print(f"  [{c['tag']}] HttpError: {e}")
            except Exception as e:
                print(f"  [{c['tag']}] error: {e}")
        print(f"\nPosted {len(posted)} comments.")


if __name__ == "__main__":
    main()
