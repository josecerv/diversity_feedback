"""Post the References (C14) change as two anchored comments on the same anchor
because the full list exceeds Drive's 4096-byte single-comment limit."""
from __future__ import annotations
import json
from google.oauth2.credentials import Credentials
from google.auth.transport.requests import Request
from googleapiclient.discovery import build

DOC_ID = "1H9jAvqG5CzQe_BksjkNmLwjLZSlAHvaEK1iyuHL23FI"
TOKEN_PATH = r"C:/Users/jcerv/.config/gws/token.json"

ANCHOR = "Imai, K., Keele, L., & Tingley, D. (2010). A general approach to causal mediation analysis. Psychological Methods, 15(4), 309"

BODY_PART_1 = (
    "Add the following references alphabetically (part 1 of 2). All DOIs verified.\n\n"
    "Andor, M. A., Gerster, A., Peters, J., & Schmidt, C. M. (2020). Social norms and energy "
    "conservation beyond the US. Journal of Environmental Economics and Management, 103, 102351. "
    "https://doi.org/10.1016/j.jeem.2020.102351\n\n"
    "Bentley University & Gallup. (2025). Bentley-Gallup Business in Society Survey: 2025 report. "
    "Gallup. https://www.gallup.com/file/analytics/696014/"
    "Gallup-Bentley-University_Business-In-Society%20Survey_2025%20Report.pdf\n\n"
    "CBS News. (2025, January 24). Costco shareholders reject an anti-DEI measure, after Walmart "
    "and others end diversity programs. "
    "https://www.cbsnews.com/news/costco-dei-policy-board-statement-shareholder-meeting-vote/\n\n"
    "Chen, X., Latham, G. P., Piccolo, R. F., & Itzchakov, G. (2020). An enumerative review and a "
    "meta-analysis of primed goal effects on organizational behavior. Applied Psychology, 70(1), "
    "216-253. https://doi.org/10.1111/apps.12239\n\n"
    "Devine, P. G., & Ash, T. L. (2022). Diversity training goals, limitations, and promise: A "
    "review of the multidisciplinary literature. Annual Review of Psychology, 73(1), 403-429. "
    "https://doi.org/10.1146/annurev-psych-060221-122215\n\n"
    "Directive (EU) 2022/2381 of the European Parliament and of the Council of 23 November 2022 on "
    "improving the gender balance among directors of listed companies and related measures. "
    "Official Journal of the European Union, L 315, 44-59. "
    "http://data.europa.eu/eli/dir/2022/2381/oj\n\n"
    "Directive (EU) 2023/970 of the European Parliament and of the Council of 10 May 2023 to "
    "strengthen the application of the principle of equal pay for equal work or work of equal value "
    "between men and women through pay transparency and enforcement mechanisms. Official Journal of "
    "the European Union, L 132, 21-44. http://data.europa.eu/eli/dir/2023/970/oj\n\n"
    "Dungan, R. (2025, March 31). Semantics: Has DEI actually gone to ground, or has it just been "
    "rebranded? HR Grapevine. https://www.hrgrapevine.com/us/content/article/"
    "2025-03-31-has-dei-really-gone-to-ground-or-has-it-just-assumed-a-new-identity\n\n"
    "Elias, J., & Palmer, A. (2025, March 30). In Trump era, companies are rebranding DEI efforts, "
    "not giving up. CNBC. "
    "https://www.cnbc.com/2025/03/30/in-trump-era-companies-are-rebranding-dei-efforts-not-giving-up.html\n\n"
    "Henry, M. L., Ferraro, P. J., & Kontoleon, A. (2019). The behavioural effect of electronic "
    "home energy reports: Evidence from a randomised field trial in the United States. Energy "
    "Policy, 132, 1256-1261. https://doi.org/10.1016/j.enpol.2019.06.039\n\n"
    "Itzchakov, G., & Latham, G. P. (2018). The moderating effect of performance feedback and the "
    "mediating effect of self-set goals on the primed goal-performance relationship. Applied "
    "Psychology, 69(2), 379-414. https://doi.org/10.1111/apps.12176"
)

BODY_PART_2 = (
    "Add the following references alphabetically (part 2 of 2).\n\n"
    "Lakens, D. (2017). Equivalence tests: A practical primer for t tests, correlations, and "
    "meta-analyses. Social Psychological and Personality Science, 8(4), 355-362. "
    "https://doi.org/10.1177/1948550617697177\n\n"
    "Lakens, D., Scheel, A. M., & Isager, P. M. (2018). Equivalence testing for psychological "
    "research: A tutorial. Advances in Methods and Practices in Psychological Science, 1(2), "
    "259-269. https://doi.org/10.1177/2515245918770963\n\n"
    "Levi, A., & Fried, Y. (2024). Diversity, equity, and inclusion programs' emphasis on "
    "symbolism: Causes and consequences. Journal of Organizational Behavior, 46(1), 172-187. "
    "https://doi.org/10.1002/job.2834\n\n"
    "Locke, E. A., & Latham, G. P. (2019). The development of goal setting theory: A half century "
    "retrospective. Motivation Science, 5(2), 93-105. https://doi.org/10.1037/mot0000127\n\n"
    "Marketplace. (2025, May 22). Costco doubles down on DEI and benefits. Marketplace. "
    "https://www.marketplace.org/story/2025/05/22/costco-doubles-down-on-dei-and-benefits\n\n"
    "Mertens, S. N., & Schultz, P. W. (2021). Referent group specificity: Optimizing normative "
    "feedback to increase residential recycling. Journal of Environmental Psychology, 73, 101541. "
    "https://doi.org/10.1016/j.jenvp.2020.101541\n\n"
    "Mignano, V. (2024). Gender Pay Gap: The Protection of the Right to Equal Pay under the Pay "
    "Transparency Directive. Zeitschrift für Europarechtliche Studien, 27(3), 371-401. "
    "https://doi.org/10.5771/1435-439X-2024-3-371\n\n"
    "Ressia, S. (2024). Addressing gender inequality issues in Australia: An annual review of "
    "developments. Journal of Industrial Relations, 66(5), 742-758. "
    "https://doi.org/10.1177/00221856241295490\n\n"
    "Schuirmann, D. J. (1987). A comparison of the two one-sided tests procedure and the power "
    "approach for assessing the equivalence of average bioavailability. Journal of Pharmacokinetics "
    "and Biopharmaceutics, 15(6), 657-680. https://doi.org/10.1007/BF01068419\n\n"
    "Schultz, P. W., Nolan, J. M., Cialdini, R. B., Goldstein, N. J., & Griskevicius, V. (2018). "
    "The constructive, destructive, and reconstructive power of social norms: Reprise. "
    "Perspectives on Psychological Science, 13(2), 249-254. "
    "https://doi.org/10.1177/1745691617693325\n\n"
    "FORMAT TBD (JC to verify before submission):\n"
    "Marketplace (2026) reporting on firm-level DEI rebranding (Nationwide/UPS).\n"
    "Paradigm (2025) Fortune 100 DEI/belonging language analysis."
)

with open(TOKEN_PATH) as f:
    tok = json.load(f)
creds = Credentials(
    token=tok.get("token"), refresh_token=tok["refresh_token"], token_uri=tok["token_uri"],
    client_id=tok["client_id"], client_secret=tok["client_secret"], scopes=tok["scopes"],
)
if not creds.valid:
    creds.refresh(Request())
drive = build("drive", "v3", credentials=creds)

for label, body in [("Part 1", BODY_PART_1), ("Part 2", BODY_PART_2)]:
    size = len(body.encode("utf-8"))
    print(f"{label}: {size} bytes")
    anchor_obj = {"r": "head", "a": [{"txt": {"v": ANCHOR}}]}
    r = drive.comments().create(
        fileId=DOC_ID,
        body={
            "content": body,
            "anchor": json.dumps(anchor_obj),
            "quotedFileContent": {"value": ANCHOR, "mimeType": "text/plain"},
        },
        fields="id,anchor,quotedFileContent",
    ).execute()
    anchored = bool(r.get("anchor") and r.get("quotedFileContent"))
    print(f"  {'anchored' if anchored else 'UNANCHORED'}  id={r.get('id')}")
