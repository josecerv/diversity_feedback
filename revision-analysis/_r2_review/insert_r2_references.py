"""Insert ~21 new R2 references directly into the manuscript Gdoc References
section, alphabetically. Each new entry is inserted at the startIndex of its
alphabetical successor (so it appears immediately before that successor).
Inserts are sorted by descending index so earlier insertions don't shift later
target indices."""
from __future__ import annotations
import json
import sys
from google.oauth2.credentials import Credentials
from google.auth.transport.requests import Request
from googleapiclient.discovery import build

DOC_ID = "1H9jAvqG5CzQe_BksjkNmLwjLZSlAHvaEK1iyuHL23FI"
TOKEN_PATH = r"C:/Users/jcerv/.config/gws/token.json"

# Each item: (successor_paragraph_prefix, new_reference_text)
# successor_paragraph_prefix must uniquely identify the existing paragraph
# immediately AFTER which the new reference should NOT appear -- rather, the
# new reference appears BEFORE this paragraph (at its startIndex).
NEW_REFS = [
    (
        "Angrist, J. D., & Pischke, J.-S. (2009)",
        "Andor, M. A., Gerster, A., Peters, J., & Schmidt, C. M. (2020). Social norms and energy conservation beyond the US. Journal of Environmental Economics and Management, 103, 102351. https://doi.org/10.1016/j.jeem.2020.102351",
    ),
    (
        "Bicchieri, C., & Xiao, E. (2009)",
        "Bentley University & Gallup. (2025). Bentley-Gallup Business in Society Survey: 2025 report. Gallup. https://www.gallup.com/file/analytics/696014/Gallup-Bentley-University_Business-In-Society%20Survey_2025%20Report.pdf",
    ),
    (
        "Cervantez, J. A., & Milkman, K. L. (2024)",
        "CBS News. (2025, January 24). Costco shareholders reject an anti-DEI measure, after Walmart and others end diversity programs. https://www.cbsnews.com/news/costco-dei-policy-board-statement-shareholder-meeting-vote/",
    ),
    (
        "Cialdini, R. B., Demaine, L. J., Sagarin",
        "Chen, X., Latham, G. P., Piccolo, R. F., & Itzchakov, G. (2020). An enumerative review and a meta-analysis of primed goal effects on organizational behavior. Applied Psychology, 70(1), 216-253. https://doi.org/10.1111/apps.12239",
    ),
    (
        "Dobbin, F., & Kalev, A. (2016)",
        "Devine, P. G., & Ash, T. L. (2022). Diversity training goals, limitations, and promise: A review of the multidisciplinary literature. Annual Review of Psychology, 73(1), 403-429. https://doi.org/10.1146/annurev-psych-060221-122215",
    ),
    (
        "Dobbin, F., & Kalev, A. (2016)",
        "Directive (EU) 2022/2381 of the European Parliament and of the Council of 23 November 2022 on improving the gender balance among directors of listed companies and related measures. Official Journal of the European Union, L 315, 44-59. http://data.europa.eu/eli/dir/2022/2381/oj",
    ),
    (
        "Dobbin, F., & Kalev, A. (2016)",
        "Directive (EU) 2023/970 of the European Parliament and of the Council of 10 May 2023 to strengthen the application of the principle of equal pay for equal work or work of equal value between men and women through pay transparency and enforcement mechanisms. Official Journal of the European Union, L 132, 21-44. http://data.europa.eu/eli/dir/2023/970/oj",
    ),
    (
        "Ellis, N. T. (2025",
        "Dungan, R. (2025, March 31). Semantics: Has DEI actually gone to ground, or has it just been rebranded? HR Grapevine. https://www.hrgrapevine.com/us/content/article/2025-03-31-has-dei-really-gone-to-ground-or-has-it-just-assumed-a-new-identity",
    ),
    (
        "Ellis, N. T. (2025",
        "Elias, J., & Palmer, A. (2025, March 30). In Trump era, companies are rebranding DEI efforts, not giving up. CNBC. https://www.cnbc.com/2025/03/30/in-trump-era-companies-are-rebranding-dei-efforts-not-giving-up.html",
    ),
    (
        "Howard, K. A., Cervone, D., & Motyl",
        "Henry, M. L., Ferraro, P. J., & Kontoleon, A. (2019). The behavioural effect of electronic home energy reports: Evidence from a randomised field trial in the United States. Energy Policy, 132, 1256-1261. https://doi.org/10.1016/j.enpol.2019.06.039",
    ),
    (
        "Jehn, K. A., Northcraft",
        "Itzchakov, G., & Latham, G. P. (2018). The moderating effect of performance feedback and the mediating effect of self-set goals on the primed goal-performance relationship. Applied Psychology, 69(2), 379-414. https://doi.org/10.1111/apps.12176",
    ),
    (
        "Larimer, M. E., Graupensperger",
        "Lakens, D. (2017). Equivalence tests: A practical primer for t tests, correlations, and meta-analyses. Social Psychological and Personality Science, 8(4), 355-362. https://doi.org/10.1177/1948550617697177",
    ),
    (
        "Larimer, M. E., Graupensperger",
        "Lakens, D., Scheel, A. M., & Isager, P. M. (2018). Equivalence testing for psychological research: A tutorial. Advances in Methods and Practices in Psychological Science, 1(2), 259-269. https://doi.org/10.1177/2515245918770963",
    ),
    (
        "Locke, E. A., & Latham, G. P. (2002)",
        "Levi, A., & Fried, Y. (2024). Diversity, equity, and inclusion programs' emphasis on symbolism: Causes and consequences. Journal of Organizational Behavior, 46(1), 172-187. https://doi.org/10.1002/job.2834",
    ),
    (
        "Minkin, R. (2024",
        "Locke, E. A., & Latham, G. P. (2019). The development of goal setting theory: A half century retrospective. Motivation Science, 5(2), 93-105. https://doi.org/10.1037/mot0000127",
    ),
    (
        "Minkin, R. (2024",
        "Marketplace. (2025, May 22). Costco doubles down on DEI and benefits. Marketplace. https://www.marketplace.org/story/2025/05/22/costco-doubles-down-on-dei-and-benefits",
    ),
    (
        "Minkin, R. (2024",
        "Mertens, S. N., & Schultz, P. W. (2021). Referent group specificity: Optimizing normative feedback to increase residential recycling. Journal of Environmental Psychology, 73, 101541. https://doi.org/10.1016/j.jenvp.2020.101541",
    ),
    (
        "Minkin, R. (2024",
        "Mignano, V. (2024). Gender Pay Gap: The Protection of the Right to Equal Pay under the Pay Transparency Directive. Zeitschrift für Europarechtliche Studien, 27(3), 371-401. https://doi.org/10.5771/1435-439X-2024-3-371",
    ),
    (
        "Richard, O. C. (2000)",
        "Ressia, S. (2024). Addressing gender inequality issues in Australia: An annual review of developments. Journal of Industrial Relations, 66(5), 742-758. https://doi.org/10.1177/00221856241295490",
    ),
    (
        "Schultz, P. W., Nolan, J. M., Cialdini, R. B., Goldstein, N. J., & Griskevicius, V. (2007)",
        "Schuirmann, D. J. (1987). A comparison of the two one-sided tests procedure and the power approach for assessing the equivalence of average bioavailability. Journal of Pharmacokinetics and Biopharmaceutics, 15(6), 657-680. https://doi.org/10.1007/BF01068419",
    ),
    (
        "Shilts, M. K., Horowitz",
        "Schultz, P. W., Nolan, J. M., Cialdini, R. B., Goldstein, N. J., & Griskevicius, V. (2018). The constructive, destructive, and reconstructive power of social norms: Reprise. Perspectives on Psychological Science, 13(2), 249-254. https://doi.org/10.1177/1745691617693325",
    ),
]


def get_creds():
    with open(TOKEN_PATH) as f:
        tok = json.load(f)
    creds = Credentials(
        token=tok.get("token"), refresh_token=tok["refresh_token"], token_uri=tok["token_uri"],
        client_id=tok["client_id"], client_secret=tok["client_secret"], scopes=tok["scopes"],
    )
    if not creds.valid:
        creds.refresh(Request())
    return creds


def find_paragraph_start_index(doc, prefix: str) -> int | None:
    """Walk doc.body.content; return the startIndex of the FIRST paragraph whose
    concatenated textRun contents start with the given prefix."""
    for el in doc["body"]["content"]:
        para = el.get("paragraph")
        if not para:
            continue
        para_text = "".join(
            run.get("textRun", {}).get("content", "")
            for run in para.get("elements", [])
        )
        if para_text.startswith(prefix):
            return el.get("startIndex")
    return None


def main():
    creds = get_creds()
    docs = build("docs", "v1", credentials=creds)
    doc = docs.documents().get(documentId=DOC_ID).execute()

    print("=== Locating successor paragraphs ===")
    plans = []
    for prefix, new_ref in NEW_REFS:
        start = find_paragraph_start_index(doc, prefix)
        if start is None:
            print(f"  MISS  prefix={prefix!r}")
            sys.exit(1)
        plans.append((start, new_ref, prefix))
        print(f"  OK    @{start}  before {prefix!r}")

    # Group by startIndex: multiple new refs may share a successor (e.g., several
    # entries inserted before "Dobbin"). When that happens we want them to land
    # in alphabetical order in the final doc. Since later inserts at the same
    # index push earlier ones DOWN (each insert at index N puts new text at N,
    # shifting any existing text including earlier inserts forward), inserting
    # in REVERSE alphabetical order within a group, all at the same index,
    # yields forward alphabetical order in the result.
    from collections import defaultdict
    grouped = defaultdict(list)
    for start, ref, prefix in plans:
        grouped[start].append(ref)
    # Within each group, sort refs reverse-alphabetically so reverse insertion
    # produces forward-alphabetical output.
    for start in grouped:
        grouped[start].sort(reverse=True)

    # Build requests sorted by descending startIndex (process bottom of doc
    # first so earlier indices remain valid).
    requests = []
    for start in sorted(grouped.keys(), reverse=True):
        for ref in grouped[start]:  # already reverse-alphabetical within group
            requests.append({
                "insertText": {
                    "location": {"index": start},
                    "text": ref + "\n",
                }
            })

    print(f"\n=== Executing batchUpdate with {len(requests)} insertText requests ===")
    result = docs.documents().batchUpdate(
        documentId=DOC_ID,
        body={"requests": requests},
    ).execute()
    print(f"OK -- {len(result.get('replies', []))} replies")


if __name__ == "__main__":
    main()
