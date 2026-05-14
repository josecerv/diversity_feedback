"""Apply the v3 cover letter + three sub-paragraph responses + XX/XXX fills + Table 2 drops to the R2 response-letter Gdoc.

Strategy:
1. Fetch the doc as JSON.
2. Locate (a) the opening 3-paragraph block "Dear Dr. Wu, ... differentiation."
   and (b) the old 4-bullet summary block at current lines 18-22, by walking
   paragraphs and matching opening text against known anchors.
3. Build a single batchUpdate request with operations ordered so index-based
   ops are in descending order:
     - deleteContentRange on the old 4-bullet block
     - deleteContentRange on the old opening (then insertText with the new
       cover letter at the now-empty index)
     - replaceAllText for each RESPONSE: [] placeholder (disambiguated by
       preceding context)
     - replaceAllText for XXX → Schultz et al. (2007), THEN XX → Locke and
       Latham (2002) (XXX first so XX replace does not eat part of XXX).
     - replaceAllText for the Table 2 sentence in the streamlining response.

Run from the repo root or anywhere; paths are absolute.
"""
from __future__ import annotations

import json
import pathlib
import sys

from googleapiclient.discovery import build
from google.oauth2.credentials import Credentials
from google.auth.transport.requests import Request


DOC_ID = "1YBHCMwD84xC3Qw38CPEzz1iv3JKg7Mw9jTJqyZGptkI"
TOKEN = pathlib.Path(r"C:/Users/jcerv/.config/gws/token.json")


def get_creds():
    data = json.load(open(TOKEN))
    creds = Credentials.from_authorized_user_info(
        data,
        [
            "https://www.googleapis.com/auth/documents",
            "https://www.googleapis.com/auth/drive",
        ],
    )
    if not creds.valid and creds.refresh_token:
        creds.refresh(Request())
    return creds


# ---------------------------------------------------------------------------
# Content
# ---------------------------------------------------------------------------

# Cover letter body. Paragraph breaks shown as literal newlines; the Gdoc API
# turns each "\n" in insertText into a paragraph break.
COVER_LETTER = (
    "May X, 2026\n"
    "George Wu, Department Editor, Management Science\n"
    "Re: MS‑BDE‑2025‑01479, “Does Feedback Enhance Diversity in Selection Decisions?”\n"
    "Dear Dr. Wu,\n"
    "We thank you for handling our revised manuscript and for the two reviewers’ continued engagement with the paper. "
    "We appreciate the opportunity to address your remaining requests as well as the small comments raised by Reviewers 1 and 2, "
    "and we have worked to strengthen the paper further in response.\n"
    "Here is a summary of the most notable changes we have made in this revision:\n"
    # Bullet 1: references
    "We have modernized the references that anchor our theoretical framing in response to Reviewer 1’s request for more contemporary citations. "
    "Both the Introduction and General Discussion now draw on more recent feedback, goal‑setting, and social‑norms work "
    "(e.g., Locke & Latham, 2019; Itzchakov & Latham, 2018; Chen, Latham, & Piccolo, 2020; Schultz, Nolan, & Cialdini, 2018; "
    "Devine & Ash, 2022; Andor, Gerster, & Peters, 2020; Henry, Ferraro, & Kontoleon, 2019; Mertens & Schultz, 2021), "
    "alongside the foundational citations we retain.\n"
    # Bullet 2: theoretical framing
    "We have clarified our theoretical account of why descriptive feedback can change behavior in this domain even without a paired explicit goal. "
    "Consistent with the broader logic of the feedback literature, feedback regulates behavior only when past performance can be evaluated against a standard. "
    "We now make explicit that this standard need not be supplied by an explicit goal when an implicit injunctive norm is already entrenched, "
    "as we argue is the case for race and gender, where anti‑discrimination law, social and reputational pressures, organizational policy, "
    "and selectors’ personally held egalitarian commitments jointly sustain a norm against prejudiced selection.\n"
    # Bullet 3: sociopolitical
    "We have situated the paper in the current sociopolitical landscape around DEI in response to Reviewer 2’s concern that the prior version read as pre‑2024. "
    "We now acknowledge the recent U.S. retreat from explicit DEI rhetoric while documenting three countervailing patterns that keep our central question relevant: "
    "(a) expanded international transparency requirements (Australia’s Workplace Gender Equality Amendment Act 2023, the EU Pay Transparency Directive 2023/970, "
    "and the EU Women on Boards Directive 2022/2381); "
    "(b) continued U.S. public and stakeholder support for corporate DEI, including the 2025 Bentley/Gallup survey reporting that 69% of U.S. adults believe businesses should promote DEI "
    "and recent defeats of anti‑DEI shareholder proposals at Costco, Apple, Levi’s, John Deere, and Goldman Sachs; "
    "and (c) firm‑level rebranding of diversity work toward “belonging,” “inclusive culture,” and related framings rather than wholesale dismantling.\n"
    # Bullet 4: scope / dual pathways
    "We have tightened the scope of our theoretical claims and articulated more explicit boundary conditions for when our effect should and should not generalize, in response to Reviewer 2. "
    "We now distinguish two pathways through which descriptive feedback can shift selection decisions: an external motivation to respond without prejudice, "
    "sustained by anti‑discrimination law, the observability of selection decisions, and broader stakeholder pressures that operate beyond any single firm’s stated DEI commitments; "
    "and an internal motivation to respond without prejudice, rooted in selectors’ personally held egalitarian self‑image, which should be less malleable to changes in organizational climate. "
    "We expect our effect to generalize where selections are observable and selectors hold at least some egalitarian commitments, "
    "and to attenuate in fully private or anonymous selection contexts and in organizational settings that explicitly disavow egalitarian norms. "
    "We now flag organizational DEI climate as a scope condition and direction for future research in the General Discussion, "
    "alongside our existing discussions of public versus private feedback delivery and attribute visibility.\n"
    # Bullet 5: empirical requests (with the "why zero" rationale)
    "We have addressed your two empirical requests. "
    "First, the new Study 1 importance post‑test (N = 300) confirms that differences in perceived attribute importance cannot explain why only gender feedback moved subsequent selections: "
    "two of the three comparison attributes were rated significantly more important than gender, and the third did not differ. "
    "Second, to address the alternative that comparison attributes are already selected at appropriate levels initially, "
    "we restrict to participants whose initial selections contained zero of the focal attribute and report a new pooled analysis across Studies 2, 3A, 3B, 4A, and 4B (response‑letter Table R1). "
    "Because we cannot observe each participant’s private view of the “appropriate” level for a given attribute, "
    "zero is a useful but imperfect floor that lets us hold a common starting point across focal and comparison attributes. "
    "If feedback merely corrected under‑selection relative to private benchmarks, it should move participants for both race/gender and comparison attributes in this subset. "
    "Instead, race and gender feedback increases subsequent selection of the focal attribute (+21.28 pp, p < .001, N = 1,222), "
    "whereas comparison‑attribute feedback does not reliably do so (+6.83 pp, p = .21, N = 429); the Wald‑test difference is +14.46 pp (p = .016). "
    "We treat this analysis as imperfect but directional evidence against the alternative that participants were simply selecting comparison attributes at appropriate levels to begin with.\n"
    # Bullet 6: streamlining (Table 2 dropped per JC)
    "We have streamlined the empirical sections of the paper following your guidance on length and repetition. "
    "The revised manuscript relegates the regression specifications underlying the secondary feedback‑about‑race/gender‑versus‑other‑attribute comparisons to a dedicated Appendix section, "
    "with explicit cross‑references from each Results section, and trims redundant Methods and Results language across later studies.\n"
    # Closing
    "Below we reproduce your letter and each of the reviews in their entirety, and we respond to every point made. "
    "We have provided normal text from the review team and our responses appear in bolded font for ease of differentiation.\n"
    "Thank you again for giving us the opportunity to revise our manuscript."
)


B1_TEXT = (
    "Thank you for the clear guidance and for offering to handle this revision yourself. "
    "A full overview of the most notable changes appears in the cover‑letter summary above, "
    "and our detailed point‑by‑point replies to Reviewers 1 and 2 appear below."
)


B2_TEXT = (
    "We have addressed both points. "
    "The Introduction and General Discussion now draw on more contemporary work in the feedback, goal‑setting, and social‑norms literatures "
    "(e.g., Locke & Latham, 2019; Itzchakov & Latham, 2018; Chen, Latham, & Piccolo, 2020; Schultz, Nolan, & Cialdini, 2018; "
    "Devine & Ash, 2022; Andor, Gerster, & Peters, 2020; Henry, Ferraro, & Kontoleon, 2019; Mertens & Schultz, 2021), "
    "alongside the foundational citations we retain. "
    "We have also expanded the Introduction’s theoretical framing (pp. X‑X) to clarify why descriptive feedback can shift selection decisions about women and underrepresented minorities even without a paired explicit goal: "
    "in this setting, we argue, feedback makes prior behavior evaluable against an already‑entrenched implicit injunctive norm against prejudiced selection. "
    "We address Reviewer 1’s specific suggestions in detail below."
)


B3_TEXT = (
    "We have addressed these points in the Introduction (pp. X) and General Discussion. "
    "To avoid making the paper read as if it were written in a pre‑2024 period, "
    "we now acknowledge the recent U.S. retreat from explicit DEI rhetoric while also documenting countervailing patterns that keep the paper’s central question relevant: "
    "expanded international transparency requirements, continued U.S. public and stakeholder support for corporate DEI, "
    "and firm‑level rebranding of diversity work toward “belonging,” “inclusive culture,” and related framings rather than wholesale dismantling.\n"
    "We have also made the theoretical scope conditions more explicit. In particular, we now distinguish an external motivation to respond without prejudice, "
    "which is shaped by firm norms but also by anti‑discrimination law, observability, employee and stakeholder scrutiny, and broader reputational pressures, "
    "from an internal motivation to respond without prejudice, rooted in selectors’ personally held egalitarian beliefs and self‑image. "
    "We therefore expect the effect to be stronger when selection patterns raise active concerns about prejudice, and weaker when those concerns are absent, muted, or explicitly rejected. "
    "Consistent with your note that an additional study is not required, and with your separate request to streamline the manuscript, we engage this tension narratively rather than empirically. "
    "Our point‑by‑point reply to Reviewer 2 provides the supporting detail and citations, "
    "and the General Discussion now flags organizational DEI climate as a scope condition and direction for future research."
)


# ---------------------------------------------------------------------------
# Anchors used to locate structural blocks
# ---------------------------------------------------------------------------

OPENING_FIRST_PARA = "Dear Dr. Wu,"
OPENING_LAST_PARA_PREFIX = "Below we reproduce your letter"

OLD_4BULLET_FIRST_PARA_PREFIX = (
    "RESPONSE: Thank you for providing such clear guidance regarding the next round of review."
)
# Last paragraph of the old block ends with this distinctive phrase
OLD_4BULLET_LAST_PARA_SUFFIX = "or in contexts that explicitly encourage discriminatory behavior."


def walk_paragraphs(content):
    """Yield (paragraph_element, full_text, start_index, end_index) for each
    structural paragraph in the document body."""
    for el in content:
        if "paragraph" not in el:
            continue
        para = el["paragraph"]
        text_parts = []
        for r in para.get("elements", []):
            if "textRun" in r:
                text_parts.append(r["textRun"].get("content", ""))
        text = "".join(text_parts).rstrip("\n")
        yield el, text, el["startIndex"], el["endIndex"]


def find_block_range(content, first_match, last_match, last_is_prefix=False, last_is_suffix=False):
    """Find the [startIndex, endIndex) span covering all paragraphs from the
    first one matching ``first_match`` through the first subsequent paragraph
    matching ``last_match``.

    Matching modes:
      - last_is_prefix=True: last_match is a prefix of the last paragraph's text
      - last_is_suffix=True: last_match is a suffix of the last paragraph's text
      - both False: last_match must equal the paragraph's text exactly
    """
    start = None
    end = None
    found_first = False
    for el, text, si, ei in walk_paragraphs(content):
        if not found_first:
            if text == first_match or text.startswith(first_match):
                start = si
                found_first = True
        if found_first:
            ok = (
                (last_is_prefix and text.startswith(last_match))
                or (last_is_suffix and text.endswith(last_match))
                or (not last_is_prefix and not last_is_suffix and text == last_match)
            )
            if ok:
                end = ei
                break
    if start is None or end is None:
        raise RuntimeError(f"Block not found: first={first_match!r}, last={last_match!r}")
    return start, end


def main():
    creds = get_creds()
    docs = build("docs", "v1", credentials=creds)
    doc = docs.documents().get(documentId=DOC_ID).execute()
    content = doc["body"]["content"]

    # ----- Locate index-based ranges -------------------------------------
    opening_start, opening_end = find_block_range(
        content,
        OPENING_FIRST_PARA,
        OPENING_LAST_PARA_PREFIX,
        last_is_prefix=True,
    )
    bullet_start, bullet_end = find_block_range(
        content,
        OLD_4BULLET_FIRST_PARA_PREFIX,
        OLD_4BULLET_LAST_PARA_SUFFIX,
        last_is_suffix=True,
    )

    print(f"[indices] opening: [{opening_start}, {opening_end})")
    print(f"[indices] old 4-bullet block: [{bullet_start}, {bullet_end})")

    # Validate that the two index-based ops do not overlap and are ordered
    # safely (we want to delete the LATER block first so the earlier indices
    # do not shift).
    assert opening_end <= bullet_start, "Opening block must precede the 4-bullet block"

    # ----- Build batchUpdate requests in safe order ---------------------
    requests = []

    # 1. Delete the LATER block first (4-bullet block), so the earlier
    #    indices remain valid for the next operation.
    requests.append(
        {
            "deleteContentRange": {
                "range": {"startIndex": bullet_start, "endIndex": bullet_end}
            }
        }
    )

    # 2. Delete the earlier opening block.
    requests.append(
        {
            "deleteContentRange": {
                "range": {"startIndex": opening_start, "endIndex": opening_end}
            }
        }
    )

    # 3. Insert the new cover letter at the opening_start index. After the
    #    deletion above, that index is the correct insertion point.
    requests.append(
        {
            "insertText": {
                "location": {"index": opening_start},
                "text": COVER_LETTER + "\n",
            }
        }
    )

    # 4. replaceAllText operations. These do not need index ordering; the API
    #    applies them after the structural inserts/deletes above.
    #    Order XXX before XX so the XX replace does not eat part of XXX.
    requests.extend(
        [
            # Fill RESPONSE: [] placeholders. Use preceding-text context to
            # disambiguate the three identical "RESPONSE: []" tokens.
            {
                "replaceAllText": {
                    "containsText": {
                        "text": "and not send to the reviewers.  \nRESPONSE: []",
                        "matchCase": True,
                    },
                    "replaceText": "and not send to the reviewers.  \nRESPONSE: " + B1_TEXT,
                }
            },
            {
                "replaceAllText": {
                    "containsText": {
                        "text": "and a bit of clarification in your writing. RESPONSE: []",
                        "matchCase": True,
                    },
                    "replaceText": "and a bit of clarification in your writing. RESPONSE: " + B2_TEXT,
                }
            },
            {
                "replaceAllText": {
                    "containsText": {
                        "text": "the issues the reviewer describes.\nRESPONSE: []",
                        "matchCase": True,
                    },
                    "replaceText": "the issues the reviewer describes.\nRESPONSE: " + B3_TEXT,
                }
            },
            # Fill XXX, then XX.
            {
                "replaceAllText": {
                    "containsText": {
                        "text": "alongside the classic XXX",
                        "matchCase": True,
                    },
                    "replaceText": "alongside the classic Schultz et al. (2007)",
                }
            },
            {
                "replaceAllText": {
                    "containsText": {
                        "text": "alongside the classic XX",
                        "matchCase": True,
                    },
                    "replaceText": "alongside the classic Locke and Latham (2002)",
                }
            },
            # Drop the Table 2 sentence from the streamlining response.
            {
                "replaceAllText": {
                    "containsText": {
                        "text": (
                            "To that end, and following your guidance, we have introduced a new "
                            "consolidated, cross-study table (Table 2) summarizing the secondary "
                            "comparisons of the relative effects of feedback about race/gender vs. "
                            "other attributes in a centralized location. Moreover, we relegated "
                            "the reporting of the regression specifications behind those "
                            "comparisons to a dedicated section of the Appendix, with explicit "
                            "references from each Results section."
                        ),
                        "matchCase": True,
                    },
                    "replaceText": (
                        "To that end, and following your guidance, we have relegated the reporting "
                        "of the regression specifications behind the secondary feedback-about-"
                        "race/gender-versus-other-attribute comparisons to a dedicated section of "
                        "the Appendix, with explicit references from each Results section."
                    ),
                }
            },
        ]
    )

    # Diagnostic print of what we are about to do.
    print(f"[batch] requests queued: {len(requests)}")
    for i, r in enumerate(requests):
        print(f"  [{i}] {next(iter(r))}")

    if "--dry-run" in sys.argv:
        print("[dry-run] not sending batchUpdate")
        return

    result = (
        docs.documents()
        .batchUpdate(documentId=DOC_ID, body={"requests": requests})
        .execute()
    )

    # Sanity-check replaceAllText results: each should have replaced 1
    # occurrence.
    replies = result.get("replies", [])
    print("[result] reply count:", len(replies))
    for i, reply in enumerate(replies):
        if "replaceAllText" in reply:
            n = reply["replaceAllText"].get("occurrencesChanged", 0)
            print(f"  [{i}] replaceAllText occurrencesChanged={n}")
        else:
            print(f"  [{i}] {list(reply.keys())[0] if reply else '<empty>'}")


if __name__ == "__main__":
    main()
