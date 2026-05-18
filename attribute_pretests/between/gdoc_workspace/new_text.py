"""All replacement text in one place so the apply script stays clean.

Curly quotes / minus signs preserved. No em-dashes per JC's style memory.
Stats use plain text here; italic ranges are computed in the apply script.
"""

# Section 1: Procedure body (replaces text after italic "Procedure. " prefix).
# Note: leading italic "Procedure. " is preserved by deleting [810, 1578).
SEC1_PROCEDURE_BODY = (
    "All participants first read a brief cover story: “Imagine you work as "
    "a producer at National Public Radio (NPR). As part of your role, you are "
    "responsible for identifying and booking expert sources to interview for "
    "various news segments and stories.” They were then told to imagine "
    "they had been asked to select expert sources for an upcoming NPR news "
    "segment focused on the future of work in America and how artificial "
    "intelligence is reshaping employment across industries. On the next "
    "screen, participants were asked: “When making your selections for "
    "this news segment, how important would it be for you to consider the "
    "expert attribute listed below?” Each participant was then randomly "
    "assigned with equal probability to rate the importance of one of the "
    "four expert attributes featured in Study 1: the percentage of experts "
    "who were women, the percentage of experts who were under 50 years old, "
    "the percentage of experts who were based on the West Coast of the United "
    "States, or the percentage of experts who worked at a university. "
    "Participants then rated the importance of considering their assigned "
    "attribute on a 7-point scale (1 = “Not at all important”, 7 = "
    "“Very important”)."
)

# Section 1: Results body (replaces full paragraph content, keeping trailing \n).
# Reframes the t-tests as comparisons against the women attribute (no "Gender condition").
SEC1_RESULTS_BODY = (
    "Following our pre-registration, we conducted three t-tests comparing the "
    "rated importance of considering whether experts were women (our reference "
    "attribute) against the rated importance of each of the three comparison "
    "attributes. Participants rated the importance of considering whether "
    "experts “worked at a university” (M = 5.04, SD = 1.93) as "
    "significantly higher than the importance of considering whether experts "
    "“were women” (M = 4.28, SD = 2.33; t(142.6) = 2.17, p = .032, "
    "d = 0.36). The importance of considering whether experts “were "
    "under 50 years old” (M = 4.49, SD = 2.17) did not differ "
    "significantly from the importance of considering whether experts "
    "“were women” (M = 4.28, SD = 2.33; t(147.2) = 0.58, p = .563, "
    "d = 0.09). Participants rated the importance of considering whether "
    "experts “were based on the West Coast of the United States” "
    "(M = 3.35, SD = 2.17) as significantly lower than the importance of "
    "considering whether experts “were women” (M = 4.28, SD = 2.33; "
    "t(146.5) = −2.51, p = .013, d = −0.41)."
)

# Section 2: New section title.
SEC2_TITLE = "Ruling Out an Alternative Explanation: Initial Selection Calibration"

# Section 2: Preamble (opens with the manuscript-footnote language JC asked us to borrow).
SEC2_PREAMBLE = (
    "One alternative explanation for the differential effects of descriptive "
    "feedback observed across attributes is that feedback about other "
    "attributes shows smaller effects simply because participants already use "
    "those attributes appropriately in their initial selections, leaving "
    "little room for feedback to move them further. To test this possibility, "
    "we pooled participant-level data from Studies 2, 3A, 3B, and 4B and "
    "restricted our attention to participants whose initial selections "
    "included zero films or books with a given attribute (e.g., a Study 3A "
    "participant whose seven initial films included no films released after "
    "2010, or a Study 4B participant whose six initial books included no "
    "books by women authors). Learning that one’s initial selections "
    "included none of a given attribute is itself a signal of "
    "underrepresentation on that dimension, so if descriptive feedback "
    "simply reflects a reaction to underrepresentation, it should move "
    "participants regardless of which attribute is involved."
)

# Section 2: Methods, paragraph 1 (the per-cell estimation).
SEC2_METHODS_P1 = (
    "We pooled participant-level data from Studies 2, 3A, 3B, and 4B. We "
    "excluded Study 4A from this analysis because no participants in that "
    "study made initial selections containing zero books with one of the "
    "attributes we targeted with feedback besides the author’s race; "
    "the other comparison attributes in Study 4A had near-saturated initial "
    "selection rates (over 90% of participants selected at least one author "
    "with each comparison attribute in their initial portfolio), leaving "
    "fewer than approximately 20 zero-initial participants per comparison "
    "cell—too few for a stable estimate."
)

# Section 2: Methods, paragraph 2 (analysis specification, in JC's voice).
SEC2_METHODS_P2 = (
    "Within each cell defined by a study and a focal attribute, we restricted "
    "the sample to participants whose initial selection set contained zero "
    "options with that attribute (e.g., a Study 3A participant whose seven "
    "initial films contained no films released after 2010, or a Study 4B "
    "participant whose six initial books contained no books by women "
    "authors). Following our pre-registration, we then estimated an ordinary "
    "least squares (OLS) regression with robust standard errors predicting "
    "whether the participant’s next selection had the focal attribute. "
    "Our primary predictor was an indicator for random assignment to receive "
    "descriptive feedback about that focal attribute. When a participant "
    "contributed observations to more than one cell (because, within a study, "
    "a participant could be in the zero-initial subset for more than one "
    "attribute), we clustered standard errors at the participant level."
)

# Section 2: Methods, paragraph 3 (pooling across cells).
SEC2_METHODS_P3 = (
    "To summarize the overall pattern across cells, we then conducted a "
    "mini meta-analysis combining the cell-level estimates (Goh et al., 2016). "
    "Using a random-effects model, which weighs each cell’s contribution "
    "by its standard error, we pooled the estimates separately for cells in "
    "which the focal attribute was race or gender and cells in which the "
    "focal attribute was a non-race/gender feature (e.g., budget, year of "
    "release, page count, protagonist profession). We then compared the "
    "pooled race/gender estimate to the pooled non-race/gender estimate "
    "using a Wald test of equality."
)

# Section 2: Results body.
SEC2_RESULTS = (
    "Within the zero-initial subset, random assignment to receive descriptive "
    "feedback about race or gender significantly increased the likelihood of "
    "subsequently selecting an option with the focal attribute by 22.50 "
    "percentage points on average (women cells: +27.59 pp; racial-minority "
    "cell: +9.77 pp; p < .001, N = 864 zero-initial participant-cell "
    "observations across Studies 2, 3A, 3B, and 4B). By contrast, within the "
    "zero-initial subset, random assignment to receive descriptive feedback "
    "about non-race/gender attributes did not significantly shift subsequent "
    "selections (pooled estimate: +7.25 pp, p = .19, N = 414 zero-initial "
    "participant-cell observations). A Wald test comparing the pooled "
    "race/gender estimate against the pooled non-race/gender estimate "
    "rejects equality (Δ = +15.25 pp, p = .016, 95% CI [+2.87, +27.63]). "
    "Per-cell estimates appear in Table B1."
)

# Section 2: Table caption (replaces the existing bold "Table B1. ..." caption).
SEC2_TABLE_CAPTION = (
    "Table B1. Ordinary Least Squares (OLS) Regressions Predicting Whether a "
    "Participant’s Subsequent Selection Exhibited the Focal Attribute, "
    "Restricted to Participants Whose Initial Selections Contained No "
    "Options Exhibiting That Attribute"
)

# Section 2: Notes (replaces the existing italic "Notes. ..." paragraph, kept italic prefix).
# The leading "Notes. " is italic; we delete the body after it and reinsert.
SEC2_NOTES_BODY = (
    "Each row reports the estimated effect of random assignment to receive "
    "descriptive feedback about the focal attribute on the likelihood that "
    "the participant’s subsequent selection exhibited that attribute, "
    "estimated from an ordinary least squares (OLS) regression with robust "
    "standard errors (HC3). Estimates are reported in percentage points (pp). "
    "Within each study, rows are ordered with the focal race/gender attribute "
    "appearing last and shown in bold. The sample is restricted within each "
    "cell to participants whose initial selections contained no options "
    "exhibiting the focal attribute. Cells with fewer than approximately 20 "
    "zero-initial participants are omitted. Study 4A is excluded because its "
    "non-race/gender comparison attributes had near-saturated initial "
    "selection rates (see Methods). The pooled estimate for race/gender "
    "attributes (+22.50 pp; women cells: +27.59 pp; racial-minority cell: "
    "+9.77 pp; N = 864) and the pooled estimate for non-race/gender "
    "attributes (+7.25 pp; N = 414) were obtained from a random-effects pool "
    "that weighs each cell’s contribution by its standard error; the "
    "Wald test compares their equality. + p < .10, * p < .05, ** p < .01, "
    "*** p < .001."
)

# Section 2: New table rows.
# Columns: Study | Focal Attribute | N (zero-init) | Effect (pp) | SE (pp) | 95% CI
# Within each study, comparison rows are listed first and the focal (race/gender)
# row is listed LAST and bolded. A final Wald row sits at the bottom.
TABLE_HEADER = ["Study", "Focal Attribute", "N (zero-init)", "Effect (pp)", "SE (pp)", "95% CI"]

# Each row is (is_focal_bold, cells)
TABLE_ROWS = [
    # Study 2 block (comparison rows first, focal last + bold)
    (False, ["Study 2", "Featured an entertainer", "66",  "+21.30+",   "10.76", "[−0.21, +42.80]"]),
    (False, ["Study 2", "Over 500 pages",          "144", "−1.79","9.25",  "[−20.08, +16.50]"]),
    (True,  ["Study 2", "Female protagonist",      "63",  "+32.66**",  "11.03", "[+10.60, +54.72]"]),
    # Study 3A (only one non-race/gender comparison, then focal)
    (False, ["Study 3A", "High budget",                "40",  "+33.33",   "20.28", "[−7.73, +74.39]"]),
    (True,  ["Study 3A", "Racial-minority protagonist","247", "+9.77",    "5.98",  "[−2.00, +21.54]"]),
    # Study 3B
    (False, ["Study 3B", "High budget",                  "61",  "−9.52","16.51", "[−42.55, +23.51]"]),
    (False, ["Study 3B", "Political-leader protagonist", "52",  "+9.77",     "16.50", "[−23.36, +42.91]"]),
    (True,  ["Study 3B", "Woman protagonist",            "336", "+31.57***", "4.91",  "[+21.93, +41.22]"]),
    # Study 4B
    (False, ["Study 4B", "Sold 30M+ copies", "29",  "+27.54",   "25.43", "[−24.64, +79.71]"]),
    (True,  ["Study 4B", "Woman author",     "218", "+20.01**", "6.43",  "[+7.35, +32.68]"]),
    # Wald summary row (not bolded; spans semantically the whole pool)
    (False, ["Pooled race/gender vs. non-race/gender contrast (Wald)",
             "Δ = +15.25 pp", "", "p = .016", "", "[+2.87, +27.63]"]),
]
