"""Quick share-with-coauthors PDF: Survey 1 NPR Attribute Test.

Four-row table (one per attribute) with columns:
  Attribute (as worded in the survey) | M | SD | Cohen's d | p

Gender ("were women.") is the reference; its d and p cells are em-dash blanks.
Comparison rows show d and p from a Welch two-sample test against the gender row.
"""

from __future__ import annotations
import math, pathlib, textwrap
import numpy as np
import pandas as pd
from scipy import stats

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.backends.backend_pdf import PdfPages

ROOT = pathlib.Path(r"C:/Users/jcerv/Jose/diversity_feedback/revision-analysis/_r2_review")
CSV  = ROOT / "option1_SV_3fuYYYXyNyTDCbs.csv"
OUT  = ROOT / "Survey1_NPR_Attribute_Test.pdf"

TARGET = "were women."
COMPARISONS = [
    "were under 50 years old.",
    "were based on the West Coast of the United States.",
    "worked at a university.",
]


def load_clean(path):
    df = pd.read_csv(path, dtype=str, keep_default_na=False, na_values=[""])
    df = df.iloc[2:].reset_index(drop=True)
    df = df[df["Finished"].isin(["True", "TRUE", "1", "true"])]
    df = df[df["Status"].isin(["IP Address", "0"])]
    df = df[df["PROLIFIC_PID"].notna() & (df["PROLIFIC_PID"].astype(str).str.strip() != "")]

    # Qualtrics export gives recode values (1,2,3,4,6,7,8); remap to 1-7 display.
    RECODE_MAP = {1: 1, 2: 2, 3: 3, 4: 4, 6: 5, 7: 6, 8: 7}

    def clean(x):
        if pd.isna(x):
            return np.nan
        s = str(x).strip().lower()
        if "not at all important" in s:
            return 1.0
        if "very important" in s:
            return 7.0
        try:
            v = int(float(s))
        except ValueError:
            return np.nan
        return float(RECODE_MAP.get(v, np.nan))

    df = df.copy()
    df["importance_num"] = df["importance"].apply(clean)
    df["attribute"] = df["attribute"].fillna("").astype(str).str.strip()
    df = df[df["importance_num"].notna() & (df["attribute"] != "")]
    return df


def welch(x, y):
    n1, n2 = len(x), len(y)
    m1, m2 = np.mean(x), np.mean(y)
    v1, v2 = np.var(x, ddof=1), np.var(y, ddof=1)
    se = math.sqrt(v1 / n1 + v2 / n2)
    dfree = (v1 / n1 + v2 / n2) ** 2 / ((v1 / n1) ** 2 / (n1 - 1) + (v2 / n2) ** 2 / (n2 - 1))
    t = (m1 - m2) / se
    p = 2 * (1 - stats.t.cdf(abs(t), dfree))
    sp = math.sqrt(((n1 - 1) * v1 + (n2 - 1) * v2) / (n1 + n2 - 2))
    d = (m1 - m2) / sp
    return t, dfree, p, d


def stars(p):
    if p < 0.001: return "***"
    if p < 0.01:  return "**"
    if p < 0.05:  return "*"
    if p < 0.10:  return "†"  # dagger for marginal
    return ""


def fmt_p(p):
    if p < 0.001: return "< .001"
    s = f"{p:.3f}"
    return s.lstrip("0") if s.startswith("0.") else s  # APA: ".003"


def main():
    df = load_clean(CSV)
    gender = df.loc[df["attribute"] == TARGET, "importance_num"].to_numpy()
    rows = []
    rows.append({
        "Attribute (as asked)": "The percentage of experts who were women.",
        "N": len(gender),
        "M": np.mean(gender),
        "SD": np.std(gender, ddof=1),
        "Cohen's d": "—",
        "p (vs. gender)": "— (ref.)",
    })
    for c in COMPARISONS:
        y = df.loc[df["attribute"] == c, "importance_num"].to_numpy()
        _, _, p, d = welch(gender, y)
        rows.append({
            "Attribute (as asked)": f"The percentage of experts who {c}",
            "N": len(y),
            "M": np.mean(y),
            "SD": np.std(y, ddof=1),
            "Cohen's d": f"{d:+.2f}{stars(p)}",
            "p (vs. gender)": fmt_p(p),
        })

    headers = ["Attribute (as asked)", "N", "M", "SD", "Cohen's $d$", "$p$ (vs. gender)"]
    body = []
    for r in rows:
        # Wrap the attribute text so the longest row fits in a portrait column
        wrapped = textwrap.fill(r["Attribute (as asked)"], width=30,
                                break_long_words=False)
        body.append([
            wrapped,
            f"{r['N']}",
            f"{r['M']:.2f}",
            f"{r['SD']:.2f}",
            r["Cohen's d"],
            r["p (vs. gender)"],
        ])

    # Portrait page
    fig_w, fig_h = 8.5, 8.0
    fig, ax = plt.subplots(figsize=(fig_w, fig_h))
    ax.set_axis_off()

    # Title / subtitle block
    title_l1 = "Attribute Importance Ratings"
    title_l2 = "Survey 1 NPR Attribute Test Between Subjects"
    subtitle = (
        "Participants rated the importance of considering one randomly\n"
        "assigned expert-attribute statement on a 1–7 scale\n"
        "(1 = Not at all important, 7 = Very important)."
    )
    fig.text(0.06, 0.965, title_l1, ha="left", va="top",
             fontsize=14, fontweight="bold")
    fig.text(0.06, 0.928, title_l2, ha="left", va="top",
             fontsize=14, fontweight="bold")
    fig.text(0.06, 0.88, subtitle, ha="left", va="top",
             fontsize=10, style="italic", color="#333333")

    # Wider d / p columns so headers don't crowd; col 0 narrower to match wrap
    col_widths = [0.40, 0.07, 0.08, 0.08, 0.15, 0.22]
    table = ax.table(
        cellText=body,
        colLabels=headers,
        colWidths=col_widths,
        cellLoc="center",
        colLoc="center",
        loc="center",
        bbox=[0.04, 0.18, 0.92, 0.55],
    )
    table.auto_set_font_size(False)
    table.set_fontsize(10)
    table.scale(1, 1.2)

    # Left-align the attribute column
    for r in range(len(body) + 1):  # +1 for header row
        cell = table[(r, 0)]
        cell.PAD = 0.015
        cell._loc = "left"
        cell.get_text().set_horizontalalignment("left")
        if r > 0:
            cell.get_text().set_fontsize(9.5)

    # Style header row
    for c in range(len(headers)):
        hcell = table[(0, c)]
        hcell.set_facecolor("#F4F1E6")
        hcell.set_edgecolor("#1B2E4B")
        hcell.set_linewidth(1.0)
        hcell.get_text().set_fontweight("bold")

    # Body row borders (subtle)
    for r in range(1, len(body) + 1):
        for c in range(len(headers)):
            cell = table[(r, c)]
            cell.set_edgecolor("#999999")
            cell.set_linewidth(0.4)
            if r % 2 == 0:
                cell.set_facecolor("#FAFAF7")

    # Highlight the gender (reference) row
    for c in range(len(headers)):
        table[(1, c)].set_facecolor("#EFE7C8")

    with PdfPages(OUT) as pdf:
        pdf.savefig(fig, bbox_inches="tight")
    print(f"Wrote {OUT}")


if __name__ == "__main__":
    main()
