"""R2 NPR Attribute-Importance Pretest -- full analysis.

Mirrors the R1 protocol in attribute_pretests/between/study1_npr_between_analysis.Rmd:
  * Filter Finished + Status == "IP Address" (Rmd convention)
  * Welch's two-sample t-test (gender target vs each comparison)
  * Cohen's d, pooled SD with (n1+n2-2) denominator
  * R1's one-sided TOST (low bound d=-0.4, high bound = +Inf)  ->
        tests whether the gender target is NOT meaningfully *lower* than comparison
  * NEW: symmetric two-sided TOST with bounds +/-0.4 d  ->
        tests whether gender target and comparison are *statistically equivalent*

The DE's actual concern (George): comparisons being seen as "a lot less important
than gender" -- i.e. comparison_mean < gender_mean by a meaningful amount. We map
this to: reject the hypothesis that comparison is >= 0.4 d less important than
gender, via a one-sided TOST in the *opposite* direction from the Rmd default.
"""

from __future__ import annotations
import math, pathlib
from typing import Dict, List, Tuple
import numpy as np
import pandas as pd
from scipy import stats

ROOT = pathlib.Path(r"C:/Users/jcerv/Jose/diversity_feedback/revision-analysis/_r2_review")

CONFIGS = {
    "option1": {
        "label": "Option 1 - narrow / verb-phrase categories (direct Study 1 replication)",
        "csv": ROOT / "option1_SV_3fuYYYXyNyTDCbs.csv",
        "target": "were women.",
        "comparisons": [
            "were under 50 years old.",
            "were based on the West Coast of the United States.",
            "worked at a university.",
        ],
    },
    "option2": {
        "label": "Option 2 - broadened categories",
        "csv": ROOT / "option2_SV_bdWaAYnNK6veYVo.csv",
        "target": "gender",
        "comparisons": ["age", "geographic location", "employment type"],
    },
}

BOUND_D = 0.4
ALPHA = 0.05


def load_clean(csv_path: pathlib.Path) -> pd.DataFrame:
    df = pd.read_csv(csv_path, dtype=str, keep_default_na=False, na_values=[""])
    df = df.iloc[2:].reset_index(drop=True)
    df = df[df["Finished"].isin(["True", "TRUE", "1", "true"])].copy()
    df = df[df["Status"].isin(["IP Address", "0"])].copy()
    # Restrict to Prolific respondents (drop test/preview rows with empty PID)
    df = df[df["PROLIFIC_PID"].notna() & (df["PROLIFIC_PID"].astype(str).str.strip() != "")].copy()

    # Qualtrics export uses the question's recode values (1,2,3,4,6,7,8) with a
    # gap at 5; remap to the 1-7 display scale.
    RECODE_MAP = {1: 1, 2: 2, 3: 3, 4: 4, 6: 5, 7: 6, 8: 7}

    def clean_imp(x):
        if pd.isna(x):
            return np.nan
        s = str(x).strip()
        low = s.lower()
        if "not at all important" in low:
            return 1.0
        if "very important" in low:
            return 7.0
        try:
            v = int(float(s))
        except ValueError:
            return np.nan
        return float(RECODE_MAP.get(v, np.nan))

    df["importance_num"] = df["importance"].apply(clean_imp)
    df["attribute"] = df["attribute"].fillna("").astype(str).str.strip()
    df = df[df["importance_num"].notna() & (df["attribute"] != "")].copy()
    return df


def welch_stats(x: np.ndarray, y: np.ndarray) -> Dict:
    n1, n2 = len(x), len(y)
    m1, m2 = np.mean(x), np.mean(y)
    v1, v2 = np.var(x, ddof=1), np.var(y, ddof=1)
    se = math.sqrt(v1 / n1 + v2 / n2)
    df = (v1 / n1 + v2 / n2) ** 2 / ((v1 / n1) ** 2 / (n1 - 1) + (v2 / n2) ** 2 / (n2 - 1))
    t = (m1 - m2) / se
    p_two = 2 * (1 - stats.t.cdf(abs(t), df))
    sp = math.sqrt(((n1 - 1) * v1 + (n2 - 1) * v2) / (n1 + n2 - 2))
    d = (m1 - m2) / sp
    tcrit = stats.t.ppf(1 - ALPHA / 2, df)
    return {
        "n1": n1, "n2": n2, "m1": m1, "m2": m2, "v1": v1, "v2": v2,
        "diff": m1 - m2, "se": se, "df": df, "t": t, "p_two": p_two,
        "sp": sp, "d": d,
        "ci_lower": (m1 - m2) - tcrit * se, "ci_upper": (m1 - m2) + tcrit * se,
    }


def tost_one_sided(w, bound_d, direction):
    """One-sided TOST.

    direction = 'gender_not_lower':
        H0: m1 - m2 <= -bound_d * sp        (gender meaningfully lower)
        H1: m1 - m2 > -bound_d * sp         (gender NOT meaningfully lower)
        reject if upper-tail p < alpha
        -> "the gender target is NOT seen as meaningfully less important than comparison"

    direction = 'comparison_not_lower':
        H0: m1 - m2 >= +bound_d * sp        (comparison meaningfully lower)
        H1: m1 - m2 < +bound_d * sp         (comparison NOT meaningfully lower)
        reject if lower-tail p < alpha
        -> "the comparison is NOT seen as meaningfully less important than gender"
           (this is George's actual concern)
    """
    if direction == "gender_not_lower":
        bound_raw = -bound_d * w["sp"]
        t_stat = (w["diff"] - bound_raw) / w["se"]
        p = 1 - stats.t.cdf(t_stat, w["df"])
    elif direction == "comparison_not_lower":
        bound_raw = +bound_d * w["sp"]
        t_stat = (w["diff"] - bound_raw) / w["se"]
        p = stats.t.cdf(t_stat, w["df"])
    else:
        raise ValueError(direction)
    return {"bound_d": bound_d, "bound_raw": bound_raw, "t": t_stat, "p": p, "reject": p < ALPHA}


def tost_two_sided(w, bound_d=BOUND_D):
    lower_raw = -bound_d * w["sp"]
    upper_raw = +bound_d * w["sp"]
    t_lower = (w["diff"] - lower_raw) / w["se"]
    t_upper = (w["diff"] - upper_raw) / w["se"]
    p_lower = 1 - stats.t.cdf(t_lower, w["df"])  # reject lower bound
    p_upper = stats.t.cdf(t_upper, w["df"])      # reject upper bound
    p = max(p_lower, p_upper)
    return {
        "bound_d": bound_d,
        "p_lower": p_lower, "p_upper": p_upper, "p_tost": p,
        "equivalent": p < ALPHA,
    }


def stars(p):
    return ("***" if p < 0.001 else "**" if p < 0.01 else "*" if p < 0.05 else "")


def adjust(p, method):
    p = np.asarray(p)
    n = len(p); order = np.argsort(p); sp = p[order]
    if method == "holm":
        adj = np.minimum.accumulate((n - np.arange(n)) * sp); adj = np.minimum(adj, 1.0)
    elif method == "bonferroni":
        adj = np.minimum(sp * n, 1.0)
    elif method == "fdr":
        adj = sp * n / (np.arange(n) + 1); adj = np.minimum.accumulate(adj[::-1])[::-1]; adj = np.minimum(adj, 1.0)
    else:
        raise ValueError(method)
    out = np.empty_like(adj); out[order] = adj
    return out


def analyze(cfg):
    print("\n" + "#" * 78)
    print(f"# {cfg['label'].upper()}")
    print("#" * 78)
    df = load_clean(cfg["csv"])
    print(f"N (Finished + IP Address + non-NA importance): {len(df)}")
    counts = df["attribute"].value_counts().reindex([cfg["target"], *cfg["comparisons"]]).fillna(0).astype(int)
    print("\nPer-cell N:")
    for k, v in counts.items():
        print(f"  {k!r}: {v}")
    target = cfg["target"]; comps = cfg["comparisons"]

    x = df.loc[df["attribute"] == target, "importance_num"].to_numpy()
    print(f"\n  GENDER TARGET ({target!r}): M = {np.mean(x):.2f}, SD = {np.std(x,ddof=1):.2f}, n = {len(x)}")

    rows = []
    for c in comps:
        y = df.loc[df["attribute"] == c, "importance_num"].to_numpy()
        w = welch_stats(x, y)
        t_strict = tost_two_sided(w)
        t_geor = tost_one_sided(w, BOUND_D, "comparison_not_lower")  # George's concern
        t_rmd  = tost_one_sided(w, BOUND_D, "gender_not_lower")
        rows.append({
            "comparison": c,
            "n_gender": w["n1"], "n_comp": w["n2"],
            "M_gender": w["m1"], "SD_gender": math.sqrt(w["v1"]),
            "M_comp": w["m2"],   "SD_comp":   math.sqrt(w["v2"]),
            "diff (gender - comp)": w["diff"],
            "Welch_t": w["t"], "df": w["df"], "p_welch": w["p_two"],
            "cohens_d": w["d"], "ci_low": w["ci_lower"], "ci_high": w["ci_upper"],
            "TOST_strict_p (eq +/-0.4d)": t_strict["p_tost"],
            "STRICT_EQUIV?": t_strict["equivalent"],
            "George_TOST_p (comp NOT meaningfully lower)": t_geor["p"],
            "George_PASS?": t_geor["reject"],
            "Rmd_TOST_p (gender NOT meaningfully lower)": t_rmd["p"],
            "Rmd_PASS?": t_rmd["reject"],
        })
    R = pd.DataFrame(rows)
    R["p_welch_holm"] = adjust(R["p_welch"], "holm")
    R["p_welch_fdr"]  = adjust(R["p_welch"], "fdr")
    R["sig"] = R["p_welch"].apply(stars)

    # Pretty print blocks
    def fmt_pct(p): return f"{p:.4f}"
    print("\n--- Welch's t-tests (gender target vs each comparison) ---")
    print(R[[
        "comparison","M_gender","SD_gender","M_comp","SD_comp","diff (gender - comp)",
        "Welch_t","df","p_welch","p_welch_holm","p_welch_fdr","cohens_d","sig",
    ]].to_string(index=False, float_format=lambda x: f"{x:.3f}"))

    print("\n--- Two-sided TOST (strict equivalence within +/-0.4d) ---")
    print(R[["comparison","TOST_strict_p (eq +/-0.4d)","STRICT_EQUIV?"]]
          .to_string(index=False, float_format=lambda x: f"{x:.4f}"))

    print("\n--- George's-direction TOST (comparison NOT meaningfully less important than gender) ---")
    print(R[["comparison","George_TOST_p (comp NOT meaningfully lower)","George_PASS?"]]
          .to_string(index=False, float_format=lambda x: f"{x:.4f}"))

    print("\n--- R1 protocol TOST (gender NOT meaningfully less important than comparison) ---")
    print(R[["comparison","Rmd_TOST_p (gender NOT meaningfully lower)","Rmd_PASS?"]]
          .to_string(index=False, float_format=lambda x: f"{x:.4f}"))

    return R


def main():
    summaries = {}
    for k, cfg in CONFIGS.items():
        summaries[k] = analyze(cfg)

    print("\n" + "#" * 78)
    print("# VERDICT SUMMARY (per-comparison)")
    print("#" * 78)
    for k, cfg in CONFIGS.items():
        R = summaries[k]
        print(f"\n[{k}] {cfg['label']}")
        print(f"  {'Comparison':50s} | strict-equiv | George (comp NOT lower) | Welch")
        for _, r in R.iterrows():
            se = "yes" if r["STRICT_EQUIV?"] else "no "
            gp = "PASS" if r["George_PASS?"] else "FAIL"
            sig = r["sig"] or "ns"
            print(f"  {r['comparison']:50s} |    {se:9s} |       {gp:6s}            | "
                  f"d={r['cohens_d']:+.2f}, p={r['p_welch']:.3f} ({sig})")

        print(f"\n  STRICT equivalence (each comparison within +/-0.4d of gender): "
              f"{sum(R['STRICT_EQUIV?'])}/{len(R)} pass")
        print(f"  GEORGE'S CRITERION (no comparison meaningfully less important than gender): "
              f"{sum(R['George_PASS?'])}/{len(R)} pass")

    # Write CSVs for the record
    for k, R in summaries.items():
        out = ROOT / f"r2_pretest_results_{k}.csv"
        R.to_csv(out, index=False)
        print(f"\nWrote {out}")


if __name__ == "__main__":
    main()
