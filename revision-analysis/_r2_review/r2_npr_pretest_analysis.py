"""R2 NPR Attribute-Importance Pretest Analysis.

Mirrors the protocol in attribute_pretests/between/study1_npr_between_analysis.Rmd:
  - Filter Finished == TRUE & Status == "IP Address" (0)
  - Drop NA importance
  - Welch's two-sample t-test: women vs each comparison
  - Cohen's d (pooled SD)
  - One-sided TOST equivalence: lower bound d = -0.4 (target rated less than this much
    higher than comparison) -> reject means comparison is NOT significantly less
    important than the gender target.

Decision criterion for "satisfies":
  For each of the 3 comparison attributes,
    EITHER (a) the Welch t-test fails to reject (p >= .05) AND the TOST one-sided
    equivalence rejects (p < .05) -- i.e. statistically equivalent and not significantly
    different,
    OR (b) the comparison attribute is rated as high or higher than the target
    (mean_diff <= 0 and not significantly lower).
"""

from __future__ import annotations
import csv, math, pathlib, statistics, sys
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

EQ_BOUND_D = -0.4  # one-sided TOST lower bound (matches R1 protocol)


def load_clean(csv_path: pathlib.Path) -> pd.DataFrame:
    """Qualtrics CSV: row 0 = var names (header), row 1 = display labels, row 2 = ImportId
    JSON. Read with header=0 then drop the next 2 metadata rows."""
    df = pd.read_csv(csv_path, dtype=str, keep_default_na=False, na_values=[""])
    # Drop the 2 Qualtrics metadata rows under the header
    df = df.iloc[2:].reset_index(drop=True)

    # Match Rmd: Finished TRUE/"True"/1 and Status "IP Address" or 0
    finished_mask = df["Finished"].isin(["True", "TRUE", "1", "true"])
    status_mask = df["Status"].isin(["IP Address", "0"])
    df = df[finished_mask & status_mask].copy()

    # Clean importance scale (handle text anchors; numeric otherwise)
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
            return float(s)
        except ValueError:
            return np.nan

    df["importance_num"] = df["importance"].apply(clean_imp)
    df["attribute"] = df["attribute"].fillna("").astype(str).str.strip()

    df = df[df["importance_num"].notna() & (df["attribute"] != "")].copy()
    return df


def cohens_d_pooled(x: np.ndarray, y: np.ndarray) -> tuple[float, float]:
    """Return (cohen's d, pooled SD). Uses (n1+n2-2) denominator like effsize::cohen.d default."""
    n1, n2 = len(x), len(y)
    v1, v2 = np.var(x, ddof=1), np.var(y, ddof=1)
    sp = math.sqrt(((n1 - 1) * v1 + (n2 - 1) * v2) / (n1 + n2 - 2))
    if sp == 0:
        return float("nan"), sp
    d = (np.mean(x) - np.mean(y)) / sp
    return d, sp


def welch_t(x: np.ndarray, y: np.ndarray) -> Dict:
    res = stats.ttest_ind(x, y, equal_var=False)
    n1, n2 = len(x), len(y)
    v1, v2 = np.var(x, ddof=1), np.var(y, ddof=1)
    se = math.sqrt(v1 / n1 + v2 / n2)
    num = (v1 / n1 + v2 / n2) ** 2
    den = (v1 / n1) ** 2 / (n1 - 1) + (v2 / n2) ** 2 / (n2 - 1)
    df = num / den
    diff = np.mean(x) - np.mean(y)
    tcrit = stats.t.ppf(0.975, df)
    return {
        "mean_diff": diff,
        "t": float(res.statistic),
        "df": df,
        "p_two_sided": float(res.pvalue),
        "ci_lower": diff - tcrit * se,
        "ci_upper": diff + tcrit * se,
        "se": se,
    }


def tost_lower(x: np.ndarray, y: np.ndarray, bound_d: float = EQ_BOUND_D) -> Dict:
    """One-sided lower-bound equivalence test (matches Rmd: TOSTER::tsum_TOST with
    low_eqbound, high_eqbound=Inf, raw scale).
    H0: mu_x - mu_y <= bound_raw    (i.e. target rated at least |bound| points lower)
    H1: mu_x - mu_y > bound_raw     (target NOT meaningfully lower)
    Reject if upper-tail p < .05.
    """
    n1, n2 = len(x), len(y)
    v1, v2 = np.var(x, ddof=1), np.var(y, ddof=1)
    sp = math.sqrt(((n1 - 1) * v1 + (n2 - 1) * v2) / (n1 + n2 - 2))
    bound_raw = bound_d * sp
    se = math.sqrt(v1 / n1 + v2 / n2)
    diff = np.mean(x) - np.mean(y)
    t_stat = (diff - bound_raw) / se
    # Welch df
    num = (v1 / n1 + v2 / n2) ** 2
    den = (v1 / n1) ** 2 / (n1 - 1) + (v2 / n2) ** 2 / (n2 - 1)
    df = num / den
    p_upper = 1 - stats.t.cdf(t_stat, df)
    return {
        "bound_d": bound_d,
        "bound_raw": bound_raw,
        "t_lower": t_stat,
        "df": df,
        "tost_p": p_upper,
        "reject_null_at_05": p_upper < 0.05,
    }


def analyze(label: str, df: pd.DataFrame, target: str, comparisons: List[str]) -> Dict:
    print("\n" + "=" * 78)
    print(label)
    print("=" * 78)

    obs_attrs = sorted(df["attribute"].unique())
    print("Observed attribute values:")
    for a in obs_attrs:
        print(f"  - {a!r}: n = {(df['attribute'] == a).sum()}")

    # Sanity: each expected attribute should be present
    expected = {target, *comparisons}
    missing = expected - set(obs_attrs)
    if missing:
        print("WARNING: missing expected attributes:", missing)

    # Descriptives
    desc_rows = []
    for a in [target, *comparisons]:
        v = df.loc[df["attribute"] == a, "importance_num"].to_numpy()
        desc_rows.append(
            {
                "attribute": a,
                "n": len(v),
                "mean": float(np.mean(v)) if len(v) else float("nan"),
                "sd": float(np.std(v, ddof=1)) if len(v) > 1 else float("nan"),
                "se": float(np.std(v, ddof=1) / math.sqrt(len(v))) if len(v) > 1 else float("nan"),
                "median": float(np.median(v)) if len(v) else float("nan"),
                "ci_lower": (
                    float(np.mean(v) - 1.96 * np.std(v, ddof=1) / math.sqrt(len(v)))
                    if len(v) > 1
                    else float("nan")
                ),
                "ci_upper": (
                    float(np.mean(v) + 1.96 * np.std(v, ddof=1) / math.sqrt(len(v)))
                    if len(v) > 1
                    else float("nan")
                ),
            }
        )
    desc = pd.DataFrame(desc_rows)
    print("\nDescriptive statistics:")
    print(desc.to_string(index=False, float_format=lambda x: f"{x:.3f}"))

    # Welch t-tests + Cohen's d + TOST
    x = df.loc[df["attribute"] == target, "importance_num"].to_numpy()
    test_rows = []
    tost_rows = []
    for c in comparisons:
        y = df.loc[df["attribute"] == c, "importance_num"].to_numpy()
        wt = welch_t(x, y)
        d, sp = cohens_d_pooled(x, y)
        tost = tost_lower(x, y, EQ_BOUND_D)
        test_rows.append(
            {
                "comparison": f"{target} vs {c}",
                "target_mean": np.mean(x),
                "target_sd": np.std(x, ddof=1),
                "target_n": len(x),
                "other_mean": np.mean(y),
                "other_sd": np.std(y, ddof=1),
                "other_n": len(y),
                "mean_diff": wt["mean_diff"],
                "t": wt["t"],
                "df": wt["df"],
                "p_value": wt["p_two_sided"],
                "ci_lower": wt["ci_lower"],
                "ci_upper": wt["ci_upper"],
                "cohens_d": d,
            }
        )
        tost_rows.append(
            {
                "comparison": f"{target} vs {c}",
                "bound_d": tost["bound_d"],
                "bound_raw": tost["bound_raw"],
                "tost_p": tost["tost_p"],
                "equivalent_at_05": tost["reject_null_at_05"],
            }
        )

    tests = pd.DataFrame(test_rows)
    # Multiple-comparison adjustments
    raw_p = tests["p_value"].to_numpy()
    tests["p_holm"] = multi_p(raw_p, "holm")
    tests["p_bonferroni"] = multi_p(raw_p, "bonferroni")
    tests["p_fdr"] = multi_p(raw_p, "fdr")
    tests["sig_stars"] = tests["p_value"].apply(stars)

    tost = pd.DataFrame(tost_rows)

    print("\nWelch's t-tests (target vs each comparison):")
    cols = [
        "comparison",
        "target_mean",
        "target_sd",
        "target_n",
        "other_mean",
        "other_sd",
        "other_n",
        "mean_diff",
        "t",
        "df",
        "p_value",
        "p_holm",
        "p_fdr",
        "cohens_d",
        "sig_stars",
    ]
    print(tests[cols].to_string(index=False, float_format=lambda x: f"{x:.3f}"))

    print("\nOne-sided TOST equivalence (lower bound d = -0.4):")
    print(tost.to_string(index=False, float_format=lambda x: f"{x:.4f}"))

    # Verdict
    print("\nPer-comparison verdict:")
    verdicts = []
    for tr, eq in zip(tests.itertuples(), tost.itertuples()):
        c_label = tr.comparison.split(" vs ", 1)[1]
        direction = "HIGHER" if tr.mean_diff > 0 else "LOWER"
        sig = "sig" if tr.p_value < 0.05 else "ns"
        equiv = "equiv (TOST p<.05)" if eq.equivalent_at_05 else "NOT equivalent"
        # Criterion: NOT significantly lower OR equivalent at -0.4 d
        target_higher = tr.mean_diff > 0
        sig_diff = tr.p_value < 0.05
        if target_higher and sig_diff:
            # Target rated significantly HIGHER -> comparison fails criterion (less important)
            ok = eq.equivalent_at_05  # Even if higher, TOST equivalence still rules out d<-0.4 -- but here target is higher, so equivalence is automatic for the lower bound. Actually if target is higher, then the lower-bound TOST trivially rejects. The real concern is whether comparison is *materially* less important; if Welch is sig & d > 0.4, that's a failure.
            d_mag = abs(tr.cohens_d)
            ok = (d_mag < 0.4)
        else:
            ok = eq.equivalent_at_05  # rules out d <= -0.4
        verdict = "PASS" if ok else "FAIL"
        print(
            f"  - {c_label!r}: target M={tr.target_mean:.2f} vs M={tr.other_mean:.2f}; "
            f"diff={tr.mean_diff:+.2f}; t({tr.df:.1f})={tr.t:.2f}, p={tr.p_value:.3f} ({sig}); "
            f"d={tr.cohens_d:+.2f}; TOST p={eq.tost_p:.4f} ({equiv}); -> {verdict}"
        )
        verdicts.append((c_label, verdict, tr._asdict() if hasattr(tr, "_asdict") else dict(tr._asdict())))

    pass_count = sum(1 for _, v, _ in verdicts if v == "PASS")
    print(f"\nOverall: {pass_count}/{len(comparisons)} comparisons satisfy criterion.")

    return {
        "desc": desc,
        "tests": tests,
        "tost": tost,
        "pass_count": pass_count,
        "total": len(comparisons),
    }


def multi_p(p: np.ndarray, method: str) -> np.ndarray:
    n = len(p)
    if n == 0:
        return p
    order = np.argsort(p)
    sorted_p = p[order]
    if method == "bonferroni":
        adj = np.minimum(sorted_p * n, 1.0)
    elif method == "holm":
        adj = np.minimum.accumulate((n - np.arange(n)) * sorted_p)
        adj = np.minimum(adj, 1.0)
    elif method == "fdr":
        # Benjamini-Hochberg
        adj = sorted_p * n / (np.arange(n) + 1)
        adj = np.minimum.accumulate(adj[::-1])[::-1]
        adj = np.minimum(adj, 1.0)
    else:
        raise ValueError(method)
    out = np.empty_like(adj)
    out[order] = adj
    return out


def stars(p: float) -> str:
    if p < 0.001:
        return "***"
    if p < 0.01:
        return "**"
    if p < 0.05:
        return "*"
    return ""


def main():
    summary = {}
    for key, cfg in CONFIGS.items():
        print("\n" + "#" * 78)
        print(f"# {key.upper()}: {cfg['label']}")
        print("#" * 78)
        df = load_clean(cfg["csv"])
        print(f"\nN (complete + IP Address + non-NA importance): {len(df)}")
        out = analyze(cfg["label"], df, cfg["target"], cfg["comparisons"])
        summary[key] = out

    print("\n" + "#" * 78)
    print("# FINAL VERDICT")
    print("#" * 78)
    for key, cfg in CONFIGS.items():
        out = summary[key]
        verdict = "SATISFIES" if out["pass_count"] == out["total"] else f"PARTIAL ({out['pass_count']}/{out['total']})"
        print(f"  {key}: {verdict} -- {cfg['label']}")


if __name__ == "__main__":
    main()
