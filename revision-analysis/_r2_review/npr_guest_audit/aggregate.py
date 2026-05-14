"""Aggregate coded TED Radio Hour guest data into demographic shares for footnote."""
import json
import os
from collections import Counter, defaultdict

base = os.path.dirname(os.path.abspath(__file__))
chunk_files = [os.path.join(base, f"chunk_{i}_coded.json") for i in (1, 2, 3)]

all_coded = []
for cf in chunk_files:
    if not os.path.exists(cf):
        print(f"MISSING: {cf}")
        continue
    with open(cf, "r", encoding="utf-8") as f:
        chunk = json.load(f)
        all_coded.extend(chunk)
        print(f"Loaded {len(chunk)} from {os.path.basename(cf)}")

n = len(all_coded)
print(f"\nTotal coded guests: {n}\n")

if n == 0:
    print("No data yet — agents still running.")
    raise SystemExit()


def share(rows, field, target_values, exclude_unknown=True):
    """Return (count_target, denom, pct) for rows where field is in target_values."""
    if exclude_unknown:
        denom_rows = [r for r in rows if r.get(field) not in ("unknown", "", None)]
    else:
        denom_rows = rows
    denom = len(denom_rows)
    target = sum(1 for r in denom_rows if r.get(field) in target_values)
    pct = (target / denom * 100) if denom else 0.0
    return target, denom, pct


# === GENDER ===
print("=== GENDER ===")
gender_counts = Counter(r.get("gender", "unknown") for r in all_coded)
for k, v in gender_counts.most_common():
    print(f"  {k}: {v} ({v/n*100:.1f}%)")
t, d, pct = share(all_coded, "gender", ["F"])
print(f"  >>> % WOMEN (of codeable): {t}/{d} = {pct:.1f}%")

# === AGE ===
print("\n=== AGE ===")
age_counts = Counter(r.get("age_bracket", "unknown") for r in all_coded)
for k, v in age_counts.most_common():
    print(f"  {k}: {v} ({v/n*100:.1f}%)")
t, d, pct = share(all_coded, "age_bracket", ["20-49"])
print(f"  >>> % AGES 20-49 (of codeable): {t}/{d} = {pct:.1f}%")

# === AFFILIATION ===
print("\n=== AFFILIATION ===")
aff_counts = Counter(r.get("affiliation_type", "unknown") for r in all_coded)
for k, v in aff_counts.most_common():
    print(f"  {k}: {v} ({v/n*100:.1f}%)")
t, d, pct = share(all_coded, "affiliation_type", ["University"])
print(f"  >>> % UNIVERSITY-AFFILIATED (of codeable): {t}/{d} = {pct:.1f}%")

# === GEOGRAPHY ===
print("\n=== US REGION ===")
reg_counts = Counter(r.get("us_region", "unknown") for r in all_coded)
for k, v in reg_counts.most_common():
    print(f"  {k}: {v} ({v/n*100:.1f}%)")

# Among US-based guests only
us_rows = [r for r in all_coded if r.get("us_region") not in ("International", "unknown", "", None)]
print(f"\n  US-based codeable: {len(us_rows)}")
if us_rows:
    for r in ["West Coast", "East Coast", "Midwest", "South", "Mountain West"]:
        c = sum(1 for x in us_rows if x.get("us_region") == r)
        print(f"    {r}: {c}/{len(us_rows)} = {c/len(us_rows)*100:.1f}%")
    mw_s = sum(1 for x in us_rows if x.get("us_region") in ("Midwest", "South"))
    print(f"  >>> % MIDWEST+SOUTH (of US-based): {mw_s}/{len(us_rows)} = {mw_s/len(us_rows)*100:.1f}%")
    wc_only = sum(1 for x in us_rows if x.get("us_region") == "West Coast")
    print(f"  >>> % WEST COAST (of US-based): {wc_only}/{len(us_rows)} = {wc_only/len(us_rows)*100:.1f}%")

# === SAVE AGGREGATE ===
agg = {
    "n_total": n,
    "show": "TED Radio Hour (NPR)",
    "date_range": f'{min(r["date"] for r in all_coded)} to {max(r["date"] for r in all_coded)}',
    "gender_distribution": dict(gender_counts),
    "age_distribution": dict(age_counts),
    "affiliation_distribution": dict(aff_counts),
    "region_distribution": dict(reg_counts),
    "key_shares": {
        "pct_women_of_codeable": round(share(all_coded, "gender", ["F"])[2], 1),
        "pct_ages_20_49_of_codeable": round(share(all_coded, "age_bracket", ["20-49"])[2], 1),
        "pct_university_affiliated_of_codeable": round(share(all_coded, "affiliation_type", ["University"])[2], 1),
        "pct_midwest_south_of_us_based": round(
            (sum(1 for x in us_rows if x.get("us_region") in ("Midwest", "South")) / len(us_rows) * 100) if us_rows else 0, 1
        ),
        "pct_west_coast_of_us_based": round(
            (sum(1 for x in us_rows if x.get("us_region") == "West Coast") / len(us_rows) * 100) if us_rows else 0, 1
        ),
    },
}

with open(os.path.join(base, "aggregate_results.json"), "w", encoding="utf-8") as f:
    json.dump(agg, f, indent=2)

print(f"\nSaved to aggregate_results.json")
