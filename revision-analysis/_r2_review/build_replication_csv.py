"""Build a clean replication CSV for Survey 1 NPR Attribute Test (Option 1).

Output columns: ResponseId, PROLIFIC_PID, attribute, importance, gender, race, duration_sec
Filters applied (identical to the PDF analysis):
  - Finished == TRUE
  - Status == "IP Address"
  - PROLIFIC_PID non-empty
  - importance and attribute non-empty
"""

import pathlib
import numpy as np
import pandas as pd

ROOT = pathlib.Path(r"C:/Users/jcerv/Jose/diversity_feedback/revision-analysis/_r2_review")
SRC  = ROOT / "option1_SV_3fuYYYXyNyTDCbs.csv"
OUT  = ROOT / "Survey1_NPR_Attribute_Test_replication.csv"


# Qualtrics export gives the question's recode values (1,2,3,4,6,7,8) with a gap
# at 5; remap to the 1-7 display rating.
RECODE_MAP = {1: 1, 2: 2, 3: 3, 4: 4, 6: 5, 7: 6, 8: 7}


def clean_imp(x):
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


def main():
    df = pd.read_csv(SRC, dtype=str, keep_default_na=False, na_values=[""])
    df = df.iloc[2:].reset_index(drop=True)  # drop the Qualtrics metadata rows

    df = df[df["Finished"].isin(["True", "TRUE", "1", "true"])]
    df = df[df["Status"].isin(["IP Address", "0"])]
    df = df[df["PROLIFIC_PID"].notna() & (df["PROLIFIC_PID"].astype(str).str.strip() != "")]

    df = df.copy()
    df["importance"] = df["importance"].apply(clean_imp)
    df["attribute"] = df["attribute"].fillna("").astype(str).str.strip()
    df = df[df["importance"].notna() & (df["attribute"] != "")]

    out = pd.DataFrame({
        "ResponseId":   df["ResponseId"],
        "PROLIFIC_PID": df["PROLIFIC_PID"],
        "attribute":    df["attribute"],
        "importance":   df["importance"].astype(int),
        "gender":       df["gender"],
        "race":         df["race"],
        "duration_sec": pd.to_numeric(df["Duration (in seconds)"], errors="coerce").astype("Int64"),
    })

    out.to_csv(OUT, index=False)
    print(f"Wrote {OUT}")
    print(f"Rows: {len(out)}")
    print("Per-attribute N:")
    print(out["attribute"].value_counts().to_string())


if __name__ == "__main__":
    main()
