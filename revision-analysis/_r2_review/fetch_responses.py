"""Export responses from both pretest surveys and save CSVs."""
import os, sys, time, json, zipfile, io, pathlib, urllib.request, urllib.error

API_KEY = "BJ6aDiEgtQihneCgtZycVLYjaj0ao9gYkdRkb1UZ"
BASE = "https://wharton.yul1.qualtrics.com/API/v3"
HEADERS = {"X-API-TOKEN": API_KEY, "Content-Type": "application/json"}

OUT_DIR = pathlib.Path(r"C:/Users/jcerv/Jose/diversity_feedback/revision-analysis/_r2_review")
OUT_DIR.mkdir(parents=True, exist_ok=True)

SURVEYS = {
    "option1": "SV_3fuYYYXyNyTDCbs",
    "option2": "SV_bdWaAYnNK6veYVo",
}


def req(method, url, data=None):
    body = None if data is None else json.dumps(data).encode("utf-8")
    request = urllib.request.Request(url, data=body, headers=HEADERS, method=method)
    try:
        with urllib.request.urlopen(request, timeout=60) as resp:
            return resp.status, resp.read()
    except urllib.error.HTTPError as e:
        return e.code, e.read()


def export(survey_id, out_csv):
    print(f"\n[{survey_id}] starting export...")
    # Start
    status, body = req(
        "POST",
        f"{BASE}/surveys/{survey_id}/export-responses",
        {"format": "csv", "useLabels": False},
    )
    if status >= 400:
        print(" start error", status, body[:400])
        return None
    progress_id = json.loads(body)["result"]["progressId"]
    # Poll
    for _ in range(60):
        status, body = req(
            "GET",
            f"{BASE}/surveys/{survey_id}/export-responses/{progress_id}",
        )
        prog = json.loads(body)["result"]
        if prog.get("status") == "complete":
            file_id = prog["fileId"]
            break
        if prog.get("status") == "failed":
            print(" export failed:", prog)
            return None
        time.sleep(1.5)
    else:
        print(" export timed out")
        return None
    # Download
    status, body = req(
        "GET",
        f"{BASE}/surveys/{survey_id}/export-responses/{file_id}/file",
    )
    if status >= 400:
        print(" download error", status, body[:400])
        return None
    z = zipfile.ZipFile(io.BytesIO(body))
    name = z.namelist()[0]
    print(f" downloaded {name}, extracting -> {out_csv}")
    with z.open(name) as f, open(out_csv, "wb") as out:
        out.write(f.read())
    return out_csv


for label, sid in SURVEYS.items():
    p = OUT_DIR / f"{label}_{sid}.csv"
    export(sid, p)
    if p.exists():
        size = p.stat().st_size
        # Count actual rows
        with open(p, "rb") as f:
            n = sum(1 for _ in f)
        print(f"  {label}: {size} bytes, {n} lines (incl. header)")
print("\nDone.")
