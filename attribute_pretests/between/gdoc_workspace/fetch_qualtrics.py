"""Pull the canonical NPR attribute-importance survey from Qualtrics."""
import os, json, pathlib, sys
import urllib.request

SURVEY_ID = "SV_aeIi4xABcJiz1vU"
HERE = pathlib.Path(__file__).parent
ENV  = pathlib.Path(r"C:/Users/jcerv/Jose/diversity_feedback/.env")

env = {}
for ln in ENV.read_text().splitlines():
    if "=" in ln and not ln.strip().startswith("#"):
        k, v = ln.split("=", 1)
        env[k.strip()] = v.strip()

API_KEY = env["QUALTRICS_API_KEY"]
BASE    = env.get("QUALTRICS_BASE_URL", "https://yul1.qualtrics.com").rstrip("/")

url = f"{BASE}/API/v3/survey-definitions/{SURVEY_ID}"
req = urllib.request.Request(url, headers={"X-API-TOKEN": API_KEY})
with urllib.request.urlopen(req) as resp:
    data = json.loads(resp.read())

(HERE / "qualtrics_survey.json").write_text(json.dumps(data, indent=2), encoding="utf-8")

# Summarize: print intro/instruction blocks and question texts so I can extract the prompt.
result = data.get("result", data)
elements = result.get("Questions", {})
flow = result.get("Flow", [])
print(f"Survey: {result.get('SurveyName')}")
print(f"#Questions: {len(elements)}")
print("---")
for qid, q in elements.items():
    qtext = q.get("QuestionText", "")
    qtype = q.get("QuestionType", "")
    sel = q.get("Selector", "")
    print(f"\n== {qid} ({qtype}/{sel}) ==")
    print(qtext)
    if "Choices" in q:
        for k, v in q["Choices"].items():
            print(f"  [{k}] {v.get('Display','')}")
