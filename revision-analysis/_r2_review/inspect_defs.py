import json, re, html

PATHS = [
    ("OPTION 1 (SV_3fuYYYXyNyTDCbs)", r"C:/Users/jcerv/Jose/diversity_feedback/revision-analysis/_r2_review/option1_def.json"),
    ("OPTION 2 (SV_bdWaAYnNK6veYVo)", r"C:/Users/jcerv/Jose/diversity_feedback/revision-analysis/_r2_review/option2_def.json"),
]

def strip_html(s):
    s = re.sub(r"<[^>]+>", " ", s or "")
    s = html.unescape(s)
    return re.sub(r"\s+", " ", s).strip()

for label, path in PATHS:
    print("=" * 78)
    print(label)
    print("=" * 78)
    d = json.load(open(path, encoding="utf-8"))
    r = d.get("result", d)
    print("SurveyName:", r.get("SurveyName"))

    # Walk SurveyFlow for EmbeddedData and BlockRandomizer
    flow = r.get("SurveyFlow", {})

    def walk(node, depth=0):
        if not isinstance(node, dict):
            return
        t = node.get("Type", "")
        pad = "  " * depth
        if t == "EmbeddedData":
            for f in node.get("EmbeddedData", []):
                print(f"{pad}ED: {f.get('Field')} = {f.get('Value')!r}")
        if t == "BlockRandomizer":
            print(
                f"{pad}BlockRandomizer SubSet={node.get('SubSet')} "
                f"EvenPresentation={node.get('EvenPresentation')}"
            )
        if t == "Block":
            print(f"{pad}Block ID={node.get('ID')}")
        for k in ("Flow",):
            v = node.get(k)
            if isinstance(v, list):
                for ch in v:
                    walk(ch, depth + 1)

    walk(flow)

    # Show questions
    qs = r.get("Questions", {})
    print("\nQuestions:")
    for qid, q in qs.items():
        qt = strip_html(q.get("QuestionText", ""))[:250]
        de = q.get("DataExportTag", "")
        qtype = q.get("QuestionType", "")
        selector = q.get("Selector", "")
        print(f"  {qid} [{de}] ({qtype}/{selector}): {qt}")
        choices = q.get("Choices", {})
        order = q.get("ChoiceOrder", list(choices.keys()))
        for ck in order:
            cv = choices.get(str(ck), {})
            print(f"     {ck}: {strip_html(cv.get('Display',''))}")
    print()
