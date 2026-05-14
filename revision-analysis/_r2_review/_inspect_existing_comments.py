import json
d = json.load(open("gdoc_comments.json", encoding="utf-8"))
print("total:", len(d))
for i, c in enumerate(d[:5]):
    print("---", i, c.get("author", {}).get("displayName"))
    print("  anchor =", repr(c.get("anchor")))
    print("  quoted =", repr((c.get("quotedFileContent") or {}).get("value", "")[:80]))
