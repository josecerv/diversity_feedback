"""Extract guest names from TED Radio Hour RSS feed (2024-2026)."""
import xml.etree.ElementTree as ET
import re
import json

tree = ET.parse('ted_radio_hour.xml')
items = tree.getroot().find('channel').findall('item')

month_map = {'Jan':1,'Feb':2,'Mar':3,'Apr':4,'May':5,'Jun':6,
             'Jul':7,'Aug':8,'Sep':9,'Oct':10,'Nov':11,'Dec':12}

ROLE_LIST = (
    r"cardiologist|physician|doctor|dr\.|author|journalist|scientist|researcher|professor|"
    r"designer|architect|activist|therapist|entrepreneur|neuroscientist|psychiatrist|psychologist|"
    r"anthropologist|sociologist|economist|educator|engineer|lawyer|filmmaker|artist|musician|"
    r"comedian|chef|chemist|biologist|physicist|astronomer|astrophysicist|mathematician|"
    r"statistician|geologist|ecologist|climatologist|oncologist|surgeon|geneticist|pediatrician|"
    r"veterinarian|historian|philosopher|theologian|linguist|poet|novelist|playwright|composer|"
    r"choreographer|dancer|actor|athlete|coach|diplomat|astronaut|writer|conductor|"
    r"cosmologist|paleontologist|primatologist|epidemiologist|microbiologist|virologist|"
    r"immunologist|pathologist|radiologist|neurologist|futurist|technologist|inventor|"
    r"consultant|critic|columnist|publisher|editor|advocate|strategist|analyst|forecaster|"
    r"geographer|biochemist|data scientist|computer scientist|ai researcher|game designer|"
    r"industrial designer|graphic designer|product designer|web designer|urban planner|"
    r"food systems expert|infrastructure engineer|cross-cultural psychologist"
)

pat_guests_block = re.compile(
    r"[Gg]uests? (?:in this episode )?include[s]? (.+?)(?:\.\s|This episode originally|Original air|TED Radio Hour\+|For more)",
    re.DOTALL,
)
pat_inline_role = re.compile(
    r"(?<!\w)(" + ROLE_LIST + r")\s+(?:Dr\.\s+)?"
    r"([A-Z][a-zA-Z\-]+(?:\s+[A-Z][a-zA-Z\-\.]+){1,3})"
    r"(?=\s+(?:shares|explains|breaks|explores|tells|joins|describes|argues|discusses|"
    r"reveals|presents|outlines|introduces|talks|guides|takes|walks|reflects|examines|"
    r"warns|imagines|considers|recounts|recalls|recommends|demonstrates|teaches|asks))",
    re.IGNORECASE,
)


def normalize_name(name):
    name = re.sub(r"^(?:Dr\.|Prof\.|Professor|Mr\.|Mrs\.|Ms\.|Sir|Lord|Lady)\s+", "", name, flags=re.IGNORECASE)
    name = re.sub(r"\s+", " ", name).strip().rstrip(".,;:")
    name = name.strip("'\"")
    return name


def is_valid_name(name):
    if not name or len(name) < 5:
        return False
    parts = name.split()
    if len(parts) < 2 or len(parts) > 5:
        return False
    for p in parts:
        if not p[0].isupper():
            return False
    if any(p.isupper() and len(p) > 2 for p in parts):
        return False
    bad = {"CEO", "CFO", "CTO", "COO", "Tech", "Founder", "Director", "President", "Chief", "VP", "Senior", "Lead"}
    if any(p in bad for p in parts):
        return False
    return True


def extract_names_from_blurb(blurb):
    blurb = re.sub(r"\s+", " ", blurb).strip().rstrip(".")
    blurb = re.sub(r",?\s+and\s+", ", ", blurb)
    parts = [p.strip() for p in blurb.split(",") if p.strip()]
    result = []
    for p in parts:
        m = re.search(r"(.+?)\s+((?:Dr\.\s+)?[A-Z][a-zA-Z\-]+(?:\s+[A-Z][a-zA-Z\-\.]+){1,3})\s*$", p)
        if m:
            role = m.group(1).strip()
            name = normalize_name(m.group(2))
            if is_valid_name(name):
                result.append((role, name))
    return result


all_mentions = []
no_match = []
total_in_window = 0

for it in items:
    pub = it.find("pubDate").text or ""
    m = re.search(r"(\d{1,2}) (\w+) (\d{4})", pub)
    if not m:
        continue
    day, mon, year = int(m.group(1)), month_map.get(m.group(2), 0), int(m.group(3))
    if year < 2024:
        continue
    total_in_window += 1

    title = it.find("title").text or ""
    d = it.find("description").text or ""
    d_clean = re.sub(r"<[^>]+>", " ", d)
    d_clean = re.sub(r"\s+", " ", d_clean).strip()

    found_any = False
    mg = pat_guests_block.search(d_clean)
    if mg:
        for role, name in extract_names_from_blurb(mg.group(1)):
            all_mentions.append({"date": f"{year:04d}-{mon:02d}-{day:02d}", "episode": title, "role": role, "name": name})
            found_any = True

    for rm in pat_inline_role.finditer(d_clean[:800]):
        role = rm.group(1).strip()
        name = normalize_name(rm.group(2))
        if is_valid_name(name):
            all_mentions.append({"date": f"{year:04d}-{mon:02d}-{day:02d}", "episode": title, "role": role, "name": name})
            found_any = True

    if not found_any:
        no_match.append({"date": f"{year:04d}-{mon:02d}-{day:02d}", "title": title, "desc_snippet": d_clean[:300]})

seen = {}
mention_count = {}
for g in all_mentions:
    k = g["name"].lower()
    mention_count[k] = mention_count.get(k, 0) + 1
    if k not in seen:
        seen[k] = g

unique_guests = sorted(seen.values(), key=lambda g: g["date"], reverse=True)

print(f"Total mentions: {len(all_mentions)}")
print(f"Unique guests: {len(unique_guests)}")
print(f"Episodes in 2024-2026: {total_in_window}")
print(f"Unmatched episodes: {len(no_match)}")
print()
print("First 30 unique guests (most recent first):")
for g in unique_guests[:30]:
    print(f'  {g["date"]} | {g["role"][:30]:30s} | {g["name"]}')

with open("extracted_guests_v2.json", "w", encoding="utf-8") as f:
    json.dump(
        {
            "all_mentions": all_mentions,
            "unique_guests": unique_guests,
            "mention_counts": mention_count,
            "unmatched_episodes": no_match,
            "methodology": {
                "show": "TED Radio Hour (NPR)",
                "source_feed": "https://feeds.npr.org/510298/podcast.xml",
                "date_range_filter": "2024-01-01 to feed end",
                "total_episodes_in_window": total_in_window,
            },
        },
        f,
        indent=2,
    )
print(f"\nSaved to extracted_guests_v2.json")
