"""Download all 25 author photos from the Study-4A Qualtrics survey
(QID40) so slide 9 can show the exact stimuli participants saw.

Images are saved to ./authors_photos/{slug}.png keyed by author name.
"""
from __future__ import annotations
import re
import urllib.request
from pathlib import Path

OUT = Path(__file__).parent / "authors_photos"
OUT.mkdir(exist_ok=True)

BASE = "https://wharton.yul1.qualtrics.com/ControlPanel/Graphic.php?IM="

AUTHOR_IMAGES = [
    ("Agatha Christie",         "IM_0UKSZltExuRIzxY"),
    ("Alice Walker",            "IM_cZ58wnXHJdlyDpI"),
    ("Charles Dickens",         "IM_emS46KUDC5DYJn0"),
    ("Emily Bronte",            "IM_3yNCKU4d7mwqZeK"),
    ("Ernest Hemingway",        "IM_dhQyEbhPn3tungW"),
    ("Gabriel Garcia Marquez",  "IM_4GjYCNpzVdBoPnE"),
    ("George Orwell",           "IM_6KmX3aObtKHdHpQ"),
    ("Herman Melville",         "IM_5dLJCT0lpct4TuC"),
    ("Isabel Allende",          "IM_bIqc9N8QR8EeU8m"),
    ("Jack London",             "IM_0cVvZ74xVzmaxSK"),
    ("James Baldwin",           "IM_3aR2B1IDKc8fGJw"),
    ("Jane Austen",             "IM_3dZZL6kupWfyybA"),
    ("JD Salinger",             "IM_9X0Wzhohxib5zro"),
    ("John Steinbeck",          "IM_a4scMZFBBQNr6Yu"),
    ("Joyce Carol Oates",       "IM_eepMxOsBNQdCyMK"),
    ("Jorge Luis Borges",       "IM_4UU3kxVnPqfL86G"),
    ("JRR Tolkien",             "IM_3atrCIPrDBfGejA"),
    ("Louisa May Alcott",       "IM_4PE54TxPCgfc6eG"),
    ("Lucy Maud",               "IM_7WZwqiXUSpWTiQe"),
    ("Michael Crichton",        "IM_cwg0vTl9AoDOdQa"),
    ("Nathaniel Hawthorne",     "IM_20t4qo2lg2zus18"),
    ("Sandra Cisneros",         "IM_6PVeDEPicE28W1M"),
    ("Sylvia Plath",            "IM_dfXbtjPeHxb1XkG"),
    ("Toni Morrison",           "IM_6xtAbCBm3r3tDwO"),
    ("WEB Du Bois",             "IM_bw5AuX7fqWtl09w"),
]


def _slug(name):
    return re.sub(r"[^a-z0-9]+", "_", name.lower()).strip("_")


def main():
    for name, im_id in AUTHOR_IMAGES:
        out = OUT / f"{_slug(name)}.png"
        if out.exists():
            print(f"  [have] {name}")
            continue
        url = BASE + im_id
        print(f"  fetching {name}...")
        req = urllib.request.Request(url,
            headers={"User-Agent": "Mozilla/5.0"})
        with urllib.request.urlopen(req) as resp:
            out.write_bytes(resp.read())
    print(f"\nSaved {len(AUTHOR_IMAGES)} images to {OUT}")


if __name__ == "__main__":
    main()
