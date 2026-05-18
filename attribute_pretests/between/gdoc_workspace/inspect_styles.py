"""Inspect inline textStyle (bold/italic) in key paragraphs."""
import json, pathlib, sys, io
sys.stdout = io.TextIOWrapper(sys.stdout.buffer, encoding="utf-8", errors="replace")
HERE = pathlib.Path(__file__).parent
doc = json.load(open(HERE / "doc_raw.json", encoding="utf-8"))

def walk(elements):
    for el in elements:
        if "paragraph" in el:
            text_runs = []
            for r in el["paragraph"]["elements"]:
                if "textRun" in r:
                    tr = r["textRun"]
                    text_runs.append((r.get("startIndex"), r.get("endIndex"), tr.get("textStyle", {}), tr.get("content", "")))
            if not text_runs:
                continue
            for s, e, st, c in text_runs:
                flags = []
                if st.get("bold"): flags.append("B")
                if st.get("italic"): flags.append("I")
                if st.get("underline"): flags.append("U")
                fonts = st.get("weightedFontFamily", {}).get("fontFamily", "")
                flag_str = "".join(flags) if flags else "-"
                content_preview = repr(c[:60])
                # Only print if there's bold/italic or it's the first run of a paragraph
                if flags or "Procedure" in c or "Participants." in c or "Notes." in c or "Methods" in c:
                    print(f"[{s}->{e}] flags={flag_str} font={fonts} {content_preview}")
        elif "table" in el:
            for row in el["table"]["tableRows"]:
                for cell in row["tableCells"]:
                    walk(cell.get("content", []))

walk(doc["body"]["content"])
