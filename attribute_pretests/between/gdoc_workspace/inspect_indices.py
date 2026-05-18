"""Print start/end index of every paragraph + table cell to plan batchUpdate."""
import json, pathlib, sys, io
sys.stdout = io.TextIOWrapper(sys.stdout.buffer, encoding="utf-8", errors="replace")
HERE = pathlib.Path(__file__).parent
doc = json.load(open(HERE / "doc_raw.json", encoding="utf-8"))

def emit(elements, depth=0):
    pad = "  " * depth
    for el in elements:
        if "paragraph" in el:
            text = ""
            for r in el["paragraph"]["elements"]:
                if "textRun" in r:
                    text += r["textRun"].get("content", "")
            ps = el["paragraph"].get("paragraphStyle", {}).get("namedStyleType", "")
            print(f"{pad}P [{el['startIndex']}->{el['endIndex']}] ({ps}) {repr(text[:120])}")
        elif "table" in el:
            print(f"{pad}TABLE [{el['startIndex']}->{el['endIndex']}] rows={len(el['table']['tableRows'])}")
            for ri, row in enumerate(el["table"]["tableRows"]):
                print(f"{pad}  Row {ri} [{row['startIndex']}->{row['endIndex']}]")
                for ci, cell in enumerate(row["tableCells"]):
                    print(f"{pad}    Cell {ci} [{cell['startIndex']}->{cell['endIndex']}]")
                    emit(cell.get("content", []), depth+3)
        elif "sectionBreak" in el:
            print(f"{pad}SECTION [{el.get('startIndex','?')}->{el.get('endIndex','?')}]")

emit(doc["body"]["content"])
