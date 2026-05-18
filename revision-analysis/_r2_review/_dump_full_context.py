"""Dump the Study 4 + Study 5 sections + relevant response letter paragraphs
into a single context file for the Codex brief."""
from pathlib import Path

manuscript = Path("revision-analysis/_r2_review/manuscript_gdoc_text.txt").read_text(encoding="utf-8", errors="replace")
mlines = manuscript.split("\n")

out = []
out.append("=" * 80)
out.append("FULL CONTEXT: Study 4 section, Study 5 section, Response letter paragraphs")
out.append("=" * 80)
out.append("")
out.append("MANUSCRIPT — STUDY 4 SECTION (lines 96-133):")
out.append("-" * 80)
for i in range(95, 133):
    out.append(f"L{i+1}: {mlines[i]}")
out.append("")
out.append("MANUSCRIPT — STUDY 5 SECTION (lines 134-160):")
out.append("-" * 80)
for i in range(133, min(160, len(mlines))):
    out.append(f"L{i+1}: {mlines[i]}")
out.append("")

# Response letter context — pull the relevant paragraphs
try:
    rl_text = Path("revision-analysis/_r2_review/response_letter_gdoc_text.txt").read_text(encoding="utf-8", errors="replace")
    out.append("RESPONSE LETTER (current Gdoc state):")
    out.append("-" * 80)
    out.append(rl_text)
except Exception:
    out.append("RESPONSE LETTER (R1 version):")
    out.append("-" * 80)
    rl_r1 = Path("revision-analysis/_r1_review/response_letter.txt").read_text(encoding="utf-8", errors="replace")
    rl_lines = rl_r1.split("\n")
    for i in range(13, min(60, len(rl_lines))):
        out.append(f"L{i+1}: {rl_lines[i]}")

Path("revision-analysis/_r2_review/codex_full_context.txt").write_text("\n".join(out), encoding="utf-8")
print(f"Wrote {len(out)} lines")
