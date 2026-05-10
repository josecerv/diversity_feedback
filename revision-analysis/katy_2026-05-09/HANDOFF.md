# HANDOFF — Page-by-page PDF audit of the manuscript Gdoc

**Status as of 2026-05-09 ~21:30 ET:** The bulk apply ran. The
manuscript Gdoc now reflects the operations spec we built from the
PDF transcripts + screenshots. **But the spec missed at least one
green-text addition** (Katy's footnote 2 on page 11 with stimulus-set
interaction stats — caught by Jose, fixed live). There are almost
certainly more such omissions because the prior LLM's transcripts
(`claude_edits_all.md`, `codex_edits_all.md`, `reconciled_edits.md`)
were derivative and conflated some adjacent green/pink edits.

**Your mission:** audit the live Gdoc *line by line* against
`katy_pdf.pdf`. The PDF is Katy's offline-edited version that never
synced — it shows every Sophia/Jose edit (pink) AND Katy's edits
(green) AND Katy's own strikethroughs of her green text (when she
changed her mind mid-edit). The "ground-truth" final document is
**the PDF as it would render if every accepted change applied** — i.e.
every pink and green addition committed, every pink and green deletion
removed, every Katy comment treated as authoritative, and Katy's
strikethrough-of-her-own-green honored as her final intent.

Find every discrepancy. Patch them. Be eagle-eyed.

> **READ THIS WHOLE FILE BEFORE TOUCHING ANYTHING.** Then ask Jose any
> clarifying questions before writing a single API call.

---

## 1. The mission, in one paragraph

The Gdoc is Jose's manuscript. Three people edit it: Sophia (pink),
Jose (purple — but historically labeled "pink" in the transcripts
because Sophia's and Jose's chips look indistinguishable in the PDF
print), and Katy (green). Katy reviews on top of Sophia/Jose and her
green text is the ultimate authority. On 2026-05-09 Katy edited
offline; her edits never synced; she sent the PDF as a print-of-her-
local-state. That PDF is the truth. The live Gdoc has had a programmatic
"apply" that approximated the truth, but that apply was driven by
imperfect transcripts and missed at least one footnote. Your job is
to compare the live Gdoc, page by page, against the PDF, and fix every
delta you find.

The final state should be: **clean text matching the PDF intent**,
**all 13 Drive comments preserved**, **3 footnotes (B1 screening
criteria, B2 stimulus sampling + interaction stats, B5 Zellner)**,
zero suggestion chips remaining (or as close to zero as the API allows).

---

## 2. What's already done (so you don't redo)

### 2a. Phase A — Sophia/Jose suggestion strip
- 400 index-based requests applied to the live doc on 2026-05-09 ~20:43.
- 102 half-ops covered all 66 distinct pending suggestions (insertion
  + deletion halves of each Google Docs "replace" action emitted as
  separate ops; see §4 for why this matters).
- 4 explicit REJECTs (per Katy's screenshots/comments):
  - `suggest.fhdqknladir8` — keep "from the original set–where their
    prior selections were highlighted–to include in the display."
    procedural sentence (screenshot 4)
  - `suggest.rxpcar4aswjg` — keep Study 1 demographic fields
    (screenshot 7)
  - `suggest.s2yp417s640` — revert Study 2 Results paragraph to
    verbose original ("We found no significant effects of the impact
    of random assignment...") instead of Sophia's collapsed
    "Consistent with Study 1, none of the three..." (screenshot 6)
  - `suggest.59zqv9un4aw4` — revert Study 3B films F-test rewrite
    to verbose original ("the percentage of initial films selected
    with a (1) budget above $40M (F(1, 1992) = 65.06, p < .001)...")
    (screenshot 10)
- Style preservation via `updateTextStyle` for kept-committed runs
  (italic stat letters, Table 2 link, M_age subscript).
- 322/330 original suggestion flags cleared; 6 paragraph-style /
  text-style residue flags remain — Jose was asked to manually accept
  them in the UI (see §6c).

### 2b. Phase B — Katy's positive edits on top
- 22 replaceAllText ops converted to delete+insert via index lookup
  (raw replaceAllText returns 500 on this doc; see §6a).
- 3 createFootnote ops (B1, B2, B5) — see §3 for current footnote IDs.
- One off-spec patch: B2 footnote was missing the stimulus-set
  interaction sentence Katy added on PDF page 11. Fixed live with
  `fix_b2_footnote.py`. **THIS IS THE EXACT KIND OF DRIFT YOU NEED TO
  HUNT FOR ELSEWHERE.**

### 2c. Phase C — Drive comments
- 13 comments posted (10 Katy verbatim from screenshots + 3 needs-
  review markers for placeholder values). See `operations.json`
  `phaseC` for the full set.
- Comments are unanchored in the Drive sense (they appear as document-
  level comments with `[anchor: <first 60 chars>]` prefix in the
  body). Reverse-engineering the Drive anchor JSON for properly-
  anchored comments was punted; Jose can re-anchor manually if needed.

---

## 3. Current live state (the starting point of YOUR audit)

- **Doc ID:** `1H9jAvqG5CzQe_BksjkNmLwjLZSlAHvaEK1iyuHL23FI`
- **Current snapshot:** `gdoc_current.json` and `gdoc_current.txt`
  (re-fetched after every batch; refresh with `python dump_gdoc.py`
  before starting your audit)
- **Pre-apply snapshot (recovery point):** `gdoc_pre_apply_snapshot.json`
  / `.txt` — taken just before Phase A fired
- **Footnotes added by us:**
  - `kix.pe6y7w68dzy9` — B1 screening criteria
  - `kix.fuhxta3rz80s` — B2 stimulus sampling **+ stimulus-set stats**
  - `kix.rxnx50b5w6fn` — B5 Zellner Wald-tests
- **Drive comments:** all visible in the right rail; check via Drive
  comments API if needed

### 3a. ⚠ Jose's manual reverts on top of the apply

The live doc is NOT exactly `gdoc_simulated_postapply.txt`. Jose has
done at least one manual edit in the UI on top of the programmatic
apply, and there may be more:

1. **Study 1 Results — Wald-test / systems-of-equations paragraph
   REVERTED.** Katy's comment was about *keeping the full writeup of
   the systems of equations* — meaning the verbose original should be
   preserved, NOT replaced by Sophia's collapsed version AND NOT
   replaced by B4's "expanded" rewrite ("controlling for participant
   demographics… see Appendix for a complete discussion of the
   systems of simultaneous equations…"). The B4 op in
   `operations.json` was MISTAKEN — `reconciled_edits.md` R7+R9 read
   Katy's green text as a rewrite when she actually wanted the
   original kept. Jose has manually reverted B4's text replacement in
   the live doc.

   **Implication for the audit:**
   - The Study 1 Results paragraph should now read with the original
     verbose systems-of-equations writeup (the one that was already
     committed text before Sophia's pass). Verify this against the
     PDF page 12 area.
   - **B5 (Zellner footnote) anchor was `"all ps ≤ .010; see Table 2)."`
     which was inside B4's replace text.** If Jose's revert removed
     that exact phrase, the B5 footnote may now be anchored at an odd
     position or orphaned. Check whether `kix.rxnx50b5w6fn` still has
     a footnote reference in the body and whether it lands in the
     correct paragraph. If the footnote is now misplaced, you may need
     to delete it and re-create at the correct anchor in the original
     verbose writeup. Similarly for **D1** (`Wald tests employed, all
     ps ≤ .010; see Table 2).` — also lived inside B4's replace text).
   - **Codex round 5 spot-checked B4 and approved its post-A presence**,
     so until Jose's revert the doc had B4's replacement text. After
     the revert, audit fresh.

2. **The 6 stuck style-only suggestion chips** (paragraph spacing /
   indentation / 1-char "." run in p[40]) — Jose was asked to manually
   accept-or-reject these in UI. State unknown to this writer; check
   via `python dump_gdoc.py` and re-count flags before assuming they
   were cleared.

When in doubt: trust `gdoc_current.json` (live) over `gdoc_simulated_*`
(stale post-Jose-reverts).

---

## 4. The bug the prior pass solved (so you don't re-hit it)

In Google Docs, a single user "replace verbose with collapsed" action
creates one suggestion ID with BOTH insertion-tagged runs (the new
text) and deletion-tagged runs (the old text). The prior enum
conflated them into a single op flagged `kind=deletion`, so default-
ACCEPT would `replaceAllText(search="<new>+<old>", replace="")`,
deleting BOTH the new and the original.

**Fix already in place:** `enum_suggestions_v2.py` emits one entry per
`(sid, kind)` pair. The apply walks textRuns at run-level (one
decision per run) so overlapping span ranges across different sids
don't double-delete. See `apply_to_gdoc.py build_phase_a_requests`.

**For your audit:** if you decide to apply more patches, use the
**run-level apply pattern** in `apply_to_gdoc.py` (or simpler text-
search delete+insert via `apply_phase_b_via_index.py`'s `find_text_range`).
Don't touch the half-op enum logic unless you have a specific reason.

---

## 5. The audit methodology — what "every line" means

The PDF is **72 pages**. Pages with Katy green-text edits (per
`katy_pages.json`): **10, 11, 12, 13, 14, 15, 16, 17, 20, 21**.
But there are likely OTHER edits on other pages — Sophia's pink edits
or strikethroughs that Katy left alone but didn't visibly mark, plus
edits that the transcripts may have just missed. **Audit ALL 72
pages**, not just the 10 Katy-edited pages. Pink edits on non-green
pages still need to be reflected in the live doc.

### 5a. For every page

1. **Open the rendered page** at `pages/page_NNN.png` (150 DPI).
   For Katy-edited pages also have `pages_hi/page_NNN_hi.png` (300
   DPI) and `pages_hi/page_NNN_marked.png` (with yellow boxes around
   green runs).
2. **Read every line of the page.** Identify every textual element:
   - Black plain text — original committed text
   - **Pink text** (`#B80672` Sophia / `#9334E6` Jose) — proposed
     additions (inserted via Suggesting mode; show as pink/purple
     underlined or boxed in the print)
   - **Green text** (`#137333` Katy) — Katy's additions
   - **Strikethrough** through black/pink — proposed deletions
   - **Strikethrough through GREEN** — Katy's edit-of-her-own-edit
     (she added then changed her mind; the strikethrough is her FINAL
     intent — that text should NOT appear in the doc)
3. **Construct the ideal final text for that paragraph.** The rule is:
   - Apply every pink insertion (UNLESS Katy's green text or comment
     contradicts)
   - Apply every pink deletion (UNLESS Katy explicitly preserved —
     screenshot 4, screenshot 7 are examples)
   - Apply every green insertion (UNLESS green has its own
     strikethrough applied to it)
   - Apply every green deletion
   - Honor every Katy margin comment that explicitly overrides
     ("revert to prior writeup" → REJECT the rewrite, restore original)
4. **Pull the corresponding paragraph from the live doc** (search
   `gdoc_current.txt` for a unique substring of the paragraph's
   black/original text).
5. **Diff your ideal text against the live text.** Anything different
   is a discrepancy. Log it with:
   - Page number + paragraph identifier
   - The exact ideal text
   - The exact live text
   - Proposed patch (which `replaceAllText`-style search/replace would
     fix it, OR a description of the structural fix needed if it's a
     footnote/style issue)

### 5b. Cross-check: 10 comment screenshots

`screenshots/screenshot01.png` … `screenshot10.png` are Katy's
right-rail margin comments (extracted from the docx Jose was sent).
`katy_comments.md` is Claude's transcript of them — **codex round 5
flagged minor wording drift in the transcript** (e.g. screenshot 3
attribution typo "Sophia's deletion" should be "Jose Cervantez 8:34
AM May 7"; some study-label tags are off). When in doubt, trust the
PNG image, not the transcript.

For each of the 10 screenshots, confirm:
- The comment exists in the live Gdoc Drive comments list
- The comment's anchor text (the doc paragraph it points at) is
  present in the live doc as expected per Katy's intent
- Any DISPOSITION the comment implies (REJECT a Sophia change,
  RESTORE deleted text, etc.) is reflected in the live doc

### 5c. Specific discrepancy classes to hunt for

These are the easy-to-miss patterns; check each explicitly:

1. **Cross-paragraph green additions.** The PDF can wrap green text
   across line breaks; the prior pass occasionally treated each line
   as a separate edit and missed the join. (PDF page 11 footnote 2
   was this — "We also stimulus sampled..." landed in B2 but the
   trailing "The effect of gender feedback did not differ
   significantly..." was missed and attributed to a different green
   block on page 12.)
2. **Strikethrough green inside green.** When Katy added text and
   then crossed part of it out, both passes had inconsistent reads.
   Verify the trailing stats in B2 footnote ("p = .220 / p = .190")
   — Katy struck `treatment × set interaction: B = 0.041,` and
   `B = -0.022,` so only the p-values should remain. (Already fixed.)
3. **Pink edits on pages WITHOUT green.** The prior pass focused on
   Katy-edited pages; pink-only pages may have un-applied or wrongly-
   handled Sophia changes.
4. **Pink that Katy left alone but the prior pass REJECTed.** A REJECT
   should only happen when Katy's green text or comment contradicts.
   If a Sophia change was REJECTed without Katy backing, that's a bug.
5. **Footnote markers / numbering.** New footnotes may have shifted
   the numbering of pre-existing footnotes. Verify that each footnote
   reference in the body still points to the right footnote text.
6. **Tables** (`Tables: 6` in the doc) — the walk doesn't always show
   table cell contents in linear text; check tables visually too.
7. **Italic stat letters / subscripts / links** — the run-level apply
   restores `textStyle` but only the editable subset. Spot-check
   `M_age` (subscript), italic `B`/`t`/`p`/`F` in stat reports, and
   the `Table 2` link/underline in Study 1 Results.
8. **Demographic fields consistency** (Katy's screenshot 7 red line):
   verify every Methods/Participants section reports the same
   demographic categories (men/women/non-binary/Mage/race breakdown).
9. **The four Phase A REJECT areas** specifically — these were
   manual disposition calls that need re-confirming against the PDF:
   - Study 2 Procedure: "from the original set..." sentence preserved
   - Study 1 Methods: full demographics restored
   - Study 2 Results: verbose "We found no significant effects..." 
     paragraph (NOT Sophia's collapsed "Consistent with Study 1...")
   - Study 3B Results: verbose F-test enumeration (NOT Sophia's "the
     other three descriptive statistics shown.")
10. **Phase B cleanup ops B21-B25**: these patched whitespace/period
    residue from Sophia's deletions. Verify the patched areas now
    read cleanly.
11. **B4 Study 1 Wald-test paragraph — Jose reverted it manually.**
    See §3a. The live doc should have the verbose ORIGINAL systems-
    of-equations writeup, not B4's "expanded" rewrite. Audit this
    paragraph fresh against the PDF, ignoring `gdoc_simulated_*` for
    this region. Also check that B5 (Zellner footnote) and D1 (Phase
    C comment) still anchor correctly post-revert; both lived inside
    B4's replacement text and may now be orphaned or misplaced.

---

## 6. Known caveats / gotchas

### 6a. `replaceAllText` returns HTTP 500 on this doc

Persistent for unknown reasons (not just suggestion-chip-related; we
hit 500 even after stripping chips). Workaround: convert every
`replaceAllText` to `deleteContentRange + insertText` with index
lookup via `find_text_range` (see `apply_phase_b_via_index.py`).

### 6b. Multi-paragraph search

`find_text_range` only matches within a single paragraph. If your
search target spans `\n\n`, split it into two paragraph-internal
searches. (PDF B20 was this — the search included `Results\n\n` which
crossed a paragraph boundary; we split it.)

### 6c. Editing-vs-Suggesting mode toggle

If Jose's UI is in **Suggesting mode**, every API edit creates a
suggestion instead of committing. Symptoms: you fire `updateTextStyle`
and the change shows up as a NEW pending chip. Fix: have Jose flip
the mode toggle (top-right of doc, next to Share) to **Editing**
(pencil icon). This bit us during residue cleanup. **Always confirm
Jose is in Editing mode before applying.**

### 6d. Style residue on paragraphs

Even in Editing mode, `updateTextStyle` and `updateParagraphStyle` do
NOT auto-clear pending `suggestedTextStyleChanges` /
`suggestedParagraphStyleChanges` flags from the existing JSON state.
After Phase A there were 6 stuck style-only chips. Jose was asked to
accept them manually in the UI. If you see new style-only chips,
Jose's UI accept is the cleanest path.

### 6e. Non-breaking spaces

The Study 2 Results paragraph uses `\xa0` (non-breaking space) between
italic stat symbols (`B`, `t`, `p`) and `=`. Search strings that use
regular spaces won't match. Always preserve `\xa0` when crafting search
text in this region. (See B12 in `operations.json` for the pattern.)

### 6f. Footnote insertion index

Inside a footnote segment, the first valid insertion index is `1`
(not `0`). The handoff's old code used
`doc["footnotes"][fn_id]["content"][0]["startIndex"]` which raised
`KeyError`. Use `{"index": 1, "segmentId": fn_id}` for new footnote
content; for appending, find the last run's `endIndex - 1` (to land
before the trailing `\n`).

### 6g. Codex CLI safeguards

If you delegate review or analysis to Codex via `codex exec`, wrap
every invocation with all four:
1. **Pre-delete the deliverable** (so its presence after exit is an
   unambiguous success signal)
2. **Tee stdout+stderr to a session log** for post-mortem
3. **Hard timeout** via the Bash tool's `timeout` parameter (≥10
   minutes for vision reviews)
4. **Post-run validation**: confirm the deliverable file exists AND
   contains the structural markers your prompt asked for

See `feedback_codex_safeguards.md` in user memory.

### 6h. Codex CLI model selection

Don't pass `-m gpt-5.5-pro` — Codex's ChatGPT-account auth rejects
that model. Use the default (`codex exec` with no `-m` flag). The
default reasoning model has caught real bugs across multiple rounds.

---

## 7. The tools you have

### 7a. Inspecting state
- `python dump_gdoc.py` — fetches live state into
  `gdoc_current.json` / `gdoc_current.txt`
- `python -c "import json; doc = json.load(open('gdoc_current.json'))"`
  for ad-hoc JSON inspection
- `gdoc_pre_apply_snapshot.json` / `.txt` — pre-apply state for
  diffing if you need to know what we changed

### 7b. Reading PDF / images
- `katy_pdf.pdf` is the source of truth (72 pages)
- `pages/page_NNN.png` — 150 DPI render of every page
- `pages_hi/page_NNN_hi.png` — 300 DPI of green-edited pages
- `pages_hi/page_NNN_marked.png` — same with yellow boxes around green
- `screenshots/screenshot01.png` … `screenshot10.png` — Katy's
  margin comments, ordered 1–10 (note: the docx originals had
  alphabetical-order names so the file rename is intentional)
- `katy_comments.md` — verbatim transcripts of the 10 screenshots
  (with known minor drift; trust PNG when in doubt)

### 7c. Applying patches

For each discrepancy you find, the simplest patch shape is a
`(search, replace)` pair against the current text. Apply via:
```python
# Pattern from apply_phase_b_via_index.py / fix_b2_footnote.py:
doc = docs.documents().get(documentId=DOC_ID).execute()
rng = find_text_range(doc, search)  # returns (start, end) or None
if rng:
    docs.documents().batchUpdate(documentId=DOC_ID, body={"requests": [
        {"deleteContentRange": {"range": {"startIndex": rng[0], "endIndex": rng[1]}}},
        {"insertText":         {"location": {"index": rng[0]}, "text": replace}},
    ]}).execute()
```

For footnote content updates: see `fix_b2_footnote.py`. Pattern:
delete the existing run content within the segment, insert the new
text at index 1.

For new footnotes: `createFootnote` with `{"location": {"index": idx}}`,
then `insertText` with `{"location": {"index": 1, "segmentId": fn_id}}`.

For Drive comments: `drive.comments().create(fileId=DOC_ID, body=...)`.
See `apply_to_gdoc.py run_phase_c` for the inline-anchor workaround.

### 7d. Auth

Token at `C:/Users/jcerv/.config/gws/token.json`. Loaded by every
script via the same `load_creds()` pattern.

---

## 8. Suggested workflow

1. **Read this whole HANDOFF.md.**
2. **Confirm baseline:** run `python dump_gdoc.py` to refresh the
   current state. Verify Phase A residue chips status (should be 6
   or fewer; Jose may have manually cleared some).
3. **Page-by-page audit:** open each PDF page (start with the 10
   green pages), read every line, construct the ideal final text,
   compare to live, log discrepancies. Build an `audit_log.md` with:
   ```
   ## Page N
   - Status: CLEAN | DRIFT
   - Drift items:
     - [n] Description: ...
       - Ideal: "..."
       - Live:  "..."
       - Patch: search/replace OR structural fix
   ```
4. **Cross-check screenshots** (1–10) against live Drive comments and
   their anchor text.
5. **Check "specific classes" listed in §5c** — these are the easy-
   to-miss patterns.
6. **Apply patches** as you find them, in batches (don't fire one
   request at a time; batch ~10–20 per call). Always re-fetch after
   each batch — indices shift.
7. **Verify after each batch:** re-fetch, find the patched substring,
   confirm it now reads correctly.
8. **Final pass:** when you believe the doc matches the PDF intent,
   spawn Codex with the visual brief (read the PDF + view the live
   doc) for an independent verdict. Use the safeguards in §6g.
9. **Hand back to Jose** with:
   - The completed `audit_log.md`
   - List of patches applied
   - Codex verdict
   - Any unresolved discrepancies that need human judgment

---

## 9. Files in this directory — what each one is

### Source of truth
- `katy_pdf.pdf` — Katy's offline-edited PDF print (72 pp)
- `pages/page_NNN.png` — 150 DPI page renders
- `pages_hi/page_NNN_hi.png`, `_marked.png` — 300 DPI of edited pages
- `screenshots/screenshot01.png` … `10.png` — Katy's margin comments
- `katy_comments.md` — transcripts of the 10 comments (minor drift)

### Live state
- `gdoc_current.json` / `.txt` — most recent fetch (refresh before use)
- `gdoc_pre_apply_snapshot.json` / `.txt` — pre-apply baseline

### Spec (record of what we ATTEMPTED to apply — see §3a for deltas)
- `dispositions.json` — sid → accept/reject map
- `operations.json` — full Phase A/B/C op spec (post-bug-fix version).
  **NOTE:** B4 was MISREAD per Katy's intent and Jose has reverted it
  manually in UI; ignore B4 in this file when auditing. B5 + D1 may
  also be orphaned because their anchors lived inside B4's replace
  text — verify against live doc.
- `pending_suggestions_v2.json` — per-half-op enumeration of pending
  suggestions (pre-apply state)

### Apply scripts
- `apply_to_gdoc.py` — main driver; run-level Phase A apply with
  style preservation
- `apply_phase_b_via_index.py` — Phase B fallback (replaceAllText →
  delete+insert)
- `apply_b20_oneoff.py` — B20 was multi-paragraph; this applies the
  paragraph-internal version
- `fix_b2_footnote.py` — replaces B2 footnote content (Katy's
  stimulus-set additions)
- `apply_residue_cleanup.py`, `force_clear_residue.py` — attempts
  at clearing the 6 stuck style flags (only partial success)
- `dump_gdoc.py`, `dump_preview_accepted.py` — fetch helpers

### Validation
- `replay_phase_a.py` — replay simulator that runs the actual request
  stream against a char-level model of the doc and confirms it
  matches the expected post-A text
- `simulate_v2.py` — Phase B simulator on top of post-A
- `validate_operations.py` — uniqueness checks for Phase A spans,
  Phase B/C anchors

### Apply artifacts
- `apply_log.txt`, `apply_run_output.log` — chronological apply logs
- `gdoc_simulated_post_a.txt`, `gdoc_simulated_postapply.txt` — what
  the doc was supposed to read after A and A+B. **NOTE:** these
  reflect the SPEC's intent; the live doc has Jose-manual-reverts on
  top (see §3a). Use as a reference, not as ground truth.
- `dryrun_apply.txt` — full request-stream preview
- `paragraph_diff.txt` — block-level diff of current vs simulated

### Codex audit trail (read-only history)
- `codex_review_round1.md` … `round6.md` — 6 rounds of independent
  review. Each round caught real issues. Round 4 caught the run-level
  vs half-op apply bug. Round 5 added image-based verification.
  Round 6 confirmed the C10/C1 anchor fixes.
- `codex_review_round*_brief.md` — the prompts we sent
- `codex_review_round*_session.log` — full Codex session output

### Archived / known-imperfect (DON'T trust as truth)
- `claude_edits_all.md`, `codex_edits_all.md`, `reconciled_edits.md`
  — early-pass PDF transcripts. **Known to have missed at least one
  green addition (the B2 stimulus-set sentence) and to have minor
  wording drift.** Useful as starting points but verify against the
  PDF directly.
- `katy_pages.json`, `runs_index.md` — page-render metadata

### Older code (kept for reference, archived to `archive/`)
- `apply_edits.py`, `enum_suggestions.py` — first-pass enum (had the
  half-op conflation bug)
- `build_operations.py`, `build_operations_v2.py` — earlier op spec
  versions
- `simulate_and_dryrun.py` — older simulator

---

## 10. Memory updates already in place (user auto-memory)

These memories were saved during this session and will load
automatically in future conversations:

- `feedback_gdoc_aggregate_range_bug.md` — per-span vs aggregate range
  rule
- `feedback_codex_safeguards.md` — Codex invocation wrapper pattern
- `project_katy_offline_recovery.md` — top-level project context

---

## 11. One last thing

**The user has stated "100% confidence" tolerance. Before applying any
patch, dry-run it (search the doc, verify the match is unique, verify
the result reads as intended). Show Jose the patch list before firing.
If a discrepancy looks ambiguous — Katy's intent isn't obvious from
the page — flag it as a Drive comment for live review with Katy on
Monday rather than guessing.**

Don't trust transcripts. Read the pixels. Find the drifts. Patch.

Good luck.

— Claude (handing off, 2026-05-09 ~21:30 ET)
