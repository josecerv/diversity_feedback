# Codex round-6 review — confirm round-5 fixes

You returned `FLAG_ISSUES` in round 5 with one substantive blocker
(C10 anchor not unique) plus minor cosmetic drift in the transcript.
Claude has applied the C10 fix. While doing so, the tightened validator
caught a SECOND ambiguous anchor (C1) that round-5 did not flag, and
Claude fixed that too.

This round is focused: confirm both fixes hold and the spec is now
ready to apply.

## Specific fixes to verify

### Fix 1 — C10 anchor (your round-5 blocker)

OLD: `"the percentage of initial films selected with a (1) budget above $40M"`
NEW: `"gender feedback condition on the likelihood of selecting a 7th film with a woman protagonist"`

Verify in `gdoc_simulated_postapply.txt`:
- The new anchor appears EXACTLY ONCE
- It lands in the Study 3B/films/gender F-test paragraph (the screenshot 10 target), NOT the Study 3A/race F-test paragraph

### Fix 2 — C1 anchor (caught by tightened validator)

OLD: `"had a minimum 90% approval rate"` (ambiguous — appears 5x across studies)
NEW: `"We only recruited participants who were U.S. based"`

Verify in `gdoc_simulated_postapply.txt`:
- The new anchor appears EXACTLY ONCE
- It lands in the B1 footnote text (the "footnote pattern" Katy was advocating)

### Fix 3 — validator now checks Phase C anchor uniqueness

`validate_operations.py` Phase C check changed from "appears at all" to
"appears EXACTLY ONCE." Confirm the validator code reflects that, and
that current output reads `13/13 anchors unique in post-apply`.

## Sanity sweep

Then a quick sweep:
- Walk all 13 Phase C anchors and confirm each appears exactly once in
  `gdoc_simulated_postapply.txt`. (You can replicate the validator's
  count check.)
- Walk all 25 Phase B anchors and confirm each appears exactly once in
  `gdoc_simulated_post_a.txt`.
- Re-confirm `replay_diagnostics.json` reports `match: true` and zero
  errors.

## What I'm NOT asking you to redo

You already approved (round 5):
- The 4 REJECT sids and their disposition mapping to screenshots 4/6/7/10
- The run-level apply, conflict resolution, request order
- 98/98 style restores
- Replay match
- The Phase B / Phase C/ACCEPT-default coverage of screenshots 1/2/3/5/8/9

Don't re-walk those unless something in the current files contradicts
your prior verdict.

## Output

Write `codex_review_round6.md`:

```markdown
# Codex round-6 review

## Verdict
APPROVE_TO_APPLY | FLAG_ISSUES | REJECT

## Fix 1 — C10 anchor
- Unique in post-apply: YES / NO (count: N)
- Lands in Study 3B films F-test paragraph: YES / NO

## Fix 2 — C1 anchor
- Unique in post-apply: YES / NO (count: N)
- Lands in B1 footnote text: YES / NO

## Fix 3 — validator uniqueness check
- Code change correct: YES / NO
- Output reads 13/13 unique: YES / NO

## Sanity sweep
- All 13 Phase C anchors unique in post-apply: YES / NO
- All 25 Phase B anchors unique in post-A: YES / NO
- Replay match: YES / NO

## New issues (if any)
- ...
```

If APPROVE_TO_APPLY and no new issues: be terse.

Read-only on every file except `codex_review_round6.md`.
