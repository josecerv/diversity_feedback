# R2 response letter — Google Doc pointers

Created 2026-04-24 by uploading `response_letter_R2_shell.docx` via
`podcasts-analysis/scripts/gdocs.py upload`. Single-tab doc (`t.0`) — so
whole-doc `gdocs.py replace` is safe here, unlike the Wharton manuscript doc.

- **Doc ID:** `1YBHCMwD84xC3Qw38CPEzz1iv3JKg7Mw9jTJqyZGptkI`
- **URL:** https://docs.google.com/document/d/1YBHCMwD84xC3Qw38CPEzz1iv3JKg7Mw9jTJqyZGptkI/edit
- **Title:** `ResponseLetter_DoesFeedbackEnhanceDiversity_R2 — 2026-04-24`
- **Owner:** `j.cerv12@gmail.com` (OAuth account used by `gdocs.py`)
- **Writer share:** `josecerv@wharton.upenn.edu`

## Placeholder labels (reference)

These are the 14 bold `RESPONSE: [RESPONSE HERE — <label>]` tags embedded
in the doc. Keep them stable so `gdocs.py edit-replace --find "[RESPONSE
HERE — <label>]" --replace "..."` remains a precise, one-shot write.

Department Editor (6):

- `opening_thanks` — Wu's paragraphs 1–3 (thank-you + reviewers signed off + "considerably stronger")
- `minor_revision_framing` — Wu's "I am calling for a 'Minor Revision'..." framing incl. R2 nuance nudge
- `style_streamlining` — Wu's slog/streamlining/page-limit request (DE item a)
- `study1_attribute_importance` — Wu's post-test-or-rerun ask (DE item b)
- `base_rate_initial_use` — Wu's "initial appropriate use" alternative (DE item c)
- `closing_thanks` — Wu's closing

Associate Editor (1):

- `ae_no_comments` — brief thank-you placeholder even though the AE wrote no report

Reviewer 1 (4):

- `r1_opening_praise` — Reviewer 1's three-paragraph praise opener
- `r1_small_1_citations` — update Kluger & DeNisi / Schultz references
- `r1_small_2_intro_theory` — intro cite update + "that other goal emphasized enough" theory
- `r1_closing_praise` — Figure 2 + reputation-management praise lines

Reviewer 2 (3):

- `r2_opening` — two-comments framing
- `r2_timestamped` — dated/pre-2024 language + DEI pullback context
- `r2_theoretical_contribution` — scope + culturally heterogeneous orgs + motivation-to-respond-without-prejudice

## Edit commands

From the `podcasts-analysis` project directory (OAuth lives there):

```bash
# Dry-run a replace on one placeholder to preview the write.
python scripts/gdocs.py edit-replace 1YBHCMwD84xC3Qw38CPEzz1iv3JKg7Mw9jTJqyZGptkI \
    --tab t.0 \
    --find "[RESPONSE HERE — opening_thanks]" \
    --replace "Thank you for your kind words..." \
    --dry-run

# Whole-doc overwrite after a full rebuild of the local .docx.
python scripts/gdocs.py replace \
    C:/Users/jcerv/Jose/diversity_feedback/revision-analysis/response_letter_R2_shell.docx \
    1YBHCMwD84xC3Qw38CPEzz1iv3JKg7Mw9jTJqyZGptkI

# Sync-down to a local .docx for reading / diff.
python scripts/gdocs.py export 1YBHCMwD84xC3Qw38CPEzz1iv3JKg7Mw9jTJqyZGptkI \
    C:/Users/jcerv/Jose/diversity_feedback/revision-analysis/response_letter_R2_snapshot.docx
```
