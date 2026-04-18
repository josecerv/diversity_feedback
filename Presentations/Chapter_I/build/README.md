# Chapter I deck — build assets

The finished deck is `../Cervantez_Chapter_I_DiversityFeedback.pptx`. This
folder holds the reusable assets and utility script that produced it.

## Contents

- `download_author_photos.py` — pulls the 25 author portraits from the
  Study-4A Qualtrics survey (`IM_` image IDs embedded in QID40). Run it
  once; images go into `authors_photos/`.
- `authors_photos/` — the Qualtrics-sourced author headshots used in the
  slide 9 stimuli grid. Filenames are slugs of the author names.
- `wbl_crops/` — images extracted from the WBL deck that are referenced
  on specific slides:
  - `study1_grid_4rows.png` / `study1_grid_4rows_hi.png` — 4-row crop of
    the Study 1 film grid (slides 5 and 7).
  - `study3_moderation_0.jpg` (teachers) / `study3_moderation_1.jpg`
    (nurses) — the interstitial photos on slide 11.

## Rebuilding

The build scripts that produced the current state have been removed (they
were accumulated over many iteration passes). The pptx is self-contained —
open it in PowerPoint/Keynote to edit. If you need to rebuild from
scratch, see git history for the full sequence, or ask Claude to
regenerate specific slides using the assets in this folder.
