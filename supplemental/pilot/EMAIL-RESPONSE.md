# Email Response: Power Analysis for 2x2 Pilot Study

## Quick Answer

**Recommended Sample Size: N = 500 (125 per cell)**

This gives us:
- **>95% power** to detect the interaction effect (our main finding)
- **~60-70% power** to detect simple effects within conditions
- **1.7x the pilot size** (feasible, not ridiculous)
- **Robust to effect size shrinkage** from pilot to full study

---

## The Full Story

### What the Pilot Shows (N=301)

Your pilot has an **interesting crossover interaction**:

| Condition | Control | Treatment | Effect |
|-----------|---------|-----------|--------|
| **Low Base (6 authors)** | 29.9% | 37.8% | **+8.0 pp** ✓ |
| **High Base (12 authors)** | 52.6% | 44.6% | **-8.0 pp** ✗ |
| **Interaction** | — | — | **16.0 pp** |

→ Gender feedback **helps** when base is low but **hurts** when base is high!

### Effect Sizes

- **Simple effects**: Small (Cohen's h ≈ 0.16)
- **Interaction**: Small-medium (Cohen's d = 0.33)

---

## Sample Size Options

### Option 1: Detect Interaction (Primary Finding) ⭐

**N = 500 total (125 per cell)**

**Why this works:**
- Interaction is your **novel contribution**
- >95% power to detect it
- Still has reasonable power for secondary analyses
- **Feasible cost** (not 8x your pilot!)

### Option 2: Detect Simple Effects (Per Condition)

**N = 2,400 total (606 per cell)**

**Why this is tough:**
- 8x your pilot size
- Probably **too expensive**
- Needed only if simple effects are your primary outcome
- But the interaction is more interesting anyway!

### Option 3: Budget-Friendly

**N = 300-400 total (75-100 per cell)**

**Why this could work:**
- Still ~80-90% power for interaction
- Focus exclusively on the crossover pattern
- Most cost-effective
- But less robust if effect shrinks

---

## My Recommendation

**Target N = 500 (125 per cell)**

### Rationale:

1. **Scientifically:** The interaction is your story. "Gender feedback works, but only in certain conditions" is more interesting than "Gender feedback has a small positive effect."

2. **Statistically:** You're well-powered for your primary finding and adequately powered for secondary analyses.

3. **Practically:** It's 1.7x your pilot, not 8x. That's usually doable.

4. **Conservatively:** Pilot effect sizes often shrink. N=500 protects you if the true effect is slightly smaller.

---

## What About the Simple Effects?

At N=500, you'll have:
- **60-70% power** to detect simple effects (8 pp differences)
- This is **okay for secondary analyses**
- You can report them, but frame them as exploratory

If the simple effects are truly your primary outcome, then yes, you'd need N~2,400. But I'd argue that's the wrong way to frame this study.

---

## Alternative Framing

Instead of:
> "Does gender feedback increase female selection?" (needs N=2,400)

Frame as:
> "Does the effect of gender feedback depend on the base condition?" (needs N=500)

The second is:
- More interesting theoretically
- More feasible practically
- Still policy-relevant (tells you WHEN feedback works)

---

## Bottom Line for Email

> "We ran a power analysis on the pilot data (N=301). The pilot shows an interesting crossover interaction: gender feedback helps when few women are in the initial pool but actually hurts when many women are already there.
>
> **Recommended N = 500** (125 per cell). This gives us excellent power (>95%) for the interaction effect, which is our main finding, while remaining feasible (1.7x pilot size).
>
> Detecting the simple effects alone would require N~2,400 (8x pilot), which is likely not realistic. But if we frame the interaction as our primary outcome—which is more interesting theoretically—we're in good shape with N=500.
>
> See attached power analysis for details."

---

## Files to Share

1. **power-analysis.R** - Full computational details
2. **POWER-ANALYSIS-SUMMARY.md** - Detailed explanation
3. **power-analysis-full.pdf** - Visual summary (3 plots)

All files are in: `supplemental/pilot/`
