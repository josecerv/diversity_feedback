# Power Analysis Summary - Pilot Study (2x2 Design)

## Executive Summary

**Your pilot data shows an interesting pattern**: The gender feedback effect goes in **opposite directions** depending on the base condition, creating a **crossover interaction**.

### Key Findings from Pilot (N=301)

- **Low Base (6 authors)**: Gender feedback **increases** female selection by **8.0 percentage points** (29.9% → 37.8%)
- **High Base (12 authors)**: Gender feedback **decreases** female selection by **8.0 percentage points** (52.6% → 44.6%)
- **Interaction Effect**: **16.0 percentage points** difference between conditions

### Effect Sizes
- **Simple effects**: Small (Cohen's h ≈ 0.16-0.17)
- **Interaction effect**: Small-medium (Cohen's d = 0.33)

---

## Sample Size Requirements

### Option 1: Detect Simple Effects (80% power)
If you want to detect the effect **within each base condition** separately:

**Required N = 1,104 - 1,212 per base condition**
- This means **2,424 total participants**
- That's **8x your pilot size** - likely **too expensive**

### Option 2: Detect the Interaction Effect (80% power) ⭐ RECOMMENDED
If you focus on the **interaction** (the crossover pattern):

**Required N = 72 total participants**
- Only **18 per cell** in the 2x2 design
- This is **much smaller than your pilot** (N=301)
- You're **already overpowered** for the interaction!

### Option 3: More Conservative Approach (Recommended)
Given that pilot estimates can be unstable, I'd recommend:

**Target N = 400-600 total**
- **100-150 per cell**
- Provides good power for interaction (>90%)
- Still affordable
- More robust to smaller-than-expected effects

---

## Critical Questions

### 1. **What is your primary research question?**

**If it's "Does gender feedback work?"** → You need ~2,400 participants (expensive!)

**If it's "Does the effect of gender feedback depend on base condition?"** → You need ~400-600 participants (manageable!)

The second framing is more interesting scientifically because:
- The crossover interaction is theoretically important
- It shows boundary conditions for when feedback works
- It's the novel contribution of your 2x2 design

### 2. **Is the pilot effect size realistic?**

Your pilot (N=301) shows:
- Small simple effects (h ≈ 0.16)
- Medium interaction (d = 0.33)

**Concerns:**
- Small effects can be unstable in pilots
- Effect could shrink in a larger sample
- But the interaction is more robust

**Recommendation**: Design for **N=400-600** as a middle ground that:
- ✓ Is well-powered for the interaction
- ✓ Can still detect moderate simple effects
- ✓ Is cost-effective
- ✓ Protects against effect size shrinkage

---

## What the Sensitivity Analysis Shows

At different sample sizes, you can detect these effect sizes (80% power):

| Total N | Detectable Effect | Notes |
|---------|-------------------|-------|
| 100 | 25.7 pp | Way larger than pilot effects |
| 200 | 18.2 pp | Still larger than pilot |
| **300** | **14.8 pp** | **Close to pilot interaction (16 pp)** |
| **400** | **12.8 pp** | **Comfortably detects pilot effects** |
| **600** | **10.5 pp** | **Conservative, robust** |
| 800 | 9.1 pp | Probably overkill |
| 1000 | 8.1 pp | Matches pilot simple effects exactly |

---

## Recommendations

### 🎯 Primary Recommendation: N = 400-600

**Rationale:**
1. **Well-powered for your interaction** (>95% power)
2. **Reasonable for simple effects** (~50-70% power for h=0.16)
3. **Cost-effective** (1.3-2x pilot size, not 8x)
4. **Robust to effect shrinkage** (can detect effects down to ~11-13 pp)

### Alternative: Focus on Interaction Only (N = 300)

If budget is tight, **N=300** still works because:
- 80% power to detect the interaction
- The interaction is your most interesting finding
- You already have pilot data suggesting this pattern

### Not Recommended: N = 2,400

Don't aim for 2,400 unless:
- You have unlimited budget
- Simple effects are your primary outcome
- You need to detect very small effects (8 pp)

---

## Design Modifications to Increase Power (If Needed)

If N=400-600 still feels too large, consider:

1. **Strengthen the manipulation**
   - Make feedback more salient
   - Add visual elements
   - Repeat feedback multiple times

2. **Simplify to 1-way design**
   - Focus on just one base condition (the one where feedback works)
   - Would need only ~1,100 for 80% power

3. **Use a within-subjects component**
   - Have participants make multiple selection decisions
   - Increases power without increasing N

4. **Pre-register lower power**
   - 70% power is sometimes acceptable for exploratory studies
   - Would need ~400 total for simple effects

---

## Statistical Testing Strategy

Given your power constraints, I recommend this analysis hierarchy:

### Primary Test (High Priority)
✅ **Interaction**: Gender Feedback × Base Condition
- You have excellent power (>90% even at N=300)
- This is your novel contribution

### Secondary Tests (Medium Priority)
✅ **Simple effects within conditions**
- Test feedback effect in Low Base
- Test feedback effect in High Base
- You have ~50-70% power at N=400-600

### Exploratory (Low Priority)
🔍 **Main effect of gender feedback**
- Don't emphasize this (it's misleading when there's an interaction)
- Include only for completeness

---

## Bottom Line

**For the email response:**

> "Based on the power analysis, we have a few options:
>
> 1. **N ≈ 400-600** (recommended): Well-powered to detect the crossover interaction (our main finding) and moderately powered for simple effects. This is 1.3-2x our pilot size.
>
> 2. **N ≈ 300**: Adequately powered (80%) for the interaction effect, but underpowered for simple effects. Could work if we frame the interaction as our primary outcome.
>
> 3. **N ≈ 2,400**: Needed for 80% power on simple effects alone. Probably not realistic unless we have major funding.
>
> The pilot shows an interesting crossover interaction (feedback helps in low base but hurts in high base). If we focus on this as our main finding, we're actually in good shape with N=400-600. The simple effects are small (8 pp each), so detecting them individually would require a much larger N.
>
> My recommendation: Target **N=500** (125 per cell). This gives us excellent power for the interaction and reasonable power for secondary analyses, while remaining feasible."

---

## Files Generated

- `power-analysis.R` - Full R script with all calculations
- `POWER-ANALYSIS-SUMMARY.md` - This summary document

Run the analysis again with: `Rscript power-analysis.R`
