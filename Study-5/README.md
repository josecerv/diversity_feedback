# Study 5: AI Expert Selection

## Study Design

### Overview
Participants selected 3 AI experts for a hypothetical NPR podcast interview from a grid of 25 experts.

### Experimental Manipulation
- **Treatment vs Control**: Participants were randomly assigned to either:
  - **Treatment**: Shown feedback about women + 2 randomly selected other attributes
  - **Control**: Shown feedback about 3 non-women attributes only

- **Set Assignment (1 vs 2)**: The feedback percentages shown varied by set:
  - **Set 1**: Women=10%, Age=30%, University=50%, Location=10%
  - **Set 2**: Women=20%, Age=50%, University=10%, Location=70%

### Dependent Variable
The primary DV is `women_proportion`: the proportion of women selected among the 3 choices (ranges from 0 to 1).

### Hypothesis
Treatment assignment (showing women feedback) increases the proportion of women selected relative to control.

## Expert Categories

### Women Experts (6 of 25 = 24%)
1. Emily Kwong
2. Moira Gunn
3. Brittany Luse
4. Zoe Kleinman
5. Davar Ardalan
6. Jane Barrett

### Other Attributes
- **Under 50**: Bobby Allyn, Emily Kwong, Paris Marx, Kevin Roose, Ezra Eeman
- **West Coast**: Erik Brynjolfsson, Moira Gunn, Ethan Mollick, Ed Zitron, Kevin Roose, Andrew Ng
- **University**: Erik Brynjolfsson, Austan Goolsbee, Ethan Mollick, Anton Korinek, Andrew Ng

## Statistical Analyses

### Primary Analysis
Linear regression with robust (HC3) standard errors:
```
women_proportion ~ treatment
```

### Set Assignment Analysis
The script includes three complementary statistical approaches to test whether Set 1 vs Set 2 matters:

#### 1. **Main Effect of Set**
Tests if the set assignment (different feedback percentages) directly affects the proportion of women selected:
```
women_proportion ~ set_num
```

#### 2. **Interaction Analysis**
Tests if the treatment effect varies by set (dose-response):
```
women_proportion ~ treatment * set_num
```
- The interaction term tests: H0: Treatment effect is same for both sets
- If significant, suggests the magnitude of feedback percentages matters

#### 3. **Stratified Analysis**
Examines treatment effects separately within each set:
```
# Within Set 1 (women feedback shows 10%)
women_proportion ~ treatment | set_num == 1

# Within Set 2 (women feedback shows 20%)
women_proportion ~ treatment | set_num == 2
```

#### 4. **Equivalence Testing**
Rather than testing if sets are different, tests if they are statistically equivalent:
- Uses Two One-Sided Tests (TOST) approach
- Defines practical equivalence bounds (e.g., ±10 percentage points)
- Reverses burden of proof: null hypothesis is "effects are different"

### Secondary Analyses
Effects of other attribute feedback (age, location, university) on their respective proportions selected.

### Robustness Checks
1. Controlling for demographics (gender, race, age)
2. Logistic regression on binary outcome (any woman selected)
3. System of simultaneous equations comparing women feedback effect to other attributes

## Files Generated
- `Study5.csv`: Processed data from Qualtrics
- `Study-5.pdf`: Full analysis report with results
- `Figure-Study5.pdf`: Visualization of results across all attribute types
- `study5_estimates.rds`: Regression estimates for meta-analysis

## Key Features Matching Other Studies
The analysis follows the same structure as Studies 1A, 1B, 2A, 2B, 3, and 4:
- Same R packages (qualtRics, dplyr, lmtest, sandwich, etc.)
- Robust HC3 standard errors
- Demographic analysis section
- Primary/robustness/secondary analysis structure
- System of simultaneous equations for cross-attribute comparisons
- Visualization with consistent color scheme (#990000 for control, #011F5B for treatment)

## Survey Information
- **Survey ID**: SV_diYpq4P39svNYou
- **Platform**: Qualtrics (yul1.qualtrics.com)
- **Data Start Date**: October 1, 2025
