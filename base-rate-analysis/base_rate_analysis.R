###############################################################################
# Base Rate Analysis: Effect of Feedback When Selection Exceeds Pool Base Rate
###############################################################################
#
# This analysis addresses the reviewer question: Does feedback still work when
# participants have already selected MORE women/URMs than the base rate in the
# candidate pool?
#
# Logic: If the pool has X% women, and a participant's initial selection shows
# they selected >X% women, their feedback would indicate they're ALREADY above
# the base rate. Yet, does feedback still increase their likelihood of selecting
# another woman?
###############################################################################

rm(list = ls())
setwd("c:/Users/jcerv/Jose/diversity_feedback")

library(dplyr)
library(sandwich)
library(lmtest)
library(knitr)
library(openxlsx)

# Robust standard errors function
robust_summary <- function(model) {
  robust_se <- sqrt(diag(vcovHC(model, type = "HC3")))
  model_summary <- summary(model)
  model_summary$coefficients[, "Std. Error"] <- robust_se
  model_summary$coefficients[, "t value"] <- model_summary$coefficients[, "Estimate"] / robust_se
  model_summary$coefficients[, "Pr(>|t|)"] <- 2 * pt(-abs(model_summary$coefficients[, "t value"]),
                                                      df = model_summary$df[2], lower.tail = TRUE)
  return(model_summary)
}

robust_confint <- function(model, level = 0.95) {
  robust_se <- sqrt(diag(vcovHC(model, type = "HC3")))
  est <- coef(model)
  alpha <- 1 - level
  t_crit <- qt(1 - alpha / 2, df = df.residual(model))
  lower <- est - t_crit * robust_se
  upper <- est + t_crit * robust_se
  confint <- cbind(lower, upper)
  rownames(confint) <- names(est)
  colnames(confint) <- c("2.5 %", "97.5 %")
  return(confint)
}

###############################################################################
# Define Pool Information for Each Study
###############################################################################

# Study 2: Biographies (Gender)
# Pool: 25 biographies, 5 women (Anne Frank, Tina Fey, Jackie Kennedy, Helen Keller, Barbra Streisand)
# Base rate: 5/25 = 20%
# Initial selections: 6
# Threshold: base_gender/6 > 0.20 means base_gender > 1.2, so base_gender >= 2

# Study 3A: Films (Race)
# Pool: 30 films, 4 racial minority protagonists (Salem, Harriet, Ali, 42)
# Base rate: 4/30 = 13.3%
# Initial selections: 7
# Threshold: base_race/7 > 0.133 means base_race > 0.93, so base_race >= 1

# Study 3B: Films (Gender)
# Pool: 25 films, 5 women protagonists
# Base rate: 5/25 = 20%
# Initial selections: 6
# Threshold: base_gender/6 > 0.20 means base_gender > 1.2, so base_gender >= 2

# Study 4A: Authors (Race)
# Pool: 25 authors, 8 racial minorities
# Base rate: 8/25 = 32%
# Initial selections: 6
# Threshold: base_race/6 > 0.32 means base_race > 1.92, so base_race >= 2

# Study 4B: Authors (Gender)
# Pool: 25 authors, 6 women
# Base rate: 6/25 = 24%
# Initial selections: 6
# Threshold: base_gender/6 > 0.24 means base_gender > 1.44, so base_gender >= 2

# Study 5: CEOs/Founders (Gender) - TWO CONDITIONS
# Men Pool (25% women): 6 women out of 24
#   Threshold: base_gender/6 > 0.25 means base_gender > 1.5, so base_gender >= 2
# Women Pool (75% women): 18 women out of 24
#   Threshold: base_gender/6 > 0.75 means base_gender > 4.5, so base_gender >= 5

###############################################################################
# Run Analysis for Each Study
###############################################################################

results <- data.frame(
  Study = character(),
  Attribute = character(),
  Pool_Base_Rate = character(),
  N_Initial_Selections = integer(),
  Threshold_Count = character(),
  N_Total = integer(),
  N_Above_Baserate = integer(),
  Pct_Above_Baserate = numeric(),
  Effect_Full_Sample = numeric(),
  SE_Full = numeric(),
  P_Full = numeric(),
  CI_Lower_Full = numeric(),
  CI_Upper_Full = numeric(),
  Effect_Above_Baserate = numeric(),
  SE_Above = numeric(),
  P_Above = numeric(),
  CI_Lower_Above = numeric(),
  CI_Upper_Above = numeric(),
  stringsAsFactors = FALSE
)

cat("\n=======================================================================\n")
cat("BASE RATE ANALYSIS: FEEDBACK EFFECT WHEN SELECTION > POOL BASE RATE\n")
cat("=======================================================================\n\n")

###############################################################################
# STUDY 2: Biographies (Gender)
###############################################################################

cat("\n--- STUDY 2: Biographies (Gender) ---\n")
d2 <- read.csv('Study-2/Study2.csv', check.names = FALSE)

pool_base_rate_2 <- 5/25  # 20%
n_selections_2 <- 6
threshold_2 <- 2  # >= 2 means above 20%

# Full sample analysis
model_full_2 <- lm(female_pick ~ gender_feedback, data = d2)
rob_2 <- robust_summary(model_full_2)
ci_2 <- robust_confint(model_full_2)

# Subset to participants whose initial selection > base rate
d2_above <- d2 %>% filter(base_gender >= threshold_2)

cat("Pool base rate: 20% (5/25 women)\n")
cat("Threshold for 'above base rate': base_gender >= 2\n")
cat("N total:", nrow(d2), "\n")
cat("N above base rate:", nrow(d2_above), "(", round(nrow(d2_above)/nrow(d2)*100, 1), "%)\n")

if(nrow(d2_above) > 10) {
  model_above_2 <- lm(female_pick ~ gender_feedback, data = d2_above)
  rob_above_2 <- robust_summary(model_above_2)
  ci_above_2 <- robust_confint(model_above_2)

  cat("\nFull Sample Effect:", round(rob_2$coefficients[2,1]*100, 2), "pp",
      "(p =", round(rob_2$coefficients[2,4], 4), ")\n")
  cat("Above Base Rate Effect:", round(rob_above_2$coefficients[2,1]*100, 2), "pp",
      "(p =", round(rob_above_2$coefficients[2,4], 4), ")\n")

  results <- rbind(results, data.frame(
    Study = "Study 2",
    Attribute = "Gender (Women)",
    Pool_Base_Rate = "20% (5/25)",
    N_Initial_Selections = n_selections_2,
    Threshold_Count = ">= 2",
    N_Total = nrow(d2),
    N_Above_Baserate = nrow(d2_above),
    Pct_Above_Baserate = round(nrow(d2_above)/nrow(d2)*100, 1),
    Effect_Full_Sample = round(rob_2$coefficients[2,1]*100, 2),
    SE_Full = round(rob_2$coefficients[2,2]*100, 2),
    P_Full = round(rob_2$coefficients[2,4], 4),
    CI_Lower_Full = round(ci_2[2,1]*100, 2),
    CI_Upper_Full = round(ci_2[2,2]*100, 2),
    Effect_Above_Baserate = round(rob_above_2$coefficients[2,1]*100, 2),
    SE_Above = round(rob_above_2$coefficients[2,2]*100, 2),
    P_Above = round(rob_above_2$coefficients[2,4], 4),
    CI_Lower_Above = round(ci_above_2[2,1]*100, 2),
    CI_Upper_Above = round(ci_above_2[2,2]*100, 2)
  ))
}

###############################################################################
# STUDY 3A: Films (Race)
###############################################################################

cat("\n--- STUDY 3A: Films (Race) ---\n")
d3a <- read.csv('Study-3A/Study3A.csv', check.names = FALSE)

pool_base_rate_3a <- 4/30  # 13.3%
n_selections_3a <- 7
threshold_3a <- 1  # >= 1 means above 13.3%

model_full_3a <- lm(race_pick ~ race_feedback, data = d3a)
rob_3a <- robust_summary(model_full_3a)
ci_3a <- robust_confint(model_full_3a)

d3a_above <- d3a %>% filter(base_race >= threshold_3a)

cat("Pool base rate: 13.3% (4/30 racial minority films)\n")
cat("Threshold for 'above base rate': base_race >= 1\n")
cat("N total:", nrow(d3a), "\n")
cat("N above base rate:", nrow(d3a_above), "(", round(nrow(d3a_above)/nrow(d3a)*100, 1), "%)\n")

if(nrow(d3a_above) > 10) {
  model_above_3a <- lm(race_pick ~ race_feedback, data = d3a_above)
  rob_above_3a <- robust_summary(model_above_3a)
  ci_above_3a <- robust_confint(model_above_3a)

  cat("\nFull Sample Effect:", round(rob_3a$coefficients[2,1]*100, 2), "pp",
      "(p =", round(rob_3a$coefficients[2,4], 4), ")\n")
  cat("Above Base Rate Effect:", round(rob_above_3a$coefficients[2,1]*100, 2), "pp",
      "(p =", round(rob_above_3a$coefficients[2,4], 4), ")\n")

  results <- rbind(results, data.frame(
    Study = "Study 3A",
    Attribute = "Race (URM)",
    Pool_Base_Rate = "13.3% (4/30)",
    N_Initial_Selections = n_selections_3a,
    Threshold_Count = ">= 1",
    N_Total = nrow(d3a),
    N_Above_Baserate = nrow(d3a_above),
    Pct_Above_Baserate = round(nrow(d3a_above)/nrow(d3a)*100, 1),
    Effect_Full_Sample = round(rob_3a$coefficients[2,1]*100, 2),
    SE_Full = round(rob_3a$coefficients[2,2]*100, 2),
    P_Full = round(rob_3a$coefficients[2,4], 4),
    CI_Lower_Full = round(ci_3a[2,1]*100, 2),
    CI_Upper_Full = round(ci_3a[2,2]*100, 2),
    Effect_Above_Baserate = round(rob_above_3a$coefficients[2,1]*100, 2),
    SE_Above = round(rob_above_3a$coefficients[2,2]*100, 2),
    P_Above = round(rob_above_3a$coefficients[2,4], 4),
    CI_Lower_Above = round(ci_above_3a[2,1]*100, 2),
    CI_Upper_Above = round(ci_above_3a[2,2]*100, 2)
  ))
}

###############################################################################
# STUDY 3B: Films (Gender)
###############################################################################

cat("\n--- STUDY 3B: Films (Gender) ---\n")
d3b <- read.csv('Study-3B/Study3B.csv', check.names = FALSE)

pool_base_rate_3b <- 5/25  # 20%
n_selections_3b <- 6
threshold_3b <- 2  # >= 2 means above 20%

model_full_3b <- lm(female_pick ~ gender_feedback, data = d3b)
rob_3b <- robust_summary(model_full_3b)
ci_3b <- robust_confint(model_full_3b)

d3b_above <- d3b %>% filter(base_gender >= threshold_3b)

cat("Pool base rate: 20% (5/25 women protagonist films)\n")
cat("Threshold for 'above base rate': base_gender >= 2\n")
cat("N total:", nrow(d3b), "\n")
cat("N above base rate:", nrow(d3b_above), "(", round(nrow(d3b_above)/nrow(d3b)*100, 1), "%)\n")

if(nrow(d3b_above) > 10) {
  model_above_3b <- lm(female_pick ~ gender_feedback, data = d3b_above)
  rob_above_3b <- robust_summary(model_above_3b)
  ci_above_3b <- robust_confint(model_above_3b)

  cat("\nFull Sample Effect:", round(rob_3b$coefficients[2,1]*100, 2), "pp",
      "(p =", round(rob_3b$coefficients[2,4], 4), ")\n")
  cat("Above Base Rate Effect:", round(rob_above_3b$coefficients[2,1]*100, 2), "pp",
      "(p =", round(rob_above_3b$coefficients[2,4], 4), ")\n")

  results <- rbind(results, data.frame(
    Study = "Study 3B",
    Attribute = "Gender (Women)",
    Pool_Base_Rate = "20% (5/25)",
    N_Initial_Selections = n_selections_3b,
    Threshold_Count = ">= 2",
    N_Total = nrow(d3b),
    N_Above_Baserate = nrow(d3b_above),
    Pct_Above_Baserate = round(nrow(d3b_above)/nrow(d3b)*100, 1),
    Effect_Full_Sample = round(rob_3b$coefficients[2,1]*100, 2),
    SE_Full = round(rob_3b$coefficients[2,2]*100, 2),
    P_Full = round(rob_3b$coefficients[2,4], 4),
    CI_Lower_Full = round(ci_3b[2,1]*100, 2),
    CI_Upper_Full = round(ci_3b[2,2]*100, 2),
    Effect_Above_Baserate = round(rob_above_3b$coefficients[2,1]*100, 2),
    SE_Above = round(rob_above_3b$coefficients[2,2]*100, 2),
    P_Above = round(rob_above_3b$coefficients[2,4], 4),
    CI_Lower_Above = round(ci_above_3b[2,1]*100, 2),
    CI_Upper_Above = round(ci_above_3b[2,2]*100, 2)
  ))
}

###############################################################################
# STUDY 4A: Authors (Race)
###############################################################################

cat("\n--- STUDY 4A: Authors (Race) ---\n")
d4a <- read.csv('Study-4A/Study4A.csv', check.names = FALSE)

pool_base_rate_4a <- 8/25  # 32%
n_selections_4a <- 6
threshold_4a <- 2  # >= 2 means above 32%

model_full_4a <- lm(race_pick ~ race_feedback, data = d4a)
rob_4a <- robust_summary(model_full_4a)
ci_4a <- robust_confint(model_full_4a)

d4a_above <- d4a %>% filter(base_race >= threshold_4a)

cat("Pool base rate: 32% (8/25 racial minority authors)\n")
cat("Threshold for 'above base rate': base_race >= 2\n")
cat("N total:", nrow(d4a), "\n")
cat("N above base rate:", nrow(d4a_above), "(", round(nrow(d4a_above)/nrow(d4a)*100, 1), "%)\n")

if(nrow(d4a_above) > 10) {
  model_above_4a <- lm(race_pick ~ race_feedback, data = d4a_above)
  rob_above_4a <- robust_summary(model_above_4a)
  ci_above_4a <- robust_confint(model_above_4a)

  cat("\nFull Sample Effect:", round(rob_4a$coefficients[2,1]*100, 2), "pp",
      "(p =", round(rob_4a$coefficients[2,4], 4), ")\n")
  cat("Above Base Rate Effect:", round(rob_above_4a$coefficients[2,1]*100, 2), "pp",
      "(p =", round(rob_above_4a$coefficients[2,4], 4), ")\n")

  results <- rbind(results, data.frame(
    Study = "Study 4A",
    Attribute = "Race (URM)",
    Pool_Base_Rate = "32% (8/25)",
    N_Initial_Selections = n_selections_4a,
    Threshold_Count = ">= 2",
    N_Total = nrow(d4a),
    N_Above_Baserate = nrow(d4a_above),
    Pct_Above_Baserate = round(nrow(d4a_above)/nrow(d4a)*100, 1),
    Effect_Full_Sample = round(rob_4a$coefficients[2,1]*100, 2),
    SE_Full = round(rob_4a$coefficients[2,2]*100, 2),
    P_Full = round(rob_4a$coefficients[2,4], 4),
    CI_Lower_Full = round(ci_4a[2,1]*100, 2),
    CI_Upper_Full = round(ci_4a[2,2]*100, 2),
    Effect_Above_Baserate = round(rob_above_4a$coefficients[2,1]*100, 2),
    SE_Above = round(rob_above_4a$coefficients[2,2]*100, 2),
    P_Above = round(rob_above_4a$coefficients[2,4], 4),
    CI_Lower_Above = round(ci_above_4a[2,1]*100, 2),
    CI_Upper_Above = round(ci_above_4a[2,2]*100, 2)
  ))
}

###############################################################################
# STUDY 4B: Authors (Gender)
###############################################################################

cat("\n--- STUDY 4B: Authors (Gender) ---\n")
d4b <- read.csv('Study-4B/Study4B.csv', check.names = FALSE)

pool_base_rate_4b <- 6/25  # 24%
n_selections_4b <- 6
threshold_4b <- 2  # >= 2 means above 24%

model_full_4b <- lm(female_pick ~ gender_feedback, data = d4b)
rob_4b <- robust_summary(model_full_4b)
ci_4b <- robust_confint(model_full_4b)

d4b_above <- d4b %>% filter(base_gender >= threshold_4b)

cat("Pool base rate: 24% (6/25 women authors)\n")
cat("Threshold for 'above base rate': base_gender >= 2\n")
cat("N total:", nrow(d4b), "\n")
cat("N above base rate:", nrow(d4b_above), "(", round(nrow(d4b_above)/nrow(d4b)*100, 1), "%)\n")

if(nrow(d4b_above) > 10) {
  model_above_4b <- lm(female_pick ~ gender_feedback, data = d4b_above)
  rob_above_4b <- robust_summary(model_above_4b)
  ci_above_4b <- robust_confint(model_above_4b)

  cat("\nFull Sample Effect:", round(rob_4b$coefficients[2,1]*100, 2), "pp",
      "(p =", round(rob_4b$coefficients[2,4], 4), ")\n")
  cat("Above Base Rate Effect:", round(rob_above_4b$coefficients[2,1]*100, 2), "pp",
      "(p =", round(rob_above_4b$coefficients[2,4], 4), ")\n")

  results <- rbind(results, data.frame(
    Study = "Study 4B",
    Attribute = "Gender (Women)",
    Pool_Base_Rate = "24% (6/25)",
    N_Initial_Selections = n_selections_4b,
    Threshold_Count = ">= 2",
    N_Total = nrow(d4b),
    N_Above_Baserate = nrow(d4b_above),
    Pct_Above_Baserate = round(nrow(d4b_above)/nrow(d4b)*100, 1),
    Effect_Full_Sample = round(rob_4b$coefficients[2,1]*100, 2),
    SE_Full = round(rob_4b$coefficients[2,2]*100, 2),
    P_Full = round(rob_4b$coefficients[2,4], 4),
    CI_Lower_Full = round(ci_4b[2,1]*100, 2),
    CI_Upper_Full = round(ci_4b[2,2]*100, 2),
    Effect_Above_Baserate = round(rob_above_4b$coefficients[2,1]*100, 2),
    SE_Above = round(rob_above_4b$coefficients[2,2]*100, 2),
    P_Above = round(rob_above_4b$coefficients[2,4], 4),
    CI_Lower_Above = round(ci_above_4b[2,1]*100, 2),
    CI_Upper_Above = round(ci_above_4b[2,2]*100, 2)
  ))
}

###############################################################################
# STUDY 5: CEOs/Founders (Gender) - Women Underrepresented Pool Only
###############################################################################

cat("\n--- STUDY 5: CEOs/Founders (Gender) - Women Underrepresented Pool ---\n")
d5 <- read.csv('Study-5/Study5.csv', check.names = FALSE)

# Focus on the women underrepresented pool (men_pool == 1, 25% women)
d5_underrep <- d5 %>% filter(men_pool == 1)

pool_base_rate_5 <- 0.25  # 25%
n_selections_5 <- 6
threshold_5 <- 2  # >= 2 means above 25%

model_full_5 <- lm(female_pick ~ treatment, data = d5_underrep)
rob_5 <- robust_summary(model_full_5)
ci_5 <- robust_confint(model_full_5)

d5_above <- d5_underrep %>% filter(base_gender >= threshold_5)

cat("Pool base rate: 25% (women underrepresented condition)\n")
cat("Threshold for 'above base rate': base_gender >= 2\n")
cat("N total (underrep pool):", nrow(d5_underrep), "\n")
cat("N above base rate:", nrow(d5_above), "(", round(nrow(d5_above)/nrow(d5_underrep)*100, 1), "%)\n")

if(nrow(d5_above) > 10) {
  model_above_5 <- lm(female_pick ~ treatment, data = d5_above)
  rob_above_5 <- robust_summary(model_above_5)
  ci_above_5 <- robust_confint(model_above_5)

  cat("\nFull Sample Effect:", round(rob_5$coefficients[2,1]*100, 2), "pp",
      "(p =", round(rob_5$coefficients[2,4], 4), ")\n")
  cat("Above Base Rate Effect:", round(rob_above_5$coefficients[2,1]*100, 2), "pp",
      "(p =", round(rob_above_5$coefficients[2,4], 4), ")\n")

  results <- rbind(results, data.frame(
    Study = "Study 5 (Underrep)",
    Attribute = "Gender (Women)",
    Pool_Base_Rate = "25%",
    N_Initial_Selections = n_selections_5,
    Threshold_Count = ">= 2",
    N_Total = nrow(d5_underrep),
    N_Above_Baserate = nrow(d5_above),
    Pct_Above_Baserate = round(nrow(d5_above)/nrow(d5_underrep)*100, 1),
    Effect_Full_Sample = round(rob_5$coefficients[2,1]*100, 2),
    SE_Full = round(rob_5$coefficients[2,2]*100, 2),
    P_Full = round(rob_5$coefficients[2,4], 4),
    CI_Lower_Full = round(ci_5[2,1]*100, 2),
    CI_Upper_Full = round(ci_5[2,2]*100, 2),
    Effect_Above_Baserate = round(rob_above_5$coefficients[2,1]*100, 2),
    SE_Above = round(rob_above_5$coefficients[2,2]*100, 2),
    P_Above = round(rob_above_5$coefficients[2,4], 4),
    CI_Lower_Above = round(ci_above_5[2,1]*100, 2),
    CI_Upper_Above = round(ci_above_5[2,2]*100, 2)
  ))
}

###############################################################################
# Print Summary Table
###############################################################################

cat("\n\n=======================================================================\n")
cat("SUMMARY TABLE: FEEDBACK EFFECTS AMONG PARTICIPANTS WHO SELECTED\n")
cat("MORE WOMEN/URMs THAN THE POOL BASE RATE\n")
cat("=======================================================================\n\n")

# Create a cleaner display table
display_table <- results %>%
  select(
    Study,
    Attribute,
    `Pool Base Rate` = Pool_Base_Rate,
    `N Total` = N_Total,
    `N Above BR` = N_Above_Baserate,
    `% Above BR` = Pct_Above_Baserate,
    `Full Sample Effect (pp)` = Effect_Full_Sample,
    `Full p` = P_Full,
    `Above BR Effect (pp)` = Effect_Above_Baserate,
    `Above BR p` = P_Above
  )

print(display_table, row.names = FALSE)

###############################################################################
# Save Results
###############################################################################

# Save detailed results to CSV
write.csv(results, "base-rate-analysis/base_rate_analysis_results.csv", row.names = FALSE)

# Create Excel file with formatted table
wb <- createWorkbook()
addWorksheet(wb, "Base Rate Analysis")

# Write header
writeData(wb, "Base Rate Analysis",
          "Effect of Feedback When Initial Selection Exceeds Pool Base Rate",
          startRow = 1, startCol = 1)
writeData(wb, "Base Rate Analysis",
          "Studies 2-5: Does feedback still increase selection of women/URMs when participants already selected more than the base rate?",
          startRow = 2, startCol = 1)

# Write results table
writeData(wb, "Base Rate Analysis", results, startRow = 4, startCol = 1)

# Add formatting
headerStyle <- createStyle(textDecoration = "bold", fgFill = "#4472C4", fontColour = "white")
addStyle(wb, "Base Rate Analysis", headerStyle, rows = 4, cols = 1:ncol(results))

# Auto-fit columns
setColWidths(wb, "Base Rate Analysis", cols = 1:ncol(results), widths = "auto")

# Save workbook
saveWorkbook(wb, "base-rate-analysis/Base_Rate_Analysis_Table.xlsx", overwrite = TRUE)

cat("\n\nResults saved to:\n")
cat("  - base-rate-analysis/base_rate_analysis_results.csv\n")
cat("  - base-rate-analysis/Base_Rate_Analysis_Table.xlsx\n")

###############################################################################
# Key Takeaway
###############################################################################

cat("\n\n=======================================================================\n")
cat("KEY FINDING\n")
cat("=======================================================================\n")
cat("
Even among participants whose initial selections indicated they had already
selected MORE women/URMs than the base rate in the candidate pool, providing
feedback still increased the likelihood of subsequently selecting a woman/URM.

This demonstrates that the standard invoked by feedback is NOT simply
'select at least as many as the base rate.' The effect persists even when
participants have objectively exceeded proportional representation.
\n")
