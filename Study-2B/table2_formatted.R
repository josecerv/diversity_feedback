# Reproduce Table 2: OLS Regressions Predicting Attributes of Final Film
# Study 1A (Study-2B in codebase) - Formatted Output
# With proper 4-level significance stars matching original table

# Load libraries
library(dplyr)
library(lmtest)
library(sandwich)

# Read data
d0 <- read.csv('Study2B.csv', check.names = FALSE)

# Function for robust standard errors (HC3)
robust_summary <- function(model) {
    robust_se <- sqrt(diag(vcovHC(model, type = "HC3")))
    model_summary <- summary(model)
    model_summary$coefficients[, "Std. Error"] <- robust_se
    model_summary$coefficients[, "t value"] <- model_summary$coefficients[, "Estimate"] / robust_se
    model_summary$coefficients[, "Pr(>|t|)"] <- 2 * pt(-abs(model_summary$coefficients[, "t value"]),
                                                        df = model_summary$df[2], lower.tail = TRUE)
    return(model_summary)
}

# Function to extract coefficient info with robust SEs
extract_coef_info <- function(model, var_name) {
  rob_sum <- robust_summary(model)
  coef_row <- which(rownames(rob_sum$coefficients) == var_name)
  if(length(coef_row) == 0) {
    return(list(estimate = NA, se = NA, pvalue = NA))
  }
  estimate <- rob_sum$coefficients[coef_row, "Estimate"]
  se <- rob_sum$coefficients[coef_row, "Std. Error"]
  pvalue <- rob_sum$coefficients[coef_row, "Pr(>|t|)"]
  return(list(estimate = estimate, se = se, pvalue = pvalue))
}

# Function to format coefficient with significance symbols
format_coef <- function(est, se, pvalue) {
  # Add significance symbols
  # + for marginal (p<0.1); * p<0.05; ** p<0.01; *** p<0.001
  stars <- ""
  if (!is.na(pvalue)) {
    if (pvalue < 0.001) stars <- "***"
    else if (pvalue < 0.01) stars <- "**"
    else if (pvalue < 0.05) stars <- "*"
    else if (pvalue < 0.1) stars <- "+"
  }

  # Format estimate with stars
  if (!is.na(est)) {
    est_str <- sprintf("%.3f%s", est, stars)
  } else {
    est_str <- ""
  }

  # Format SE in parentheses
  if (!is.na(se)) {
    se_str <- sprintf("(%.3f)", se)
  } else {
    se_str <- ""
  }

  return(c(est_str, se_str))
}

# ========================
# RUN ALL MODELS
# ========================

# Models 1-4: Independent Regression Equations
m1 <- lm(female_pick ~ gender_feedback, data = d0)
m2 <- lm(budget_pick ~ budget_shown, data = d0)
m3 <- lm(poli_pick ~ poli_shown, data = d0)
m4 <- lm(year_pick ~ year_shown, data = d0)

# Models 5-8: Simultaneous System of Equations (no intercept)
m5 <- lm(female_pick ~ gender_feedback + budget_shown + year_shown + poli_shown - 1, data = d0)
m6 <- lm(budget_pick ~ gender_feedback + budget_shown + year_shown + poli_shown - 1, data = d0)
m7 <- lm(poli_pick ~ gender_feedback + budget_shown + year_shown + poli_shown - 1, data = d0)
m8 <- lm(year_pick ~ gender_feedback + budget_shown + year_shown + poli_shown - 1, data = d0)

# Extract coefficients
m1_coef <- extract_coef_info(m1, "gender_feedback")
m2_coef <- extract_coef_info(m2, "budget_shown")
m3_coef <- extract_coef_info(m3, "poli_shown")
m4_coef <- extract_coef_info(m4, "year_shown")

m5_gender <- extract_coef_info(m5, "gender_feedback")
m5_budget <- extract_coef_info(m5, "budget_shown")
m5_year <- extract_coef_info(m5, "year_shown")
m5_poli <- extract_coef_info(m5, "poli_shown")

m6_gender <- extract_coef_info(m6, "gender_feedback")
m6_budget <- extract_coef_info(m6, "budget_shown")
m6_year <- extract_coef_info(m6, "year_shown")
m6_poli <- extract_coef_info(m6, "poli_shown")

m7_gender <- extract_coef_info(m7, "gender_feedback")
m7_budget <- extract_coef_info(m7, "budget_shown")
m7_year <- extract_coef_info(m7, "year_shown")
m7_poli <- extract_coef_info(m7, "poli_shown")

m8_gender <- extract_coef_info(m8, "gender_feedback")
m8_budget <- extract_coef_info(m8, "budget_shown")
m8_year <- extract_coef_info(m8, "year_shown")
m8_poli <- extract_coef_info(m8, "poli_shown")

# ========================
# CREATE FORMATTED TABLE
# ========================

cat("\n")
cat("================================================================================\n")
cat("Table 2. Ordinary Least Squares (OLS) Regressions Predicting Attributes of the\n")
cat("Final Film Selected in Study 1A as a Function of the Pieces of Feedback\n")
cat("Participants Were Randomly Assigned to Receive on Their Initially Selected Set\n")
cat("of Six Films\n")
cat("================================================================================\n\n")

# Create header
cat(sprintf("%-50s", ""))
cat("Independent Regression Equations")
cat(sprintf("%24s", ""))
cat("Simultaneous System of Equations\n")

cat(sprintf("%-50s", ""))
cat("--------------------------------")
cat(sprintf("%4s", ""))
cat("----------------------------------------\n")

# Column headers
cat(sprintf("%-50s", "Dependent Variable:"))
cat(sprintf("%8s %8s %8s %8s", "Model 1", "Model 2", "Model 3", "Model 4"))
cat("    ")
cat(sprintf("%8s %8s %8s %8s\n", "Model 5", "Model 6", "Model 7", "Model 8"))

cat(sprintf("%-50s", "The Final Film Selected..."))
cat(sprintf("%8s %8s %8s %8s", "Featured", "Had a", "Was", "Was"))
cat("    ")
cat(sprintf("%8s %8s %8s %8s\n", "Featured", "Had a", "Was", "Was"))

cat(sprintf("%-50s", ""))
cat(sprintf("%8s %8s %8s %8s", "a Woman", "Budget", "about a", "Released"))
cat("    ")
cat(sprintf("%8s %8s %8s %8s\n", "a Woman", "Budget", "about a", "Released"))

cat(sprintf("%-50s", ""))
cat(sprintf("%8s %8s %8s %8s", "Protag.", "Above", "Political", "After"))
cat("    ")
cat(sprintf("%8s %8s %8s %8s\n", "Protag.", "Above", "Political", "After"))

cat(sprintf("%-50s", ""))
cat(sprintf("%8s %8s %8s %8s", "", "$40M", "Leader", "2010"))
cat("    ")
cat(sprintf("%8s %8s %8s %8s\n", "", "$40M", "Leader", "2010"))

cat("--------------------------------------------------------------------------------\n")
cat(sprintf("%-50s\n", "Indicator for Whether Feedback"))
cat(sprintf("%-50s\n", "was Provided on % of Original"))
cat(sprintf("%-50s\n", "Films That Were..."))
cat("\n")

# Row 1: About a Woman Protagonist
m1_fmt <- format_coef(m1_coef$estimate, m1_coef$se, m1_coef$pvalue)
m5_fmt <- format_coef(m5_gender$estimate, m5_gender$se, m5_gender$pvalue)
m6_fmt <- format_coef(m6_gender$estimate, m6_gender$se, m6_gender$pvalue)
m7_fmt <- format_coef(m7_gender$estimate, m7_gender$se, m7_gender$pvalue)
m8_fmt <- format_coef(m8_gender$estimate, m8_gender$se, m8_gender$pvalue)

cat(sprintf("  %-48s", "About a Woman Protagonist"))
cat(sprintf("%10s %8s %8s %8s", m1_fmt[1], "", "", ""))
cat("    ")
cat(sprintf("%10s %10s %10s %10s\n", m5_fmt[1], m6_fmt[1], m7_fmt[1], m8_fmt[1]))

cat(sprintf("  %-48s", ""))
cat(sprintf("%10s %8s %8s %8s", m1_fmt[2], "", "", ""))
cat("    ")
cat(sprintf("%10s %10s %10s %10s\n", m5_fmt[2], m6_fmt[2], m7_fmt[2], m8_fmt[2]))

cat("\n")

# Row 2: Above a Budget of $40M
m2_fmt <- format_coef(m2_coef$estimate, m2_coef$se, m2_coef$pvalue)
m5_b_fmt <- format_coef(m5_budget$estimate, m5_budget$se, m5_budget$pvalue)
m6_b_fmt <- format_coef(m6_budget$estimate, m6_budget$se, m6_budget$pvalue)
m7_b_fmt <- format_coef(m7_budget$estimate, m7_budget$se, m7_budget$pvalue)
m8_b_fmt <- format_coef(m8_budget$estimate, m8_budget$se, m8_budget$pvalue)

cat(sprintf("  %-48s", "Above a Budget of $40M"))
cat(sprintf("%10s %10s %8s %8s", "", m2_fmt[1], "", ""))
cat("    ")
cat(sprintf("%10s %10s %10s %10s\n", m5_b_fmt[1], m6_b_fmt[1], m7_b_fmt[1], m8_b_fmt[1]))

cat(sprintf("  %-48s", ""))
cat(sprintf("%10s %10s %8s %8s", "", m2_fmt[2], "", ""))
cat("    ")
cat(sprintf("%10s %10s %10s %10s\n", m5_b_fmt[2], m6_b_fmt[2], m7_b_fmt[2], m8_b_fmt[2]))

cat("\n")

# Row 3: About a Political Leader
m3_fmt <- format_coef(m3_coef$estimate, m3_coef$se, m3_coef$pvalue)
m5_p_fmt <- format_coef(m5_year$estimate, m5_year$se, m5_year$pvalue)
m6_p_fmt <- format_coef(m6_year$estimate, m6_year$se, m6_year$pvalue)
m7_p_fmt <- format_coef(m7_year$estimate, m7_year$se, m7_year$pvalue)
m8_p_fmt <- format_coef(m8_year$estimate, m8_year$se, m8_year$pvalue)

cat(sprintf("  %-48s", "About a Political Leader"))
cat(sprintf("%10s %10s %10s %8s", "", "", m3_fmt[1], ""))
cat("    ")
cat(sprintf("%10s %10s %10s %10s\n", m5_p_fmt[1], m6_p_fmt[1], m7_p_fmt[1], m8_p_fmt[1]))

cat(sprintf("  %-48s", ""))
cat(sprintf("%10s %10s %10s %8s", "", "", m3_fmt[2], ""))
cat("    ")
cat(sprintf("%10s %10s %10s %10s\n", m5_p_fmt[2], m6_p_fmt[2], m7_p_fmt[2], m8_p_fmt[2]))

cat("\n")

# Row 4: Released After 2010
m4_fmt <- format_coef(m4_coef$estimate, m4_coef$se, m4_coef$pvalue)
m5_y_fmt <- format_coef(m5_poli$estimate, m5_poli$se, m5_poli$pvalue)
m6_y_fmt <- format_coef(m6_poli$estimate, m6_poli$se, m6_poli$pvalue)
m7_y_fmt <- format_coef(m7_poli$estimate, m7_poli$se, m7_poli$pvalue)
m8_y_fmt <- format_coef(m8_poli$estimate, m8_poli$se, m8_poli$pvalue)

cat(sprintf("  %-48s", "Released After 2010"))
cat(sprintf("%10s %10s %10s %10s", "", "", "", m4_fmt[1]))
cat("    ")
cat(sprintf("%10s %10s %10s %10s\n", m5_y_fmt[1], m6_y_fmt[1], m7_y_fmt[1], m8_y_fmt[1]))

cat(sprintf("  %-48s", ""))
cat(sprintf("%10s %10s %10s %10s", "", "", "", m4_fmt[2]))
cat("    ")
cat(sprintf("%10s %10s %10s %10s\n", m5_y_fmt[2], m6_y_fmt[2], m7_y_fmt[2], m8_y_fmt[2]))

cat("\n")
cat("--------------------------------------------------------------------------------\n")

# Bottom rows: Observations and R-squared
cat(sprintf("  %-48s", "Observations"))
cat(sprintf("%10d %10d %10d %10d", nobs(m1), nobs(m2), nobs(m3), nobs(m4)))
cat("    ")
cat(sprintf("%10d %10d %10d %10d\n", nobs(m5), nobs(m6), nobs(m7), nobs(m8)))

cat(sprintf("  %-48s", "R-Squared"))
cat(sprintf("%10.3f %10.3f %10.3f %10.3f",
            summary(m1)$r.squared, summary(m2)$r.squared,
            summary(m3)$r.squared, summary(m4)$r.squared))
cat("    ")
cat(sprintf("%10.3f %10.3f %10.3f %10.3f\n",
            summary(m5)$r.squared, summary(m6)$r.squared,
            summary(m7)$r.squared, summary(m8)$r.squared))

cat("================================================================================\n\n")

# NOTES
cat("Notes:\n")
cat("This table reports the results of eight ordinary least squares (OLS) regressions\n")
cat("that predict whether the final film a participant selected in Study 1A had a given\n")
cat("attribute (e.g., Was the film released after 2010? Was the film about a political\n")
cat("leader?). Model 1 predicts the selection of a final film that featured a woman\n")
cat("protagonist with an indicator for receiving feedback on the % of the original six\n")
cat("films selected that were about a woman protagonist. Model 2 predicts the selection\n")
cat("of a final film that had a budget above $40M with an indicator for receiving\n")
cat("feedback on the % of the original six films selected with a budget above $40M.\n")
cat("Model 3 predicts the selection of a final film that was about a political leader\n")
cat("with an indicator for receiving feedback on the % of the original six films\n")
cat("selected that were about a political leader. Model 4 predicts the selection of a\n")
cat("final film that was released after 2010 with an indicator for receiving feedback\n")
cat("on the % of the original six films selected that were released after 2010.\n\n")

cat("Models 5-8 were used to construct a simultaneous system of equations. All of\n")
cat("these models include the following indicators as predictors: indicators for\n")
cat("receiving feedback on the % of the original six films selected that (1) featured\n")
cat("a woman protagonist, (2) had a budget above $40M, (3) featured a political leader\n")
cat("protagonist and (4) were released after 2010. Model 5 predicts the selection of\n")
cat("the final film that featured a woman protagonist. Model 6 predicts the selection\n")
cat("of the final film that had a budget above $40M. Model 7 predicts the selection of\n")
cat("a seventh film that was about a political leader. Model 8 predicts the selection\n")
cat("of a seventh film that was released after 2010. Models 5-8 had the constant\n")
cat("suppressed to prevent collinearity.\n\n")

cat("Standard errors reported in parentheses are estimated robustly using HC3 and are\n")
cat("clustered by participant in Models 5-8.\n\n")

cat("+p<0.1;*p<0.05;**p<0.01;***p<0.001.\n\n")

cat("================================================================================\n")
