# Reproduce Table 2: OLS Regressions Predicting Attributes of Final Film
# Study 1A (Study-2B in codebase)

# Load libraries
library(dplyr)
library(lmtest)
library(sandwich)

# Read data
d0 <- read.csv('Study2B.csv', check.names = FALSE)

# Function for robust standard errors
robust_summary <- function(model) {
    # Calculating robust standard errors (HC3)
    robust_se <- sqrt(diag(vcovHC(model, type = "HC3")))

    # Getting original model summary
    model_summary <- summary(model)

    # Updating standard errors, t-values, and p-values in the coefficients table
    model_summary$coefficients[, "Std. Error"] <- robust_se
    model_summary$coefficients[, "t value"] <- model_summary$coefficients[, "Estimate"] / robust_se
    model_summary$coefficients[, "Pr(>|t|)"] <- 2 * pt(-abs(model_summary$coefficients[, "t value"]),
                                                        df = model_summary$df[2], lower.tail = TRUE)

    return(model_summary)
}

# Function to extract coefficient info with robust SEs
extract_coef_info <- function(model, var_name) {
  rob_sum <- robust_summary(model)

  # Find the row for the variable
  coef_row <- which(rownames(rob_sum$coefficients) == var_name)

  if(length(coef_row) == 0) {
    return(list(estimate = NA, se = NA, pvalue = NA))
  }

  estimate <- rob_sum$coefficients[coef_row, "Estimate"]
  se <- rob_sum$coefficients[coef_row, "Std. Error"]
  pvalue <- rob_sum$coefficients[coef_row, "Pr(>|t|)"]

  return(list(estimate = estimate, se = se, pvalue = pvalue))
}

# Function to format coefficient with stars
format_coef <- function(est, se, pvalue) {
  # Add significance stars
  stars <- ""
  if (pvalue < 0.001) stars <- "***"
  else if (pvalue < 0.01) stars <- "**"
  else if (pvalue < 0.05) stars <- "*"

  # Format estimate with stars
  est_str <- sprintf("%.3f%s", est, stars)
  # Format SE in parentheses
  se_str <- sprintf("(%.3f)", se)

  return(c(est_str, se_str))
}

cat("\n=================================================================\n")
cat("TABLE 2: OLS REGRESSIONS PREDICTING ATTRIBUTES OF FINAL FILM\n")
cat("Study 1A (Study-2B): Film Selection Study\n")
cat("=================================================================\n\n")

# ========================
# MODELS 1-4: Independent Regression Equations
# ========================

cat("INDEPENDENT REGRESSION EQUATIONS (Models 1-4)\n")
cat("-----------------------------------------------------------------\n\n")

# Model 1: Featured a Woman Protagonist ~ Feedback on Women
cat("Model 1: Featured a Woman Protagonist\n")
cat("DV: female_pick | IV: gender_feedback\n")
m1 <- lm(female_pick ~ gender_feedback, data = d0)
rob_sum_m1 <- robust_summary(m1)
print(rob_sum_m1)
m1_coef <- extract_coef_info(m1, "gender_feedback")
cat(sprintf("\nCoefficient: %.3f (%.3f), p = %.4f\n",
            m1_coef$estimate, m1_coef$se, m1_coef$pvalue))
cat(sprintf("R-squared: %.3f\n", summary(m1)$r.squared))
cat(sprintf("Observations: %d\n\n", nobs(m1)))

# Model 2: Had a Budget Above $40M ~ Feedback on Budget
cat("Model 2: Had a Budget Above $40M\n")
cat("DV: budget_pick | IV: budget_shown\n")
m2 <- lm(budget_pick ~ budget_shown, data = d0)
rob_sum_m2 <- robust_summary(m2)
print(rob_sum_m2)
m2_coef <- extract_coef_info(m2, "budget_shown")
cat(sprintf("\nCoefficient: %.3f (%.3f), p = %.4f\n",
            m2_coef$estimate, m2_coef$se, m2_coef$pvalue))
cat(sprintf("R-squared: %.3f\n", summary(m2)$r.squared))
cat(sprintf("Observations: %d\n\n", nobs(m2)))

# Model 3: Was about a Political Leader ~ Feedback on Political Leader
cat("Model 3: Was about a Political Leader\n")
cat("DV: poli_pick | IV: poli_shown\n")
m3 <- lm(poli_pick ~ poli_shown, data = d0)
rob_sum_m3 <- robust_summary(m3)
print(rob_sum_m3)
m3_coef <- extract_coef_info(m3, "poli_shown")
cat(sprintf("\nCoefficient: %.3f (%.3f), p = %.4f\n",
            m3_coef$estimate, m3_coef$se, m3_coef$pvalue))
cat(sprintf("R-squared: %.3f\n", summary(m3)$r.squared))
cat(sprintf("Observations: %d\n\n", nobs(m3)))

# Model 4: Was Released After 2010 ~ Feedback on Release Year
cat("Model 4: Was Released After 2010\n")
cat("DV: year_pick | IV: year_shown\n")
m4 <- lm(year_pick ~ year_shown, data = d0)
rob_sum_m4 <- robust_summary(m4)
print(rob_sum_m4)
m4_coef <- extract_coef_info(m4, "year_shown")
cat(sprintf("\nCoefficient: %.3f (%.3f), p = %.4f\n",
            m4_coef$estimate, m4_coef$se, m4_coef$pvalue))
cat(sprintf("R-squared: %.3f\n", summary(m4)$r.squared))
cat(sprintf("Observations: %d\n\n", nobs(m4)))

# ========================
# MODELS 5-8: Simultaneous System of Equations
# ========================

cat("\n=================================================================\n")
cat("SIMULTANEOUS SYSTEM OF EQUATIONS (Models 5-8)\n")
cat("All models include all 4 feedback types as predictors\n")
cat("Constant suppressed to prevent collinearity\n")
cat("-----------------------------------------------------------------\n\n")

# Model 5: Featured a Woman Protagonist ~ All Feedback (no intercept)
cat("Model 5: Featured a Woman Protagonist\n")
cat("DV: female_pick | IVs: gender_feedback + budget_shown + year_shown + poli_shown\n")
m5 <- lm(female_pick ~ gender_feedback + budget_shown + year_shown + poli_shown - 1, data = d0)
rob_sum_m5 <- robust_summary(m5)
print(rob_sum_m5)
m5_gender <- extract_coef_info(m5, "gender_feedback")
m5_budget <- extract_coef_info(m5, "budget_shown")
m5_year <- extract_coef_info(m5, "year_shown")
m5_poli <- extract_coef_info(m5, "poli_shown")
cat(sprintf("R-squared: %.3f\n", summary(m5)$r.squared))
cat(sprintf("Observations: %d\n\n", nobs(m5)))

# Model 6: Had a Budget Above $40M ~ All Feedback (no intercept)
cat("Model 6: Had a Budget Above $40M\n")
cat("DV: budget_pick | IVs: gender_feedback + budget_shown + year_shown + poli_shown\n")
m6 <- lm(budget_pick ~ gender_feedback + budget_shown + year_shown + poli_shown - 1, data = d0)
rob_sum_m6 <- robust_summary(m6)
print(rob_sum_m6)
m6_gender <- extract_coef_info(m6, "gender_feedback")
m6_budget <- extract_coef_info(m6, "budget_shown")
m6_year <- extract_coef_info(m6, "year_shown")
m6_poli <- extract_coef_info(m6, "poli_shown")
cat(sprintf("R-squared: %.3f\n", summary(m6)$r.squared))
cat(sprintf("Observations: %d\n\n", nobs(m6)))

# Model 7: Was about a Political Leader ~ All Feedback (no intercept)
cat("Model 7: Was about a Political Leader\n")
cat("DV: poli_pick | IVs: gender_feedback + budget_shown + year_shown + poli_shown\n")
m7 <- lm(poli_pick ~ gender_feedback + budget_shown + year_shown + poli_shown - 1, data = d0)
rob_sum_m7 <- robust_summary(m7)
print(rob_sum_m7)
m7_gender <- extract_coef_info(m7, "gender_feedback")
m7_budget <- extract_coef_info(m7, "budget_shown")
m7_year <- extract_coef_info(m7, "year_shown")
m7_poli <- extract_coef_info(m7, "poli_shown")
cat(sprintf("R-squared: %.3f\n", summary(m7)$r.squared))
cat(sprintf("Observations: %d\n\n", nobs(m7)))

# Model 8: Was Released After 2010 ~ All Feedback (no intercept)
cat("Model 8: Was Released After 2010\n")
cat("DV: year_pick | IVs: gender_feedback + budget_shown + year_shown + poli_shown\n")
m8 <- lm(year_pick ~ gender_feedback + budget_shown + year_shown + poli_shown - 1, data = d0)
rob_sum_m8 <- robust_summary(m8)
print(rob_sum_m8)
m8_gender <- extract_coef_info(m8, "gender_feedback")
m8_budget <- extract_coef_info(m8, "budget_shown")
m8_year <- extract_coef_info(m8, "year_shown")
m8_poli <- extract_coef_info(m8, "poli_shown")
cat(sprintf("R-squared: %.3f\n", summary(m8)$r.squared))
cat(sprintf("Observations: %d\n\n", nobs(m8)))

# ========================
# CREATE FORMATTED TABLE
# ========================

cat("\n=================================================================\n")
cat("FORMATTED TABLE 2\n")
cat("=================================================================\n\n")

# Create the table data structure
table_data <- data.frame(
  Variable = c(
    "About a Woman Protagonist",
    "",
    "Above a Budget of $40M",
    "",
    "About a Political Leader",
    "",
    "Released After 2010",
    "",
    "Observations",
    "R-Squared"
  ),
  Model1 = c(
    format_coef(m1_coef$estimate, m1_coef$se, m1_coef$pvalue)[1],
    format_coef(m1_coef$estimate, m1_coef$se, m1_coef$pvalue)[2],
    "", "", "", "", "", "",
    nobs(m1),
    sprintf("%.3f", summary(m1)$r.squared)
  ),
  Model2 = c(
    "", "",
    format_coef(m2_coef$estimate, m2_coef$se, m2_coef$pvalue)[1],
    format_coef(m2_coef$estimate, m2_coef$se, m2_coef$pvalue)[2],
    "", "", "", "",
    nobs(m2),
    sprintf("%.3f", summary(m2)$r.squared)
  ),
  Model3 = c(
    "", "", "", "",
    format_coef(m3_coef$estimate, m3_coef$se, m3_coef$pvalue)[1],
    format_coef(m3_coef$estimate, m3_coef$se, m3_coef$pvalue)[2],
    "", "",
    nobs(m3),
    sprintf("%.3f", summary(m3)$r.squared)
  ),
  Model4 = c(
    "", "", "", "", "", "",
    format_coef(m4_coef$estimate, m4_coef$se, m4_coef$pvalue)[1],
    format_coef(m4_coef$estimate, m4_coef$se, m4_coef$pvalue)[2],
    nobs(m4),
    sprintf("%.3f", summary(m4)$r.squared)
  ),
  Model5 = c(
    format_coef(m5_gender$estimate, m5_gender$se, m5_gender$pvalue)[1],
    format_coef(m5_gender$estimate, m5_gender$se, m5_gender$pvalue)[2],
    format_coef(m5_budget$estimate, m5_budget$se, m5_budget$pvalue)[1],
    format_coef(m5_budget$estimate, m5_budget$se, m5_budget$pvalue)[2],
    format_coef(m5_year$estimate, m5_year$se, m5_year$pvalue)[1],
    format_coef(m5_year$estimate, m5_year$se, m5_year$pvalue)[2],
    format_coef(m5_poli$estimate, m5_poli$se, m5_poli$pvalue)[1],
    format_coef(m5_poli$estimate, m5_poli$se, m5_poli$pvalue)[2],
    nobs(m5),
    sprintf("%.3f", summary(m5)$r.squared)
  ),
  Model6 = c(
    format_coef(m6_gender$estimate, m6_gender$se, m6_gender$pvalue)[1],
    format_coef(m6_gender$estimate, m6_gender$se, m6_gender$pvalue)[2],
    format_coef(m6_budget$estimate, m6_budget$se, m6_budget$pvalue)[1],
    format_coef(m6_budget$estimate, m6_budget$se, m6_budget$pvalue)[2],
    format_coef(m6_year$estimate, m6_year$se, m6_year$pvalue)[1],
    format_coef(m6_year$estimate, m6_year$se, m6_year$pvalue)[2],
    format_coef(m6_poli$estimate, m6_poli$se, m6_poli$pvalue)[1],
    format_coef(m6_poli$estimate, m6_poli$se, m6_poli$pvalue)[2],
    nobs(m6),
    sprintf("%.3f", summary(m6)$r.squared)
  ),
  Model7 = c(
    format_coef(m7_gender$estimate, m7_gender$se, m7_gender$pvalue)[1],
    format_coef(m7_gender$estimate, m7_gender$se, m7_gender$pvalue)[2],
    format_coef(m7_budget$estimate, m7_budget$se, m7_budget$pvalue)[1],
    format_coef(m7_budget$estimate, m7_budget$se, m7_budget$pvalue)[2],
    format_coef(m7_year$estimate, m7_year$se, m7_year$pvalue)[1],
    format_coef(m7_year$estimate, m7_year$se, m7_year$pvalue)[2],
    format_coef(m7_poli$estimate, m7_poli$se, m7_poli$pvalue)[1],
    format_coef(m7_poli$estimate, m7_poli$se, m7_poli$pvalue)[2],
    nobs(m7),
    sprintf("%.3f", summary(m7)$r.squared)
  ),
  Model8 = c(
    format_coef(m8_gender$estimate, m8_gender$se, m8_gender$pvalue)[1],
    format_coef(m8_gender$estimate, m8_gender$se, m8_gender$pvalue)[2],
    format_coef(m8_budget$estimate, m8_budget$se, m8_budget$pvalue)[1],
    format_coef(m8_budget$estimate, m8_budget$se, m8_budget$pvalue)[2],
    format_coef(m8_year$estimate, m8_year$se, m8_year$pvalue)[1],
    format_coef(m8_year$estimate, m8_year$se, m8_year$pvalue)[2],
    format_coef(m8_poli$estimate, m8_poli$se, m8_poli$pvalue)[1],
    format_coef(m8_poli$estimate, m8_poli$se, m8_poli$pvalue)[2],
    nobs(m8),
    sprintf("%.3f", summary(m8)$r.squared)
  )
)

# Print the table
colnames(table_data) <- c("Indicator for Whether Feedback was Provided on % of Original Films That Were...",
                          "Model 1", "Model 2", "Model 3", "Model 4",
                          "Model 5", "Model 6", "Model 7", "Model 8")
print(table_data, row.names = FALSE)

cat("\n\nDependent Variables:\n")
cat("Model 1 & 5: Featured a Woman Protagonist\n")
cat("Model 2 & 6: Had a Budget Above $40M\n")
cat("Model 3 & 7: Was about a Political Leader\n")
cat("Model 4 & 8: Was Released After 2010\n")
cat("\nModels 1-4: Independent Regression Equations\n")
cat("Models 5-8: Simultaneous System of Equations (constant suppressed)\n")
cat("\nStandard errors reported in parentheses are estimated robustly using HC3.\n")
cat("*p<0.1; **p<0.05; ***p<0.01; ****p<0.001\n")

cat("\n=================================================================\n")
cat("TABLE 2 REPRODUCTION COMPLETE\n")
cat("=================================================================\n")
