# Table 2: OLS Regressions Predicting Attributes of NPR AI Experts Selected
# Study 1: NPR AI Expert Selection Study

# Load libraries
library(dplyr)
library(lmtest)
library(sandwich)

# Read data
d0 <- read.csv('Study1.csv', check.names = FALSE)

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
m1 <- lm(women_proportion ~ women_feedback, data = d0)
m2 <- lm(age_proportion ~ age_feedback, data = d0)
m3 <- lm(location_proportion ~ location_feedback, data = d0)
m4 <- lm(university_proportion ~ university_feedback, data = d0)

# Models 5-8: Simultaneous System of Equations (no intercept)
m5 <- lm(women_proportion ~ women_feedback + age_feedback + location_feedback + university_feedback - 1, data = d0)
m6 <- lm(age_proportion ~ women_feedback + age_feedback + location_feedback + university_feedback - 1, data = d0)
m7 <- lm(location_proportion ~ women_feedback + age_feedback + location_feedback + university_feedback - 1, data = d0)
m8 <- lm(university_proportion ~ women_feedback + age_feedback + location_feedback + university_feedback - 1, data = d0)

# Extract coefficients
m1_coef <- extract_coef_info(m1, "women_feedback")
m2_coef <- extract_coef_info(m2, "age_feedback")
m3_coef <- extract_coef_info(m3, "location_feedback")
m4_coef <- extract_coef_info(m4, "university_feedback")

m5_women <- extract_coef_info(m5, "women_feedback")
m5_age <- extract_coef_info(m5, "age_feedback")
m5_location <- extract_coef_info(m5, "location_feedback")
m5_university <- extract_coef_info(m5, "university_feedback")

m6_women <- extract_coef_info(m6, "women_feedback")
m6_age <- extract_coef_info(m6, "age_feedback")
m6_location <- extract_coef_info(m6, "location_feedback")
m6_university <- extract_coef_info(m6, "university_feedback")

m7_women <- extract_coef_info(m7, "women_feedback")
m7_age <- extract_coef_info(m7, "age_feedback")
m7_location <- extract_coef_info(m7, "location_feedback")
m7_university <- extract_coef_info(m7, "university_feedback")

m8_women <- extract_coef_info(m8, "women_feedback")
m8_age <- extract_coef_info(m8, "age_feedback")
m8_location <- extract_coef_info(m8, "location_feedback")
m8_university <- extract_coef_info(m8, "university_feedback")

# ========================
# CREATE FORMATTED TABLE
# ========================

cat("\n")
cat("================================================================================\n")
cat("Table 2. Ordinary Least Squares (OLS) Regressions Predicting Attributes of the\n")
cat("NPR AI Experts Selected in Study 1 as a Function of the Pieces of Feedback\n")
cat("Participants Were Randomly Assigned to Receive on Their Initially Selected Set\n")
cat("of Three Experts\n")
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

cat(sprintf("%-50s", "Proportion of Experts Selected..."))
cat(sprintf("%8s %8s %8s %8s", "Were", "Were", "Were", "Work at"))
cat("    ")
cat(sprintf("%8s %8s %8s %8s\n", "Were", "Were", "Were", "Work at"))

cat(sprintf("%-50s", ""))
cat(sprintf("%8s %8s %8s %8s", "Women", "Under", "Based on", "a"))
cat("    ")
cat(sprintf("%8s %8s %8s %8s\n", "Women", "Under", "Based on", "a"))

cat(sprintf("%-50s", ""))
cat(sprintf("%8s %8s %8s %8s", "", "50", "West", "Univer-"))
cat("    ")
cat(sprintf("%8s %8s %8s %8s\n", "", "50", "West", "Univer-"))

cat(sprintf("%-50s", ""))
cat(sprintf("%8s %8s %8s %8s", "", "", "Coast", "sity"))
cat("    ")
cat(sprintf("%8s %8s %8s %8s\n", "", "", "Coast", "sity"))

cat("--------------------------------------------------------------------------------\n")
cat(sprintf("%-50s\n", "Indicator for Whether Feedback"))
cat(sprintf("%-50s\n", "was Provided on % of Original"))
cat(sprintf("%-50s\n", "Experts That Were..."))
cat("\n")

# Row 1: Women
m1_fmt <- format_coef(m1_coef$estimate, m1_coef$se, m1_coef$pvalue)
m5_fmt <- format_coef(m5_women$estimate, m5_women$se, m5_women$pvalue)
m6_fmt <- format_coef(m6_women$estimate, m6_women$se, m6_women$pvalue)
m7_fmt <- format_coef(m7_women$estimate, m7_women$se, m7_women$pvalue)
m8_fmt <- format_coef(m8_women$estimate, m8_women$se, m8_women$pvalue)

cat(sprintf("  %-48s", "Women"))
cat(sprintf("%10s %8s %8s %8s", m1_fmt[1], "", "", ""))
cat("    ")
cat(sprintf("%10s %10s %10s %10s\n", m5_fmt[1], m6_fmt[1], m7_fmt[1], m8_fmt[1]))

cat(sprintf("  %-48s", ""))
cat(sprintf("%10s %8s %8s %8s", m1_fmt[2], "", "", ""))
cat("    ")
cat(sprintf("%10s %10s %10s %10s\n", m5_fmt[2], m6_fmt[2], m7_fmt[2], m8_fmt[2]))

cat("\n")

# Row 2: Under 50
m2_fmt <- format_coef(m2_coef$estimate, m2_coef$se, m2_coef$pvalue)
m5_a_fmt <- format_coef(m5_age$estimate, m5_age$se, m5_age$pvalue)
m6_a_fmt <- format_coef(m6_age$estimate, m6_age$se, m6_age$pvalue)
m7_a_fmt <- format_coef(m7_age$estimate, m7_age$se, m7_age$pvalue)
m8_a_fmt <- format_coef(m8_age$estimate, m8_age$se, m8_age$pvalue)

cat(sprintf("  %-48s", "Under 50 Years Old"))
cat(sprintf("%10s %10s %8s %8s", "", m2_fmt[1], "", ""))
cat("    ")
cat(sprintf("%10s %10s %10s %10s\n", m5_a_fmt[1], m6_a_fmt[1], m7_a_fmt[1], m8_a_fmt[1]))

cat(sprintf("  %-48s", ""))
cat(sprintf("%10s %10s %8s %8s", "", m2_fmt[2], "", ""))
cat("    ")
cat(sprintf("%10s %10s %10s %10s\n", m5_a_fmt[2], m6_a_fmt[2], m7_a_fmt[2], m8_a_fmt[2]))

cat("\n")

# Row 3: West Coast
m3_fmt <- format_coef(m3_coef$estimate, m3_coef$se, m3_coef$pvalue)
m5_l_fmt <- format_coef(m5_location$estimate, m5_location$se, m5_location$pvalue)
m6_l_fmt <- format_coef(m6_location$estimate, m6_location$se, m6_location$pvalue)
m7_l_fmt <- format_coef(m7_location$estimate, m7_location$se, m7_location$pvalue)
m8_l_fmt <- format_coef(m8_location$estimate, m8_location$se, m8_location$pvalue)

cat(sprintf("  %-48s", "Based on West Coast"))
cat(sprintf("%10s %10s %10s %8s", "", "", m3_fmt[1], ""))
cat("    ")
cat(sprintf("%10s %10s %10s %10s\n", m5_l_fmt[1], m6_l_fmt[1], m7_l_fmt[1], m8_l_fmt[1]))

cat(sprintf("  %-48s", ""))
cat(sprintf("%10s %10s %10s %8s", "", "", m3_fmt[2], ""))
cat("    ")
cat(sprintf("%10s %10s %10s %10s\n", m5_l_fmt[2], m6_l_fmt[2], m7_l_fmt[2], m8_l_fmt[2]))

cat("\n")

# Row 4: University
m4_fmt <- format_coef(m4_coef$estimate, m4_coef$se, m4_coef$pvalue)
m5_u_fmt <- format_coef(m5_university$estimate, m5_university$se, m5_university$pvalue)
m6_u_fmt <- format_coef(m6_university$estimate, m6_university$se, m6_university$pvalue)
m7_u_fmt <- format_coef(m7_university$estimate, m7_university$se, m7_university$pvalue)
m8_u_fmt <- format_coef(m8_university$estimate, m8_university$se, m8_university$pvalue)

cat(sprintf("  %-48s", "Working at a University"))
cat(sprintf("%10s %10s %10s %10s", "", "", "", m4_fmt[1]))
cat("    ")
cat(sprintf("%10s %10s %10s %10s\n", m5_u_fmt[1], m6_u_fmt[1], m7_u_fmt[1], m8_u_fmt[1]))

cat(sprintf("  %-48s", ""))
cat(sprintf("%10s %10s %10s %10s", "", "", "", m4_fmt[2]))
cat("    ")
cat(sprintf("%10s %10s %10s %10s\n", m5_u_fmt[2], m6_u_fmt[2], m7_u_fmt[2], m8_u_fmt[2]))

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
cat("that predict the proportion of NPR AI experts selected by participants in Study 1\n")
cat("that had a given attribute (e.g., Were women? Were under 50 years old?). Model 1\n")
cat("predicts the proportion of women selected with an indicator for receiving feedback\n")
cat("on the % of the original experts selected that were women. Model 2 predicts the\n")
cat("proportion of experts under 50 selected with an indicator for receiving feedback on\n")
cat("the % of the original experts selected that were under 50. Model 3 predicts the\n")
cat("proportion of experts based on the West Coast with an indicator for receiving\n")
cat("feedback on the % of the original experts selected that were based on the West\n")
cat("Coast. Model 4 predicts the proportion of experts working at a university with an\n")
cat("indicator for receiving feedback on the % of the original experts selected that\n")
cat("worked at a university.\n\n")

cat("Models 5-8 were used to construct a simultaneous system of equations. All of\n")
cat("these models include the following indicators as predictors: indicators for\n")
cat("receiving feedback on the % of the original experts selected that (1) were women,\n")
cat("(2) were under 50 years old, (3) were based on the West Coast, and (4) worked at a\n")
cat("university. Model 5 predicts the proportion of women selected. Model 6 predicts the\n")
cat("proportion of experts under 50 selected. Model 7 predicts the proportion of experts\n")
cat("based on the West Coast. Model 8 predicts the proportion of experts working at a\n")
cat("university. Models 5-8 had the constant suppressed to prevent collinearity.\n\n")

cat("Standard errors reported in parentheses are estimated robustly using HC3 and are\n")
cat("clustered by participant in Models 5-8.\n\n")

cat("+p<0.1;*p<0.05;**p<0.01;***p<0.001.\n\n")

cat("================================================================================\n")
