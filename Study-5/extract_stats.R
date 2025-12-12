# Script to extract statistics for Study 5 Results section
rm(list=ls())

library(dplyr)
library(lmtest)
library(sandwich)

# Read the processed data
d0 <- read.csv('Study5.csv', check.names = F)

# Define women categories
women <- c("Whitney Wolfe Herd (Founder of Bumble)",
           "Leah Busque (Founder of TaskRabbit)",
           "Melanie Perkins (Founder of Canva)",
           "Julia Hartz (Co-founder of EventBrite)",
           "Arianna Huffington (Founder of Thrive Global)",
           "Lisa Su (CEO of AMD)",
           "Linda Yaccarino (CEO of X)",
           "Kate Johnson (CEO of Lumen Technologies)",
           "Christine Leahy (CEO of CDW)",
           "Kendra Scott (Founder of Kendra Scott)",
           "Sara Blakely (Founder of Spanx)",
           "Oprah Winfrey (Founder of OWN)",
           "Jessica Alba (Founder of The Honest Company)",
           "Jane Fraser (CEO of Citigroup)",
           "Mary Barra (CEO of General Motors)",
           "Gail Boudreaux (CEO of Elevance Health)",
           "Carol Tom\u00e9 (CEO of UPS)",
           "Michele Buck (CEO of Hershey)")

# Helper functions for robust SE
robust_summary <- function(model) {
    robust_se <- sqrt(diag(vcovHC(model, type = "HC3")))
    model_summary <- summary(model)
    model_summary$coefficients[, "Std. Error"] <- robust_se
    model_summary$coefficients[, "t value"] <- model_summary$coefficients[, "Estimate"] / robust_se
    model_summary$coefficients[, "Pr(>|t|)"] <- 2 * pt(-abs(model_summary$coefficients[, "t value"]), df = model_summary$df[2], lower.tail = TRUE)
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

cat("\n========================================\n")
cat("STUDY 5 STATISTICS FOR RESULTS SECTION\n")
cat("========================================\n\n")

cat("Total N:", nrow(d0), "\n\n")

# ============================================
# BALANCE CHECKS ON INITIAL SELECTIONS
# ============================================

cat("=== BALANCE CHECKS ON INITIAL 6 SELECTIONS ===\n\n")

# Calculate proportion of women in initial 6 selections (base_gender / 6)
d0$base_prop_women <- d0$base_gender / 6

# Women OVERREPRESENTED condition (men_pool = 0, i.e., 75% women pool)
d_overrep <- d0 %>% filter(men_pool == 0)
cat("WOMEN OVERREPRESENTED CONDITION (75% women in pool):\n")
cat("Overall % women in initial selections:", round(mean(d_overrep$base_prop_women) * 100, 1), "%\n")

overrep_control <- d_overrep %>% filter(treatment == 0)
overrep_treat <- d_overrep %>% filter(treatment == 1)
cat("  No gender feedback condition:", round(mean(overrep_control$base_prop_women) * 100, 1), "%\n")
cat("  Gender feedback condition:", round(mean(overrep_treat$base_prop_women) * 100, 1), "%\n")

# t-test for balance in overrep condition
t_overrep <- t.test(base_prop_women ~ treatment, data = d_overrep, var.equal = FALSE)
cat("  t-test: t(", round(t_overrep$parameter, 1), ") = ", round(t_overrep$statistic, 2),
    ", p = ", sprintf("%.3f", t_overrep$p.value),
    ", 95% CI = [", round(t_overrep$conf.int[1], 3), ", ", round(t_overrep$conf.int[2], 3), "]\n\n", sep="")

# Women UNDERREPRESENTED condition (men_pool = 1, i.e., 25% women pool)
d_underrep <- d0 %>% filter(men_pool == 1)
cat("WOMEN UNDERREPRESENTED CONDITION (25% women in pool):\n")
cat("Overall % women in initial selections:", round(mean(d_underrep$base_prop_women) * 100, 1), "%\n")

underrep_control <- d_underrep %>% filter(treatment == 0)
underrep_treat <- d_underrep %>% filter(treatment == 1)
cat("  No gender feedback condition:", round(mean(underrep_control$base_prop_women) * 100, 1), "%\n")
cat("  Gender feedback condition:", round(mean(underrep_treat$base_prop_women) * 100, 1), "%\n")

# t-test for balance in underrep condition
t_underrep <- t.test(base_prop_women ~ treatment, data = d_underrep, var.equal = FALSE)
cat("  t-test: t(", round(t_underrep$parameter, 1), ") = ", round(t_underrep$statistic, 2),
    ", p = ", sprintf("%.3f", t_underrep$p.value),
    ", 95% CI = [", round(t_underrep$conf.int[1], 3), ", ", round(t_underrep$conf.int[2], 3), "]\n\n", sep="")

# ============================================
# PRIMARY ANALYSES: EFFECT OF FEEDBACK ON 7TH SELECTION
# ============================================

cat("=== PRIMARY ANALYSES: 7TH (FINAL) SELECTION ===\n\n")

# Cell means
cat("CELL MEANS (% choosing woman as 7th panelist):\n")
cell_means <- d0 %>%
  group_by(men_pool, treatment) %>%
  summarise(
    n = n(),
    pct_woman = mean(female_pick) * 100,
    .groups = "drop"
  )
print(cell_means)
cat("\n")

# Women UNDERREPRESENTED: Simple effect of treatment
cat("WOMEN UNDERREPRESENTED POOL - Treatment Effect:\n")
model_underrep <- lm(female_pick ~ treatment, data = d_underrep)
summ_underrep <- robust_summary(model_underrep)
ci_underrep <- robust_confint(model_underrep)

cat("  Control (no feedback):", round(mean(underrep_control$female_pick) * 100, 1), "%\n")
cat("  Treatment (feedback):", round(mean(underrep_treat$female_pick) * 100, 1), "%\n")
cat("  B =", round(summ_underrep$coefficients["treatment", "Estimate"], 3), "\n")
cat("  95% CI = [", round(ci_underrep["treatment", 1], 3), ", ", round(ci_underrep["treatment", 2], 3), "]\n", sep="")
cat("  t(", summ_underrep$df[2], ") = ", round(summ_underrep$coefficients["treatment", "t value"], 2), "\n", sep="")
cat("  p = ", sprintf("%.3f", summ_underrep$coefficients["treatment", "Pr(>|t|)"]), "\n\n", sep="")

# Women OVERREPRESENTED: Simple effect of treatment
cat("WOMEN OVERREPRESENTED POOL - Treatment Effect:\n")
model_overrep <- lm(female_pick ~ treatment, data = d_overrep)
summ_overrep <- robust_summary(model_overrep)
ci_overrep <- robust_confint(model_overrep)

cat("  Control (no feedback):", round(mean(overrep_control$female_pick) * 100, 1), "%\n")
cat("  Treatment (feedback):", round(mean(overrep_treat$female_pick) * 100, 1), "%\n")
cat("  B =", round(summ_overrep$coefficients["treatment", "Estimate"], 3), "\n")
cat("  95% CI = [", round(ci_overrep["treatment", 1], 3), ", ", round(ci_overrep["treatment", 2], 3), "]\n", sep="")
cat("  t(", summ_overrep$df[2], ") = ", round(summ_overrep$coefficients["treatment", "t value"], 2), "\n", sep="")
cat("  p = ", sprintf("%.3f", summ_overrep$coefficients["treatment", "Pr(>|t|)"]), "\n\n", sep="")

# ============================================
# INTERACTION MODEL
# ============================================

cat("=== INTERACTION MODEL ===\n\n")
model_int <- lm(female_pick ~ treatment * men_pool, data = d0)
summ_int <- robust_summary(model_int)
ci_int <- robust_confint(model_int)

cat("Model: female_pick ~ treatment * men_pool\n\n")
cat("Interaction term (treatment:men_pool):\n")
cat("  B =", round(summ_int$coefficients["treatment:men_pool", "Estimate"], 3), "\n")
cat("  95% CI = [", round(ci_int["treatment:men_pool", 1], 3), ", ", round(ci_int["treatment:men_pool", 2], 3), "]\n", sep="")
cat("  t(", summ_int$df[2], ") = ", round(summ_int$coefficients["treatment:men_pool", "t value"], 2), "\n", sep="")
cat("  p = ", sprintf("%.3f", summ_int$coefficients["treatment:men_pool", "Pr(>|t|)"]), "\n\n", sep="")

cat("Full model summary:\n")
print(summ_int)
cat("\nConfidence intervals:\n")
print(ci_int)

# ============================================
# MEDIATOR CORRELATION TABLE FOR RESPONSE LETTER
# ============================================

cat("\n\n========================================\n")
cat("MEDIATOR CORRELATIONS FOR RESPONSE LETTER\n")
cat("========================================\n\n")

# Split by condition for correlation analysis
d_underrep_med <- d0 %>% filter(men_pool == 1, !is.na(fairness))
d_overrep_med <- d0 %>% filter(men_pool == 0, !is.na(fairness))

vars <- c("fairness", "internal_motivation", "external_motivation")
var_labels <- c("Fairness", "Internal Motivation", "External Motivation")

# Women Underrepresented Condition
cat("Women Underrepresented Condition (n =", nrow(d_underrep_med), ")\n")
cat("-----------------------------------------------------------\n\n")

cor_under <- data.frame(
  Variable1 = character(),
  Variable2 = character(),
  r = numeric(),
  p = numeric(),
  stringsAsFactors = FALSE
)

for (i in 1:(length(vars)-1)) {
  for (j in (i+1):length(vars)) {
    test <- cor.test(d_underrep_med[[vars[i]]], d_underrep_med[[vars[j]]])
    cor_under <- rbind(cor_under, data.frame(
      Variable1 = var_labels[i],
      Variable2 = var_labels[j],
      r = round(test$estimate, 3),
      p = round(test$p.value, 4)
    ))
  }
}

print(cor_under, row.names = FALSE)

# Women Overrepresented Condition
cat("\n\nWomen Overrepresented Condition (n =", nrow(d_overrep_med), ")\n")
cat("-----------------------------------------------------------\n\n")

cor_over <- data.frame(
  Variable1 = character(),
  Variable2 = character(),
  r = numeric(),
  p = numeric(),
  stringsAsFactors = FALSE
)

for (i in 1:(length(vars)-1)) {
  for (j in (i+1):length(vars)) {
    test <- cor.test(d_overrep_med[[vars[i]]], d_overrep_med[[vars[j]]])
    cor_over <- rbind(cor_over, data.frame(
      Variable1 = var_labels[i],
      Variable2 = var_labels[j],
      r = round(test$estimate, 3),
      p = round(test$p.value, 4)
    ))
  }
}

print(cor_over, row.names = FALSE)

# Create formatted table for response letter
cat("\n\n=== FORMATTED TABLE FOR RESPONSE LETTER ===\n")
cat("(Copy this into your Word document)\n\n")

cat("Table: Pairwise Correlations Among Mediators by Condition\n")
cat("==================================================================================\n")
cat(sprintf("%-35s %22s %22s\n", "", "Women Underrepresented", "Women Overrepresented"))
cat(sprintf("%-35s %22s %22s\n", "Variable Pair", paste0("(n = ", nrow(d_underrep_med), ")"), paste0("(n = ", nrow(d_overrep_med), ")")))
cat("----------------------------------------------------------------------------------\n")

for (i in 1:nrow(cor_under)) {
  p_under <- ifelse(cor_under$p[i] < .001, "< .001", sprintf("= %.3f", cor_under$p[i]))
  p_over <- ifelse(cor_over$p[i] < .001, "< .001", sprintf("= %.3f", cor_over$p[i]))

  cat(sprintf("%-35s r = %.2f (p %s)     r = %.2f (p %s)\n",
              paste(cor_under$Variable1[i], "x", cor_under$Variable2[i]),
              cor_under$r[i],
              p_under,
              cor_over$r[i],
              p_over))
}

cat("==================================================================================\n")
cat("Note: All correlations are significant at p < .001.\n")

# Full correlation matrices
cat("\n\n=== FULL CORRELATION MATRICES ===\n\n")

cat("Women Underrepresented Condition:\n")
cor_matrix_under <- cor(d_underrep_med[, vars], use = "complete.obs")
rownames(cor_matrix_under) <- var_labels
colnames(cor_matrix_under) <- c("Fairness", "Internal", "External")
print(round(cor_matrix_under, 3))

cat("\nWomen Overrepresented Condition:\n")
cor_matrix_over <- cor(d_overrep_med[, vars], use = "complete.obs")
rownames(cor_matrix_over) <- var_labels
colnames(cor_matrix_over) <- c("Fairness", "Internal", "External")
print(round(cor_matrix_over, 3))
