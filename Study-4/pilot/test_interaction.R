# Test if treatment effect differs between surgeon vs nursing context
# when women are underrepresented in the pool

library(dplyr)
library(lmtest)
library(sandwich)

# Load data
d0 <- read.csv('Study-4/pilot/Study-4-pilot.csv', check.names = FALSE)

# First, let's verify the cell means that the user mentioned
cat("=== CELL MEANS BY CONTEXT (Women Underrepresented in Pool) ===\n\n")
cell_means <- d0 %>%
  filter(base == "low") %>%
  group_by(context, cond) %>%
  summarise(
    n = n(),
    mean_female_pick = mean(female_pick) * 100,
    se = sd(female_pick) / sqrt(n()) * 100,
    .groups = "drop"
  ) %>%
  arrange(context, cond)

print(cell_means)

# Calculate treatment effects
cat("\n\n=== TREATMENT EFFECTS BY CONTEXT ===\n\n")
surgeon_control <- cell_means %>% filter(context == "surgeon", cond == "control") %>% pull(mean_female_pick)
surgeon_treat <- cell_means %>% filter(context == "surgeon", cond == "treat") %>% pull(mean_female_pick)
nursing_control <- cell_means %>% filter(context == "nursing", cond == "control") %>% pull(mean_female_pick)
nursing_treat <- cell_means %>% filter(context == "nursing", cond == "treat") %>% pull(mean_female_pick)

surgeon_effect <- surgeon_treat - surgeon_control
nursing_effect <- nursing_treat - nursing_control

cat(sprintf("Surgeon context: %.1f%% (control) vs %.1f%% (treatment) = %.1f pp effect\n",
            surgeon_control, surgeon_treat, surgeon_effect))
cat(sprintf("Nursing context: %.1f%% (control) vs %.1f%% (treatment) = %.1f pp effect\n",
            nursing_control, nursing_treat, nursing_effect))
cat(sprintf("\nDifference in treatment effects: %.1f pp\n\n", surgeon_effect - nursing_effect))

# Now test if this difference is statistically significant
# Filter to low base only
d_low <- d0 %>% filter(base == "low")

# Model with interaction: gender_feedback * surgeon_context
# The interaction term tests if the treatment effect differs by context
model <- lm(female_pick ~ gender_feedback * surgeon_context, data = d_low)

# Get robust standard errors
robust_se <- sqrt(diag(vcovHC(model, type = "HC3")))
coef_est <- coef(model)
t_stats <- coef_est / robust_se
p_values <- 2 * pt(-abs(t_stats), df = df.residual(model))

cat("=== REGRESSION MODEL (Women Underrepresented in Pool Only) ===\n")
cat("Model: female_pick ~ gender_feedback * surgeon_context\n\n")

results <- data.frame(
  Coefficient = names(coef_est),
  Estimate = coef_est,
  Robust_SE = robust_se,
  t_value = t_stats,
  p_value = p_values
)
rownames(results) <- NULL

print(results)

cat("\n=== INTERPRETATION ===\n\n")
cat("The interaction term 'gender_feedback:surgeon_context' tests whether\n")
cat("the treatment effect differs between surgeon and nursing contexts\n")
cat("when women are underrepresented in the pool.\n\n")

interaction_coef <- coef_est["gender_feedback:surgeon_context"]
interaction_p <- p_values["gender_feedback:surgeon_context"]

cat(sprintf("Interaction coefficient: %.4f (p = %.4f)\n\n", interaction_coef, interaction_p))

if (interaction_p < 0.05) {
  cat("CONCLUSION: The treatment effects ARE statistically significantly different\n")
  cat("between surgeon and nursing contexts (p < 0.05).\n")
} else if (interaction_p < 0.10) {
  cat("CONCLUSION: The treatment effects show a marginally significant difference\n")
  cat("between surgeon and nursing contexts (p < 0.10).\n")
} else {
  cat("CONCLUSION: The treatment effects are NOT statistically significantly different\n")
  cat("between surgeon and nursing contexts (p >= 0.10).\n")
}

# Calculate 95% confidence interval for the interaction
alpha <- 0.05
t_crit <- qt(1 - alpha/2, df = df.residual(model))
ci_lower <- interaction_coef - t_crit * robust_se["gender_feedback:surgeon_context"]
ci_upper <- interaction_coef + t_crit * robust_se["gender_feedback:surgeon_context"]

cat(sprintf("\n95%% CI for interaction: [%.4f, %.4f]\n", ci_lower, ci_upper))
