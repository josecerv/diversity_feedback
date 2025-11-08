# Debug significance tests
library(dplyr)
library(lmtest)
library(sandwich)

# Load data
d0 <- read.csv('Study-4/pilot/Study-4-pilot.csv', check.names = FALSE)

# Calculate significance for each context × base combination
sig_tests <- expand.grid(
  context = c("surgeon", "nursing"),
  base = c("low", "high")
) %>%
  rowwise() %>%
  mutate(
    # Run t-test for treatment effect
    p_value = {
      subset_data <- d0 %>% filter(context == .data$context, base == .data$base)
      model <- lm(female_pick ~ gender_feedback, data = subset_data)
      robust_se <- sqrt(diag(vcovHC(model, type = "HC3")))
      coef_est <- coef(model)["gender_feedback"]
      t_stat <- coef_est / robust_se["gender_feedback"]
      p <- 2 * pt(-abs(t_stat), df = df.residual(model))
      p
    },
    sig_label = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01 ~ "**",
      p_value < 0.05 ~ "*",
      p_value < 0.10 ~ "†",
      TRUE ~ "n.s."
    )
  ) %>%
  ungroup()

print(sig_tests)

# Let's specifically check surgeon + high
cat("\n\n=== DETAILED CHECK: SURGEON + HIGH BASE ===\n")
subset_data <- d0 %>% filter(context == "surgeon", base == "high")
model <- lm(female_pick ~ gender_feedback, data = subset_data)

cat("\nStandard summary:\n")
print(summary(model))

cat("\nRobust SEs:\n")
robust_se <- sqrt(diag(vcovHC(model, type = "HC3")))
coef_est <- coef(model)["gender_feedback"]
t_stat <- coef_est / robust_se["gender_feedback"]
p <- 2 * pt(-abs(t_stat), df = df.residual(model))

cat(sprintf("Coefficient: %.5f\n", coef_est))
cat(sprintf("Robust SE: %.5f\n", robust_se["gender_feedback"]))
cat(sprintf("t-stat: %.5f\n", t_stat))
cat(sprintf("p-value: %.5f\n", p))
