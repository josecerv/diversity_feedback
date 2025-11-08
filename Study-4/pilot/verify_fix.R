# Verify the fix
library(dplyr)
library(lmtest)
library(sandwich)

# Load data
d0 <- read.csv('Study-4/pilot/Study-4-pilot.csv', check.names = FALSE)

# Using the fixed approach
calc_sig_test <- function(ctx, bs) {
  subset_data <- d0 %>% filter(context == ctx, base == bs)
  model <- lm(female_pick ~ gender_feedback, data = subset_data)
  robust_se <- sqrt(diag(vcovHC(model, type = "HC3")))
  coef_est <- coef(model)["gender_feedback"]
  t_stat <- coef_est / robust_se["gender_feedback"]
  p <- 2 * pt(-abs(t_stat), df = df.residual(model))
  return(p)
}

sig_tests <- expand.grid(
  context = c("surgeon", "nursing"),
  base = c("low", "high"),
  stringsAsFactors = FALSE
) %>%
  mutate(
    p_value = mapply(calc_sig_test, context, base),
    sig_label = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01 ~ "**",
      p_value < 0.05 ~ "*",
      p_value < 0.10 ~ "†",
      TRUE ~ "n.s."
    )
  )

print(sig_tests)
