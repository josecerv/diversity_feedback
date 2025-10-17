# Calculate main effect combining both sets

library(dplyr)
library(lmtest)
library(sandwich)

# Read the data
d0 <- read.csv('Study5.csv', check.names = F)

robust_summary <- function(model) {
    robust_se <- sqrt(diag(vcovHC(model, type = "HC3")))
    model_summary <- summary(model)
    model_summary$coefficients[, "Std. Error"] <- robust_se
    model_summary$coefficients[, "t value"] <- model_summary$coefficients[, "Estimate"] / robust_se
    model_summary$coefficients[, "Pr(>|t|)"] <- 2 * pt(-abs(model_summary$coefficients[, "t value"]),
                                                        df = model_summary$df[2], lower.tail = TRUE)
    return(model_summary)
}

# Function to convert p-value to significance stars
get_sig_stars <- function(p) {
  if (p < 0.001) return("***")
  else if (p < 0.01) return("**")
  else if (p < 0.05) return("*")
  else return("n.s.")
}

# Main effect (combining both sets)
r_main <- lm(women_proportion ~ treatment, data = d0)

# Get robust summary
rob_sum <- robust_summary(r_main)
rob_ci <- confint(r_main)

# Calculate robust confidence intervals
robust_se <- sqrt(diag(vcovHC(r_main, type = "HC3")))
t_crit <- qt(0.975, df = df.residual(r_main))
rob_ci <- cbind(
  coef(r_main) - t_crit * robust_se,
  coef(r_main) + t_crit * robust_se
)

# Extract values
control_mean <- coef(r_main)["(Intercept)"]
treatment_effect <- coef(r_main)["treatment"]
treatment_mean <- control_mean + treatment_effect
p_value <- rob_sum$coefficients["treatment", "Pr(>|t|)"]
sig <- get_sig_stars(p_value)

# Get sample sizes
n_control <- sum(d0$treatment == 0)
n_treatment <- sum(d0$treatment == 1)

# Calculate descriptive stats by group
desc_stats <- d0 |>
  group_by(treatment) |>
  summarize(
    n = n(),
    mean = mean(women_proportion),
    sd = sd(women_proportion),
    se = sd / sqrt(n)
  )

cat("\n=======================================================\n")
cat("          MAIN EFFECT (Combining Both Sets)           \n")
cat("=======================================================\n\n")

cat(sprintf("Sample Size: N = %d (Control: %d, Treatment: %d)\n\n",
            nrow(d0), n_control, n_treatment))

cat("Descriptive Statistics:\n")
cat("------------------------\n")
cat(sprintf("Control Mean:    %.1f%% women selected\n", control_mean * 100))
cat(sprintf("Treatment Mean:  %.1f%% women selected\n", treatment_mean * 100))
cat(sprintf("Standard Error:  %.2f percentage points\n\n", rob_sum$coefficients["treatment", "Std. Error"] * 100))

cat("Treatment Effect:\n")
cat("------------------\n")
cat(sprintf("Effect Size:     +%.2f percentage points\n", treatment_effect * 100))
cat(sprintf("95%% CI:          [%.2f, %.2f]\n", rob_ci[2,1] * 100, rob_ci[2,2] * 100))
cat(sprintf("t-statistic:     %.3f\n", rob_sum$coefficients["treatment", "t value"]))
cat(sprintf("p-value:         %.4f %s\n", p_value, sig))
cat(sprintf("Significance:    %s\n\n", sig))

cat("Interpretation:\n")
cat("----------------\n")
cat(sprintf("Showing women feedback increases the proportion of women\n"))
cat(sprintf("selected by %.1f percentage points (from %.1f%% to %.1f%%).\n",
            treatment_effect * 100, control_mean * 100, treatment_mean * 100))

if (p_value < 0.05) {
  cat("This effect is statistically significant.\n")
} else {
  cat("This effect is not statistically significant.\n")
}

cat("\n=======================================================\n\n")

# Print full regression output
print(rob_sum)
