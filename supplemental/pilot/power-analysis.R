################################################################################
# Power Analysis for 2x2 Factorial Design Pilot Study
# Design: Gender Feedback (Control vs Treatment) × Base Condition (Low vs High)
# Primary Outcome: female_pick (binary)
################################################################################

# Load required packages
library(dplyr)
library(pwr)

# Clear workspace
rm(list=ls())

# Read pilot data
d0 <- read.csv('pilot-study.csv', check.names = FALSE)

cat("================================================================================\n")
cat("PILOT DATA SUMMARY\n")
cat("================================================================================\n\n")

# Overall sample size
cat("Total N in pilot:", nrow(d0), "\n\n")

# Sample sizes by condition
sample_sizes <- d0 %>%
  group_by(base_condition, gender_feedback) %>%
  summarize(n = n(), .groups = 'drop')

print(sample_sizes)

cat("\n================================================================================\n")
cat("EFFECT SIZES FROM PILOT DATA\n")
cat("================================================================================\n\n")

# Calculate proportions and effects for each base condition
results_by_condition <- d0 %>%
  group_by(base_condition, gender_feedback) %>%
  summarize(
    n = n(),
    prop_female = mean(female_pick, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(base_condition, gender_feedback)

print(results_by_condition)

# Calculate effect sizes (differences in proportions)
cat("\n--- Simple Effects ---\n")

# Low base condition
low_control <- results_by_condition %>%
  filter(base_condition == "low", gender_feedback == 0) %>%
  pull(prop_female)
low_treatment <- results_by_condition %>%
  filter(base_condition == "low", gender_feedback == 1) %>%
  pull(prop_female)
effect_low <- low_treatment - low_control

cat(sprintf("Low Base: Control = %.3f, Treatment = %.3f, Diff = %.3f (%.1f pp)\n",
            low_control, low_treatment, effect_low, effect_low * 100))

# High base condition
high_control <- results_by_condition %>%
  filter(base_condition == "high", gender_feedback == 0) %>%
  pull(prop_female)
high_treatment <- results_by_condition %>%
  filter(base_condition == "high", gender_feedback == 1) %>%
  pull(prop_female)
effect_high <- high_treatment - high_control

cat(sprintf("High Base: Control = %.3f, Treatment = %.3f, Diff = %.3f (%.1f pp)\n",
            high_control, high_treatment, effect_high, effect_high * 100))

# Interaction effect
interaction_effect <- effect_low - effect_high
cat(sprintf("\nInteraction Effect: %.3f (%.1f pp)\n", interaction_effect, interaction_effect * 100))

# Convert to Cohen's h for proportions
cohens_h_low <- 2 * (asin(sqrt(low_treatment)) - asin(sqrt(low_control)))
cohens_h_high <- 2 * (asin(sqrt(high_treatment)) - asin(sqrt(high_control)))

cat(sprintf("\nCohen's h (Low Base): %.3f", cohens_h_low))
cat(sprintf("\nCohen's h (High Base): %.3f\n", cohens_h_high))

# Interpret effect sizes
interpret_h <- function(h) {
  if (abs(h) < 0.2) return("small")
  else if (abs(h) < 0.5) return("small-medium")
  else if (abs(h) < 0.8) return("medium-large")
  else return("large")
}

cat(sprintf("Effect size interpretation (Low): %s\n", interpret_h(cohens_h_low)))
cat(sprintf("Effect size interpretation (High): %s\n", interpret_h(cohens_h_high)))

cat("\n================================================================================\n")
cat("POWER ANALYSIS: SAMPLE SIZE CALCULATIONS\n")
cat("================================================================================\n\n")

# Function to calculate required sample size for 2x2 design
# Using chi-square test for proportions

# For simple effects within each base condition
cat("--- Simple Effects Power Analysis ---\n\n")

# Power levels to test
power_levels <- c(0.80, 0.85, 0.90)
alpha <- 0.05

# Calculate sample sizes for Low Base condition
cat("LOW BASE CONDITION:\n")
for (pwr_target in power_levels) {
  tryCatch({
    result <- pwr.2p.test(
      h = cohens_h_low,
      sig.level = alpha,
      power = pwr_target,
      alternative = "two.sided"
    )
    n_per_group <- ceiling(result$n)
    n_total_low <- n_per_group * 2  # Control + Treatment
    cat(sprintf("  Power = %.0f%%: N = %d per group, Total N (Low Base) = %d\n",
                pwr_target * 100, n_per_group, n_total_low))
  }, error = function(e) {
    cat(sprintf("  Power = %.0f%%: Unable to calculate (effect may be too small)\n",
                pwr_target * 100))
  })
}

cat("\nHIGH BASE CONDITION:\n")
for (pwr_target in power_levels) {
  tryCatch({
    result <- pwr.2p.test(
      h = cohens_h_high,
      sig.level = alpha,
      power = pwr_target,
      alternative = "two.sided"
    )
    n_per_group <- ceiling(result$n)
    n_total_high <- n_per_group * 2  # Control + Treatment
    cat(sprintf("  Power = %.0f%%: N = %d per group, Total N (High Base) = %d\n",
                pwr_target * 100, n_per_group, n_total_high))
  }, error = function(e) {
    cat(sprintf("  Power = %.0f%%: Unable to calculate (effect may be too small)\n",
                pwr_target * 100))
  })
}

# Calculate total sample size for full 2x2 design
cat("\n--- Full 2x2 Design (Both Conditions Combined) ---\n\n")

# Average effect size across conditions
avg_effect_h <- mean(c(cohens_h_low, cohens_h_high))
cat(sprintf("Average Cohen's h across conditions: %.3f\n\n", avg_effect_h))

cat("TOTAL SAMPLE SIZE NEEDED (all 4 cells):\n")
for (pwr_target in power_levels) {
  tryCatch({
    result <- pwr.2p.test(
      h = avg_effect_h,
      sig.level = alpha,
      power = pwr_target,
      alternative = "two.sided"
    )
    n_per_group <- ceiling(result$n)
    # For 2x2 design: need n_per_group in each of 4 cells
    n_total_2x2 <- n_per_group * 4
    cat(sprintf("  Power = %.0f%%: N = %d per cell, Total N = %d\n",
                pwr_target * 100, n_per_group, n_total_2x2))
  }, error = function(e) {
    cat(sprintf("  Power = %.0f%%: Unable to calculate\n", pwr_target * 100))
  })
}

# Alternative: Use smaller of the two effects (conservative)
min_effect_h <- min(abs(cohens_h_low), abs(cohens_h_high))
cat(sprintf("\n\nCONSERVATIVE ESTIMATE (using smaller effect, h = %.3f):\n", min_effect_h))

for (pwr_target in power_levels) {
  tryCatch({
    result <- pwr.2p.test(
      h = min_effect_h,
      sig.level = alpha,
      power = pwr_target,
      alternative = "two.sided"
    )
    n_per_group <- ceiling(result$n)
    n_total_2x2 <- n_per_group * 4
    cat(sprintf("  Power = %.0f%%: N = %d per cell, Total N = %d\n",
                pwr_target * 100, n_per_group, n_total_2x2))
  }, error = function(e) {
    cat(sprintf("  Power = %.0f%%: Unable to calculate\n", pwr_target * 100))
  })
}

cat("\n================================================================================\n")
cat("POWER ANALYSIS: INTERACTION EFFECT\n")
cat("================================================================================\n\n")

# For interaction, we need to think about it differently
# The interaction tests whether the effect differs across base conditions
# Effect size for interaction is the difference in differences

# Pooled standard deviation approximation for binary outcome
pooled_p <- mean(d0$female_pick)
pooled_sd <- sqrt(pooled_p * (1 - pooled_p))

# Interaction effect in Cohen's d terms
interaction_d <- abs(interaction_effect) / pooled_sd
cat(sprintf("Interaction effect size (Cohen's d): %.3f\n", interaction_d))
cat(sprintf("Interaction effect size interpretation: %s\n",
            ifelse(interaction_d < 0.2, "negligible",
                   ifelse(interaction_d < 0.5, "small",
                          ifelse(interaction_d < 0.8, "medium", "large")))))

cat("\n\nSample size to detect interaction at different power levels:\n")
for (pwr_target in power_levels) {
  tryCatch({
    # For 2x2 ANOVA-style interaction
    result <- pwr.f2.test(
      u = 1,  # 1 df for interaction
      f2 = interaction_d^2 / (1 - interaction_d^2),  # Convert d to f^2
      sig.level = alpha,
      power = pwr_target
    )
    # For 2x2 design with equal cell sizes
    n_per_cell <- ceiling(result$v / 4 + 1)
    n_total <- n_per_cell * 4
    cat(sprintf("  Power = %.0f%%: N = %d per cell, Total N = %d\n",
                pwr_target * 100, n_per_cell, n_total))
  }, error = function(e) {
    cat(sprintf("  Power = %.0f%%: Unable to calculate\n", pwr_target * 100))
  })
}

cat("\n================================================================================\n")
cat("RECOMMENDATIONS\n")
cat("================================================================================\n\n")

# Calculate recommended sample size (80% power, conservative)
tryCatch({
  result <- pwr.2p.test(
    h = min_effect_h,
    sig.level = 0.05,
    power = 0.80,
    alternative = "two.sided"
  )
  n_per_group <- ceiling(result$n)
  recommended_n <- n_per_group * 4

  cat(sprintf("RECOMMENDED TOTAL SAMPLE SIZE: N = %d\n", recommended_n))
  cat(sprintf("  - %d participants per cell (2x2 design)\n", n_per_group))
  cat(sprintf("  - %d in each base condition (Low and High)\n", n_per_group * 2))
  cat(sprintf("  - %d in each treatment condition (Control and Treatment)\n\n", n_per_group * 2))

  cat("This is based on:\n")
  cat("  - 80% power (standard)\n")
  cat("  - Alpha = 0.05 (two-sided)\n")
  cat("  - Conservative effect size (smaller of the two conditions)\n")
  cat(sprintf("  - Effect size h = %.3f\n\n", min_effect_h))

  # Compare to pilot
  pilot_n <- nrow(d0)
  if (recommended_n > pilot_n) {
    cat(sprintf("NOTE: This is %.1fx your pilot sample size of N=%d.\n",
                recommended_n / pilot_n, pilot_n))
  } else {
    cat(sprintf("NOTE: Your pilot of N=%d already exceeds this threshold.\n", pilot_n))
  }

  # Realistic recommendations
  cat("\n--- Practical Recommendations ---\n\n")

  if (recommended_n < 200) {
    cat("✓ This is a very manageable sample size.\n")
  } else if (recommended_n < 400) {
    cat("✓ This is a moderate sample size, reasonable for most studies.\n")
  } else if (recommended_n < 800) {
    cat("⚠ This is a larger sample size. Consider:\n")
    cat("  - Whether the effect size from pilot is realistic\n")
    cat("  - If a slightly lower power (75%) would be acceptable\n")
    cat("  - Increasing the effect size through design changes\n")
  } else {
    cat("⚠ This is a very large sample size. Consider:\n")
    cat("  - The pilot effect sizes may be unreliable (small pilot N)\n")
    cat("  - Design modifications to increase effect size\n")
    cat("  - Whether detecting such a small effect is practically important\n")
  }

}, error = function(e) {
  cat("Unable to calculate recommended sample size due to small effect sizes.\n")
  cat("Consider increasing the strength of your manipulation or simplifying the design.\n")
})

cat("\n================================================================================\n")
cat("SENSITIVITY ANALYSIS: DETECTABLE EFFECT SIZES\n")
cat("================================================================================\n\n")

# What effects can we detect with different sample sizes?
sample_sizes <- c(100, 200, 300, 400, 500, 600, 800, 1000)

cat("Minimum detectable effect (Cohen's h) at 80% power:\n\n")
cat(sprintf("%-12s %-20s %-20s\n", "Total N", "h (detectable)", "Prop Diff (approx)"))
cat(strrep("-", 52), "\n")

for (n_total in sample_sizes) {
  n_per_group <- n_total / 4  # 2x2 design
  result <- pwr.2p.test(
    n = n_per_group,
    sig.level = 0.05,
    power = 0.80,
    alternative = "two.sided"
  )
  # Approximate conversion to proportion difference (for p around 0.3)
  approx_prop_diff <- result$h / 2 * sqrt(0.3 * 0.7 * 2)
  cat(sprintf("%-12d %-20.3f %-20.3f (%.1f pp)\n",
              n_total, result$h, approx_prop_diff, approx_prop_diff * 100))
}

cat("\n================================================================================\n")
cat("END OF POWER ANALYSIS\n")
cat("================================================================================\n")
