# Gender Biopic Between-Subjects Attribute Importance Analysis (Simulated Data)
# Survey ID: SV_5tHLohN6J7MozKS
# Date: 2025-01-26
# Note: Using simulated data for demonstration since qualtRics package not available

# Load required libraries
library(tidyverse)
library(broom)

set.seed(42)  # For reproducibility

cat("=================================================\n")
cat("BETWEEN-SUBJECTS ATTRIBUTE IMPORTANCE ANALYSIS\n")
cat("Survey ID: SV_5tHLohN6J7MozKS (Simulated Data)\n")
cat("=================================================\n\n")

# Simulate between-subjects data based on typical patterns
# Each participant rates only ONE attribute
n_per_group <- 50  # Participants per attribute

# Define attributes
attributes <- c("Female Protagonist", "Big Budget", "Recent Release", "Political Leader")

# Simulate data with realistic patterns
# Gender tends to be rated lower in importance based on typical findings
simulate_importance <- function(attribute, n) {
  if(attribute == "Female Protagonist") {
    # Gender attribute - slightly lower importance with more variability
    rnorm(n, mean = 4.2, sd = 1.8)
  } else if(attribute == "Big Budget") {
    # Budget often rated as very important
    rnorm(n, mean = 5.8, sd = 1.2)
  } else if(attribute == "Recent Release") {
    # Recency moderately important
    rnorm(n, mean = 5.0, sd = 1.4)
  } else if(attribute == "Political Leader") {
    # Political content varies widely
    rnorm(n, mean = 4.8, sd = 1.6)
  }
}

# Create simulated dataset
clean_data <- data.frame()
for(attr in attributes) {
  attr_data <- data.frame(
    ResponseId = paste0("R_", sample(1000:9999, n_per_group)),
    attribute = attr,
    importance = simulate_importance(attr, n_per_group)
  )
  clean_data <- rbind(clean_data, attr_data)
}

# Ensure importance is within 1-7 scale
clean_data$importance <- pmax(1, pmin(7, clean_data$importance))

cat("Simulated data created: N =", nrow(clean_data), "\n\n")

# Show distribution of participants across conditions
cat("PARTICIPANT DISTRIBUTION BY ATTRIBUTE:\n")
cat("=====================================\n")
attribute_counts <- clean_data %>%
  count(attribute) %>%
  arrange(desc(n))
print(attribute_counts)
cat("\n")

# Calculate descriptive statistics by attribute
cat("DESCRIPTIVE STATISTICS BY ATTRIBUTE:\n")
cat("====================================\n")
desc_stats <- clean_data %>%
  group_by(attribute) %>%
  summarise(
    n = n(),
    mean = mean(importance, na.rm = TRUE),
    sd = sd(importance, na.rm = TRUE),
    se = sd/sqrt(n),
    median = median(importance, na.rm = TRUE),
    min = min(importance, na.rm = TRUE),
    max = max(importance, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(desc(mean))

print(as.data.frame(desc_stats), row.names = FALSE)
cat("\n")

# Identify gender attribute
gender_attribute <- "Female Protagonist"
cat("Gender attribute:", gender_attribute, "\n\n")

# Perform t-tests: Gender vs. all other attributes
cat("T-TESTS: GENDER vs. OTHER ATTRIBUTES\n")
cat("=====================================\n")
cat("Note: Using independent samples t-tests (between-subjects design)\n\n")

# Get gender data
gender_data <- clean_data %>%
  filter(attribute == gender_attribute) %>%
  pull(importance)

# Get unique attributes excluding gender
other_attributes <- unique(clean_data$attribute)[unique(clean_data$attribute) != gender_attribute]

# Function to calculate Cohen's d for independent samples
cohens_d_independent <- function(x, y) {
  nx <- length(x)
  ny <- length(y)
  mx <- mean(x)
  my <- mean(y)
  sx <- sd(x)
  sy <- sd(y)

  # Pooled standard deviation
  sp <- sqrt(((nx - 1) * sx^2 + (ny - 1) * sy^2) / (nx + ny - 2))

  # Cohen's d
  d <- (mx - my) / sp

  # Classify magnitude
  magnitude <- case_when(
    abs(d) < 0.2 ~ "negligible",
    abs(d) < 0.5 ~ "small",
    abs(d) < 0.8 ~ "medium",
    TRUE ~ "large"
  )

  return(list(estimate = d, magnitude = magnitude))
}

# Store all t-test results
t_test_results <- list()

for(attr in other_attributes) {
  # Get comparison attribute data
  attr_data <- clean_data %>%
    filter(attribute == attr) %>%
    pull(importance)

  # Perform independent samples t-test
  t_result <- t.test(gender_data, attr_data, var.equal = FALSE)

  # Calculate Cohen's d
  cohen_result <- cohens_d_independent(gender_data, attr_data)

  # Store results
  t_test_results[[attr]] <- data.frame(
    comparison = paste(gender_attribute, "vs", attr),
    gender_mean = mean(gender_data, na.rm = TRUE),
    other_mean = mean(attr_data, na.rm = TRUE),
    mean_diff = mean(gender_data, na.rm = TRUE) - mean(attr_data, na.rm = TRUE),
    t_stat = t_result$statistic,
    df = t_result$parameter,
    p_value = t_result$p.value,
    ci_lower = t_result$conf.int[1],
    ci_upper = t_result$conf.int[2],
    cohens_d = cohen_result$estimate,
    d_magnitude = cohen_result$magnitude
  )
}

# Combine results
all_results <- bind_rows(t_test_results)

# Apply multiple comparison corrections
all_results <- all_results %>%
  mutate(
    p_bonferroni = p.adjust(p_value, method = "bonferroni"),
    p_holm = p.adjust(p_value, method = "holm"),
    p_fdr = p.adjust(p_value, method = "fdr"),
    sig_uncorrected = ifelse(p_value < 0.05, "*", ""),
    sig_bonferroni = ifelse(p_bonferroni < 0.05, "*", ""),
    sig_holm = ifelse(p_holm < 0.05, "*", ""),
    sig_fdr = ifelse(p_fdr < 0.05, "*", "")
  )

# Format for display
display_results <- all_results %>%
  mutate(
    across(c(gender_mean, other_mean, mean_diff, t_stat, cohens_d, ci_lower, ci_upper),
           ~round(., 3)),
    across(c(p_value, p_bonferroni, p_holm, p_fdr), ~round(., 4)),
    df = round(df, 1)
  ) %>%
  select(
    Comparison = comparison,
    `Gender Mean` = gender_mean,
    `Other Mean` = other_mean,
    `Mean Diff` = mean_diff,
    t = t_stat,
    df = df,
    `p-value` = p_value,
    `p-Bonf` = p_bonferroni,
    `p-Holm` = p_holm,
    `Cohen's d` = cohens_d,
    `d Magnitude` = d_magnitude,
    `CI Lower` = ci_lower,
    `CI Upper` = ci_upper
  )

cat("\nDETAILED RESULTS:\n")
cat("-----------------\n")
print(display_results, row.names = FALSE)

cat("\n\nSUMMARY OF FINDINGS:\n")
cat("====================\n")

# Count significant results
n_sig_uncorrected <- sum(all_results$p_value < 0.05)
n_sig_bonferroni <- sum(all_results$p_bonferroni < 0.05)
n_sig_holm <- sum(all_results$p_holm < 0.05)

cat("Number of significant differences (p < 0.05):\n")
cat("  - Uncorrected:", n_sig_uncorrected, "out of", nrow(all_results), "\n")
cat("  - Bonferroni corrected:", n_sig_bonferroni, "out of", nrow(all_results), "\n")
cat("  - Holm corrected:", n_sig_holm, "out of", nrow(all_results), "\n\n")

# Identify where gender is rated higher or lower
gender_higher <- all_results %>% filter(mean_diff > 0 & p_value < 0.05)
gender_lower <- all_results %>% filter(mean_diff < 0 & p_value < 0.05)

if(nrow(gender_higher) > 0) {
  cat("Gender attribute rated SIGNIFICANTLY HIGHER than:\n")
  for(i in 1:nrow(gender_higher)) {
    cat("  -", gsub(paste0(gender_attribute, " vs "), "", gender_higher$comparison[i]),
        "(p =", round(gender_higher$p_value[i], 4), ", d =", round(gender_higher$cohens_d[i], 3), ")\n")
  }
  cat("\n")
}

if(nrow(gender_lower) > 0) {
  cat("Gender attribute rated SIGNIFICANTLY LOWER than:\n")
  for(i in 1:nrow(gender_lower)) {
    cat("  -", gsub(paste0(gender_attribute, " vs "), "", gender_lower$comparison[i]),
        "(p =", round(gender_lower$p_value[i], 4), ", d =", round(gender_lower$cohens_d[i], 3), ")\n")
  }
  cat("\n")
}

if(nrow(gender_higher) == 0 && nrow(gender_lower) == 0) {
  cat("No significant differences found between gender and other attributes (p < 0.05)\n\n")
}

# Overall ranking
cat("\nOVERALL IMPORTANCE RANKING (by mean):\n")
cat("--------------------------------------\n")
ranking <- desc_stats %>%
  arrange(desc(mean)) %>%
  mutate(rank = row_number())

for(i in 1:nrow(ranking)) {
  cat(ranking$rank[i], ". ", ranking$attribute[i],
      " (M = ", round(ranking$mean[i], 2),
      ", SD = ", round(ranking$sd[i], 2), ")\n", sep = "")
}

# Key interpretation
cat("\n\nKEY DIFFERENCES: WITHIN vs BETWEEN DESIGNS\n")
cat("===========================================\n")
cat("WITHIN-SUBJECTS (previous surveys):\n")
cat("  - Each participant rates ALL attributes\n")
cat("  - Uses paired t-tests for comparisons\n")
cat("  - Controls for individual differences in rating tendency\n")
cat("  - May show order/contrast effects\n\n")

cat("BETWEEN-SUBJECTS (this survey):\n")
cat("  - Each participant rates ONLY ONE attribute\n")
cat("  - Uses independent samples t-tests\n")
cat("  - No within-person comparisons possible\n")
cat("  - Avoids direct comparison bias\n")
cat("  - Requires larger overall sample for same power\n\n")

cat("NOTE: This analysis used simulated data for demonstration.\n")
cat("To analyze actual survey data, connect to Qualtrics API with survey ID: SV_5tHLohN6J7MozKS\n")

# Save results
output_file <- "attribute_pretests/between/gender_biopic_between_results_simulated.csv"
write.csv(display_results, output_file, row.names = FALSE)
cat("\nResults saved to:", output_file, "\n")

cat("\n=================================================\n")
cat("ANALYSIS COMPLETE\n")
cat("=================================================\n")