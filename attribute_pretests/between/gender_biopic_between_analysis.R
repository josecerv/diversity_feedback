# Gender Biopic Between-Subjects Attribute Importance Analysis
# Survey ID: SV_5tHLohN6J7MozKS
# Date: 2025-01-26

# Load required libraries
library(tidyverse)
library(qualtRics)
library(broom)
library(effsize)

# Set up Qualtrics API credentials
qualtrics_api_credentials(
  api_key = "BJ6aDiEgtQihneCgtZycVLYjaj0ao9gYkdRkb1UZ",
  base_url = "yul1.qualtrics.com",
  install = FALSE,
  overwrite = TRUE
)

# Survey ID for between-subjects gender biopic pretest
survey_id <- "SV_5tHLohN6J7MozKS"

cat("=================================================\n")
cat("BETWEEN-SUBJECTS ATTRIBUTE IMPORTANCE ANALYSIS\n")
cat("Survey ID:", survey_id, "\n")
cat("=================================================\n\n")

# Fetch survey data
cat("Fetching survey data...\n")
raw_data <- fetch_survey(survey_id, force_request = TRUE)
cat("Raw data collected: N =", nrow(raw_data), "\n\n")

# Check data structure
cat("Data structure:\n")
cat("Columns:", paste(names(raw_data), collapse = ", "), "\n\n")

# Clean and prepare data
# First, let's identify the relevant columns
# Look for importance ratings and condition/attribute assignment
importance_cols <- names(raw_data)[grepl("importance|Important", names(raw_data), ignore.case = TRUE)]
condition_cols <- names(raw_data)[grepl("condition|attribute|Attribute", names(raw_data), ignore.case = TRUE)]

cat("Importance columns found:", paste(importance_cols, collapse = ", "), "\n")
cat("Condition columns found:", paste(condition_cols, collapse = ", "), "\n\n")

# Clean data - remove incomplete responses
clean_data <- raw_data %>%
  filter(Finished == TRUE | Finished == "True" | Finished == 1) %>%
  filter(Status == "IP Address" | Status == 0) %>%
  select(ResponseId, all_of(importance_cols), all_of(condition_cols))

# Find the actual importance rating column (should be just one since it's between-subjects)
importance_col <- importance_cols[!grepl("_TEXT|_DO", importance_cols)][1]

# Find the condition/attribute column
condition_col <- condition_cols[!grepl("_TEXT|_DO", condition_cols)][1]

cat("Using importance column:", importance_col, "\n")
cat("Using condition column:", condition_col, "\n\n")

# Clean the data with proper column names
if(!is.na(importance_col) && !is.na(condition_col)) {
  clean_data <- clean_data %>%
    rename(
      importance = !!importance_col,
      attribute = !!condition_col
    ) %>%
    mutate(
      importance = as.numeric(importance),
      attribute = as.character(attribute)
    ) %>%
    filter(!is.na(importance) & !is.na(attribute))

  cat("Cleaned data: N =", nrow(clean_data), "\n\n")

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
      max = max(importance, na.rm = TRUE)
    ) %>%
    arrange(desc(mean))

  print(desc_stats)
  cat("\n")

  # Identify gender attribute (might be labeled as "Female", "Gender", "Female Protagonist", etc.)
  gender_labels <- c("Female", "Gender", "Female Protagonist", "female", "gender", "Woman", "woman")
  gender_attribute <- unique(clean_data$attribute)[
    tolower(unique(clean_data$attribute)) %in% tolower(gender_labels) |
    grepl("female|gender|woman", unique(clean_data$attribute), ignore.case = TRUE)
  ]

  if(length(gender_attribute) == 0) {
    cat("WARNING: Could not identify gender attribute. Using first attribute for comparison.\n")
    gender_attribute <- unique(clean_data$attribute)[1]
  } else {
    gender_attribute <- gender_attribute[1]
  }

  cat("Gender attribute identified as:", gender_attribute, "\n\n")

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
    cohen_result <- cohen.d(gender_data, attr_data)

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

  # Save results to CSV
  output_file <- "attribute_pretests/between/gender_biopic_between_results.csv"
  write.csv(display_results, output_file, row.names = FALSE)
  cat("\n\nResults saved to:", output_file, "\n")

} else {
  cat("ERROR: Could not identify importance or condition columns in the data.\n")
  cat("Please check the survey structure.\n")
  cat("Available columns:\n")
  print(names(raw_data))
}

cat("\n=================================================\n")
cat("ANALYSIS COMPLETE\n")
cat("=================================================\n")