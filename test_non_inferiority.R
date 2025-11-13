# Test non-inferiority with different margins
library(dplyr)
library(qualtRics)
library(TOSTER)

# Set up Qualtrics API
qualtrics_api_credentials(
  api_key = "BJ6aDiEgtQihneCgtZycVLYjaj0ao9gYkdRkb1UZ",
  base_url = "yul1.qualtrics.com",
  install = FALSE,
  overwrite = TRUE
)

# Download data
data_raw <- fetch_survey(surveyID = "SV_cASPdJCJOBdOu1M", verbose = FALSE)

# Clean
clean_importance <- function(x) {
  x <- as.character(x)
  x <- gsub("Not at all important|Very important", "", x)
  x <- gsub("[^0-9]", "", x)
  return(as.numeric(x))
}

data <- data_raw %>%
  mutate(date = as.Date(StartDate)) %>%
  filter(date == as.Date("2025-09-26"),
         Finished == TRUE,
         !is.na(attribute),
         !is.na(importance)) %>%
  mutate(importance_clean = clean_importance(importance),
         attribute_clean = trimws(attribute)) %>%
  filter(!is.na(importance_clean))

# Key attributes to test
key_attrs <- c(
  "wrote  books, poems or essays spanning multiple genres.",
  "wrote at least one book that sold over 1 million copies.",
  "wrote at least one book that remained in continuous print for over 50 years."
)

cat("\n=== NON-INFERIORITY TESTS ===\n")
cat("Testing: Other attributes are NOT LESS important than gender\n\n")

# Test with different margins
margins <- c(0.3, 0.5, 0.8)

for(margin in margins) {
  cat("\n", strrep("=", 70), "\n")
  cat("MARGIN: Cohen's d = ", margin, "\n")
  cat(strrep("=", 70), "\n\n")

  for(attr in key_attrs) {
    # Get data
    gender_data <- data %>%
      filter(attribute_clean == "were women.") %>%
      pull(importance_clean)

    other_data <- data %>%
      filter(attribute_clean == attr) %>%
      pull(importance_clean)

    # Calculate stats
    m_gender <- mean(gender_data)
    m_other <- mean(other_data)
    sd_gender <- sd(gender_data)
    sd_other <- sd(other_data)
    n_gender <- length(gender_data)
    n_other <- length(other_data)

    # Pooled SD
    pooled_sd <- sqrt(((n_gender-1)*sd_gender^2 + (n_other-1)*sd_other^2) /
                      (n_gender + n_other - 2))

    # Cohen's d
    cohen_d <- (m_gender - m_other) / pooled_sd

    # ONE-SIDED TEST: Test if difference is NOT less than -margin
    # This is equivalent to testing if lower bound > -margin
    t_stat <- ((m_gender - m_other) - (-margin * pooled_sd)) /
              (pooled_sd * sqrt(1/n_gender + 1/n_other))
    df <- n_gender + n_other - 2
    p_value <- pt(t_stat, df, lower.tail = FALSE)

    # Display
    cat("Attribute:", gsub("wrote at least one book that |wrote  books, poems or essays ", "", attr), "\n")
    cat("  Gender mean:", round(m_gender, 2), "(n=", n_gender, ")\n", sep="")
    cat("  Other mean: ", round(m_other, 2), "(n=", n_other, ")\n", sep="")
    cat("  Cohen's d:  ", round(cohen_d, 3), "\n", sep="")
    cat("  Non-inferiority p:", round(p_value, 4))

    if(p_value < 0.05) {
      cat(" ✓ PASSES - Attribute is NOT inferior to gender!\n")
    } else {
      cat(" ✗ FAILS\n")
    }
    cat("\n")
  }
}

cat("\n", strrep("=", 70), "\n")
cat("INTERPRETATION:\n")
cat(strrep("=", 70), "\n\n")
cat("Non-inferiority test asks: 'Is the other attribute at least as\n")
cat("important as gender (within our tolerance margin)?'\n\n")
cat("If p < 0.05: YES - we can conclude non-inferiority\n")
cat("If p > 0.05: NO - we cannot conclude non-inferiority\n\n")
cat("Margin = tolerance for how much less important we'd accept\n")
cat("  d = 0.3 = small difference (strict)\n")
cat("  d = 0.5 = medium difference (moderate)\n")
cat("  d = 0.8 = large difference (lenient)\n")
