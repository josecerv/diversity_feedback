# Check which attributes are more vs less important than gender
library(dplyr)
library(qualtRics)

# Set up Qualtrics API credentials
qualtrics_api_credentials(
  api_key = "BJ6aDiEgtQihneCgtZycVLYjaj0ao9gYkdRkb1UZ",
  base_url = "yul1.qualtrics.com",
  install = FALSE,
  overwrite = TRUE
)

# Download the data
data_raw <- fetch_survey(
  surveyID = "SV_cASPdJCJOBdOu1M",
  verbose = FALSE
)

# Function to clean importance values
clean_importance <- function(x) {
  x <- as.character(x)
  x <- gsub("Not at all important", "", x)
  x <- gsub("Very important", "", x)
  x <- gsub("[^0-9]", "", x)
  x <- as.numeric(x)
  return(x)
}

# Clean and calculate means
data <- data_raw %>%
  mutate(date = as.Date(StartDate)) %>%
  filter(date == as.Date("2025-09-26"))

attribute_means <- data %>%
  filter(Finished == TRUE,
         !is.na(attribute),
         !is.na(importance)) %>%
  mutate(importance_clean = clean_importance(importance),
         attribute_clean = trimws(attribute)) %>%
  filter(!is.na(importance_clean)) %>%
  group_by(attribute_clean) %>%
  summarise(
    n = n(),
    mean = mean(importance_clean, na.rm = TRUE),
    sd = sd(importance_clean, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(mean))

# Print all attributes ranked by importance
cat("\n=== ALL ATTRIBUTES RANKED BY IMPORTANCE ===\n\n")
gender_mean <- attribute_means$mean[attribute_means$attribute_clean == "were women."]
race_mean <- attribute_means$mean[attribute_means$attribute_clean == "were racial minorities."]

cat("Gender mean:", round(gender_mean, 2), "\n")
cat("Race mean:", round(race_mean, 2), "\n\n")

for(i in 1:nrow(attribute_means)) {
  cat(sprintf("%2d. [%.2f] (n=%d) %s\n",
              i,
              attribute_means$mean[i],
              attribute_means$n[i],
              attribute_means$attribute_clean[i]))

  if(attribute_means$attribute_clean[i] == "were women.") {
    cat("    ^^^^^^^^^ GENDER (baseline) ^^^^^^^^^\n\n")
  }
  if(attribute_means$attribute_clean[i] == "were racial minorities.") {
    cat("    ^^^^^^^^^ RACE (baseline) ^^^^^^^^^\n\n")
  }
}

# Identify attributes MORE important than gender (numerically)
cat("\n\n=== ATTRIBUTES MORE IMPORTANT THAN GENDER (M=", round(gender_mean, 2), ") ===\n\n")
more_important <- attribute_means %>%
  filter(mean > gender_mean,
         attribute_clean != "were racial minorities.")

for(i in 1:nrow(more_important)) {
  cat(sprintf("  %.2f  %s\n",
              more_important$mean[i],
              more_important$attribute_clean[i]))
}

# Identify attributes LESS important than gender
cat("\n\n=== ATTRIBUTES LESS IMPORTANT THAN GENDER (M=", round(gender_mean, 2), ") ===\n\n")
less_important <- attribute_means %>%
  filter(mean < gender_mean,
         attribute_clean != "were racial minorities.")

for(i in 1:nrow(less_important)) {
  cat(sprintf("  %.2f  %s\n",
              less_important$mean[i],
              less_important$attribute_clean[i]))
}

cat("\n\n=== SUMMARY ===\n")
cat("Attributes MORE important than gender:", nrow(more_important), "\n")
cat("Attributes LESS important than gender:", nrow(less_important), "\n")
cat("Gender rank:", which(attribute_means$attribute_clean == "were women."), "out of", nrow(attribute_means), "\n")
