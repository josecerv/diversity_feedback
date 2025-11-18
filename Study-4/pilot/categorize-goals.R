# Categorize Goals Responses into Three Buckets
# Overrepresented Treatment Group Only

library(dplyr)
library(stringr)

# Read the saved data
goals_data <- read.csv("goals_overrep_treatment.csv", stringsAsFactors = FALSE)

cat("Total responses:", nrow(goals_data), "\n\n")

# Initialize categories
goals_data$category <- NA_character_

# Convert to lowercase for easier matching
goals_lower <- tolower(goals_data$goals)

# BUCKET 1: Balancing toward men (picked a man + mentioned balance)
# Keywords: balance, equal, even, sex, gender distribution, mostly women, more women than men
balance_keywords <- c("balanc", "equal", "even", "sex", "imbalanc",
                      "more women than men", "mostly women", "had more women",
                      "obligated")

bucket1_indices <- c()
for(i in 1:length(goals_lower)) {
  if(goals_data$female_pick[i] == 0) {  # Only if picked a man
    if(any(str_detect(goals_lower[i], balance_keywords))) {
      bucket1_indices <- c(bucket1_indices, i)
      goals_data$category[i] <- "Bucket 1: Balancing (selected man)"
    }
  }
}

# BUCKET 2: Doubling down on women (picked a woman + explicitly mentioned wanting women)
# Keywords: wanted women, more women, female, mostly female, add another woman, woman as panelist
women_keywords <- c("wanted? more women", "add another woman", "wanted? a woman",
                    "wanted? (more )?female", "mostly female", "female majority",
                    "needed a female", "equalize.*female", "balance gender.*woman",
                    "chose.*female", "woman as.*panelist", "at least.*woman",
                    "to add another woman", "more of.*women", "another woman",
                    "all women", "women to gain")

bucket2_indices <- c()
for(i in 1:length(goals_lower)) {
  if(goals_data$female_pick[i] == 1) {  # Only if picked a woman
    if(any(str_detect(goals_lower[i], women_keywords))) {
      bucket2_indices <- c(bucket2_indices, i)
      goals_data$category[i] <- "Bucket 2: Doubling down (selected woman)"
    }
  }
}

# BUCKET 3: Reactive/defensive (claiming gender didn't matter)
# Keywords: did not consider gender, not about gender, gender did not, not looking at names/gender
defensive_keywords <- c("did not.*consider gender", "not.*about gender",
                       "gender did not", "nothing to do with gender",
                       "not.*looking at.*names", "wasn't.*looking at.*names",
                       "looked at.*compan.*not.*gender", "went by.*compan.*not",
                       "not.*gender representative", "gender.*not.*factor",
                       "not even consider", "never.*consider.*gender",
                       "regardless.*gender")

bucket3_indices <- c()
for(i in 1:length(goals_lower)) {
  if(is.na(goals_data$category[i])) {  # Not already categorized
    if(any(str_detect(goals_lower[i], defensive_keywords))) {
      bucket3_indices <- c(bucket3_indices, i)
      goals_data$category[i] <- "Bucket 3: Reactive (gender didn't matter)"
    }
  }
}

# Remaining responses
other_indices <- which(is.na(goals_data$category))
goals_data$category[other_indices] <- "Other/Unclear"

# Summary statistics
cat("=======================================================\n")
cat("CATEGORIZATION SUMMARY\n")
cat("=======================================================\n\n")

summary_table <- goals_data %>%
  group_by(category) %>%
  summarise(
    Count = n(),
    Percentage = round(n() / nrow(goals_data) * 100, 1),
    Picked_Women = sum(female_pick),
    Picked_Men = sum(female_pick == 0),
    .groups = "drop"
  ) %>%
  arrange(desc(Count))

print(summary_table)

cat("\n\n=======================================================\n")
cat("BUCKET 1: BALANCING (Selected Man to Balance Gender)\n")
cat("=======================================================\n")
cat("Count:", length(bucket1_indices), "out of 100 (",
    round(length(bucket1_indices)/100*100, 1), "%)\n\n")

cat("Examples:\n")
for(i in bucket1_indices[1:min(5, length(bucket1_indices))]) {
  cat("\n", goals_data$goals[i])
}

cat("\n\n=======================================================\n")
cat("BUCKET 2: DOUBLING DOWN (Selected Woman, Wanted Women)\n")
cat("=======================================================\n")
cat("Count:", length(bucket2_indices), "out of 100 (",
    round(length(bucket2_indices)/100*100, 1), "%)\n\n")

cat("Examples:\n")
for(i in bucket2_indices[1:min(5, length(bucket2_indices))]) {
  cat("\n", goals_data$goals[i])
}

cat("\n\n=======================================================\n")
cat("BUCKET 3: REACTIVE (Claimed Gender Didn't Matter)\n")
cat("=======================================================\n")
cat("Count:", length(bucket3_indices), "out of 100 (",
    round(length(bucket3_indices)/100*100, 1), "%)\n\n")

cat("Examples:\n")
for(i in bucket3_indices[1:min(5, length(bucket3_indices))]) {
  cat("\n", goals_data$goals[i])
}

cat("\n\n=======================================================\n")
cat("DETAILED BREAKDOWN\n")
cat("=======================================================\n\n")

# Bucket 1 - All responses
cat("--- BUCKET 1: ALL RESPONSES ---\n")
cat("(Balancing toward men after seeing feedback)\n\n")
for(i in bucket1_indices) {
  cat("Response", i, ":", goals_data$goals[i], "\n")
}

cat("\n\n--- BUCKET 2: ALL RESPONSES ---\n")
cat("(Doubling down on selecting women)\n\n")
for(i in bucket2_indices) {
  cat("Response", i, ":", goals_data$goals[i], "\n")
}

cat("\n\n--- BUCKET 3: ALL RESPONSES ---\n")
cat("(Reactive - claiming gender didn't matter)\n\n")
for(i in bucket3_indices) {
  cat("Response", i, ":", goals_data$goals[i], "\n")
}

# Save categorized data
write.csv(goals_data, "goals_categorized.csv", row.names = FALSE)
cat("\n\nCategorized data saved to: goals_categorized.csv\n")
