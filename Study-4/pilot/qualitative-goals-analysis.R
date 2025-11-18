# Qualitative Analysis of "goals" Column
# Focus: Overrepresented Treatment Group (Women Pool + Treatment Condition)
# Author: Analysis Script
# Date: 2025-11-18

# Load libraries
library(dplyr)
library(tidyverse)
library(qualtRics)

# Set up Qualtrics API
qualtrics_api_credentials(api_key = "BJ6aDiEgtQihneCgtZycVLYjaj0ao9gYkdRkb1UZ",
                          base_url = "yul1.qualtrics.com",
                          install = F,
                          overwrite = T)

# Pull data from Qualtrics
qual_data <- fetch_survey(surveyID='SV_eysJmstT7Zxvd2K',
                          label = T,
                          convert = F,
                          start_date = "2025-11-18",
                          force_request = T)

# Define women CEOs/Founders
women <- c("Whitney Wolfe Herd (Founder of Bumble)",
           "Leah Busque (Founder of TaskRabbit)",
           "Melanie Perkins (Founder of Canva)",
           "Julia Hartz (Co-founder of EventBrite)",
           "Arianna Huffington (Co-founder of Huffington Post)",
           "Anne Wojcicki (CEO of 23andMe)",
           "Corie Barry (CEO of Best Buy)",
           "Safra Katz (CEO of Oracle)",
           "Mary Barra (CEO of General Motors)",
           "Tory Burch (Founder of Tory Burch)",
           "Sara Blakely (Founder of Spanx)",
           "Oprah Winfrey (Founder of OWN)",
           "Bobbi Brown (Founder of Bobbi Brown Cosmetics)",
           "Jane Fraser (CEO of Citigroup)",
           "Rosalind Brewer (CEO of Walgreens)",
           "Karen Lynch (CEO of CVS Health)",
           "Carol Tomé (CEO of UPS)",
           "Indra Nooyi (Former CEO of PepsiCo)")

# Process data
d0 <- qual_data %>%
  filter(!is.na(`choice-7`), !is.na(PROLIFIC_PID), Finished==1) %>%
  mutate(
    treatment = case_when(cond == "treat" ~ 1, TRUE ~ 0),
    women_pool = case_when(pool == "women" ~ 1, TRUE ~ 0),
    female_pick = case_when(`choice-7` %in% women ~ 1, TRUE ~ 0)
  )

# Filter for overrepresented treatment group
# Overrepresented = women pool (women_pool == 1)
# Treatment = treatment condition (treatment == 1)
overrep_treat <- d0 %>%
  filter(women_pool == 1, treatment == 1)

cat("=======================================================\n")
cat("QUALITATIVE ANALYSIS: GOALS COLUMN\n")
cat("OVERREPRESENTED POOL + TREATMENT CONDITION\n")
cat("=======================================================\n\n")

cat("Sample Size: N =", nrow(overrep_treat), "\n\n")

# Check if goals column exists
if(!"goals" %in% colnames(overrep_treat)) {
  cat("ERROR: 'goals' column not found in data.\n")
  cat("\nAvailable columns containing 'goal':\n")
  goal_cols <- grep("goal", colnames(overrep_treat), ignore.case = TRUE, value = TRUE)
  print(goal_cols)

  cat("\n\nAll available columns:\n")
  print(colnames(overrep_treat))
} else {
  # Analyze goals column
  cat("--- GOALS DATA OVERVIEW ---\n\n")

  # Remove NA responses
  goals_data <- overrep_treat %>%
    filter(!is.na(goals), goals != "", nchar(as.character(goals)) > 0)

  cat("Responses with goals data:", nrow(goals_data), "/", nrow(overrep_treat), "\n")
  cat("Missing/empty goals:", nrow(overrep_treat) - nrow(goals_data), "\n\n")

  # Show all responses
  cat("=======================================================\n")
  cat("ALL GOALS RESPONSES\n")
  cat("=======================================================\n\n")

  for(i in 1:nrow(goals_data)) {
    cat("Response", i, ":\n")
    cat("Female pick:", goals_data$female_pick[i], "\n")
    cat("Final choice:", goals_data$`choice-7`[i], "\n")
    cat("Goals:\n")
    cat(as.character(goals_data$goals[i]), "\n")
    cat("\n---\n\n")
  }

  # Analysis by final selection
  cat("=======================================================\n")
  cat("BREAKDOWN BY FINAL SELECTION (FEMALE VS MALE)\n")
  cat("=======================================================\n\n")

  # Those who picked women
  picked_women <- goals_data %>% filter(female_pick == 1)
  cat("Participants who picked a WOMAN (N =", nrow(picked_women), "):\n\n")
  if(nrow(picked_women) > 0) {
    for(i in 1:nrow(picked_women)) {
      cat("  Response", i, ":\n")
      cat("  Choice:", picked_women$`choice-7`[i], "\n")
      cat("  ", as.character(picked_women$goals[i]), "\n\n")
    }
  }

  cat("\n")

  # Those who picked men
  picked_men <- goals_data %>% filter(female_pick == 0)
  cat("Participants who picked a MAN (N =", nrow(picked_men), "):\n\n")
  if(nrow(picked_men) > 0) {
    for(i in 1:nrow(picked_men)) {
      cat("  Response", i, ":\n")
      cat("  Choice:", picked_men$`choice-7`[i], "\n")
      cat("  ", as.character(picked_men$goals[i]), "\n\n")
    }
  }

  # Keyword analysis
  cat("\n=======================================================\n")
  cat("KEYWORD ANALYSIS\n")
  cat("=======================================================\n\n")

  # Convert to lowercase for analysis
  goals_text <- tolower(as.character(goals_data$goals))

  # Keywords to search for
  keywords <- c("diversity", "gender", "women", "female", "balance", "representation",
                "fairness", "equal", "quota", "bias", "discrimination",
                "merit", "qualified", "best", "experience", "skill",
                "company", "success", "innovation", "performance")

  cat("Keyword frequencies:\n\n")
  for(kw in keywords) {
    count <- sum(grepl(kw, goals_text))
    if(count > 0) {
      cat(sprintf("  %-20s: %d (%.1f%%)\n", kw, count, 100*count/length(goals_text)))
    }
  }

  # Save to CSV for further analysis
  write.csv(goals_data %>% select(PROLIFIC_PID, cond, pool, female_pick, `choice-7`, goals),
            "goals_overrep_treatment.csv", row.names = FALSE)

  cat("\n\nData saved to: goals_overrep_treatment.csv\n")
}
