# Gender Moderation Analysis for Overrepresented Pool
# Test script to verify the fixed code works

library(dplyr)
library(tidyverse)
library(qualtRics)

# Set up Qualtrics API
qualtrics_api_credentials(api_key = "BJ6aDiEgtQihneCgtZycVLYjaj0ao9gYkdRkb1UZ",
                          base_url = "yul1.qualtrics.com",
                          install = F,
                          overwrite = T)

# Pull data
qual_data <- fetch_survey(surveyID='SV_eysJmstT7Zxvd2K',
                          label = T,
                          convert = F,
                          start_date = "2025-11-18",
                          force_request = T)

# Define women
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

# Filter to women pool with binary gender only
d_women_pool_binary <- d0 %>%
  filter(women_pool == 1, gender %in% c("Woman", "Man"))

cat("Gender Moderation Analysis - Overrepresented Pool\n")
cat("=================================================\n\n")
cat("Sample size (binary gender only):", nrow(d_women_pool_binary), "\n\n")

# Gender distribution
cat("Gender distribution:\n")
print(table(d_women_pool_binary$gender, d_women_pool_binary$treatment))
cat("\n\n")

# Interaction model
model_women_gender <- lm(female_pick ~ treatment * gender, data = d_women_pool_binary)

cat("INTERACTION MODEL: female_pick ~ treatment * gender\n")
cat("---------------------------------------------------\n")
print(summary(model_women_gender))
cat("\n\n")

# Simple slopes for Women
cat("SIMPLE SLOPES ANALYSIS\n")
cat("---------------------------------------------------\n\n")
cat("Effect for WOMEN participants:\n")
model_women_woman <- lm(female_pick ~ treatment,
                        data = d_women_pool_binary %>% filter(gender == "Woman"))
print(summary(model_women_woman))

effect_woman <- coef(model_women_woman)["treatment"]
se_woman <- summary(model_women_woman)$coefficients["treatment", "Std. Error"]
cat("\nTreatment effect for Women: b =", round(effect_woman, 4),
    ", SE =", round(se_woman, 4), "\n\n")

cat("\nEffect for MEN participants:\n")
model_women_man <- lm(female_pick ~ treatment,
                      data = d_women_pool_binary %>% filter(gender == "Man"))
print(summary(model_women_man))

effect_man <- coef(model_women_man)["treatment"]
se_man <- summary(model_women_man)$coefficients["treatment", "Std. Error"]
cat("\nTreatment effect for Men: b =", round(effect_man, 4),
    ", SE =", round(se_man, 4), "\n\n")

# Test difference
diff_effects <- effect_woman - effect_man
se_diff <- sqrt(se_woman^2 + se_man^2)
z_stat <- diff_effects / se_diff
p_value <- 2 * (1 - pnorm(abs(z_stat)))

cat("\nTEST OF DIFFERENCE IN TREATMENT EFFECTS\n")
cat("---------------------------------------------------\n")
cat("Difference (Women - Men):", round(diff_effects, 4), "\n")
cat("SE of difference:", round(se_diff, 4), "\n")
cat("Z-statistic:", round(z_stat, 4), "\n")
cat("P-value:", round(p_value, 4), "\n")
