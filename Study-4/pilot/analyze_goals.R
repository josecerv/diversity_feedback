# Load libraries
library(dplyr)
library(qualtRics)

# Pull directly from Qualtrics API
qual_data <- fetch_survey(surveyID='SV_eysJmstT7Zxvd2K',
                   label = TRUE,
                   convert = FALSE,
                   force_request = TRUE)

# Define all women leaders
all_women <- c("Jane Fraser (CEO of Citigroup)", "Oprah Winfrey (CEO of Oprah Winfrey Network)",
               "Delphine Arnault (CEO of Christian Dior)", "Michelle Buck (CEO of The Hershey Company)",
               "Mary Barra (CEO of General Motors)", "Rosalind Brewer (CEO of Walgreens)",
               "Anne Wojcicki (CEO of 23andMe)", "Arianna Huffington (Co-founder of Huffington Post)",
               "Karen Lynch (CEO of CVS Health)", "Tricia Griffith (CEO of Progressive)",
               "Tory Burch (Founder of Tory Burch)", "Carol Tome (CEO of UPS)",
               "Leah Busque (Founder of TaskRabbit)", "Whitney Wolfe Herd (Founder of Bumble)",
               "Corie Barry (CEO of Best Buy)", "Melanie Perkins (Founder of Canva)",
               "Kathy Warden (CEO of Northrupp Grumman)", "Julia Hartz (Founder of EventBrite)",
               "Safra Katz (CEO of Oracle)")

# Process data
d0 <- qual_data %>%
  filter(!is.na(`choice-7`), !is.na(PROLIFIC_PID), Finished==1) %>%
  mutate(
    pool = tolower(pool),
    cond = tolower(cond)
  )

# Check if goals variable exists
if("goals" %in% names(qual_data)) {
  cat("'goals' variable found!\n\n")

  # Filter for women overrepresented condition
  goals_women_pool <- qual_data %>%
    filter(!is.na(`choice-7`), !is.na(PROLIFIC_PID), Finished==1) %>%
    mutate(pool = tolower(pool)) %>%
    filter(pool == "women") %>%
    select(pool, cond, goals) %>%
    filter(!is.na(goals) & goals != "")

  cat("==========================================================\n")
  cat("QUALITATIVE RESPONSES: GOALS\n")
  cat("Women Overrepresented Condition (Women Pool)\n")
  cat("==========================================================\n\n")
  cat("Total responses:", nrow(goals_women_pool), "\n\n")

  for(i in 1:nrow(goals_women_pool)) {
    cat("--- Response", i, "(", goals_women_pool$cond[i], ") ---\n")
    cat(goals_women_pool$goals[i], "\n\n")
  }

} else {
  cat("Variable 'goals' not found. Available variables:\n")
  cat(paste(names(qual_data), collapse="\n"))
}
