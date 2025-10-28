# Quick analysis to answer specific questions
library(dplyr)

# Read the data
d0 <- read.csv('pilot-study.csv', check.names = FALSE)

cat("===========================================================\n")
cat("QUESTION 1: Base rates of female selection across conditions\n")
cat("===========================================================\n\n")

cat("Overall Statistics:\n")
cat("Mean number of women selected (out of 6):", round(mean(d0$base_gender), 2), "\n")
cat("Mean percentage of women selected:", round(mean(d0$base_gender)/6 * 100, 2), "%\n\n")

cat("By Base Condition:\n")
base_rates_summary <- d0 |>
  group_by(base_condition) |>
  summarise(
    n = n(),
    mean_count = round(mean(base_gender), 2),
    mean_pct = round(mean(base_gender)/6 * 100, 2),
    sd_count = round(sd(base_gender), 2)
  )
print(base_rates_summary)

cat("\n--- Checking if High Base leads to >50% women selected ---\n")
d0_high_summary <- d0 |>
  filter(base_condition == "high") |>
  summarise(
    pct_with_majority_women = round(mean(base_gender > 3) * 100, 2),
    pct_with_half_or_more = round(mean(base_gender >= 3) * 100, 2)
  )

cat("In HIGH base condition:\n")
cat("  % with >50% women (4+ out of 6):", d0_high_summary$pct_with_majority_women, "%\n")
cat("  % with 50% or more women (3+ out of 6):", d0_high_summary$pct_with_half_or_more, "%\n\n")

cat("\n===========================================================\n")
cat("QUESTION 2: What feedback is shown to participants?\n")
cat("===========================================================\n\n")

feedback_summary <- d0 |>
  summarise(
    pct_gender = round(mean(gender_feedback) * 100, 1),
    pct_poets = round(mean(poets_shown) * 100, 1),
    pct_oldies = round(mean(oldies_shown) * 100, 1),
    pct_books = round(mean(books_shown) * 100, 1)
  )

cat("Feedback Distribution:\n")
cat("  Gender feedback (treatment):", feedback_summary$pct_gender, "%\n")
cat("  Poets feedback shown:", feedback_summary$pct_poets, "%\n")
cat("  Oldies feedback shown:", feedback_summary$pct_oldies, "%\n")
cat("  Books feedback shown:", feedback_summary$pct_books, "%\n\n")

cat("\n===========================================================\n")
cat("QUESTION 3: Base rates of ALL attributes by condition\n")
cat("===========================================================\n\n")

by_base <- d0 |>
  group_by(base_condition) |>
  summarise(
    n = n(),
    women_pct = round(mean(base_gender)/6 * 100, 1),
    poets_pct = round(mean(base_poets)/6 * 100, 1),
    oldies_pct = round(mean(base_oldies)/6 * 100, 1),
    books_pct = round(mean(base_books)/6 * 100, 1)
  )

cat("Initial selection rates (% of first 6 picks):\n")
print(by_base)

cat("\n7th Selection Rates by Attribute:\n")
seventh_selection <- d0 |>
  group_by(base_condition) |>
  summarise(
    n = n(),
    pct_select_woman = round(mean(female_pick) * 100, 1),
    pct_select_poet = round(mean(poets_pick) * 100, 1),
    pct_select_oldies = round(mean(oldies_pick) * 100, 1),
    pct_select_books = round(mean(books_pick) * 100, 1)
  )
print(seventh_selection)

cat("\n===========================================================\n")
cat("QUESTION 4: Which attribute wins when multiple are underrepresented?\n")
cat("===========================================================\n\n")

# Create underrepresentation indicators
d0 <- d0 |>
  mutate(
    women_underrep = as.numeric(base_gender < 3),
    poets_underrep = as.numeric(base_poets < 3),
    oldies_underrep = as.numeric(base_oldies < 3),
    books_underrep = as.numeric(base_books < 3),
    num_underrep = women_underrep + poets_underrep + oldies_underrep + books_underrep,
    most_underrep = case_when(
      base_gender < pmin(base_poets, base_oldies, base_books) ~ "women",
      base_poets < pmin(base_gender, base_oldies, base_books) ~ "poets",
      base_oldies < pmin(base_gender, base_poets, base_books) ~ "oldies",
      base_books < pmin(base_gender, base_poets, base_oldies) ~ "books",
      TRUE ~ "tie"
    )
  )

cat("Distribution of number of underrepresented attributes:\n")
underrep_dist <- d0 |>
  group_by(num_underrep) |>
  summarise(n = n(), pct = round(n/nrow(d0) * 100, 1))
print(underrep_dist)

cat("\nWhen WOMEN are most underrepresented, 7th selection:\n")
d0 |> filter(most_underrep == "women") |>
  summarise(
    n = n(),
    pct_woman = round(mean(female_pick) * 100, 1),
    pct_poet = round(mean(poets_pick) * 100, 1),
    pct_oldies = round(mean(oldies_pick) * 100, 1),
    pct_books = round(mean(books_pick) * 100, 1)
  ) |> print()

cat("\nWhen POETS are most underrepresented, 7th selection:\n")
d0 |> filter(most_underrep == "poets") |>
  summarise(
    n = n(),
    pct_woman = round(mean(female_pick) * 100, 1),
    pct_poet = round(mean(poets_pick) * 100, 1),
    pct_oldies = round(mean(oldies_pick) * 100, 1),
    pct_books = round(mean(books_pick) * 100, 1)
  ) |> print()

cat("\nWhen OLDIES are most underrepresented, 7th selection:\n")
d0 |> filter(most_underrep == "oldies") |>
  summarise(
    n = n(),
    pct_woman = round(mean(female_pick) * 100, 1),
    pct_poet = round(mean(poets_pick) * 100, 1),
    pct_oldies = round(mean(oldies_pick) * 100, 1),
    pct_books = round(mean(books_pick) * 100, 1)
  ) |> print()

cat("\nWhen BOOKS are most underrepresented, 7th selection:\n")
d0 |> filter(most_underrep == "books") |>
  summarise(
    n = n(),
    pct_woman = round(mean(female_pick) * 100, 1),
    pct_poet = round(mean(poets_pick) * 100, 1),
    pct_oldies = round(mean(oldies_pick) * 100, 1),
    pct_books = round(mean(books_pick) * 100, 1)
  ) |> print()

cat("\n===========================================================\n")
cat("SUMMARY: Do people select the most underrepresented attribute?\n")
cat("===========================================================\n\n")

# Calculate "hit rate" - when attribute X is most underrepresented,
# do people select attribute X for 7th pick?
women_hit <- d0 |> filter(most_underrep == "women") |> summarise(hit = mean(female_pick))
poets_hit <- d0 |> filter(most_underrep == "poets") |> summarise(hit = mean(poets_pick))
oldies_hit <- d0 |> filter(most_underrep == "oldies") |> summarise(hit = mean(oldies_pick))
books_hit <- d0 |> filter(most_underrep == "books") |> summarise(hit = mean(books_pick))

cat("When X is most underrepresented, % who select X for 7th pick:\n")
cat("  Women:", round(women_hit$hit * 100, 1), "%\n")
cat("  Poets:", round(poets_hit$hit * 100, 1), "%\n")
cat("  Oldies:", round(oldies_hit$hit * 100, 1), "%\n")
cat("  Books:", round(books_hit$hit * 100, 1), "%\n")
