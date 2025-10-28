# Analysis: Do participants select the most underrepresented attribute?
# And how often are women the most underrepresented?

library(dplyr)
library(ggplot2)
library(tidyr)
library(lmtest)
library(sandwich)

# Read the data
d0 <- read.csv('pilot-study.csv', check.names = FALSE)

# Robust summary function
robust_summary <- function(model) {
    robust_se <- sqrt(diag(vcovHC(model, type = "HC3")))
    model_summary <- summary(model)
    model_summary$coefficients[, "Std. Error"] <- robust_se
    model_summary$coefficients[, "t value"] <- model_summary$coefficients[, "Estimate"] / robust_se
    model_summary$coefficients[, "Pr(>|t|)"] <- 2 * pt(-abs(model_summary$coefficients[, "t value"]), df = model_summary$df[2], lower.tail = TRUE)
    return(model_summary)
}

cat("===========================================================\n")
cat("QUESTION 1: Do participants select the MOST underrepresented attribute?\n")
cat("===========================================================\n\n")

# Identify which attribute is most underrepresented for each participant
d0 <- d0 %>%
  mutate(
    # Create underrepresentation indicators
    women_underrep = as.numeric(base_gender < 3),
    poets_underrep = as.numeric(base_poets < 3),
    oldies_underrep = as.numeric(base_oldies < 3),
    books_underrep = as.numeric(base_books < 3),
    num_underrep = women_underrep + poets_underrep + oldies_underrep + books_underrep,

    # Identify THE MOST underrepresented (lowest count)
    min_count = pmin(base_gender, base_poets, base_oldies, base_books),

    # Count how many attributes are tied for most underrepresented
    num_at_min = (base_gender == min_count) +
                 (base_poets == min_count) +
                 (base_oldies == min_count) +
                 (base_books == min_count),

    # Identify which specific attribute(s) are most underrepresented
    women_most_underrep = as.numeric(base_gender == min_count),
    poets_most_underrep = as.numeric(base_poets == min_count),
    oldies_most_underrep = as.numeric(base_oldies == min_count),
    books_most_underrep = as.numeric(base_books == min_count),

    # Create a categorical variable for the single most underrepresented
    # (if tied, we'll handle that separately)
    most_underrep_attr = case_when(
      num_at_min > 1 ~ "tie",
      base_gender < pmin(base_poets, base_oldies, base_books) ~ "women",
      base_poets < pmin(base_gender, base_oldies, base_books) ~ "poets",
      base_oldies < pmin(base_gender, base_poets, base_books) ~ "oldies",
      base_books < pmin(base_gender, base_poets, base_oldies) ~ "books",
      TRUE ~ "tie"
    ),

    # Did they select the most underrepresented attribute?
    selected_most_underrep = case_when(
      most_underrep_attr == "women" ~ as.numeric(female_pick == 1),
      most_underrep_attr == "poets" ~ as.numeric(poets_pick == 1),
      most_underrep_attr == "oldies" ~ as.numeric(oldies_pick == 1),
      most_underrep_attr == "books" ~ as.numeric(books_pick == 1),
      most_underrep_attr == "tie" ~ NA_real_
    ),

    # Did they select ANY of the tied most underrepresented attributes?
    selected_any_most_underrep = case_when(
      num_at_min == 1 ~ selected_most_underrep,
      num_at_min > 1 ~ as.numeric(
        (women_most_underrep == 1 & female_pick == 1) |
        (poets_most_underrep == 1 & poets_pick == 1) |
        (oldies_most_underrep == 1 & oldies_pick == 1) |
        (books_most_underrep == 1 & books_pick == 1)
      ),
      TRUE ~ NA_real_
    )
  )

cat("--- Distribution of most underrepresented attribute ---\n")
most_underrep_dist <- d0 %>%
  group_by(most_underrep_attr) %>%
  summarise(n = n(), pct = round(n/nrow(d0) * 100, 1))
print(most_underrep_dist)

cat("\n--- Distribution of ties (how many attributes at minimum) ---\n")
tie_dist <- d0 %>%
  group_by(num_at_min) %>%
  summarise(n = n(), pct = round(n/nrow(d0) * 100, 1))
print(tie_dist)

cat("\n--- Overall: Did participants select the MOST underrepresented attribute? ---\n")
cat("(Excluding ties)\n")
overall_hit_rate <- d0 %>%
  filter(!is.na(selected_most_underrep)) %>%
  summarise(
    n = n(),
    n_selected = sum(selected_most_underrep),
    hit_rate = round(mean(selected_most_underrep) * 100, 1)
  )
print(overall_hit_rate)

cat("\n--- Including ties (selected ANY of the most underrepresented) ---\n")
overall_hit_rate_any <- d0 %>%
  filter(!is.na(selected_any_most_underrep)) %>%
  summarise(
    n = n(),
    n_selected = sum(selected_any_most_underrep),
    hit_rate = round(mean(selected_any_most_underrep) * 100, 1)
  )
print(overall_hit_rate_any)

cat("\n--- Hit rates by which attribute is most underrepresented ---\n")
hit_rates_by_attr <- d0 %>%
  filter(most_underrep_attr != "tie") %>%
  group_by(most_underrep_attr) %>%
  summarise(
    n = n(),
    n_selected = sum(selected_most_underrep),
    hit_rate = round(mean(selected_most_underrep) * 100, 1)
  ) %>%
  arrange(desc(hit_rate))
print(hit_rates_by_attr)

cat("\n\n===========================================================\n")
cat("QUESTION 2: How often are WOMEN the most underrepresented?\n")
cat("===========================================================\n\n")

cat("--- Overall (all base conditions) ---\n")
women_most_overall <- d0 %>%
  summarise(
    n = n(),
    n_women_most = sum(most_underrep_attr == "women"),
    pct_women_most = round(mean(most_underrep_attr == "women") * 100, 1),
    n_women_tied = sum(women_most_underrep == 1 & num_at_min > 1),
    pct_women_tied = round(mean(women_most_underrep == 1 & num_at_min > 1) * 100, 1)
  )
cat("Women are THE MOST underrepresented (sole minimum):", women_most_overall$pct_women_most, "% (n=", women_most_overall$n_women_most, ")\n")
cat("Women are TIED for most underrepresented:", women_most_overall$pct_women_tied, "% (n=", women_most_overall$n_women_tied, ")\n")

cat("\n--- By Base Condition ---\n")
women_most_by_base <- d0 %>%
  group_by(base_condition) %>%
  summarise(
    n = n(),
    n_women_most = sum(most_underrep_attr == "women"),
    pct_women_most = round(mean(most_underrep_attr == "women") * 100, 1),
    n_women_tied = sum(women_most_underrep == 1 & num_at_min > 1),
    pct_women_tied = round(mean(women_most_underrep == 1 & num_at_min > 1) * 100, 1),
    mean_base_gender = round(mean(base_gender), 2)
  ) %>%
  arrange(base_condition)
print(women_most_by_base)

cat("\n--- Distribution of minimum counts by base condition ---\n")
min_counts_by_base <- d0 %>%
  group_by(base_condition, min_count) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(base_condition) %>%
  mutate(pct = round(n/sum(n) * 100, 1)) %>%
  arrange(base_condition, min_count)
print(min_counts_by_base)

cat("\n\n===========================================================\n")
cat("ANALYSIS 3: Does TREATMENT affect selection of most underrepresented?\n")
cat("===========================================================\n\n")

cat("--- Overall: Treatment effect on selecting most underrepresented ---\n")
treatment_overall <- d0 %>%
  filter(!is.na(selected_most_underrep)) %>%
  group_by(gender_feedback) %>%
  summarise(
    n = n(),
    hit_rate = round(mean(selected_most_underrep) * 100, 1)
  )
print(treatment_overall)

cat("\n--- Regression: selected_most_underrep ~ gender_feedback ---\n")
r_underrep <- lm(selected_most_underrep ~ gender_feedback, data = d0)
print(robust_summary(r_underrep))

cat("\n--- By Base Condition ---\n")
treatment_by_base <- d0 %>%
  filter(!is.na(selected_most_underrep)) %>%
  group_by(base_condition, gender_feedback) %>%
  summarise(
    n = n(),
    hit_rate = round(mean(selected_most_underrep) * 100, 1),
    .groups = "drop"
  ) %>%
  arrange(base_condition, gender_feedback)
print(treatment_by_base)

# Interaction model
cat("\n--- Interaction: selected_most_underrep ~ gender_feedback * base_condition ---\n")
d0$base_condition_factor <- factor(d0$base_condition, levels = c("low", "med", "high"))
r_underrep_interact <- lm(selected_most_underrep ~ gender_feedback * base_condition_factor, data = d0)
print(robust_summary(r_underrep_interact))

cat("\n\n===========================================================\n")
cat("ANALYSIS 4: When WOMEN are most underrepresented, does gender feedback help?\n")
cat("===========================================================\n\n")

# Filter to cases where women are most underrepresented
d0_women_most <- d0 %>% filter(most_underrep_attr == "women")

cat("Sample: When women are THE MOST underrepresented (n=", nrow(d0_women_most), ")\n\n")

cat("--- Selection of women by treatment ---\n")
women_most_treatment <- d0_women_most %>%
  group_by(gender_feedback) %>%
  summarise(
    n = n(),
    pct_select_woman = round(mean(female_pick) * 100, 1)
  )
print(women_most_treatment)

cat("\n--- Regression: female_pick ~ gender_feedback (when women most underrep) ---\n")
r_women_most <- lm(female_pick ~ gender_feedback, data = d0_women_most)
print(robust_summary(r_women_most))

cat("\n--- By Base Condition ---\n")
women_most_by_base_treat <- d0_women_most %>%
  group_by(base_condition, gender_feedback) %>%
  summarise(
    n = n(),
    pct_select_woman = round(mean(female_pick) * 100, 1),
    .groups = "drop"
  ) %>%
  arrange(base_condition, gender_feedback)
print(women_most_by_base_treat)

cat("\n\n===========================================================\n")
cat("ANALYSIS 5: Comparing 'correcting underrepresentation' across attributes\n")
cat("===========================================================\n\n")

# For each attribute, when it's most underrepresented, what % select it?
cat("When attribute X is THE MOST underrepresented, % who select X:\n")
correction_rates <- data.frame(
  Attribute = c("Women", "Poets", "Oldies", "Books"),
  N = c(
    sum(d0$most_underrep_attr == "women"),
    sum(d0$most_underrep_attr == "poets"),
    sum(d0$most_underrep_attr == "oldies"),
    sum(d0$most_underrep_attr == "books")
  ),
  Hit_Rate = c(
    round(mean(d0$female_pick[d0$most_underrep_attr == "women"]) * 100, 1),
    round(mean(d0$poets_pick[d0$most_underrep_attr == "poets"]) * 100, 1),
    round(mean(d0$oldies_pick[d0$most_underrep_attr == "oldies"]) * 100, 1),
    round(mean(d0$books_pick[d0$most_underrep_attr == "books"]) * 100, 1)
  )
) %>%
  arrange(desc(Hit_Rate))
print(correction_rates)

cat("\n--- Statistical test: Are these rates different? ---\n")
# Chi-square test
correction_data <- d0 %>%
  filter(most_underrep_attr != "tie") %>%
  select(most_underrep_attr, selected_most_underrep)

chisq_result <- chisq.test(table(correction_data$most_underrep_attr, correction_data$selected_most_underrep))
print(chisq_result)

cat("\n\n===========================================================\n")
cat("CREATING VISUALIZATION\n")
cat("===========================================================\n\n")

# Visualization 1: Distribution of most underrepresented attribute by base condition
plot_data_dist <- d0 %>%
  filter(most_underrep_attr != "tie") %>%
  group_by(base_condition, most_underrep_attr) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(base_condition) %>%
  mutate(pct = n/sum(n) * 100)

p_dist <- ggplot(plot_data_dist, aes(x = factor(base_condition, levels = c("low", "med", "high")),
                                       y = pct, fill = most_underrep_attr)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(aes(label = paste0(round(pct, 1), "%\n(n=", n, ")")),
            position = position_dodge(width = 0.7), vjust = -0.3, size = 3.5) +
  scale_fill_manual(values = c("women" = "#990000", "poets" = "#FF6B35",
                                "oldies" = "#4ECDC4", "books" = "#011F5B"),
                    labels = c("women" = "Women", "poets" = "Poets",
                              "oldies" = "1800s-born", "books" = "10+ Books")) +
  scale_x_discrete(labels = c("low" = "Low\n(3 authors)", "med" = "Medium\n(6 authors)",
                              "high" = "High\n(9 authors)")) +
  labs(
    x = "Base Condition",
    y = "% of Participants",
    title = "Which Attribute is Most Underrepresented?",
    subtitle = "(Excludes ties; based on first 6 selections)",
    fill = "Most Underrepresented\nAttribute"
  ) +
  theme_bw() +
  theme(
    legend.position = "right",
    legend.text = element_text(size = 11),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 11),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5),
    panel.grid.minor = element_blank()
  ) +
  ylim(0, 55)

ggsave("Figure-Most-Underrepresented-Distribution.pdf", plot = p_dist,
       width = 10, height = 7, device = cairo_pdf)
cat("Saved: Figure-Most-Underrepresented-Distribution.pdf\n")

# Visualization 2: Correction rates by attribute
plot_data_correction <- correction_rates %>%
  mutate(Attribute = factor(Attribute, levels = Attribute))

p_correction <- ggplot(plot_data_correction, aes(x = Attribute, y = Hit_Rate, fill = Attribute)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = paste0(Hit_Rate, "%\n(n=", N, ")")),
            vjust = -0.3, size = 5) +
  scale_fill_manual(values = c("Women" = "#990000", "Poets" = "#FF6B35",
                                "Oldies" = "#4ECDC4", "Books" = "#011F5B")) +
  labs(
    x = "Most Underrepresented Attribute",
    y = "% Selecting That Attribute for 7th Pick",
    title = "Do Participants 'Correct' for Underrepresentation?",
    subtitle = "When attribute X is the most underrepresented in first 6 picks,\nwhat % select attribute X for the 7th pick?"
  ) +
  theme_bw() +
  theme(
    legend.position = "none",
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 11),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5),
    panel.grid.minor = element_blank()
  ) +
  ylim(0, 60)

ggsave("Figure-Correction-Rates.pdf", plot = p_correction,
       width = 10, height = 7, device = cairo_pdf)
cat("Saved: Figure-Correction-Rates.pdf\n")

cat("\nDone!\n")
